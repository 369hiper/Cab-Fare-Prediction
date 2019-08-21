library(ggplot2) # Visualisation
library(dplyr) # Data Wrangling
library(e1071) # Prediction: SVR
library(randomForest) # Prediction: Random Forest
head(df)
NY_Train = read.csv('C:/Users/admin/Documents/Edwisor Cab Fare/train_cab.csv')
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information","caTools",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
lapply(x, require, character.only = TRUE)

myData <- NY_Train[-c(1125),]
library(dplyr)
NY_Train$fare_amount <- as.numeric(NY_Train$fare_amount)
NY_Train$pickup_datetime = as.POSIXct(NY_Train$pickup_datetime, format="%Y-%m-%d %H:%M:%S")
NY_Train$passenger_count <- as.integer(NY_Train$passenger_count)
# We are going to be checking about the NA and Missing Values
apply(NY_Train, 2, function(x){sum(is.na(x))})
str(NY_Train)
# install.packages(x)
rm(x)

df$fare_amount  <- gsub("[^0-9A-Za-z///' ]","'" , NY_Train$fare_amount,ignore.case = TRUE)


Data <- gsub("''","" , Data ,ignore.case = TRUE)

str(df)
summary(df)
#Distribution of Fare Amount
P1 <- ggplot(aes(x =fare_amount), data = NY_Train)+ geom_histogram(bins = 100) +
  scale_x_continuous(limits = c(-50,125)) + 
  geom_vline(aes(xintercept = median(fare_amount)), linetype = "dashed", size = 0.5, color = "blue") +
  geom_text(aes(x = 8.5, y = 2, label = "median")) + ggtitle("Distribtion of Fare Amount")


#Distribtion of Passenger Count
P2 <- ggplot(aes(x=factor(passenger_count)), data = NY_Train) + geom_bar() +
  geom_text(aes(label =scales::percent(..count../sum(..count..))),stat = 'count',vjust = -0.5) + ggtitle("Distribution of Passenger Count") + labs(x = "passenger Count")

P1

#Distribution of Fare Amount
P1 <- ggplot(aes(x =fare_amount), data = NY_Train)+ geom_histogram(bins = 100) +
  scale_x_continuous(limits = c(-50,125)) + 
  geom_vline(aes(xintercept = median(fare_amount)), linetype = "dashed", size = 0.5, color = "blue") +
  geom_text(aes(x = 8.5, y = 2000, label = "median")) + ggtitle("Distribtion of Fare Amount")


#Distribtion of Passenger Count
P2 <- ggplot(aes(x=factor(passenger_count)), data = NY_Train) + geom_bar() +
  geom_text(aes(label =scales::percent(..count../sum(..count..))),stat = 'count',vjust = -0.5) + ggtitle("Distribution of Passenger Count") + labs(x = "passenger Count")

P1




# Outlier Analysis
numeric_index = sapply(df,is.numeric)
numeric_index
numeric_data = df[,numeric_index]
cnames = colnames(numeric_data)
for(i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(df))+ 
                      stat_boxplot(geom = "errorbar", width = 0.5) +
                      geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                                   outlier.size=1, notch=FALSE) +
                      theme(legend.position="bottom")+
                      labs(y=cnames[i],x="responded")+
                      ggtitle(paste("Box plot of responded for",cnames[i])))
}
# Plot the Graph
gridExtra::grid.arrange(gn1,gn5,gn2,ncol=3)
gridExtra::grid.arrange(gn6,gn7,ncol=2)
gridExtra::grid.arrange(gn8,gn9,ncol=2)

# Remove the outliers using boxplot method
val = df$previous[df$previous %in% boxplot.stats(marketing_train$previous)$out]

for(i in cnames){
    print(i)
    val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
    print(length(val))
    marketing_train = df[which(!df[,i] %in% val),]
}
for(i in cnames){
    val = marketing_train[,i][marketing_train[,i] %in% boxplot.stats(marketing_train[,i])$out]
    print(length(val))
    marketing_train[,i][marketing_train[,i] %in% val] = NA
}

# We will now be applying the Multiple Linear Regression
marketing_train
# For Fitting our regression models first we have to split the data into Training and Testing
sample = sample.split(marketing_train,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train1 = subset(marketing_train,sample == TRUE)
test1 = subset(marketing_train, sample == FALSE)
train1
test1
# Now fitting Multiple Linear Regression to the Training Set.
train_subset = select(train1, season, holiday, workingday, weather, temp, hum, windspeed, cnt)

regressor = lm (formula= cnt~ . , data = marketing_train)
# Choosing the best model in AIC in a stepwise Algorithm
# The Step( function iteratively removes insignificant features from the model
regressor = step(regressor)
