library(ggplot2) # Visualisation
library(dplyr) # Data Wrangling
library(e1071) # Prediction: SVR
library(randomForest) # Prediction: Random Forest
head(df)
df = read.csv('C:/Users/admin/Documents/Edwisor Cab Fare/train_cab.csv',header=TRUE,
              'colClasses=c("key"="chafare_amount"="numeric",
                           "pickup_datetime"="character",
                           "dropoff_longitude"="numeric",
                           "pickup_longitude"="numeric","dropoff_latitude"="numeric",
                           "pickup_latitude"="numeric",
                           "passenger_count"="integer"), stringsAsFactors = FALSE)'c
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
install.packages("caTools")
library(caTools)
# install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)
df$pickup_datetime = as.POSIXct(df$pickup_datetime, format="%Y-%m-%d %H:%M:%S")
str(df)
summary(df)
# Plotting the Count of Rental along with the Temprature
ggplot(df, aes(passenger_count, fare_amount)) +
  geom_point(aes(color=passenger_count),alpha=0.2) + theme_bw()



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
