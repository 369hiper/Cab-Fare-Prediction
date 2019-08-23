library(tidyverse)
library(xgboost)
library(caret)
library(magrittr)
library(Matrix)
library(geosphere)

install.packages("geosphere")
train = read.csv('C:/Users/nsida/Cab fare prediction//train_cab.csv')
head(train)
train$fare_amount  <- gsub("[^0-9A-Za-z///' ]","" , train$fare_amount,ignore.case = TRUE)
train$fare_amount <- as.character(train$fare_amount)
train$fare_amount <- as.numeric(train$fare_amount)
# train$fare_amount <- as.numeric(as.character(train$fare_amount))

train$pickup_datetime = as.POSIXct(train$pickup_datetime, format="%Y-%m-%d %H:%M:%S UTC")

train_1  = train %>%
  mutate(pickup_datetime = as.POSIXct(pickup_datetime)) %>%
  mutate(hour = as.numeric(format(pickup_datetime, "%H"))) %>%
  mutate(min = as.numeric(format(pickup_datetime, "%M"))) %>%   
  mutate(year = as.factor(format(pickup_datetime, "%Y"))) %>%
  mutate(day = as.factor(format(pickup_datetime, "%d"))) %>%
  mutate(month = as.factor(format(pickup_datetime, "%m"))) %>%
  mutate(Wday = as.factor(weekdays(pickup_datetime))) %>%
  mutate(hour_class=as.factor(ifelse(hour<5,"Overnight",ifelse(hour<11,"Morning",ifelse(hour<16,"Noon",ifelse(hour<20,"Evening",ifelse(hour<23,"Night","Overnight"))))))) %>% 
  filter(pickup_longitude>-80 & pickup_longitude < -70) %>% 
  filter(pickup_latitude>35 & pickup_latitude < 45) %>% 
  filter(dropoff_longitude > -80 & dropoff_longitude < -70) %>%
  filter(dropoff_latitude > 35 & dropoff_latitude < 45) %>%
  filter(fare_amount > 0 & fare_amount <= 1000) %>%
  filter(passenger_count > 0 & passenger_count < 15)

#jfk
jfk_lat<-40.6413
jfk_long<--73.7781
jfk<-c(jfk_long, jfk_lat)
#newark
nwk_lat<-40.6895
nwk_long<--74.1745
nwk<-c(nwk_long, nwk_lat)
#laguardia
lag_lat<-40.779
lag_long<--73.8740
lag<-c(lag_long, lag_lat)
#MSG
msg_lat<-40.7505
msg_long<--73.9934
msg<-c(msg_long, msg_lat)

#times square
ts_lat<-40.7589
ts_long<--73.9851
ts<-c(ts_long, ts_lat)
#freedom tower
freedom_lat<-40.7127
freedom_long<--74.0134
freedom<-c(freedom_long, freedom_lat)
#empire state building
esb_lat<-40.7484
esb_long<--73.9857
esb<-c(esb_long, esb_lat)
#grand central
grand_lat<-40.7527
grand_long<--73.9772
grand<-c(grand_long, grand_lat)

#bronx
bronx_lat <- (40.837048 * pi)/180
bronx_long <- (-73.865433 * pi)/180
bronx<-c(bronx_long, bronx_lat)
nyc<-c(-74.0063889, 40.7141667)


train_2=train_1 %>% 
  mutate(
    dist = distHaversine(cbind(pickup_longitude, pickup_latitude), cbind(dropoff_longitude, dropoff_latitude), r = 6371),
    to_jfk = distHaversine(cbind(pickup_longitude, pickup_latitude), jfk, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), jfk, r = 6371),
    to_nkw = distHaversine(cbind(pickup_longitude, pickup_latitude), nwk, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), nwk, r = 6371),
    to_lag = distHaversine(cbind(pickup_longitude, pickup_latitude), lag, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), lag, r = 6371),
    to_msg = distHaversine(cbind(pickup_longitude, pickup_latitude), msg, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), msg, r = 6371),
    to_ts = distHaversine(cbind(pickup_longitude, pickup_latitude), ts, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), ts, r = 6371),
    to_freedom = distHaversine(cbind(pickup_longitude, pickup_latitude), freedom, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), freedom, r = 6371),
    #to_esb = distHaversine(cbind(pickup_longitude, pickup_latitude), esb, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), esb, r = 6371),
    to_grand = distHaversine(cbind(pickup_longitude, pickup_latitude), grand, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), grand, r = 6371),
    to_bronx = distHaversine(cbind(pickup_longitude, pickup_latitude), bronx, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), bronx, r = 6371),
    to_nyc = distHaversine(cbind(pickup_longitude, pickup_latitude), nyc, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), nyc, r = 6371)
  )


# splitting into train and test dataset , since we have large dataset we split 60%/40%
  
index=createDataPartition(train_2$year,p=0.6,list=FALSE)
dtrain_3=train_2[index,]
dtest_3=train_2[-index,]


to_rad=function(df)
{
  df$pickup_longitude=df$pickup_longitude*pi/180
  df$pickup_latitude=df$pickup_latitude*pi/180
  df$dropoff_longitude=df$dropoff_longitude*pi/180
  df$dropoff_latitude=df$dropoff_latitude*pi/180
  return(df)
}
dtrain_4=to_rad(dtrain_3)
dtest_4=to_rad(dtest_3)

dtrain_4_matrix=xgb.DMatrix(data=data.matrix(dtrain_4[,-1]),label=dtrain_4$fare_amount)
dtest_4_matrix=xgb.DMatrix(data=data.matrix(dtest_4[,-1]),label=dtest_4$fare_amount)


p = list(objective = "reg:linear",eval_metric = "rmse",max_depth = 6 ,eta = .05,subsample=1,colsample_bytree=0.8,num_boost_round=1000,nrounds = 300)
set.seed(0)
m_xgb <- xgb.train(p,dtrain_4_matrix,p$nrounds,list(val = dtest_4_matrix),print_every_n = 1,early_stopping_rounds = 10)


test=read_csv('C:/Users/nsida/Cab fare prediction/test.csv',col_types =list(
  pickup_datetime=col_datetime("%Y-%m-%d %H:%M:%S %Z"),
  pickup_longitude=col_double(),
  pickup_latitude=col_double(),
  dropoff_longitude=col_double(),
  dropoff_latitude=col_double(),
  passenger_count=col_integer()
))
test_1  = test %>%
  mutate(pickup_datetime = as.POSIXct(pickup_datetime)) %>%
  mutate(hour = as.numeric(format(pickup_datetime, "%H"))) %>%
  mutate(min = as.numeric(format(pickup_datetime, "%M"))) %>%   
  mutate(year = as.factor(format(pickup_datetime, "%Y"))) %>%
  mutate(day = as.factor(format(pickup_datetime, "%d"))) %>%
  mutate(month = as.factor(format(pickup_datetime, "%m"))) %>%
  mutate(Wday = as.factor(weekdays(pickup_datetime))) %>%
  mutate(hour_class=as.factor(ifelse(hour<5,"Overnight",ifelse(hour<11,"Morning",ifelse(hour<16,"Noon",ifelse(hour<20,"Evening",ifelse(hour<23,"Night","Overnight")))))))

test_2=test_1 %>% 
  mutate(
    dist = distHaversine(cbind(pickup_longitude, pickup_latitude), cbind(dropoff_longitude, dropoff_latitude), r = 6371),
    to_jfk = distHaversine(cbind(pickup_longitude, pickup_latitude), jfk, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), jfk, r = 6371),
    to_nkw = distHaversine(cbind(pickup_longitude, pickup_latitude), nwk, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), nwk, r = 6371),
    to_lag = distHaversine(cbind(pickup_longitude, pickup_latitude), lag, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), lag, r = 6371),
    to_msg = distHaversine(cbind(pickup_longitude, pickup_latitude), msg, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), msg, r = 6371),
    to_ts = distHaversine(cbind(pickup_longitude, pickup_latitude), ts, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), ts, r = 6371),
    to_freedom = distHaversine(cbind(pickup_longitude, pickup_latitude), freedom, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), freedom, r = 6371),
    #to_esb = distHaversine(cbind(pickup_longitude, pickup_latitude), esb, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), esb, r = 6371),
    to_grand = distHaversine(cbind(pickup_longitude, pickup_latitude), grand, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), grand, r = 6371),
    to_bronx = distHaversine(cbind(pickup_longitude, pickup_latitude), bronx, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), bronx, r = 6371),
    to_nyc = distHaversine(cbind(pickup_longitude, pickup_latitude), nyc, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), nyc, r = 6371)
  )
train
test_3=to_rad(test_2)

test_3_matrix=xgb.DMatrix(data=data.matrix(test_3[,-1]))

# Random Forest\

library(data.table)
library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)
library(tictoc)
install.packages("tictoc")

apply(NY_Train, 2, function(x){sum(is.na(x))})
# # Remove the NA
NY_Train <- na.omit(train_2)
tic()
fitRF <- randomForest(formula = fare_amount ~ ., data = NY_Train, ntree = 15, na.rm=T)
toc()
regressor = randomForest(x = NY_Train[,-which(names(NY_Train)=="fare_amount")],
                         y = NY_Train$fare_amount)
# Predicting a new result with Random Forest Regression
y_pred = predict(regressor, test_2)
summary(regressor)
results <- data.frame(datetime = test_2$pickup_datetime, count = y_pred)
write.csv(results, file = 'Cab_Fare_Prediction_RandomForest.csv', row.names = False, quote = FALSE)
# mEASURING tHE MEAN aBSOLUTE Error
# Mean Absolute error
mae(y_pred, observed, na.rm = FALSE)
# Write the results to a csv file

# library(flexclust)

#GETTING THE PREDICTION FROM THE XGBOOST
prediction_xgb=predict(m_xgb,newdata=test_3_matrix)


prediction=data.frame(key=test$pickup_datetime, fare_amount= fitRF)
hello = write.csv(prediction, "prediction.csv")
