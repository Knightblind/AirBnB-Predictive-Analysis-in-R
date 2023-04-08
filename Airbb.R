#@Author: Thomas Sorenson
###Description:The goal of this code is to create an ensemble of models and procedures that will be effective at predicting the rating of an airBB



#installed Packqages and Libraries

install.packages("readr")
library(readr)
install.packages("caret")
library(caret)



install.packages("glmnet")
library(glmnet)

install.packages("tree")
library(tree)
install.packages("randomForest")
library(randomForest)






###Read in Data
#airbb_test   = read.csv("i:/Analysis6100 - 6110/Airbnb_Testing.csv")
airbb_data = read_csv("i:/Analysis6100 - 6110/Airbnb_Training.csv")
str(airbb_data)
which(lapply(airbb_data, class)=="character")


##The Data


###feature name,	description,	variable type
###index,	a reference column of indices for each listing (numbered 1 to 29985),	numerical
###avg_rating,	the average rating of the Airbnb property (out of 100),	numerical
###accommodates,	how many guests can stay in the listing,	numerical
###amenities,	list of amenties available in the listing,	categorical list
###availability_30,	how many days out of the next 30 the listing is available for,	numerical
###availability_365,	how many days out of the next 365 the listing is available for,	numerical
###availability_60,	how many days out of the next 60 the listing is available for,	numerical
##availability_90,	how many days out of the next 90 the listing is available for,	numerical
##bathrooms,	number of bathrooms in the listing,	numerical
##bed_type,	description of the bed,	categorical
##bedrooms,	number of bedrooms in the listing,	numerical
###beds,	number of beds in the listing,	numerical
##cancellation_policy,	description of how strict the cancellation policy is,	categorical
##city_name,	the broader metro area the listing is in,	categorical
##cleaning_fee,	how much, if any, is the cleaning fee the host charges,	numerical
##country,	country,	categorical
###experiences_offered,	whether there are "experiences" offered with the listing (t) or not (f),	categorical
###extra_people,	additional charge for extra people in the rental,	numerical
###first_review,	when the first review for the listing was written,	date
###guests_included,	how many guests are included in the price of the rental,	numerical
###host_acceptance_rate,	percent of stay requests the host accepts,	numerical
###host_has_profile_pic,	if the host has a visible profile picture,	categorical
###host_identity_verified,	whether the host's identity has been verified using Airbnb's process,	categorical
###host_is_superhost,	whether the host is a "superhost",	categorical
###host_listings_count,	how many total listings the host has,	numerical
###host_response_rate,	percent of stay requests the host responds to,	numerical
###host_response_time,	how long it takes the host to respond to requests,	categorical
###host_since,	date the host joined Airbnb,	date
###host_total_listings_count,	how many total listings the host has ever had,	numerical
###host_verifications,	ways the host has verified their identity	categorical list
###house_rules	free text field describing the rules in the residence,	text
###instant_bookable,	whether you can instantly book the airbnb (t) or not (f),	categorical
###is_business_travel_ready,	whether the listing is available for business travel (t) or not (f),	categorical
###is_location_exact,	whether the listing reports the exact location (t) or not (f) (usually for privacy purposes),	categorical
###latitude,	the number of degrees west of the prime meridien,	numerical
###longitude,	the number of degrees north of the equator,	numerical
###maximum_nights,	maximum nights you can book the listing for,	numerical
###minimum_nights,	minimum nights you can book the listing for,	numerical
###monthly_price,	price to rent the listing for a month,	numerical
###neighborhood_overview,	free text field written by the host describing their neighborhood,	text
###price,	price to rent the listing for one night,	numerical
##property_type,	description of the type of dwelling the listing is in,	categorical
###require_guest_phone_verification,	whether the host requires a phone number to verify the guest's ID (t) or not (f),	categorical
###require_guest_profile_picture,	whether the host requires the guest's profile picture (t) or not (f),	categorical
##requires_license,	whether the listing is in a jurisdiction that requires the host to have a license (t) or not (f),	categorical
##room_type,	description of the type of accomodation of the listing,	categorical
##security_deposit,	the amount of security deposit required to rent the listing,	numerical
##summary,	free text field summarizing the description of the listing,	text
##transit,	free text field describing nearby transit options for the listing,	text
##weekly_price,	price to rent the listing for a week,	numerical



###Data Cleaning

airbb_data$amenities = NULL
airbb_data$experiences_offered = NULL
airbb_data$house_rules = NULL
airbb_data$neighborhood_overview = NULL
airbb_data$summary = NULL
airbb_data$transit = NULL


#percent missing values in Cancellation policy
sum(is.na(airbb_data$cancellation_policy))/nrow(airbb_data)

#percent of missing accommodates
sum(is.na(airbb_data$accommodates))/nrow(airbb_data)

#percent missing values inavailability_3
sum(is.na(airbb_data$availability_30))/nrow(airbb_data)

#percent issing in availability_365
sum(is.na(airbb_data$availability_365))/nrow(airbb_data)

#percent missing values in availability_60
 sum(is.na(airbb_data$availability_60))/nrow(airbb_data)

#percent missing in availability_90
sum(is.na(airbb_data$availability_90))/nrow(airbb_data)

##percent missing in bathrooms

sum(is.na(airbb_data$bathrooms))/nrow(airbb_data)

##percent missing in bed_type
 sum(is.na(airbb_data$bed_type))/nrow(airbb_data)

##percent missing in bedrooms)
sum(is.na(airbb_data$bedrooms))/nrow(airbb_data)


##percent missing in beds
sum(is.na(airbb_data$beds))/nrow(airbb_data)

##percent missing in cancellation_policy
sum(is.na(airbb_data$cancellation_policy))/nrow(airbb_data)

##percent missing incity_name
sum(is.na(airbb_data$city_name))/nrow(airbb_data)

##percent missing in cleaning_fee
sum(is.na(airbb_data$cleaning_fee))/nrow(airbb_data)

##percent missing in country
sum(is.na(airbb_data$country))/nrow(airbb_data)

##percent missing in extra_people
sum(is.na(airbb_data$extra_people))/nrow(airbb_data)


##percent missing in first_review
 sum(is.na(airbb_data$first_review))/nrow(airbb_data)


#percent missing in guests_included
sum(is.na(airbb_data$guests_included))/nrow(airbb_data)

#percent missing in host_acceptance_rate

sum(is.na(airbb_data$host_acceptance_rate))/nrow(airbb_data)


#percent missing in host_has_profile_pic

sum(is.na(airbb_data$host_has_profile_pic))/nrow(airbb_data)

#percent missing in host_identity_verified

sum(is.na(airbb_data$host_identity_verified))/nrow(airbb_data)


#percent missing in host_is_superhost

sum(is.na(airbb_data$host_is_superhost))/nrow(airbb_data)

#percent missing in host_listings_count

sum(is.na(airbb_data$host_listings_count))/nrow(airbb_data)


#percent missing in host_response_rate
sum(is.na(airbb_data$host_response_rate))/nrow(airbb_data)


#percent missing in host_response_time
sum(is.na(airbb_data$host_response_time))/nrow(airbb_data)


#percent missing in host_since
 
sum(is.na(airbb_data$host_since))/nrow(airbb_data)


#percent missing in  host_total_listings_count

sum(is.na(airbb_data$host_total_listings_count))/nrow(airbb_data)


#percent missing in host_verifications

sum(is.na(airbb_data$host_verifications))/nrow(airbb_data)


#percentmissing in instant_bookable

sum(is.na(airbb_data$instant_bookable))/nrow(airbb_data)


#percentmissing in is_business_travel_ready

sum(is.na(airbb_data$is_business_travel_ready))/nrow(airbb_data)


#percentmissing in is_location_exact
 sum(is.na(airbb_data$is_location_exact))/nrow(airbb_data)


#percentmissing in latitude
sum(is.na(airbb_data$latitude))/nrow(airbb_data)


#percentmissing in longitude
sum(is.na(airbb_data$longitude))/nrow(airbb_data)


#percentmissing in maximum_nights
sum(is.na(airbb_data$maximum_nights))/nrow(airbb_data)


#percentmissing in minimum_nights
sum(is.na(airbb_data$minimum_nights))/nrow(airbb_data)


#percentmissing in monthly_price
 sum(is.na(airbb_data$monthly_price))/nrow(airbb_data)


#percentmissing in price
sum(is.na(airbb_data$price))/nrow(airbb_data)


#percentmissing in property_type
 sum(is.na(airbb_data$property_type))/nrow(airbb_data)


#percentmissing in require_guest_phone_verification
sum(is.na(airbb_data$require_guest_phone_verification))/nrow(airbb_data)


#percentmissing in require_guest_profile_picture
sum(is.na(airbb_data$require_guest_profile_picture))/nrow(airbb_data)


#percentmissing in requires_license
sum(is.na(airbb_data$requires_license))/nrow(airbb_data)


#percentmissing in  room_type
sum(is.na(airbb_data$room_type))/nrow(airbb_data)


#percentmissing in security_deposit
sum(is.na(airbb_data$security_deposit))/nrow(airbb_data)


#percentmissing in weekly_price
sum(is.na(airbb_data$weekly_price))/nrow(airbb_data)

#%missing in price
sum(is.na(airbb_data$price))/nrow(airbb_data)




###Next Step is going to be removing any variables missing a large amount of data if they cannot be altered

airbb_data$beds = ifelse(is.na(airbb_data$beds)==TRUE,0,airbb_data$beds) 

airbb_data$first_review =NULL

airbb_data$host_acceptance_rate = ifelse(is.na(airbb_data$host_acceptance_rate)==TRUE,0,airbb_data$host_acceptance_rate)

airbb_data$maximum_nights = ifelse(is.na(airbb_data$maximum_nights)==TRUE,1,airbb_data$maximum_nights)
airbb_data$maximum_nights = ifelse(airbb_data$maximum_nights==TRUE,mean(airbb_data$maximum_nights),airbb_data$maximum_nights)

airbb_data$minimum_nights = ifelse(is.na(airbb_data$minimum_nights)==TRUE,1,airbb_data$minimum_nights)


airbb_data$cleaning_fee = ifelse(is.na(airbb_data$cleaning_fee)==TRUE,0,airbb_data$cleaning_fee)

airbb_data$price = ifelse(is.na(airbb_data$price)==TRUE,median(airbb_data$price),airbb_data$price)

airbb_data$security_deposit = ifelse(is.na(airbb_data$security_deposit)==TRUE,0,airbb_data$security_deposit)

airbb_data$monthly_price = ifelse(is.na(airbb_data$monthly_price)==TRUE,airbb_data$price *30,airbb_data$monthly_price)  

airbb_data$weekly_price = ifelse(is.na(airbb_data$weekly_price)==TRUE,airbb_data$price*7 , airbb_data$weekly_price)

airbb_data$index = NULL

airbb_data$country=NULL

airbb_data$is_business_travel_ready = ifelse(is.na(airbb_data$is_business_travel_ready)==TRUE,0,airbb_data$is_business_travel_ready)

airbb_data$host_response_rate = ifelse(is.na(airbb_data$host_response_rate)==TRUE,median(airbb_data$host_response_rate,na.rm=TRUE),airbb_data$host_response_rate)

airbb_data$host_response_time = NULL

airbb_data = na.omit(airbb_data)

# Check all the variables and print out number of NAs
sapply(airbb_data, function(x) sum(is.na(x)))

## Character variables that are not factors:
airbb_data$bed_type = NULL
airbb_data$cancellation_policy = NULL
airbb_data$city_name= NULL
airbb_data$host_verifications= NULL
airbb_data$property_type= NULL
airbb_data$room_type= NULL




# Now lets start  partitioning our data

set.seed(4335)


## First, partition 30% of the data for testing data

num_obs =  nrow(airbb_data)
test_obs = sample(num_obs, 0.3*num_obs)
##Then save the rest of our data
airbb_rest = airbb_data[-test_obs,]
airbb_test = airbb_data[test_obs,]


## First, start by creating a full model (all variables we want to use) and a null model (no variables)
airbb_all = glm(avg_rating~., data=airbb_rest)
airbb_null = glm(avg_rating~1, data =airbb_rest)
  
## The step function always goes step(starting model, ending model (if necessary), direction to search)
## The backward elimination model does not need an ending model, because it ends when there are no more variables

##Stepwise backward model  
airbb.backward = step(airbb_all, direction="backward")
summary(airbb.backward)


#stepwise forward Model
airbb.forward = step(airbb_null, scope=list(upper=airbb_all), direction="forward")
summary(airbb.forward)


##Stepwise both model
#air.both = step(airbb_null, scope=list(upper=airbb_all), direction="both", trace=1)
#summary(air.both)

## Ridge Model

airbb.ridge = glmnet(as.matrix(airbb_rest[,-1]), airbb_rest$avg_rating, alpha = 0)
summary(airbb.ridge)


ridge_cv = cv.glmnet(as.matrix(airbb_rest[,]), airbb_rest$avg_rating, alpha = 0)
best_ridge = ridge_cv$lambda.min
best_ridge
predict(airbb.ridge, s = best_ridge,type="coefficients")


###•	LASSO
airbb.lasso = glmnet(as.matrix(airbb_rest[,-1]), airbb_rest$avg_rating, alpha = 1)
summary(airbb.lasso)

lasso_cv = cv.glmnet(as.matrix(airbb_rest[,]), airbb_rest$avg_rating,alpha = 1)
best_lasso = lasso_cv$lambda.min
best_lasso
predict(airbb.lasso, s = best_lasso,type="coefficients")


##Bootstrapping

num_rest=nrow(airbb_rest)
bootstrap_sample = sample(seq(1,num_rest),num_rest,replace=T)

bag.tree = tree(avg_rating~., data=airbb_rest[bootstrap_sample,])
summary(bag.tree)

plot(bag.tree)
text(bag.tree,pretty=1)


##Bagging procedure, 500 trees
ncol(airbb_rest)


bag.forest = randomForest(avg_rating~., data=airbb_rest,ntree=100,mtry=43,importance=TRUE)
varImpPlot(bag.forest)
importance(bag.forest)


##Random Forest procedure
random.forest = randomForest(avg_rating~., data=airbb_rest,ntree=100,mtry=5,importance=TRUE)
random.forest
importance(random.forest)
varImpPlot(random.forest)









##Now lets calculate our predictions and accurracies 

## Make sure you don't have the type = "response" argument

##Forward Predictions

forward_preds = predict(airbb.forward, airbb_test)
summary(forward_preds)

##Backward Predictions

backward_preds = predict(airbb.backward, airbb_test)
summary(backward_preds)

##ridge Predictions

ridge_preds =  predict(airbb.ridge, s = best_ridge, newx = as.matrix(airbb_test[,-1]))
summary(ridge_preds)
##Lasso Predictions

lasso_preds = predict(airbb.lasso, s = best_lasso,                        newx = as.matrix(airbb_test[,-1])) 
summary(lasso_preds)
 
##Bagged Tree predictions
bag.tree_preds = predict(bag.tree,newdata=airbb_test)
##Bagged forest predictions
bag.forest_preds_test = predict(bag.forest, newdata=airbb_test)
summary(bag.forest_preds_test)


##random forest predictions
random.forest_preds = predict(random.forest, newdata= airbb_test)
summary(random.forest_preds)






##Now lets check Accuracies


## Baseline accuracy

Baseline



##backward accuracy

backward_rmse = sqrt(mean((backward_preds-airbb_test$avg_rating)^2)) 
backward_rmse

##Forward Accuracy
forward_rmse = sqrt(mean((forward_preds-airbb_test$avg_rating)^2)) 
forward_rmse

##Ridge Accuracy
ridge_rmse = sqrt(mean((ridge_preds-airbb_test$avg_rating)^2)) 
ridge_rmse




##Lasso Accuracy

lasso_rmse = sqrt(mean((lasso_preds-airbb_test$avg_rating)^2)) 
lasso_rmse
##Bag tree accuracy

bag.tree_rmse = sqrt(mean((bag.forest_preds-airbb_test$avg_rating)^2))  

bag.forest_rmse = sqrt(mean((bag.forest_preds_test-airbb_test$avg_rating)^2)) 
bag.forest_rmse
##Random.Forest Accuracy

random.forest_rmse= sqrt(mean((random.forest_preds-airbb_test$avg_rating)^2))
random.forest_rmse


##Now lets calculate ensemble predictions


ensemble_preds = (forward_preds + backward_preds+ridge_preds+lasso_preds+bag.forest_preds_test+random.forest_preds)/6 
#ensemble_preds

ensemble_rmse = sqrt(mean((ensemble_preds-airbb_test$avg_rating)^2)) 
ensemble_rmse

##Now lets export predictions
prediction_data = cbind(airbb_test$index,ensemble_preds)
write.csv(prediction_data,"I:/Analysis6100 - 6110/predictiondata.csv")

