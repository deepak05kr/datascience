library(recommenderlab)
library(ggplot2)
library(dplyr)

# Loading of data
beer_data <- read.csv("beer_data.csv")

summary(beer_data) #review_profilename have missing values, reviews are in range 0-5
str(beer_data) #beer_beeridid is numeric, review_profilename is factor, review_overall is numeric, 
dim(beer_data) #475984 user rating and  3 features

# missing and na values Removal
nrow(beer_data[(is.na(beer_data$review_profilename) ), ]) # 0
nrow(beer_data[( beer_data$review_profilename==""), ]) # 100 
beer_data<-beer_data[!(beer_data$review_profilename==""), ] # removed missing values

#Check for duplicate values where item and user both are duplicate and same user has given different ratings to same beer
beer_data[!(duplicated(beer_data[c("beer_beerid","review_profilename")]) | duplicated(beer_data[c("beer_beerid","review_profilename")], fromLast = TRUE)), ] %>% nrow()
beer_data %>% distinct(beer_beerid,review_profilename,.keep_all=TRUE) %>% nrow() #474462 unique reviews
beer_data<-distinct(beer_data,beer_beerid,review_profilename,.keep_all = TRUE) #Removing duplicates 


########################################################################################
#1. Choose only those beers that have at least N number of reviews
########################################################################################

# calculating unique beer count and review count 
beer_data_byID <- beer_data %>% group_by(beer_beerid) 
reviews_count  <- summarise(beer_data_byID,count_beer_reviews=n())
dim(reviews_count) # 40308 unique beers
summary(reviews_count) 

# calculating number of unique users
beer_data_byUser <- beer_data %>% group_by(review_profilename) 
summarise(beer_data_byUser,count_user_reviews=n()) #22497 unique users

# analysis of review count to decide value of N
qplot(reviews_count$count_beer_reviews, geom = "histogram") # maximum number of beer have single reviews
reviews_count %>% subset(count_beer_reviews==1) %>% dim() # 18080 beers have only 1 review

# creating seprate df with  count of reviews and frequency of count of reviews
review_freq<-reviews_count %>% group_by(count_beer_reviews) %>% summarise(review_occured=n())
review_freq #Review frequency drops by almost half with each increase in total_beer_review
ggplot(review_freq,aes(x=count_beer_reviews,y=review_occured)) + geom_point()

# now removing beers with only one review  and checking remaining data 
review_freq_remaining <- review_freq %>% subset(count_beer_reviews!=1)
ggplot(review_freq,aes(x=count_beer_reviews,y=review_occured)) + geom_point() # we can see that count of reviews per beer has dropped at fixed rate by increasing num of review

# Calculation of ratio of review count vs total reviews 
review_freq$review_ratio<-(review_freq$review_occured*100)/40308
ggplot(review_freq,aes(x=count_beer_reviews,y=review_ratio)) + geom_point() 

#Lets further filter this to ratios less then .003
review_freq_remaining<-subset(review_freq,review_freq$review_ratio<.003)
ggplot(review_freq_remaining,aes(x=count_beer_reviews,y=review_ratio)) + geom_point() # almost constant ratio

# Finalising value of N:
beer_matrix <- as(beer_data[,c(2,1,3)], "realRatingMatrix")

# keeping only those beers whose review count is more than or equal to 50
more_than_50_beer_reviews<-subset(reviews_count,reviews_count$count_beer_reviews>=50)

ggplot(more_than_50_beer_reviews,aes(x=count_beer_reviews)) + geom_bar()


beer_data_byuser <- beer_data %>% group_by(review_profilename)
userwise_review <- summarise(beer_data_byuser,count_user_reviews=n()) 
more_than_50_userwise_review<-subset(userwise_review,userwise_review$count_user_reviews>=50)

beers<-merge(beer_data,more_than_50_beer_reviews,by.x="beer_beerid",by.y="beer_beerid")
beers<-merge(beers,more_than_50_userwise_review,by.x="review_profilename",by.y="review_profilename")

summary(beers) 

###################################################################################
#2. Convert this data frame to a “realratingMatrix” before you build your collaborative filtering models
###################################################################################

beer_matrix <- as(beers[,c(1,2,3)], "realRatingMatrix")
class(beer_matrix)

dimnames(beer_matrix)
rowCounts(beer_matrix)
colCounts(beer_matrix)
rowMeans(beer_matrix)

beer_df <- as(beer_matrix, "data.frame")
summary(beer_df)
str(beer_df)

###################################################################################
#3. Determine how similar the first ten users are with each other and visualise it
###################################################################################


similar_users <- similarity(beer_matrix[1:10, ],
                            method = "cosine",
                            which = "users")

#Similarity matrix
as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")

###################################################################################
#4. Compute and visualise the similarity between the first 10 beers
###################################################################################

similar_beers <- similarity(beer_matrix[1:10, ],
                            method = "cosine",
                            which = "items")
#Similarity matrix
as.matrix(similar_beers)

#Visualise similarity matrix
image(as.matrix(similar_beers), main = "Beer similarity")

###################################################################################
#5. What are the unique values of ratings?
###################################################################################

unique_rating <- group_by(beer_df,rating)
summary_unique_rating <- summarise(unique_rating,rating_freq=n()) 
nrow(summary_unique_rating) # 9 unique rating
#View(summary_unique_rating) # 4 and 4.5 has max'm count 1 and 1.5 has least count


###################################################################################
#6.Visualise the rating values and notice : average beer rating, avg user rating etc.
###################################################################################

#1 The average beer ratings
item_wise_beer_df <- group_by(beer_df,item)
average_beer_rating <- summarise(item_wise_beer_df,average_rating=mean(rating))
ggplot(average_beer_rating,aes(x=average_rating)) + geom_histogram() + labs(x="Average Rating", y="num of Beers")
summary(average_beer_rating$average_rating) # Mean=3.804 , Median=3.867

#2 The average user ratings
userwise_beer_df<- group_by(beer_df,user) 
average_user_rating <- summarise(userwise_beer_df,average_rating=mean(rating))
ggplot(average_user_rating,aes(x=average_rating)) + geom_histogram() + labs(x="Average Rating", y="num of Users")
summary(average_user_rating$average_rating) # Mean=3.871 & Median=3.894

#3 The average number of ratings given to the beers
average_beer_df<- group_by(beers,beer_beerid) 
average_beer_review <- summarise(average_beer_df,average_review=mean(count_beer_reviews))
ggplot(average_beer_review,aes(x=average_review)) + geom_histogram() + labs(x="Average Rating", y="# of Beers")
summary(average_beer_review$average_review) # mean = 143.4

#4 The average number of ratings given by the users
average_user_rating_df<-group_by(beers,review_profilename)
average_user_review <-  summarise(average_user_rating_df,average_review=mean(count_user_reviews))
ggplot(average_user_review,aes(x=average_review)) + geom_histogram()
summary(average_user_review$average_review) # mean = 158 


###################################################################################
#         Recommendation Models
###################################################################################
#######################################
# 7. Divide your data into training and testing datasets, Experiment with 'split' and 'cross-validation' evaluation schemes
#######################################
# split_scheme with train/test(90/10) and goodRating as 4  
split_scheme <- evaluationScheme(beer_matrix, method = "split", train = 0.9,
                            k = 1, given = -1, goodRating = 4)
split_scheme
# cross_validation_scheme with goodRating as 4  
cross_validation_scheme <- evaluationScheme(beer_matrix, method = "cross-validation",k = 5, given = -1, goodRating = 4)
cross_validation_scheme

#######################################
# 8. Build IBCF and UBCF models
#######################################

# Creation of the model - U(ser) B(ased) C(ollaborative) F(iltering)
algorithms <- list(
  "user-based Collaborative Filtering" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=50,minRating=4)),
  "item-based Collaborative Filtering" = list(name="IBCF", param=list(normalize = "Z-score")))


split_scheme_results <- evaluate(split_scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(split_scheme_results)

cross_validation_scheme_results <- evaluate(cross_validation_scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(cross_validation_scheme_results)

#######################################################################################
# 9. Compare the performance of the two models and suggest the one that should be deployed
#######################################################################################
#Drawing ROC curve
plot(split_scheme_results, annotate = 1:4, legend="topleft")  
plot(cross_validation_scheme_results, annotate = 1:4, legend="topleft")
# from both the plots we can say that UBCF seems  better then IBCF

#######################################################################################
# 10. Give the names of the top 5 beers that you would recommend to the users "cokes", "genog" & "giblet"
#######################################################################################

#ubcf_model<-Recommender(beer_matrix[1:500], method = "UBCF")
ubcf_model <- Recommender(beer_matrix, method = "UBCF") 
ubcf_model

recommend_cokes <- predict(ubcf_model, beer_matrix['cokes'], n=5)
as(recommend_cokes, "list") # recommended values "7971"  "47658" "1158"  "1717"  "1339" 

recommend_genog <- predict(ubcf_model, beer_matrix['genog'], n=5)
as(recommend_genog, "list")  # recommended values "2093" "6075" "571"  "782"  "932" 

recommend_giblet <- predict(ubcf_model, beer_matrix['giblet'], n=5)
as(recommend_giblet, "list") # recommended values "7971"  "1545"  "34420" "29619" "19960"

