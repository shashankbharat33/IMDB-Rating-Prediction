# Include all libraries
library(ggplot2)

movie.df<-read.csv("movie_metadata.csv",header=TRUE)

dim(movie.df)
options(scipen=999)
#There are in total 5043 records in the dataset  with 28 different variables.
str(movie.df)
t(t(colnames(movie.df)))
movie.num <- movie.df[,c(3:6,8,9,13,14,16,19,23,25,26,27,28)]
movie.cat <-movie.df[,-c(3:6,8,9,13,14,16,19,23,25,26,27,28)]
summary(movie.cat)
data.frame(mean=round(sapply(movie.num, mean, na.rm = TRUE),0), 
           min=round(sapply(movie.num, min,na.rm = TRUE),0), 
           max=round(sapply(movie.num, max,na.rm = TRUE),0), 
           median=round(sapply(movie.num, median,na.rm = TRUE),0), 
           miss.val=sapply(movie.num, function(x) 
             sum(length(which(is.na(x))))))

#checking for duplicate rows if any
sum(duplicated(movie.df))
#there are 45 rows which are duplicate we are omitting the duplicate rows.
movie.df <- movie.df[!duplicated(movie.df), ]
dim(movie.df)

#Analysis of variables
plot(movie.df$language,xlab = "Language", ylab="Movies", main ="By Language", ylim =c(0,5000))
#Only English is most viewed language among all languages. We can ignore this variable as it is not useful for our anlaysis
plot(movie.df$color, xlab ="Type of Color", ylab ="Movies", main = "By Color",ylim =c(0,5000))
plot(movie.df$content_rating,xlab ="Content Rating", ylab ="Movies", main = "By Content Rating",ylim =c(0,5000))
plot(movie.df$country, xlab ="Country", ylab ="Movies", main = "By Country",ylim =c(0,5000))

data.for.plot <- aggregate(movie.df$imdb_score, by = list(movie.df$content_rating), FUN = sum)
data.for.plot
summary(movie.df$content_rating)
summary(movie.df$country)
levels(movie.df$content_rating)[levels(movie.df$content_rating)%in%c('Approved','Passed','G','TV-G')] <- 'G'
levels(movie.df$content_rating)[levels(movie.df$content_rating)%in%c('PG','PG-13','TV-PG','GP','TV-Y','TV-Y7','TV-14')] <- 'PG'
levels(movie.df$content_rating)[levels(movie.df$content_rating)%in%c('NC-17','R','X','M','TV-MA')] <- 'NC-17'
levels(movie.df$content_rating)[levels(movie.df$content_rating)%in%c('Unrated','Not Rated','')] <- 'Not_Rated'
######################
t((t(colnames(movie.df))))
movie1.df<-movie.df[,c(3,4,5,6,8,9,13,14,19,21,22,23,25,26,27,28)]
movie1.df[movie1.df==""]<-NA
data.frame(miss.val=sapply(movie.df,function(x) 
  sum(length(which(is.na(x))))))
heatmap(1 * is.na(movie1.df), Rowv = NA, Colv = NA)


movie1.df<-movie1.df[!is.na(movie1.df$gross),]
movie1.df<-movie1.df[!is.na(movie1.df$budget),]
data.for.plot <- aggregate(movie.df$aspect_ratio, by = list(movie.df$title_year), FUN = mean,na.rm=TRUE)
movie1.df$aspect_ratio[is.na(movie1.df$aspect_ratio)] <- median(movie1.df$aspect_ratio, na.rm =TRUE)
movie1.df<-movie1.df[!is.na(movie1.df$actor_3_facebook_likes),]
movie1.df<-movie1.df[!is.na(movie1.df$content_rating),]
movie1.df<-movie1.df[!is.na(movie1.df$num_critic_for_reviews),]
data.frame(miss.val=sapply(movie1.df,function(x) 
  sum(length(which(is.na(x))))))
dim(movie1.df)# final dimension of the datset
summary(movie1.df$country)
movie1.df$rating <- .bincode(movie1.df$imdb_score, c(0,5,7,10))
movie1.df$rating <-as.factor(movie1.df$rating)
levels(movie1.df$rating)<-list("low"=1,"medium"=2,"high"=3)
summary(movie1.df$rating)
levels(movie1.df$country)[-c(5,12,21,23,63,65)] <-NA
droplevels(movie1.df$country)
movie1.df<-movie1.df[!is.na(movie1.df$country),]
############################################################
summary(movie1.df)

library(reshape) 

# use melt() to stack a set of columns into a single column of data.
# stack Price values for each combination of (binned) Age and Fuel Type
mlt <- melt(movie1.df, id=c("content_rating", "rating"), measure=c("gross"))
head(mlt, 5)

#library(ggplot2)
#p <- ggplot(data = mlt, aes(x = content_rating, y = value, fill = rating))
#p <- p + geom_bar(stat = "identity", width = 0.5, position = "dodge")
#p <- p + theme_bw()
#p <- p + theme(axis.text.x = element_text(angle = 90))
#p

# use cast() to reshape data and generate pivot table
cast(mlt, content_rating~ rating, subset=variable=="gross", 
     margins=c("grand_row", "grand_col"), mean)


summary(movie1.df[,c(1,2,6,7,9,12,14,15)])

## simple panel of scatterplots
library(GGally)
t(t(colnames(movie1.df)))
ggpairs(movie1.df[,c(1,2,6,7,9,12,14)])
names(movie1.df)<-make.names(names(movie1.df),unique = TRUE)
str(movie1.df)

###########################Clustering #########################################
#=======================clustering=====================================
#content rating, use dummy variables
library(dummies)
str(movie1.df)
movie2.df<-dummy.data.frame(movie1.df)
movie2.df<-movie2.df[,-c(20,25)]
movie2.df<-movie2.df[,-c(10:15,24:26)]

#normalize
library(caret)
movie.norm<-preProcess(movie2.df[,-c(10:13)],method = c("center","scale"))
movie.norm.df <- movie2.df
movie.norm.df[,c(1:9,14:17)]<-predict(movie.norm,movie2.df[,-c(10:13)])
summary(movie.norm.df)
row.names(movie.norm.df)

#compute Euclidean Distance - movie.norm.df
movie.d<- dist(movie.norm.df, method = "euclidean")

#complete method (Euclidean)
movie.cluster <-hclust(movie.d,method="complete")
plot(movie.cluster,hang=-1,ann=FALSE)

#set k=4
moviememb <-cutree(movie.cluster,k=4)

# set labels as cluster membership and utility name
row.names(movie.norm.df) <- paste(moviememb, ": ", row.names(movie1.df), sep = "")

# plot heatmap (Euclidean)
heatmap(as.matrix(movie.norm.df), Colv = NA, hclustfun = hclust,
        col=rev(paste("gray",1:99,sep="")))

#k means
set.seed(211)
moviekm<-kmeans(movie.norm.df,4)
moviekm
#only 33.3%, let's see if we need higher % to achieve heterogeneity between clusters and homogeineity within cluster.

# compute AvgWithinSS for different k
movie.WithinSS.df <- data.frame(k = seq(1, 10, 1), AvgWithinSS = rep(0, 10))
for(i in 1:10) {
  movie.WithinSS.df[i,2] <-  mean(kmeans(movie.norm.df, i)$withinss)
}

plot(movie.WithinSS.df$AvgWithinSS ~ movie.WithinSS.df$k, xlab="k", ylab = "Average WithinSS", lty=2, type="l")
#as shown in the graph, k further than 4 does not minimalize the average within sum squared.
#we will stick with k=4.

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l",
     ylim = c(min(moviekm$centers), max(moviekm$centers)), xlim = c(0, 17))
axis(1, at = c(1:17), labels = names(movie.norm.df))
for (i in c(1:4)){
  lines(moviekm$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                            "black", "dark grey"))}

text(x = 0.5, y = moviekm$centers[, 1], labels = paste("Cluster", c(1:4)))

t(t(colnames(movie.norm.df)))
#Cluster analysis based on graph:
#Cluster 1: Lies between highest(c3) and lowest clusters (C4)
#Cluster 2: High in actor 1,2 & 3 fb likes, cast total fb likes, and content rating pg
#Cluster 3: High in number of voted users, number user for reviews, content rating pg, imdb score and movie fb likes.
#(note: this cluster has the highest IMDB score and Gross)
#Cluster 4: Lowest in most variables, especially lowest in IMDB score and Gross.




#######################Correlation###############################
#Correlation
library(ggcorrplot)
round(cor(movie1.df[,c(1:9,12:16)]),2)
#observations:
#Actor 1 Face book likes and Cast total facebook likes are highly correlated
#Num of user reviews and Num of voted users are highly correlated
# gross and budget also collinear with each other. As the currencies in budget differ we are removing budget
# we can use either one of the variable and delete the other variable
# we are removing cast total fb likes and Num of user for reviews
movie1.df <- movie1.df[,-c(8,9)]
movie1.df <-movie1.df[,-10]
str(movie1.df)

library(corrplot)
correlations <- round(cor(movie1.df[,c(1:7,10:13)]),2)
corrplot(correlations, method="circle")
ggcorrplot(corr,  type = "upper", outline.col = "white", lab = TRUE)

#############################Partition of  Data##############################
set.seed(211)
train.rows<-sample(rownames(movie1.df), dim(movie1.df)[1]*0.5)
valid.rows<-sample(setdiff(rownames(movie1.df), train.rows), dim(movie1.df)[1]*0.3)
test.rows<-setdiff(rownames(movie1.df), union(train.rows, valid.rows))
train.data<-movie1.df[train.rows,];valid.data<-movie1.df[valid.rows,];test.data<-movie1.df[test.rows,]
dim(train.data)
dim(valid.data)
dim(test.data)
str(train.data)

###############################Linear Regression##########################################

# For Linear Regression, Response variable will be "imdb_score", hence removing the column "rating"

# Running the linear regression using all 12 predictors
t(t(colnames(train.data[, -14])))
mvr <- lm(imdb_score ~.,data=train.data[,-14])
options(scipen=999) # avoid scientific notation
summary(mvr)
#Adj R square is 0.3445
accuracy(mvr$fitted.values,train.data$imdb_score)
summary(mvr$fitted.values)
#                               ME      RMSE       MAE       MPE     MAPE
#Test set -0.00000000000000001008085 0.8412661 0.6415981 -2.475292 11.41797

#######################################################
library(forecast)
pred.valid.mvr <- predict(mvr, newdata = valid.data[,-14])
valid.res <- data.frame(valid.data$imdb_score, pred.valid.mvr, residuals = 
                          valid.data$imdb_score - pred.valid.mvr)
head(valid.res)
hist(valid.res$residuals)
plot(valid.res$residuals)
sd(valid.res$residuals)
mean(valid.res$residuals)
summary(valid.res)
# compute accuracy on validation data
accuracy(pred.valid.mvr, valid.data$imdb_score)

#                 ME      RMSE       MAE       MPE     MAPE
#Test set 0.04968232 0.8546566 0.6549334 -1.803692 11.58426


#on test data
pred.test.mvr <- predict(mvr, newdata = test.data[,-14])
test.res <- data.frame(test.data$imdb_score, pred.test.mvr, residuals = 
                         test.data$imdb_score - pred.test.mvr)
# compute accuracy on test data
accuracy(pred.test.mvr, test.data$imdb_score)
#                 ME      RMSE       MAE       MPE     MAPE
#Test set 0.04191114 0.8586888 0.660845 -1.912273 11.73256

###########################################To pick best model########################################

# use regsubsets() in package leaps to run an exhaustive search. 
# unlike with lm, categorical predictors must be turned into dummies manually. May not be needed in newer versions
library(leaps)
str(train.data)
search <- regsubsets(imdb_score ~ ., data = train.data[,-14], nbest = 1, nvmax = dim(train.data[,-14])[2],
                     method = "exhaustive")
sum <- summary(search)
# show models
sum$which
# show metrics
sum$rsq #Bad criteria to use. R-square always increases with number of variables
sum$adjr2
sum$cp
############################################################################

# use step() to run stepwise regression.
imdb.lm.step <- step(mvr, direction = "both")
summary(imdb.lm.step)  # Which variables were dropped/added?
imdb.lm.step.pred <- predict(imdb.lm.step, valid.data[,-14])
accuracy(imdb.lm.step.pred, valid.data$imdb_score)

#                 ME      RMSE      MAE       MPE     MAPE
#Test set 0.04898452 0.8535308 0.653961 -1.810979 11.56956
########################################################################

#Removed movie fb likes,aspect ratio, actor 3, actor 1 fb likes
mvrmod2 <- lm(imdb_score ~ num_critic_for_reviews + duration +
                num_voted_users +country + content_rating + gross , data = train.data[,-14])
options(scipen=999) # avoid scientific notation
summary(mvrmod2)
#Adj R squared : 0.3388
str(valid.data)
pred.valid.mvrmod2 <- predict(mvrmod2, newdata = valid.data[,-14])
valid.res.mod2 <- data.frame(valid.data$imdb_score, pred.valid.mvrmod2, residuals = 
                               valid.data$imdb_score - pred.valid.mvrmod2)
hist(valid.res.mod2$residuals)
plot(valid.res.mod2$residuals)
accuracy(pred.valid.mvrmod2, valid.data$imdb_score)

#                 ME      RMSE       MAE       MPE     MAPE
#Test set 0.04920471 0.8611151 0.6608178 -1.844395 11.68123

# Accuracy on test data
pred.test.mvrmod2 <- predict(mvrmod2, newdata = test.data[,-14])
accuracy(pred.test.mvrmod2, test.data$imdb_score)

#                 ME      RMSE       MAE       MPE    MAPE
# Test set 0.0413541 0.8537702 0.6598276 -1.926052 11.7067

################################## New movie Prediction ######################################

mvrmod3 <- lm(imdb_score ~ duration  +  actor_1_facebook_likes +
                director_facebook_likes + 
                country + num_voted_users +
                content_rating , data = train.data[,-14])
options(scipen=999) # avoid scientific notation
summary(mvrmod3)
#Adj R squared : 0.3485
pred.valid.mvrmod3 <- predict(mvrmod3, newdata = valid.data[,-14])
valid.res.mod3 <- data.frame(valid.data$imdb_score, pred.valid.mvrmod3, residuals = 
                               valid.data$imdb_score - pred.valid.mvrmod3)
accuracy(pred.valid.mvrmod3, valid.data$imdb_score)
hist(valid.res.mod3$residuals)
plot(valid.res.mod3$residuals)

accuracy(pred.valid.mvrmod3, valid.data$imdb_score)

#                  ME     RMSE       MAE       MPE     MAPE
# Test set 0.05124116 0.8610606 0.6564395 -1.821529 11.61765

#Based on Parsimonus model selected from exhaustive search

# Accuracy metrics on test data:

pred.test.mvrmod3 <- predict(mvrmod3, newdata = test.data[,-14])
accuracy(pred.test.mvrmod3, test.data$imdb_score)

#                  ME      RMSE       MAE       MPE     MAPE
# Test set 0.03166651 0.8517458 0.6533458 -2.093495 11.62699


############################Neural Networks #######################################




library(scales)
library(dummies)
str(train.data)

# Since here our response variable is "Imdb Score" and we are predicting it before the movie is 
# released, we'll use only those predictors that we finalized in our regression model.

train1.data<- dummy.data.frame(train.data[ ,-c(1,4,6,10,12,13,14)], sep =".")
train_nn.data <- as.data.frame(apply(train1.data,2,rescale))
summary(train_nn.data)

valid1.data <- dummy.data.frame(valid.data[,-c(1,4,6,10,12,13,14)], sep =".")
valid_nn.data<-as.data.frame(apply(valid1.data,2,rescale))

test1.data <- dummy.data.frame(test.data[,-c(1,4,6,10,12,13,14)], sep =".")
test_nn.data<-as.data.frame(apply(test1.data,2,rescale))


################################################################################
names(train_nn.data)<-make.names(names(train_nn.data),unique = TRUE)
names(valid_nn.data)<-make.names(names(valid_nn.data),unique = TRUE)
names(test_nn.data)<-make.names(names(test_nn.data),unique = TRUE)

set.seed(211)
library(neuralnet)

#----------------------------------------------------------------------------------

# Creating network using 1 hidden layer with 2 nodes

movie_nn<- neuralnet(imdb_score ~., data = train_nn.data,linear.output=TRUE, hidden = 2)
movie_nn$weights

# plot network
plot(movie_nn, rep="best")
# display predictions
pred.train<-compute(movie_nn,train_nn.data)

#### Comparing the accuracy

library(forecast)
library(caret)

## Accuracy on the training data
pred.train.score <- pred.train$net.result*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
train.score <- (train_nn.data$imdb_score)*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
#head(data.frame(train_nn.data$imdb_score,train.score))

library(ModelMetrics)
rmse(train.score,pred.train.score)
# RMSE on training data: 0.8084767

## Accuracy on validation data
pred.valid.score <- pred.valid$net.result*(max(valid1.data$imdb_score)-min(valid1.data$imdb_score))+min(valid1.data$imdb_score)
valid.score <- (valid_nn.data$imdb_score)*(max(valid1.data$imdb_score)-min(valid1.data$imdb_score))+min(valid1.data$imdb_score)
head(data.frame(pred.valid.score,valid.score))

rmse(valid.score,pred.valid.score)
# RMSE on validation data: 0.9559723


mae(valid.score,pred.valid.score)
# MAE on validation data: 0.763728



#---------------------------------------------------------------------------------

# Creating network using 1 hidden layer with 3 nodes

movienn_1_3<- neuralnet(imdb_score ~ ., data = train_nn.data, linear.output = T, hidden = 3)
movienn_1_3$weights

# plot network
plot(movienn_1_3, rep="best")

pred1_3.train<-compute(movienn_1_3,train_nn.data)
pred1_3.valid<-compute(movienn_1_3,valid_nn.data)
pred1_3.test<-compute(movienn_1_3,test_nn.data)

## Accuracy on the training data
pred1_3.train.score <- pred1_3.train$net.result*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
train1_3.score <- (train_nn.data$imdb_score)*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)

rmse(train1_3.score,pred1_3.train.score)
# RMSE on training data: 0.8015041

## Accuracy on validation data

pred1_3.valid.score <- pred1_3.valid$net.result*(max(valid1.data$imdb_score)-min(valid1.data$imdb_score))+min(valid1.data$imdb_score)
valid1_3.score <- (valid_nn.data$imdb_score)*(max(valid1.data$imdb_score)-min(valid1.data$imdb_score))+min(valid1.data$imdb_score)

head(data.frame(pred1_3.valid.score,valid1_3.score))

##RMSE on validation data 
rmse(valid1_3.score,pred1_3.valid.score)
mae(valid1_3.score,pred1_3.valid.score)

# RMSE: 0.9755903
# MAE: 0.7917165


## Accuracy on test data
pred1_3.test.score <- pred1_3.test$net.result*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
test1_3.score <- (test_nn.data$imdb_score)*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)

head(data.frame(pred1_3.test.score,test1_3.score))

##RMSE on test data 
rmse(test1_3.score,pred1_3.test.score)
mae(test1_3.score,pred1_3.test.score)

# RMSE: 0.8878733
# MAE: 0.6772107


#------------------------------------------------------------------------------------------------------


# Creating network using 1 hidden layer with 4 nodes

movienn_1_4<- neuralnet(imdb_score ~ ., data = train_nn.data, linear.output = T, hidden = 4)
movienn_1_4$weights

# plot network
plot(movienn_1_4, rep="best")

pred1_4.train<-compute(movienn_1_4,train_nn.data)
pred1_4.valid<-compute(movienn_1_4,valid_nn.data)
pred1_4.test<-compute(movienn_1_4,test_nn.data)

## Accuracy on the training data
pred1_4.train.score <- pred1_4.train$net.result*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
train1_4.score <- (train_nn.data$imdb_score)*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
###RMSE on train data
rmse(train1_4.score,pred1_4.train.score)
# RMSE: 0.7939452

## Accuracy on validation data

pred1_4.valid.score <- pred1_4.valid$net.result*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
valid1_4.score <- (valid_nn.data$imdb_score)*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)

head(data.frame(valid_nn.data$imdb_score,valid1_4.score))

##RMSE on validation data 
rmse(valid1_4.score,pred1_4.valid.score)
mae(valid1_4.score,pred1_4.valid.score)

# RMSE: 1.080024
# MAE: 0.8088769


## Accuracy on test data
pred1_4.test.score <- pred1_4.test$net.result*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
test1_4.score <- (test_nn.data$imdb_score)*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)

head(data.frame(pred1_4.test.score,test1_4.score))

##RMSE on test data 
rmse(test1_4.score,pred1_4.test.score)
mae(test1_4.score,pred1_4.test.score)

# RMSE: 0.9746632
# MAE: 0.7078741

#------------------------------------------------------------------------------------------------

# Creating network using 2 hidden layers with 2 nodes each

movienn_2_2<- neuralnet(imdb_score ~ ., data = train_nn.data, linear.output = T, hidden = c(2,2))
movienn_2_2$weights

# plot network
plot(movienn_2_2, rep="best")

pred2_2.train<-compute(movienn_2_2,train_nn.data)
pred2_2.valid<-compute(movienn_2_2,valid_nn.data)
pred2_2.test<-compute(movienn_2_2,test_nn.data)
## Accuracy on the training data
pred2_2.train.score <- pred2_2.train$net.result*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
train2_2.score <- (train_nn.data$imdb_score)*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
###RMSE on train data
rmse(train2_2.score,pred2_2.train.score)
# RMSE: 0.8051397

## Accuracy on validation data
pred2_2.valid.score <- pred2_2.valid$net.result*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
valid2_2.score <- (valid_nn.data$imdb_score)*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
##RMSE on validation data 
rmse(valid2_2.score,pred2_2.valid.score)
mae(valid2_2.score,pred2_2.valid.score)

# Accuracy from network with 2 hidden layers and 4 nodes each.
# RMSE: 0.9431654
# MAE: 0.7529754

## Accuracy on test data
pred2_2.test.score <- pred2_2.test$net.result*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
test2_2.score <- (test_nn.data$imdb_score)*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
##RMSE on test data 
rmse(test2_2.score,pred2_2.test.score)
mae(test2_2.score,pred2_2.test.score)

# Accuracy on test data from network with 2 hidden layers and 4 nodes each.
# RMSE: 0.9032142
# MAE:  0.698787


#----------------------------------------------------------------------------------------------------------

# Creating network using 2 hidden layers with 4 nodes each

movienn_2_4<- neuralnet(imdb_score ~ ., data = train_nn.data, linear.output = T, hidden = c(4,4))
movienn_2_4$weights

# plot network
plot(movienn_2_4, rep="best")

pred2_4.train<-compute(movienn_2_4,train_nn.data)
pred2_4.valid<-compute(movienn_2_4,valid_nn.data)
pred2_4.test<-compute(movienn_2_4,test_nn.data)
## Accuracy on the training data
pred2_4.train.score <- pred2_4.train$net.result*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
train2_4.score <- (train_nn.data$imdb_score)*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
###RMSE on train data
rmse(train2_4.score,pred2_4.train.score)
# RMSE: 0.787909

## Accuracy on validation data
pred2_4.valid.score <- pred2_4.valid$net.result*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
valid2_4.score <- (valid_nn.data$imdb_score)*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
##RMSE on validation data 
rmse(valid2_4.score,pred2_4.valid.score)
mae(valid2_4.score,pred2_4.valid.score)

# Accuracy from network with 2 hidden layers and 4 nodes each.
# RMSE: 0.9758006
# MAE: 0.7834324

## Accuracy on test data
pred2_4.test.score <- pred2_4.test$net.result*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
test2_4.score <- (test_nn.data$imdb_score)*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
##RMSE on test data 
rmse(test2_4.score,pred2_4.test.score)
mae(test2_4.score,pred2_4.test.score)

# Accuracy on test data from network with 2 hidden layers and 4 nodes each.
# RMSE: 0.9035779
# MAE:  0.6906065


#---------------------------------------------------------------------------------

# Creating network using 3 hidden layers with 4 nodes each

movienn_4_4_4<- neuralnet(imdb_score ~ ., data = train_nn.data, linear.output = T, hidden = c(4,4,4))
movienn_4_4_4$weights

# plot network
plot(movienn_4_4_4, rep="best")

pred4_4_4.train<-compute(movienn_4_4_4,train_nn.data)
pred4_4_4.valid<-compute(movienn_4_4_4,valid_nn.data)
pred4_4_4.test<- compute(movienn_4_4_4,test_nn.data)
## Accuracy on the training data
pred4_4_4.train.score <- pred4_4_4.train$net.result*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
train4_4_4.score <- (train_nn.data$imdb_score)*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
#RMSE on train data
rmse(train4_4_4.score,pred4_4_4.train.score)
# RMSE: 0.7878875


## Accuracy on validation data
pred4_4_4.valid.score <- pred4_4_4.valid$net.result*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
valid4_4_4.score <- (valid_nn.data$imdb_score)*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
##RMSE on validation data 
rmse(valid4_4_4.score,pred4_4_4.valid.score)
mae(valid4_4_4.score,pred4_4_4.valid.score)

# Accuracy from network with 3 hidden layers and 4 nodes each.
# RMSE: 0.9734486
# MAE:  0.7773902


## Accuracy on test data
pred4_4_4.test.score <- pred4_4_4.test$net.result*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)
test4_4_4.score <- (test_nn.data$imdb_score)*(max(train1.data$imdb_score)-min(train1.data$imdb_score))+min(train1.data$imdb_score)

head(data.frame(test_nn.data$imdb_score,test4_4_4.score))

##RMSE on test data 
rmse(test4_4_4.score,pred4_4_4.test.score)
mae(test4_4_4.score,pred4_4_4.test.score)

# RMSE: 0.9551679
# MAE:  0.7258534

###########################################################################


###########################################################################

# scaling
#library(caret)
#norm.values <- preProcess(train.data[, ], method=c("center", "scale"))
#head(norm.values)
#train.norm.df[, 1:2] <- predict(norm.values, train.df[, 1:2])
#head(train.norm.df)
##Similarly scale valid data, mower data and the new data
#valid.norm.df <- predict(norm.values, valid.df[, 1:2])
#mower.norm.df <- predict(norm.values, mower.df[, 1:2])
#new.norm.df <- predict(norm.values, new.df[, 1:2])
#head(mower.norm.df)

############################### Common data###############################
###########################################################
#classification using Logit model
t(t(colnames(train.data)))
train1.data <-train.data[,-11]
valid1.data <-valid.data[,-11]
test1.data <-test.data[,-11]
library(MASS)
head(train1.data)
train1.data$rating<- relevel(train1.data$rating, ref = "low")
#train1.data$rating<- relevel(train1.data$rating, ref = 3)
multinom.fit <- polr(rating ~ ., data = train1.data, Hess = TRUE) 
multinom.fit
#summary(multinom.fit)
#exp(coef(multinom.fit))
head(probability.table <- fitted(multinom.fit))
# Predicting the values for valid dataset
str(valid1.data)
pred1<-predict(multinom.fit,valid1.data[,-c(13)],type = "class")
confusionMatrix(pred1,valid1.data$rating)
head(pred1)
head(valid.data)
#pred1 <- predict(multinom.fit, valid1.data[,-c(13)])
# first 5 actual and predicted records from valid data
#data.frame(actual.rating = valid.data$rating[1:5], predicted.rating = valid.data$pred[1:5])
#valid.data$rating <-as.numeric(valid.data$rating)
#valid.data$pred <-as.numeric( valid.data$pred)
#bankgain <- gains(valid.data$rating, valid.data$pred, groups=10)
#bankgain
#str(valid.data)
library(caret)

confusionMatrix(pred1,valid1.data$rating)
testpred <- predict(multinom.fit, newdata = test1.data)
confusionMatrix(testpred,test1.data$rating)


############################################

# finding best model using glmulti

#library(glmulti)
library(caret)
#movie.glmulti<-glmulti(multinom.fit,name = "glmulti.analysis", intercept = TRUE, 
#                    marginality = FALSE, bunch=5, level = 1,method = "h", crit = "aic")

#library(glmulti)
#str(train1.data)
#prednames <- c("num_critic_for_reviews","duration","director_facebook_likes",
#              "actor_1_facebook_likes","gross","num_voted_users","num_user_for_reviews",
#             "content_rating","movie_facebook_likes")
#g1 <- glmulti("multinom.fit",xr= prednames,data=train1.data,level=1, fitfunction = "multinom")

#View(train1.data)

#summary(valid1.data$rating)
#After 1050 models:
#Best model: rating~1+content_rating+duration+director_facebook_likes+
#  actor_1_facebook_likes+gross+num_voted_users+num_user_for_reviews+movie_facebook_likes

multinom1.fit <- polr(rating ~ duration + director_facebook_likes + actor_1_facebook_likes + 
                        gross + num_voted_users + 
                        country + content_rating + actor_2_facebook_likes + aspect_ratio + 
                        movie_facebook_likes, data = train1.data , Hess = TRUE)

pred2 <- predict(multinom1.fit, newdata = valid1.data[,-c(13)])
confusionMatrix(pred2,valid1.data$rating)

####################################################################
library(caret)
# use step() to run stepwise regression.
movie.lm.step <- step(multinom.fit, direction = "both")
summary(movie.lm.step)  # Which variables were dropped/added?
movie.lm.step.pred <- predict(movie.lm.step, valid1.data)
confusionMatrix(movie.lm.step.pred, valid1.data$rating)

######################################################################

#################Logit for predictors that are available before movie release
multinom1.fit <- polr(rating ~ duration + director_facebook_likes + actor_1_facebook_likes + 
                        num_voted_users +  
                        country + content_rating +  
                        movie_facebook_likes, data = train1.data)

pred2 <- predict(multinom1.fit, newdata = valid1.data[,-c(13)])
confusionMatrix(pred2,valid1.data$rating)


#for test data
pred3 <- predict(multinom1.fit, newdata = test1.data[,-c(13)])
confusionMatrix(pred3,test1.data$rating)


######################knn for all predictors #####################
str(train1.data)
#8,9 and 13 are factors
knntrain.norm <- train1.data
knnvalid.norm <-valid1.data
knntest.norm<-test1.data

# for variables 

library(caret)
knnnorm.values <- preProcess(train1.data[,c(1:7,10:12)], method=c("center", "scale"))
head(knnnorm.values)
knntrain.norm[,c(1:7,10:12)] <- predict(knnnorm.values, train1.data[,c(1:7,10:12)])
head(knntrain.norm)
##Similarly scale valid data, mower data and the new data
knnvalid.norm[,c(1:7,10:12)] <- predict(knnnorm.values, valid1.data[,c(1:7,10:12)])
knntest.norm[,c(1:7,10:12)] <- predict(knnnorm.values, test1.data[,c(1:7,10:12)])
#new.norm.df <- predict(norm.values, new.df[, 1:2])
#head(mower.norm.df)
knntrain.norm<-dummy.data.frame(data=knntrain.norm,names=c("country","content_rating")
                                ,omit.constants=FALSE,dummy.classes=getOption("dummy.classes"),all=TRUE)

knnvalid.norm<-dummy.data.frame(data=knnvalid.norm,names=c("country","content_rating")
                                ,omit.constants=FALSE,dummy.classes=getOption("dummy.classes"),all=TRUE)

knntest.norm<-dummy.data.frame(data=knntest.norm,names=c("country","content_rating")
                               ,omit.constants=FALSE,dummy.classes=getOption("dummy.classes"),all=TRUE)



#2,3,5,7,8,10,11
# knn() is available in library FNN (provides a list of the nearest neighbors) 
library(FNN)
movieknn <- knn(train = knntrain.norm[,c(1:20)], test = knnvalid.norm[,1:20], 
                cl = knntrain.norm[,21], k = 3) #we are predicting the thrid column (the last input in cl) using k=3

#row.names(train1.data)[attr(Mowersknn, "nn.index")]##Display the row names
#train.df[attr(Mowersknn, "nn.index"),]

summary(movieknn) #The final predicted class

confusionMatrix(movieknn, valid1.data[, 13])

# initialize a data frame with two columns: k, and accuracy.
movieaccuracy.df <- data.frame(k = seq(1, 30, 1), accuracy = rep(0, 30)) #rep just repeats a value (0 in this case) 14 times. We are just initiating accuracy
movieaccuracy.df

# compute knn for different k on validation.
for(i in 1:30) {
  #Use knn function with k=i and predict for valid dataset
  movieknn.pred <- knn(train = knntrain.norm[,c(1:20)], test = knnvalid.norm[,1:20], 
                       cl = knntrain.norm[,21], k = i)
  movieaccuracy.df[i, 2] <- confusionMatrix(movieknn.pred, valid1.data[, 13])$overall[1] 
}

movieaccuracy.df
# k -10 and accuracy is 66% for valid data



#####################################KNN with variables available for new data
t(t(colnames(knntrain.norm)))

knntrain1.norm <- knntrain.norm[,-c(1,4,6,18,19)]
knnvalid1.norm <-knnvalid.norm[,-c(1,4,6,18,19)]
knntest1.norm<-knntest.norm[,-c(1,4,6,18,19)]

# for variables 
str(knntrain1.norm)
movieknn1 <- knn(train = knntrain1.norm[,c(1:15)], test = knnvalid1.norm[,c(1:15)], 
                 cl = knntrain1.norm[,16], k = 10) #we are predicting the thrid column (the last input in cl) using k=3

#row.names(train1.data)[attr(Mowersknn, "nn.index")]##Display the row names
#train.df[attr(Mowersknn, "nn.index"),]

summary(movieknn1) #The final predicted class

confusionMatrix(movieknn1, knnvalid1.norm[,16])

# initialize a data frame with two columns: k, and accuracy.
movieaccuracy1.df <- data.frame(k = seq(1, 50, 1), accuracy = rep(0, 50)) #rep just repeats a value (0 in this case) 14 times. We are just initiating accuracy
movieaccuracy1.df

# compute knn for different k on validation.
for(i in 1:50) {
  #Use knn function with k=i and predict for valid dataset
  movieknn1.pred <- knn(train = knntrain1.norm[,c(1:15)], test = knnvalid1.norm[,c(1:15)], 
                        cl = knntrain1.norm[,16], k = i)
  movieaccuracy1.df[i, 2] <- confusionMatrix(movieknn1.pred, knnvalid1.norm[, 16])$overall[1] 
}

movieaccuracy1.df


#k =10 for new movie available variables accuracy is 65%


