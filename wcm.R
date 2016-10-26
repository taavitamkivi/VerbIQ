setwd("C:/Users/User/Downloads")
wcm_scores = read.csv("output.csv")
head(wcm_scores)


# count of words
a = (aggregate(score ~ data_id * age , data=wcm_scores, FUN=length))
plot(density(a[a$age > 27& is.na(a[ ,3]) == FALSE  ,3],bw = 6), ylim = c(0,0.02),
     main = "Count of words"
     , xlab = "count of words per child")
lines(density(a[a$age < 19 & is.na(a[ ,3]) == FALSE ,3],bw = 6), col = "red")
legend(lty = c(1,1), "topright", 
       legend = c("age: 28-30 months","age: 16-18 months"), 
       col = c(1,2))

# max wcm score
a = (aggregate(score ~ data_id * age , data=wcm_scores, FUN=max))
plot(density(a[a$age > 28& is.na(a[ ,3]) == FALSE  ,3],bw = 0.02),
     main = "Max WCM score"
     , xlab = "max WCM per child")
lines(density(a[a$age < 19 & is.na(a[ ,3]) == FALSE ,3],bw = 0.02), col = "red")
legend(lty = c(1,1), "topleft", 
       legend = c("age: 28-30 months","age: 16-18 months"), 
       col = c(1,2))

# mean wcm score
a = (aggregate(score ~ data_id * age , data=wcm_scores, FUN=mean))
plot(density(a[a$age > 28& is.na(a[ ,3]) == FALSE  ,3],bw = 0.02),
     main = "Mean WCM score"
     , xlab = "mean WCM per child")
lines(density(a[a$age < 19 & is.na(a[ ,3]) == FALSE ,3],bw = 0.02), col = "red")
legend(lty = c(1,1), "topleft", 
       legend = c("age: 28-30 months","age: 16-18 months"), 
       col = c(1,2))

# fit theoretical distribution (currently logistic, but need to find better one)
# on empirical score distribution
fitl <- fitdist(a$score, "logis")

# value of distribution function F(x), i.e. what's the overall ranking of 
# the particular child vs the grand population
plogis(5.25 , location = fitl[1]$estimate[1], scale = fitl[1]$estimate[2])




# variation of wcm score
a = (aggregate(score ~ data_id * age , data=wcm_scores, FUN=var))
plot(density(a[a$age > 28& is.na(a[ ,3]) == FALSE  ,3],bw = 0.02),
     main = "Variation of WCM score"
     , xlab = "variation of WCM per child")
lines(density(a[a$age < 19 & is.na(a[ ,3]) == FALSE ,3],bw = 0.02), col = "red")
legend(lty = c(1,1), "topleft", 
       legend = c("age: 28-30 months","age: 16-18 months"), 
       col = c(1,2))

## function to calculate verbiq components for new child

verbiq_wcm = function(words, age, gender){
  age = 16
  gender = "Female"
  tmp = wcm_scores[wcm_scores$age == age & wcm_scores$sex == gender,]
  (aggregate(score ~ data_id , data=tmp, FUN=max))
  
}

