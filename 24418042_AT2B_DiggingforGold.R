####Digging for Gold####
#Code taken from DADM Block Session 1
#Clear script
rm(list=ls())

#load packages
library(dplyr)
library(readr)

####Income_data####
#Using the triangular distribution, using the inverse transform method.
inv_triangle_cdf_income <- function(P, vmin_inc, vml_inc, vmax_inc){
  
  Pvml_income <- (vml_inc-vmin_inc)/(vmax_inc-vmin_inc)
  
  return(ifelse(P < Pvml_income,
                vmin_inc + sqrt(P*(vml_inc-vmin_inc)*(vmax_inc-vmin_inc)),
                vmax_inc - sqrt((1-P)*(vmax_inc-vml_inc)*(vmax_inc-vmin_inc))))
}

#no of simulation trials
n=1000

#read in income data
gold_income <- read.csv(file="gold_income.csv", stringsAsFactors = F)
str(gold_income)

#set seed for reproducibility
set.seed(11)

#create data frame with rows = number of trials and cols = number of tasks
gold_sim <- as.data.frame(matrix(nrow=n,ncol=nrow(gold_income)))

# for each income/cost
for (i in 1:nrow(gold_income)){
  vmin_inc <- gold_income$min_income_pa[i]
  vml_inc <- gold_income$ml_income_pa[i]
  vmax_inc <- gold_income$max_income_pa[i]

  #generate n random numbers (one per trial)
  prob_sim_inc <- runif(n)
  #simulate n instances of task
  gold_sim[,i] <- inv_triangle_cdf_income(prob_sim_inc,vmin_inc,vml_inc,vmax_inc) 
}

summary(gold_sim$V1)
summary(gold_sim$V3)
summary(gold_sim$V3)
summary(gold_sim$V4)
summary(gold_sim$V5)



####Cost_data####

#Using the triangular distribution, using the inverse transform method.
inv_triangle_cdf_cost <- function(P, vmin_cost, vml_cost, vmax_cost){
  
  Pvml_cost <- (vml_cost-vmin_cost)/(vmax_cost-vmin_cost)
  
  return(ifelse(P < Pvml_cost,
                vmin_cost + sqrt(P*(vml_cost-vmin_cost)*(vmax_cost-vmin_cost)),
                vmax_cost - sqrt((1-P)*(vmax_cost-vml_cost)*(vmax_cost-vmin_cost))))
}

#no of simulation trials
n=1000

#read cost data
cost_data <- read.csv(file="cost_data.csv", stringsAsFactors = F)
str(cost_data)

#set seed for reproducibility
set.seed(15)

#create data frame with rows = number of trials and cols = number of tasks
cost_sim <- as.data.frame(matrix(nrow=n,ncol=nrow(cost_data)))

# for each income/cost
for (i in 1:nrow(cost_data)){
  vmin_cost <- cost_data$min_fix_cost_pa[i]
  vml_cost <- cost_data$ml_fix_cost_pa[i]
  vmax_cost <- cost_data$max_fix_cost_pa[i]
  
  #generate n random numbers (one per trial)
  prob_sim_cost <- runif(n)
  #simulate n instances of task
  cost_sim[,i] <- inv_triangle_cdf_cost(prob_sim_cost,vmin_cost,vml_cost,vmax_cost) 
}

summary(cost_sim$V1)
summary(cost_sim$V3)
summary(cost_sim$V3)
summary(cost_sim$V4)
summary(cost_sim$V5)

####legal_cost####
#Using the triangular distribution, using the inverse transform method.
inv_triangle_cdf_legal <- function(P, vmin_legal, vml_legal, vmax_legal){
  
  Pvml_legal <- (vml_legal-vmin_legal)/(vmax_legal-vmin_legal)
  
  return(ifelse(P < Pvml_legal,
                vmin_legal + sqrt(P*(vml_legal-vmin_legal)*(vmax_legal-vmin_legal)),
                vmax_legal - sqrt((1-P)*(vmax_legal-vml_legal)*(vmax_legal-vmin_legal))))
}

#no of simulation trials
n=1000

#read legal data
legal_cost <- read.csv(file="legal_cost.csv", stringsAsFactors = F)
str(legal_cost)

#set seed for reproducibility
set.seed(17)

#create data frame with rows = number of trials and cols = number of tasks
legal_sim <- as.data.frame(matrix(nrow=n,ncol=nrow(legal_cost)))

# for each legal cost
for (i in 1:nrow(legal_cost)){
  vmin_legal <- legal_cost$min_legal_cost[i]
  vml_legal <- legal_cost$ml_legal_cost[i]
  vmax_legal <- legal_cost$max_legal_cost[i]
  
  #generate n random numbers (one per trial)
  prob_sim_legal <- runif(n)
  #simulate n instances of task
  legal_sim[,i] <- inv_triangle_cdf_legal(prob_sim_legal,vmin_legal,vml_legal,vmax_legal) 
}

summary(legal_sim$V1)
summary(legal_sim$V2)
summary(legal_sim$V3)
summary(legal_sim$V4)
summary(legal_sim$V5)

####YEAR 1 Combine income/ costs into dataframe####
year_1 <- cbind(gold_sim$V1, cost_sim$V1, legal_sim$V1)

year_1_data_frame <- data.frame(year_1, stringsAsFactors = TRUE)

colnames(year_1_data_frame)<- c("income", "cost", "legal")

#Prepare for distribution and pdf
final_year_1 <- year_1_data_frame[,1] - year_1_data_frame[,2] - year_1_data_frame[,3]

hist(final_year_1)
hist(final_year_1, freq = FALSE, main = "Histogram and Density")
dx <- density(final_year_1)
lines(dx, lwd = 2, col = "red")
plot(dx, lwd = 2, col = "red",
     main = "Density")

#mean, max, min and median cost
mean(final_year_1)
max(final_year_1)
min(final_year_1)
median(final_year_1)

#standard deviation
sd(final_year_1)

#plot cdf
plot(ecdf(final_year_1))

#% probabiliity of mean income
quantile(ecdf(final_year_1),.485,type=7)

####YEAR 2 Combine income/ costs into dataframe####
year_2 <- cbind(gold_sim$V2, cost_sim$V2, legal_sim$V2)

year_2_data_frame <- data.frame(year_2, stringsAsFactors = TRUE)

colnames(year_2_data_frame)<- c("income", "cost", "legal")

#Prepare for distribution and pdf
final_year_2 <- year_2_data_frame[,1] - year_2_data_frame[,2] - year_2_data_frame[,3]

hist(final_year_2)
hist(final_year_2, freq = FALSE, main = "Histogram and Density")
dx <- density(final_year_2)
lines(dx, lwd = 2, col = "red")
plot(dx, lwd = 2, col = "red",
     main = "Density")

#mean, max, min and median cost
mean(final_year_2)
max(final_year_2)
min(final_year_2)
median(final_year_2)

#standard deviation
sd(final_year_2)

#plot cdf
plot(ecdf(final_year_2))

#% probabiliity of mean income
quantile(ecdf(final_year_2),.53,type=7)

####YEAR 3 Combine income/ costs into dataframe####
year_3 <- cbind(gold_sim$V3, cost_sim$V3, legal_sim$V3)

year_3_data_frame <- data.frame(year_3, stringsAsFactors = TRUE)

colnames(year_3_data_frame)<- c("income", "cost", "legal")

#Prepare for distribution and pdf
final_year_3 <- year_3_data_frame[,1] - year_3_data_frame[,2] - year_3_data_frame[,3]

hist(final_year_3)
hist(final_year_3, freq = FALSE, main = "Histogram and Density")
dx <- density(final_year_3)
lines(dx, lwd = 2, col = "red")
plot(dx, lwd = 2, col = "red",
     main = "Density")

#mean, max, min and median cost
mean(final_year_3)
max(final_year_3)
min(final_year_3)
median(final_year_3)

#standard deviation
sd(final_year_3)

#plot cdf
plot(ecdf(final_year_3))

#% probabiliity of mean income
quantile(ecdf(final_year_3),.52,type=7)

####YEAR 4 Combine income/ costs into dataframe####
year_4 <- cbind(gold_sim$V4, cost_sim$V4, legal_sim$V4)

year_4_data_frame <- data.frame(year_4, stringsAsFactors = TRUE)

colnames(year_4_data_frame)<- c("income", "cost", "legal")

#Prepare for distribution and pdf
final_year_4 <- year_4_data_frame[,1] - year_4_data_frame[,2] - year_4_data_frame[,3]

hist(final_year_4)
hist(final_year_4, freq = FALSE, main = "Histogram and Density")
dx <- density(final_year_4)
lines(dx, lwd = 2, col = "red")
plot(dx, lwd = 2, col = "red",
     main = "Density")

#mean, max, min and median cost
mean(final_year_4)
max(final_year_4)
min(final_year_4)
median(final_year_4)

#standard deviation
sd(final_year_4)

#plot cdf
plot(ecdf(final_year_4))

#% probabiliity of mean income
quantile(ecdf(final_year_4),.55,type=7)

####YEAR 5 Combine income/ costs into dataframe####
year_5 <- cbind(gold_sim$V5, cost_sim$V5, legal_sim$V5)

year_5_data_frame <- data.frame(year_5, stringsAsFactors = TRUE)

colnames(year_5_data_frame)<- c("income", "cost", "legal")

#Prepare for distribution and pdf
final_year_5 <- year_5_data_frame[,1] - year_5_data_frame[,2] - year_5_data_frame[,3]

hist(final_year_5)
hist(final_year_5, freq = FALSE, main = "Histogram and Density")
dx <- density(final_year_5)
lines(dx, lwd = 2, col = "red")
plot(dx, lwd = 2, col = "red",
     main = "Density")

#mean, max, min and median cost
mean(final_year_5)
max(final_year_5)
min(final_year_5)
median(final_year_5)

#standard deviation
sd(final_year_5)

#plot cdf
plot(ecdf(final_year_5))

#% probabiliity of mean income
quantile(ecdf(final_year_5),.046,type=7)

####Case resolved Year 2####
year_2B <- cbind(gold_sim$V2, cost_sim$V2)

year_2B_data_frame <- data.frame(year_2B, stringsAsFactors = TRUE)

colnames(year_2B_data_frame)<- c("income", "cost")

#Prepare for distribution and pdf
final_year_2B <- year_2B_data_frame[,1] - year_2B_data_frame[,2]

hist(final_year_2B)
hist(final_year_2B, freq = FALSE, main = "Histogram and Density")
dx <- density(final_year_2B)
lines(dx, lwd = 2, col = "red")
plot(dx, lwd = 2, col = "red",
     main = "Density")

#mean, max, min and median cost
mean(final_year_2B)
max(final_year_2B)
min(final_year_2B)
median(final_year_2B)

#standard deviation
sd(final_year_2B)

#plot cdf
plot(ecdf(final_year_2B))

####Case Resolved Year 3####
year_3B <- cbind(gold_sim$V3, cost_sim$V3)

year_3B_data_frame <- data.frame(year_3B, stringsAsFactors = TRUE)

colnames(year_3B_data_frame)<- c("income", "cost")

#Prepare for distribution and pdf
final_year_3B <- year_3B_data_frame[,1] - year_3B_data_frame[,2]

hist(final_year_3B)
hist(final_year_3B, freq = FALSE, main = "Histogram and Density")
dx <- density(final_year_3B)
lines(dx, lwd = 2, col = "red")
plot(dx, lwd = 2, col = "red",
     main = "Density")

#mean, max, min and median cost
mean(final_year_3B)
max(final_year_3B)
min(final_year_3B)
median(final_year_3B)

#standard deviation
sd(final_year_3B)

#plot cdf
plot(ecdf(final_year_3B))

####Case Resolved Year 4####
year_4B <- cbind(gold_sim$V4, cost_sim$V4)

year_4B_data_frame <- data.frame(year_4B, stringsAsFactors = TRUE)

colnames(year_4B_data_frame)<- c("income", "cost")

#Prepare for distribution and pdf
final_year_4B <- year_4B_data_frame[,1] - year_4B_data_frame[,2]

hist(final_year_4B)
hist(final_year_4B, freq = FALSE, main = "Histogram and Density")
dx <- density(final_year_4B)
lines(dx, lwd = 2, col = "red")
plot(dx, lwd = 2, col = "red",
     main = "Density")

#mean, max, min and median cost
mean(final_year_4B)
max(final_year_4B)
min(final_year_4B)
median(final_year_4B)

#standard deviation
sd(final_year_4B)

#plot cdf
plot(ecdf(final_year_4B))

####Case Resolved Year 5####
year_5B <- cbind(gold_sim$V5, cost_sim$V5)

year_5B_data_frame <- data.frame(year_5B, stringsAsFactors = TRUE)

colnames(year_5B_data_frame)<- c("income", "cost")

#Prepare for distribution and pdf
final_year_5B <- year_5B_data_frame[,1] - year_5B_data_frame[,2]

hist(final_year_5B)
hist(final_year_5B, freq = FALSE, main = "Histogram and Density")
dx <- density(final_year_5B)
lines(dx, lwd = 2, col = "red")
plot(dx, lwd = 2, col = "red",
     main = "Density")

#mean, max, min and median cost
mean(final_year_5B)
max(final_year_5B)
min(final_year_5B)
median(final_year_5B)

#standard deviation
sd(final_year_5B)

#plot cdf
plot(ecdf(final_year_5B))

#% probabiliity of mean income
quantile(ecdf(final_year_5B),.46,type=7)

####Future Claim Year 2####
####Income data with future claim####
#Using the triangular distribution, using the inverse transform method.

inv_triangle_cdf_income_future <- function(P, vmin_inc_fut, vml_inc_fut, vmax_inc_fut){
  
  Pvml_income_future <- (vml_inc_fut-vmin_inc_fut)/(vmax_inc_fut-vmin_inc_fut)
  
  return(ifelse(P < Pvml_income_future,
                vmin_inc_fut + sqrt(P*(vml_inc_fut-vmin_inc_fut)*(vmax_inc_fut-vmin_inc_fut)),
                vmax_inc_fut - sqrt((1-P)*(vmax_inc_fut-vml_inc_fut)*(vmax_inc_fut-vmin_inc_fut))))
}

#no of simulation trials
n=1000

#read in income data
gold_income_future <- read.csv(file="gold_income_futureclaim_year2.csv", stringsAsFactors = F)
str(gold_income_future)

#set seed for reproducibility
set.seed(11)

#create data frame with rows = number of trials and cols = number of tasks
gold_sim_future <- as.data.frame(matrix(nrow=n,ncol=nrow(gold_income_future)))

# for each income/cost
for (i in 1:nrow(gold_income_future)){
  vmin_inc_fut <- gold_income_future$min_income_pa[i]
  vml_inc_fut <- gold_income_future$ml_income_pa[i]
  vmax_inc_fut <- gold_income_future$max_income_pa[i]
  
  #generate n random numbers (one per trial)
  prob_sim_inc_fut <- runif(n)
  #simulate n instances of task
  gold_sim_future[,i] <- inv_triangle_cdf_income_future(prob_sim_inc_fut,vmin_inc_fut,vml_inc_fut,vmax_inc_fut) 
}

summary(gold_sim_future$V1)
summary(gold_sim_future$V2)
summary(gold_sim_future$V3)
summary(gold_sim_future$V4)
summary(gold_sim_future$V5)

#Combine with cost and legal
year_2_future <- cbind(gold_sim_future$V2, cost_sim$V2, legal_sim$V2)

year_2_data_future <- data.frame(year_2_future, stringsAsFactors = TRUE)

colnames(year_2_data_future)<- c("income", "cost", "legal")

#Prepare for distribution and pdf
future_final_year_2 <- year_2_data_future[,1] - year_2_data_future[,2] - year_2_data_future[,3]

hist(future_final_year_2)
hist(future_final_year_2, freq = FALSE, main = "Histogram and Density")
dx <- density(future_final_year_2)
lines(dx, lwd = 2, col = "red")
plot(dx, lwd = 2, col = "red",
     main = "Density")

#mean, max, min and median cost
mean(future_final_year_2)
max(future_final_year_2)
min(future_final_year_2)
median(future_final_year_2)

#standard deviation
sd(future_final_year_2)

#plot cdf
plot(ecdf(future_final_year_2))

####YEAR 3 Combine income/ costs into dataframe####
year_3_fut <- cbind(gold_sim_future$V3, cost_sim$V3, legal_sim$V3)

year_3_data_fut <- data.frame(year_3_fut, stringsAsFactors = TRUE)

colnames(year_3_data_fut)<- c("income", "cost", "legal")

#Prepare for distribution and pdf
fut_final_year_3 <- year_3_data_fut[,1] - year_3_data_fut[,2] - year_3_data_fut[,3]

hist(fut_final_year_3)
hist(fut_final_year_3, freq = FALSE, main = "Histogram and Density")
dx <- density(fut_final_year_3)
lines(dx, lwd = 2, col = "red")
plot(dx, lwd = 2, col = "red",
     main = "Density")

#mean, max, min and median cost
mean(fut_final_year_3)
max(fut_final_year_3)
min(fut_final_year_3)
median(fut_final_year_3)

#standard deviation
sd(fut_final_year_3)

#plot cdf
plot(ecdf(fut_final_year_3))

####YEAR 4 Combine income/ costs into dataframe####
year_4_fut <- cbind(gold_sim_future$V4, cost_sim$V4, legal_sim$V4)

year_4_data_fut <- data.frame(year_4_fut, stringsAsFactors = TRUE)

colnames(year_4_data_fut)<- c("income", "cost", "legal")

#Prepare for distribution and pdf
final_year_4_fut <- year_4_data_fut[,1] - year_4_data_fut[,2] - year_4_data_fut[,3]

hist(final_year_4_fut)
hist(final_year_4_fut, freq = FALSE, main = "Histogram and Density")
dx <- density(final_year_4_fut)
lines(dx, lwd = 2, col = "red")
plot(dx, lwd = 2, col = "red",
     main = "Density")

#mean, max, min and median cost
mean(final_year_4_fut)
max(final_year_4_fut)
min(final_year_4_fut)
median(final_year_4_fut)

#standard deviation
sd(final_year_4_fut)

#plot cdf
plot(ecdf(final_year_4_fut))

####YEAR 5 Combine income/ costs into dataframe####
year_5_fut <- cbind(gold_sim_future$V5, cost_sim$V5, legal_sim$V5)

year_5_data_fut <- data.frame(year_5_fut, stringsAsFactors = TRUE)

colnames(year_5_data_fut)<- c("income", "cost", "legal")

#Prepare for distribution and pdf
final_year_5_fut <- year_5_data_fut[,1] - year_5_data_fut[,2] - year_5_data_fut[,3]

hist(final_year_5_fut)
hist(final_year_5_fut, freq = FALSE, main = "Histogram and Density")
dx <- density(final_year_5_fut)
lines(dx, lwd = 2, col = "red")
plot(dx, lwd = 2, col = "red",
     main = "Density")

#mean, max, min and median cost
mean(final_year_5_fut)
max(final_year_5_fut)
min(final_year_5_fut)
median(final_year_5_fut)

#standard deviation
sd(final_year_5_fut)

#plot cdf
plot(ecdf(final_year_5_fut))

####Case Resolved + Future Claim Year 3####

inv_triangle_cdf_future_nl <- function(P, vmin_inc_fut_nl, vml_inc_fut_nl, vmax_inc_fut_nl){
  
  Pvml_income_future_nl <- (vml_inc_fut_nl-vmin_inc_fut_nl)/(vmax_inc_fut_nl-vmin_inc_fut_nl)
  
  return(ifelse(P < Pvml_income_future_nl,
                vmin_inc_fut_nl + sqrt(P*(vml_inc_fut_nl-vmin_inc_fut_nl)*(vmax_inc_fut_nl-vmin_inc_fut_nl)),
                vmax_inc_fut_nl - sqrt((1-P)*(vmax_inc_fut_nl-vml_inc_fut_nl)*(vmax_inc_fut_nl-vmin_inc_fut_nl))))
}

#no of simulation trials
n=1000

#read in income data
gold_income_future_year3 <- read.csv(file="gold_income_futureclaim_year3.csv", stringsAsFactors = F)
str(gold_income_future_year3)

#set seed for reproducibility
set.seed(27)

#create data frame with rows = number of trials and cols = number of tasks
gold_sim_future_nl_y3 <- as.data.frame(matrix(nrow=n,ncol=nrow(gold_income_future_year3)))

# for each income/cost
for (i in 1:nrow(gold_income_future)){
  vmin_inc_fut_nl <- gold_income_future_year3$min_income_pa[i]
  vml_inc_fut_nl <- gold_income_future_year3$ml_income_pa[i]
  vmax_inc_fut_nl <- gold_income_future_year3$max_income_pa[i]
  
  #generate n random numbers (one per trial)
  prob_sim_inc_fut_nl <- runif(n)
  #simulate n instances of task
  gold_sim_future_nl_y3[,i] <- inv_triangle_cdf_future_nl(prob_sim_inc_fut_nl,vmin_inc_fut_nl,vml_inc_fut_nl,vmax_inc_fut_nl) 
}


#Combine with cost
year_3_future_nl <- cbind(gold_sim_future$V3, cost_sim$V3)

year_3_data_nl <- data.frame(year_3_future_nl, stringsAsFactors = TRUE)

colnames(year_3_data_nl)<- c("income", "cost")

#Prepare for distribution and pdf
future_nl_year_3 <- year_3_data_nl[,1] - year_3_data_nl[,2]

hist(future_nl_year_3)
hist(future_nl_year_3, freq = FALSE, main = "Histogram and Density")
dx <- density(future_nl_year_3)
lines(dx, lwd = 2, col = "red")
plot(dx, lwd = 2, col = "red",
     main = "Density")

#mean, max, min and median cost
mean(future_nl_year_3)
max(future_nl_year_3)
min(future_nl_year_3)
median(future_nl_year_3)

#standard deviation
sd(future_nl_year_3)

#plot cdf
plot(ecdf(future_nl_year_3))

####Future Claim + no Legal Year 4####

inv_triangle_cdf_future_nl4 <- function(P, vmin_inc_fut_nl4, vml_inc_fut_nl4, vmax_inc_fut_nl4){
  
  Pvml_income_future_nl4 <- (vml_inc_fut_nl4-vmin_inc_fut_nl4)/(vmax_inc_fut_nl4-vmin_inc_fut_nl4)
  
  return(ifelse(P < Pvml_income_future_nl4,
                vmin_inc_fut_nl4 + sqrt(P*(vml_inc_fut_nl4-vmin_inc_fut_nl4)*(vmax_inc_fut_nl4-vmin_inc_fut_nl4)),
                vmax_inc_fut_nl4 - sqrt((1-P)*(vmax_inc_fut_nl4-vml_inc_fut_nl4)*(vmax_inc_fut_nl4-vmin_inc_fut_nl4))))
}

#no of simulation trials
n=1000

#read in income data
gold_income_future_year4 <- read.csv(file="gold_income_futureclaim_year3.csv", stringsAsFactors = F)
str(gold_income_future_year3)

#set seed for reproducibility
set.seed(29)

#create data frame with rows = number of trials and cols = number of tasks
gold_sim_future_nl4 <- as.data.frame(matrix(nrow=n,ncol=nrow(gold_income_future_year4)))

# for each income/cost
for (i in 1:nrow(gold_income_future)){
  vmin_inc_fut_nl4 <- gold_income_future_year4$min_income_pa[i]
  vml_inc_fut_nl4 <- gold_income_future_year4$ml_income_pa[i]
  vmax_inc_fut_nl4 <- gold_income_future_year4$max_income_pa[i]
  
  #generate n random numbers (one per trial)
  prob_sim_inc_fut_nl4 <- runif(n)
  #simulate n instances of task
  gold_sim_future_nl4[,i] <- inv_triangle_cdf_future_nl4(prob_sim_inc_fut_nl4,vmin_inc_fut_nl4,vml_inc_fut_nl4,vmax_inc_fut_nl4) 
}


#Combine with cost
year_4_future_nl <- cbind(gold_sim_future$V4, cost_sim$V4)

year_4_data_nl <- data.frame(year_4_future_nl, stringsAsFactors = TRUE)

colnames(year_4_data_nl)<- c("income", "cost")

#Prepare for distribution and pdf
future_nl_year_4 <- year_4_data_nl[,1] - year_4_data_nl[,2]

hist(future_nl_year_4)
hist(future_nl_year_4, freq = FALSE, main = "Histogram and Density")
dx <- density(future_nl_year_4)
lines(dx, lwd = 2, col = "red")
plot(dx, lwd = 2, col = "red",
     main = "Density")

#mean, max, min and median cost
mean(future_nl_year_4)
max(future_nl_year_4)
min(future_nl_year_4)
median(future_nl_year_4)

#standard deviation
sd(future_nl_year_4)

#plot cdf
plot(ecdf(future_nl_year_4))

####Future Claim + no Legal Year 5####

inv_triangle_cdf_future_nl5 <- function(P, vmin_inc_fut_nl5, vml_inc_fut_nl5, vmax_inc_fut_nl5){
  
  Pvml_income_future_nl5 <- (vml_inc_fut_nl5-vmin_inc_fut_nl5)/(vmax_inc_fut_nl5-vmin_inc_fut_nl5)
  
  return(ifelse(P < Pvml_income_future_nl5,
                vmin_inc_fut_nl5 + sqrt(P*(vml_inc_fut_nl5-vmin_inc_fut_nl5)*(vmax_inc_fut_nl5-vmin_inc_fut_nl5)),
                vmax_inc_fut_nl5 - sqrt((1-P)*(vmax_inc_fut_nl5-vml_inc_fut_nl5)*(vmax_inc_fut_nl5-vmin_inc_fut_nl5))))
}

#no of simulation trials
n=1000

#read in income data
gold_income_future_year5 <- read.csv(file="gold_income_futureclaim_year3.csv", stringsAsFactors = F)
str(gold_income_future_year5)

#set seed for reproducibility
set.seed(31)

#create data frame with rows = number of trials and cols = number of tasks
gold_sim_future_nl5 <- as.data.frame(matrix(nrow=n,ncol=nrow(gold_income_future_year5)))

# for each income/cost
for (i in 1:nrow(gold_income_future)){
  vmin_inc_fut_nl5 <- gold_income_future_year5$min_income_pa[i]
  vml_inc_fut_nl5 <- gold_income_future_year5$ml_income_pa[i]
  vmax_inc_fut_nl5 <- gold_income_future_year5$max_income_pa[i]
  
  #generate n random numbers (one per trial)
  prob_sim_inc_fut_nl5 <- runif(n)
  #simulate n instances of task
  gold_sim_future_nl5[,i] <- inv_triangle_cdf_future_nl5(prob_sim_inc_fut_nl5,vmin_inc_fut_nl5,vml_inc_fut_nl5,vmax_inc_fut_nl5) 
}


#Combine with cost
year_5_future_nl <- cbind(gold_sim_future$V5, cost_sim$V5)

year_5_data_nl <- data.frame(year_5_future_nl, stringsAsFactors = TRUE)

colnames(year_5_data_nl)<- c("income", "cost")

#Prepare for distribution and pdf
future_nl_year_5 <- year_5_data_nl[,1] - year_5_data_nl[,2]

hist(future_nl_year_5)
hist(future_nl_year_5, freq = FALSE, main = "Histogram and Density")
dx <- density(future_nl_year_5)
lines(dx, lwd = 2, col = "red")
plot(dx, lwd = 2, col = "red",
     main = "Density")

#mean, max, min and median cost
mean(future_nl_year_5)
max(future_nl_year_5)
min(future_nl_year_5)
median(future_nl_year_5)

#standard deviation
sd(future_nl_year_5)

#plot cdf
plot(ecdf(future_nl_year_5))

####Plots and Graphs####

library(ggplot2)

mean_income <- read.csv(file="line_data.csv", stringsAsFactors = F)

ggplot(mean_income, aes(x=year, y=mean_income, colour=Scenario)) +geom_line() +ggtitle("Year vs Mean Income")

scenario_comp <- read.csv(file="bar_chart_data.csv", stringsAsFactors = F)

scenario_comp$scenario <- as.factor(scenario_comp$scenario)
ggplot(scenario_comp, aes(x= year, y= mean_income, fill= scenario, colour= scenario)) + 
  geom_col(position= position_dodge(), width= 0.5, show.legend = T) +
  ggtitle("Scenario 1 vs Scenario 2 Mean Income Comparison")
 

scenario3_comp <- read.csv(file="3A_vs_3B_bar.csv", stringsAsFactors = F)

scenario3_comp$scenario <- as.factor(scenario_comp3$scenario)
ggplot(scenario3_comp, aes(x= year, y= mean_income, fill= scenario, colour= scenario)) + 
  geom_col(position= position_dodge(), width= 0.5, show.legend = T) +
  ggtitle("Scenario 3A vs Scenario 3B Mean Income Comparison")


