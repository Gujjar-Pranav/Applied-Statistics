#CW_01
#1st step: 
#Create 1st vector with name Time_Measured_min.

Time_Measured_min <-c(5,10,12.5,7,5.5,6.4,4,4.2,5.1,2,2.5,3.5,3,4.5,3.2)

# create 2nd vector with name Muscial_genre and
#in this vector use repeat function to match with value 
#of Time_Measured_min.

Musical_genre <-c(rep("classical music",3),rep("jazz",3),rep("blues",3),rep("reggae",3),rep("heavy metal",3))

#2nd step:
#Create dataframe 

Musical_Genre_DataFrame <-data.frame(Time_Measured_min,Musical_genre)

#3rd step:
#Measured time in min is in ascending order 

df <-Musical_Genre_DataFrame[order(Musical_Genre_DataFrame$Time_Measured_min),]
df

#CW1_ANS_2.a

# 1st step:
# read "wallaby-nr-47.csv" dataset by using read.csv command and 
# check variables and values in dataset by using head command

data <- read.csv("D:/Pranav -UK/Applied Statistics - R language/CW-01/wallaby-nr-47.csv")
head(data)

#2nd step:
# construct a vector p that consists of the 3rd, 5th, and 7th variables of the dataset.

p <-data[,c(3,5,7)]

#3rd step:
#Using p, list the names of the variables.

list <-variable.names(p)
list

#CW1_ANS_2.b_i

# the mean, median, max, and min values of the variable Ear
ear_mean <-mean(data$Ear)
ear_mean

ear_median<-median(data$Ear)
ear_median

ear_max <-max(data$Ear)
ear_max

ear_min <-min(data$Ear)
ear_min

#CW1_ANS_2.b_ii

#To find the time point in the study when ear length is above the mean
#but below the median ear length , there is no specific time point variable given in data set. 
# Only available variable is 'Age' in dataset. So we can consider 'Leng' variable for time point 
# in our further analysis

ear_d <- data$Ear
Leng_time_point <- data$Leng[ear_d > ear_mean & ear_d < ear_median]
Leng_time_point

#CW1_ANS_2.c_i
#create Notched Box and Wishker plot for variable name length
boxplot(data$Leng,notch = TRUE,names = c("length"),
        col = 'blue',main="Notched Box and Whisker Plot - Leng",
        xlab="Variable_Leng",ylab="Measurement")
summary(data$Leng)
#create Notched Box and Wishker plot for variable name leg
boxplot(data$Leg,notch = TRUE,names = c("leg"),
        col = 'blue',main="Notched Box and Whisker Plot - Leg",
        xlab="Variable_Leg",ylab="Measurement")
summary(data$Leg)


#create Notched Box and Whisker plot for variable name leg and leng
boxplot(data$Leng,data$Leg,notch = TRUE,names = c("length","leg"),
        col = 'blue',main="Notched Box and Whisker Plot",
        xlab="Variable",ylab="Measurement")

#CW1_ANS_2.c_ii

#Interpretation of notched box and whisker plot 
#for variable names length and leg given in pdf.
 
#CW1_ANS_2.d

# create scatterplot - leg length vs length
plot(data$Leg,data$Leng,pch=4,xlab = "Leg_Length",ylab = "Length",
     col='red',main='Leg_Length vs Length')

#CW1_ANS_2.e

#1st Step:
#Simple linear regression model for leng and leg variables.
length <-data$Leng
leg_length <-data$Leg
model <-lm(length ~ leg_length)

#2nd step:
#create scatterplot for simple linear regession model.
plot(data$Leg,data$Leng,pch=4,xlab = "Leg_Length",ylab = "Length",
     ,col='red',main='Leg_Length vs Length')

#3rd step:
# Draw simple linear regression line.
abline(model,col='blue')

#CW1_ANS_2.f
# Predict value from Simple linear regression model for leng and leg variables.
model_fit_value <-predict(model)
model_fit_value
#findout observed value >=18mm from predict value.
indices_values <-which((data$Leng-model_fit_value)>=18)
indices_values
weight_of_wallaby <-data$Weight[indices_values]
# Print out weight of wallaby 
weight_of_wallaby

#CW1_ANS_2.g

#1st step:
#fit linear regression model for leng (variable) and leg(variable)
# r means residuals
r_model <-lm(data$Leng ~ data$Leg)

#2nd step:
#calculate residuals
r <- resid(r_model)
r_table <-data.frame(residuals=r)
r_table

#3rd step:
#scatter plot for leg_length vs length with residual
plot(data$Leg,data$Leng,pch=4,xlab = 'Leg_length',ylab = 'Length',col='red',main='Leg_Length vs Length with residuals')
abline(r_model,col='blue')
segments(data$Leg,data$Leng,data$Leg,data$Leng-r,pch=25,col='black')

#CW1_ANS_2.h

#findout correlation between leng and leg by formula.
X_bar <-mean(data$Leng)
X_bar
Y_bar <-mean(data$Leg)
Y_bar
Leng_stddev <-sd(data$Leng)
Leng_stddev
Leg_stddev <-sd(data$Leg)
Leg_stddev
Denominator <- (Leng_stddev*Leg_stddev)
Denominator
Covariance <- cov(data$Leng,data$Leg)
Covariance
Covariance_coefficient <-(Covariance/Denominator)
Covariance_coefficient

#CW1_ANS_2.i

#create Histogram of weight with 7bin.
hist(data$Weight,breaks = 7,col='blue',border = 'red',
     main='Histogram of Weight', xlab = 'Weight')

#CW1_ANS_2.j

#1st step:
#Create a vector to store the weight and age when weight loss is observed
weight_age_loss <-c()

#2nd step:
# make starting variable to store the previous weight and age
#p means previous
p_weight <- data$Weight[1]
p_age <- data$age[1]

#3rd step
#iterate through the data
#c means Current
for (i in 2:length(data$Weight)) {
  c_weight <-data$Weight[i]
  c_age <- data$Age[i]
  
# Check if the current weight is less than the previous weight
  if(c_weight<p_weight) {
    weight_age_loss[[length(weight_age_loss)+1]] <-c(c_weight,c_age)
  }
  
#update the previous weight and age
  p_weight <- c_weight
  p_age <-c_age
}

#4th step:
#print the vector containing weight and age when weight loss is observed
for (i in 1:length(weight_age_loss)){
  cat("At the point",i,"weight:",weight_age_loss[[i]][1],
      "age:",weight_age_loss[[i]][2],"\n")
}

#CW1_ANS_2.k

#To check assumption of variable Head is normally distributed, we make Q-Q plot.

#In addition, we can perform Q-Q Plot 
#to ensure that our assumption about Variable Head is normally distributed.
qqnorm(data$Head)
qqline(data$Head)

#CW1_ANS_2.l_i

#1st step:
#calculate mean and Standard deviation from variable Head.
head_mean <- mean(data$Head)
head_sd <-sd(data$Head)
Lower_value <- 1090
Upper_value <- 1164

#2nd step:
#Calculate Z_score for Lower_value and Upper_value
z_lower <-(Lower_value - head_mean) / head_sd
z_upper <-(Upper_value - head_mean) / head_sd

#3rd step:
#calculate cumulative probabilities and probability
c_p_lower <-pnorm(z_lower)
c_p_upper <-pnorm(z_upper)
probalility_between_values <- c_p_upper - c_p_lower
cat("Probability that Head is between 1090mm and 1164mm:", probalility_between_values, "\n")

#CW1_ANS_2.l_ii

#1st step:
#calculate Z score for the 30th percentile( 40% inside the interval) 
#and 70th percentile(60% outside the interval) 
#of a standard normal distribution
mean_Head <- mean(data$Head)
sd_Head <-sd(data$Head)
p_30th <- 0.30
p_70th <- 0.70

#2nd step:
#calculate z score for 30th and 70th percentile
zs_30th <- qnorm(p_30th)
zs_70th <- qnorm(p_70th)

#3rd step:
#calculate values for Head variable
V_30th <- mean_Head + zs_30th * sd_Head
V_70th <- mean_Head + zs_70th * sd_Head

#4th step:
#Print the values at 30th and 70th percentile.
cat("Value for 40% inside:", V_30th, "mm\n")
cat("Value for 60% outside:", V_70th, "mm\n")

#CW1_ANS_3

#1st step:
#Using Bayes' theoram
#p means Probability 
#r means red car
#g means green car
#f means faulty 
prob_redcar <- 0.6 # red car percentage
prob_greencar <- 0.4 # green car percentage
prob_faulty_redcar <- 0.015 # faulty red car percentage
prob_faulty_greencar <- 0.035 # falulty green car percentage

# probability for redcar included faulty redcars
prob_redcar_total <- prob_redcar*prob_faulty_redcar 
prob_redcar_total

#Probalility for greencar included faulty greencars
prob_greencar_total <- prob_greencar*prob_faulty_greencar 
prob_greencar_total

#calculate using the law of probability for faulty cars
prob_total_faultycars <-prob_redcar_total + prob_greencar_total
prob_total_faultycars
#2nd step:
#calculate probabilities for given red car which is faulty
prob_redcar_faulty <- (prob_faulty_redcar*prob_redcar)/prob_total_faultycars
cat("Probability that the given red car which is faulty (P(redcar|faulty)):", prob_redcar_faulty, "\n")
#calculate probabilities for given green car which is faulty
prob_greencar_faulty <-(prob_faulty_greencar*prob_greencar)/prob_total_faultycars
cat("Probability that the given green car which is faulty (P(green|faulty)):",prob_greencar_faulty, "\n")

#CW1_ANS_4_i

#Use Poisson Distribution for calculating probability
#because flow occurs randomly and we have details to find out average rate of flow per meter

#calculate average rate of flow per meter 
lambda <- 0.05 # we have details like one flow per 20meters. so flow per meter is 1/20 means 0.05.

#no. of events  we need to calculate probability (exactly 3 flow in 5 meter means k=3)
k<-3

#calculate probability using poisson distribution
probability <- dpois(k,lambda)

#Print probability of exactly 3 flaws in a 5-meter cloth
cat("Probability of exactly 3 flaws in a 5-meter cloth:", probability, "\n")

#CW1_ANS_4_ii

#Use Poisson Distribution for calculating probability
# because flow occurs randomly and we have details to find out average rate of flow per meter

#calculate average rate of flow per meter 
lambda <- 0.05 # we have details like one flow per 20meters. so flow per meter is 1/20 means 0.05.

#no. of events  we need to calculate probability (no flow in 10 meter means k=0)
k<-0

#calculate Probability using poisson distribution
probability <- dpois(k,lambda)

#print probability of no flows in a 10-meter cloth
cat("Probability of no flaws in a 10-meter cloth:", probability, "\n")
