options(java.parameters = "-XX:+UseConcMarkSweepGC")
library(xlsx)
library(XLConnect)
library(ggplot2)
rm(list = ls())
options(java.parameters = "-Xmx2048m")
airline <- XLConnect::readTable("C:/Users/Rick/Documents/CRM/Satisfaction-Survey_IBM.xlsx/Satisfaction Survey.xlsx")

airline <- xlsx::read.xlsx(file ="C:/Users/Rick/Documents/CRM/Satisfaction-Survey_IBM.xlsx/Satisfaction Survey.xls", sheetIndex = 1)

airline_new <- read.csv(file = "C:/Users/Rick/Documents/CRM/Satisfaction-Survey_IBM.xlsx/Satisfaction Survey.csv")

str(airline)

airline$ï..Satisfaction <- as.numeric(airline$ï..Satisfaction)
summary(airline$ï..Satisfaction)
#check NA for satisfaction col
sum(is.na(airline$ï..Satisfaction))

#keep only non-NA rows in df airline_clean
#airline_clean <- airline[complete.cases(airline),]

#check if any missing values in df
apply(airline, 2, function(x) any(is.na(x)))

# investogate rows with missing values
head(airline[which(is.na(airline$Arrival.Delay.in.Minutes)),])

# appears that missing values are in columns where flight is cancelled , so NA in cancelled flights
#columns dep delay in mins/ arival delay/ flight duration

# discretise the columns arrival delay/ departure delay

arrivalDeparture_delay_func <- function(col1, col2){
  category <- replicate(length(col2),"No Delay")
  category[is.na(col2) & col1 == 'Yes'] <- "Cancelled"
  category[col2 > 0 & col2 < 5] <- "5 minutes"
  category[col2 >= 5 & col2 < 30] <- "Half hour"
  category[col2 >= 30 & col2 < 60] <- "1 hours"
  category[col2 >= 60 & col2 < 120] <- "2 hours"
  category[col2 >= 120] <- "Greater than 2 hours"
  return (category)
}

airline$Arrival_Delay <- arrivalDeparture_delay_func(airline$Flight.cancelled, airline$Arrival.Delay.in.Minutes)

airline$Departure_Delay <- arrivalDeparture_delay_func(airline$Flight.cancelled, airline$Departure.Delay.in.Minutes)

# REMOVE EXTRA COLUMNS of arrival delay in mins/departure delay in mins/dep. delay > 5mins

airline <- airline[-c(30,25,26)]

# Weekday might have some interesting affect on air travel
airline$weekday <- weekdays(as.Date(airline$Flight.date,format = "%m-%d-%Y" ))

#viz for avg satisfaction vs arrival delay

#calculate avg satisfaction for each arrival_delay category
cat1 <- airline[airline$Arrival_Delay=="No Delay",]
cat1
avg_cat1 <- mean(cat1$ï..Satisfaction)
avg_cat1

cat2 <- airline[airline$Arrival_Delay=="Half hour",]
cat2
avg_cat2 <- mean(cat2$ï..Satisfaction)
avg_cat2

cat3 <- airline[airline$Arrival_Delay=="1 hours",]
avg_cat3 <- mean(cat3$ï..Satisfaction)
avg_cat3

cat4 <- airline[airline$Arrival_Delay=="2 hours",]
avg_cat4 <- mean(cat4$ï..Satisfaction)


cat5 <-airline[airline$Arrival_Delay=="5 minutes",]
avg_cat5 <- mean(cat5$ï..Satisfaction)

cat6 <- airline[airline$Arrival_Delay=="Cancelled",]
avg_cat6 <- mean(cat6$ï..Satisfaction)

cat7 <- airline[airline$Arrival_Delay=="Greater than 2 hours",]
avg_cat7 <- mean(cat7$ï..Satisfaction)


arrivalDel_vs_avgsatisfaction<- data.frame("Arrival_delay" = c("No Delay","Half hour","1 hours","2 hours","5 minutes","Cancelled","Greater than 2 hours"), "average_Satisfaction"=c(avg_cat1,avg_cat2,avg_cat3,avg_cat4,avg_cat5,avg_cat6,avg_cat7))

g1 <-ggplot(data=arrivalDel_vs_avgsatisfaction, aes(x=Arrival_delay, y=average_Satisfaction)) + geom_bar(stat = "identity") + scale_y_continuous(breaks = c(0,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5))
#arrival delay (catg) vs Avg satisfaction
g1

#possible correlation b/w flight time and distance

cor(airline$Flight.Distance,airline$Flight.time.in.minutes,use="complete.obs")

#corr is 0.98 which is very high +ve correlation

# remove flight time and keeping only flight distance

airline <- airline[,-26]


# CHECK distribution of eating&drinking/ shopping at airport

hist(airline$Shopping.Amount.at.Airport)
summary(airline$Shopping.Amount.at.Airport)

#right skewed

hist(airline$Eating.and.Drinking.at.Airport)
summary(airline$Eating.and.Drinking.at.Airport)

#right skewed

#categorise these columns

shopping_cat_func <- function(col1){
  category <- replicate(length(col1),"No Shopping")
  category[col1 < 20] <- "less than 20$"
  category[col1 >= 20 & col1 < 50] <- "20$ to 50$"
  category[col1 >= 50 & col1 < 150] <- "50$ to 150$"
  category[col1 >= 150] <- "More than 150$"
  category[col1 == 0] <- "No Shopping"
  return (category)
}

EatingDrinking_cat_func <- function(col1){
  category <- replicate(length(col1),"No EatingDrinking")
  category[col1 < 20] <- "less than 20$"
  category[col1 >= 20 & col1 < 50] <- "20$ to 50$"
  category[col1 >= 50 & col1 < 150] <- "50$ to 150$"
  category[col1 >= 150] <- "More than 150$"
  category[col1 == 0] <- "No EatingDrinking"
  return (category)
}
#temp_air <- airline

airline$shopping_cat <- shopping_cat_func(airline$Shopping.Amount.at.Airport)
airline$eating_Driking <- EatingDrinking_cat_func(airline$Eating.and.Drinking.at.Airport)

count <-table(airline$shopping_cat)
count2 <- table(airline$eating_Driking)
#temp2 <- temp_air[,c("Shopping.Amount.at.Airport","shopping_cat")]

barplot(count, xlab = 'Shopping amounts')
barplot(count2, xlab = 'Eating Drinking amount')

#remove similar columns
airline <- airline[,-18]

#age of plane might be a useful predictor , lets see its values

sort(unique(airline$Year.of.First.Flight))
#the latest year is 2012. So we can calculate age with reference as 2012 year

airline$Plane_Age <- (2012 -airline$Year.of.First.Flight )

#relevant columns 

airline_v1 <- airline[,c(1,3,5,6,30,8,11,12,15,23,25,26,27,29,31,2)]

#dummy encoding for catg. variables

#library(mlr)

#temp_air_Regression <- airline_v1
#temp_air_Regression <- mlr::createDummyFeatures(temp_air_Regression, cols = "Gender")

#Stepwise Regression 

data <- airline_v1[,c(2:16)]

#Adding "Age" to lm
reg <- lm(formula = ï..Satisfaction ~ Age, data = airline_v1 )
summary(reg)
# age adjusted R-squared = 0.03654


#adding "age +gender" to lm
reg <- lm(formula = ï..Satisfaction ~ Age + factor(Gender), data = airline_v1 )
summary(reg)
#Age + gender adj R Sq. = 0.05723

#adding "age+ gender + Price sensitivity"
reg <- lm(formula = ï..Satisfaction ~ Age + factor(Gender) + Price.Sensitivity, data = airline_v1 )
summary((reg))
# Adjusted R-squared:  0.06765 

# adding "age+ gender + Price sensitivity + Plane_Age"
reg <- lm(formula = ï..Satisfaction ~ Age + factor(Gender) + Price.Sensitivity + Plane_Age, data = airline_v1 )
summary(reg)
#Adjusted R-squared:  0.0684 

#adding "age+ gender + Price sensitivity + Plane_Age + No of flights p.a"
reg <- lm(formula = ï..Satisfaction ~ Age + factor(Gender) + Price.Sensitivity + Plane_Age + No.of.Flights.p.a., data = airline_v1 )
summary(reg)
#Adjusted R-squared:  0.1086 

#adding "age+ gender + Price sensitivity + Plane_Age + No of flights p.a + Type.of.Travel"
reg <- lm(formula = ï..Satisfaction ~ Age + factor(Gender) + Price.Sensitivity + Plane_Age + No.of.Flights.p.a. + Type.of.Travel, data = airline_v1 ) 
summary(reg)
#sudden jump in Adj r sq : 0.3092

#adding "age+ gender + Price sensitivity + Plane_Age + No of flights p.a + Type.of.Travel + Class"
reg <- lm(formula = ï..Satisfaction ~ Age + factor(Gender) + Price.Sensitivity + Plane_Age + No.of.Flights.p.a. + Type.of.Travel + Class, data = airline_v1 ) 
summary(reg)
#Adj. r sq : 0.3101

##adding "age+ gender + Price sensitivity + Plane_Age + No of flights p.a + Type.of.Travel + Class + Scheduled.Departure.Hour"
reg <- lm(formula = ï..Satisfaction ~ Age + factor(Gender) + Price.Sensitivity + Plane_Age + No.of.Flights.p.a. + Type.of.Travel + Class +Scheduled.Departure.Hour, data = airline_v1 ) 
summary(reg)
#Adj. r sq : 0.3101
# same not chnaged, Scheduled.Departure.Hour" is not significant predictor


##adding "age+ gender + Price sensitivity + Plane_Age + No of flights p.a + Type.of.Travel + Class + Scheduled.Departure.Hour + Arrival_Delay"
reg <- lm(formula = ï..Satisfaction ~ Age + factor(Gender) + Price.Sensitivity + Plane_Age + No.of.Flights.p.a. + Type.of.Travel + Class  + Arrival_Delay, data = airline_v1 ) 
summary(reg)
#Adjusted R-squared:  0.3272


##adding "age+ gender + Price sensitivity + Plane_Age + No of flights p.a + Type.of.Travel + Class + Scheduled.Departure.Hour + Arrival_Delay + Departure_Delay"
reg <- lm(formula = ï..Satisfaction ~ Age + factor(Gender) + Price.Sensitivity + Plane_Age + No.of.Flights.p.a. + Type.of.Travel + Class  + Arrival_Delay + Departure_Delay , data = airline_v1 ) 
summary(reg)
#Adjusted R-squared:  0.3272
#No change and Departure_delay not significant hence remove from model


##adding "age+ gender + Price sensitivity + Plane_Age + No of flights p.a + Type.of.Travel + Class + Scheduled.Departure.Hour + Arrival_Delay + shop at airport
reg <- lm(formula = ï..Satisfaction ~ Age + factor(Gender) + Price.Sensitivity + Plane_Age + No.of.Flights.p.a. + Type.of.Travel + Class  + Arrival_Delay + Departure_Delay + shopping_cat, data = airline_v1 ) 
summary(reg)
#Adjusted R-squared:  0.328 
#n_shopping is significant predictor in shopping_cat

##adding "age+ gender + Price sensitivity + Plane_Age + No of flights p.a + Type.of.Travel + Class + Scheduled.Departure.Hour + Arrival_Delay + shop at airport +eating_drinking
reg <- lm(formula = ï..Satisfaction ~ Age + factor(Gender) + Price.Sensitivity + Plane_Age + No.of.Flights.p.a. + Type.of.Travel + Class  + Arrival_Delay + Departure_Delay + shopping_cat + eating_Driking, data = airline_v1 ) 
summary(reg)
#Adjusted R-squared:  0.3287

airline_v1$Weekday <- airline$weekday
reg <- lm(formula = ï..Satisfaction ~ Age + factor(Gender) + Price.Sensitivity + Plane_Age + No.of.Flights.p.a. + Type.of.Travel + Class  + Arrival_Delay + Departure_Delay + shopping_cat + eating_Driking + Weekday, data = airline_v1 ) 
summary(reg)
#no effect of weekday as predictor for satisfactionc 

##Final Model for Stepwise Regression
# #Coefficients:
# #Estimate Std. Error  t value Pr(>|t|)    
# #(Intercept)                          6.0725085  0.0387898  156.549  < 2e-16 ***
#   Age                                 -0.0010306  0.0003255   -3.167  0.00154 ** 
#   factor(Gender)Male                   0.4191476  0.0107142   39.121  < 2e-16 ***
#   Price.Sensitivity                   -0.1805537  0.0095575  -18.891  < 2e-16 ***
#   Plane_Age                           -0.0253816  0.0017326  -14.650  < 2e-16 ***
#   No.of.Flights.p.a.                  -0.0136505  0.0003887  -35.116  < 2e-16 ***
#   Type.of.TravelMileage tickets       -0.5396301  0.0197914  -27.266  < 2e-16 ***
#   Type.of.TravelPersonal Travel       -2.4656989  0.0126140 -195.473  < 2e-16 ***
#   ClassEco                            -0.2129533  0.0189495  -11.238  < 2e-16 ***
#   ClassEco Plus                       -0.3128525  0.0242408  -12.906  < 2e-16 ***
#   Arrival_Delay2 hours                 0.0452297  0.0415920    1.087  0.27683    
# Arrival_Delay5 minutes               0.6756609  0.0323272   20.901  < 2e-16 ***
#   Arrival_DelayCancelled               0.4314189  0.2486958    1.735  0.08279 .  
# Arrival_DelayGreater than 2 hours    0.0942848  0.0773600    1.219  0.22293    
# Arrival_DelayHalf hour               0.0709192  0.0269477    2.632  0.00850 ** 
#   Arrival_DelayNo Delay                0.6893252  0.0284308   24.246  < 2e-16 ***
#   Departure_Delay2 hours              -0.0090323  0.0423066   -0.213  0.83094    
# Departure_Delay5 minutes            -0.0505128  0.0319569   -1.581  0.11396    
# Departure_DelayCancelled            -0.2331593  0.2514491   -0.927  0.35379    
# Departure_DelayGreater than 2 hours -0.0765648  0.0784462   -0.976  0.32906    
# Departure_DelayHalf hour            -0.0234257  0.0272767   -0.859  0.39044    
# Departure_DelayNo Delay             -0.0422357  0.0285378   -1.480  0.13888    
# shopping_cat50$ to 150$              0.0108982  0.0199620    0.546  0.58510    
# shopping_catless than 20$            0.0034357  0.0210683    0.163  0.87046    
# shopping_catMore than 150$           0.0236044  0.0292629    0.807  0.41988    
# shopping_catNo Shopping             -0.1158768  0.0163767   -7.076 1.49e-12 ***
#   eating_Driking50$ to 150$            0.0960717  0.0121283    7.921 2.37e-15 ***
#   eating_Drikingless than 20$         -0.0848564  0.0212603   -3.991 6.57e-05 ***
#   eating_DrikingMore than 150$         0.1140195  0.0212265    5.372 7.82e-08 ***
#   eating_DrikingNo EatingDrinking     -0.0376293  0.0257533   -1.461  0.14398    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.854 on 129859 degrees of freedom
# Multiple R-squared:  0.3288,	Adjusted R-squared:  0.3287 
# F-statistic:  2194 on 29 and 129859 DF,  p-value: < 2.2e-16


#Stepwise reg using AIC criterion

library(MASS)
full_LR <- lm(ï..Satisfaction ~., data = airline_v1)
# stepwise

step_LR <- stepAIC(full_LR, direction = "both", trace = FALSE)
summary(step_LR)

# Call:
#   lm(formula = ï..Satisfaction ~ Age + Gender + Price.Sensitivity + 
#        Plane_Age + No.of.Flights.p.a. + Type.of.Travel + No..of.other.Loyalty.Cards + 
#        Class + Scheduled.Departure.Hour + Arrival_Delay + shopping_cat + 
#        eating_Driking + Airline.Status, data = airline_v1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.3238 -1.0134 -0.1529  0.8708  7.7219 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                        5.2217399  0.0405664  128.721  < 2e-16 ***
#   Age                               -0.0029145  0.0003382   -8.617  < 2e-16 ***
#   GenderMale                         0.4046833  0.0100999   40.068  < 2e-16 ***
#   Price.Sensitivity                 -0.1006238  0.0090215  -11.154  < 2e-16 ***
#   Plane_Age                         -0.0177209  0.0016303  -10.870  < 2e-16 ***
#   No.of.Flights.p.a.                -0.0082314  0.0003710  -22.190  < 2e-16 ***
#   Type.of.TravelMileage tickets     -0.3940120  0.0186469  -21.130  < 2e-16 ***
#   Type.of.TravelPersonal Travel     -2.2947933  0.0119692 -191.724  < 2e-16 ***
#   No..of.other.Loyalty.Cards         0.0203883  0.0048026    4.245 2.18e-05 ***
#   ClassEco                          -0.1926082  0.0178189  -10.809  < 2e-16 ***
#   ClassEco Plus                     -0.1928387  0.0228306   -8.446  < 2e-16 ***
#   Scheduled.Departure.Hour           0.0094057  0.0010558    8.908  < 2e-16 ***
#   Arrival_Delay2 hours               0.0373371  0.0286186    1.305  0.19202    
# Arrival_Delay5 minutes             0.6678559  0.0248435   26.882  < 2e-16 ***
#   Arrival_DelayCancelled             0.2801034  0.0398423    7.030 2.07e-12 ***
#   Arrival_DelayGreater than 2 hours  0.0529915  0.0353587    1.499  0.13396    
# Arrival_DelayHalf hour             0.0630165  0.0206500    3.052  0.00228 ** 
#   Arrival_DelayNo Delay              0.6692469  0.0190239   35.179  < 2e-16 ***
#   shopping_cat50$ to 150$            0.0011947  0.0187708    0.064  0.94925    
# shopping_catless than 20$          0.0357400  0.0198132    1.804  0.07126 .  
# shopping_catMore than 150$         0.0750169  0.0275190    2.726  0.00641 ** 
#   shopping_catNo Shopping           -0.0346106  0.0154125   -2.246  0.02473 *  
#   eating_Driking50$ to 150$          0.0165463  0.0114206    1.449  0.14739    
# eating_Drikingless than 20$       -0.0362289  0.0199958   -1.812  0.07002 .  
# eating_DrikingMore than 150$      -0.0413505  0.0200054   -2.067  0.03874 *  
#   eating_DrikingNo EatingDrinking    0.0096822  0.0242207    0.400  0.68934    
# Airline.StatusGold                 1.2695891  0.0179943   70.555  < 2e-16 ***
#   Airline.StatusPlatinum             1.1888805  0.0279337   42.561  < 2e-16 ***
#   Airline.StatusSilver               1.4723234  0.0125291  117.513  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.743 on 129860 degrees of freedom
# Multiple R-squared:  0.4066,	Adjusted R-squared:  0.4064 
# F-statistic:  3177 on 28 and 129860 DF,  p-value: < 2.2e-16

##plots

Monday <-airline[airline$weekday == 'Monday',]
Tuesday <- airline[airline$weekday == 'Tuesday',]
Wednesday <- airline[airline$weekday == 'Wednesday',]
Thursday <- airline[airline$weekday == 'Thursday',]
Friday <- airline[airline$weekday == 'Friday',]
Saturday <- airline[airline$weekday == 'Saturday',]
Sunday <- airline[airline$weekday == 'Sunday',]

#Find avg.satisfaction for each day
avg_mon <- mean(Monday$ï..Satisfaction)
avg_tue <- mean(Tuesday$ï..Satisfaction)
avg_wed <- mean(Wednesday$ï..Satisfaction)
avg_thur <- mean(Thursday$ï..Satisfaction)
avg_fri <- mean(Friday$ï..Satisfaction)
avg_sat <- mean(Saturday$ï..Satisfaction)
avg_sun <- mean(Sunday$ï..Satisfaction)

week_data <- data.frame("Weekday" = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),"Average_Satisfaction"=c(avg_mon,avg_tue,avg_wed,avg_thur,avg_fri,avg_sat,avg_sun))

ggplot(data = week_data, aes(x=Weekday, y= Average_Satisfaction)) +
  geom_point(colour = "blue", size = 7)

str(week_data)

week_data$Weekday <- as.character(week_data$Weekday)
week_data$Weekday <- factor(week_data$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


# gender and Avg satisfaction

library(dplyr)

df_MF <- airline_v1 %>% group_by(Gender) %>% dplyr::summarise(Mean = mean(ï..Satisfaction))

graph_MF_satisfaction <- ggplot(data = df_MF, aes(x= Gender, y= Mean)) + geom_point(colour ="black", size =7)

graph_MF_satisfaction

# age and satisfaction

age_VS_satisfaction <- airline_v1[,c(1,2)]
graph_age_vs_satisfaction <- ggplot(data = age_VS_satisfaction, aes(x = Age, y =ï..Satisfaction, fill= Age))+ geom_col() + xlab("age") + ylab("Avg. satisfaction") 

graph_age_vs_satisfaction
# satisfaction lower with age increase




















##################################Association Rules#################################################### 

#need to discretize numerical attributes for data

satisfaction_cat_func <- function(col1){
  catg <- replicate(length(col1), "High")
  catg[col1 <=3 & col1 >2] <- "Medium"
  catg[col1 <=2] <- "Low"
  return(catg)
}

airline_v2 <- airline_v1

airline_v2$Satisfaction_cat <- as.factor(satisfaction_cat_func(airline_v1$ï..Satisfaction))

age_cat_func <- function(col1){
  catg <- replicate(length(col1),"Teen")
  catg[col1 > 20 & col1 <= 37]<- "Adult"
  catg[col1 > 37 & col1 <= 57] <- "Middle Aged"
  catg[col1 > 57] <- "Senior citizens"
  return(catg)
}

airline_v2$age_cat <- as.factor(age_cat_func(airline_v2$Age))

plan_age_cat_func <- function(col1){
  catg <- replicate(length(col1),"New Plane")
  catg[col1 >= 4] <- "Old Plane"
  return(catg)
}

airline_v2$planeage_cat <- as.factor(plan_age_cat_func(airline_v2$Plane_Age))




#association rules
library(arules)
library(arulesViz)

str(airline_v2)
airline_v2$eating_Driking <- as.factor(airline_v2$eating_Driking)
airline_v2$shopping_cat <- as.factor(airline_v2$shopping_cat)
airline_v2$Arrival_Delay <-as.factor(airline_v2$Arrival_Delay)
airline_v2$Departure_Delay <- as.factor(airline_v2$Departure_Delay)

Rules <- apriori(airline_v2[,c(7,9,12,13,14,15,16:19)],parameter = list(support = 0.01, confidence= 0.6), appearance = list(default ="lhs",rhs=("Satisfaction_cat=Low")))
inspect(Rules)
rules_by_lift <- sort(Rules, by= "lift")
inspect(head(rules_by_lift))

low_satifaction_Rules <- Rules[quality(Rules)$lift > 4.5]
low_satifaction_Rules_bylift <- sort(low_satifaction_Rules, by= "lift")
inspect(low_satifaction_Rules_bylift)

# lhs                                 rhs                       support confidence     lift count
# [1]  {Type.of.Travel=Personal Travel,                                                               
# Arrival_Delay=1 hours,                                                                        
# shopping_cat=No Shopping,                                                                     
# Airline.Status=Blue}            => {Satisfaction_cat=Low} 0.01013173  0.9570909 4.675979  1316

# [2]  {Type.of.Travel=Personal Travel,                                                               
# Arrival_Delay=Half hour,                                                                      
# Departure_Delay=Half hour,                                                                    
# Airline.Status=Blue,                                                                          
# age_cat=Senior citizens}        => {Satisfaction_cat=Low} 0.01322668  0.9565702 4.673435  1718

# [3]  {Type.of.Travel=Personal Travel,                                                               
# Class=Eco,                                                                                    
# Arrival_Delay=Half hour,                                                                      
# Departure_Delay=Half hour,                                                                    
# Airline.Status=Blue,                                                                          
# age_cat=Senior citizens}        => {Satisfaction_cat=Low} 0.01054747  0.9547038 4.664317  1370

# [4]  {Type.of.Travel=Personal Travel,                                                               
# Class=Eco,                                                                                    
# Arrival_Delay=1 hours,                                                                        
# Airline.Status=Blue}            => {Satisfaction_cat=Low} 0.01320358  0.9538376 4.660085  1715

# [5]  {Type.of.Travel=Personal Travel,                                                               
# Arrival_Delay=1 hours,                                                                        
# Airline.Status=Blue}            => {Satisfaction_cat=Low} 0.01629853  0.9536036 4.658941  2117

# [6]  {Type.of.Travel=Personal Travel,                                                               
# Arrival_Delay=2 hours,                                                                        
# Airline.Status=Blue}            => {Satisfaction_cat=Low} 0.01057056  0.9495159 4.638971  1373
# [7]  {Type.of.Travel=Personal Travel,                                                               
# Arrival_Delay=Half hour,                                                                      
# shopping_cat=No Shopping,                                                                     
# Airline.Status=Blue,                                                                          
# age_cat=Senior citizens}        => {Satisfaction_cat=Low} 0.01589049  0.9485294 4.634151  2064
# [8]  {Type.of.Travel=Personal Travel,                                                               
# Class=Eco,                                                                                    
# Arrival_Delay=Half hour,                                                                      
# eating_Driking=50$ to 150$,                                                                   
# Airline.Status=Blue,                                                                          
# age_cat=Senior citizens}        => {Satisfaction_cat=Low} 0.01089392  0.9483914 4.633477  1415
# [9]  {Type.of.Travel=Personal Travel,                                                               
# Class=Eco,                                                                                    
# Arrival_Delay=Half hour,                                                                      
# shopping_cat=No Shopping,                                                                     
# Airline.Status=Blue,                                                                          
# age_cat=Senior citizens}        => {Satisfaction_cat=Low} 0.01287253  0.9483834 4.633438  1672
# [10] {Type.of.Travel=Personal Travel,                                                               
# Arrival_Delay=Half hour,                                                                      
# Airline.Status=Blue,                                                                          
# age_cat=Senior citizens,                                                                      
# planeage_cat=Old Plane}         => {Satisfaction_cat=Low} 0.01510521  0.9473684 4.628479  1962
# [11] {Type.of.Travel=Personal Travel,                                                               
# Arrival_Delay=Half hour,                                                                      
# Airline.Status=Blue,                                                                          
# age_cat=Senior citizens}        => {Satisfaction_cat=Low} 0.02524463  0.9468669 4.626028  3279
# [12] {Type.of.Travel=Personal Travel,                                                               
# Departure_Delay=1 hours,                                                                      
# shopping_cat=No Shopping,                                                                     
# Airline.Status=Blue}            => {Satisfaction_cat=Low} 0.01034730  0.9464789 4.624133  1344
# [13] {Type.of.Travel=Personal Travel,                                                               
# Class=Eco,                                                                                    
# Arrival_Delay=Half hour,                                                                      
# Airline.Status=Blue,                                                                          
# age_cat=Senior citizens}        => {Satisfaction_cat=Low} 0.02024806  0.9463836 4.623667  2630
# [14] {Type.of.Travel=Personal Travel,                                                               
# Departure_Delay=1 hours,                                                                      
# Airline.Status=Blue}            => {Satisfaction_cat=Low} 0.01657569  0.9463736 4.623619  2153
# [15] {Type.of.Travel=Personal Travel,                                                               
# Arrival_Delay=Half hour,                                                                      
# Airline.Status=Blue,                                                                          
# age_cat=Senior citizens,                                                                      
# planeage_cat=New Plane}         => {Satisfaction_cat=Low} 0.01013943  0.9461207 4.622383  1317
# [16] {Type.of.Travel=Personal Travel,                                                               
# Arrival_Delay=Half hour,                                                                      
# eating_Driking=50$ to 150$,                                                                   
# Airline.Status=Blue,                                                                          
# age_cat=Senior citizens}        => {Satisfaction_cat=Low} 0.01391188  0.9460733 4.622151  1807
# [17] {Type.of.Travel=Personal Travel,                                                               
# Class=Eco,                                                                                    
# Departure_Delay=1 hours,                                                                      
# Airline.Status=Blue}            => {Satisfaction_cat=Low} 0.01348074  0.9459751 4.621672  1751
# [18] {Type.of.Travel=Personal Travel,                                                               
# Class=Eco,                                                                                    
# Arrival_Delay=Half hour,                                                                      
# Airline.Status=Blue,                                                                          
# age_cat=Senior citizens,                                                                      
# planeage_cat=Old Plane}         => {Satisfaction_cat=Low} 0.01204875  0.9456193 4.619933  1565
# [19] {Type.of.Travel=Personal Travel,                                                               
# Departure_Delay=2 hours,                                                                      
# Airline.Status=Blue}            => {Satisfaction_cat=Low} 0.01018562  0.9416370 4.600477  1323
# [20] {Type.of.Travel=Personal Travel,                                                               
# Arrival_Delay=Half hour,                                                                      
# Departure_Delay=Half hour,                                                                    
# shopping_cat=No Shopping,                                                                     
# Airline.Status=Blue}            => {Satisfaction_cat=Low} 0.01505131  0.9296243 4.541788  1955
# [21] {Type.of.Travel=Personal Travel,                                                               
# Class=Eco,                                                                                    
# Arrival_Delay=Half hour,                                                                      
# Departure_Delay=Half hour,                                                                    
# shopping_cat=No Shopping,                                                                     
# Airline.Status=Blue}            => {Satisfaction_cat=Low} 0.01218733  0.9279015 4.533371  1583
# [22] {Type.of.Travel=Personal Travel,                                                               
# Arrival_Delay=Half hour,                                                                      
# shopping_cat=No Shopping,                                                                     
# Airline.Status=Blue,                                                                          
# planeage_cat=New Plane}         => {Satisfaction_cat=Low} 0.01148673  0.9272840 4.530354  1492
# [23] {Type.of.Travel=Personal Travel,                                                               
# Class=Eco,                                                                                    
# Arrival_Delay=Half hour,                                                                      
# Departure_Delay=Half hour,                                                                    
# Airline.Status=Blue,                                                                          
# planeage_cat=Old Plane}         => {Satisfaction_cat=Low} 0.01215653  0.9266432 4.527223  1579
# [24] {Type.of.Travel=Personal Travel,                                                               
# Arrival_Delay=Half hour,                                                                      
# Departure_Delay=Half hour,                                                                    
# Airline.Status=Blue,                                                                          
# planeage_cat=Old Plane}         => {Satisfaction_cat=Low} 0.01498202  0.9253447 4.520880  1946
# [25] {Type.of.Travel=Personal Travel,                                                               
# Arrival_Delay=Half hour,                                                                      
# Departure_Delay=Half hour,                                                                    
# eating_Driking=50$ to 150$,                                                                   
# Airline.Status=Blue}            => {Satisfaction_cat=Low} 0.01221043  0.9242424 4.515494  1586
# [26] {Type.of.Travel=Personal Travel,                                                               
# Arrival_Delay=Half hour,                                                                      
# Departure_Delay=Half hour,                                                                    
# Airline.Status=Blue}            => {Satisfaction_cat=Low} 0.02409750  0.9238489 4.513571  3130
# [27] {Type.of.Travel=Personal Travel,                                                               
# Class=Eco,                                                                                    
# Arrival_Delay=Half hour,                                                                      
# shopping_cat=No Shopping,                                                                     
# Airline.Status=Blue}            => {Satisfaction_cat=Low} 0.02349699  0.9223330 4.506165  3052
# [28] {Type.of.Travel=Personal Travel,                                                               
# Class=Eco,                                                                                    
# Arrival_Delay=Half hour,                                                                      
# Departure_Delay=Half hour,                                                                    
# Airline.Status=Blue}            => {Satisfaction_cat=Low} 0.01954746  0.9215971 4.502570  2539


plot(low_satifaction_Rules_bylift, method = "grouped")

subrules <- sample(low_satifaction_Rules_bylift,10)
plot(subrules, method = "graph")
plot(subrules, method="graph", control=list(layout=igraph::in_circle()))

quality(itemsets)


