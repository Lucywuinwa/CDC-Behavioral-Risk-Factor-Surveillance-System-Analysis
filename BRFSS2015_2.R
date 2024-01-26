#install.packages("caret")
#install.packages("Hmisc")
#install.packages("lsr")
#install.packages("olsrr")
#install.packages("psych")
#install.packages("lm")
#install.packages("beta")


suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(Hmisc))
suppressPackageStartupMessages(library(lsr))
suppressPackageStartupMessages(library(olsrr))
suppressPackageStartupMessages(library(psych))


######Q10 Address the values of each of the variables:

##1. SCNTLWK1   Thinking about the last time you worked, about how many hours did you work per week at all of your jobs and 
                #businesses combined?  
     #1-96 : (hours)count for those who work, 
     # 97 : is Don’t know/Not Sure  ; 
     # 98 : Zero (none) ; 

##2. LSATISFY    In general, how satisfied are you with your life? 
     # 1 is for Very satisfied ; 
     # 2 is for Satisfied ; 
     # 3 is for Dissatisfied ; 
     # 4 is for Very dissatisfied ; 
     # 7 is Don’t know/Not sure
     # 9  is Refused 


##3. SMOKE100    Have you smoked at least 100 cigarettes in your entire life?
     # 1 is  yes ; 
     # 2 is No;  
     # 7 is Don’t know/Not sure ; 
     # 9  is Refused 


##4.  EMPLOY1   Employment Status 
     # 1 is Employed for wages ; 
     # 2 is Self-employed  ; 
     # 3 is Out of work for 1 year or more ;
     # 4 is Out of work for less than 1 year ;
     # 5 is A homemaker ;
     # 6 is A student ;
     # 7 is Retired ;
     # 8 is Unable to work ;
     # 9  is Refused 

#update those to dataset
brf <- read_csv("BRFSS2015_650.csv")
 
    

df<-  brf  %>% 
  select(SCNTLWK1, LSATISFY, SMOKE100, EMPLOY1 ) %>%
  na.omit()

  
  


######Q11: Remove any outliers for each applicable variable
#I will use Boxplot Method: Use boxplots to identify outliers as points that fall outside the whiskers of the boxplot. 
#he whiskers are typically defined as 1.5 times the IQR.

# Remove outliers for SCNTLWK1 variable
boxplot(df$SCNTLWK1, outline=FALSE)
lower_fence_SCNTLWK1 <- quantile(df$SCNTLWK1, 0.25) - 1.5 * IQR(df$SCNTLWK1)
upper_fence_SCNTLWK1 <- quantile(df$SCNTLWK1, 0.75) + 1.5 * IQR(df$SCNTLWK1)
df <- df[df$SCNTLWK1 >= lower_fence_SCNTLWK1 & df$SCNTLWK1 <= upper_fence_SCNTLWK1, ]

# Remove outliers for LSATISFY variable
boxplot(df$LSATISFY, outline=FALSE)
lower_fence_LSATISFY <- quantile(df$LSATISFY, 0.25) - 1.5 * IQR(df$LSATISFY)
upper_fence_LSATISFY <- quantile(df$LSATISFY, 0.75) + 1.5 * IQR(df$LSATISFY)
df <- df[df$LSATISFY >= lower_fence_LSATISFY & df$LSATISFY <= upper_fence_LSATISFY, ]

# Remove outliers for SMOKE100 variable
boxplot(df$SMOKE100, outline=FALSE)
lower_fence_SMOKE100 <- quantile(df$SMOKE100, 0.25) - 1.5 * IQR(df$SMOKE100)
upper_fence_SMOKE100 <- quantile(df$SMOKE100, 0.75) + 1.5 * IQR(df$SMOKE100)
df <- df[df$SMOKE100 >= lower_fence_SMOKE100 & df$SMOKE100 <= upper_fence_SMOKE100, ]

# Remove outliers for EMPLOY1 variable
boxplot(df$EMPLOY1, outline=FALSE)
lower_fence_EMPLOY1 <- quantile(df$EMPLOY1, 0.25) - 1.5 * IQR(df$EMPLOY1)
upper_fence_EMPLOY1 <- quantile(df$EMPLOY1, 0.75) + 1.5 * IQR(df$EMPLOY1)
df <- df[df$EMPLOY1 >= lower_fence_EMPLOY1 & df$EMPLOY1 <= upper_fence_EMPLOY1, ]



###### Q12: Complete exploratory analyses (for each variable) doing appropriate visualizations with ggplot2.

###SCNTLWK1 Variable
# Create the Boxplot
ggplot(df, aes(x = "", y = SCNTLWK1)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "", y = "hours") +
  ggtitle("Boxplot - hours of work")



###LSATISFY Variable:

satisfaction_freq <- table(df$LSATISFY)

# Create the bar chart
barplot(satisfaction_freq, 
        names.arg = c("1", "2", "3"),
        xlab = "Satisfaction Level",
        ylab = "Count",
        main = "Distribution of Life Satisfaction",
        col = "steelblue",
        border = "black"
)



###SMOKE100 Variable:

# Create a bar plot for SMOKE100
barplot(table(df$SMOKE100), 
        main = "Have you smoked at least 100 cigarettes", 
        xlab = "1: Yes 2: No", 
        ylab = "Count", 
        col = "steelblue")


#EMPLOY1 Variable:
# Create a bar plot for EMPLOY1
ggplot(df, aes(x = factor(EMPLOY1))) +
  geom_bar(fill = "steelblue") +
  labs(x = "Employment Status", y = "Count") +
  ggtitle("Distribution of Employment Status")



######Q13: Run basic descriptive statistics. Be sure to address each variable. 
# Descriptive statistics for each variable
# Assuming 'Q10_df' is the dataset

# Variable: SCNTLWK1
summary(df$SCNTLWK1)

# Variable: LSATISFY
summary(df$LSATISFY)

# Variable: SMOKE100
summary(df$SMOKE100)

# Variable: EMPLOY1
summary(df$EMPLOY1)



######Q14: Finally, run at least 2 different, appropriate regressions predicting one of those variables. 
           #These regressions should use different predictor(s). Identify the best model.


# Assuming we want to predict 'LSATISFY' using different predictors
# Assuming 'Q10_df' is the dataset


# Model 1: Predicting LSATISFY using SCNTLWK1 and SMOKE100
model1 <- lm(LSATISFY ~ SMOKE100, data = df)

# Summary of Model 1
summary(model1)


# Model 2: Predicting LSATISFY using EMPLOY1
model2 <- lm(LSATISFY ~  SCNTLWK1, data = df)

# Summary of Model 2
summary(model2)



###Identify the best model.

#  model 2 is a better fit for model for predicting LSATISFY as it has a lower p-value.
