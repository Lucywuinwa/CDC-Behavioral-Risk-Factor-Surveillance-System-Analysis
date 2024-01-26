
  ###
### Clear memory
###
#rm(list = ls())
suppressPackageStartupMessages(library(tidyverse))

### 
### Make sure the BRFSS2015_650.csv file is in the same folder as your RScript
### file (BRFSS2015.R). In RStudio, set your Working Directory to your Source 
### File location and then read the data into the brf object using read_csv().
###
brf <- read_csv("BRFSS2015_650.csv")
#view(brf)
### 
### Q1: How many people reported their general health is excellent?
###     The answer should be assigned to Q1.
### 



### Your code goes here. Uncomment the line below to assign your answer.
Q1 <-  brf %>%
  select(GENHLTH) %>%
  filter(GENHLTH == 1)  %>%
  nrow()
Q1
  
  
### 
### Q2: What is the highest value for the number of adult women in the 
###     household where someone has ever had a stroke? Summarise the value in 
###     a variable called max_num_women. 
###     The output should be a dataframe assigned to Q2.
### 

### Your code goes here. Uncomment the line below to assign your answer.
Q2 <- brf %>%
  filter(CVDSTRK3 == 1 ) %>%
  summarise(max_num_women = max(NUMWOMEN, na.rm = TRUE), .groups = 'drop') %>%
  as.data.frame()  
  
Q2
 
  
### 
### Q3: Compute the mean and standard deviation for MENTHLTH comparing 
###     caregivers who managed personal care such as giving medications, 
###     feeding, dressing, or bathing and those who did not. The summary 
###     variable names should be mean_mental and sd_mental. 
###     The output should be a dataframe assigned to Q3.
### 

### Your code goes here. Uncomment the line below to assign your answer.

  
modified_brf <-   brf %>% 
mutate(MENTHLTH = replace(MENTHLTH, MENTHLTH == 88, 0))



Q3<- modified_brf %>%
      filter(CRGVPERS == 1 | CRGVPERS == 2) %>%
      filter(MENTHLTH %in% 0:30) %>%
      group_by(CRGVPERS) %>%
      summarise(mean_mental = mean(MENTHLTH), sd_mental = sd(MENTHLTH))

Q3

  


### 
### Q4: What is the median age when respondents were told they had diabetes 
###     for those living in Pennsylvania? Only calculate it for those who gave 
###     an age. The summary variable name should be med_diab_age.
###     The output should be a dataframe assigned to Q4.
### 

### Your code goes here. Uncomment the line below to assign your answer.
# Q4 <- 



Q4 <- brf %>%
  filter(DIABAGE2 > 1  | `_STATE` == 42) %>%
  summarise(med_diab_age = median(DIABAGE2, na.rm = TRUE) ) %>% 
  as.data.frame()
Q4




### 
### Q5: Predict the number of days in the past 30 days that mental health was 
###     not good from marital status. Keep in mind that one of the possible 
###     answers to “how many days” is 0, not just 1-30. Make sure you know what 
###     type of variable MARITAL is. You’ll need to consider this when 
###     determining how to do linear regression with it.
###     Assign the summary of the model to Q5. 
###     Note: The general instructions say to round all output but the summary()
###           of a model is not able to be rounded. 
### 

### Your code goes here. Uncomment the line below to assign your answer.
# Q5 <- 


Q5 <-
  Q5_data <- modified_brf %>%
  filter(MENTHLTH %in% 0:30) %>%
  filter(MARITAL %in% 1:6)
  Q5_data$MARITAL <- as.factor(Q5_data$MARITAL)
  model <- lm(MENTHLTH ~ MARITAL, data = Q5_data)
  summary(model)

### 
### Q6: Use summarise to compare the mean number of days in the past 30 days 
###     that mental health was not good by marital status. The summary variable 
###     name should be mean_mental. The mean value for marital status 1 should 
###     help you to confirm the intercept value from Q5. 
###     The output should be a dataframe assigned to Q6.
### 

### Your code goes here. Uncomment the line below to assign your answer.
Q6 <- 
  
  Q5_data <- modified_brf %>%
  filter(MENTHLTH %in% 0:30) %>%
  filter(MARITAL %in% 1:6)  %>%

  group_by(MARITAL) %>%
  summarise(mean_mental = mean(MENTHLTH))
Q6



### 
### Q7: Calculate the means and standard deviations of MENTHLTH for those who 
###     have ever been diagnosed with a stroke and those who have not had a 
###     stroke only for those who do not have any kind of healthcare coverage. 
###     The summary variable names should be mean_mental and sd_mental.
###     The output should be a dataframe assigned to Q7.
### 

### Your code goes here. Uncomment the line below to assign your answer.
# Q7 <- 
Q7 <- modified_brf %>%
  filter(MENTHLTH %in% 0:30) %>%
  filter(CVDSTRK3 == 1 | CVDSTRK3 == 2) %>%
  filter(HLTHPLN1 == 2) %>%
  group_by(CVDSTRK3) %>%
  summarise(mean_mental = mean(MENTHLTH), sd_mental = sd(MENTHLTH))





### 
### Q8: Each respondent was asked if they participated in any physical 
###     activities in the past month. They were then asked what physical 
###     activity they spent the most time doing (or did the most) in the past 
###     month. Next, they were asked how many times per week or per month they 
###     took part in that exercise/activity. Run an ANOVA comparing how many 
###     times per week they took part in that exercise/activity with marital 
###     status. You may need to do some research on how to do this in R. 
###     Assign the summary of the ANOVA to Q8. 
###     Note: the general instructions say to round all output but the summary 
###           of an ANOVA  is not able to be rounded.
### 

### Your code goes here. Uncomment the line below to assign your answer.
# Q8 <- 

Q8 <-
  Q8_data <- modified_brf %>%
  select(MARITAL, EXEROFT1) %>%
  filter(MARITAL %in% 1:6) %>%
  filter(EXEROFT1 %in% 101:199) 
  Q8_data$MARITAL <- as.factor(Q8_data$MARITAL)
  anova_model <- aov(EXEROFT1 ~ MARITAL, data = Q8_data )
  summary(anova_model)

### 
### Q9: Consider only men for the following question. Each respondent was 
###     asked if they participated in any physical activities in the past month.
###     They were then asked what physical activity they spent the most time 
###     doing (or did the most) in the past month. Respondents were also asked 
###     to consider the past 30 days and answer either a) how many days per week
###     or b) how many days per month did they have at least one drink of any 
###     alcoholic beverage. For each type of physical activity or exercise, 
###     calculate the variance of the number of days per week a respondent 
###     drank alcohol. Note: pay careful attention to how values are coded in 
###     the Codebook. The summary variable name should be called var_drinks.
###     Arrange in descending order, and include only the six with the highest 
###     variance in drinks.
###     The output should be a dataframe assigned to Q9.
###

### Your code goes here. Uncomment the line below to assign your answer.
# Q9 <- 


# Filter the data down to only the rows we care about.
Q9_data <-
  brf %>%
  select(SEX, EXRACT11, ALCDAY5) %>% 
  filter(SEX == 1) %>%
  filter(ALCDAY5 %in% 100: 107 | ALCDAY5 == 888) %>%
  drop_na(EXRACT11)

# Transform the days of alcohol use into per week numbers.
per_week_respondent_indices <- which(Q9_data$ALCDAY5 %in% 100:107)
Q9_data$ALCDAY5[per_week_respondent_indices] <- Q9_data$ALCDAY5[per_week_respondent_indices] - 100
non_drinker_indices <- which(Q9_data$ALCDAY5 == 888)
Q9_data$ALCDAY5[non_drinker_indices] <- 0

# Compute the final summary.
Q9 <-
  Q9_data %>%
  group_by(EXRACT11) %>% 
  summarise(var_drinks = var(ALCDAY5)) %>%
  arrange(desc(var_drinks)) %>%
  head(6)

      