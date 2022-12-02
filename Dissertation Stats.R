######################## DISSERTATION STATISTICS ##############################

# Libraries and Setup -----------------------------------------------------
#load libraries
library(haven)
library(dplyr)
library(plyr)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(haven)
library(data.table)
library(janitor)
library(funModeling)
library(scales)
library(rstatix)
library(ggpubr)
library(ggstatsplot)
library(afex)
library(tinytex)
library(car)
library(multcomp)
library(hrbrthemes)
library(viridis)
library(mediation)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(lsr)
library(writexl)
library(readxl)
library(reshape2)
library(rstatix)
library(effectsize)
library(mediation)

#Report 3 way repeated measures ANOVA: 
#https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/#three-way
#Nonparametric test info: 
#https://yuzar-blog.netlify.app/posts/2022-02-08-friedman/
  
#Import Data
Dissertation_Database <- read_excel("/Volumes/SAya_USB/Server_Dissertation//Dissertation_Database.xlsx")
View(Dissertation_Database)

#Merge data
#Dissertation_Database<-merge(df1, df2, by = "SubjectID", all = TRUE)
#write_xlsx(Dissertation_Database, '/Volumes/SAya_USB/Server_Dissertation//Dissertation_Database.xlsx') #export long format df

#rename database
df<-Dissertation_Database
names(Dissertation_Database)


# Demographics ------------------------------------------------------------
#Demographics
#Age, mean and sd
mean(df$dob_years, na.rm = TRUE)
sd(df$dob_years, na.rm = TRUE)
summary(aov(b_rand~dob_years, data = df))
ggplot(df, aes(x = dob_years, fill = b_rand_string, color = b_rand_string))+
  geom_histogram(alpha = .5, binwidth =1.5)+
  ylab("Count")+
  xlab("Years Old")+
  ggtitle("Age in Years")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Randomization")+
  labs(color = "Randomization")

#Sex, 1=male, 0 = female, 3 = non-binary, 4 = prefer to self describe
tabyl(df, b_vo2_sex)
chisq.test(df$b_rand, df$ b_vo2_sex, correct = FALSE)
ggplot(df, aes(x = b_vo2_sex, fill = b_rand_string, color = b_rand_string))+
  geom_bar(alpha = .5)+
  ylab("Count")+
  xlab("Sex")+
  ggtitle("Sex")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Randomization")+
  labs(color = "Randomization")

#Race, 1 = American Indian or Alaskan Native, 2 = Asian, 3= African American, 4 = Native Hawaiian or Pacific Islander, 5 = Caucasian, 6 = Mixed or Other
tabyl(df, race)
chisq.test(df$b_rand, df$race, correct = FALSE)
ggplot(df, aes(x = race, fill = b_rand_string, color = b_rand_string))+
  geom_bar(alpha = .5)+
  ylab("Count")+
  xlab("Race")+
  ggtitle("Race")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Randomization")+
  labs(color = "Randomization")

#Ethnicity, 1 = yes, 0 = no 
tabyl(df, hispanic)
chisq.test(df$b_rand, df$hispanic, correct = FALSE)
ggplot(df, aes(x = hispanic, fill = b_rand_string, color = b_rand_string))+
  geom_bar(alpha = .5)+
  ylab("Count")+
  xlab("Ethnicity (Hispanic)")+
  ggtitle("Ethnicity")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Randomization")+
  labs(color = "Randomization")

#Household Income
#0	< $10,000
#1	1 $10,000-20,000
#2	$21,000-30,000
#3	$30,000-40,000
#4	$41,000-50,000
#5	$51,000-60,000
#6	$61,000-70,000
#7	$71,000-80,000
#8	$81,000-90,000
#9	$91,000-100,000
#10	+ $100,000
tabyl(df, householdincome)

#VO2 relative 
mean(df$b_vo2_value, na.rm = TRUE)
sd(df$b_vo2_value, na.rm = TRUE)
summary(aov(b_rand~b_vo2_value, data = df))

#Vo2 percentile 
mean(df$VO2max_perc, na.rm = TRUE)
sd(df$VO2max_perc, na.rm = TRUE)
summary(aov(b_rand~VO2max_perc, data = df))

Sex <- df$b_vo2_sex
VO2max_perc_mean<- ddply(df, "Sex", summarise, grp.mean=mean(VO2max_perc))
head(VO2max_perc_mean)

ggplot(df, aes(x=VO2max_perc, fill = Sex, color = Sex))+
  geom_histogram(binwidth = 10, alpha=.5, position="identity")+
  geom_vline(data=VO2max_perc_mean, aes(xintercept=grp.mean, color=Sex),
             linetype="dashed", size=1)+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="VO2 Percentile by Sex", x ="VO2 Percentile (mL/kg/min)", y = "Frequency")
 
#BMI
mean(df$VO2_BMI, na.rm = TRUE)
sd(df$VO2_BMI, na.rm = TRUE)
summary(aov(b_rand~VO2_BMI, data = df))

#BMI class
mean(df$VO2_BMI_class, na.rm = TRUE)
sd(df$VO2_BMI_class, na.rm = TRUE)
chisq.test(df$b_rand, df$VO2_BMI_class, correct = FALSE)
ggplot(df, aes(x = VO2_BMI_class, fill = b_rand_string, color = b_rand_string))+
  geom_bar(alpha = .5)+
  ylab("Count")+
  xlab("BMI Class")+
  ggtitle("BMI Class")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Randomization")+
  labs(color = "Randomization")

#Questionnaires
#Handedness, 0 = strongly prefer left, 1 = prefer left, 2 = no pref, 3 = prefer right, 4 = strongly prefer right 
tabyl(df, ehi_writing)
chisq.test(df$b_rand, df$ehi_writing, correct = FALSE)
ggplot(df, aes(x = ehi_writing, fill = b_rand_string, color = b_rand_string))+
  geom_bar(alpha = .5)+
  ylab("Count")+
  xlab("Handedness")+
  ggtitle("Handedness")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Randomization")+
  labs(color = "Randomization")

#PANAS-positive affect
mean(df$panas_posaffect, na.rm = TRUE)
sd(df$panas_posaffect, na.rm = TRUE)
summary(aov(b_rand~panas_posaffect, data = df))

#PANAS-negative affect
mean(df$panas_negaffect, na.rm = TRUE)
sd(df$panas_negaffect, na.rm = TRUE)
summary(aov(b_rand~panas_negaffect, data = df))
pairwise.t.test(df$panas_negaffect, df$b_rand, p.adj = "bonf")

#Satisfaction with Life Scale
mean(df$swls_total, na.rm = TRUE)
sd(df$swls_total, na.rm = TRUE)
summary(aov(b_rand~swls_total, data = df))

#Perceived Stress Scale
mean(df$pss_total, na.rm = TRUE)
sd(df$pss_total, na.rm = TRUE)
summary(aov(b_rand~pss_total, data = df))

#Pittsburgh Sleep Quality Index
mean(df$Global_Component, na.rm = TRUE)
sd(df$Global_Component, na.rm = TRUE)
summary(aov(b_rand~Global_Component, data = df))


# Saliva Demographics -----------------------------------------------------
#load necessary variables: 
SAA_1_Rest_E <- df$SAA_1_Rest_E
SAA_4_Rest_E <- df$SAA_4_Rest_E
SAA_6_Rest_E <- df$SAA_6_Rest_E
SAA_7_Rest_E <- df$SAA_7_Rest_E
SAA_Rest_E_mean<-data.frame(SAA_1_Rest_E, SAA_4_Rest_E, SAA_6_Rest_E, SAA_7_Rest_E)
SAA_Rest_E_mean<-rowMeans(SAA_Rest_E_mean)
SAA_1_Exercise <- df$SAA_1_Exercise
SAA_4_Exercise <- df$SAA_4_Exercise
SAA_6_Exercise <- df$SAA_6_Exercise
SAA_7_Exercise <- df$SAA_7_Exercise
SAA_Exercise_mean<-data.frame(SAA_1_Exercise, SAA_4_Exercise, SAA_6_Exercise, SAA_7_Exercise)
SAA_Exercise_mean<-rowMeans(SAA_Exercise_mean)
SAA_1_Rest_T <- df$SAA_1_Rest_T
SAA_4_Rest_T <- df$SAA_4_Rest_T
SAA_6_Rest_T <- df$SAA_6_Rest_T
SAA_7_Rest_T <- df$SAA_7_Rest_T
SAA_Rest_T_mean<-data.frame(SAA_1_Rest_T, SAA_4_Rest_T, SAA_6_Rest_T, SAA_7_Rest_T)
SAA_Rest_T_mean<-rowMeans(SAA_Rest_T_mean)
SAA_1_Trier <- df$SAA_1_Trier
SAA_4_Trier <- df$SAA_4_Trier
SAA_6_Trier <- df$SAA_6_Trier
SAA_7_Trier <- df$SAA_7_Trier
SAA_Trier_mean<-data.frame(SAA_1_Trier, SAA_4_Trier, SAA_6_Trier, SAA_7_Trier)
SAA_Trier_mean<-rowMeans(SAA_Trier_mean)

Cort_1_Rest_E <- df$Cort_1_Rest_E
Cort_4_Rest_E <- df$Cort_4_Rest_E
Cort_6_Rest_E <- df$Cort_6_Rest_E
Cort_7_Rest_E <- df$Cort_7_Rest_E
Cort_Rest_E_mean<-data.frame(Cort_1_Rest_E, Cort_4_Rest_E, Cort_6_Rest_E, Cort_7_Rest_E)
Cort_Rest_E_mean<-rowMeans(Cort_Rest_E_mean)
Cort_1_Exercise <- df$Cort_1_Exercise
Cort_4_Exercise <- df$Cort_4_Exercise
Cort_6_Exercise <- df$Cort_6_Exercise
Cort_7_Exercise <- df$Cort_7_Exercise
Cort_Exercise_mean<-data.frame(Cort_1_Exercise, Cort_4_Exercise, Cort_6_Exercise, Cort_7_Exercise)
Cort_Exercise_mean<-rowMeans(Cort_Exercise_mean)
Cort_1_Rest_T <- df$Cort_1_Rest_T
Cort_4_Rest_T <- df$Cort_4_Rest_T
Cort_6_Rest_T <- df$Cort_6_Rest_T
Cort_7_Rest_T <- df$Cort_7_Rest_T
Cort_Rest_T_mean<-data.frame(Cort_1_Rest_T, Cort_4_Rest_T, Cort_6_Rest_T, Cort_7_Rest_T)
Cort_Rest_T_mean<-rowMeans(Cort_Rest_T_mean)
Cort_1_Trier <- df$Cort_1_Trier
Cort_4_Trier <- df$Cort_4_Trier
Cort_6_Trier <- df$Cort_6_Trier
Cort_7_Trier <- df$Cort_7_Trier
Cort_Trier_mean<-data.frame(Cort_1_Trier, Cort_4_Trier, Cort_6_Trier, Cort_7_Trier)
Cort_Trier_mean<-rowMeans(Cort_Trier_mean)


Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
TOD_M1<-data.frame(Rest_E_TOD, E_TOD)
TOD_M1<-rowMeans(TOD_M1_TOD)
Rest_T_TOD<-df$r_end_time_T
T_TOD<-df$start_time
TOD_M2<-data.frame(Rest_T_TOD, T_TOD)
TOD_M2<-rowMeans(TOD_M2_TOD)

Age<-df$dob_years
Sex<-df$b_vo2_sex
Stress<-df$pss_total

#Is TOD related to sAA? 
summary(aov(SAA_Rest_E_mean~Rest_E_TOD, data = df))
summary(aov(SAA_Exercise_mean~E_TOD, data = df))
summary(aov(SAA_Rest_T_mean~Rest_T_TOD, data = df))
summary(aov(SAA_Trier_mean~T_TOD, data = df))

#is TOD related to cortisol? 
summary(aov(Cort_Rest_E_mean~Rest_E_TOD, data = df))
summary(aov(Cort_Exercise_mean~E_TOD, data = df))
summary(aov(Cort_Rest_T_mean~Rest_T_TOD, data = df))
summary(aov(Cort_Trier_mean~T_TOD, data = df))

#is age related to sAA? 
summary(aov(SAA_Rest_E_mean~Age, data = df))
summary(aov(SAA_Exercise_mean~Age, data = df))
summary(aov(SAA_Rest_T_mean~Age, data = df))
summary(aov(SAA_Trier_mean~Age, data = df))

#Is age related to cortisol? 
summary(aov(Cort_Rest_E_mean~Age, data = df))
summary(aov(Cort_Exercise_mean~Age, data = df))
summary(aov(Cort_Rest_T_mean~Age, data = df))
summary(aov(Cort_Trier_mean~Age, data = df))

#is Sex related to sAA? 
summary(aov(SAA_Rest_E_mean~Sex, data = df))
summary(aov(SAA_Exercise_mean~Sex, data = df))
summary(aov(SAA_Rest_T_mean~Sex, data = df))
summary(aov(SAA_Trier_mean~Sex, data = df))

#Is Sex related to cortisol? 
summary(aov(Cort_Rest_E_mean~Sex, data = df))
summary(aov(Cort_Exercise_mean~Sex, data = df))
summary(aov(Cort_Rest_T_mean~Sex, data = df))
summary(aov(Cort_Trier_mean~Sex, data = df))

#is stress related to sAA? 
summary(aov(SAA_Rest_E_mean~Stress, data = df))
summary(aov(SAA_Exercise_mean~Stress, data = df))
summary(aov(SAA_Rest_T_mean~Stress, data = df))
summary(aov(SAA_Trier_mean~Stress, data = df))

#is stress related to cortisol? 
summary(aov(Cort_Rest_E_mean~Stress, data = df))
summary(aov(Cort_Exercise_mean~Stress, data = df))
summary(aov(Cort_Rest_T_mean~Stress, data = df))
summary(aov(Cort_Trier_mean~Stress, data = df))

# COGNITIVE OUTCOMES - Flanker Response Time --------------------------------------------------------------
#Call Variables
SubjectID <- (df$SubjectID)
SAya_FL_Congruent_MeanRT_E_pre <- (df$ SAya_FL_Congruent_MeanRT_E_pre)
SAya_FL_Congruent_MeanRT_Rest_E_pre <- (df$ SAya_FL_Congruent_MeanRT_Rest_E_pre)
SAya_FL_Congruent_MeanRT_B_pre <- (df$ SAya_FL_Congruent_MeanRT_B_pre)
SAya_FL_Incongruent_MeanRT_E_pre <- (df$ SAya_FL_Incongruent_MeanRT_E_pre)
SAya_FL_Incongruent_MeanRT_Rest_E_pre <- (df$ SAya_FL_Incongruent_MeanRT_Rest_E_pre)
SAya_FL_Incongruent_MeanRT_B_pre <- (df$ SAya_FL_Incongruent_MeanRT_B_pre)
SAya_FL_Congruent_MeanRT_E_post <- (df$ SAya_FL_Congruent_MeanRT_E_post)
SAya_FL_Congruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Congruent_MeanRT_Rest_E_post)
SAya_FL_Incongruent_MeanRT_E_post <- (df$ SAya_FL_Incongruent_MeanRT_E_post)
SAya_FL_Incongruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Incongruent_MeanRT_Rest_E_post)
SAya_FL_Congruent_MeanRT_T_pre <- (df$ SAya_FL_Congruent_MeanRT_T_pre)
SAya_FL_Congruent_MeanRT_Rest_T_pre <- (df$ SAya_FL_Congruent_MeanRT_Rest_T_pre)
SAya_FL_Incongruent_MeanRT_T_pre <- (df$ SAya_FL_Incongruent_MeanRT_T_pre)
SAya_FL_Incongruent_MeanRT_Rest_T_pre <- (df$ SAya_FL_Incongruent_MeanRT_Rest_T_pre)
SAya_FL_Congruent_MeanRT_T_post <- (df$ SAya_FL_Congruent_MeanRT_T_post)
SAya_FL_Congruent_MeanRT_Rest_T_post <- (df$ SAya_FL_Congruent_MeanRT_Rest_T_post)
SAya_FL_Incongruent_MeanRT_T_post <- (df$ SAya_FL_Incongruent_MeanRT_T_post)
SAya_FL_Incongruent_MeanRT_Rest_T_post <- (df$ SAya_FL_Incongruent_MeanRT_Rest_T_post)

#Visualize all conditions
RT_Model1.df <- data.frame(SubjectID, SAya_FL_Congruent_MeanRT_B_pre, SAya_FL_Incongruent_MeanRT_B_pre, 
                           SAya_FL_Congruent_MeanRT_Rest_E_pre, SAya_FL_Incongruent_MeanRT_Rest_E_pre, 
                           SAya_FL_Congruent_MeanRT_Rest_E_post, SAya_FL_Incongruent_MeanRT_Rest_E_post, 
                           SAya_FL_Congruent_MeanRT_E_pre, SAya_FL_Incongruent_MeanRT_E_pre,
                           SAya_FL_Congruent_MeanRT_E_post, SAya_FL_Incongruent_MeanRT_E_post)
longformat.RT_Model1.df <- melt(RT_Model1.df, id = "SubjectID", variable.name = "Condition")
ggplot(longformat.RT_Model1.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "jitter", alpha = 0.5)+
  labs(title="Mean RT for Model 1 Exercise vs. Rest", x ="Intervention", y = "Mean Response Time (ms)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(250,550))

#M1 Difference in RT Exercise vs. Difference in Rest
Diff_PrePost_R_Congruent_RT <- SAya_FL_Congruent_MeanRT_Rest_E_post-SAya_FL_Congruent_MeanRT_Rest_E_pre
Diff_PrePost_R_Incongruent_RT <- SAya_FL_Incongruent_MeanRT_Rest_E_post-SAya_FL_Incongruent_MeanRT_Rest_E_pre
Diff_PrePost_E_Congruent_RT <- SAya_FL_Congruent_MeanRT_E_post-SAya_FL_Incongruent_MeanRT_E_pre
Diff_PrePost_E_Incongruent_RT <- SAya_FL_Incongruent_MeanRT_E_post-SAya_FL_Incongruent_MeanRT_E_pre
RT_Model1_Diff_PrePost.df <- data.frame(SubjectID, Diff_PrePost_R_Congruent_RT, Diff_PrePost_R_Incongruent_RT, 
                                        Diff_PrePost_E_Congruent_RT, Diff_PrePost_E_Incongruent_RT)
#Summarize
summary(RT_Model1_Diff_PrePost.df)
#Melt dataframe
longformat.RT_Model1_Diff_PrePost.df <- melt(RT_Model1_Diff_PrePost.df, id = "SubjectID", variable.name = "Condition")
View(longformat.RT_Model1_Diff_PrePost.df)
#Visualize
ggplot(longformat.RT_Model1_Diff_PrePost.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean RT Difference Post-Pre for Model 1 Exercise vs. Rest", x ="Intervention", y = "Mean Response Time Difference (ms)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(-130, 55))
#check for outliers
longformat.RT_Model1_Diff_PrePost.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
RT_Model1_Diff_PrePost.df %>%
 shapiro_test(Diff_PrePost_R_Congruent_RT, Diff_PrePost_R_Incongruent_RT, 
              Diff_PrePost_E_Congruent_RT, Diff_PrePost_E_Incongruent_RT)
histogram(RT_Model1_Diff_PrePost.df$Diff_PrePost_R_Congruent_RT)
#Passed normality test! Parametric stats used 
ggwithinstats(data = longformat.RT_Model1_Diff_PrePost.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_kendalls_w(0.46)


#M2 Difference in RT Trier vs. Difference in Rest
Diff_PrePost_R_Congruent_RT <- SAya_FL_Congruent_MeanRT_Rest_T_post-SAya_FL_Congruent_MeanRT_Rest_T_pre
Diff_PrePost_R_Incongruent_RT <- SAya_FL_Incongruent_MeanRT_Rest_T_post-SAya_FL_Incongruent_MeanRT_Rest_T_pre
Diff_PrePost_T_Congruent_RT <- SAya_FL_Congruent_MeanRT_T_post-SAya_FL_Incongruent_MeanRT_T_pre
Diff_PrePost_T_Incongruent_RT <- SAya_FL_Incongruent_MeanRT_T_post-SAya_FL_Incongruent_MeanRT_T_pre
RT_Model2_Diff_PrePost.df <- data.frame(SubjectID, Diff_PrePost_R_Congruent_RT, Diff_PrePost_R_Incongruent_RT, 
                                        Diff_PrePost_T_Congruent_RT, Diff_PrePost_T_Incongruent_RT)
#Summarize
summary(RT_Model2_Diff_PrePost.df)
#Melt dataframe
longformat.RT_Model2_Diff_PrePost.df <- melt(RT_Model2_Diff_PrePost.df, id = "SubjectID", variable.name = "Condition")
View(longformat.RT_Model2_Diff_PrePost.df)
#Visualize
ggplot(longformat.RT_Model2_Diff_PrePost.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean RT Difference Post-Pre for Model 1 Trier vs. Rest", x ="Intervention", y = "Mean Response Time Difference (ms)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(-130, 55))
#check for outliers
longformat.RT_Model2_Diff_PrePost.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
RT_Model2_Diff_PrePost.df %>%
  shapiro_test(Diff_PrePost_R_Congruent_RT, Diff_PrePost_R_Incongruent_RT, 
               Diff_PrePost_T_Congruent_RT, Diff_PrePost_T_Incongruent_RT)
histogram(RT_Model2_Diff_PrePost.df$Diff_PrePost_T_Congruent_RT)
#Did not pass normality for T congruent condition! Nonparametric stats used 
ggwithinstats(data = longformat.RT_Model2_Diff_PrePost.df, x = Condition, y = value, type = "nonparametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_kendalls_w(0.41)


# COGNITIVE OUTCOMES - Flanker Response Accuracy --------------------------
#Call Variables
SubjectID <- (df$SubjectID)
SAya_FL_Congruent_ResponseAccuracy_E_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_pre)
SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre)
SAya_FL_Congruent_ResponseAccuracy_B_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_B_pre)
SAya_FL_Incongruent_ResponseAccuracy_E_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_pre)
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre)
SAya_FL_Incongruent_ResponseAccuracy_B_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_B_pre)
SAya_FL_Congruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_post)
SAya_FL_Congruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_post)
SAya_FL_Incongruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_post)
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
SAya_FL_Congruent_ResponseAccuracy_T_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_T_pre)
SAya_FL_Congruent_ResponseAccuracy_Rest_T_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_T_pre)
SAya_FL_Incongruent_ResponseAccuracy_T_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_T_pre)
SAya_FL_Incongruent_ResponseAccuracy_Rest_T_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_T_pre)
SAya_FL_Congruent_ResponseAccuracy_T_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_T_post)
SAya_FL_Congruent_ResponseAccuracy_Rest_T_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_T_post)
SAya_FL_Incongruent_ResponseAccuracy_T_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_T_post)
SAya_FL_Incongruent_ResponseAccuracy_Rest_T_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_T_post)
#Visualize all conditions
ResponseAccuracy_Model1.df <- data.frame(SubjectID, SAya_FL_Congruent_ResponseAccuracy_B_pre, SAya_FL_Incongruent_ResponseAccuracy_B_pre, 
                                         SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre, 
                                         SAya_FL_Congruent_ResponseAccuracy_Rest_E_post, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post, 
                                         SAya_FL_Congruent_ResponseAccuracy_E_pre, SAya_FL_Incongruent_ResponseAccuracy_E_pre,
                                         SAya_FL_Congruent_ResponseAccuracy_E_post, SAya_FL_Incongruent_ResponseAccuracy_E_post)
longformat.ResponseAccuracy_Model1.df <- melt(ResponseAccuracy_Model1.df, id = "SubjectID", variable.name = "Condition")
summary(longformat.ResponseAccuracy_Model1.df)

ggplot(longformat.ResponseAccuracy_Model1.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "jitter", alpha = 0.5)+
  labs(title="Mean Response Accuracy for Model 1 Exercise vs. Rest", x ="Intervention", y = "Mean Accuracy (%)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(60, 100))

#M1 Difference in ACC Trier vs. Difference in Rest
Diff_PrePost_R_Congruent_ACC <- SAya_FL_Congruent_ResponseAccuracy_Rest_E_post-SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre
Diff_PrePost_R_Incongruent_ACC <- SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post-SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre
Diff_PrePost_E_Congruent_ACC <- SAya_FL_Congruent_ResponseAccuracy_E_post-SAya_FL_Incongruent_ResponseAccuracy_E_pre
Diff_PrePost_E_Incongruent_ACC <- SAya_FL_Incongruent_ResponseAccuracy_E_post-SAya_FL_Incongruent_ResponseAccuracy_E_pre
ACC_Model1_Diff_PrePost.df <- data.frame(SubjectID, Diff_PrePost_R_Congruent_ACC, Diff_PrePost_R_Incongruent_ACC, 
                                         Diff_PrePost_E_Congruent_ACC, Diff_PrePost_E_Incongruent_ACC)
#Summarize
summary(ACC_Model1_Diff_PrePost.df)
#Melt dataframe
longformat.ACC_Model1_Diff_PrePost.df <- melt(ACC_Model1_Diff_PrePost.df, id = "SubjectID", variable.name = "Condition")
View(longformat.ACC_Model1_Diff_PrePost.df)
#Visualize
ggplot(longformat.ACC_Model1_Diff_PrePost.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "jitter", alpha = 0.5)+
  labs(title="Mean ACC Difference Post-Pre for Model 1 Exercise vs. Rest", x ="Intervention", y = "Mean Response Time Difference (ms)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.ACC_Model1_Diff_PrePost.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
ACC_Model1_Diff_PrePost.df %>%
  shapiro_test(Diff_PrePost_R_Congruent_ACC, Diff_PrePost_R_Incongruent_ACC, 
               Diff_PrePost_E_Congruent_ACC, Diff_PrePost_E_Incongruent_ACC)
histogram(ACC_Model1_Diff_PrePost.df$Diff_PrePost_E_Incongruent_ACC)
#Did not pass normality test, non-parametric stats used
ggwithinstats(data = longformat.ACC_Model1_Diff_PrePost.df, x = Condition, y = value, type = "nonparametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_kendalls_w(0.54)


#M2 Difference in ACC Trier vs. Difference in Rest
Diff_PrePost_R_Congruent_ACC <- SAya_FL_Congruent_ResponseAccuracy_Rest_T_post-SAya_FL_Congruent_ResponseAccuracy_Rest_T_pre
Diff_PrePost_R_Incongruent_ACC <- SAya_FL_Incongruent_ResponseAccuracy_Rest_T_post-SAya_FL_Incongruent_ResponseAccuracy_Rest_T_pre
Diff_PrePost_T_Congruent_ACC <- SAya_FL_Congruent_ResponseAccuracy_T_post-SAya_FL_Incongruent_ResponseAccuracy_T_pre
Diff_PrePost_T_Incongruent_ACC <- SAya_FL_Incongruent_ResponseAccuracy_T_post-SAya_FL_Incongruent_ResponseAccuracy_T_pre
ACC_Model2_Diff_PrePost.df <- data.frame(SubjectID, Diff_PrePost_R_Congruent_ACC, Diff_PrePost_R_Incongruent_ACC, 
                                         Diff_PrePost_T_Congruent_ACC, Diff_PrePost_T_Incongruent_ACC)
#Summarize
summary(ACC_Model2_Diff_PrePost.df)
#Melt dataframe
longformat.ACC_Model2_Diff_PrePost.df <- melt(ACC_Model2_Diff_PrePost.df, id = "SubjectID", variable.name = "Condition")
View(longformat.ACC_Model2_Diff_PrePost.df)
#Visualize
ggplot(longformat.ACC_Model2_Diff_PrePost.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "jitter", alpha = 0.5)+
  labs(title="Mean ACC Difference Post-Pre for Model 2 Trier vs. Rest", x ="Intervention", y = "Mean Response Time Difference (ms)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.ACC_Model2_Diff_PrePost.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
ACC_Model2_Diff_PrePost.df %>%
  shapiro_test(Diff_PrePost_R_Congruent_ACC, Diff_PrePost_R_Incongruent_ACC, 
               Diff_PrePost_T_Congruent_ACC, Diff_PrePost_T_Incongruent_ACC)
histogram(ACC_Model2_Diff_PrePost.df$Diff_PrePost_R_Congruent_ACC)
#Did not pass normality test, non-parametric stats used
ggwithinstats(data = longformat.ACC_Model2_Diff_PrePost.df, x = Condition, y = value, type = "nonparametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_kendalls_w(0.51)

# COGNITIVE & FITNESS OUTCOME - Flanker Performance and VO2  ---------------------------
#Call Variables 
SubjectID <- (df$SubjectID)
SAya_FL_Congruent_MeanRT_B_pre <- (df$ SAya_FL_Congruent_MeanRT_B_pre)
SAya_FL_Incongruent_MeanRT_B_pre <- (df$ SAya_FL_Incongruent_MeanRT_B_pre)
SAya_FL_Congruent_ResponseAccuracy_B_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_B_pre)
SAya_FL_Incongruent_ResponseAccuracy_B_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_B_pre)
SAya_FL_Congruent_InverseEfficiency_B_pre<- df$SAya_FL_Congruent_InverseEfficiency_B_pre
SAya_FL_Incongruent_InverseEfficiency_B_pre<- df$SAya_FL_Incongruent_InverseEfficiency_B_pre
VO2<-df$b_vo2_value

#Baseline congruent Flanker RT related to fitness?
summary(lm(VO2~SAya_FL_Congruent_MeanRT_B_pre))

#Baseline incongruent Flanker RT related to fitness?
summary(lm(VO2~SAya_FL_Incongruent_MeanRT_B_pre))

#Baseline congruent flanker ACC realted to fitness? 
summary(lm(VO2~SAya_FL_Congruent_ResponseAccuracy_B_pre))

#Baseline incongruent flanker ACC related to fitness? 
summary(lm(VO2~SAya_FL_Incongruent_ResponseAccuracy_B_pre))

#Baseline congruent Flanker IE related to fitness?
  summary(lm(VO2~SAya_FL_Congruent_InverseEfficiency_B_pre))

#Baseline incongruent Flanker IE related to fitness?
summary(lm(VO2~SAya_FL_Incongruent_InverseEfficiency_B_pre))

# BRAIN FUNCTION OUTCOMES - Flanker P3 Amplitude ----------------------------------
#Call Variables
SubjectID<-df$SubjectID
FC_Rest_E_pre_P3_Con_CPZ_Amplitude<- df$FC_Rest_E_pre_P3_Con_CPZ_Amplitude
FC_Rest_E_pre_P3_Con_CP1_Amplitude<- df$FC_Rest_E_pre_P3_Con_CP1_Amplitude
FC_Rest_E_pre_P3_Con_CP2_Amplitude<- df$FC_Rest_E_pre_P3_Con_CP2_Amplitude
FC_Rest_E_po_P3_Con_CPZ_Amplitude<- df$FC_Rest_E_po_P3_Con_CPZ_Amplitude
FC_Rest_E_po_P3_Con_CP1_Amplitude<- df$FC_Rest_E_po_P3_Con_CP1_Amplitude
FC_Rest_E_po_P3_Con_CP2_Amplitude<- df$FC_Rest_E_po_P3_Con_CP2_Amplitude
FC_Rest_E_pre_P3_Inc_CPZ_Amplitude<- df$FC_Rest_E_pre_P3_Inc_CPZ_Amplitude
FC_Rest_E_pre_P3_Inc_CP1_Amplitude<- df$FC_Rest_E_pre_P3_Inc_CP1_Amplitude
FC_Rest_E_pre_P3_Inc_CP2_Amplitude<- df$FC_Rest_E_pre_P3_Inc_CP2_Amplitude
FC_Rest_E_po_P3_Inc_CPZ_Amplitude<- df$FC_Rest_E_po_P3_Inc_CPZ_Amplitude
FC_Rest_E_po_P3_Inc_CP1_Amplitude<- df$FC_Rest_E_po_P3_Inc_CP1_Amplitude
FC_Rest_E_po_P3_Inc_CP2_Amplitude<- df$FC_Rest_E_po_P3_Inc_CP2_Amplitude
FC_E_pre_P3_Con_CPZ_Amplitude<- df$FC_E_pre_P3_Con_CPZ_Amplitude
FC_E_pre_P3_Con_CP1_Amplitude<- df$FC_E_pre_P3_Con_CP1_Amplitude
FC_E_pre_P3_Con_CP2_Amplitude<- df$FC_E_pre_P3_Con_CP2_Amplitude
FC_E_po_P3_Con_CPZ_Amplitude<- df$FC_E_po_P3_Con_CPZ_Amplitude
FC_E_po_P3_Con_CP1_Amplitude<- df$FC_E_po_P3_Con_CP1_Amplitude
FC_E_po_P3_Con_CP2_Amplitude<- df$FC_E_po_P3_Con_CP2_Amplitude
FC_E_pre_P3_Inc_CPZ_Amplitude<- df$FC_E_pre_P3_Inc_CPZ_Amplitude
FC_E_pre_P3_Inc_CP1_Amplitude<- df$FC_E_pre_P3_Inc_CP1_Amplitude
FC_E_pre_P3_Inc_CP2_Amplitude<- df$FC_E_pre_P3_Inc_CP2_Amplitude
FC_E_po_P3_Inc_CPZ_Amplitude<- df$FC_E_po_P3_Inc_CPZ_Amplitude
FC_E_po_P3_Inc_CP1_Amplitude<- df$FC_E_po_P3_Inc_CP1_Amplitude
FC_E_po_P3_Inc_CP2_Amplitude<- df$FC_E_po_P3_Inc_CP2_Amplitude
FC_Rest_T_pre_P3_Con_CPZ_Amplitude<- df$FC_Rest_T_pre_P3_Con_CPZ_Amplitude
FC_Rest_T_pre_P3_Con_CP1_Amplitude<- df$FC_Rest_T_pre_P3_Con_CP1_Amplitude
FC_Rest_T_pre_P3_Con_CP2_Amplitude<- df$FC_Rest_T_pre_P3_Con_CP2_Amplitude
FC_Rest_T_po_P3_Con_CPZ_Amplitude<- df$FC_Rest_T_po_P3_Con_CPZ_Amplitude
FC_Rest_T_po_P3_Con_CP1_Amplitude<- df$FC_Rest_T_po_P3_Con_CP1_Amplitude
FC_Rest_T_po_P3_Con_CP2_Amplitude<- df$FC_Rest_T_po_P3_Con_CP2_Amplitude
FC_Rest_T_pre_P3_Inc_CPZ_Amplitude<- df$FC_Rest_T_pre_P3_Inc_CPZ_Amplitude
FC_Rest_T_pre_P3_Inc_CP1_Amplitude<- df$FC_Rest_T_pre_P3_Inc_CP1_Amplitude
FC_Rest_T_pre_P3_Inc_CP2_Amplitude<- df$FC_Rest_T_pre_P3_Inc_CP2_Amplitude
FC_Rest_T_po_P3_Inc_CPZ_Amplitude<- df$FC_Rest_T_po_P3_Inc_CPZ_Amplitude
FC_Rest_T_po_P3_Inc_CP1_Amplitude<- df$FC_Rest_T_po_P3_Inc_CP1_Amplitude
FC_Rest_T_po_P3_Inc_CP2_Amplitude<- df$FC_Rest_T_po_P3_Inc_CP2_Amplitude
FC_T_pre_P3_Con_CPZ_Amplitude<- df$FC_T_pre_P3_Con_CPZ_Amplitude
FC_T_pre_P3_Con_CP1_Amplitude<- df$FC_T_pre_P3_Con_CP1_Amplitude
FC_T_pre_P3_Con_CP2_Amplitude<- df$FC_T_pre_P3_Con_CP2_Amplitude
FC_T_po_P3_Con_CPZ_Amplitude<- df$FC_T_po_P3_Con_CPZ_Amplitude
FC_T_po_P3_Con_CP1_Amplitude<- df$FC_T_po_P3_Con_CP1_Amplitude
FC_T_po_P3_Con_CP2_Amplitude<- df$FC_T_po_P3_Con_CP2_Amplitude
FC_T_pre_P3_Inc_CPZ_Amplitude<- df$FC_T_pre_P3_Inc_CPZ_Amplitude
FC_T_pre_P3_Inc_CP1_Amplitude<- df$FC_T_pre_P3_Inc_CP1_Amplitude
FC_T_pre_P3_Inc_CP2_Amplitude<- df$FC_T_pre_P3_Inc_CP2_Amplitude
FC_T_po_P3_Inc_CPZ_Amplitude<- df$FC_T_po_P3_Inc_CPZ_Amplitude
FC_T_po_P3_Inc_CP1_Amplitude<- df$FC_T_po_P3_Inc_CP1_Amplitude
FC_T_po_P3_Inc_CP2_Amplitude<- df$FC_T_po_P3_Inc_CP2_Amplitude

#M1 Difference P3 Amplitude (narrow ROI) between Exercise vs. Rest
##Average together ROI sites
Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_pre_P3_Con_CPZ_Amplitude, FC_Rest_E_pre_P3_Con_CP1_Amplitude, FC_Rest_E_pre_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.df)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Amplitude, FC_Rest_E_po_P3_Con_CP1_Amplitude, FC_Rest_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df)
Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.df <-data.frame(FC_Rest_E_pre_P3_Inc_CPZ_Amplitude, FC_Rest_E_pre_P3_Inc_CP1_Amplitude, FC_Rest_E_pre_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.df)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Inc_CPZ_Amplitude, FC_Rest_E_po_P3_Inc_CP1_Amplitude, FC_Rest_E_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df)
Narrow.ROI_FC_E_pre_P3_Con_Amplitude.df <-data.frame(FC_E_pre_P3_Con_CPZ_Amplitude, FC_E_pre_P3_Con_CP1_Amplitude, FC_E_pre_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_pre_P3_Con_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.df <-data.frame(FC_E_po_P3_Con_CPZ_Amplitude, FC_E_po_P3_Con_CP1_Amplitude, FC_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Con_Amplitude.df)
Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.df <-data.frame(FC_E_pre_P3_Inc_CPZ_Amplitude, FC_E_pre_P3_Inc_CP1_Amplitude, FC_E_pre_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Inc_Amplitude.df <-data.frame(FC_E_po_P3_Inc_CPZ_Amplitude, FC_E_po_P3_Inc_CP1_Amplitude, FC_E_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Inc_Amplitude.df)
#Create difference scores of ROI and conditions
Diff_PrePost_R_Congruent_P3Amp <-Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean-Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean
Diff_PrePost_R_Incongruent_P3Amp<-Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean-Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean
Diff_PrePost_E_Congruent_P3Amp <-Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean-Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean
Diff_PrePost_E_Incongruent_P3Amp<-Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean-Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean
#create dataframe 
M1_P3.Amplitude.df <- data.frame(SubjectID, Diff_PrePost_R_Congruent_P3Amp, Diff_PrePost_R_Incongruent_P3Amp, Diff_PrePost_E_Congruent_P3Amp, Diff_PrePost_E_Incongruent_P3Amp)
#Summarize
summary(M1_P3.Amplitude.df)
#Melt dataframe
longformat.M1_P3.Amplitude.df <- melt(M1_P3.Amplitude.df, id = "SubjectID", variable.name = "Condition")
View(longformat.M1_P3.Amplitude.df)
#Visualize
ggplot(longformat.M1_P3.Amplitude.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean P3 Amplitude Difference Post-Pre for Model 1 Exercise vs. Rest", x ="Intervention", y = "Mean Amplitude Difference uV")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.M1_P3.Amplitude.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
M1_P3.Amplitude.df %>%
  shapiro_test(Diff_PrePost_R_Congruent_P3Amp, Diff_PrePost_R_Incongruent_P3Amp, Diff_PrePost_E_Congruent_P3Amp, Diff_PrePost_E_Incongruent_P3Amp)
histogram(M1_P3.Amplitude.df$Diff_PrePost_R_Congruent_P3Amp)
#passed normality test! used parametric 
ggwithinstats(data = longformat.M1_P3.Amplitude.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_kendalls_w(-0.01)


#M2 Difference P3 Amplitude (narrow ROI) between Trier vs. Rest
##Average together ROI sites
Narrow.ROI_FC_Rest_T_pre_P3_Con_Amplitude.df <-data.frame(FC_Rest_T_pre_P3_Con_CPZ_Amplitude, FC_Rest_T_pre_P3_Con_CP1_Amplitude, FC_Rest_T_pre_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_T_pre_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_T_pre_P3_Con_Amplitude.df,na.rm = TRUE)
Narrow.ROI_FC_Rest_T_po_P3_Con_Amplitude.df <-data.frame(FC_Rest_T_po_P3_Con_CPZ_Amplitude, FC_Rest_T_po_P3_Con_CP1_Amplitude, FC_Rest_T_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_T_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_T_po_P3_Con_Amplitude.df, na.rm = TRUE)
Narrow.ROI_FC_Rest_T_pre_P3_Inc_Amplitude.df <-data.frame(FC_Rest_T_pre_P3_Inc_CPZ_Amplitude, FC_Rest_T_pre_P3_Inc_CP1_Amplitude, FC_Rest_T_pre_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_Rest_T_pre_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_T_pre_P3_Inc_Amplitude.df,na.rm = TRUE)
Narrow.ROI_FC_Rest_T_po_P3_Inc_Amplitude.df <-data.frame(FC_Rest_T_po_P3_Inc_CPZ_Amplitude, FC_Rest_T_po_P3_Inc_CP1_Amplitude, FC_Rest_T_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_Rest_T_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_T_po_P3_Inc_Amplitude.df,na.rm = TRUE)
Narrow.ROI_FC_T_pre_P3_Con_Amplitude.df <-data.frame(FC_T_pre_P3_Con_CPZ_Amplitude, FC_T_pre_P3_Con_CP1_Amplitude, FC_T_pre_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_T_pre_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_T_pre_P3_Con_Amplitude.df,na.rm = TRUE)
Narrow.ROI_FC_T_po_P3_Con_Amplitude.df <-data.frame(FC_T_po_P3_Con_CPZ_Amplitude, FC_T_po_P3_Con_CP1_Amplitude, FC_T_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_T_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_T_po_P3_Con_Amplitude.df,na.rm = TRUE)
Narrow.ROI_FC_T_pre_P3_Inc_Amplitude.df <-data.frame(FC_T_pre_P3_Inc_CPZ_Amplitude, FC_T_pre_P3_Inc_CP1_Amplitude, FC_T_pre_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_T_pre_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_T_pre_P3_Inc_Amplitude.df,na.rm = TRUE)
Narrow.ROI_FC_T_po_P3_Inc_Amplitude.df <-data.frame(FC_T_po_P3_Inc_CPZ_Amplitude, FC_T_po_P3_Inc_CP1_Amplitude, FC_T_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_T_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_T_po_P3_Inc_Amplitude.df,na.rm = TRUE)
#Create difference scores of ROI and conditions
Diff_PrePost_R_Congruent_P3Amp <-Narrow.ROI_FC_Rest_T_po_P3_Con_Amplitude.mean-Narrow.ROI_FC_Rest_T_pre_P3_Con_Amplitude.mean
Diff_PrePost_R_Incongruent_P3Amp<-Narrow.ROI_FC_Rest_T_po_P3_Inc_Amplitude.mean-Narrow.ROI_FC_Rest_T_pre_P3_Inc_Amplitude.mean
Diff_PrePost_T_Congruent_P3Amp <-Narrow.ROI_FC_T_po_P3_Con_Amplitude.mean-Narrow.ROI_FC_T_pre_P3_Con_Amplitude.mean
Diff_PrePost_T_Incongruent_P3Amp<-Narrow.ROI_FC_T_po_P3_Inc_Amplitude.mean-Narrow.ROI_FC_T_pre_P3_Inc_Amplitude.mean
#create dataframe 
M2_P3.Amplitude.df <- data.frame(SubjectID, Diff_PrePost_R_Congruent_P3Amp, Diff_PrePost_R_Incongruent_P3Amp, Diff_PrePost_T_Congruent_P3Amp, Diff_PrePost_T_Incongruent_P3Amp)
#Summarize
summary(M2_P3.Amplitude.df)
#Melt dataframe
longformat.M2_P3.Amplitude.df <- melt(M2_P3.Amplitude.df, id = "SubjectID", variable.name = "Condition")
View(longformat.M2_P3.Amplitude.df)
#Visualize
ggplot(longformat.M2_P3.Amplitude.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean P3 Amplitude Difference Post-Pre for Model 2 Trier vs. Rest", x ="Intervention", y = "Mean Amplitude Difference uV")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.M2_P3.Amplitude.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
M2_P3.Amplitude.df %>%
  shapiro_test(Diff_PrePost_R_Congruent_P3Amp, Diff_PrePost_R_Incongruent_P3Amp, Diff_PrePost_T_Congruent_P3Amp, Diff_PrePost_T_Incongruent_P3Amp)
histogram(M2_P3.Amplitude.df$Diff_PrePost_T_Incongruent_P3Amp)
#Does not pass normality, used nonparametric
ggwithinstats(data = longformat.M2_P3.Amplitude.df, x = Condition, y = value, type = "nonparametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_kendalls_w(0.02)


# BRAIN FUNCTION OUTCOMES - Flanker P3 Latency ------------------------------------
#Call Variables
SubjectID<-df$SubjectID
FC_Rest_E_pre_P3_Con_CPZ_Latency<- df$FC_Rest_E_pre_P3_Con_CPZ_Latency
FC_Rest_E_pre_P3_Con_CP1_Latency<- df$FC_Rest_E_pre_P3_Con_CP1_Latency
FC_Rest_E_pre_P3_Con_CP2_Latency<- df$FC_Rest_E_pre_P3_Con_CP2_Latency
FC_Rest_E_po_P3_Con_CPZ_Latency<- df$FC_Rest_E_po_P3_Con_CPZ_Latency
FC_Rest_E_po_P3_Con_CP1_Latency<- df$FC_Rest_E_po_P3_Con_CP1_Latency
FC_Rest_E_po_P3_Con_CP2_Latency<- df$FC_Rest_E_po_P3_Con_CP2_Latency
FC_Rest_E_pre_P3_Inc_CPZ_Latency<- df$FC_Rest_E_pre_P3_Inc_CPZ_Latency
FC_Rest_E_pre_P3_Inc_CP1_Latency<- df$FC_Rest_E_pre_P3_Inc_CP1_Latency
FC_Rest_E_pre_P3_Inc_CP2_Latency<- df$FC_Rest_E_pre_P3_Inc_CP2_Latency
FC_Rest_E_po_P3_Inc_CPZ_Latency<- df$FC_Rest_E_po_P3_Inc_CPZ_Latency
FC_Rest_E_po_P3_Inc_CP1_Latency<- df$FC_Rest_E_po_P3_Inc_CP1_Latency
FC_Rest_E_po_P3_Inc_CP2_Latency<- df$FC_Rest_E_po_P3_Inc_CP2_Latency
FC_E_pre_P3_Con_CPZ_Latency<- df$FC_E_pre_P3_Con_CPZ_Latency
FC_E_pre_P3_Con_CP1_Latency<- df$FC_E_pre_P3_Con_CP1_Latency
FC_E_pre_P3_Con_CP2_Latency<- df$FC_E_pre_P3_Con_CP2_Latency
FC_E_po_P3_Con_CPZ_Latency<- df$FC_E_po_P3_Con_CPZ_Latency
FC_E_po_P3_Con_CP1_Latency<- df$FC_E_po_P3_Con_CP1_Latency
FC_E_po_P3_Con_CP2_Latency<- df$FC_E_po_P3_Con_CP2_Latency
FC_E_pre_P3_Inc_CPZ_Latency<- df$FC_E_pre_P3_Inc_CPZ_Latency
FC_E_pre_P3_Inc_CP1_Latency<- df$FC_E_pre_P3_Inc_CP1_Latency
FC_E_pre_P3_Inc_CP2_Latency<- df$FC_E_pre_P3_Inc_CP2_Latency
FC_E_po_P3_Inc_CPZ_Latency<- df$FC_E_po_P3_Inc_CPZ_Latency
FC_E_po_P3_Inc_CP1_Latency<- df$FC_E_po_P3_Inc_CP1_Latency
FC_E_po_P3_Inc_CP2_Latency<- df$FC_E_po_P3_Inc_CP2_Latency
FC_Rest_T_pre_P3_Con_CPZ_Latency<- df$FC_Rest_T_pre_P3_Con_CPZ_Latency
FC_Rest_T_pre_P3_Con_CP1_Latency<- df$FC_Rest_T_pre_P3_Con_CP1_Latency
FC_Rest_T_pre_P3_Con_CP2_Latency<- df$FC_Rest_T_pre_P3_Con_CP2_Latency
FC_Rest_T_po_P3_Con_CPZ_Latency<- df$FC_Rest_T_po_P3_Con_CPZ_Latency
FC_Rest_T_po_P3_Con_CP1_Latency<- df$FC_Rest_T_po_P3_Con_CP1_Latency
FC_Rest_T_po_P3_Con_CP2_Latency<- df$FC_Rest_T_po_P3_Con_CP2_Latency
FC_Rest_T_pre_P3_Inc_CPZ_Latency<- df$FC_Rest_T_pre_P3_Inc_CPZ_Latency
FC_Rest_T_pre_P3_Inc_CP1_Latency<- df$FC_Rest_T_pre_P3_Inc_CP1_Latency
FC_Rest_T_pre_P3_Inc_CP2_Latency<- df$FC_Rest_T_pre_P3_Inc_CP2_Latency
FC_Rest_T_po_P3_Inc_CPZ_Latency<- df$FC_Rest_T_po_P3_Inc_CPZ_Latency
FC_Rest_T_po_P3_Inc_CP1_Latency<- df$FC_Rest_T_po_P3_Inc_CP1_Latency
FC_Rest_T_po_P3_Inc_CP2_Latency<- df$FC_Rest_T_po_P3_Inc_CP2_Latency
FC_T_pre_P3_Con_CPZ_Latency<- df$FC_T_pre_P3_Con_CPZ_Latency
FC_T_pre_P3_Con_CP1_Latency<- df$FC_T_pre_P3_Con_CP1_Latency
FC_T_pre_P3_Con_CP2_Latency<- df$FC_T_pre_P3_Con_CP2_Latency
FC_T_po_P3_Con_CPZ_Latency<- df$FC_T_po_P3_Con_CPZ_Latency
FC_T_po_P3_Con_CP1_Latency<- df$FC_T_po_P3_Con_CP1_Latency
FC_T_po_P3_Con_CP2_Latency<- df$FC_T_po_P3_Con_CP2_Latency
FC_T_pre_P3_Inc_CPZ_Latency<- df$FC_T_pre_P3_Inc_CPZ_Latency
FC_T_pre_P3_Inc_CP1_Latency<- df$FC_T_pre_P3_Inc_CP1_Latency
FC_T_pre_P3_Inc_CP2_Latency<- df$FC_T_pre_P3_Inc_CP2_Latency
FC_T_po_P3_Inc_CPZ_Latency<- df$FC_T_po_P3_Inc_CPZ_Latency
FC_T_po_P3_Inc_CP1_Latency<- df$FC_T_po_P3_Inc_CP1_Latency
FC_T_po_P3_Inc_CP2_Latency<- df$FC_T_po_P3_Inc_CP2_Latency

#M1 Difference in P3 Latency between Exercise vs. Rest
#Average together ROI sites
Narrow.ROI_FC_Rest_E_pre_P3_Con_Latency.df <-data.frame(FC_Rest_E_pre_P3_Con_CPZ_Latency, FC_Rest_E_pre_P3_Con_CP1_Latency, FC_Rest_E_pre_P3_Con_CP2_Latency)
Narrow.ROI_FC_Rest_E_pre_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Con_Latency.df)
Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Latency, FC_Rest_E_po_P3_Con_CP1_Latency, FC_Rest_E_po_P3_Con_CP2_Latency)
Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.df)
Narrow.ROI_FC_Rest_E_pre_P3_Inc_Latency.df <-data.frame(FC_Rest_E_pre_P3_Inc_CPZ_Latency, FC_Rest_E_pre_P3_Inc_CP1_Latency, FC_Rest_E_pre_P3_Inc_CP2_Latency)
Narrow.ROI_FC_Rest_E_pre_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Inc_Latency.df)
Narrow.ROI_FC_E_pre_P3_Con_Latency.df <-data.frame(FC_E_pre_P3_Con_CPZ_Latency, FC_E_pre_P3_Con_CP1_Latency, FC_E_pre_P3_Con_CP2_Latency)
Narrow.ROI_FC_E_pre_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_E_pre_P3_Con_Latency.df)
Narrow.ROI_FC_E_po_P3_Con_Latency.df <-data.frame(FC_E_po_P3_Con_CPZ_Latency, FC_E_po_P3_Con_CP1_Latency, FC_E_po_P3_Con_CP2_Latency)
Narrow.ROI_FC_E_po_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Con_Latency.df)
Narrow.ROI_FC_E_pre_P3_Inc_Latency.df <-data.frame(FC_E_pre_P3_Inc_CPZ_Latency, FC_E_pre_P3_Inc_CP1_Latency, FC_E_pre_P3_Inc_CP2_Latency)
Narrow.ROI_FC_E_pre_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_E_pre_P3_Inc_Latency.df)
Narrow.ROI_FC_E_po_P3_Inc_Latency.df <-data.frame(FC_E_po_P3_Inc_CPZ_Latency, FC_E_po_P3_Inc_CP1_Latency, FC_E_po_P3_Inc_CP2_Latency)
Narrow.ROI_FC_E_po_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Inc_Latency.df)
#Create difference scores of ROI and conditions
Diff_PrePost_R_Congruent_P3Lat <-Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.mean-Narrow.ROI_FC_Rest_E_pre_P3_Con_Latency.mean
Diff_PrePost_R_Incongruent_P3Lat<-Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.mean-Narrow.ROI_FC_Rest_E_pre_P3_Inc_Latency.mean
Diff_PrePost_E_Congruent_P3Lat <-Narrow.ROI_FC_E_po_P3_Con_Latency.mean-Narrow.ROI_FC_E_pre_P3_Con_Latency.mean
Diff_PrePost_E_Incongruent_P3Lat<-Narrow.ROI_FC_E_po_P3_Inc_Latency.mean-Narrow.ROI_FC_E_pre_P3_Inc_Latency.mean
#create dataframe 
M1_P3.Latency.df <- data.frame(SubjectID, Diff_PrePost_R_Congruent_P3Lat, Diff_PrePost_R_Incongruent_P3Lat, Diff_PrePost_E_Congruent_P3Lat, Diff_PrePost_E_Incongruent_P3Lat)
#Summarize
summary(M1_P3.Latency.df)
#Melt dataframe
longformat.M1_P3.Latency.df <- melt(M1_P3.Latency.df, id = "SubjectID", variable.name = "Condition")
View(longformat.M1_P3.Latency.df)
#Visualize
ggplot(longformat.M1_P3.Latency.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean P3 Latency Difference Post-Pre for Model 1 Exercise vs. Rest", x ="Intervention", y = "Mean Latency Difference ms")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.M1_P3.Latency.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
M1_P3.Latency.df %>%
  shapiro_test(Diff_PrePost_R_Congruent_P3Lat, Diff_PrePost_R_Incongruent_P3Lat, Diff_PrePost_E_Congruent_P3Lat, Diff_PrePost_E_Incongruent_P3Lat)
histogram(M1_P3.Latency.df$Diff_PrePost_R_Congruent_P3Lat)
#does not pass normality, used non-parametric
ggwithinstats(data = longformat.M1_P3.Latency.df, x = Condition, y = value, type = "nonparametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_kendalls_w(0.0005)


#M2 Difference in P3 Latency Trier vs. Rest
#Average together ROI sites
Narrow.ROI_FC_Rest_T_pre_P3_Con_Latency.df <-data.frame(FC_Rest_T_pre_P3_Con_CPZ_Latency, FC_Rest_T_pre_P3_Con_CP1_Latency, FC_Rest_T_pre_P3_Con_CP2_Latency)
Narrow.ROI_FC_Rest_T_pre_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_T_pre_P3_Con_Latency.df, na.rm = TRUE)
Narrow.ROI_FC_Rest_T_po_P3_Con_Latency.df <-data.frame(FC_Rest_T_po_P3_Con_CPZ_Latency, FC_Rest_T_po_P3_Con_CP1_Latency, FC_Rest_T_po_P3_Con_CP2_Latency)
Narrow.ROI_FC_Rest_T_po_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_T_po_P3_Con_Latency.df, na.rm = TRUE)
Narrow.ROI_FC_Rest_T_pre_P3_Inc_Latency.df <-data.frame(FC_Rest_T_pre_P3_Inc_CPZ_Latency, FC_Rest_T_pre_P3_Inc_CP1_Latency, FC_Rest_T_pre_P3_Inc_CP2_Latency)
Narrow.ROI_FC_Rest_T_pre_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_T_pre_P3_Inc_Latency.df, na.rm = TRUE)
Narrow.ROI_FC_Rest_T_po_P3_Inc_Latency.df <-data.frame(FC_Rest_T_po_P3_Inc_CPZ_Latency, FC_Rest_T_po_P3_Inc_CP1_Latency, FC_Rest_T_po_P3_Inc_CP2_Latency)
Narrow.ROI_FC_Rest_T_po_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_T_po_P3_Inc_Latency.df, na.rm = TRUE)
Narrow.ROI_FC_T_pre_P3_Con_Latency.df <-data.frame(FC_T_pre_P3_Con_CPZ_Latency, FC_T_pre_P3_Con_CP1_Latency, FC_T_pre_P3_Con_CP2_Latency)
Narrow.ROI_FC_T_pre_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_T_pre_P3_Con_Latency.df, na.rm = TRUE)
Narrow.ROI_FC_T_po_P3_Con_Latency.df <-data.frame(FC_T_po_P3_Con_CPZ_Latency, FC_T_po_P3_Con_CP1_Latency, FC_T_po_P3_Con_CP2_Latency)
Narrow.ROI_FC_T_po_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_T_po_P3_Con_Latency.df, na.rm = TRUE)
Narrow.ROI_FC_T_pre_P3_Inc_Latency.df <-data.frame(FC_T_pre_P3_Inc_CPZ_Latency, FC_T_pre_P3_Inc_CP1_Latency, FC_T_pre_P3_Inc_CP2_Latency)
Narrow.ROI_FC_T_pre_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_T_pre_P3_Inc_Latency.df, na.rm = TRUE)
Narrow.ROI_FC_T_po_P3_Inc_Latency.df <-data.frame(FC_T_po_P3_Inc_CPZ_Latency, FC_T_po_P3_Inc_CP1_Latency, FC_T_po_P3_Inc_CP2_Latency)
Narrow.ROI_FC_T_po_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_T_po_P3_Inc_Latency.df, na.rm = TRUE)
#Create difference scores of ROI and conditions
Diff_PrePost_R_Congruent_P3Lat <-Narrow.ROI_FC_Rest_T_po_P3_Con_Latency.mean-Narrow.ROI_FC_Rest_T_pre_P3_Con_Latency.mean
Diff_PrePost_R_Incongruent_P3Lat<-Narrow.ROI_FC_Rest_T_po_P3_Inc_Latency.mean-Narrow.ROI_FC_Rest_T_pre_P3_Inc_Latency.mean
Diff_PrePost_T_Congruent_P3Lat <-Narrow.ROI_FC_T_po_P3_Con_Latency.mean-Narrow.ROI_FC_T_pre_P3_Con_Latency.mean
Diff_PrePost_T_Incongruent_P3Lat<-Narrow.ROI_FC_T_po_P3_Inc_Latency.mean-Narrow.ROI_FC_T_pre_P3_Inc_Latency.mean
#create dataframe 
M2_P3.Latency.df <- data.frame(SubjectID, Diff_PrePost_R_Congruent_P3Lat, Diff_PrePost_R_Incongruent_P3Lat, Diff_PrePost_T_Congruent_P3Lat, Diff_PrePost_T_Incongruent_P3Lat)
#Summarize
summary(M2_P3.Latency.df)
#Melt dataframe
longformat.M2_P3.Latency.df <- melt(M2_P3.Latency.df, id = "SubjectID", variable.name = "Condition")
View(longformat.M2_P3.Latency.df)
#Visualize
ggplot(longformat.M2_P3.Latency.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean P3 Latency Difference Post-Pre for Model 1 Trier vs. Rest", x ="Intervention", y = "Mean Latency Difference ms")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.M2_P3.Latency.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
M2_P3.Latency.df %>%
  shapiro_test(Diff_PrePost_R_Congruent_P3Lat, Diff_PrePost_R_Incongruent_P3Lat, Diff_PrePost_T_Congruent_P3Lat, Diff_PrePost_T_Incongruent_P3Lat)
histogram(M2_P3.Latency.df$Diff_PrePost_R_Congruent_P3Lat)
#does not pass normality, used non-parametric
ggwithinstats(data = longformat.M2_P3.Latency.df, x = Condition, y = value, type = "nonparametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_kendalls_w(0.01)



--------------------------------------------------------------------------------

# BRAIN FUNCTION OUTCOMES - Resting Posterior Alpha -----------------------
#Call variables
P1_Alpha_E_Pre <- df$P1_Alpha_E_Pre
PZ_Alpha_E_Pre <- df$PZ_Alpha_E_Pre
P2_Alpha_E_Pre <- df$P2_Alpha_E_Pre
PO3_Alpha_E_Pre <- df$PO3_Alpha_E_Pre
POZ_Alpha_E_Pre <- df$POZ_Alpha_E_Pre
PO4_Alpha_E_Pre <- df$PO4_Alpha_E_Pre
P1_Alpha_E_Post <- df$P1_Alpha_E_Post
PZ_Alpha_E_Post <- df$PZ_Alpha_E_Post
P2_Alpha_E_Post <- df$P2_Alpha_E_Post
PO3_Alpha_E_Post <- df$PO3_Alpha_E_Post
POZ_Alpha_E_Post <- df$POZ_Alpha_E_Post
PO4_Alpha_E_Post <- df$PO4_Alpha_E_Post
P1_Alpha_Rest_E_Pre <- df$P1_Alpha_Rest_E_Pre
PZ_Alpha_Rest_E_Pre <- df$PZ_Alpha_Rest_E_Pre
P2_Alpha_Rest_E_Pre <- df$P2_Alpha_Rest_E_Pre
PO3_Alpha_Rest_E_Pre <- df$PO3_Alpha_Rest_E_Pre
POZ_Alpha_Rest_E_Pre <- df$POZ_Alpha_Rest_E_Pre
PO4_Alpha_Rest_E_Pre <- df$PO4_Alpha_Rest_E_Pre
P1_Alpha_Rest_E_Post <- df$P1_Alpha_Rest_E_Post
PZ_Alpha_Rest_E_Post <- df$PZ_Alpha_Rest_E_Post
P2_Alpha_Rest_E_Post <- df$P2_Alpha_Rest_E_Post
PO3_Alpha_Rest_E_Post <- df$PO3_Alpha_Rest_E_Post
POZ_Alpha_Rest_E_Post <- df$POZ_Alpha_Rest_E_Post
PO4_Alpha_Rest_E_Post <- df$PO4_Alpha_Rest_E_Post
P1_Alpha_T_Pre <- df$P1_Alpha_T_Pre
PZ_Alpha_T_Pre <- df$PZ_Alpha_T_Pre
P2_Alpha_T_Pre <- df$P2_Alpha_T_Pre
PO3_Alpha_T_Pre <- df$PO3_Alpha_T_Pre
POZ_Alpha_T_Pre <- df$POZ_Alpha_T_Pre
PO4_Alpha_T_Pre <- df$PO4_Alpha_T_Pre
P1_Alpha_T_Post <- df$P1_Alpha_T_Post
PZ_Alpha_T_Post <- df$PZ_Alpha_T_Post
P2_Alpha_T_Post <- df$P2_Alpha_T_Post
PO3_Alpha_T_Post <- df$PO3_Alpha_T_Post
POZ_Alpha_T_Post <- df$POZ_Alpha_T_Post
PO4_Alpha_T_Post <- df$PO4_Alpha_T_Post
P1_Alpha_Rest_T_Pre <- df$P1_Alpha_Rest_T_Pre
PZ_Alpha_Rest_T_Pre <- df$PZ_Alpha_Rest_T_Pre
P2_Alpha_Rest_T_Pre <- df$P2_Alpha_Rest_T_Pre
PO3_Alpha_Rest_T_Pre <- df$PO3_Alpha_Rest_T_Pre
POZ_Alpha_Rest_T_Pre <- df$POZ_Alpha_Rest_T_Pre
PO4_Alpha_Rest_T_Pre <- df$PO4_Alpha_Rest_T_Pre
P1_Alpha_Rest_T_Post <- df$P1_Alpha_Rest_T_Post
PZ_Alpha_Rest_T_Post <- df$PZ_Alpha_Rest_T_Post
P2_Alpha_Rest_T_Post <- df$P2_Alpha_Rest_T_Post
PO3_Alpha_Rest_T_Post <- df$PO3_Alpha_Rest_T_Post
POZ_Alpha_Rest_T_Post <- df$POZ_Alpha_Rest_T_Post
PO4_Alpha_Rest_T_Post <- df$PO4_Alpha_Rest_T_Post

#M1 Difference in Resting Posterior Alpha between Exercise vs. Rest 
##Average together ROI sites
ROI_FC_Rest_E_pre_Alpha.df <-data.frame(P1_Alpha_Rest_E_Pre, PZ_Alpha_Rest_E_Pre, P2_Alpha_Rest_E_Pre, PO3_Alpha_Rest_E_Pre, POZ_Alpha_Rest_E_Pre, PO4_Alpha_Rest_E_Pre)
ROI_FC_Rest_E_pre_Alpha.mean <- rowMeans(ROI_FC_Rest_E_pre_Alpha.df)
ROI_FC_Rest_E_post_Alpha.df <-data.frame(P1_Alpha_Rest_E_Post, PZ_Alpha_Rest_E_Post, P2_Alpha_Rest_E_Post, PO3_Alpha_Rest_E_Post, POZ_Alpha_Rest_E_Post, PO4_Alpha_Rest_E_Post)
ROI_FC_Rest_E_post_Alpha.mean <- rowMeans(ROI_FC_Rest_E_post_Alpha.df)
ROI_FC_E_pre_Alpha.df <-data.frame(P1_Alpha_E_Pre, PZ_Alpha_E_Pre, P2_Alpha_E_Pre, PO3_Alpha_E_Pre, POZ_Alpha_E_Pre, PO4_Alpha_E_Pre)
ROI_FC_E_pre_Alpha.mean <- rowMeans(ROI_FC_E_pre_Alpha.df)
ROI_FC_E_post_Alpha.df <-data.frame(P1_Alpha_E_Post, PZ_Alpha_E_Post, P2_Alpha_E_Post, PO3_Alpha_E_Post, POZ_Alpha_E_Pre, PO4_Alpha_E_Post)
ROI_FC_E_post_Alpha.mean <- rowMeans(ROI_FC_E_post_Alpha.df)
#Create difference scores of ROI and conditions
Diff_PrePost_R_Alpha <- ROI_FC_Rest_E_post_Alpha.mean-ROI_FC_Rest_E_pre_Alpha.mean
Diff_PrePost_E_Alpha <- ROI_FC_E_post_Alpha.mean-ROI_FC_E_pre_Alpha.mean
#create dataframe 
M1_Alpha.df <- data.frame(SubjectID, Diff_PrePost_R_Alpha, Diff_PrePost_E_Alpha)
#Summarize
summary(M1_Alpha.df)
#Melt dataframe
longformat.M1_Alpha.df <- melt(M1_Alpha.df, id = "SubjectID", variable.name = "Condition")
View(M1_Alpha.df)
#Visualize
ggplot(longformat.M1_Alpha.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Alpha Difference Post-Pre for Model 1 Trier vs. Rest", x ="Intervention", y = "Mean Log Power Difference")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.M1_Alpha.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
M1_Alpha.df %>%
  shapiro_test(Diff_PrePost_R_Alpha, Diff_PrePost_E_Alpha)
histogram(M1_Alpha.df$Diff_PrePost_R_Alpha)
#Passes Normality, parametric used
ggwithinstats(data = longformat.M1_Alpha.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_hedges_g(-.19)


#M2 Difference in Resting Posterior Alpha between Trier vs. Rest 
##Average together ROI sites
ROI_FC_Rest_T_pre_Alpha.df <-data.frame(P1_Alpha_Rest_T_Pre, PZ_Alpha_Rest_T_Pre, P2_Alpha_Rest_T_Pre, PO3_Alpha_Rest_T_Pre, POZ_Alpha_Rest_T_Pre, PO4_Alpha_Rest_T_Pre)
ROI_FC_Rest_T_pre_Alpha.mean <- rowMeans(ROI_FC_Rest_T_pre_Alpha.df)
ROI_FC_Rest_T_post_Alpha.df <-data.frame(P1_Alpha_Rest_T_Post, PZ_Alpha_Rest_T_Post, P2_Alpha_Rest_T_Post, PO3_Alpha_Rest_T_Post, POZ_Alpha_Rest_T_Post, PO4_Alpha_Rest_T_Post)
ROI_FC_Rest_T_post_Alpha.mean <- rowMeans(ROI_FC_Rest_T_post_Alpha.df)
ROI_FC_T_pre_Alpha.df <-data.frame(P1_Alpha_T_Pre, PZ_Alpha_T_Pre, P2_Alpha_T_Pre, PO3_Alpha_T_Pre, POZ_Alpha_T_Pre, PO4_Alpha_T_Pre)
ROI_FC_T_pre_Alpha.mean <- rowMeans(ROI_FC_T_pre_Alpha.df)
ROI_FC_T_post_Alpha.df <-data.frame(P1_Alpha_T_Post, PZ_Alpha_T_Post, P2_Alpha_T_Post, PO3_Alpha_T_Post, POZ_Alpha_T_Pre, PO4_Alpha_T_Post)
ROI_FC_T_post_Alpha.mean <- rowMeans(ROI_FC_T_post_Alpha.df)
#Create difference scores of ROI and conditions
Diff_PrePost_R_Alpha <- ROI_FC_Rest_T_post_Alpha.mean-ROI_FC_Rest_T_pre_Alpha.mean
Diff_PrePost_T_Alpha <- ROI_FC_T_post_Alpha.mean-ROI_FC_T_pre_Alpha.mean
#create dataframe 
M2_Alpha.df <- data.frame(SubjectID, Diff_PrePost_R_Alpha, Diff_PrePost_T_Alpha)
#Summarize
summary(M2_Alpha.df)
#Melt dataframe
longformat.M2_Alpha.df <- melt(M2_Alpha.df, id = "SubjectID", variable.name = "Condition")
View(M2_Alpha.df)
#Visualize
ggplot(longformat.M2_Alpha.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Alpha Difference Post-Pre for Model 1 Trier vs. Rest", x ="Intervention", y = "Mean Log Power Difference")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.M2_Alpha.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
M2_Alpha.df %>%
  shapiro_test(Diff_PrePost_R_Alpha, Diff_PrePost_T_Alpha)
histogram(M2_Alpha.df$Diff_PrePost_R_Alpha)
#Passes Normality, parametric used
ggwithinstats(data = longformat.M2_Alpha.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_hedges_g(.08)



# BRAIN FUNCTION OUTCOMES - Resting MidFrontal Theta ----------------------
#Call Variables 
SubjectID <- (df$SubjectID)
F1_Theta_E_Pre <- (df$F1_Theta_E_Pre)
FCZ_Theta_E_Pre <- df$FCZ_Theta_E_Pre
FZ_Theta_E_Pre <- df$FZ_Theta_E_Pre
FC1_Theta_E_Pre <- df$FC1_Theta_E_Pre
FC2_Theta_E_Pre <- df$FC2_Theta_E_Pre
C1_Theta_E_Pre <- df$C1_Theta_E_Pre
C2_Theta_E_Pre <- df$C2_Theta_E_Pre
F1_Theta_E_Post <- df$F1_Theta_E_Post
FCZ_Theta_E_Post <- df$FCZ_Theta_E_Post
FZ_Theta_E_Post <- df$FZ_Theta_E_Post
FC1_Theta_E_Post <- df$FC1_Theta_E_Post
FC2_Theta_E_Post <- df$FC2_Theta_E_Post
C1_Theta_E_Post <- df$C1_Theta_E_Post
C2_Theta_E_Post <- df$C2_Theta_E_Post
F1_Theta_Rest_E_Pre <- df$F1_Theta_Rest_E_Pre
FCZ_Theta_Rest_E_Pre <- df$FCZ_Theta_Rest_E_Pre
FZ_Theta_Rest_E_Pre <- df$FZ_Theta_Rest_E_Pre
FC1_Theta_Rest_E_Pre <- df$FC1_Theta_Rest_E_Pre
FC2_Theta_Rest_E_Pre <- df$FC2_Theta_Rest_E_Pre
C1_Theta_Rest_E_Pre <- df$C1_Theta_Rest_E_Pre
C2_Theta_Rest_E_Pre <- df$C2_Theta_Rest_E_Pre
F1_Theta_Rest_E_Post <- df$F1_Theta_Rest_E_Post
FCZ_Theta_Rest_E_Post <- df$FCZ_Theta_Rest_E_Post
FZ_Theta_Rest_E_Post <- df$FZ_Theta_Rest_E_Post
FC1_Theta_Rest_E_Post <- df$FC1_Theta_Rest_E_Post
FC2_Theta_Rest_E_Post <- df$FC2_Theta_Rest_E_Post
C1_Theta_Rest_E_Post <- df$C1_Theta_Rest_E_Post
C2_Theta_Rest_E_Post <- df$C2_Theta_Rest_E_Post
F1_Theta_T_Pre <- df$F1_Theta_T_Pre
FCZ_Theta_T_Pre <- df$FCZ_Theta_T_Pre
FZ_Theta_T_Pre <- df$FZ_Theta_T_Pre
FC1_Theta_T_Pre <- df$FC1_Theta_T_Pre
FC2_Theta_T_Pre <- df$FC2_Theta_T_Pre
C1_Theta_T_Pre <- df$C1_Theta_T_Pre
C2_Theta_T_Pre <- df$C2_Theta_T_Pre
F1_Theta_T_Post <- df$F1_Theta_T_Post
FCZ_Theta_T_Post <- df$FCZ_Theta_T_Post
FZ_Theta_T_Post <- df$FZ_Theta_T_Post
FC1_Theta_T_Post <- df$FC1_Theta_T_Post
FC2_Theta_T_Post <- df$FC2_Theta_T_Post
C1_Theta_T_Post <- df$C1_Theta_T_Post
C2_Theta_T_Post <- df$C2_Theta_T_Post
F1_Theta_Rest_T_Pre <- df$F1_Theta_Rest_T_Pre
FCZ_Theta_Rest_T_Pre <- df$FCZ_Theta_Rest_T_Pre
FZ_Theta_Rest_T_Pre <- df$FZ_Theta_Rest_T_Pre
FC1_Theta_Rest_T_Pre <- df$FC1_Theta_Rest_T_Pre
FC2_Theta_Rest_T_Pre <- df$FC2_Theta_Rest_T_Pre
C1_Theta_Rest_T_Pre <- df$C1_Theta_Rest_T_Pre
C2_Theta_Rest_T_Pre <- df$C2_Theta_Rest_T_Pre
F1_Theta_Rest_T_Post <- df$F1_Theta_Rest_T_Post
FCZ_Theta_Rest_T_Post <- df$FCZ_Theta_Rest_T_Post
FZ_Theta_Rest_T_Post <- df$FZ_Theta_Rest_T_Post
FC1_Theta_Rest_T_Post <- df$FC1_Theta_Rest_T_Post
FC2_Theta_Rest_T_Post <- df$FC2_Theta_Rest_T_Post
C1_Theta_Rest_T_Post <- df$C1_Theta_Rest_T_Post
C2_Theta_Rest_T_Post <- df$C2_Theta_Rest_T_Post

#M1 Difference in Resting MidFrontal Theta between Exercise vs. Rest 
#Average together Wide ROI sites
Wide.ROI_FC_E_pre_Theta.df <-data.frame(F1_Theta_E_Pre, FCZ_Theta_E_Pre, FZ_Theta_E_Pre, FC1_Theta_E_Pre, FC2_Theta_E_Pre, C1_Theta_E_Pre)
Wide.ROI_FC_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_E_pre_Theta.df)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_E_Pre, FCZ_Theta_E_Post, FZ_Theta_E_Post, FC1_Theta_E_Post, FC2_Theta_E_Pre, C1_Theta_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_pre_Theta.df <-data.frame(F1_Theta_Rest_E_Pre, FCZ_Theta_Rest_E_Pre, FZ_Theta_Rest_E_Pre, FC1_Theta_Rest_E_Pre, FC2_Theta_Rest_E_Pre, C1_Theta_Rest_E_Pre)
Wide.ROI_FC_Rest_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_pre_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_Rest_E_Post, FCZ_Theta_Rest_E_Post, FZ_Theta_Rest_E_Post, FC1_Theta_Rest_E_Post, FC2_Theta_Rest_E_Post, C1_Theta_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#Create difference scores of ROI and conditions
Wide.Diff_PrePost_R_Theta <- Wide.ROI_FC_Rest_E_post_Theta.mean-Wide.ROI_FC_Rest_E_pre_Theta.mean
Wide.Diff_PrePost_E_Theta <- Wide.ROI_FC_E_post_Theta.mean-Wide.ROI_FC_E_pre_Theta.mean
#create dataframe 
Wide.M1_Theta.df <- data.frame(SubjectID, Wide.Diff_PrePost_R_Theta, Wide.Diff_PrePost_E_Theta)
#Summarize
summary(Wide.M1_Theta.df)
#Melt dataframe
longformat.Wide.M1_Theta.df <- melt(Wide.M1_Theta.df, id = "SubjectID", variable.name = "Condition")
View(M1_Theta.df)
#Visualize
ggplot(longformat.Wide.M1_Theta.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", Theta = 0.5)+
  labs(title="Theta Difference Post-Pre for Model 1 Exercise vs. Rest", x ="Intervention", y = "Mean Log Power Difference")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.Wide.M1_Theta.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
Wide.M1_Theta.df %>%
  shapiro_test(Wide.Diff_PrePost_R_Theta, Wide.Diff_PrePost_E_Theta)
histogram(Wide.M1_Theta.df$Wide.Diff_PrePost_R_Theta)
#Passes Normality, parametric used
ggwithinstats(data = longformat.Wide.M1_Theta.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_hedges_g(-.10)


#M2 Difference in Resting MidFrontal Theta between Trier vs. Rest 
#Average together Wide ROI sites
Wide.ROI_FC_T_pre_Theta.df <-data.frame(F1_Theta_T_Pre, FCZ_Theta_T_Pre, FZ_Theta_T_Pre, FC1_Theta_T_Pre, FC2_Theta_T_Pre, C1_Theta_T_Pre)
Wide.ROI_FC_T_pre_Theta.mean <- rowMeans(Wide.ROI_FC_T_pre_Theta.df)
Wide.ROI_FC_T_post_Theta.df <-data.frame(F1_Theta_T_Pre, FCZ_Theta_T_Post, FZ_Theta_T_Post, FC1_Theta_T_Post, FC2_Theta_T_Pre, C1_Theta_T_Post)
Wide.ROI_FC_T_post_Theta.mean <- rowMeans(Wide.ROI_FC_T_post_Theta.df)
Wide.ROI_FC_Rest_T_pre_Theta.df <-data.frame(F1_Theta_Rest_T_Pre, FCZ_Theta_Rest_T_Pre, FZ_Theta_Rest_T_Pre, FC1_Theta_Rest_T_Pre, FC2_Theta_Rest_T_Pre, C1_Theta_Rest_T_Pre)
Wide.ROI_FC_Rest_T_pre_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_T_pre_Theta.df)
Wide.ROI_FC_Rest_T_post_Theta.df <-data.frame(F1_Theta_Rest_T_Post, FCZ_Theta_Rest_T_Post, FZ_Theta_Rest_T_Post, FC1_Theta_Rest_T_Post, FC2_Theta_Rest_T_Post, C1_Theta_Rest_T_Post)
Wide.ROI_FC_Rest_T_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_T_post_Theta.df)
#Create difference scores of ROI and conditions
Wide.Diff_PrePost_R_Theta <- Wide.ROI_FC_Rest_T_post_Theta.mean-Wide.ROI_FC_Rest_T_pre_Theta.mean
Wide.Diff_PrePost_T_Theta <- Wide.ROI_FC_T_post_Theta.mean-Wide.ROI_FC_T_pre_Theta.mean
#create dataframe 
Wide.M2_Theta.df <- data.frame(SubjectID, Wide.Diff_PrePost_R_Theta, Wide.Diff_PrePost_T_Theta)
#Summarize
summary(Wide.M2_Theta.df)
#Melt dataframe
longformat.Wide.M2_Theta.df <- melt(Wide.M2_Theta.df, id = "SubjectID", variable.name = "Condition")
View(Wide.M2_Theta.df)
#Visualize
ggplot(longformat.Wide.M2_Theta.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", Theta = 0.5)+
  labs(title="Theta Difference Post-Pre for Model 1 Trier vs. Rest", x ="Intervention", y = "Mean Log Power Difference")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.Wide.M2_Theta.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
Wide.M2_Theta.df %>%
  shapiro_test(Wide.Diff_PrePost_R_Theta, Wide.Diff_PrePost_T_Theta)
histogram(Wide.M2_Theta.df$Wide.Diff_PrePost_R_Theta)
#Passes Normality, parametric used
ggwithinstats(data = longformat.Wide.M2_Theta.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_hedges_g(.26)


#M1 Difference in Resting MidFrontal Theta between Exercise vs. Rest 
#Create difference scores of ROI and conditions
Narrow.Diff_PrePost_R_Theta <- FCZ_Theta_Rest_E_Post-FCZ_Theta_Rest_E_Pre
Narrow.Diff_PrePost_E_Theta <- FCZ_Theta_E_Post-FCZ_Theta_E_Pre
#create dataframe 
Narrow.M1_Theta.df <- data.frame(SubjectID, Narrow.Diff_PrePost_R_Theta, Narrow.Diff_PrePost_E_Theta)
#Summarize
summary(Narrow.M1_Theta.df)
#Melt dataframe
longformat.Narrow.M1_Theta.df <- melt(Narrow.M1_Theta.df, id = "SubjectID", variable.name = "Condition")
View(Narrow.M1_Theta.df)
#Visualize
ggplot(longformat.Narrow.M1_Theta.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", Theta = 0.5)+
  labs(title="Theta Difference Post-Pre for Model 1 Exercise vs. Rest", x ="Intervention", y = "Mean Log Power Difference")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.Narrow.M1_Theta.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using Narrow format
Narrow.M1_Theta.df %>%
  shapiro_test(Narrow.Diff_PrePost_R_Theta, Narrow.Diff_PrePost_E_Theta)
histogram(Narrow.M1_Theta.df$Narrow.Diff_PrePost_R_Theta)
#Passes Normality, parametric used
ggwithinstats(data = longformat.Narrow.M1_Theta.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_hedges_g(-.10)


#M2 Difference in Resting MidFrontal Theta between Trier vs. Rest 
#Create difference scores of ROI and conditions
Narrow.Diff_PrePost_R_Theta <- FCZ_Theta_Rest_T_Post-FCZ_Theta_Rest_T_Pre
Narrow.Diff_PrePost_T_Theta <- FCZ_Theta_T_Post-FCZ_Theta_T_Pre
#create dataframe 
Narrow.M2_Theta.df <- data.frame(SubjectID, Narrow.Diff_PrePost_R_Theta, Narrow.Diff_PrePost_T_Theta)
#Summarize
summary(Narrow.M2_Theta.df)
#Melt dataframe
longformat.Narrow.M2_Theta.df <- melt(Narrow.M2_Theta.df, id = "SubjectID", variable.name = "Condition")
View(Narrow.M2_Theta.df)
#Visualize
ggplot(longformat.Narrow.M2_Theta.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", Theta = 0.5)+
  labs(title="Theta Difference Post-Pre for Model 1 Trier vs. Rest", x ="Intervention", y = "Mean Log Power Difference")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.Narrow.M2_Theta.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using Narrow format
Narrow.M2_Theta.df %>%
  shapiro_test(Narrow.Diff_PrePost_R_Theta, Narrow.Diff_PrePost_T_Theta)
histogram(Narrow.M2_Theta.df$Narrow.Diff_PrePost_R_Theta)
#Passes Normality, parametric used
ggwithinstats(data = longformat.Narrow.M2_Theta.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_hedges_g(.15)

--------------------------------------------------------------------------------
  -----------------------------------------------------------------------------

# COGNITIVE AND BRAIN FUNCTION OUTCOMES - Alpha and P3 Peak ----------------
#Call Variables 
SubjectID<-df$SubjectID
P1_Alpha_E_Pre <- df$P1_Alpha_E_Pre
PZ_Alpha_E_Pre <- df$PZ_Alpha_E_Pre
P2_Alpha_E_Pre <- df$P2_Alpha_E_Pre
PO3_Alpha_E_Pre <- df$PO3_Alpha_E_Pre
POZ_Alpha_E_Pre <- df$POZ_Alpha_E_Pre
PO4_Alpha_E_Pre <- df$PO4_Alpha_E_Pre
P1_Alpha_E_Post <- df$P1_Alpha_E_Post
PZ_Alpha_E_Post <- df$PZ_Alpha_E_Post
P2_Alpha_E_Post <- df$P2_Alpha_E_Post
PO3_Alpha_E_Post <- df$PO3_Alpha_E_Post
POZ_Alpha_E_Post <- df$POZ_Alpha_E_Post
PO4_Alpha_E_Post <- df$PO4_Alpha_E_Post
P1_Alpha_Rest_E_Pre <- df$P1_Alpha_Rest_E_Pre
PZ_Alpha_Rest_E_Pre <- df$PZ_Alpha_Rest_E_Pre
P2_Alpha_Rest_E_Pre <- df$P2_Alpha_Rest_E_Pre
PO3_Alpha_Rest_E_Pre <- df$PO3_Alpha_Rest_E_Pre
POZ_Alpha_Rest_E_Pre <- df$POZ_Alpha_Rest_E_Pre
PO4_Alpha_Rest_E_Pre <- df$PO4_Alpha_Rest_E_Pre
P1_Alpha_Rest_E_Post <- df$P1_Alpha_Rest_E_Post
PZ_Alpha_Rest_E_Post <- df$PZ_Alpha_Rest_E_Post
P2_Alpha_Rest_E_Post <- df$P2_Alpha_Rest_E_Post
PO3_Alpha_Rest_E_Post <- df$PO3_Alpha_Rest_E_Post
POZ_Alpha_Rest_E_Post <- df$POZ_Alpha_Rest_E_Post
PO4_Alpha_Rest_E_Post <- df$PO4_Alpha_Rest_E_Post
P1_Alpha_T_Pre <- df$P1_Alpha_T_Pre
PZ_Alpha_T_Pre <- df$PZ_Alpha_T_Pre
P2_Alpha_T_Pre <- df$P2_Alpha_T_Pre
PO3_Alpha_T_Pre <- df$PO3_Alpha_T_Pre
POZ_Alpha_T_Pre <- df$POZ_Alpha_T_Pre
PO4_Alpha_T_Pre <- df$PO4_Alpha_T_Pre
P1_Alpha_T_Post <- df$P1_Alpha_T_Post
PZ_Alpha_T_Post <- df$PZ_Alpha_T_Post
P2_Alpha_T_Post <- df$P2_Alpha_T_Post
PO3_Alpha_T_Post <- df$PO3_Alpha_T_Post
POZ_Alpha_T_Post <- df$POZ_Alpha_T_Post
PO4_Alpha_T_Post <- df$PO4_Alpha_T_Post
P1_Alpha_Rest_T_Pre <- df$P1_Alpha_Rest_T_Pre
PZ_Alpha_Rest_T_Pre <- df$PZ_Alpha_Rest_T_Pre
P2_Alpha_Rest_T_Pre <- df$P2_Alpha_Rest_T_Pre
PO3_Alpha_Rest_T_Pre <- df$PO3_Alpha_Rest_T_Pre
POZ_Alpha_Rest_T_Pre <- df$POZ_Alpha_Rest_T_Pre
PO4_Alpha_Rest_T_Pre <- df$PO4_Alpha_Rest_T_Pre
P1_Alpha_Rest_T_Post <- df$P1_Alpha_Rest_T_Post
PZ_Alpha_Rest_T_Post <- df$PZ_Alpha_Rest_T_Post
P2_Alpha_Rest_T_Post <- df$P2_Alpha_Rest_T_Post
PO3_Alpha_Rest_T_Post <- df$PO3_Alpha_Rest_T_Post
POZ_Alpha_Rest_T_Post <- df$POZ_Alpha_Rest_T_Post
PO4_Alpha_Rest_T_Post <- df$PO4_Alpha_Rest_T_Post

FC_Rest_E_pre_P3_Con_CPZ_Amplitude<- df$FC_Rest_E_pre_P3_Con_CPZ_Amplitude
FC_Rest_E_pre_P3_Con_CP1_Amplitude<- df$FC_Rest_E_pre_P3_Con_CP1_Amplitude
FC_Rest_E_pre_P3_Con_CP2_Amplitude<- df$FC_Rest_E_pre_P3_Con_CP2_Amplitude
FC_Rest_E_po_P3_Con_CPZ_Amplitude<- df$FC_Rest_E_po_P3_Con_CPZ_Amplitude
FC_Rest_E_po_P3_Con_CP1_Amplitude<- df$FC_Rest_E_po_P3_Con_CP1_Amplitude
FC_Rest_E_po_P3_Con_CP2_Amplitude<- df$FC_Rest_E_po_P3_Con_CP2_Amplitude
FC_Rest_E_pre_P3_Inc_CPZ_Amplitude<- df$FC_Rest_E_pre_P3_Inc_CPZ_Amplitude
FC_Rest_E_pre_P3_Inc_CP1_Amplitude<- df$FC_Rest_E_pre_P3_Inc_CP1_Amplitude
FC_Rest_E_pre_P3_Inc_CP2_Amplitude<- df$FC_Rest_E_pre_P3_Inc_CP2_Amplitude
FC_Rest_E_po_P3_Inc_CPZ_Amplitude<- df$FC_Rest_E_po_P3_Inc_CPZ_Amplitude
FC_Rest_E_po_P3_Inc_CP1_Amplitude<- df$FC_Rest_E_po_P3_Inc_CP1_Amplitude
FC_Rest_E_po_P3_Inc_CP2_Amplitude<- df$FC_Rest_E_po_P3_Inc_CP2_Amplitude
FC_E_pre_P3_Con_CPZ_Amplitude<- df$FC_E_pre_P3_Con_CPZ_Amplitude
FC_E_pre_P3_Con_CP1_Amplitude<- df$FC_E_pre_P3_Con_CP1_Amplitude
FC_E_pre_P3_Con_CP2_Amplitude<- df$FC_E_pre_P3_Con_CP2_Amplitude
FC_E_po_P3_Con_CPZ_Amplitude<- df$FC_E_po_P3_Con_CPZ_Amplitude
FC_E_po_P3_Con_CP1_Amplitude<- df$FC_E_po_P3_Con_CP1_Amplitude
FC_E_po_P3_Con_CP2_Amplitude<- df$FC_E_po_P3_Con_CP2_Amplitude
FC_E_pre_P3_Inc_CPZ_Amplitude<- df$FC_E_pre_P3_Inc_CPZ_Amplitude
FC_E_pre_P3_Inc_CP1_Amplitude<- df$FC_E_pre_P3_Inc_CP1_Amplitude
FC_E_pre_P3_Inc_CP2_Amplitude<- df$FC_E_pre_P3_Inc_CP2_Amplitude
FC_E_po_P3_Inc_CPZ_Amplitude<- df$FC_E_po_P3_Inc_CPZ_Amplitude
FC_E_po_P3_Inc_CP1_Amplitude<- df$FC_E_po_P3_Inc_CP1_Amplitude
FC_E_po_P3_Inc_CP2_Amplitude<- df$FC_E_po_P3_Inc_CP2_Amplitude
FC_Rest_T_pre_P3_Con_CPZ_Amplitude<- df$FC_Rest_T_pre_P3_Con_CPZ_Amplitude
FC_Rest_T_pre_P3_Con_CP1_Amplitude<- df$FC_Rest_T_pre_P3_Con_CP1_Amplitude
FC_Rest_T_pre_P3_Con_CP2_Amplitude<- df$FC_Rest_T_pre_P3_Con_CP2_Amplitude
FC_Rest_T_po_P3_Con_CPZ_Amplitude<- df$FC_Rest_T_po_P3_Con_CPZ_Amplitude
FC_Rest_T_po_P3_Con_CP1_Amplitude<- df$FC_Rest_T_po_P3_Con_CP1_Amplitude
FC_Rest_T_po_P3_Con_CP2_Amplitude<- df$FC_Rest_T_po_P3_Con_CP2_Amplitude
FC_Rest_T_pre_P3_Inc_CPZ_Amplitude<- df$FC_Rest_T_pre_P3_Inc_CPZ_Amplitude
FC_Rest_T_pre_P3_Inc_CP1_Amplitude<- df$FC_Rest_T_pre_P3_Inc_CP1_Amplitude
FC_Rest_T_pre_P3_Inc_CP2_Amplitude<- df$FC_Rest_T_pre_P3_Inc_CP2_Amplitude
FC_Rest_T_po_P3_Inc_CPZ_Amplitude<- df$FC_Rest_T_po_P3_Inc_CPZ_Amplitude
FC_Rest_T_po_P3_Inc_CP1_Amplitude<- df$FC_Rest_T_po_P3_Inc_CP1_Amplitude
FC_Rest_T_po_P3_Inc_CP2_Amplitude<- df$FC_Rest_T_po_P3_Inc_CP2_Amplitude
FC_T_pre_P3_Con_CPZ_Amplitude<- df$FC_T_pre_P3_Con_CPZ_Amplitude
FC_T_pre_P3_Con_CP1_Amplitude<- df$FC_T_pre_P3_Con_CP1_Amplitude
FC_T_pre_P3_Con_CP2_Amplitude<- df$FC_T_pre_P3_Con_CP2_Amplitude
FC_T_po_P3_Con_CPZ_Amplitude<- df$FC_T_po_P3_Con_CPZ_Amplitude
FC_T_po_P3_Con_CP1_Amplitude<- df$FC_T_po_P3_Con_CP1_Amplitude
FC_T_po_P3_Con_CP2_Amplitude<- df$FC_T_po_P3_Con_CP2_Amplitude
FC_T_pre_P3_Inc_CPZ_Amplitude<- df$FC_T_pre_P3_Inc_CPZ_Amplitude
FC_T_pre_P3_Inc_CP1_Amplitude<- df$FC_T_pre_P3_Inc_CP1_Amplitude
FC_T_pre_P3_Inc_CP2_Amplitude<- df$FC_T_pre_P3_Inc_CP2_Amplitude
FC_T_po_P3_Inc_CPZ_Amplitude<- df$FC_T_po_P3_Inc_CPZ_Amplitude
FC_T_po_P3_Inc_CP1_Amplitude<- df$FC_T_po_P3_Inc_CP1_Amplitude
FC_T_po_P3_Inc_CP2_Amplitude<- df$FC_T_po_P3_Inc_CP2_Amplitude

#M1 Resting Posterior Alpha x P3 Peak in Exercise vs. Rest 
##Average together Resting Alpha ROI site
ROI_FC_Rest_E_pre_Alpha.df <-data.frame(P1_Alpha_Rest_E_Pre, PZ_Alpha_Rest_E_Pre, P2_Alpha_Rest_E_Pre, PO3_Alpha_Rest_E_Pre, POZ_Alpha_Rest_E_Pre, PO4_Alpha_Rest_E_Pre)
ROI_FC_Rest_E_pre_Alpha.mean <- rowMeans(ROI_FC_Rest_E_pre_Alpha.df)
ROI_FC_Rest_E_post_Alpha.df <-data.frame(P1_Alpha_Rest_E_Post, PZ_Alpha_Rest_E_Post, P2_Alpha_Rest_E_Post, PO3_Alpha_Rest_E_Post, POZ_Alpha_Rest_E_Post, PO4_Alpha_Rest_E_Post)
ROI_FC_Rest_E_post_Alpha.mean <- rowMeans(ROI_FC_Rest_E_post_Alpha.df)
ROI_FC_E_pre_Alpha.df <-data.frame(P1_Alpha_E_Pre, PZ_Alpha_E_Pre, P2_Alpha_E_Pre, PO3_Alpha_E_Pre, POZ_Alpha_E_Pre, PO4_Alpha_E_Pre)
ROI_FC_E_pre_Alpha.mean <- rowMeans(ROI_FC_E_pre_Alpha.df)
ROI_FC_E_post_Alpha.df <-data.frame(P1_Alpha_E_Post, PZ_Alpha_E_Post, P2_Alpha_E_Post, PO3_Alpha_E_Post, POZ_Alpha_E_Pre, PO4_Alpha_E_Post)
ROI_FC_E_post_Alpha.mean <- rowMeans(ROI_FC_E_post_Alpha.df)
##Average together P3 Peak ROI Site
Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_pre_P3_Con_CPZ_Amplitude, FC_Rest_E_pre_P3_Con_CP1_Amplitude, FC_Rest_E_pre_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.df)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Amplitude, FC_Rest_E_po_P3_Con_CP1_Amplitude, FC_Rest_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df)
Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.df <-data.frame(FC_Rest_E_pre_P3_Inc_CPZ_Amplitude, FC_Rest_E_pre_P3_Inc_CP1_Amplitude, FC_Rest_E_pre_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.df)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Inc_CPZ_Amplitude, FC_Rest_E_po_P3_Inc_CP1_Amplitude, FC_Rest_E_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df)
Narrow.ROI_FC_E_pre_P3_Con_Amplitude.df <-data.frame(FC_E_pre_P3_Con_CPZ_Amplitude, FC_E_pre_P3_Con_CP1_Amplitude, FC_E_pre_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_pre_P3_Con_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.df <-data.frame(FC_E_po_P3_Con_CPZ_Amplitude, FC_E_po_P3_Con_CP1_Amplitude, FC_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Con_Amplitude.df)
Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.df <-data.frame(FC_E_pre_P3_Inc_CPZ_Amplitude, FC_E_pre_P3_Inc_CP1_Amplitude, FC_E_pre_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Inc_Amplitude.df <-data.frame(FC_E_po_P3_Inc_CPZ_Amplitude, FC_E_po_P3_Inc_CP1_Amplitude, FC_E_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Inc_Amplitude.df)
#create dataframe 
M1_Alpha_x_P3.df <- data.frame(ROI_FC_Rest_E_pre_Alpha.mean, ROI_FC_Rest_E_post_Alpha.mean, ROI_FC_E_pre_Alpha.mean, ROI_FC_E_post_Alpha.mean, 
                             Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean, Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean, Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean, Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean, 
                             Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean,Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean)
#normality test 
M1_Alpha_x_P3.df%>%
shapiro_test(ROI_FC_Rest_E_pre_Alpha.mean, ROI_FC_Rest_E_post_Alpha.mean, ROI_FC_E_pre_Alpha.mean, ROI_FC_E_post_Alpha.mean, 
             Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean, Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean, Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean, Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean, 
             Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean,Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean)
#Exploratory log df
M1_Alpha_x_P3.log.df <- data.frame(ROI_FC_Rest_E_pre_Alpha.mean, ROI_FC_Rest_E_post_Alpha.mean, ROI_FC_E_pre_Alpha.mean, ROI_FC_E_post_Alpha.mean, 
                                   Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean.log, Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean.log, Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean.log, Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean.log, 
                                   Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean.log, Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean.log,Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean.log, Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean.log  )
#Computation - Pre_E_Congruent
summary(lm(formula = Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean~ROI_FC_E_pre_Alpha.mean))
ggplot(M1_Alpha_x_P3.df, aes(x=ROI_FC_E_pre_Alpha.mean, y=Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_E_Incongruent
summary(lm(formula = Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean~ROI_FC_E_pre_Alpha.mean, data = M1_Alpha_x_P3.df))
ggplot(M1_Alpha_x_P3.df, aes(x=ROI_FC_E_pre_Alpha.mean, y=Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Congruent
summary(lm(formula = Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean~ROI_FC_Rest_E_pre_Alpha.mean, data = M1_Alpha_x_P3.df))
ggplot(M1_Alpha_x_P3.df, aes(x=ROI_FC_Rest_E_pre_Alpha.mean, y=Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Incongruent
summary(lm(formula = Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean~ROI_FC_Rest_E_pre_Alpha.mean, data = M1_Alpha_x_P3.df))
ggplot(M1_Alpha_x_P3.df, aes(x=ROI_FC_Rest_E_pre_Alpha.mean, y=Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Congruent
summary(lm(formula = Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean~ROI_FC_E_post_Alpha.mean, data = M1_Alpha_x_P3.df))
ggplot(M1_Alpha_x_P3.df, aes(x=ROI_FC_E_post_Alpha.mean, y=Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Incongruent
summary(lm(formula = Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean~ROI_FC_E_post_Alpha.mean, data = M1_Alpha_x_P3.df))
ggplot(M1_Alpha_x_P3.df, aes(x=ROI_FC_E_post_Alpha.mean, y=Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Congruent
summary(lm(formula = Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean~ROI_FC_Rest_E_post_Alpha.mean, data = M1_Alpha_x_P3.df))
ggplot(M1_Alpha_x_P3.df, aes(x=ROI_FC_Rest_E_post_Alpha.mean, y=Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Incongruent
summary(lm(formula = Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean~ROI_FC_Rest_E_post_Alpha.mean, data = M1_Alpha_x_P3.df))
ggplot(M1_Alpha_x_P3.df, aes(x=ROI_FC_Rest_E_post_Alpha.mean, y=Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))


# COGNITIVE AND BRAIN FUNCTION OUTCOMES - Theta and Flanker ACC -----------
#Call Variables 
SubjectID <- (df$SubjectID)
SAya_FL_Congruent_ResponseAccuracy_E_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_pre)
SAya_FL_Congruent_ResponseAccuracy_E_pre.log10<-log10(SAya_FL_Congruent_ResponseAccuracy_E_pre)
SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre)
SAya_FL_Incongruent_ResponseAccuracy_E_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_pre)
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre)
SAya_FL_Congruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_post)
SAya_FL_Congruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_post)
SAya_FL_Incongruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_post)
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)

F1_Theta_E_Pre <- (df$F1_Theta_E_Pre)
FCZ_Theta_E_Pre <- df$FCZ_Theta_E_Pre
FZ_Theta_E_Pre <- df$FZ_Theta_E_Pre
FC1_Theta_E_Pre <- df$FC1_Theta_E_Pre
FC2_Theta_E_Pre <- df$FC2_Theta_E_Pre
C1_Theta_E_Pre <- df$C1_Theta_E_Pre
C2_Theta_E_Pre <- df$C2_Theta_E_Pre
F1_Theta_E_Post <- df$F1_Theta_E_Post
FCZ_Theta_E_Post <- df$FCZ_Theta_E_Post
FZ_Theta_E_Post <- df$FZ_Theta_E_Post
FC1_Theta_E_Post <- df$FC1_Theta_E_Post
FC2_Theta_E_Post <- df$FC2_Theta_E_Post
C1_Theta_E_Post <- df$C1_Theta_E_Post
C2_Theta_E_Post <- df$C2_Theta_E_Post
F1_Theta_Rest_E_Pre <- df$F1_Theta_Rest_E_Pre
FCZ_Theta_Rest_E_Pre <- df$FCZ_Theta_Rest_E_Pre
FZ_Theta_Rest_E_Pre <- df$FZ_Theta_Rest_E_Pre
FC1_Theta_Rest_E_Pre <- df$FC1_Theta_Rest_E_Pre
FC2_Theta_Rest_E_Pre <- df$FC2_Theta_Rest_E_Pre
C1_Theta_Rest_E_Pre <- df$C1_Theta_Rest_E_Pre
C2_Theta_Rest_E_Pre <- df$C2_Theta_Rest_E_Pre
F1_Theta_Rest_E_Post <- df$F1_Theta_Rest_E_Post
FCZ_Theta_Rest_E_Post <- df$FCZ_Theta_Rest_E_Post
FZ_Theta_Rest_E_Post <- df$FZ_Theta_Rest_E_Post
FC1_Theta_Rest_E_Post <- df$FC1_Theta_Rest_E_Post
FC2_Theta_Rest_E_Post <- df$FC2_Theta_Rest_E_Post
C1_Theta_Rest_E_Post <- df$C1_Theta_Rest_E_Post
C2_Theta_Rest_E_Post <- df$C2_Theta_Rest_E_Post
F1_Theta_T_Pre <- df$F1_Theta_T_Pre
FCZ_Theta_T_Pre <- df$FCZ_Theta_T_Pre
FZ_Theta_T_Pre <- df$FZ_Theta_T_Pre
FC1_Theta_T_Pre <- df$FC1_Theta_T_Pre
FC2_Theta_T_Pre <- df$FC2_Theta_T_Pre
C1_Theta_T_Pre <- df$C1_Theta_T_Pre
C2_Theta_T_Pre <- df$C2_Theta_T_Pre
F1_Theta_T_Post <- df$F1_Theta_T_Post
FCZ_Theta_T_Post <- df$FCZ_Theta_T_Post
FZ_Theta_T_Post <- df$FZ_Theta_T_Post
FC1_Theta_T_Post <- df$FC1_Theta_T_Post
FC2_Theta_T_Post <- df$FC2_Theta_T_Post
C1_Theta_T_Post <- df$C1_Theta_T_Post
C2_Theta_T_Post <- df$C2_Theta_T_Post
F1_Theta_Rest_T_Pre <- df$F1_Theta_Rest_T_Pre
FCZ_Theta_Rest_T_Pre <- df$FCZ_Theta_Rest_T_Pre
FZ_Theta_Rest_T_Pre <- df$FZ_Theta_Rest_T_Pre
FC1_Theta_Rest_T_Pre <- df$FC1_Theta_Rest_T_Pre
FC2_Theta_Rest_T_Pre <- df$FC2_Theta_Rest_T_Pre
C1_Theta_Rest_T_Pre <- df$C1_Theta_Rest_T_Pre
C2_Theta_Rest_T_Pre <- df$C2_Theta_Rest_T_Pre
F1_Theta_Rest_T_Post <- df$F1_Theta_Rest_T_Post
FCZ_Theta_Rest_T_Post <- df$FCZ_Theta_Rest_T_Post
FZ_Theta_Rest_T_Post <- df$FZ_Theta_Rest_T_Post
FC1_Theta_Rest_T_Post <- df$FC1_Theta_Rest_T_Post
FC2_Theta_Rest_T_Post <- df$FC2_Theta_Rest_T_Post
C1_Theta_Rest_T_Post <- df$C1_Theta_Rest_T_Post
C2_Theta_Rest_T_Post <- df$C2_Theta_Rest_T_Post

#M1 Resting Posterior Theta x Flanker ACC in Exercise vs. Rest 
##Average together Resting Theta ROI site
Wide.ROI_FC_E_pre_Theta.df <-data.frame(F1_Theta_E_Pre, FCZ_Theta_E_Pre, FZ_Theta_E_Pre, FC1_Theta_E_Pre, FC2_Theta_E_Pre, C1_Theta_E_Pre)
Wide.ROI_FC_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_E_pre_Theta.df)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_E_Pre, FCZ_Theta_E_Post, FZ_Theta_E_Post, FC1_Theta_E_Post, FC2_Theta_E_Pre, C1_Theta_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_pre_Theta.df <-data.frame(F1_Theta_Rest_E_Pre, FCZ_Theta_Rest_E_Pre, FZ_Theta_Rest_E_Pre, FC1_Theta_Rest_E_Pre, FC2_Theta_Rest_E_Pre, C1_Theta_Rest_E_Pre)
Wide.ROI_FC_Rest_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_pre_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_Rest_E_Post, FCZ_Theta_Rest_E_Post, FZ_Theta_Rest_E_Post, FC1_Theta_Rest_E_Post, FC2_Theta_Rest_E_Post, C1_Theta_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#create dataframe 
M1_Theta_x_ACC.df <- data.frame(Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
                                SAya_FL_Congruent_ResponseAccuracy_E_pre, SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre, SAya_FL_Incongruent_ResponseAccuracy_E_pre, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre, 
                                SAya_FL_Congruent_ResponseAccuracy_E_post, SAya_FL_Congruent_ResponseAccuracy_Rest_E_post, SAya_FL_Incongruent_ResponseAccuracy_E_post, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
#normality test 
M1_Theta_x_ACC.df%>%
  shapiro_test(Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
               SAya_FL_Congruent_ResponseAccuracy_E_pre, SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre, SAya_FL_Incongruent_ResponseAccuracy_E_pre, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre, 
               SAya_FL_Congruent_ResponseAccuracy_E_post, SAya_FL_Congruent_ResponseAccuracy_Rest_E_post, SAya_FL_Incongruent_ResponseAccuracy_E_post, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
#Computation - Pre_E_Congruent
summary(lm(formula = SAya_FL_Congruent_ResponseAccuracy_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_x_ACC.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Congruent_ResponseAccuracy_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_E_Incoongruent
summary(lm(formula = SAya_FL_Incongruent_ResponseAccuracy_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_x_ACC.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Incongruent_ResponseAccuracy_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Congruent
summary(lm(formula = SAya_FL_Congruent_ResponseAccuracy_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_x_ACC.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Congruent_ResponseAccuracy_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Incoongruent
summary(lm(formula = SAya_FL_Incongruent_ResponseAccuracy_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_x_ACC.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Incongruent_ResponseAccuracy_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_x_ACC.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Incoongruent
summary(lm(formula = SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_x_ACC.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_x_ACC.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Incoongruent
summary(lm(formula = SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_x_ACC.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))

# COGNITIVE AND BRAIN FUNCTION OUTCOMES - Theta and Flanker RT ------------
#Call Variables 
SubjectID <- (df$SubjectID)
SAya_FL_Congruent_MeanRT_E_pre <- (df$ SAya_FL_Congruent_MeanRT_E_pre)
SAya_FL_Congruent_MeanRT_E_pre.log10<-log10(SAya_FL_Congruent_MeanRT_E_pre)
SAya_FL_Congruent_MeanRT_Rest_E_pre <- (df$ SAya_FL_Congruent_MeanRT_Rest_E_pre)
SAya_FL_Incongruent_MeanRT_E_pre <- (df$ SAya_FL_Incongruent_MeanRT_E_pre)
SAya_FL_Incongruent_MeanRT_Rest_E_pre <- (df$ SAya_FL_Incongruent_MeanRT_Rest_E_pre)
SAya_FL_Congruent_MeanRT_E_post <- (df$ SAya_FL_Congruent_MeanRT_E_post)
SAya_FL_Congruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Congruent_MeanRT_Rest_E_post)
SAya_FL_Incongruent_MeanRT_E_post <- (df$ SAya_FL_Incongruent_MeanRT_E_post)
SAya_FL_Incongruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Incongruent_MeanRT_Rest_E_post)

F1_Theta_E_Pre <- (df$F1_Theta_E_Pre)
FCZ_Theta_E_Pre <- df$FCZ_Theta_E_Pre
FZ_Theta_E_Pre <- df$FZ_Theta_E_Pre
FC1_Theta_E_Pre <- df$FC1_Theta_E_Pre
FC2_Theta_E_Pre <- df$FC2_Theta_E_Pre
C1_Theta_E_Pre <- df$C1_Theta_E_Pre
C2_Theta_E_Pre <- df$C2_Theta_E_Pre
F1_Theta_E_Post <- df$F1_Theta_E_Post
FCZ_Theta_E_Post <- df$FCZ_Theta_E_Post
FZ_Theta_E_Post <- df$FZ_Theta_E_Post
FC1_Theta_E_Post <- df$FC1_Theta_E_Post
FC2_Theta_E_Post <- df$FC2_Theta_E_Post
C1_Theta_E_Post <- df$C1_Theta_E_Post
C2_Theta_E_Post <- df$C2_Theta_E_Post
F1_Theta_Rest_E_Pre <- df$F1_Theta_Rest_E_Pre
FCZ_Theta_Rest_E_Pre <- df$FCZ_Theta_Rest_E_Pre
FZ_Theta_Rest_E_Pre <- df$FZ_Theta_Rest_E_Pre
FC1_Theta_Rest_E_Pre <- df$FC1_Theta_Rest_E_Pre
FC2_Theta_Rest_E_Pre <- df$FC2_Theta_Rest_E_Pre
C1_Theta_Rest_E_Pre <- df$C1_Theta_Rest_E_Pre
C2_Theta_Rest_E_Pre <- df$C2_Theta_Rest_E_Pre
F1_Theta_Rest_E_Post <- df$F1_Theta_Rest_E_Post
FCZ_Theta_Rest_E_Post <- df$FCZ_Theta_Rest_E_Post
FZ_Theta_Rest_E_Post <- df$FZ_Theta_Rest_E_Post
FC1_Theta_Rest_E_Post <- df$FC1_Theta_Rest_E_Post
FC2_Theta_Rest_E_Post <- df$FC2_Theta_Rest_E_Post
C1_Theta_Rest_E_Post <- df$C1_Theta_Rest_E_Post
C2_Theta_Rest_E_Post <- df$C2_Theta_Rest_E_Post

#M1 Resting Posterior Theta x Flanker RT in Exercise vs. Rest 
##Average together Resting Theta ROI site
Wide.ROI_FC_E_pre_Theta.df <-data.frame(F1_Theta_E_Pre, FCZ_Theta_E_Pre, FZ_Theta_E_Pre, FC1_Theta_E_Pre, FC2_Theta_E_Pre, C1_Theta_E_Pre)
Wide.ROI_FC_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_E_pre_Theta.df)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_E_Pre, FCZ_Theta_E_Post, FZ_Theta_E_Post, FC1_Theta_E_Post, FC2_Theta_E_Pre, C1_Theta_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_pre_Theta.df <-data.frame(F1_Theta_Rest_E_Pre, FCZ_Theta_Rest_E_Pre, FZ_Theta_Rest_E_Pre, FC1_Theta_Rest_E_Pre, FC2_Theta_Rest_E_Pre, C1_Theta_Rest_E_Pre)
Wide.ROI_FC_Rest_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_pre_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_Rest_E_Post, FCZ_Theta_Rest_E_Post, FZ_Theta_Rest_E_Post, FC1_Theta_Rest_E_Post, FC2_Theta_Rest_E_Post, C1_Theta_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#create dataframe 
M1_Theta_x_RT.df <- data.frame(Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
                               SAya_FL_Congruent_MeanRT_E_pre, SAya_FL_Congruent_MeanRT_Rest_E_pre, SAya_FL_Incongruent_MeanRT_E_pre, SAya_FL_Incongruent_MeanRT_Rest_E_pre, 
                               SAya_FL_Congruent_MeanRT_E_post, SAya_FL_Congruent_MeanRT_Rest_E_post, SAya_FL_Incongruent_MeanRT_E_post, SAya_FL_Incongruent_MeanRT_Rest_E_post)
#normality test 
M1_Theta_x_RT.df%>%
  shapiro_test(Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
               SAya_FL_Congruent_MeanRT_E_pre, SAya_FL_Congruent_MeanRT_Rest_E_pre, SAya_FL_Incongruent_MeanRT_E_pre, SAya_FL_Incongruent_MeanRT_Rest_E_pre, 
               SAya_FL_Congruent_MeanRT_E_post, SAya_FL_Congruent_MeanRT_Rest_E_post, SAya_FL_Incongruent_MeanRT_E_post, SAya_FL_Incongruent_MeanRT_Rest_E_post)
#Computation - Pre_E_Congruent
summary(lm(formula = SAya_FL_Congruent_MeanRT_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_x_RT.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Congruent_MeanRT_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_E_Incoongruent
summary(lm(formula = SAya_FL_Incongruent_MeanRT_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_x_RT.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Incongruent_MeanRT_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Congruent
summary(lm(formula = SAya_FL_Congruent_MeanRT_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_x_RT.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Congruent_MeanRT_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Incoongruent
summary(lm(formula = SAya_FL_Incongruent_MeanRT_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_x_RT.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Incongruent_MeanRT_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_MeanRT_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_x_RT.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Congruent_MeanRT_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Incoongruent
summary(lm(formula = SAya_FL_Incongruent_MeanRT_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_x_RT.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Incongruent_MeanRT_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_MeanRT_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_x_RT.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Congruent_MeanRT_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Incoongruent
summary(lm(formula = SAya_FL_Incongruent_MeanRT_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_x_RT.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Incongruent_MeanRT_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))


# COGNITIVE AND BRAIN FUNCTION OUTCOMES - Theta and Flanker Inverse Efficiency --------
#Call Variables 
SAya_FL_Congruent_InverseEfficiency_Rest_E_pre<-(df$SAya_FL_Congruent_InverseEfficiency_Rest_E_pre)
SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre<-(df$SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre)
SAya_FL_Congruent_InverseEfficiency_Rest_E_post<-(df$SAya_FL_Congruent_InverseEfficiency_Rest_E_post)
SAya_FL_Incongruent_InverseEfficiency_Rest_E_post<-(df$SAya_FL_Incongruent_InverseEfficiency_Rest_E_post)
SAya_FL_Congruent_InverseEfficiency_E_pre<-(df$SAya_FL_Congruent_InverseEfficiency_E_pre)
SAya_FL_Incongruent_InverseEfficiency_E_pre<-(df$SAya_FL_Incongruent_InverseEfficiency_E_pre)
SAya_FL_Congruent_InverseEfficiency_E_post<-(df$SAya_FL_Congruent_InverseEfficiency_E_post)
SAya_FL_Incongruent_InverseEfficiency_E_post<-(df$SAya_FL_Incongruent_InverseEfficiency_E_post)

F1_Theta_E_Pre <- (df$F1_Theta_E_Pre)
FCZ_Theta_E_Pre <- df$FCZ_Theta_E_Pre
FZ_Theta_E_Pre <- df$FZ_Theta_E_Pre
FC1_Theta_E_Pre <- df$FC1_Theta_E_Pre
FC2_Theta_E_Pre <- df$FC2_Theta_E_Pre
C1_Theta_E_Pre <- df$C1_Theta_E_Pre
C2_Theta_E_Pre <- df$C2_Theta_E_Pre
F1_Theta_E_Post <- df$F1_Theta_E_Post
FCZ_Theta_E_Post <- df$FCZ_Theta_E_Post
FZ_Theta_E_Post <- df$FZ_Theta_E_Post
FC1_Theta_E_Post <- df$FC1_Theta_E_Post
FC2_Theta_E_Post <- df$FC2_Theta_E_Post
C1_Theta_E_Post <- df$C1_Theta_E_Post
C2_Theta_E_Post <- df$C2_Theta_E_Post
F1_Theta_Rest_E_Pre <- df$F1_Theta_Rest_E_Pre
FCZ_Theta_Rest_E_Pre <- df$FCZ_Theta_Rest_E_Pre
FZ_Theta_Rest_E_Pre <- df$FZ_Theta_Rest_E_Pre
FC1_Theta_Rest_E_Pre <- df$FC1_Theta_Rest_E_Pre
FC2_Theta_Rest_E_Pre <- df$FC2_Theta_Rest_E_Pre
C1_Theta_Rest_E_Pre <- df$C1_Theta_Rest_E_Pre
C2_Theta_Rest_E_Pre <- df$C2_Theta_Rest_E_Pre
F1_Theta_Rest_E_Post <- df$F1_Theta_Rest_E_Post
FCZ_Theta_Rest_E_Post <- df$FCZ_Theta_Rest_E_Post
FZ_Theta_Rest_E_Post <- df$FZ_Theta_Rest_E_Post
FC1_Theta_Rest_E_Post <- df$FC1_Theta_Rest_E_Post
FC2_Theta_Rest_E_Post <- df$FC2_Theta_Rest_E_Post
C1_Theta_Rest_E_Post <- df$C1_Theta_Rest_E_Post
C2_Theta_Rest_E_Post <- df$C2_Theta_Rest_E_Post
#M1 Resting Posterior Theta x Flanker RT in Exercise vs. Rest 
##Average together Resting Theta ROI site
Wide.ROI_FC_E_pre_Theta.df <-data.frame(F1_Theta_E_Pre, FCZ_Theta_E_Pre, FZ_Theta_E_Pre, FC1_Theta_E_Pre, FC2_Theta_E_Pre, C1_Theta_E_Pre)
Wide.ROI_FC_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_E_pre_Theta.df)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_E_Pre, FCZ_Theta_E_Post, FZ_Theta_E_Post, FC1_Theta_E_Post, FC2_Theta_E_Pre, C1_Theta_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_pre_Theta.df <-data.frame(F1_Theta_Rest_E_Pre, FCZ_Theta_Rest_E_Pre, FZ_Theta_Rest_E_Pre, FC1_Theta_Rest_E_Pre, FC2_Theta_Rest_E_Pre, C1_Theta_Rest_E_Pre)
Wide.ROI_FC_Rest_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_pre_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_Rest_E_Post, FCZ_Theta_Rest_E_Post, FZ_Theta_Rest_E_Post, FC1_Theta_Rest_E_Post, FC2_Theta_Rest_E_Post, C1_Theta_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#create dataframe 
M1_Theta_x_IE.df <- data.frame(Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
                               SAya_FL_Congruent_InverseEfficiency_Rest_E_pre, SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre, SAya_FL_Congruent_InverseEfficiency_Rest_E_post, SAya_FL_Incongruent_InverseEfficiency_Rest_E_post, 
                               SAya_FL_Congruent_InverseEfficiency_E_pre, SAya_FL_Incongruent_InverseEfficiency_E_pre, SAya_FL_Congruent_InverseEfficiency_E_post, SAya_FL_Incongruent_InverseEfficiency_E_post)
#normality test 
M1_Theta_x_IE.df %>%
  shapiro_test(Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
               SAya_FL_Congruent_InverseEfficiency_Rest_E_pre, SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre, SAya_FL_Congruent_InverseEfficiency_Rest_E_post, SAya_FL_Incongruent_InverseEfficiency_Rest_E_post, 
               SAya_FL_Congruent_InverseEfficiency_E_pre, SAya_FL_Incongruent_InverseEfficiency_E_pre, SAya_FL_Congruent_InverseEfficiency_E_post, SAya_FL_Incongruent_InverseEfficiency_E_post)
#Computation - Pre_E_Congruent
summary(lm(formula = SAya_FL_Congruent_InverseEfficiency_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_x_IE.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Congruent_InverseEfficiency_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_E_Incoongruent
summary(lm(formula = SAya_FL_Incongruent_InverseEfficiency_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_x_IE.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Incongruent_InverseEfficiency_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Congruent
summary(lm(formula = SAya_FL_Congruent_InverseEfficiency_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_x_IE.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Congruent_InverseEfficiency_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Incoongruent
summary(lm(formula = SAya_FL_Incongruent_InverseEfficiency_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_x_IE.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Incongruent_InverseEfficiency_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_InverseEfficiency_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_x_IE.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Congruent_InverseEfficiency_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Incoongruent
summary(lm(formula = SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_x_IE.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_InverseEfficiency_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_x_IE.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Congruent_InverseEfficiency_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Incoongruent
summary(lm(formula = SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_x_IE.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))

# SALIVARY OUTCOMES - SAA -------------------------------------------------
#Call variables 
SAA_1_Rest_E <- df$SAA_1_Rest_E
SAA_4_Rest_E <- df$SAA_4_Rest_E
SAA_6_Rest_E <- df$SAA_6_Rest_E
SAA_7_Rest_E <- df$SAA_7_Rest_E
SAA_1_Exercise <- df$SAA_1_Exercise
SAA_4_Exercise <- df$SAA_4_Exercise
SAA_6_Exercise <- df$SAA_6_Exercise
SAA_7_Exercise <- df$SAA_7_Exercise
SAA_1_Rest_T <- df$SAA_1_Rest_T
SAA_4_Rest_T <- df$SAA_4_Rest_T
SAA_6_Rest_T <- df$SAA_6_Rest_T
SAA_7_Rest_T <- df$SAA_7_Rest_T
SAA_1_Trier <- df$SAA_1_Trier
SAA_4_Trier <- df$SAA_4_Trier
SAA_6_Trier <- df$SAA_6_Trier
SAA_7_Trier <- df$SAA_7_Trier
#M1 Difference in sAA in Exercise vs. Rest 
#Create abbreviated df 
SAA.M1.df <- data.frame(SubjectID, SAA_1_Rest_E, SAA_4_Rest_E, SAA_6_Rest_E, SAA_7_Rest_E, 
                        SAA_1_Exercise, SAA_4_Exercise, SAA_6_Exercise, SAA_7_Exercise)
#melt df 
longformat.SAA.M1.df <- melt(SAA.M1.df, id = "SubjectID", variable.name = "Condition")
View(longformat.SAA.M1.df)
#Manipulation Check - T-test at each timepoint
#test for normality
SAA.M1.df %>%
shapiro_test(SAA_1_Rest_E, SAA_4_Rest_E, SAA_6_Rest_E, SAA_7_Rest_E, 
             SAA_1_Exercise, SAA_4_Exercise, SAA_6_Exercise, SAA_7_Exercise)
histogram(df$SAA_1_Rest_E)
histogram(df$SAA_1_Exercise)
#does not pass normaility - used nonparametric
ggwithinstats(longformat.SAA.M1.df, Condition, value, type = "nonparametric", p.adjust.method = "bonferroni")

#Visualize
mean.SAA_1_Rest_E<-mean(SAA_1_Rest_E, na.rm=TRUE)
sd.SAA_1_Rest_E<-sd(SAA_1_Rest_E, na.rm=TRUE)
mean.SAA_4_Rest_E<-mean(SAA_4_Rest_E, na.rm=TRUE)
sd.SAA_4_Rest_E<-sd(SAA_4_Rest_E, na.rm=TRUE)
mean.SAA_6_Rest_E<-mean(SAA_6_Rest_E, na.rm=TRUE)
sd.SAA_6_Rest_E<-sd(SAA_6_Rest_E, na.rm=TRUE)
mean.SAA_7_Rest_E<-mean(SAA_7_Rest_E, na.rm=TRUE)
sd.SAA_7_Rest_E<-sd(SAA_7_Rest_E, na.rm=TRUE)
mean.SAA_1_Exercise<-mean(SAA_1_Exercise, na.rm=TRUE)
sd.SAA_1_Exercise<-sd(SAA_1_Exercise, na.rm=TRUE)
mean.SAA_4_Exercise<-mean(SAA_4_Exercise, na.rm=TRUE)
sd.SAA_4_Exercise<-sd(SAA_4_Exercise, na.rm=TRUE)
mean.SAA_6_Exercise<-mean(SAA_6_Exercise, na.rm=TRUE)
sd.SAA_6_Exercise<-sd(SAA_6_Exercise, na.rm=TRUE)
mean.SAA_7_Exercise<-mean(SAA_7_Exercise, na.rm=TRUE)
sd.SAA_7_Exercise<-sd(SAA_7_Exercise, na.rm=TRUE)
#create dataframe of means and SDs
SAA.M1.mean.sd.df<-data.frame(mean.SAA_1_Rest_E, mean.SAA_4_Rest_E, mean.SAA_6_Rest_E, mean.SAA_7_Rest_E, 
                              mean.SAA_1_Exercise, mean.SAA_4_Exercise, mean.SAA_6_Exercise, mean.SAA_7_Exercise,
                              sd.SAA_1_Rest_E, sd.SAA_4_Rest_E, sd.SAA_6_Rest_E,sd.SAA_7_Rest_E,
                              sd.SAA_1_Exercise, sd.SAA_4_Exercise, sd.SAA_6_Exercise, sd.SAA_7_Exercise)
#rearrange into table in excel
SAA_M1_mean_sd_df <- read_excel("/Volumes/SAya_USB/Server_Dissertation/SAA.M1.mean.sd.df.xlsx")
View(SAA_M1_mean_sd_df)
#graph
ggplot(SAA_M1_mean_sd_df, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.1, 
                position=position_dodge(0.05)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  labs(title="Salivary Alpha Amylase in Exercise vs. Rest", x ="Timepoint", y = "Mean sAA")
  
#M2 Difference in sAA in Trier vs. Rest 
#Create abbreviated df 
SAA.M2.df <- data.frame(SubjectID, SAA_1_Rest_T, SAA_4_Rest_T, SAA_6_Rest_T, SAA_7_Rest_T, 
                        SAA_1_Trier, SAA_4_Trier, SAA_6_Trier, SAA_7_Trier)
#melt df 
longformat.SAA.M2.df <- melt(SAA.M2.df, id = "SubjectID", variable.name = "Condition")
View(longformat.SAA.M2.df)
#Manipulation Check - T-test at each timepoint
#test for normality
SAA.M2.df %>%
  shapiro_test(SAA_1_Rest_T, SAA_4_Rest_T, SAA_6_Rest_T, SAA_7_Rest_T, 
               SAA_1_Trier, SAA_4_Trier, SAA_6_Trier, SAA_7_Trier)
histogram(df$SAA_1_Rest_T)
histogram(df$SAA_1_Trier)
#does not pass normaility - used nonparametric
ggwithinstats(longformat.SAA.M2.df, Condition, value, type = "nonparametric", p.adjust.method = "bonferroni")
#Visualize
mean.SAA_1_Rest_T<-mean(SAA_1_Rest_T, na.rm=TRUE)
sd.SAA_1_Rest_T<-sd(SAA_1_Rest_T, na.rm=TRUE)
mean.SAA_4_Rest_T<-mean(SAA_4_Rest_T, na.rm=TRUE)
sd.SAA_4_Rest_T<-sd(SAA_4_Rest_T, na.rm=TRUE)
mean.SAA_6_Rest_T<-mean(SAA_6_Rest_T, na.rm=TRUE)
sd.SAA_6_Rest_T<-sd(SAA_6_Rest_T, na.rm=TRUE)
mean.SAA_7_Rest_T<-mean(SAA_7_Rest_T, na.rm=TRUE)
sd.SAA_7_Rest_T<-sd(SAA_7_Rest_T, na.rm=TRUE)
mean.SAA_1_Trier<-mean(SAA_1_Trier, na.rm=TRUE)
sd.SAA_1_Trier<-sd(SAA_1_Trier, na.rm=TRUE)
mean.SAA_4_Trier<-mean(SAA_4_Trier, na.rm=TRUE)
sd.SAA_4_Trier<-sd(SAA_4_Trier, na.rm=TRUE)
mean.SAA_6_Trier<-mean(SAA_6_Trier, na.rm=TRUE)
sd.SAA_6_Trier<-sd(SAA_6_Trier, na.rm=TRUE)
mean.SAA_7_Trier<-mean(SAA_7_Trier, na.rm=TRUE)
sd.SAA_7_Trier<-sd(SAA_7_Trier, na.rm=TRUE)
#create dataframe of means and SDs
SAA.M2.mean.sd.df<-data.frame(mean.SAA_1_Rest_T, mean.SAA_4_Rest_T, mean.SAA_6_Rest_T, mean.SAA_7_Rest_T, 
                              mean.SAA_1_Trier, mean.SAA_4_Trier, mean.SAA_6_Trier, mean.SAA_7_Trier,
                              sd.SAA_1_Rest_T, sd.SAA_4_Rest_T, sd.SAA_6_Rest_T,sd.SAA_7_Rest_T,
                              sd.SAA_1_Trier, sd.SAA_4_Trier, sd.SAA_6_Trier, sd.SAA_7_Trier)
#rearrange into table in excel
SAA_M2_mean_sd_df <- read_excel("/Volumes/SAya_USB/Server_Dissertation/SAA.M2.mean.sd.df.xlsx")
View(SAA_M2_mean_sd_df)
#graph
ggplot(SAA_M2_mean_sd_df, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.1, 
                position=position_dodge(0.05)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  labs(title="Salivary Alpha Amylase in Trier vs. Rest", x ="Timepoint", y = "Mean sAA")

# SALIVARY OUTCOMES - CORTISOL --------------------------------------------
#Call variables 
Cort_1_Rest_E <- df$Cort_1_Rest_E
Cort_4_Rest_E <- df$Cort_4_Rest_E
Cort_6_Rest_E <- df$Cort_6_Rest_E
Cort_7_Rest_E <- df$Cort_7_Rest_E
Cort_1_Exercise <- df$Cort_1_Exercise
Cort_4_Exercise <- df$Cort_4_Exercise
Cort_6_Exercise <- df$Cort_6_Exercise
Cort_7_Exercise <- df$Cort_7_Exercise
Cort_1_Rest_T <- df$Cort_1_Rest_T
Cort_4_Rest_T <- df$Cort_4_Rest_T
Cort_6_Rest_T <- df$Cort_6_Rest_T
Cort_7_Rest_T <- df$Cort_7_Rest_T
Cort_1_Trier <- df$Cort_1_Trier
Cort_4_Trier <- df$Cort_4_Trier
Cort_6_Trier <- df$Cort_6_Trier
Cort_7_Trier <- df$Cort_7_Trier
#M1 Difference in Cort in Exercise vs. Rest 
#Create abbreviated df 
Cort.M1.df <- data.frame(SubjectID, Cort_1_Rest_E, Cort_4_Rest_E, Cort_6_Rest_E, Cort_7_Rest_E, 
                         Cort_1_Exercise, Cort_4_Exercise, Cort_6_Exercise, Cort_7_Exercise)
#melt df 
longformat.Cort.M1.df <- melt(Cort.M1.df, id = "SubjectID", variable.name = "Condition")
View(longformat.Cort.M1.df)
#Manipulation Check - T-test at each timepoint
#test for normality
Cort.M1.df %>%
  shapiro_test(Cort_1_Rest_E, Cort_4_Rest_E, Cort_6_Rest_E, Cort_7_Rest_E, 
               Cort_1_Exercise, Cort_4_Exercise, Cort_6_Exercise, Cort_7_Exercise)
histogram(df$Cort_1_Rest_E)
histogram(df$Cort_1_Exercise)
#does not pass normaility - used nonparametric
ggwithinstats(longformat.Cort.M1.df, Condition, value, type = "nonparametric", p.adjust.method = "bonferroni")
#Visualize
mean.Cort_1_Rest_E<-mean(Cort_1_Rest_E, na.rm=TRUE)
sd.Cort_1_Rest_E<-sd(Cort_1_Rest_E, na.rm=TRUE)
mean.Cort_4_Rest_E<-mean(Cort_4_Rest_E, na.rm=TRUE)
sd.Cort_4_Rest_E<-sd(Cort_4_Rest_E, na.rm=TRUE)
mean.Cort_6_Rest_E<-mean(Cort_6_Rest_E, na.rm=TRUE)
sd.Cort_6_Rest_E<-sd(Cort_6_Rest_E, na.rm=TRUE)
mean.Cort_7_Rest_E<-mean(Cort_7_Rest_E, na.rm=TRUE)
sd.Cort_7_Rest_E<-sd(Cort_7_Rest_E, na.rm=TRUE)
mean.Cort_1_Exercise<-mean(Cort_1_Exercise, na.rm=TRUE)
sd.Cort_1_Exercise<-sd(Cort_1_Exercise, na.rm=TRUE)
mean.Cort_4_Exercise<-mean(Cort_4_Exercise, na.rm=TRUE)
sd.Cort_4_Exercise<-sd(Cort_4_Exercise, na.rm=TRUE)
mean.Cort_6_Exercise<-mean(Cort_6_Exercise, na.rm=TRUE)
sd.Cort_6_Exercise<-sd(Cort_6_Exercise, na.rm=TRUE)
mean.Cort_7_Exercise<-mean(Cort_7_Exercise, na.rm=TRUE)
sd.Cort_7_Exercise<-sd(Cort_7_Exercise, na.rm=TRUE)
#create dataframe of means and SDs
Cort.M1.mean.sd.df<-data.frame(mean.Cort_1_Rest_E, mean.Cort_4_Rest_E, mean.Cort_6_Rest_E, mean.Cort_7_Rest_E, 
                               mean.Cort_1_Exercise, mean.Cort_4_Exercise, mean.Cort_6_Exercise, mean.Cort_7_Exercise,
                               sd.Cort_1_Rest_E, sd.Cort_4_Rest_E, sd.Cort_6_Rest_E,sd.Cort_7_Rest_E,
                               sd.Cort_1_Exercise, sd.Cort_4_Exercise, sd.Cort_6_Exercise, sd.Cort_7_Exercise)
#rearrange into table in excel
Cort_M1_mean_sd_df <- read_excel("/Volumes/SAya_USB/Server_Dissertation/CORT.M1.mean.sd.df.xlsx")
View(Cort_M1_mean_sd_df)
#graph
ggplot(Cort_M1_mean_sd_df, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.1, 
                position=position_dodge(0.05)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  labs(title="Cortisol in Exercise vs. Rest", x ="Timepoint", y = "Mean CORT")


#M2 Difference in Cort in Trier vs. Rest 
#Create abbreviated df 
Cort.M2.df <- data.frame(SubjectID, Cort_1_Rest_T, Cort_4_Rest_T, Cort_6_Rest_T, Cort_7_Rest_T, 
                         Cort_1_Trier, Cort_4_Trier, Cort_6_Trier, Cort_7_Trier)
#melt df 
longformat.Cort.M2.df <- melt(Cort.M2.df, id = "SubjectID", variable.name = "Condition")
View(longformat.Cort.M2.df)
#Manipulation Check - T-test at each timepoint
#test for normality
Cort.M2.df %>%
  shapiro_test(Cort_1_Rest_T, Cort_4_Rest_T, Cort_6_Rest_T, Cort_7_Rest_T, 
               Cort_1_Trier, Cort_4_Trier, Cort_6_Trier, Cort_7_Trier)
histogram(df$Cort_1_Rest_T)
histogram(df$Cort_1_Trier)
#does not pass normaility - used nonparametric
ggwithinstats(longformat.Cort.M2.df, Condition, value, type = "nonparametric", p.adjust.method = "bonferroni")
#Visualize
mean.Cort_1_Rest_T<-mean(Cort_1_Rest_T, na.rm=TRUE)
sd.Cort_1_Rest_T<-sd(Cort_1_Rest_T, na.rm=TRUE)
mean.Cort_4_Rest_T<-mean(Cort_4_Rest_T, na.rm=TRUE)
sd.Cort_4_Rest_T<-sd(Cort_4_Rest_T, na.rm=TRUE)
mean.Cort_6_Rest_T<-mean(Cort_6_Rest_T, na.rm=TRUE)
sd.Cort_6_Rest_T<-sd(Cort_6_Rest_T, na.rm=TRUE)
mean.Cort_7_Rest_T<-mean(Cort_7_Rest_T, na.rm=TRUE)
sd.Cort_7_Rest_T<-sd(Cort_7_Rest_T, na.rm=TRUE)
mean.Cort_1_Trier<-mean(Cort_1_Trier, na.rm=TRUE)
sd.Cort_1_Trier<-sd(Cort_1_Trier, na.rm=TRUE)
mean.Cort_4_Trier<-mean(Cort_4_Trier, na.rm=TRUE)
sd.Cort_4_Trier<-sd(Cort_4_Trier, na.rm=TRUE)
mean.Cort_6_Trier<-mean(Cort_6_Trier, na.rm=TRUE)
sd.Cort_6_Trier<-sd(Cort_6_Trier, na.rm=TRUE)
mean.Cort_7_Trier<-mean(Cort_7_Trier, na.rm=TRUE)
sd.Cort_7_Trier<-sd(Cort_7_Trier, na.rm=TRUE)
#create dataframe of means and SDs
Cort.M2.mean.sd.df<-data.frame(mean.Cort_1_Rest_T, mean.Cort_4_Rest_T, mean.Cort_6_Rest_T, mean.Cort_7_Rest_T, 
                               mean.Cort_1_Trier, mean.Cort_4_Trier, mean.Cort_6_Trier, mean.Cort_7_Trier,
                               sd.Cort_1_Rest_T, sd.Cort_4_Rest_T, sd.Cort_6_Rest_T,sd.Cort_7_Rest_T,
                               sd.Cort_1_Trier, sd.Cort_4_Trier, sd.Cort_6_Trier, sd.Cort_7_Trier)
#rearrange into table in excel
Cort_M2_mean_sd_df <- read_excel("/Volumes/SAya_USB/Server_Dissertation/CORT.M2.mean.sd.df.xlsx")
View(Cort_M2_mean_sd_df)
#graph
ggplot(Cort_M2_mean_sd_df, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.1, 
                position=position_dodge(0.05)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  labs(title="Cortisol in Trier vs. Rest", x ="Timepoint", y = "Mean CORT")


# MEDIATIONS - COGNITIVE OUTCOMES - Flanker Response Time  --------
#M1 Mediation - X = intervention group, M = sAA at Timepoint 4, Y = post intervention Congruent Flanker RT
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_4_Exercise<- df$SAA_4_Exercise
SAya_FL_Congruent_MeanRT_E_post <- (df$ SAya_FL_Congruent_MeanRT_E_post)
SAya_FL_Congruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Congruent_MeanRT_Rest_E_post)
#create dataframe 
postRT_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise,SAya_FL_Congruent_MeanRT_E_post, SAya_FL_Congruent_MeanRT_Rest_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(postRT_Congruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//postRT_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postRT_Congruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//postRT_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postRT_Congruent_Model1_Mediation.df)
#mediation computation
model.M.postRT_Congruent_Model1_Mediation<- lm(SAA~Intervention, longformat.postRT_Congruent_Model1_Mediation.df)
model.Y.postRT_Congruent_Model1_Mediation<- lm(Post_RT~Intervention + SAA, longformat.postRT_Congruent_Model1_Mediation.df)
set.seed(123)
results.postRT_Congruent_Model1_Mediation<- mediate(model.M.postRT_Congruent_Model1_Mediation, model.Y.postRT_Congruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA', boot = TRUE, sims = 1000)
summary(results.postRT_Congruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = sAA at Timepoint 4, Y = post intervention Incongruent Flanker RT
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_4_Exercise<- df$SAA_4_Exercise
SAya_FL_Incongruent_MeanRT_E_post <- (df$ SAya_FL_Incongruent_MeanRT_E_post)
SAya_FL_Incongruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Incongruent_MeanRT_Rest_E_post)
#create dataframe 
postRT_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, SAya_FL_Incongruent_MeanRT_E_post, SAya_FL_Incongruent_MeanRT_Rest_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(postRT_Incongruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//postRT_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postRT_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation/postRT_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postRT_Incongruent_Model1_Mediation.df)
#mediation computation
model.M.postRT_Incongruent_Model1_Mediation<- lm(SAA~Intervention, longformat.postRT_Incongruent_Model1_Mediation.df)
model.Y.postRT_Incongruent_Model1_Mediation<- lm(Post_RT~Intervention + SAA, longformat.postRT_Incongruent_Model1_Mediation.df)
set.seed(123)
results.postRT_Incongruent_Model1_Mediation<- mediate(model.M.postRT_Incongruent_Model1_Mediation, model.Y.postRT_Incongruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA', boot = TRUE, sims = 1000)
summary(results.postRT_Incongruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = change in Congruent Flanker RT
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-df$SAA_1_Rest_E
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_1_Exercise<- df$SAA_1_Exercise
SAA_4_Exercise<- df$SAA_4_Exercise
SAya_FL_Congruent_MeanRT_E_pre <- (df$ SAya_FL_Congruent_MeanRT_E_pre)
SAya_FL_Congruent_MeanRT_Rest_E_pre <- (df$ SAya_FL_Congruent_MeanRT_Rest_E_pre)
SAya_FL_Congruent_MeanRT_E_post <- (df$ SAya_FL_Congruent_MeanRT_E_post)
SAya_FL_Congruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Congruent_MeanRT_Rest_E_post)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
SAya_FL_Congruent_MeanRT_Rest_E_Change<- SAya_FL_Congruent_MeanRT_Rest_E_post-SAya_FL_Congruent_MeanRT_Rest_E_pre
SAya_FL_Congruent_MeanRT_E_Change<- SAya_FL_Congruent_MeanRT_E_post-SAya_FL_Congruent_MeanRT_E_pre
#create dataframe 
changeRT_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Congruent_MeanRT_E_Change, SAya_FL_Congruent_MeanRT_Rest_E_Change)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeRT_Congruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeRT_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeRT_Congruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeRT_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeRT_Congruent_Model1_Mediation.df)
#mediation computation
model.M.changeRT_Congruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeRT_Congruent_Model1_Mediation.df)
model.Y.changeRT_Congruent_Model1_Mediation<- lm(MeanRT_Change~Intervention + SAA_Change, longformat.changeRT_Congruent_Model1_Mediation.df)
set.seed(123)
results.changeRT_Congruent_Model1_Mediation<- mediate(model.M.changeRT_Congruent_Model1_Mediation, model.Y.changeRT_Congruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeRT_Congruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = change in Incongruent Flanker RT
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-df$SAA_1_Rest_E
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_1_Exercise<- df$SAA_1_Exercise
SAA_4_Exercise<- df$SAA_4_Exercise
SAya_FL_Incongruent_MeanRT_E_pre <- (df$ SAya_FL_Incongruent_MeanRT_E_pre)
SAya_FL_Incongruent_MeanRT_Rest_E_pre <- (df$ SAya_FL_Incongruent_MeanRT_Rest_E_pre)
SAya_FL_Incongruent_MeanRT_E_post <- (df$ SAya_FL_Incongruent_MeanRT_E_post)
SAya_FL_Incongruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Incongruent_MeanRT_Rest_E_post)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
SAya_FL_Incongruent_MeanRT_Rest_E_Change<- SAya_FL_Incongruent_MeanRT_Rest_E_post-SAya_FL_Incongruent_MeanRT_Rest_E_pre
SAya_FL_Incongruent_MeanRT_E_Change<- SAya_FL_Incongruent_MeanRT_E_post-SAya_FL_Incongruent_MeanRT_E_pre
#create dataframe 
changeRT_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Incongruent_MeanRT_E_Change, SAya_FL_Incongruent_MeanRT_Rest_E_Change)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeRT_Incongruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeRT_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeRT_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeRT_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeRT_Incongruent_Model1_Mediation.df)
#mediation computation
model.M.changeRT_Incongruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeRT_Incongruent_Model1_Mediation.df)
model.Y.changeRT_Incongruent_Model1_Mediation<- lm(MeanRT_Change~Intervention + SAA_Change, longformat.changeRT_Incongruent_Model1_Mediation.df)
set.seed(123)
results.changeRT_Incongruent_Model1_Mediation<- mediate(model.M.changeRT_Incongruent_Model1_Mediation, model.Y.changeRT_Incongruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeRT_Incongruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = post Congruent Flanker RT
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-df$SAA_1_Rest_E
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_1_Exercise<- df$SAA_1_Exercise
SAA_4_Exercise<- df$SAA_4_Exercise
SAya_FL_Congruent_MeanRT_E_post <- (df$ SAya_FL_Congruent_MeanRT_E_post)
SAya_FL_Congruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Congruent_MeanRT_Rest_E_post)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#create dataframe 
changeSAA_PostRT_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Congruent_MeanRT_E_post, SAya_FL_Congruent_MeanRT_Rest_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostRT_Congruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostRT_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostRT_Congruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostRT_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostRT_Congruent_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostRT_Congruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostRT_Congruent_Model1_Mediation.df)
model.Y.changeSAA_PostRT_Congruent_Model1_Mediation<- lm(PostRT~Intervention + SAA_Change, longformat.changeSAA_PostRT_Congruent_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostRT_Congruent_Model1_Mediation<- mediate(model.M.changeSAA_PostRT_Congruent_Model1_Mediation, model.Y.changeSAA_PostRT_Congruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostRT_Congruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = post Incongruent Flanker RT
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-df$SAA_1_Rest_E
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_1_Exercise<- df$SAA_1_Exercise
SAA_4_Exercise<- df$SAA_4_Exercise
SAya_FL_Incongruent_MeanRT_E_post <- (df$ SAya_FL_Incongruent_MeanRT_E_post)
SAya_FL_Incongruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Incongruent_MeanRT_Rest_E_post)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#create dataframe 
changeSAA_PostRT_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Incongruent_MeanRT_E_post, SAya_FL_Incongruent_MeanRT_Rest_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostRT_Incongruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostRT_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostRT_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostRT_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostRT_Incongruent_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostRT_Incongruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostRT_Incongruent_Model1_Mediation.df)
model.Y.changeSAA_PostRT_Incongruent_Model1_Mediation<- lm(PostRT~Intervention + SAA_Change, longformat.changeSAA_PostRT_Incongruent_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostRT_Incongruent_Model1_Mediation<- mediate(model.M.changeSAA_PostRT_Incongruent_Model1_Mediation, model.Y.changeSAA_PostRT_Incongruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostRT_Incongruent_Model1_Mediation)


# MEDIATIONS - COGNITIVE OUTCOMES - Flanker Response Accuracy--------------------
#M1 Mediation - X = intervention group, M = sAA at Timepoint 4, Y = post intervention Congruent Flanker ACC
#Call Variables 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_4_Exercise<- df$SAA_4_Exercise
SAya_FL_Congruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_post)
SAya_FL_Congruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_post)
#create dataframe 
postACC_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, SAya_FL_Congruent_ResponseAccuracy_Rest_E_post, SAya_FL_Congruent_ResponseAccuracy_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(postACC_Congruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//postACC_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postACC_Congruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation/postACC_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postACC_Congruent_Model1_Mediation.df)
#mediation computation 
model.M.postACC_Congruent_Model1_Mediation<- lm(SAA~Intervention, longformat.postACC_Congruent_Model1_Mediation.df)
model.Y.postACC_Congruent_Model1_Mediation<- lm(Post_ACC~Intervention + SAA, longformat.postACC_Congruent_Model1_Mediation.df)
set.seed(123)
results.postACC_Congruent_Model1_Mediation<- mediate(model.M.postACC_Congruent_Model1_Mediation, model.Y.postACC_Congruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA', boot = TRUE, sims = 1000)
summary(results.postACC_Congruent_Model1_Mediation)

#M1 Mediation - X = Intervention Group, M = sAA at Timepoint 4, Y = post intervention Incongruent Flanker ACC
#Call Variables 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_4_Exercise<- df$SAA_4_Exercise
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
SAya_FL_Incongruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_post)
#create dataframe 
postACC_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post, SAya_FL_Incongruent_ResponseAccuracy_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(postACC_Incongruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//postACC_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postACC_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation/postACC_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postACC_Incongruent_Model1_Mediation.df)
#mediation computation 
model.M.postACC_Incongruent_Model1_Mediation<- lm(SAA~Intervention, longformat.postACC_Incongruent_Model1_Mediation.df)
model.Y.postACC_Incongruent_Model1_Mediation<- lm(Post_ACC~Intervention + SAA, longformat.postACC_Incongruent_Model1_Mediation.df)
set.seed(123)
results.postACC_Incongruent_Model1_Mediation<- mediate(model.M.postACC_Incongruent_Model1_Mediation, model.Y.postACC_Incongruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA', boot = TRUE, sims = 1000)
summary(results.postACC_Incongruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = change in Congruent Flanker ACC
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-df$SAA_1_Rest_E
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_1_Exercise<- df$SAA_1_Exercise
SAA_4_Exercise<- df$SAA_4_Exercise
SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre)
SAya_FL_Congruent_ResponseAccuracy_E_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_pre)
SAya_FL_Congruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_post)
SAya_FL_Congruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_post)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
SAya_FL_Congruent_ResponseAccuracy_Rest_E_Change<- SAya_FL_Congruent_ResponseAccuracy_Rest_E_post-SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre
SAya_FL_Congruent_ResponseAccuracy_E_Change<- SAya_FL_Congruent_ResponseAccuracy_E_post-SAya_FL_Congruent_ResponseAccuracy_E_pre
#create dataframe 
changeACC_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Congruent_ResponseAccuracy_E_Change, SAya_FL_Congruent_ResponseAccuracy_Rest_E_Change)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeACC_Congruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeACC_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeACC_Congruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeACC_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeACC_Congruent_Model1_Mediation.df)
#mediation computation
model.M.changeACC_Congruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeACC_Congruent_Model1_Mediation.df)
model.Y.changeACC_Congruent_Model1_Mediation<- lm(MeanACC_Change~Intervention + SAA_Change, longformat.changeACC_Congruent_Model1_Mediation.df)
set.seed(123)
results.changeACC_Congruent_Model1_Mediation<- mediate(model.M.changeACC_Congruent_Model1_Mediation, model.Y.changeACC_Congruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeACC_Congruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = change in Incongruent Flanker ACC
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-df$SAA_1_Rest_E
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_1_Exercise<- df$SAA_1_Exercise
SAA_4_Exercise<- df$SAA_4_Exercise
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre)
SAya_FL_Incongruent_ResponseAccuracy_E_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_pre)
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
SAya_FL_Incongruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_post)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_Change<- SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post-SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre
SAya_FL_Incongruent_ResponseAccuracy_E_Change<- SAya_FL_Incongruent_ResponseAccuracy_E_post-SAya_FL_Incongruent_ResponseAccuracy_E_pre
#create dataframe 
changeACC_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Incongruent_ResponseAccuracy_E_Change, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_Change)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeACC_Incongruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeACC_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeACC_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeACC_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeACC_Incongruent_Model1_Mediation.df)
#mediation computation
model.M.changeACC_Incongruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeACC_Incongruent_Model1_Mediation.df)
model.Y.changeACC_Incongruent_Model1_Mediation<- lm(MeanACC_Change~Intervention + SAA_Change, longformat.changeACC_Incongruent_Model1_Mediation.df)
set.seed(123)
results.changeACC_Incongruent_Model1_Mediation<- mediate(model.M.changeACC_Incongruent_Model1_Mediation, model.Y.changeACC_Incongruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeACC_Incongruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = post Congruent Flanker ACC
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-df$SAA_1_Rest_E
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_1_Exercise<- df$SAA_1_Exercise
SAA_4_Exercise<- df$SAA_4_Exercise
SAya_FL_Congruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_post)
SAya_FL_Congruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_post)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#create dataframe 
changeSAA_PostACC_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Congruent_ResponseAccuracy_E_post, SAya_FL_Congruent_ResponseAccuracy_Rest_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostACC_Congruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostACC_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostACC_Congruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostACC_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostACC_Congruent_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostACC_Congruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostACC_Congruent_Model1_Mediation.df)
model.Y.changeSAA_PostACC_Congruent_Model1_Mediation<- lm(PostACC~Intervention + SAA_Change, longformat.changeSAA_PostACC_Congruent_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostACC_Congruent_Model1_Mediation<- mediate(model.M.changeSAA_PostACC_Congruent_Model1_Mediation, model.Y.changeSAA_PostACC_Congruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostACC_Congruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = post Incongruent Flanker ACC
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-df$SAA_1_Rest_E
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_1_Exercise<- df$SAA_1_Exercise
SAA_4_Exercise<- df$SAA_4_Exercise
SAya_FL_Incongruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_post)
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#create dataframe 
changeSAA_PostACC_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Incongruent_ResponseAccuracy_E_post, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostACC_Incongruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostACC_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostACC_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostACC_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostACC_Incongruent_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostACC_Incongruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostACC_Incongruent_Model1_Mediation.df)
model.Y.changeSAA_PostACC_Incongruent_Model1_Mediation<- lm(PostACC~Intervention + SAA_Change, longformat.changeSAA_PostACC_Incongruent_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostACC_Incongruent_Model1_Mediation<- mediate(model.M.changeSAA_PostACC_Incongruent_Model1_Mediation, model.Y.changeSAA_PostACC_Incongruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostACC_Incongruent_Model1_Mediation)

# MEDIATIONS - BRAIN FUNCTION OUTCOMES - Flanker P3 Amplitude --------------
#M1 Mediation - X = intervention group, M = sAA at Timepoint 4, Y = post-intervention congruent P3 Amplitude
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_4_Exercise<- df$SAA_4_Exercise
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Amplitude, FC_Rest_E_po_P3_Con_CP1_Amplitude, FC_Rest_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.df <-data.frame(FC_E_po_P3_Con_CPZ_Amplitude, FC_E_po_P3_Con_CP1_Amplitude, FC_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Con_Amplitude.df)
#create dataframe 
postP3Amp_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(postP3Amp_Congruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//postP3Amp_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postP3Amp_Congruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation/postP3Amp_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postP3Amp_Congruent_Model1_Mediation.df)
#mediation computation 
model.M.postP3Amp_Congruent_Model1_Mediation<- lm(SAA~Intervention, longformat.postP3Amp_Congruent_Model1_Mediation.df)
model.Y.postP3Amp_Congruent_Model1_Mediation<- lm(Post_Amp~Intervention + SAA, longformat.postP3Amp_Congruent_Model1_Mediation.df)
set.seed(123)
results.postP3Amp_Congruent_Model1_Mediation<- mediate(model.M.postP3Amp_Congruent_Model1_Mediation, model.Y.postP3Amp_Congruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA', boot = TRUE, sims = 1000)
summary(results.postP3Amp_Congruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = sAA at Timepoint 4, Y = post-intervention Incongruent P3 Amplitude
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_4_Exercise<- df$SAA_4_Exercise
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Inc_CPZ_Amplitude, FC_Rest_E_po_P3_Inc_CP1_Amplitude, FC_Rest_E_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Inc_Amplitude.df <-data.frame(FC_E_po_P3_Inc_CPZ_Amplitude, FC_E_po_P3_Inc_CP1_Amplitude, FC_E_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Inc_Amplitude.df)
#create dataframe 
postP3Amp_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Exercise, Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(postP3Amp_Incongruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//postP3Amp_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postP3Amp_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation/postP3Amp_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postP3Amp_Incongruent_Model1_Mediation.df)
#mediation computation 
model.M.postP3Amp_Incongruent_Model1_Mediation<- lm(SAA~Intervention, longformat.postP3Amp_Incongruent_Model1_Mediation.df)
model.Y.postP3Amp_Incongruent_Model1_Mediation<- lm(Post_Amp~Intervention + SAA, longformat.postP3Amp_Incongruent_Model1_Mediation.df)
set.seed(123)
results.postP3Amp_Incongruent_Model1_Mediation<- mediate(model.M.postP3Amp_Incongruent_Model1_Mediation, model.Y.postP3Amp_Incongruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA', boot = TRUE, sims = 1000)
summary(results.postP3Amp_Incongruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = change in Congruent Flanker P3 Amplitude
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-df$SAA_1_Rest_E
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_1_Exercise<- df$SAA_1_Exercise
SAA_4_Exercise<- df$SAA_4_Exercise
Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_pre_P3_Con_CPZ_Amplitude, FC_Rest_E_pre_P3_Con_CP1_Amplitude, FC_Rest_E_pre_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.df)
Narrow.ROI_FC_E_pre_P3_Con_Amplitude.df <-data.frame(FC_E_pre_P3_Con_CPZ_Amplitude, FC_E_pre_P3_Con_CP1_Amplitude, FC_E_pre_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_pre_P3_Con_Amplitude.df)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Amplitude, FC_Rest_E_po_P3_Con_CP1_Amplitude, FC_Rest_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.df <-data.frame(FC_E_po_P3_Con_CPZ_Amplitude, FC_E_po_P3_Con_CP1_Amplitude, FC_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Con_Amplitude.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
SAya_FL_Congruent_P3Amp_Rest_E_Change<- Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean-Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean
SAya_FL_Congruent_P3Amp_E_Change<- Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean-Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean
#create dataframe 
changeP3Amp_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Congruent_P3Amp_E_Change, SAya_FL_Congruent_P3Amp_Rest_E_Change)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeP3Amp_Congruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeP3Amp_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeP3Amp_Congruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeP3Amp_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeP3Amp_Congruent_Model1_Mediation.df)
#mediation computation
model.M.changeACC_Congruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeACC_Congruent_Model1_Mediation.df)
model.Y.changeACC_Congruent_Model1_Mediation<- lm(MeanACC_Change~Intervention + SAA_Change, longformat.changeACC_Congruent_Model1_Mediation.df)
set.seed(123)
results.changeACC_Congruent_Model1_Mediation<- mediate(model.M.changeACC_Congruent_Model1_Mediation, model.Y.changeACC_Congruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeACC_Congruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = change in Incongruent Flanker P3 Amplitude
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-df$SAA_1_Rest_E
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_1_Exercise<- df$SAA_1_Exercise
SAA_4_Exercise<- df$SAA_4_Exercise
Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_pre_P3_Con_CPZ_Amplitude, FC_Rest_E_pre_P3_Con_CP1_Amplitude, FC_Rest_E_pre_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.df)
Narrow.ROI_FC_E_pre_P3_Con_Amplitude.df <-data.frame(FC_E_pre_P3_Con_CPZ_Amplitude, FC_E_pre_P3_Con_CP1_Amplitude, FC_E_pre_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_pre_P3_Con_Amplitude.df)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Amplitude, FC_Rest_E_po_P3_Con_CP1_Amplitude, FC_Rest_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.df <-data.frame(FC_E_po_P3_Con_CPZ_Amplitude, FC_E_po_P3_Con_CP1_Amplitude, FC_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Con_Amplitude.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
SAya_FL_Incongruent_P3Amp_Rest_E_Change<- Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean-Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean
SAya_FL_Incongruent_P3Amp_E_Change<- Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean-Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean
#create dataframe 
changeP3Amp_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Incongruent_P3Amp_E_Change, SAya_FL_Incongruent_P3Amp_Rest_E_Change)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeP3Amp_Incongruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeP3Amp_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeP3Amp_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeP3Amp_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeP3Amp_Incongruent_Model1_Mediation.df)
#mediation computation
model.M.changeACC_Incongruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeACC_Incongruent_Model1_Mediation.df)
model.Y.changeACC_Incongruent_Model1_Mediation<- lm(MeanACC_Change~Intervention + SAA_Change, longformat.changeACC_Incongruent_Model1_Mediation.df)
set.seed(123)
results.changeACC_Incongruent_Model1_Mediation<- mediate(model.M.changeACC_Incongruent_Model1_Mediation, model.Y.changeACC_Incongruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeACC_Incongruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = Post Congruent Flanker P3 Amplitude
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-df$SAA_1_Rest_E
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_1_Exercise<- df$SAA_1_Exercise
SAA_4_Exercise<- df$SAA_4_Exercise
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Amplitude, FC_Rest_E_po_P3_Con_CP1_Amplitude, FC_Rest_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.df <-data.frame(FC_E_po_P3_Con_CPZ_Amplitude, FC_E_po_P3_Con_CP1_Amplitude, FC_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Con_Amplitude.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#create dataframe 
changeSAA_PostP3Amp_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostP3Amp_Congruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostP3Amp_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostP3Amp_Congruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostP3Amp_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostP3Amp_Congruent_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostP3Amp_Congruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostP3Amp_Congruent_Model1_Mediation.df)
model.Y.changeSAA_PostP3Amp_Congruent_Model1_Mediation<- lm(PostP3Amp~Intervention + SAA_Change, longformat.changeSAA_PostP3Amp_Congruent_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostP3Amp_Congruent_Model1_Mediation<- mediate(model.M.changeSAA_PostP3Amp_Congruent_Model1_Mediation, model.Y.changeSAA_PostP3Amp_Congruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostP3Amp_Congruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = Post Incongruent Flanker P3 Amplitude
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-df$SAA_1_Rest_E
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_1_Exercise<- df$SAA_1_Exercise
SAA_4_Exercise<- df$SAA_4_Exercise
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Inc_CPZ_Amplitude, FC_Rest_E_po_P3_Inc_CP1_Amplitude, FC_Rest_E_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Inc_Amplitude.df <-data.frame(FC_E_po_P3_Inc_CPZ_Amplitude, FC_E_po_P3_Inc_CP1_Amplitude, FC_E_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Inc_Amplitude.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#create dataframe 
changeSAA_PostP3Amp_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostP3Amp_Incongruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostP3Amp_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostP3Amp_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostP3Amp_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostP3Amp_Incongruent_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostP3Amp_Incongruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostP3Amp_Incongruent_Model1_Mediation.df)
model.Y.changeSAA_PostP3Amp_Incongruent_Model1_Mediation<- lm(PostP3Amp~Intervention + SAA_Change, longformat.changeSAA_PostP3Amp_Incongruent_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostP3Amp_Incongruent_Model1_Mediation<- mediate(model.M.changeSAA_PostP3Amp_Incongruent_Model1_Mediation, model.Y.changeSAA_PostP3Amp_Incongruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostP3Amp_Incongruent_Model1_Mediation)

# MEDIATIONS - BRAIN FUNCTION OUTCOMES - Flanker P3 Latency ----------------
#M1 Mediation - X = intervention group, M = sAA at Timepoint 4, Y = post-intervention congruent P3 Latency
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_4_Exercise<- df$SAA_4_Exercise
Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Latency, FC_Rest_E_po_P3_Con_CP1_Latency, FC_Rest_E_po_P3_Con_CP2_Latency)
Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.df)
Narrow.ROI_FC_E_po_P3_Con_Latency.df <-data.frame(FC_E_po_P3_Con_CPZ_Latency, FC_E_po_P3_Con_CP1_Latency, FC_E_po_P3_Con_CP2_Latency)
Narrow.ROI_FC_E_po_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Con_Latency.df)
#create dataframe 
postP3Lat_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.mean, Narrow.ROI_FC_E_po_P3_Con_Latency.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(postP3Lat_Congruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//postP3Lat_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postP3Lat_Congruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation/postP3Lat_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postP3Lat_Congruent_Model1_Mediation.df)
#mediation computation 
model.M.postP3Lat_Congruent_Model1_Mediation<- lm(SAA~Intervention, longformat.postP3Lat_Congruent_Model1_Mediation.df)
model.Y.postP3Lat_Congruent_Model1_Mediation<- lm(Post_Lat~Intervention + SAA, longformat.postP3Lat_Congruent_Model1_Mediation.df)
set.seed(123)
results.postP3Lat_Congruent_Model1_Mediation<- mediate(model.M.postP3Lat_Congruent_Model1_Mediation, model.Y.postP3Lat_Congruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA', boot = TRUE, sims = 1000)
summary(results.postP3Lat_Congruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = sAA at Timepoint 4, Y = post-intervention Incongruent P3 Latency
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_4_Exercise<- df$SAA_4_Exercise
Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.df <-data.frame(FC_Rest_E_po_P3_Inc_CPZ_Latency, FC_Rest_E_po_P3_Inc_CP1_Latency, FC_Rest_E_po_P3_Inc_CP2_Latency)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.df)
Narrow.ROI_FC_E_po_P3_Inc_Latency.df <-data.frame(FC_E_po_P3_Inc_CPZ_Latency, FC_E_po_P3_Inc_CP1_Latency, FC_E_po_P3_Inc_CP2_Latency)
Narrow.ROI_FC_E_po_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Inc_Latency.df)
#create dataframe 
postP3Lat_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.mean, Narrow.ROI_FC_E_po_P3_Inc_Latency.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(postP3Lat_Incongruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//postP3Lat_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postP3Lat_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation/postP3Lat_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postP3Lat_Incongruent_Model1_Mediation.df)
#mediation computation 
model.M.postP3Lat_Incongruent_Model1_Mediation<- lm(SAA~Intervention, longformat.postP3Lat_Incongruent_Model1_Mediation.df)
model.Y.postP3Lat_Incongruent_Model1_Mediation<- lm(Post_Lat~Intervention + SAA, longformat.postP3Lat_Incongruent_Model1_Mediation.df)
set.seed(123)
results.postP3Lat_Incongruent_Model1_Mediation<- mediate(model.M.postP3Lat_Incongruent_Model1_Mediation, model.Y.postP3Lat_Incongruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA', boot = TRUE, sims = 1000)
summary(results.postP3Lat_Incongruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = change in Congruent Flanker P3 Latency
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-df$SAA_1_Rest_E
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_1_Exercise<- df$SAA_1_Exercise
SAA_4_Exercise<- df$SAA_4_Exercise
Narrow.ROI_FC_Rest_E_pre_P3_Con_Latency.df <-data.frame(FC_Rest_E_pre_P3_Con_CPZ_Latency, FC_Rest_E_pre_P3_Con_CP1_Latency, FC_Rest_E_pre_P3_Con_CP2_Latency)
Narrow.ROI_FC_Rest_E_pre_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Con_Latency.df)
Narrow.ROI_FC_E_pre_P3_Con_Latency.df <-data.frame(FC_E_pre_P3_Con_CPZ_Latency, FC_E_pre_P3_Con_CP1_Latency, FC_E_pre_P3_Con_CP2_Latency)
Narrow.ROI_FC_E_pre_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_E_pre_P3_Con_Latency.df)
Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Latency, FC_Rest_E_po_P3_Con_CP1_Latency, FC_Rest_E_po_P3_Con_CP2_Latency)
Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.df)
Narrow.ROI_FC_E_po_P3_Con_Latency.df <-data.frame(FC_E_po_P3_Con_CPZ_Latency, FC_E_po_P3_Con_CP1_Latency, FC_E_po_P3_Con_CP2_Latency)
Narrow.ROI_FC_E_po_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Con_Latency.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
SAya_FL_Congruent_P3Lat_Rest_E_Change<- Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.mean-Narrow.ROI_FC_Rest_E_pre_P3_Con_Latency.mean
SAya_FL_Congruent_P3Lat_E_Change<- Narrow.ROI_FC_E_po_P3_Con_Latency.mean-Narrow.ROI_FC_E_pre_P3_Con_Latency.mean
#create dataframe 
changeP3Lat_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Congruent_P3Lat_E_Change, SAya_FL_Congruent_P3Lat_Rest_E_Change)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeP3Lat_Congruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeP3Lat_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeP3Lat_Congruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeP3Lat_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeP3Lat_Congruent_Model1_Mediation.df)
#mediation computation
model.M.changeP3Lat_Congruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeP3Lat_Congruent_Model1_Mediation.df)
model.Y.changeP3Lat_Congruent_Model1_Mediation<- lm(MeanLat_Change~Intervention + SAA_Change, longformat.changeP3Lat_Congruent_Model1_Mediation.df)
set.seed(123)
results.changeP3Lat_Congruent_Model1_Mediation<- mediate(model.M.changeP3Lat_Congruent_Model1_Mediation, model.Y.changeP3Lat_Congruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeP3Lat_Congruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = change in Incongruent Flanker P3 Latency
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-df$SAA_1_Rest_E
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_1_Exercise<- df$SAA_1_Exercise
SAA_4_Exercise<- df$SAA_4_Exercise
Narrow.ROI_FC_Rest_E_pre_P3_Inc_Latency.df <-data.frame(FC_Rest_E_pre_P3_Inc_CPZ_Latency, FC_Rest_E_pre_P3_Inc_CP1_Latency, FC_Rest_E_pre_P3_Inc_CP2_Latency)
Narrow.ROI_FC_Rest_E_pre_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Inc_Latency.df)
Narrow.ROI_FC_E_pre_P3_Inc_Latency.df <-data.frame(FC_E_pre_P3_Inc_CPZ_Latency, FC_E_pre_P3_Inc_CP1_Latency, FC_E_pre_P3_Inc_CP2_Latency)
Narrow.ROI_FC_E_pre_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_E_pre_P3_Inc_Latency.df)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.df <-data.frame(FC_Rest_E_po_P3_Inc_CPZ_Latency, FC_Rest_E_po_P3_Inc_CP1_Latency, FC_Rest_E_po_P3_Inc_CP2_Latency)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.df)
Narrow.ROI_FC_E_po_P3_Inc_Latency.df <-data.frame(FC_E_po_P3_Inc_CPZ_Latency, FC_E_po_P3_Inc_CP1_Latency, FC_E_po_P3_Inc_CP2_Latency)
Narrow.ROI_FC_E_po_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Inc_Latency.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
SAya_FL_Incongruent_P3Lat_Rest_E_Change<- Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.mean-Narrow.ROI_FC_Rest_E_pre_P3_Inc_Latency.mean
SAya_FL_Incongruent_P3Lat_E_Change<- Narrow.ROI_FC_E_po_P3_Inc_Latency.mean-Narrow.ROI_FC_E_pre_P3_Inc_Latency.mean
#create dataframe 
changeP3Lat_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Incongruent_P3Lat_E_Change, SAya_FL_Incongruent_P3Lat_Rest_E_Change)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeP3Lat_Incongruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeP3Lat_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeP3Lat_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeP3Lat_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeP3Lat_Incongruent_Model1_Mediation.df)
#mediation computation
model.M.changeP3Lat_Incongruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeP3Lat_Incongruent_Model1_Mediation.df)
model.Y.changeP3Lat_Incongruent_Model1_Mediation<- lm(MeanLat_Change~Intervention + SAA_Change, longformat.changeP3Lat_Incongruent_Model1_Mediation.df)
set.seed(123)
results.changeP3Lat_Incongruent_Model1_Mediation<- mediate(model.M.changeP3Lat_Incongruent_Model1_Mediation, model.Y.changeP3Lat_Incongruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeP3Lat_Incongruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = Post Congruent Flanker P3 Latency
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-df$SAA_1_Rest_E
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_1_Exercise<- df$SAA_1_Exercise
SAA_4_Exercise<- df$SAA_4_Exercise
Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Latency, FC_Rest_E_po_P3_Con_CP1_Latency, FC_Rest_E_po_P3_Con_CP2_Latency)
Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.df)
Narrow.ROI_FC_E_po_P3_Con_Latency.df <-data.frame(FC_E_po_P3_Con_CPZ_Latency, FC_E_po_P3_Con_CP1_Latency, FC_E_po_P3_Con_CP2_Latency)
Narrow.ROI_FC_E_po_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Con_Latency.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#create dataframe 
changeSAA_PostP3Lat_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.mean, Narrow.ROI_FC_E_po_P3_Con_Latency.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostP3Lat_Congruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostP3Lat_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostP3Lat_Congruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostP3Lat_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostP3Lat_Congruent_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostP3Lat_Congruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostP3Lat_Congruent_Model1_Mediation.df)
model.Y.changeSAA_PostP3Lat_Congruent_Model1_Mediation<- lm(PostP3Lat~Intervention + SAA_Change, longformat.changeSAA_PostP3Lat_Congruent_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostP3Lat_Congruent_Model1_Mediation<- mediate(model.M.changeSAA_PostP3Lat_Congruent_Model1_Mediation, model.Y.changeSAA_PostP3Lat_Congruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostP3Lat_Congruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = Post Incongruent Flanker P3 Latency
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-df$SAA_1_Rest_E
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_1_Exercise<- df$SAA_1_Exercise
SAA_4_Exercise<- df$SAA_4_Exercise
Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.df <-data.frame(FC_Rest_E_po_P3_Inc_CPZ_Latency, FC_Rest_E_po_P3_Inc_CP1_Latency, FC_Rest_E_po_P3_Inc_CP2_Latency)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.df)
Narrow.ROI_FC_E_po_P3_Inc_Latency.df <-data.frame(FC_E_po_P3_Inc_CPZ_Latency, FC_E_po_P3_Inc_CP1_Latency, FC_E_po_P3_Inc_CP2_Latency)
Narrow.ROI_FC_E_po_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Inc_Latency.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#create dataframe 
changeSAA_PostP3Lat_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.mean, Narrow.ROI_FC_E_po_P3_Inc_Latency.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostP3Lat_Incongruent_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostP3Lat_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostP3Lat_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostP3Lat_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostP3Lat_Incongruent_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostP3Lat_Incongruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostP3Lat_Incongruent_Model1_Mediation.df)
model.Y.changeSAA_PostP3Lat_Incongruent_Model1_Mediation<- lm(PostP3Lat~Intervention + SAA_Change, longformat.changeSAA_PostP3Lat_Incongruent_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostP3Lat_Incongruent_Model1_Mediation<- mediate(model.M.changeSAA_PostP3Lat_Incongruent_Model1_Mediation, model.Y.changeSAA_PostP3Lat_Incongruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostP3Lat_Incongruent_Model1_Mediation)

# MEDIATIONS - BRAIN FUNCTION OUTCOMES - Resting Posterior Alpha ---------------
#M1 Mediation - X = intervention group, M = sAA at Timepoint 4, Y = post-intervention resting alpha
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_4_Exercise<- df$SAA_4_Exercise
ROI_FC_Rest_E_post_Alpha.df <-data.frame(P1_Alpha_Rest_E_Post, PZ_Alpha_Rest_E_Post, P2_Alpha_Rest_E_Post, PO3_Alpha_Rest_E_Post, POZ_Alpha_Rest_E_Post, PO4_Alpha_Rest_E_Post)
ROI_FC_Rest_E_post_Alpha.mean <- rowMeans(ROI_FC_Rest_E_post_Alpha.df)
ROI_FC_E_post_Alpha.df <-data.frame(P1_Alpha_E_Post, PZ_Alpha_E_Post, P2_Alpha_E_Post, PO3_Alpha_E_Post, POZ_Alpha_E_Pre, PO4_Alpha_E_Post)
ROI_FC_E_post_Alpha.mean <- rowMeans(ROI_FC_E_post_Alpha.df)
#create dataframe 
postAlpha_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, ROI_FC_Rest_E_post_Alpha.mean, ROI_FC_E_post_Alpha.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(postAlpha_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//postAlpha_Model1_Mediation.df.xlsx') #export long format df
longformat.postAlpha_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation/postAlpha_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postAlpha_Model1_Mediation.df) #980 MEAN REPLACED - NEGATIVE VALUE! 
#mediation computation 
model.M.postAlpha_Model1_Mediation.df<- lm(SAA~Intervention, longformat.postAlpha_Model1_Mediation.df)
model.Y.postAlpha_Model1_Mediation.df<- lm(Post_Alpha~Intervention + SAA, longformat.postAlpha_Model1_Mediation.df)
set.seed(123)
results.postAlpha_Model1_Mediation<- mediate(model.M.postAlpha_Model1_Mediation.df, model.Y.postAlpha_Model1_Mediation.df, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA', boot = TRUE, sims = 1000)
summary(results.postAlpha_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = change in alpha 
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_4_Exercise<- df$SAA_4_Exercise
ROI_FC_Rest_E_pre_Alpha.df <-data.frame(P1_Alpha_Rest_E_Pre, PZ_Alpha_Rest_E_Pre, P2_Alpha_Rest_E_Pre, PO3_Alpha_Rest_E_Pre, POZ_Alpha_Rest_E_Pre, PO4_Alpha_Rest_E_Pre)
ROI_FC_Rest_E_pre_Alpha.mean <- rowMeans(ROI_FC_Rest_E_pre_Alpha.df)
ROI_FC_E_pre_Alpha.df <-data.frame(P1_Alpha_E_Pre, PZ_Alpha_E_Pre, P2_Alpha_E_Pre, PO3_Alpha_E_Pre, POZ_Alpha_E_Pre, PO4_Alpha_E_Pre)
ROI_FC_E_pre_Alpha.mean <- rowMeans(ROI_FC_E_pre_Alpha.df)
ROI_FC_Rest_E_post_Alpha.df <-data.frame(P1_Alpha_Rest_E_Post, PZ_Alpha_Rest_E_Post, P2_Alpha_Rest_E_Post, PO3_Alpha_Rest_E_Post, POZ_Alpha_Rest_E_Post, PO4_Alpha_Rest_E_Post)
ROI_FC_Rest_E_post_Alpha.mean <- rowMeans(ROI_FC_Rest_E_post_Alpha.df)
ROI_FC_E_post_Alpha.df <-data.frame(P1_Alpha_E_Post, PZ_Alpha_E_Post, P2_Alpha_E_Post, PO3_Alpha_E_Post, POZ_Alpha_E_Pre, PO4_Alpha_E_Post)
ROI_FC_E_post_Alpha.mean <- rowMeans(ROI_FC_E_post_Alpha.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
ROI_Alpha_Rest_E_Change<- ROI_FC_Rest_E_post_Alpha.mean-ROI_FC_Rest_E_pre_Alpha.mean
ROI_Alpha_E_Change<- ROI_FC_E_post_Alpha.mean-ROI_FC_E_pre_Alpha.mean
#Create dataframe
changeAlpha_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, ROI_Alpha_Rest_E_Change, ROI_Alpha_E_Change)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeAlpha_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeAlpha_Model1_Mediation.df.xlsx') #export long format df
longformat.changeAlpha_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeAlpha_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeAlpha_Model1_Mediation.df)
#mediation computation
model.M.changeAlpha_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeAlpha_Model1_Mediation.df)
model.Y.changeAlpha_Model1_Mediation<- lm(Alpha_Change~Intervention + SAA_Change, longformat.changeAlpha_Model1_Mediation.df)
set.seed(123)
results.changeAlpha_Model1_Mediation<- mediate(model.M.changeAlpha_Model1_Mediation, model.Y.changeAlpha_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeAlpha_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = post alpha 
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_4_Exercise<- df$SAA_4_Exercise
ROI_FC_Rest_E_post_Alpha.df <-data.frame(P1_Alpha_Rest_E_Post, PZ_Alpha_Rest_E_Post, P2_Alpha_Rest_E_Post, PO3_Alpha_Rest_E_Post, POZ_Alpha_Rest_E_Post, PO4_Alpha_Rest_E_Post)
ROI_FC_Rest_E_post_Alpha.mean <- rowMeans(ROI_FC_Rest_E_post_Alpha.df)
ROI_FC_E_post_Alpha.df <-data.frame(P1_Alpha_E_Post, PZ_Alpha_E_Post, P2_Alpha_E_Post, PO3_Alpha_E_Post, POZ_Alpha_E_Pre, PO4_Alpha_E_Post)
ROI_FC_E_post_Alpha.mean <- rowMeans(ROI_FC_E_post_Alpha.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#Create dataframe
changeSAA_PostAlpha_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, ROI_FC_Rest_E_post_Alpha.mean, ROI_FC_E_post_Alpha.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostAlpha_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostAlpha_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostAlpha_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostAlpha_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostAlpha_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostAlpha_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostAlpha_Model1_Mediation.df)
model.Y.changeSAA_PostAlpha_Model1_Mediation<- lm(PostAlpha~Intervention + SAA_Change, longformat.changeSAA_PostAlpha_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostAlpha_Model1_Mediation<- mediate(model.M.changeSAA_PostAlpha_Model1_Mediation, model.Y.changeSAA_PostAlpha_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostAlpha_Model1_Mediation)

# MEDIATIONS - BRAIN FUNCTION OUTCOMES - Resting Midfrontal Theta ---------
#M1 Mediation - X = intervention group, M = sAA at Timepoint 4, Y = post-intervention resting theta
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_4_Exercise<- df$SAA_4_Exercise
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_E_Pre, FCZ_Theta_E_Post, FZ_Theta_E_Post, FC1_Theta_E_Post, FC2_Theta_E_Pre, C1_Theta_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_Rest_E_Post, FCZ_Theta_Rest_E_Post, FZ_Theta_Rest_E_Post, FC1_Theta_Rest_E_Post, FC2_Theta_Rest_E_Post, C1_Theta_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#create dataframe 
postTheta_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(postTheta_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//postTheta_Model1_Mediation.df.xlsx') #export long format df
longformat.postTheta_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation/postTheta_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postTheta_Model1_Mediation.df)
#mediation computation 
model.M.postTheta_Model1_Mediation.df<- lm(SAA~Intervention, longformat.postTheta_Model1_Mediation.df)
model.Y.postTheta_Model1_Mediation.df<- lm(Post_Theta~Intervention + SAA, longformat.postTheta_Model1_Mediation.df)
set.seed(123)
results.postTheta_Model1_Mediation<- mediate(model.M.postTheta_Model1_Mediation.df, model.Y.postTheta_Model1_Mediation.df, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA', boot = TRUE, sims = 1000)
summary(results.postTheta_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = change in theta 
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_4_Exercise<- df$SAA_4_Exercise
Wide.ROI_FC_E_pre_Theta.df <-data.frame(F1_Theta_E_Pre, FCZ_Theta_E_Pre, FZ_Theta_E_Pre, FC1_Theta_E_Pre, FC2_Theta_E_Pre, C1_Theta_E_Pre)
Wide.ROI_FC_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_E_pre_Theta.df)
Wide.ROI_FC_Rest_E_pre_Theta.df <-data.frame(F1_Theta_Rest_E_Pre, FCZ_Theta_Rest_E_Pre, FZ_Theta_Rest_E_Pre, FC1_Theta_Rest_E_Pre, FC2_Theta_Rest_E_Pre, C1_Theta_Rest_E_Pre)
Wide.ROI_FC_Rest_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_pre_Theta.df)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_E_Pre, FCZ_Theta_E_Post, FZ_Theta_E_Post, FC1_Theta_E_Post, FC2_Theta_E_Pre, C1_Theta_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_Rest_E_Post, FCZ_Theta_Rest_E_Post, FZ_Theta_Rest_E_Post, FC1_Theta_Rest_E_Post, FC2_Theta_Rest_E_Post, C1_Theta_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
ROI_Theta_Rest_E_Change<- Wide.ROI_FC_Rest_E_post_Theta.mean-Wide.ROI_FC_Rest_E_pre_Theta.mean
ROI_Theta_E_Change<- Wide.ROI_FC_E_post_Theta.mean-Wide.ROI_FC_E_pre_Theta.mean
#Create dataframe
changeTheta_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, ROI_Theta_Rest_E_Change, ROI_Theta_E_Change)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeTheta_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeTheta_Model1_Mediation.df.xlsx') #export long format df
longformat.changeTheta_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeTheta_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeTheta_Model1_Mediation.df)
#mediation computation
model.M.changeTheta_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeTheta_Model1_Mediation.df)
model.Y.changeTheta_Model1_Mediation<- lm(Theta_Change~Intervention + SAA_Change, longformat.changeTheta_Model1_Mediation.df)
set.seed(123)
results.changeTheta_Model1_Mediation<- mediate(model.M.changeTheta_Model1_Mediation, model.Y.changeTheta_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeTheta_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = post Theta 
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_4_Exercise<- df$SAA_4_Exercise
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_E_Pre, FCZ_Theta_E_Post, FZ_Theta_E_Post, FC1_Theta_E_Post, FC2_Theta_E_Pre, C1_Theta_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_Rest_E_Post, FCZ_Theta_Rest_E_Post, FZ_Theta_Rest_E_Post, FC1_Theta_Rest_E_Post, FC2_Theta_Rest_E_Post, C1_Theta_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#Create dataframe
changeSAA_PostTheta_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, Wide.ROI_FC_Rest_E_post_Theta.mean, Wide.ROI_FC_E_post_Theta.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostTheta_Model1_Mediation.df, '/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostTheta_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostTheta_Model1_Mediation.df <- read_excel("/Volumes/SAya_USB/Server_Dissertation//changeSAA_PostTheta_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostTheta_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostTheta_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostTheta_Model1_Mediation.df)
model.Y.changeSAA_PostTheta_Model1_Mediation<- lm(PostTheta~Intervention + SAA_Change, longformat.changeSAA_PostTheta_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostTheta_Model1_Mediation<- mediate(model.M.changeSAA_PostTheta_Model1_Mediation, model.Y.changeSAA_PostTheta_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostTheta_Model1_Mediation)




# Old stats ------------------------------------------------
#COGNITIVE OUTCOMES STATS
#M1: Flanker RT Exercise vs. Rest
#MeanRT
SubjectID<-(df$SubjectID)
SAya_FL_Congruent_MeanRT_Rest_E_pre <- (df$ SAya_FL_Congruent_MeanRT_Rest_E_pre)
SAya_FL_Congruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Congruent_MeanRT_Rest_E_post)
SAya_FL_Incongruent_MeanRT_Rest_E_pre <- (df$ SAya_FL_Incongruent_MeanRT_Rest_E_pre)
SAya_FL_Incongruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Incongruent_MeanRT_Rest_E_post)
SAya_FL_Congruent_MeanRT_E_pre <- (df$ SAya_FL_Congruent_MeanRT_E_pre)
SAya_FL_Congruent_MeanRT_E_post <- (df$ SAya_FL_Congruent_MeanRT_E_post)
SAya_FL_Incongruent_MeanRT_E_pre <- (df$ SAya_FL_Incongruent_MeanRT_E_pre)
SAya_FL_Incongruent_MeanRT_E_post <- (df$ SAya_FL_Incongruent_MeanRT_E_post)
M1_RT.df<-data.frame(SubjectID, SAya_FL_Congruent_MeanRT_Rest_E_pre, SAya_FL_Congruent_MeanRT_Rest_E_post, SAya_FL_Incongruent_MeanRT_Rest_E_pre, SAya_FL_Incongruent_MeanRT_Rest_E_post,
                                SAya_FL_Congruent_MeanRT_E_pre, SAya_FL_Congruent_MeanRT_E_post, SAya_FL_Incongruent_MeanRT_E_pre, SAya_FL_Incongruent_MeanRT_E_post)
longformat.M1_RT.df <- melt(M1_RT.df, id = "SubjectID", variable.name = "Condition")
summary(M1_RT.df)
#database needs to be formatted for RM ANOVA, add 3 columns and sort by Condition (2x2x2), resave and reupload
write_xlsx(longformat.M1_RT.df, '/Volumes/SAya_USB/Server_Dissertation//longformat.M1_RT.df.xlsx') #export long format df
longformat.M1_RT.df.edited <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/longformat.M1_RT.df.edited.xlsx") #reupload new df
View(longformat.M1_RT.df.edited) #check and see if new version 
#convert everything but value into factors
longformat.M1_RT.df.edited <- longformat.M1_RT.df.edited %>%
  convert_as_factor(SubjectID, Condition, Intervention, Timepoint, Congruency)
#summary stats
longformat.M1_RT.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  get_summary_stats(Average.RT.ms, type = "mean_sd")
#visualize
ggboxplot(longformat.M1_RT.df.edited, x = "Timepoint", y = "Average.RT.ms",
  color = "Congruency", facet.by = "Intervention", short.panel.labs = FALSE)
#check for outliers
longformat.M1_RT.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  identify_outliers(Average.RT.ms)
#check for normality
longformat.M1_RT.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  shapiro_test(Average.RT.ms) 
ggqqplot(longformat.M1_RT.df.edited, "Average.RT.ms", ggtheme = theme_bw()) +
  facet_grid(Intervention+Congruency ~ Timepoint, labeller = "label_both") #if points fall approx along ref line, normality is assumed
#computation 
M1_RT.aov <- anova_test(data = longformat.M1_RT.df.edited, dv = Average.RT.ms, wid = SubjectID, within = c(Intervention, Timepoint, Congruency), type = 3)
get_anova_table(M1_RT.aov) #check for 3-way interaction, if none, check two way interactions with pairwise comparisons
#Investigate main effects
t.test(longformat.M1_RT.df.edited$Average.RT.ms~longformat.M1_RT.df.edited$Congruency, paired = TRUE)
#Visualize 
Main_Effect<-longformat.M1_RT.df.edited %>%
  group_by(Congruency) %>%
  get_summary_stats(Average.RT.ms, type = "mean_sd")
Main_Effect %>%
  ggplot(aes(x=Congruency, y = mean))+
  geom_point(size = 5)

#M2: Flanker RT Trier vs. Rest
#MeanRT
SubjectID<-(df$SubjectID)
SAya_FL_Congruent_MeanRT_Rest_T_pre <- (df$ SAya_FL_Congruent_MeanRT_Rest_T_pre)
SAya_FL_Congruent_MeanRT_Rest_T_post <- (df$ SAya_FL_Congruent_MeanRT_Rest_T_post)
SAya_FL_Incongruent_MeanRT_Rest_T_pre <- (df$ SAya_FL_Incongruent_MeanRT_Rest_T_pre)
SAya_FL_Incongruent_MeanRT_Rest_T_post <- (df$ SAya_FL_Incongruent_MeanRT_Rest_T_post)
SAya_FL_Congruent_MeanRT_T_pre <- (df$ SAya_FL_Congruent_MeanRT_T_pre)
SAya_FL_Congruent_MeanRT_T_post <- (df$ SAya_FL_Congruent_MeanRT_T_post)
SAya_FL_Incongruent_MeanRT_T_pre <- (df$ SAya_FL_Incongruent_MeanRT_T_pre)
SAya_FL_Incongruent_MeanRT_T_post <- (df$ SAya_FL_Incongruent_MeanRT_T_post)
M2_RT.df<-data.frame(SubjectID, SAya_FL_Congruent_MeanRT_Rest_T_pre, SAya_FL_Congruent_MeanRT_Rest_T_post, SAya_FL_Incongruent_MeanRT_Rest_T_pre, SAya_FL_Incongruent_MeanRT_Rest_T_post,
                     SAya_FL_Congruent_MeanRT_T_pre, SAya_FL_Congruent_MeanRT_T_post, SAya_FL_Incongruent_MeanRT_T_pre, SAya_FL_Incongruent_MeanRT_T_post)
longformat.M2_RT.df <- melt(M2_RT.df, id = "SubjectID", variable.name = "Condition")
summary(M2_RT.df)
#database needs to be formatted for RM ANOVA, add 3 columns and sort by Condition (2x2x2), resave and reupload
write_xlsx(longformat.M2_RT.df, '/Volumes/SAya_USB/Server_Dissertation//longformat.M2_RT.df.xlsx') #export long format df
longformat.M2_RT.df.edited <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/longformat.M2_RT.df.edited.xlsx") #reupload new df
View(longformat.M2_RT.df.edited) #check and see if new version 
#convert everything but value into factors
longformat.M2_RT.df.edited <- longformat.M2_RT.df.edited %>%
  convert_as_factor(SubjectID, Condition, Intervention, Timepoint, Congruency)
#summary stats
longformat.M2_RT.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  get_summary_stats(Average.RT.ms, type = "mean_sd")
#visualize
ggboxplot(longformat.M2_RT.df.edited, x = "Timepoint", y = "Average.RT.ms",
          color = "Intervention", facet.by = "Congruency", short.panel.labs = FALSE)
#check for outliers
longformat.M2_RT.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  identify_outliers(Average.RT.ms)
#check for normality
longformat.M2_RT.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  shapiro_test(Average.RT.ms) 
ggqqplot(longformat.M2_RT.df.edited, "Average.RT.ms", ggtheme = theme_bw()) +
  facet_grid(Intervention+Congruency ~ Timepoint, labeller = "label_both") #if points fall approx along ref line, normality is assumed
hist(1/longformat.M2_RT.df.edited$Average.RT.ms)
#computation 
M2_RT.aov <- anova_test(data = longformat.M2_RT.df.edited, dv = Average.RT.ms, wid = SubjectID, within = c(Intervention, Timepoint, Congruency), type = 3)
get_anova_table(M2_RT.aov) #check for 3-way interaction, if none, check two way interactions with pairwise comparisons

#nonparametric option-Friedman test
friedman.test(y = longformat.M2_RT.df.edited$Average.RT.ms, groups = longformat.M2_RT.df.edited$Condition, blocks = longformat.M2_RT.df.edited$SubjectID)
pairwise.wilcox.test(longformat.M2_RT.df.edited$Average.RT.ms, longformat.M2_RT.df.edited$Condition, p.adj = "bonf")

ggwithinstats(data = longformat.M2_RT.df.edited, x = Condition, y = Average.RT.ms, type = "nonparametric")

#M3: Flanker RT effect sizes M1 vs. M2 

#M1: Flanker ACC Exercise vs. Rest 
#ResponseAccuracy
SubjectID<-(df$SubjectID)
SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre)
SAya_FL_Congruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_post)
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre)
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
SAya_FL_Congruent_ResponseAccuracy_E_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_pre)
SAya_FL_Congruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_post)
SAya_FL_Incongruent_ResponseAccuracy_E_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_pre)
SAya_FL_Incongruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_post)
M1_ACC.df<-data.frame(SubjectID, SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre, SAya_FL_Congruent_ResponseAccuracy_Rest_E_post, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post,
                      SAya_FL_Congruent_ResponseAccuracy_E_pre, SAya_FL_Congruent_ResponseAccuracy_E_post, SAya_FL_Incongruent_ResponseAccuracy_E_pre, SAya_FL_Incongruent_ResponseAccuracy_E_post)
longformat.M1_ACC.df <- melt(M1_ACC.df, id = "SubjectID", variable.name = "Condition")
summary(M1_ACC.df)
#database needs to be formatted for RM ANOVA, add 3 columns and sort by Condition (2x2x2), resave and reupload
write_xlsx(longformat.M1_ACC.df, '/Volumes/SAya_USB/Server_Dissertation//longformat.M1_ACC.df.xlsx') #export long format df
longformat.M1_ACC.df.edited <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/longformat.M1_ACC.df.edited.xlsx") #reupload new df
View(longformat.M1_ACC.df.edited) #check and see if new version 
#convert everything but value into factors
longformat.M1_ACC.df.edited <- longformat.M1_ACC.df.edited %>%
  convert_as_factor(SubjectID, Condition, Intervention, Timepoint, Congruency)
#summary stats
longformat.M1_ACC.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  get_summary_stats(Accuracy.percent, type = "mean_sd")
#visualize
ggboxplot(longformat.M1_ACC.df.edited, x = "Congruency", y = "Accuracy.percent", color = "Intervention", facet.by = "Timepoint", short.panel.labs = FALSE)
#check for outliers
longformat.M1_ACC.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  identify_outliers(Accuracy.percent)
#check for normality
longformat.M1_ACC.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  shapiro_test(Accuracy.percent)
ggqqplot(longformat.M1_ACC.df.edited, "Accuracy.percent", ggtheme = theme_bw()) +
  facet_grid(Intervention+Congruency ~ Timepoint, labeller = "label_both") #if points fall approx along ref line, normality is assumed
#computation 
M1_ACC.aov<-anova_test(data = longformat.M1_ACC.df.edited, dv = Accuracy.percent, wid = SubjectID, within = c(Intervention, Timepoint, Congruency), type = 3)
M1_ACC.aov
#Investigate main effects
t.test(longformat.M1_ACC.df.edited$Accuracy.percent~longformat.M1_ACC.df.edited$Congruency, paired = TRUE)
#Visualize 
Main_Effect<-longformat.M1_ACC.df.edited %>%
  group_by(Congruency) %>%
  get_summary_stats(Accuracy.percent, type = "mean_sd")
Main_Effect %>%
  ggplot(aes(x=Congruency, y = mean))+
  geom_point(size = 5)
#Post-hoc computations using Bonferroni adjustment for interactions
#Simple Two-way interaction (may require switching variables around to maintain interaction?)
M1_ACC_posthoc_main <- longformat.M1_ACC.df.edited %>%
  group_by(Congruency)%>% 
  anova_test(dv = Accuracy.percent, wid = SubjectID, within = c(Intervention, Timepoint)) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
M1_ACC_posthoc_main
#Simple Simple Main Effect
M1_ACC_posthoc_simple <- longformat.M1_ACC.df.edited %>%
  group_by(Congruency, Timepoint)%>% 
  anova_test(dv = Accuracy.percent, wid = SubjectID, within = Intervention)
M1_ACC_posthoc_simple
M1_ACC_posthoc_simple <- longformat.M1_ACC.df.edited %>%
  group_by(Intervention, Congruency)%>% 
  anova_test(dv = Accuracy.percent, wid = SubjectID, within =Timepoint)
M1_ACC_posthoc_simple
#Pairwise Multiple Comparisons
M1_ACC_posthoc_pwc<- longformat.M1_ACC.df.edited %>%
  group_by(Timepoint)%>% 
  pairwise_t_test(Accuracy.percent~Congruency, paired = TRUE, p.adjust.method = "bonferroni")
M1_ACC_posthoc_pwc
#Visualize PostHoc comparisons
post_hoc_interaction<-longformat.M1_ACC.df.edited %>%
  group_by(Intervention, Timepoint) %>%
  get_summary_stats(Accuracy.percent, type = "mean_sd")
post_hoc_interaction %>%
  mutate(Timepoint = factor(Timepoint, levels = c("Pre", "Post")))%>%
ggplot(aes(Timepoint, y = mean, color = Intervention))+
  geom_line(size = 1)+
  geom_point(aes(shape = Intervention), size = 3)

M1_ACC_posthoc_pwc <- M1_ACC_posthoc_pwc %>% add_xy_position(x = "Congruency")
ggboxplot(longformat.M1_ACC.df.edited, x = "Congruency", y = "Accuracy.percent", color = "Intervention", palette = "jco", facet.by = "Timepoint", short.panel.labs = FALSE)+
  stat_pvalue_manual(M1_ACC_posthoc_pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(M1_ACC.aov, detailed = TRUE),
    caption = get_pwc_label(M1_ACC_posthoc_pwc)
  )

#M2: Flanker ACC Trier vs. Rest 
#ResponseAccuracy
SubjectID<-(df$SubjectID)
SAya_FL_Congruent_ResponseAccuracy_Rest_T_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_T_pre)
SAya_FL_Congruent_ResponseAccuracy_Rest_T_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_T_post)
SAya_FL_Incongruent_ResponseAccuracy_Rest_T_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_T_pre)
SAya_FL_Incongruent_ResponseAccuracy_Rest_T_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_T_post)
SAya_FL_Congruent_ResponseAccuracy_T_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_T_pre)
SAya_FL_Congruent_ResponseAccuracy_T_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_T_post)
SAya_FL_Incongruent_ResponseAccuracy_T_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_T_pre)
SAya_FL_Incongruent_ResponseAccuracy_T_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_T_post)
M2_ACC.df<-data.frame(SubjectID, SAya_FL_Congruent_ResponseAccuracy_Rest_T_pre, SAya_FL_Congruent_ResponseAccuracy_Rest_T_post, SAya_FL_Incongruent_ResponseAccuracy_Rest_T_pre, SAya_FL_Incongruent_ResponseAccuracy_Rest_T_post,
                      SAya_FL_Congruent_ResponseAccuracy_T_pre, SAya_FL_Congruent_ResponseAccuracy_T_post, SAya_FL_Incongruent_ResponseAccuracy_T_pre, SAya_FL_Incongruent_ResponseAccuracy_T_post)
longformat.M2_ACC.df <- melt(M2_ACC.df, id = "SubjectID", variable.name = "Condition")
summary(M2_ACC.df)
#database needs to be formatted for RM ANOVA, add 3 columns and sort by Condition (2x2x2), resave and reupload
write_xlsx(longformat.M2_ACC.df, '/Volumes/SAya_USB/Server_Dissertation//longformat.M2_ACC.df.xlsx') #export long format df
longformat.M2_ACC.df.edited <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/longformat.M2_ACC.df.edited.xlsx") #reupload new df
View(longformat.M2_ACC.df.edited) #check and see if new version 
#convert everything but value into factors
longformat.M2_ACC.df.edited <- longformat.M2_ACC.df.edited %>%
  convert_as_factor(SubjectID, Condition, Intervention, Timepoint, Congruency)
#summary stats
longformat.M2_ACC.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  get_summary_stats(Accuracy.percent, type = "mean_sd")
#visualize
ggboxplot(longformat.M2_ACC.df.edited, x = "Congruency", y = "Accuracy.percent", color = "Intervention", facet.by = "Timepoint", short.panel.labs = FALSE)
#check for outliers
longformat.M2_ACC.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  identify_outliers(Accuracy.percent)
#check for normality
longformat.M2_ACC.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  shapiro_test(Accuracy.percent) 
ggqqplot(longformat.M2_ACC.df.edited, "Accuracy.percent", ggtheme = theme_bw()) +
  facet_grid(Intervention+Congruency ~ Timepoint, labeller = "label_both") #if points fall approx along ref line, normality is assumed
#computation 
M2_ACC.aov <- anova_test(data = longformat.M2_ACC.df.edited, dv = Accuracy.percent, wid = SubjectID, within = c(Intervention, Timepoint, Congruency, type = 3))
get_anova_table(M2_ACC.aov) #check for 3-way interaction, if none, check two way interactions with pairwise comparisons
#Investigate main effects
t.test(longformat.M2_ACC.df.edited$Accuracy.percent~longformat.M2_ACC.df.edited$Timepoint, paired = TRUE)
t.test(longformat.M2_ACC.df.edited$Accuracy.percent~longformat.M2_ACC.df.edited$Congruency, paired = TRUE)
#Visualize 
Main_Effect<-longformat.M2_ACC.df.edited %>%
  group_by(Congruency) %>%
  get_summary_stats(Accuracy.percent, type = "mean_sd")
Main_Effect %>%
  ggplot(aes(x=Congruency, y = mean))+
  geom_point(size = 5)

Main_Effect<-longformat.M2_ACC.df.edited %>%
  group_by(Timepoint) %>%
  get_summary_stats(Accuracy.percent, type = "mean_sd")
Main_Effect %>%
  mutate(Timepoint = factor(Timepoint, levels = c("Pre", "Post")))%>%
  ggplot(aes(x=Timepoint, y = mean))+
  geom_point(size = 5)
#Post-hoc computations using Bonferroni adjustment
#Simple Two-way interaction (may require switching variables around to maintain interaction?)
M2_ACC_posthoc_main <- longformat.M2_ACC.df.edited %>%
  group_by(Intervention)%>% 
  anova_test(dv = Accuracy.percent, wid = SubjectID, within = c(Congruency, Timepoint)) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
M2_ACC_posthoc_main
#Simple Simple Main Effect
M2_ACC_posthoc_simple <- longformat.M2_ACC.df.edited %>%
  group_by(Intervention, Timepoint)%>% 
  anova_test(dv = Accuracy.percent, wid = SubjectID, within = Congruency)
M2_ACC_posthoc_simple
M2_ACC_posthoc_simple <- longformat.M2_ACC.df.edited %>%
  group_by(Intervention, Congruency)%>% 
  anova_test(dv = Accuracy.percent, wid = SubjectID, within =Timepoint)
M2_ACC_posthoc_simple
#Pairwise Multiple Comparisons
M2_ACC_posthoc_pwc<- longformat.M2_ACC.df.edited %>%
  group_by(Timepoint)%>% 
  pairwise_t_test(Accuracy.percent~Congruency, paired = TRUE, p.adjust.method = "bonferroni")
M2_ACC_posthoc_pwc
#Visualize PostHoc comparisons
post_hoc_interaction<-longformat.M2_ACC.df.edited %>%
  group_by(Congruency, Timepoint) %>%
  get_summary_stats(Accuracy.percent, type = "mean_sd")
post_hoc_interaction %>%
  mutate(Timepoint = factor(Timepoint, levels = c("Pre", "Post")))%>%
  ggplot(aes(Timepoint, y = mean, color = Congruency))+
  geom_line(size = 1)+
  geom_point(aes(shape = Congruency), size = 3)

M2_ACC_posthoc_pwc <- M2_ACC_posthoc_pwc %>% add_xy_position(x = "Timepoint")
ggboxplot(longformat.M2_ACC.df.edited, x = "Congruency", y = "Accuracy.percent", color = "Intervention", palette = "jco", facet.by = "Timepoint", short.panel.labs = FALSE)+
  stat_pvalue_manual(M2_ACC_posthoc_pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(M2_ACC.aov, detailed = TRUE),
    caption = get_pwc_label(M2_ACC_posthoc_pwc)
      )

#M3: Flanker ACC effect sizes M1 vs. M2 

#M1: Flanker Inverse Efficiency Exercise vs. Rest 
SubjectID<-(df$SubjectID)
SAya_FL_Congruent_InverseEfficiency_Rest_E_pre<-(df$SAya_FL_Congruent_InverseEfficiency_Rest_E_pre)
SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre<-(df$SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre)
SAya_FL_Congruent_InverseEfficiency_Rest_E_post<-(df$SAya_FL_Congruent_InverseEfficiency_Rest_E_post)
SAya_FL_Incongruent_InverseEfficiency_Rest_E_post<-(df$SAya_FL_Incongruent_InverseEfficiency_Rest_E_post)
SAya_FL_Congruent_InverseEfficiency_E_pre<-(df$SAya_FL_Congruent_InverseEfficiency_E_pre)
SAya_FL_Incongruent_InverseEfficiency_E_pre<-(df$SAya_FL_Incongruent_InverseEfficiency_E_pre)
SAya_FL_Congruent_InverseEfficiency_E_post<-(df$SAya_FL_Congruent_InverseEfficiency_E_post)
SAya_FL_Incongruent_InverseEfficiency_E_post<-(df$SAya_FL_Incongruent_InverseEfficiency_E_post)
M1_InverseEfficiency.df <- data.frame(SubjectID, SAya_FL_Congruent_InverseEfficiency_Rest_E_pre, SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre, 
                                      SAya_FL_Congruent_InverseEfficiency_Rest_E_post, SAya_FL_Incongruent_InverseEfficiency_Rest_E_post, 
                                      SAya_FL_Congruent_InverseEfficiency_E_pre, SAya_FL_Incongruent_InverseEfficiency_E_pre, 
                                      SAya_FL_Congruent_InverseEfficiency_E_post, SAya_FL_Incongruent_InverseEfficiency_E_post)
longformat.M1_InverseEfficiency.df <- melt(M1_InverseEfficiency.df, id = "SubjectID", variable.name = "Condition")
summary(longformat.M1_InverseEfficiency.df)
#database needs to be formatted for RM ANOVA, add 3 columns and sort by Condition (2x2x2), resave and reupload
write_xlsx(longformat.M1_InverseEfficiency.df, '/Volumes/SAya_USB/Server_Dissertation//longformat.M1_InverseEfficiency.df.xlsx') #export long format df
longformat.M1_InverseEfficiency.df.edited <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/longformat.M1_InverseEfficiency.df.edited.xlsx") #reupload new df
View(longformat.M1_InverseEfficiency.df.edited) #check and see if new version 
#convert everything but value into factors
longformat.M1_InverseEfficiency.df.edited <- longformat.M1_InverseEfficiency.df.edited %>%
  convert_as_factor(SubjectID, Condition, Intervention, Timepoint, Congruency)
#summary stats
longformat.M1_InverseEfficiency.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  get_summary_stats(IES, type = "mean_sd")
#visualize
ggboxplot(longformat.M1_InverseEfficiency.df.edited, x = "Congruency", y = "IES", color = "Intervention", facet.by = "Timepoint", short.panel.labs = FALSE)
#check for outliers
longformat.M1_InverseEfficiency.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  identify_outliers(IES)
#check for normality
longformat.M1_InverseEfficiency.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  shapiro_test(IES) 
ggqqplot(longformat.M1_InverseEfficiency.df.edited, "IES", ggtheme = theme_bw()) +
  facet_grid(Intervention+Congruency ~ Timepoint, labeller = "label_both") #if points fall approx along ref line, normality is assumed
#computation 
M1_ACC.aov <- anova_test(data = longformat.M1_InverseEfficiency.df.edited, dv = IES, wid = SubjectID, within = c(Intervention, Timepoint, Congruency))
get_anova_table(M1_ACC.aov) #check for 3-way interaction, if none, check two way interactions with pairwise comparisons

#M2: Flanker Inverse Efficiency Trier vs. Rest 
SubjectID<-(df$SubjectID)
SAya_FL_Congruent_InverseEfficiency_Rest_T_pre<-(df$SAya_FL_Congruent_InverseEfficiency_Rest_T_pre)
SAya_FL_Incongruent_InverseEfficiency_Rest_T_pre<-(df$SAya_FL_Incongruent_InverseEfficiency_Rest_T_pre)
SAya_FL_Congruent_InverseEfficiency_Rest_T_post<-(df$SAya_FL_Congruent_InverseEfficiency_Rest_T_post)
SAya_FL_Incongruent_InverseEfficiency_Rest_T_post<-(df$SAya_FL_Incongruent_InverseEfficiency_Rest_T_post)
SAya_FL_Congruent_InverseEfficiency_T_pre<-(df$SAya_FL_Congruent_InverseEfficiency_T_pre)
SAya_FL_Incongruent_InverseEfficiency_T_pre<-(df$SAya_FL_Incongruent_InverseEfficiency_T_pre)
SAya_FL_Congruent_InverseEfficiency_T_post<-(df$SAya_FL_Congruent_InverseEfficiency_T_post)
SAya_FL_Incongruent_InverseEfficiency_T_post<-(df$SAya_FL_Incongruent_InverseEfficiency_T_post)
M2_InverseEfficiency.df <- data.frame(SubjectID, SAya_FL_Congruent_InverseEfficiency_Rest_T_pre, SAya_FL_Incongruent_InverseEfficiency_Rest_T_pre, 
                                      SAya_FL_Congruent_InverseEfficiency_Rest_T_post, SAya_FL_Incongruent_InverseEfficiency_Rest_T_post, 
                                      SAya_FL_Congruent_InverseEfficiency_T_pre, SAya_FL_Incongruent_InverseEfficiency_T_pre, 
                                      SAya_FL_Congruent_InverseEfficiency_T_post, SAya_FL_Incongruent_InverseEfficiency_T_post)
longformat.M2_InverseEfficiency.df <- melt(M2_InverseEfficiency.df, id = "SubjectID", variable.name = "Condition")
summary(longformat.M2_InverseEfficiency.df)
#database needs to be formatted for RM ANOVA, add 3 columns and sort by Condition (2x2x2), resave and reupload
write_xlsx(longformat.M2_InverseEfficiency.df, '/Volumes/SAya_USB/Server_Dissertation//longformat.M2_InverseEfficiency.df.xlsx') #export long format df
longformat.M2_InverseEfficiency.df.edited <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/longformat.M2_InverseEfficiency.df.edited.xlsx") #reupload new df
View(longformat.M2_InverseEfficiency.df.edited) #check and see if new version 
#convert everything but value into factors
longformat.M2_InverseEfficiency.df.edited <- longformat.M2_InverseEfficiency.df.edited %>%
  convert_as_factor(SubjectID, Condition, Intervention, Timepoint, Congruency)
#summary stats
longformat.M2_InverseEfficiency.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  get_summary_stats(IES, type = "mean_sd")
#visualize
ggboxplot(longformat.M2_InverseEfficiency.df.edited, x = "Congruency", y = "IES", color = "Intervention", facet.by = "Timepoint", short.panel.labs = FALSE)
#check for outliers
longformat.M2_InverseEfficiency.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  identify_outliers(IES)
#check for normality
longformat.M2_InverseEfficiency.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  shapiro_test(IES) 
ggqqplot(longformat.M2_InverseEfficiency.df.edited, "IES", ggtheme = theme_bw()) +
  facet_grid(Intervention+Congruency ~ Timepoint, labeller = "label_both") #if points fall approx along ref line, normality is assumed
#computation 
M2_ACC.aov <- anova_test(data = longformat.M2_InverseEfficiency.df.edited, dv = IES, wid = SubjectID, within = c(Intervention, Timepoint, Congruency))
get_anova_table(M2_ACC.aov) #check for 3-way interaction, if none, check two way interactions with pairwise comparisons

#M3: Flanker IES effect sizes M1 vs. M2 
#old P3 Amplitude P3
#create long format of averaged ROI sites dataframe
longformat.M1_P3.Amplitude.df<- melt(M1_P3.Amplitude.df , id = "SubjectID", variable.name = "Condition", variable.type = numeric)
longformat.M1_P3.Amplitude.df$value<-as.numeric(as.character(longformat.M1_P3.Amplitude.df$value))
#database needs to be formatted for RM ANOVA, add 3 columns and sort by Condition (2x2x1), resave and reupload
write_xlsx(longformat.M1_P3.Amplitude.df, '/Volumes/SAya_USB/Server_Dissertation//longformat.M1_P3.Amplitude.df.xlsx') #export long format df
longformat.M1_P3.Amplitude.df.edited <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/longformat.M1_P3.Amplitude.df.edited.xlsx") #reupload new df
View(longformat.M1_P3.Amplitude.df.edited) #check and see if new version 
#convert everything but value into factors
longformat.M1_P3.Amplitude.df.edited <- longformat.M1_P3.Amplitude.df.edited %>%
  convert_as_factor(SubjectID, Condition, Intervention, Timepoint, Congruency)
#summary stats
longformat.M1_P3.Amplitude.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  get_summary_stats(P3.Amplitude.uV, type = "mean_sd")
#visualize
ggboxplot(longformat.M1_P3.Amplitude.df.edited, x = "Timepoint", y = "P3.Amplitude.uV",
          color = "Congruency", facet.by = "Intervention", short.panel.labs = FALSE)
#check for outliers
longformat.M1_P3.Amplitude.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  identify_outliers(P3.Amplitude.uV)
#check for normality
longformat.M1_P3.Amplitude.df.edited %>%
  group_by(Intervention, Timepoint, Congruency) %>%
  shapiro_test(P3.Amplitude.uV) 
ggqqplot(longformat.M1_P3.Amplitude.df.edited, "P3.Amplitude.uV", ggtheme = theme_bw()) +
  facet_grid(Intervention+Congruency ~ Timepoint, labeller = "label_both") #if points fall approx along ref line, normality is assumed
#computation 
M1_P3.Amplitude.aov <- anova_test(data = longformat.M1_P3.Amplitude.df.edited, dv = P3.Amplitude.uV, wid = SubjectID, within = c(Intervention, Timepoint, Congruency), type = 3)
get_anova_table(M1_P3.Amplitude.aov) #check for 3-way interaction, if none, check two way interactions with pairwise comparisons
#Investigate main effects
t.test(longformat.M1_P3.Amplitude.df.edited$P3.Amplitude.uV~longformat.M1_P3.Amplitude.df.edited$Congruency, paired = TRUE)
#Visualize 
Main_Effect<-longformat.M1_P3.Amplitude.df.edited %>%
  group_by(Congruency) %>%
  get_summary_stats(P3.Amplitude.uV, type = "mean_sd")
Main_Effect %>%
  ggplot(aes(x=Congruency, y = mean))+
  geom_point(size = 5)

# Visualize P3 Data -------------------------------------------------------
#Visualize P3 Data
#Import Data
SAya_pr_Flanker_Rest_E_Congruent <- read_excel("/Volumes/Data/ZShared/SNEACY/2_SNEACY_YA/SNEACY_YA_EEG/Flanker_EEG/Flanker_EEG_Rest_E/Pre/Export_Excel/GrandAVG/SAya_pr_Flanker_Rest_E_Congruent.xlsm")
SAya_pr_Flanker_Rest_E_Incongruent <- read_excel("/Volumes/Data/ZShared/SNEACY/2_SNEACY_YA/SNEACY_YA_EEG/Flanker_EEG/Flanker_EEG_Rest_E/Pre/Export_Excel/GrandAVG/SAya_pr_Flanker_Rest_E_Incongruent.xlsm")
SAya_pr_Flanker_Rest_T_Congruent <- read_excel("/Volumes/Data/ZShared/SNEACY/2_SNEACY_YA/SNEACY_YA_EEG/Flanker_EEG/Flanker_EEG_Rest_T/Pre/Export_Excel/GrandAVG/SAya_pr_Flanker_Rest_T_Congruent.xlsm")
SAya_pr_Flanker_Rest_T_Incongruent <- read_excel("/Volumes/Data/ZShared/SNEACY/2_SNEACY_YA/SNEACY_YA_EEG/Flanker_EEG/Flanker_EEG_Rest_T/Pre/Export_Excel/GrandAVG/SAya_pr_Flanker_Rest_T_Incongruent.xlsm")
SAya_pr_Flanker_E_Congruent <- read_excel("/Volumes/Data/ZShared/SNEACY/2_SNEACY_YA/SNEACY_YA_EEG/Flanker_EEG/Flanker_EEG_Exercise/Pre/Export_Excel/GrandAVG/SAya_pr_Flanker_E_Congruent.xlsm")
SAya_pr_Flanker_E_Incongruent <- read_excel("/Volumes/Data/ZShared/SNEACY/2_SNEACY_YA/SNEACY_YA_EEG/Flanker_EEG/Flanker_EEG_Exercise/Pre/Export_Excel/GrandAVG/SAya_pr_Flanker_E_Incongruent.xlsm")
SAya_pr_Flanker_T_Congruent <- read_excel("/Volumes/Data/ZShared/SNEACY/2_SNEACY_YA/SNEACY_YA_EEG/Flanker_EEG/Flanker_EEG_Trier/Pre/Export_Excel/GrandAVG/SAya_pr_Flanker_T_Congruent.xlsm")
SAya_pr_Flanker_T_Incongruent <- read_excel("/Volumes/Data/ZShared/SNEACY/2_SNEACY_YA/SNEACY_YA_EEG/Flanker_EEG/Flanker_EEG_Trier/Pre/Export_Excel/GrandAVG/SAya_pr_Flanker_T_Incongruent.xlsm")
SAya_po_Flanker_Rest_E_Congruent <- read_excel("/Volumes/Data/ZShared/SNEACY/2_SNEACY_YA/SNEACY_YA_EEG/Flanker_EEG/Flanker_EEG_Rest_E/Post/Export_Excel/GrandAVG/SAya_po_Flanker_Rest_E_Congruent.xlsm")
SAya_po_Flanker_Rest_E_Incongruent <- read_excel("/Volumes/Data/ZShared/SNEACY/2_SNEACY_YA/SNEACY_YA_EEG/Flanker_EEG/Flanker_EEG_Rest_E/Post/Export_Excel/GrandAVG/SAya_po_Flanker_Rest_E_Incongruent.xlsm")
SAya_po_Flanker_Rest_T_Congruent <- read_excel("/Volumes/Data/ZShared/SNEACY/2_SNEACY_YA/SNEACY_YA_EEG/Flanker_EEG/Flanker_EEG_Rest_T/Post/Export_Excel/GrandAVG/SAya_po_Flanker_Rest_T_Congruent.xlsm")
SAya_po_Flanker_Rest_T_Incongruent <- read_excel("/Volumes/Data/ZShared/SNEACY/2_SNEACY_YA/SNEACY_YA_EEG/Flanker_EEG/Flanker_EEG_Rest_T/Post/Export_Excel/GrandAVG/SAya_po_Flanker_Rest_T_Incongruent.xlsm")
SAya_po_Flanker_E_Congruent <- read_excel("/Volumes/Data/ZShared/SNEACY/2_SNEACY_YA/SNEACY_YA_EEG/Flanker_EEG/Flanker_EEG_Exercise/Post/Export_Excel/GrandAVG/SAya_po_Flanker_E_Congruent.xlsm")
SAya_po_Flanker_E_Incongruent <- read_excel("/Volumes/Data/ZShared/SNEACY/2_SNEACY_YA/SNEACY_YA_EEG/Flanker_EEG/Flanker_EEG_Exercise/Post/Export_Excel/GrandAVG/SAya_po_Flanker_E_Incongruent.xlsm")
SAya_po_Flanker_T_Congruent <- read_excel("/Volumes/Data/ZShared/SNEACY/2_SNEACY_YA/SNEACY_YA_EEG/Flanker_EEG/Flanker_EEG_Trier/Post/Export_Excel/GrandAVG/SAya_po_Flanker_T_Congruent.xlsm")
SAya_po_Flanker_T_Incongruent <- read_excel("/Volumes/Data/ZShared/SNEACY/2_SNEACY_YA/SNEACY_YA_EEG/Flanker_EEG/Flanker_EEG_Trier/Post/Export_Excel/GrandAVG/SAya_po_Flanker_T_Incongruent.xlsm")

#Call Variables
time <- (SAya_pr_Flanker_Rest_E_Congruent$time)
FPZ_pr_Rest_E_Congruent <- (SAya_pr_Flanker_Rest_E_Congruent$FPZ)
FZ_pr_Rest_E_Congruent <- (SAya_pr_Flanker_Rest_E_Congruent$FZ)
FCZ_pr_Rest_E_Congruent <- (SAya_pr_Flanker_Rest_E_Congruent$FCZ)
CZ_pr_Rest_E_Congruent <- (SAya_pr_Flanker_Rest_E_Congruent$CZ)
CPZ_pr_Rest_E_Congruent <- (SAya_pr_Flanker_Rest_E_Congruent$CPZ)
PZ_pr_Rest_E_Congruent <- (SAya_pr_Flanker_Rest_E_Congruent$PZ)
POZ_pr_Rest_E_Congruent<- (SAya_pr_Flanker_Rest_E_Congruent$POZ)
FPZ_pr_Rest_E_Incongruent <- (SAya_pr_Flanker_Rest_E_Incongruent$FPZ)
FZ_pr_Rest_E_Incongruent <- (SAya_pr_Flanker_Rest_E_Incongruent$FZ)
FCZ_pr_Rest_E_Incongruent <- (SAya_pr_Flanker_Rest_E_Incongruent$FCZ)
CZ_pr_Rest_E_Incongruent <- (SAya_pr_Flanker_Rest_E_Incongruent$CZ)
CPZ_pr_Rest_E_Incongruent <- (SAya_pr_Flanker_Rest_E_Incongruent$CPZ)
PZ_pr_Rest_E_Incongruent <- (SAya_pr_Flanker_Rest_E_Incongruent$PZ)
POZ_pr_Rest_E_Incongruent<- (SAya_pr_Flanker_Rest_E_Incongruent$POZ)
FPZ_pr_Rest_T_Congruent <- (SAya_pr_Flanker_Rest_T_Congruent$FPZ)
FZ_pr_Rest_T_Congruent <- (SAya_pr_Flanker_Rest_T_Congruent$FZ)
FCZ_pr_Rest_T_Congruent <- (SAya_pr_Flanker_Rest_T_Congruent$FCZ)
CZ_pr_Rest_T_Congruent <- (SAya_pr_Flanker_Rest_T_Congruent$CZ)
CPZ_pr_Rest_T_Congruent <- (SAya_pr_Flanker_Rest_T_Congruent$CPZ)
PZ_pr_Rest_T_Congruent <- (SAya_pr_Flanker_Rest_T_Congruent$PZ)
POZ_pr_Rest_T_Congruent<- (SAya_pr_Flanker_Rest_T_Congruent$POZ)
FPZ_pr_Rest_T_Incongruent <- (SAya_pr_Flanker_Rest_T_Incongruent$FPZ)
FZ_pr_Rest_T_Incongruent <- (SAya_pr_Flanker_Rest_T_Incongruent$FZ)
FCZ_pr_Rest_T_Incongruent <- (SAya_pr_Flanker_Rest_T_Incongruent$FCZ)
CZ_pr_Rest_T_Incongruent <- (SAya_pr_Flanker_Rest_T_Incongruent$CZ)
CPZ_pr_Rest_T_Incongruent <- (SAya_pr_Flanker_Rest_T_Incongruent$CPZ)
PZ_pr_Rest_T_Incongruent <- (SAya_pr_Flanker_Rest_T_Incongruent$PZ)
POZ_pr_Rest_T_Incongruent<- (SAya_pr_Flanker_Rest_T_Incongruent$POZ)
FPZ_pr_E_Congruent <- (SAya_pr_Flanker_E_Congruent$FPZ)
FZ_pr_E_Congruent <- (SAya_pr_Flanker_E_Congruent$FZ)
FCZ_pr_E_Congruent <- (SAya_pr_Flanker_E_Congruent$FCZ)
CZ_pr_E_Congruent <- (SAya_pr_Flanker_E_Congruent$CZ)
CPZ_pr_E_Congruent <- (SAya_pr_Flanker_E_Congruent$CPZ)
PZ_pr_E_Congruent <- (SAya_pr_Flanker_E_Congruent$PZ)
POZ_pr_E_Congruent<- (SAya_pr_Flanker_E_Congruent$POZ)
FPZ_pr_E_Incongruent <- (SAya_pr_Flanker_E_Incongruent$FPZ)
FZ_pr_E_Incongruent <- (SAya_pr_Flanker_E_Incongruent$FZ)
FCZ_pr_E_Incongruent <- (SAya_pr_Flanker_E_Incongruent$FCZ)
CZ_pr_E_Incongruent <- (SAya_pr_Flanker_E_Incongruent$CZ)
CPZ_pr_E_Incongruent <- (SAya_pr_Flanker_E_Incongruent$CPZ)
PZ_pr_E_Incongruent <- (SAya_pr_Flanker_E_Incongruent$PZ)
POZ_pr_E_Incongruent<- (SAya_pr_Flanker_E_Incongruent$POZ)
FPZ_pr_T_Congruent <- (SAya_pr_Flanker_T_Congruent$FPZ)
FZ_pr_T_Congruent <- (SAya_pr_Flanker_T_Congruent$FZ)
FCZ_pr_T_Congruent <- (SAya_pr_Flanker_T_Congruent$FCZ)
CZ_pr_T_Congruent <- (SAya_pr_Flanker_T_Congruent$CZ)
CPZ_pr_T_Congruent <- (SAya_pr_Flanker_T_Congruent$CPZ)
PZ_pr_T_Congruent <- (SAya_pr_Flanker_T_Congruent$PZ)
POZ_pr_T_Congruent<- (SAya_pr_Flanker_T_Congruent$POZ)
FPZ_pr_T_Incongruent <- (SAya_pr_Flanker_T_Incongruent$FPZ)
FZ_pr_T_Incongruent <- (SAya_pr_Flanker_T_Incongruent$FZ)
FCZ_pr_T_Incongruent <- (SAya_pr_Flanker_T_Incongruent$FCZ)
CZ_pr_T_Incongruent <- (SAya_pr_Flanker_T_Incongruent$CZ)
CPZ_pr_T_Incongruent <- (SAya_pr_Flanker_T_Incongruent$CPZ)
PZ_pr_T_Incongruent <- (SAya_pr_Flanker_T_Incongruent$PZ)
POZ_pr_T_Incongruent<- (SAya_pr_Flanker_T_Incongruent$POZ)
FPZ_po_Rest_E_Congruent <- (SAya_po_Flanker_Rest_E_Congruent$FPZ)
FZ_po_Rest_E_Congruent <- (SAya_po_Flanker_Rest_E_Congruent$FZ)
FCZ_po_Rest_E_Congruent <- (SAya_po_Flanker_Rest_E_Congruent$FCZ)
CZ_po_Rest_E_Congruent <- (SAya_po_Flanker_Rest_E_Congruent$CZ)
CPZ_po_Rest_E_Congruent <- (SAya_po_Flanker_Rest_E_Congruent$CPZ)
PZ_po_Rest_E_Congruent <- (SAya_po_Flanker_Rest_E_Congruent$PZ)
POZ_po_Rest_E_Congruent<- (SAya_po_Flanker_Rest_E_Congruent$POZ)
FPZ_po_Rest_E_Incongruent <- (SAya_po_Flanker_Rest_E_Incongruent$FPZ)
FZ_po_Rest_E_Incongruent <- (SAya_po_Flanker_Rest_E_Incongruent$FZ)
FCZ_po_Rest_E_Incongruent <- (SAya_po_Flanker_Rest_E_Incongruent$FCZ)
CZ_po_Rest_E_Incongruent <- (SAya_po_Flanker_Rest_E_Incongruent$CZ)
CPZ_po_Rest_E_Incongruent <- (SAya_po_Flanker_Rest_E_Incongruent$CPZ)
PZ_po_Rest_E_Incongruent <- (SAya_po_Flanker_Rest_E_Incongruent$PZ)
POZ_po_Rest_E_Incongruent<- (SAya_po_Flanker_Rest_E_Incongruent$POZ)
FPZ_po_Rest_T_Congruent <- (SAya_po_Flanker_Rest_T_Congruent$FPZ)
FZ_po_Rest_T_Congruent <- (SAya_po_Flanker_Rest_T_Congruent$FZ)
FCZ_po_Rest_T_Congruent <- (SAya_po_Flanker_Rest_T_Congruent$FCZ)
CZ_po_Rest_T_Congruent <- (SAya_po_Flanker_Rest_T_Congruent$CZ)
CPZ_po_Rest_T_Congruent <- (SAya_po_Flanker_Rest_T_Congruent$CPZ)
PZ_po_Rest_T_Congruent <- (SAya_po_Flanker_Rest_T_Congruent$PZ)
POZ_po_Rest_T_Congruent<- (SAya_po_Flanker_Rest_T_Congruent$POZ)
FPZ_po_Rest_T_Incongruent <- (SAya_po_Flanker_Rest_T_Incongruent$FPZ)
FZ_po_Rest_T_Incongruent <- (SAya_po_Flanker_Rest_T_Incongruent$FZ)
FCZ_po_Rest_T_Incongruent <- (SAya_po_Flanker_Rest_T_Incongruent$FCZ)
CZ_po_Rest_T_Incongruent <- (SAya_po_Flanker_Rest_T_Incongruent$CZ)
CPZ_po_Rest_T_Incongruent <- (SAya_po_Flanker_Rest_T_Incongruent$CPZ)
PZ_po_Rest_T_Incongruent <- (SAya_po_Flanker_Rest_T_Incongruent$PZ)
POZ_po_Rest_T_Incongruent<- (SAya_po_Flanker_Rest_T_Incongruent$POZ)
FPZ_po_E_Congruent <- (SAya_po_Flanker_E_Congruent$FPZ)
FZ_po_E_Congruent <- (SAya_po_Flanker_E_Congruent$FZ)
FCZ_po_E_Congruent <- (SAya_po_Flanker_E_Congruent$FCZ)
CZ_po_E_Congruent <- (SAya_po_Flanker_E_Congruent$CZ)
CPZ_po_E_Congruent <- (SAya_po_Flanker_E_Congruent$CPZ)
PZ_po_E_Congruent <- (SAya_po_Flanker_E_Congruent$PZ)
POZ_po_E_Congruent<- (SAya_po_Flanker_E_Congruent$POZ)
FPZ_po_E_Incongruent <- (SAya_po_Flanker_E_Incongruent$FPZ)
FZ_po_E_Incongruent <- (SAya_po_Flanker_E_Incongruent$FZ)
FCZ_po_E_Incongruent <- (SAya_po_Flanker_E_Incongruent$FCZ)
CZ_po_E_Incongruent <- (SAya_po_Flanker_E_Incongruent$CZ)
CPZ_po_E_Incongruent <- (SAya_po_Flanker_E_Incongruent$CPZ)
PZ_po_E_Incongruent <- (SAya_po_Flanker_E_Incongruent$PZ)
POZ_po_E_Incongruent<- (SAya_po_Flanker_E_Incongruent$POZ)
FPZ_po_T_Congruent <- (SAya_po_Flanker_T_Congruent$FPZ)
FZ_po_T_Congruent <- (SAya_po_Flanker_T_Congruent$FZ)
FCZ_po_T_Congruent <- (SAya_po_Flanker_T_Congruent$FCZ)
CZ_po_T_Congruent <- (SAya_po_Flanker_T_Congruent$CZ)
CPZ_po_T_Congruent <- (SAya_po_Flanker_T_Congruent$CPZ)
PZ_po_T_Congruent <- (SAya_po_Flanker_T_Congruent$PZ)
POZ_po_T_Congruent<- (SAya_po_Flanker_T_Congruent$POZ)
FPZ_po_T_Incongruent <- (SAya_po_Flanker_T_Incongruent$FPZ)
FZ_po_T_Incongruent <- (SAya_po_Flanker_T_Incongruent$FZ)
FCZ_po_T_Incongruent <- (SAya_po_Flanker_T_Incongruent$FCZ)
CZ_po_T_Incongruent <- (SAya_po_Flanker_T_Incongruent$CZ)
CPZ_po_T_Incongruent <- (SAya_po_Flanker_T_Incongruent$CPZ)
PZ_po_T_Incongruent <- (SAya_po_Flanker_T_Incongruent$PZ)
POZ_po_T_Incongruent<- (SAya_po_Flanker_T_Incongruent$POZ)

#additional ROI Variables of Interest
CP1_pr_E_Congruent <- (SAya_pr_Flanker_E_Congruent$CP1)
CP2_pr_E_Congruent <- (SAya_pr_Flanker_E_Congruent$CP2)
P1_pr_E_Congruent <- (SAya_pr_Flanker_E_Congruent$P1)
P2_pr_E_Congruent <- (SAya_pr_Flanker_E_Congruent$P2)
C1_pr_E_Congruent <- (SAya_pr_Flanker_E_Congruent$C1)
C2_pr_E_Congruent <- (SAya_pr_Flanker_E_Congruent$C2)
CP1_pr_E_Incongruent <- (SAya_pr_Flanker_E_Incongruent$CP1)
CP2_pr_E_Incongruent <- (SAya_pr_Flanker_E_Incongruent$CP2)
P1_pr_E_Incongruent <- (SAya_pr_Flanker_E_Incongruent$P1)
P2_pr_E_Incongruent <- (SAya_pr_Flanker_E_Incongruent$P2)
C1_pr_E_Incongruent <- (SAya_pr_Flanker_E_Incongruent$C1)
C2_pr_E_Incongruent <- (SAya_pr_Flanker_E_Incongruent$C2)
CP1_po_E_Congruent <- (SAya_po_Flanker_E_Congruent$CP1)
CP2_po_E_Congruent <- (SAya_po_Flanker_E_Congruent$CP2)
P1_po_E_Congruent <- (SAya_po_Flanker_E_Congruent$P1)
P2_po_E_Congruent <- (SAya_po_Flanker_E_Congruent$P2)
C1_po_E_Congruent <- (SAya_po_Flanker_E_Congruent$C1)
C2_po_E_Congruent <- (SAya_po_Flanker_E_Congruent$C2)
CP1_po_E_Incongruent <- (SAya_po_Flanker_E_Incongruent$CP1)
CP2_po_E_Incongruent <- (SAya_po_Flanker_E_Incongruent$CP2)
P1_po_E_Incongruent <- (SAya_po_Flanker_E_Incongruent$P1)
P2_po_E_Incongruent <- (SAya_po_Flanker_E_Incongruent$P2)
C1_po_E_Incongruent <- (SAya_po_Flanker_E_Incongruent$C1)
C2_po_E_Incongruent <- (SAya_po_Flanker_E_Incongruent$C2)
CP1_pr_Rest_E_Congruent <- (SAya_pr_Flanker_Rest_E_Congruent$CP1)
CP2_pr_Rest_E_Congruent <- (SAya_pr_Flanker_Rest_E_Congruent$CP2)
P1_pr_Rest_E_Congruent <- (SAya_pr_Flanker_Rest_E_Congruent$P1)
P2_pr_Rest_E_Congruent <- (SAya_pr_Flanker_Rest_E_Congruent$P2)
C1_pr_Rest_E_Congruent <- (SAya_pr_Flanker_Rest_E_Congruent$C1)
C2_pr_Rest_E_Congruent <- (SAya_pr_Flanker_Rest_E_Congruent$C2)
CP1_pr_Rest_E_Incongruent <- (SAya_pr_Flanker_Rest_E_Incongruent$CP1)
CP2_pr_Rest_E_Incongruent <- (SAya_pr_Flanker_Rest_E_Incongruent$CP2)
P1_pr_Rest_E_Incongruent <- (SAya_pr_Flanker_Rest_E_Incongruent$P1)
P2_pr_Rest_E_Incongruent <- (SAya_pr_Flanker_Rest_E_Incongruent$P2)
C1_pr_Rest_E_Incongruent <- (SAya_pr_Flanker_Rest_E_Incongruent$C1)
C2_pr_Rest_E_Incongruent <- (SAya_pr_Flanker_Rest_E_Incongruent$C2)
CP1_po_Rest_E_Congruent <- (SAya_po_Flanker_Rest_E_Congruent$CP1)
CP2_po_Rest_E_Congruent <- (SAya_po_Flanker_Rest_E_Congruent$CP2)
P1_po_Rest_E_Congruent <- (SAya_po_Flanker_Rest_E_Congruent$P1)
P2_po_Rest_E_Congruent <- (SAya_po_Flanker_Rest_E_Congruent$P2)
C1_po_Rest_E_Congruent <- (SAya_po_Flanker_Rest_E_Congruent$C1)
C2_po_Rest_E_Congruent <- (SAya_po_Flanker_Rest_E_Congruent$C2)
CP1_po_Rest_E_Incongruent <- (SAya_po_Flanker_Rest_E_Incongruent$CP1)
CP2_po_Rest_E_Incongruent <- (SAya_po_Flanker_Rest_E_Incongruent$CP2)
P1_po_Rest_E_Incongruent <- (SAya_po_Flanker_Rest_E_Incongruent$P1)
P2_po_Rest_E_Incongruent <- (SAya_po_Flanker_Rest_E_Incongruent$P2)
C1_po_Rest_E_Incongruent <- (SAya_po_Flanker_Rest_E_Incongruent$C1)
C2_po_Rest_E_Incongruent <- (SAya_po_Flanker_Rest_E_Incongruent$C2)
CP1_pr_T_Congruent <- (SAya_pr_Flanker_T_Congruent$CP1)
CP2_pr_T_Congruent <- (SAya_pr_Flanker_T_Congruent$CP2)
P1_pr_T_Congruent <- (SAya_pr_Flanker_T_Congruent$P1)
P2_pr_T_Congruent <- (SAya_pr_Flanker_T_Congruent$P2)
C1_pr_T_Congruent <- (SAya_pr_Flanker_T_Congruent$C1)
C2_pr_T_Congruent <- (SAya_pr_Flanker_T_Congruent$C2)
CP1_pr_T_Incongruent <- (SAya_pr_Flanker_T_Incongruent$CP1)
CP2_pr_T_Incongruent <- (SAya_pr_Flanker_T_Incongruent$CP2)
P1_pr_T_Incongruent <- (SAya_pr_Flanker_T_Incongruent$P1)
P2_pr_T_Incongruent <- (SAya_pr_Flanker_T_Incongruent$P2)
C1_pr_T_Incongruent <- (SAya_pr_Flanker_T_Incongruent$C1)
C2_pr_T_Incongruent <- (SAya_pr_Flanker_T_Incongruent$C2)
CP1_po_T_Congruent <- (SAya_po_Flanker_T_Congruent$CP1)
CP2_po_T_Congruent <- (SAya_po_Flanker_T_Congruent$CP2)
P1_po_T_Congruent <- (SAya_po_Flanker_T_Congruent$P1)
P2_po_T_Congruent <- (SAya_po_Flanker_T_Congruent$P2)
C1_po_T_Congruent <- (SAya_po_Flanker_T_Congruent$C1)
C2_po_T_Congruent <- (SAya_po_Flanker_T_Congruent$C2)
CP1_po_T_Incongruent <- (SAya_po_Flanker_T_Incongruent$CP1)
CP2_po_T_Incongruent <- (SAya_po_Flanker_T_Incongruent$CP2)
P1_po_T_Incongruent <- (SAya_po_Flanker_T_Incongruent$P1)
P2_po_T_Incongruent <- (SAya_po_Flanker_T_Incongruent$P2)
C1_po_T_Incongruent <- (SAya_po_Flanker_T_Incongruent$C1)
C2_po_T_Incongruent <- (SAya_po_Flanker_T_Incongruent$C2)
CP1_pr_Rest_T_Congruent <- (SAya_pr_Flanker_Rest_T_Congruent$CP1)
CP2_pr_Rest_T_Congruent <- (SAya_pr_Flanker_Rest_T_Congruent$CP2)
P1_pr_Rest_T_Congruent <- (SAya_pr_Flanker_Rest_T_Congruent$P1)
P2_pr_Rest_T_Congruent <- (SAya_pr_Flanker_Rest_T_Congruent$P2)
C1_pr_Rest_T_Congruent <- (SAya_pr_Flanker_Rest_T_Congruent$C1)
C2_pr_Rest_T_Congruent <- (SAya_pr_Flanker_Rest_T_Congruent$C2)
CP1_pr_Rest_T_Incongruent <- (SAya_pr_Flanker_Rest_T_Incongruent$CP1)
CP2_pr_Rest_T_Incongruent <- (SAya_pr_Flanker_Rest_T_Incongruent$CP2)
P1_pr_Rest_T_Incongruent <- (SAya_pr_Flanker_Rest_T_Incongruent$P1)
P2_pr_Rest_T_Incongruent <- (SAya_pr_Flanker_Rest_T_Incongruent$P2)
C1_pr_Rest_T_Incongruent <- (SAya_pr_Flanker_Rest_T_Incongruent$C1)
C2_pr_Rest_T_Incongruent <- (SAya_pr_Flanker_Rest_T_Incongruent$C2)
CP1_po_Rest_T_Congruent <- (SAya_po_Flanker_Rest_T_Congruent$CP1)
CP2_po_Rest_T_Congruent <- (SAya_po_Flanker_Rest_T_Congruent$CP2)
P1_po_Rest_T_Congruent <- (SAya_po_Flanker_Rest_T_Congruent$P1)
P2_po_Rest_T_Congruent <- (SAya_po_Flanker_Rest_T_Congruent$P2)
C1_po_Rest_T_Congruent <- (SAya_po_Flanker_Rest_T_Congruent$C1)
C2_po_Rest_T_Congruent <- (SAya_po_Flanker_Rest_T_Congruent$C2)
CP1_po_Rest_T_Incongruent <- (SAya_po_Flanker_Rest_T_Incongruent$CP1)
CP2_po_Rest_T_Incongruent <- (SAya_po_Flanker_Rest_T_Incongruent$CP2)
P1_po_Rest_T_Incongruent <- (SAya_po_Flanker_Rest_T_Incongruent$P1)
P2_po_Rest_T_Incongruent <- (SAya_po_Flanker_Rest_T_Incongruent$P2)
C1_po_Rest_T_Incongruent <- (SAya_po_Flanker_Rest_T_Incongruent$C1)
C2_po_Rest_T_Incongruent <- (SAya_po_Flanker_Rest_T_Incongruent$C2)


#Model 1 NARROW ROI ERP (Exercise vs. Rest_E) CPZ, CP1, CP2
ROI_pr_E_Congruent.mean <-data.frame(CPZ_pr_E_Congruent, CP1_pr_E_Congruent, CP2_pr_E_Congruent)
ROI_pr_E_Congruent.mean <- rowMeans(ROI_pr_E_Congruent.mean,c(CPZ_pr_E_Congruent, CP1_pr_E_Congruent, CP2_pr_E_Congruent))
ROI_pr_E_Incongruent.mean<-data.frame(CPZ_pr_E_Incongruent, CP1_pr_E_Incongruent, CP2_pr_E_Incongruent)
ROI_pr_E_Incongruent.mean <- rowMeans(ROI_pr_E_Incongruent.mean,c(CPZ_pr_E_Incongruent, CP1_pr_E_Incongruent, CP2_pr_E_Incongruent))
ROI_po_E_Congruent.mean <- data.frame(CPZ_po_E_Congruent, CP1_po_E_Congruent, CP2_po_E_Congruent)
ROI_po_E_Congruent.mean <- rowMeans(ROI_po_E_Congruent.mean,c(CPZ_po_E_Congruent, CP1_po_E_Congruent, CP2_po_E_Congruent))
ROI_po_E_Incongruent.mean <- data.frame(CPZ_po_E_Incongruent, CP1_po_E_Incongruent, CP2_po_E_Incongruent)
ROI_po_E_Incongruent.mean <- rowMeans(ROI_po_E_Incongruent.mean,c(CPZ_po_E_Incongruent, CP1_po_E_Incongruent, CP2_po_E_Incongruent))
ROI_pr_Rest_E_Congruent.mean <-data.frame(CPZ_pr_Rest_E_Congruent, CP1_pr_Rest_E_Congruent, CP2_pr_Rest_E_Congruent)
ROI_pr_Rest_E_Congruent.mean <- rowMeans(ROI_pr_Rest_E_Congruent.mean,c(CPZ_pr_Rest_E_Congruent, CP1_pr_Rest_E_Congruent, CP2_pr_Rest_E_Congruent))
ROI_pr_Rest_E_Incongruent.mean <- data.frame(CPZ_pr_Rest_E_Incongruent, CP1_pr_Rest_E_Incongruent, CP2_pr_Rest_E_Incongruent)
ROI_pr_Rest_E_Incongruent.mean <- rowMeans(ROI_pr_Rest_E_Incongruent.mean,c(CPZ_pr_Rest_E_Incongruent, CP1_pr_Rest_E_Incongruent, CP2_pr_Rest_E_Incongruent))
ROI_po_Rest_E_Congruent.mean <- data.frame(CPZ_po_Rest_E_Congruent, CP1_po_Rest_E_Congruent, CP2_po_Rest_E_Congruent)
ROI_po_Rest_E_Congruent.mean <- rowMeans(ROI_po_Rest_E_Congruent.mean,c(CPZ_po_Rest_E_Congruent, CP1_po_Rest_E_Congruent, CP2_po_Rest_E_Congruent))
ROI_po_Rest_E_Incongruent.mean <- data.frame(CPZ_po_Rest_E_Incongruent, CP1_po_Rest_E_Incongruent, CP2_po_Rest_E_Incongruent)
ROI_po_Rest_E_Incongruent.mean <- rowMeans(ROI_po_Rest_E_Incongruent.mean,c(CPZ_po_Rest_E_Incongruent, CP1_po_Rest_E_Incongruent, CP2_po_Rest_E_Incongruent))

ROI.Narrow.df <- data.frame(time, ROI_pr_E_Congruent.mean, ROI_po_E_Congruent.mean, ROI_pr_E_Incongruent.mean, ROI_po_E_Incongruent.mean,  
                            ROI_pr_Rest_E_Congruent.mean, ROI_po_Rest_E_Congruent.mean, ROI_pr_Rest_E_Incongruent.mean, ROI_po_Rest_E_Incongruent.mean)
longformat.ROI.Narrow.df<- melt(ROI.Narrow.df , id = "time", variable.name = "Condition", variable.type = numeric)
longformat.ROI.Narrow.df$value<-as.numeric(as.character(longformat.ROI.Narrow.df$value))

ggplot(longformat.ROI.Narrow.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(13, -5)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 1 Exercise vs. Rest Flanker ERP using Narrow ROI ERP (CPZ, CP1, CP2 average)")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")



#Model 1 WIDE ROI ERP (Exercise vs. Rest_E) CPZ, CP1, CP2, P1, PZ, P2, C1, CZ, C2
ROI_pr_E_Congruent.mean <-data.frame(CPZ_pr_E_Congruent, CP1_pr_E_Congruent, CP2_pr_E_Congruent, P1_pr_E_Congruent, PZ_pr_E_Congruent, P2_pr_E_Congruent, C1_pr_E_Congruent, CZ_pr_E_Congruent, C2_pr_E_Congruent)
ROI_pr_E_Congruent.mean <- rowMeans(ROI_pr_E_Congruent.mean,c(CPZ_pr_E_Congruent, CP1_pr_E_Congruent, CP2_pr_E_Congruent, P1_pr_E_Congruent, PZ_pr_E_Congruent, P2_pr_E_Congruent, C1_pr_E_Congruent, CZ_pr_E_Congruent, C2_pr_E_Congruent))
ROI_pr_E_Incongruent.mean<-data.frame(CPZ_pr_E_Incongruent, CP1_pr_E_Incongruent, CP2_pr_E_Incongruent, P1_pr_E_Incongruent, PZ_pr_E_Incongruent, P2_pr_E_Incongruent, C1_pr_E_Incongruent, CZ_pr_E_Incongruent, C2_pr_E_Incongruent)
ROI_pr_E_Incongruent.mean <- rowMeans(ROI_pr_E_Incongruent.mean,c(CPZ_pr_E_Incongruent, CP1_pr_E_Incongruent, CP2_pr_E_Incongruent, P1_pr_E_Incongruent, PZ_pr_E_Incongruent, P2_pr_E_Incongruent, C1_pr_E_Incongruent, CZ_pr_E_Incongruent, C2_pr_E_Incongruent))
ROI_po_E_Congruent.mean <- data.frame(CPZ_po_E_Congruent, CP1_po_E_Congruent, CP2_po_E_Congruent, P1_po_E_Congruent, PZ_po_E_Congruent, P2_po_E_Congruent, C1_po_E_Congruent, CZ_po_E_Congruent, C2_po_E_Congruent)
ROI_po_E_Congruent.mean <- rowMeans(ROI_po_E_Congruent.mean,c(CPZ_po_E_Congruent, CP1_po_E_Congruent, CP2_po_E_Congruent, P1_po_E_Congruent, PZ_po_E_Congruent, P2_po_E_Congruent, C1_po_E_Congruent, CZ_po_E_Congruent, C2_po_E_Congruent))
ROI_po_E_Incongruent.mean <- data.frame(CPZ_po_E_Incongruent, CP1_po_E_Incongruent, CP2_po_E_Incongruent, P1_po_E_Incongruent, PZ_po_E_Incongruent, P2_po_E_Incongruent, C1_po_E_Incongruent, CZ_po_E_Incongruent, C2_po_E_Incongruent)
ROI_po_E_Incongruent.mean <- rowMeans(ROI_po_E_Incongruent.mean,c(CPZ_po_E_Incongruent, CP1_po_E_Incongruent, CP2_po_E_Incongruent, P1_po_E_Incongruent, PZ_po_E_Incongruent, P2_po_E_Incongruent, C1_po_E_Incongruent, CZ_po_E_Incongruent, C2_po_E_Incongruent))
ROI_pr_Rest_E_Congruent.mean <-data.frame(CPZ_pr_Rest_E_Congruent, CP1_pr_Rest_E_Congruent, CP2_pr_Rest_E_Congruent, P1_pr_Rest_E_Congruent, PZ_pr_Rest_E_Congruent, P2_pr_Rest_E_Congruent, C1_pr_Rest_E_Congruent, CZ_pr_Rest_E_Congruent, C2_pr_Rest_E_Congruent)
ROI_pr_Rest_E_Congruent.mean <- rowMeans(ROI_pr_Rest_E_Congruent.mean,c(CPZ_pr_Rest_E_Congruent, CP1_pr_Rest_E_Congruent, CP2_pr_Rest_E_Congruent, P1_pr_Rest_E_Congruent, PZ_pr_Rest_E_Congruent, P2_pr_Rest_E_Congruent, C1_pr_Rest_E_Congruent, CZ_pr_Rest_E_Congruent, C2_pr_Rest_E_Congruent))
ROI_pr_Rest_E_Incongruent.mean <- data.frame(CPZ_pr_Rest_E_Incongruent, CP1_pr_Rest_E_Incongruent, CP2_pr_Rest_E_Incongruent, P1_pr_Rest_E_Incongruent, PZ_pr_Rest_E_Incongruent, P2_pr_Rest_E_Incongruent, C1_pr_Rest_E_Incongruent, CZ_pr_Rest_E_Incongruent, C2_pr_Rest_E_Incongruent)
ROI_pr_Rest_E_Incongruent.mean <- rowMeans(ROI_pr_Rest_E_Incongruent.mean,c(CPZ_pr_Rest_E_Incongruent, CP1_pr_Rest_E_Incongruent, CP2_pr_Rest_E_Incongruent, P1_pr_Rest_E_Incongruent, PZ_pr_Rest_E_Incongruent, P2_pr_Rest_E_Incongruent, C1_pr_Rest_E_Incongruent, CZ_pr_Rest_E_Incongruent, C2_pr_Rest_E_Incongruent))
ROI_po_Rest_E_Congruent.mean <- data.frame(CPZ_po_Rest_E_Congruent, CP1_po_Rest_E_Congruent, CP2_po_Rest_E_Congruent, P1_po_Rest_E_Congruent, PZ_po_Rest_E_Congruent, P2_po_Rest_E_Congruent, C1_po_Rest_E_Congruent, CZ_po_Rest_E_Congruent, C2_po_Rest_E_Congruent)
ROI_po_Rest_E_Congruent.mean <- rowMeans(ROI_po_Rest_E_Congruent.mean,c(CPZ_po_Rest_E_Congruent, CP1_po_Rest_E_Congruent, CP2_po_Rest_E_Congruent, P1_po_Rest_E_Congruent, PZ_po_Rest_E_Congruent, P2_po_Rest_E_Congruent, C1_po_Rest_E_Congruent, CZ_po_Rest_E_Congruent, C2_po_Rest_E_Congruent))
ROI_po_Rest_E_Incongruent.mean <- data.frame(CPZ_po_Rest_E_Incongruent, CP1_po_Rest_E_Incongruent, CP2_po_Rest_E_Incongruent, P1_po_Rest_E_Incongruent, PZ_po_Rest_E_Incongruent, P2_po_Rest_E_Incongruent, C1_po_Rest_E_Incongruent, CZ_po_Rest_E_Incongruent, C2_po_Rest_E_Incongruent)
ROI_po_Rest_E_Incongruent.mean <- rowMeans(ROI_po_Rest_E_Incongruent.mean,c(CPZ_po_Rest_E_Incongruent, CP1_po_Rest_E_Incongruent, CP2_po_Rest_E_Incongruent, P1_po_Rest_E_Incongruent, PZ_po_Rest_E_Incongruent, P2_po_Rest_E_Incongruent, C1_po_Rest_E_Incongruent, CZ_po_Rest_E_Incongruent, C2_po_Rest_E_Incongruent))


ROI.Wide.df <- data.frame(time, ROI_pr_E_Congruent.mean, ROI_po_E_Congruent.mean, ROI_pr_E_Incongruent.mean, ROI_po_E_Incongruent.mean,  
                          ROI_pr_Rest_E_Congruent.mean, ROI_po_Rest_E_Congruent.mean, ROI_pr_Rest_E_Incongruent.mean, ROI_po_Rest_E_Incongruent.mean)
longformat.ROI.Wide.df<- melt(ROI.Wide.df , id = "time", variable.name = "Condition", variable.type = numeric)
longformat.ROI.Wide.df$value<-as.numeric(as.character(longformat.ROI.Wide.df$value))

ggplot(longformat.ROI.Wide.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(13, -5)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 1 Exercise vs. Rest Flanker ERP using Narrow ROI ERP (CPZ, CP1, CP2, P1, PZ, P2, C1, CZ, C2 average)")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#Model 1 ERPs (Exercise vs. Rest_E)
#FPZ Data
Model1.FPZ.df <- data.frame(time, FPZ_pr_E_Congruent, FPZ_po_E_Congruent, FPZ_pr_E_Incongruent, FPZ_po_E_Incongruent,
                            FPZ_pr_Rest_E_Congruent, FPZ_po_Rest_E_Congruent, FPZ_pr_Rest_E_Incongruent, FPZ_po_Rest_E_Incongruent)
longformat.Model1.FPZ.df <- melt(Model1.FPZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Model1.FPZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 1 Exercise vs. Rest Flanker ERP at FPZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#FZ Data
Model1.FZ.df <- data.frame(time, FZ_pr_E_Congruent, FZ_po_E_Congruent, FZ_pr_E_Incongruent, FZ_po_E_Incongruent,
                           FZ_pr_Rest_E_Congruent, FZ_po_Rest_E_Congruent, FZ_pr_Rest_E_Incongruent, FZ_po_Rest_E_Incongruent)
longformat.Model1.FZ.df <- melt(Model1.FZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Model1.FZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 1 Exercise vs. Rest Flanker ERP at FZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#FCZ Data
Model1.FCZ.df <- data.frame(time, FCZ_pr_E_Congruent, FCZ_po_E_Congruent, FCZ_pr_E_Incongruent, FCZ_po_E_Incongruent,
                            FCZ_pr_Rest_E_Congruent, FCZ_po_Rest_E_Congruent, FCZ_pr_Rest_E_Incongruent, FCZ_po_Rest_E_Incongruent)
longformat.Model1.FCZ.df <- melt(Model1.FCZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Model1.FCZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 1 Exercise vs. Rest Flanker ERP at FCZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#CZ Data
Model1.CZ.df <- data.frame(time, CZ_pr_E_Congruent, CZ_po_E_Congruent, CZ_pr_E_Incongruent, CZ_po_E_Incongruent,
                           CZ_pr_Rest_E_Congruent, CZ_po_Rest_E_Congruent, CZ_pr_Rest_E_Incongruent, CZ_po_Rest_E_Incongruent)
longformat.Model1.CZ.df <- melt(Model1.CZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Model1.CZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 1 Exercise vs. Rest Flanker ERP at CZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#CPZ Data
Model1.CPZ.df <- data.frame(time, CPZ_pr_E_Congruent, CPZ_po_E_Congruent, CPZ_pr_E_Incongruent, CPZ_po_E_Incongruent,
                            CPZ_pr_Rest_E_Congruent, CPZ_po_Rest_E_Congruent, CPZ_pr_Rest_E_Incongruent, CPZ_po_Rest_E_Incongruent)
longformat.Model1.CPZ.df <- melt(Model1.CPZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Model1.CPZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 1 Exercise vs. Rest Flanker ERP at CPZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#PZ Data
Model1.PZ.df <- data.frame(time, PZ_pr_E_Congruent, PZ_po_E_Congruent, PZ_pr_E_Incongruent, PZ_po_E_Incongruent,
                           PZ_pr_Rest_E_Congruent, PZ_po_Rest_E_Congruent, PZ_pr_Rest_E_Incongruent, PZ_po_Rest_E_Incongruent)
longformat.Model1.PZ.df <- melt(Model1.PZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Model1.PZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 1 Exercise vs. Rest Flanker ERP at PZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#POZ Data
Model1.POZ.df <- data.frame(time, POZ_pr_E_Congruent, POZ_po_E_Congruent, POZ_pr_E_Incongruent, POZ_po_E_Incongruent,
                            POZ_pr_Rest_E_Congruent, POZ_po_Rest_E_Congruent, POZ_pr_Rest_E_Incongruent, POZ_po_Rest_E_Incongruent)
longformat.Model1.POZ.df <- melt(Model1.POZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Model1.POZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 1 Exercise vs. Rest Flanker ERP at POZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#By intervention 
#Exercise
#FPZ Data
Exercise.FPZ.df <- data.frame(time, FPZ_pr_E_Congruent, FPZ_po_E_Congruent, FPZ_pr_E_Incongruent, FPZ_po_E_Incongruent)
longformat.Exercise.FPZ.df <- melt(Exercise.FPZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Exercise.FPZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Exercise Condition Flanker ERP at FPZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#FZ Data
Exercise.FZ.df <- data.frame(time, FZ_pr_E_Congruent, FZ_po_E_Congruent, FZ_pr_E_Incongruent, FZ_po_E_Incongruent)
longformat.Exercise.FZ.df <- melt(Exercise.FZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Exercise.FZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Exercise Condition Flanker ERP at FZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#FCZ Data
Exercise.FCZ.df <- data.frame(time, FCZ_pr_E_Congruent, FCZ_po_E_Congruent, FCZ_pr_E_Incongruent, FCZ_po_E_Incongruent)
longformat.Exercise.FCZ.df <- melt(Exercise.FCZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Exercise.FCZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Exercise Condition Flanker ERP at FCZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#CZ Data
Exercise.CZ.df <- data.frame(time, CZ_pr_E_Congruent, CZ_po_E_Congruent, CZ_pr_E_Incongruent, CZ_po_E_Incongruent)
longformat.Exercise.CZ.df <- melt(Exercise.CZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Exercise.CZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Exercise Condition Flanker ERP at CZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#CPZ Data
Exercise.CPZ.df <- data.frame(time, CPZ_pr_E_Congruent, CPZ_po_E_Congruent, CPZ_pr_E_Incongruent, CPZ_po_E_Incongruent)
longformat.Exercise.CPZ.df <- melt(Exercise.CPZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Exercise.CPZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Exercise Condition Flanker ERP at CPZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#PZ Data
Exercise.PZ.df <- data.frame(time, PZ_pr_E_Congruent, PZ_po_E_Congruent, PZ_pr_E_Incongruent, PZ_po_E_Incongruent)
longformat.Exercise.PZ.df <- melt(Exercise.PZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Exercise.PZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Exercise Condition Flanker ERP at PZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#POZ Data
Exercise.POZ.df <- data.frame(time, POZ_pr_E_Congruent, POZ_po_E_Congruent, POZ_pr_E_Incongruent, POZ_po_E_Incongruent)
longformat.Exercise.POZ.df <- melt(Exercise.POZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Exercise.POZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Exercise Condition Flanker ERP at POZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#Rest_E
#FPZ Data
Rest.FPZ.df <- data.frame(time, FPZ_pr_Rest_E_Congruent, FPZ_po_Rest_E_Congruent, FPZ_pr_Rest_E_Incongruent, FPZ_po_Rest_E_Incongruent)
longformat.Rest.FPZ.df <- melt(Rest.FPZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Rest.FPZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Rest_E Condition Flanker ERP at FPZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#FZ Data
Rest.FZ.df <- data.frame(time, FZ_pr_Rest_E_Congruent, FZ_po_Rest_E_Congruent, FZ_pr_Rest_E_Incongruent, FZ_po_Rest_E_Incongruent)
longformat.Rest.FZ.df <- melt(Rest.FZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Rest.FZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Rest_E Condition Flanker ERP at  FZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#FCZ Data
Rest.FCZ.df <- data.frame(time, FCZ_pr_Rest_E_Congruent, FCZ_po_Rest_E_Congruent, FCZ_pr_Rest_E_Incongruent, FCZ_po_Rest_E_Incongruent)
longformat.Rest.FCZ.df <- melt(Rest.FCZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Rest.FCZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Rest_E Condition Flanker ERP at  FCZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#CZ Data
Rest.CZ.df <- data.frame(time, CZ_pr_Rest_E_Congruent, CZ_po_Rest_E_Congruent, CZ_pr_Rest_E_Incongruent, CZ_po_Rest_E_Incongruent)
longformat.Rest.CZ.df <- melt(Rest.CZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Rest.CZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Rest_E Condition Flanker ERP at  CZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#CPZ Data
Rest.CPZ.df <- data.frame(time, CPZ_pr_Rest_E_Congruent, CPZ_po_Rest_E_Congruent, CPZ_pr_Rest_E_Incongruent, CPZ_po_Rest_E_Incongruent)
longformat.Rest.CPZ.df <- melt(Rest.CPZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Rest.CPZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Rest_E Condition Flanker ERP at  CPZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#PZ Data
Rest.PZ.df <- data.frame(time, PZ_pr_Rest_E_Congruent, PZ_po_Rest_E_Congruent, PZ_pr_Rest_E_Incongruent, PZ_po_Rest_E_Incongruent)
longformat.Rest.PZ.df <- melt(Rest.PZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Rest.PZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Rest_E Condition Flanker ERP at  PZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#POZ Data
Rest.POZ.df <- data.frame(time, POZ_pr_Rest_E_Congruent, POZ_po_Rest_E_Congruent, POZ_pr_Rest_E_Incongruent, POZ_po_Rest_E_Incongruent)
longformat.Rest.POZ.df <- melt(Rest.POZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Rest.POZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Rest_E Condition Flanker ERP at  POZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#Rest_T
#FPZ Data
Rest.FPZ.df <- data.frame(time, FPZ_pr_Rest_T_Congruent, FPZ_po_Rest_T_Congruent, FPZ_pr_Rest_T_Incongruent, FPZ_po_Rest_T_Incongruent)
longformat.Rest.FPZ.df <- melt(Rest.FPZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Rest.FPZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Rest_T Condition Flanker ERP at FPZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#FZ Data
Rest.FZ.df <- data.frame(time, FZ_pr_Rest_T_Congruent, FZ_po_Rest_T_Congruent, FZ_pr_Rest_T_Incongruent, FZ_po_Rest_T_Incongruent)
longformat.Rest.FZ.df <- melt(Rest.FZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Rest.FZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Rest_T Condition Flanker ERP at  FZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#FCZ Data
Rest.FCZ.df <- data.frame(time, FCZ_pr_Rest_T_Congruent, FCZ_po_Rest_T_Congruent, FCZ_pr_Rest_T_Incongruent, FCZ_po_Rest_T_Incongruent)
longformat.Rest.FCZ.df <- melt(Rest.FCZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Rest.FCZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Rest_T Condition Flanker ERP at  FCZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#CZ Data
Rest.CZ.df <- data.frame(time, CZ_pr_Rest_T_Congruent, CZ_po_Rest_T_Congruent, CZ_pr_Rest_T_Incongruent, CZ_po_Rest_T_Incongruent)
longformat.Rest.CZ.df <- melt(Rest.CZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Rest.CZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Rest_T Condition Flanker ERP at  CZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#CPZ Data
Rest.CPZ.df <- data.frame(time, CPZ_pr_Rest_T_Congruent, CPZ_po_Rest_T_Congruent, CPZ_pr_Rest_T_Incongruent, CPZ_po_Rest_T_Incongruent)
longformat.Rest.CPZ.df <- melt(Rest.CPZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Rest.CPZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Rest_T Condition Flanker ERP at  CPZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#PZ Data
Rest.PZ.df <- data.frame(time, PZ_pr_Rest_T_Congruent, PZ_po_Rest_T_Congruent, PZ_pr_Rest_T_Incongruent, PZ_po_Rest_T_Incongruent)
longformat.Rest.PZ.df <- melt(Rest.PZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Rest.PZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Rest_T Condition Flanker ERP at  PZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#POZ Data
Rest.POZ.df <- data.frame(time, POZ_pr_Rest_T_Congruent, POZ_po_Rest_T_Congruent, POZ_pr_Rest_T_Incongruent, POZ_po_Rest_T_Incongruent)
longformat.Rest.POZ.df <- melt(Rest.POZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Rest.POZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Rest_T Condition Flanker ERP at  POZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#Trier
#FPZ Data
Trier.FPZ.df <- data.frame(time, FPZ_pr_T_Congruent, FPZ_po_T_Congruent, FPZ_pr_T_Incongruent, FPZ_po_T_Incongruent)
longformat.Trier.FPZ.df <- melt(Trier.FPZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Trier.FPZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Trier Condition Flanker ERP at FPZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#FZ Data
Trier.FZ.df <- data.frame(time, FZ_pr_T_Congruent, FZ_po_T_Congruent, FZ_pr_T_Incongruent, FZ_po_T_Incongruent)
longformat.Trier.FZ.df <- melt(Trier.FZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Trier.FZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Trier Condition Flanker ERP at FZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#FCZ Data
Trier.FCZ.df <- data.frame(time, FCZ_pr_T_Congruent, FCZ_po_T_Congruent, FCZ_pr_T_Incongruent, FCZ_po_T_Incongruent)
longformat.Trier.FCZ.df <- melt(Trier.FCZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Trier.FCZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Trier Condition Flanker ERP at FCZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#CZ Data
Trier.CZ.df <- data.frame(time, CZ_pr_T_Congruent, CZ_po_T_Congruent, CZ_pr_T_Incongruent, CZ_po_T_Incongruent)
longformat.Trier.CZ.df <- melt(Trier.CZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Trier.CZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Trier Condition Flanker ERP at CZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#CPZ Data
Trier.CPZ.df <- data.frame(time, CPZ_pr_T_Congruent, CPZ_po_T_Congruent, CPZ_pr_T_Incongruent, CPZ_po_T_Incongruent)
longformat.Trier.CPZ.df <- melt(Trier.CPZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Trier.CPZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Trier Condition Flanker ERP at CPZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#PZ Data
Trier.PZ.df <- data.frame(time, PZ_pr_T_Congruent, PZ_po_T_Congruent, PZ_pr_T_Incongruent, PZ_po_T_Incongruent)
longformat.Trier.PZ.df <- melt(Trier.PZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Trier.PZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Trier Condition Flanker ERP at PZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#POZ Data
Trier.POZ.df <- data.frame(time, POZ_pr_T_Congruent, POZ_po_T_Congruent, POZ_pr_T_Incongruent, POZ_po_T_Incongruent)
longformat.Trier.POZ.df <- melt(Trier.POZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Trier.POZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(15, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Trier Condition Flanker ERP at POZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#Model 2 NARROW ROI ERP (Trier vs. Rest_E) CPZ, CP1, CP2
ROI_pr_T_Congruent.mean <-data.frame(CPZ_pr_T_Congruent, CP1_pr_T_Congruent, CP2_pr_T_Congruent)
ROI_pr_T_Congruent.mean <- rowMeans(ROI_pr_T_Congruent.mean,c(CPZ_pr_T_Congruent, CP1_pr_T_Congruent, CP2_pr_T_Congruent))
ROI_pr_T_Incongruent.mean<-data.frame(CPZ_pr_T_Incongruent, CP1_pr_T_Incongruent, CP2_pr_T_Incongruent)
ROI_pr_T_Incongruent.mean <- rowMeans(ROI_pr_T_Incongruent.mean,c(CPZ_pr_T_Incongruent, CP1_pr_T_Incongruent, CP2_pr_T_Incongruent))
ROI_po_T_Congruent.mean <- data.frame(CPZ_po_T_Congruent, CP1_po_T_Congruent, CP2_po_T_Congruent)
ROI_po_T_Congruent.mean <- rowMeans(ROI_po_T_Congruent.mean,c(CPZ_po_T_Congruent, CP1_po_T_Congruent, CP2_po_T_Congruent))
ROI_po_T_Incongruent.mean <- data.frame(CPZ_po_T_Incongruent, CP1_po_T_Incongruent, CP2_po_T_Incongruent)
ROI_po_T_Incongruent.mean <- rowMeans(ROI_po_T_Incongruent.mean,c(CPZ_po_T_Incongruent, CP1_po_T_Incongruent, CP2_po_T_Incongruent))
ROI_pr_Rest_T_Congruent.mean <-data.frame(CPZ_pr_Rest_T_Congruent, CP1_pr_Rest_T_Congruent, CP2_pr_Rest_T_Congruent)
ROI_pr_Rest_T_Congruent.mean <- rowMeans(ROI_pr_Rest_T_Congruent.mean,c(CPZ_pr_Rest_T_Congruent, CP1_pr_Rest_T_Congruent, CP2_pr_Rest_T_Congruent))
ROI_pr_Rest_T_Incongruent.mean <- data.frame(CPZ_pr_Rest_T_Incongruent, CP1_pr_Rest_T_Incongruent, CP2_pr_Rest_T_Incongruent)
ROI_pr_Rest_T_Incongruent.mean <- rowMeans(ROI_pr_Rest_T_Incongruent.mean,c(CPZ_pr_Rest_T_Incongruent, CP1_pr_Rest_T_Incongruent, CP2_pr_Rest_T_Incongruent))
ROI_po_Rest_T_Congruent.mean <- data.frame(CPZ_po_Rest_T_Congruent, CP1_po_Rest_T_Congruent, CP2_po_Rest_T_Congruent)
ROI_po_Rest_T_Congruent.mean <- rowMeans(ROI_po_Rest_T_Congruent.mean,c(CPZ_po_Rest_T_Congruent, CP1_po_Rest_T_Congruent, CP2_po_Rest_T_Congruent))
ROI_po_Rest_T_Incongruent.mean <- data.frame(CPZ_po_Rest_T_Incongruent, CP1_po_Rest_T_Incongruent, CP2_po_Rest_T_Incongruent)
ROI_po_Rest_T_Incongruent.mean <- rowMeans(ROI_po_Rest_T_Incongruent.mean,c(CPZ_po_Rest_T_Incongruent, CP1_po_Rest_T_Incongruent, CP2_po_Rest_T_Incongruent))

ROI.Narrow.df <- data.frame(time, ROI_pr_T_Congruent.mean, ROI_po_T_Congruent.mean, ROI_pr_T_Incongruent.mean, ROI_po_T_Incongruent.mean,  
                            ROI_pr_Rest_T_Congruent.mean, ROI_po_Rest_T_Congruent.mean, ROI_pr_Rest_T_Incongruent.mean, ROI_po_Rest_T_Incongruent.mean)
longformat.ROI.Narrow.df<- melt(ROI.Narrow.df , id = "time", variable.name = "Condition", variable.type = numeric)
longformat.ROI.Narrow.df$value<-as.numeric(as.character(longformat.ROI.Narrow.df$value))

ggplot(longformat.ROI.Narrow.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(13, -5)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 2 Trier vs. Rest Flanker ERP using Narrow ROI ERP (CPZ, CP1, CP2 average)")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")



#Model 2 WIDE ROI ERP (Trier vs. Rest_E) CPZ, CP1, CP2, P1, PZ, P2, C1, CZ, C2
ROI_pr_T_Congruent.mean <-data.frame(CPZ_pr_T_Congruent, CP1_pr_T_Congruent, CP2_pr_T_Congruent, P1_pr_T_Congruent, PZ_pr_T_Congruent, P2_pr_T_Congruent, C1_pr_T_Congruent, CZ_pr_T_Congruent, C2_pr_T_Congruent)
ROI_pr_T_Congruent.mean <- rowMeans(ROI_pr_T_Congruent.mean,c(CPZ_pr_T_Congruent, CP1_pr_T_Congruent, CP2_pr_T_Congruent, P1_pr_T_Congruent, PZ_pr_T_Congruent, P2_pr_T_Congruent, C1_pr_T_Congruent, CZ_pr_T_Congruent, C2_pr_T_Congruent))
ROI_pr_T_Incongruent.mean<-data.frame(CPZ_pr_T_Incongruent, CP1_pr_T_Incongruent, CP2_pr_T_Incongruent, P1_pr_T_Incongruent, PZ_pr_T_Incongruent, P2_pr_T_Incongruent, C1_pr_T_Incongruent, CZ_pr_T_Incongruent, C2_pr_T_Incongruent)
ROI_pr_T_Incongruent.mean <- rowMeans(ROI_pr_T_Incongruent.mean,c(CPZ_pr_T_Incongruent, CP1_pr_T_Incongruent, CP2_pr_T_Incongruent, P1_pr_T_Incongruent, PZ_pr_T_Incongruent, P2_pr_T_Incongruent, C1_pr_T_Incongruent, CZ_pr_T_Incongruent, C2_pr_T_Incongruent))
ROI_po_T_Congruent.mean <- data.frame(CPZ_po_T_Congruent, CP1_po_T_Congruent, CP2_po_T_Congruent, P1_po_T_Congruent, PZ_po_T_Congruent, P2_po_T_Congruent, C1_po_T_Congruent, CZ_po_T_Congruent, C2_po_T_Congruent)
ROI_po_T_Congruent.mean <- rowMeans(ROI_po_T_Congruent.mean,c(CPZ_po_T_Congruent, CP1_po_T_Congruent, CP2_po_T_Congruent, P1_po_T_Congruent, PZ_po_T_Congruent, P2_po_T_Congruent, C1_po_T_Congruent, CZ_po_T_Congruent, C2_po_T_Congruent))
ROI_po_T_Incongruent.mean <- data.frame(CPZ_po_T_Incongruent, CP1_po_T_Incongruent, CP2_po_T_Incongruent, P1_po_T_Incongruent, PZ_po_T_Incongruent, P2_po_T_Incongruent, C1_po_T_Incongruent, CZ_po_T_Incongruent, C2_po_T_Incongruent)
ROI_po_T_Incongruent.mean <- rowMeans(ROI_po_T_Incongruent.mean,c(CPZ_po_T_Incongruent, CP1_po_T_Incongruent, CP2_po_T_Incongruent, P1_po_T_Incongruent, PZ_po_T_Incongruent, P2_po_T_Incongruent, C1_po_T_Incongruent, CZ_po_T_Incongruent, C2_po_T_Incongruent))
ROI_pr_Rest_T_Congruent.mean <-data.frame(CPZ_pr_Rest_T_Congruent, CP1_pr_Rest_T_Congruent, CP2_pr_Rest_T_Congruent, P1_pr_Rest_T_Congruent, PZ_pr_Rest_T_Congruent, P2_pr_Rest_T_Congruent, C1_pr_Rest_T_Congruent, CZ_pr_Rest_T_Congruent, C2_pr_Rest_T_Congruent)
ROI_pr_Rest_T_Congruent.mean <- rowMeans(ROI_pr_Rest_T_Congruent.mean,c(CPZ_pr_Rest_T_Congruent, CP1_pr_Rest_T_Congruent, CP2_pr_Rest_T_Congruent, P1_pr_Rest_T_Congruent, PZ_pr_Rest_T_Congruent, P2_pr_Rest_T_Congruent, C1_pr_Rest_T_Congruent, CZ_pr_Rest_T_Congruent, C2_pr_Rest_T_Congruent))
ROI_pr_Rest_T_Incongruent.mean <- data.frame(CPZ_pr_Rest_T_Incongruent, CP1_pr_Rest_T_Incongruent, CP2_pr_Rest_T_Incongruent, P1_pr_Rest_T_Incongruent, PZ_pr_Rest_T_Incongruent, P2_pr_Rest_T_Incongruent, C1_pr_Rest_T_Incongruent, CZ_pr_Rest_T_Incongruent, C2_pr_Rest_T_Incongruent)
ROI_pr_Rest_T_Incongruent.mean <- rowMeans(ROI_pr_Rest_T_Incongruent.mean,c(CPZ_pr_Rest_T_Incongruent, CP1_pr_Rest_T_Incongruent, CP2_pr_Rest_T_Incongruent, P1_pr_Rest_T_Incongruent, PZ_pr_Rest_T_Incongruent, P2_pr_Rest_T_Incongruent, C1_pr_Rest_T_Incongruent, CZ_pr_Rest_T_Incongruent, C2_pr_Rest_T_Incongruent))
ROI_po_Rest_T_Congruent.mean <- data.frame(CPZ_po_Rest_T_Congruent, CP1_po_Rest_T_Congruent, CP2_po_Rest_T_Congruent, P1_po_Rest_T_Congruent, PZ_po_Rest_T_Congruent, P2_po_Rest_T_Congruent, C1_po_Rest_T_Congruent, CZ_po_Rest_T_Congruent, C2_po_Rest_T_Congruent)
ROI_po_Rest_T_Congruent.mean <- rowMeans(ROI_po_Rest_T_Congruent.mean,c(CPZ_po_Rest_T_Congruent, CP1_po_Rest_T_Congruent, CP2_po_Rest_T_Congruent, P1_po_Rest_T_Congruent, PZ_po_Rest_T_Congruent, P2_po_Rest_T_Congruent, C1_po_Rest_T_Congruent, CZ_po_Rest_T_Congruent, C2_po_Rest_T_Congruent))
ROI_po_Rest_T_Incongruent.mean <- data.frame(CPZ_po_Rest_T_Incongruent, CP1_po_Rest_T_Incongruent, CP2_po_Rest_T_Incongruent, P1_po_Rest_T_Incongruent, PZ_po_Rest_T_Incongruent, P2_po_Rest_T_Incongruent, C1_po_Rest_T_Incongruent, CZ_po_Rest_T_Incongruent, C2_po_Rest_T_Incongruent)
ROI_po_Rest_T_Incongruent.mean <- rowMeans(ROI_po_Rest_T_Incongruent.mean,c(CPZ_po_Rest_T_Incongruent, CP1_po_Rest_T_Incongruent, CP2_po_Rest_T_Incongruent, P1_po_Rest_T_Incongruent, PZ_po_Rest_T_Incongruent, P2_po_Rest_T_Incongruent, C1_po_Rest_T_Incongruent, CZ_po_Rest_T_Incongruent, C2_po_Rest_T_Incongruent))


ROI.Wide.df <- data.frame(time, ROI_pr_T_Congruent.mean, ROI_po_T_Congruent.mean, ROI_pr_T_Incongruent.mean, ROI_po_T_Incongruent.mean,  
                          ROI_pr_Rest_T_Congruent.mean, ROI_po_Rest_T_Congruent.mean, ROI_pr_Rest_T_Incongruent.mean, ROI_po_Rest_T_Incongruent.mean)
longformat.ROI.Wide.df<- melt(ROI.Wide.df , id = "time", variable.name = "Condition", variable.type = numeric)
longformat.ROI.Wide.df$value<-as.numeric(as.character(longformat.ROI.Wide.df$value))

ggplot(longformat.ROI.Wide.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(13, -5)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 2 Trier vs. Rest Flanker ERP using Narrow ROI ERP (CPZ, CP1, CP2, P1, PZ, P2, C1, CZ, C2 average)")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")


#Model 2 ERPs (Trier vs. Rest_T)
#FPZ Data
Model2.FPZ.df <- data.frame(time, FPZ_pr_T_Congruent, FPZ_po_T_Congruent, FPZ_pr_T_Incongruent, FPZ_po_T_Incongruent,
                            FPZ_pr_Rest_T_Congruent, FPZ_po_Rest_T_Congruent, FPZ_pr_Rest_T_Incongruent, FPZ_po_Rest_T_Incongruent)
longformat.Model2.FPZ.df <- melt(Model2.FPZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Model2.FPZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(12, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 2 Trier vs. Rest Flanker ERP at FPZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#FZ Data
Model2.FZ.df <- data.frame(time, FZ_pr_T_Congruent, FZ_po_T_Congruent, FZ_pr_T_Incongruent, FZ_po_T_Incongruent,
                           FZ_pr_Rest_T_Congruent, FZ_po_Rest_T_Congruent, FZ_pr_Rest_T_Incongruent, FZ_po_Rest_T_Incongruent)
longformat.Model2.FZ.df <- melt(Model2.FZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Model2.FZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(12, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 2 Trier vs. Rest Flanker ERP at FZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#FCZ Data
Model2.FCZ.df <- data.frame(time, FCZ_pr_T_Congruent, FCZ_po_T_Congruent, FCZ_pr_T_Incongruent, FCZ_po_T_Incongruent,
                            FCZ_pr_Rest_T_Congruent, FCZ_po_Rest_T_Congruent, FCZ_pr_Rest_T_Incongruent, FCZ_po_Rest_T_Incongruent)
longformat.Model2.FCZ.df <- melt(Model2.FCZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Model2.FCZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(12, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 2 Trier vs. Rest Flanker ERP at FCZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#CZ Data
Model2.CZ.df <- data.frame(time, CZ_pr_T_Congruent, CZ_po_T_Congruent, CZ_pr_T_Incongruent, CZ_po_T_Incongruent,
                           CZ_pr_Rest_T_Congruent, CZ_po_Rest_T_Congruent, CZ_pr_Rest_T_Incongruent, CZ_po_Rest_T_Incongruent)
longformat.Model2.CZ.df <- melt(Model2.CZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Model2.CZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(12, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 2 Trier vs. Rest Flanker ERP at CZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#CPZ Data
Model2.CPZ.df <- data.frame(time, CPZ_pr_T_Congruent, CPZ_po_T_Congruent, CPZ_pr_T_Incongruent, CPZ_po_T_Incongruent,
                            CPZ_pr_Rest_T_Congruent, CPZ_po_Rest_T_Congruent, CPZ_pr_Rest_T_Incongruent, CPZ_po_Rest_T_Incongruent)
longformat.Model2.CPZ.df <- melt(Model2.CPZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Model2.CPZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(12, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 2 Trier vs. Rest Flanker ERP at CPZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#PZ Data
Model2.PZ.df <- data.frame(time, PZ_pr_T_Congruent, PZ_po_T_Congruent, PZ_pr_T_Incongruent, PZ_po_T_Incongruent,
                           PZ_pr_Rest_T_Congruent, PZ_po_Rest_T_Congruent, PZ_pr_Rest_T_Incongruent, PZ_po_Rest_T_Incongruent)
longformat.Model2.PZ.df <- melt(Model2.PZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Model2.PZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(12, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 2 Trier vs. Rest Flanker ERP at PZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")

#POZ Data
Model2.POZ.df <- data.frame(time, POZ_pr_T_Congruent, POZ_po_T_Congruent, POZ_pr_T_Incongruent, POZ_po_T_Incongruent,
                            POZ_pr_Rest_T_Congruent, POZ_po_Rest_T_Congruent, POZ_pr_Rest_T_Incongruent, POZ_po_Rest_T_Incongruent)
longformat.Model2.POZ.df <- melt(Model2.POZ.df, id = "time", variable.name = "Condition")

ggplot(longformat.Model2.POZ.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(12, -10)+
  xlim(-200, 1000)+
  xlab("Time")+
  ylab("ERP Amplitude")+
  ggtitle("Model 2 Trier vs. Rest Flanker ERP at POZ Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_brewer(palette = "Paired")


# Exploratory Analyses ----------------------------------------------------
#Inverse Efficiency
#Call variables
SubjectID<-(df$SubjectID)
SAya_FL_Congruent_InverseEfficiency_Rest_E_pre<-(df$SAya_FL_Congruent_InverseEfficiency_Rest_E_pre)
SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre<-(df$SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre)
SAya_FL_Congruent_InverseEfficiency_Rest_E_post<-(df$SAya_FL_Congruent_InverseEfficiency_Rest_E_post)
SAya_FL_Incongruent_InverseEfficiency_Rest_E_post<-(df$SAya_FL_Incongruent_InverseEfficiency_Rest_E_post)
SAya_FL_Congruent_InverseEfficiency_E_pre<-(df$SAya_FL_Congruent_InverseEfficiency_E_pre)
SAya_FL_Incongruent_InverseEfficiency_E_pre<-(df$SAya_FL_Incongruent_InverseEfficiency_E_pre)
SAya_FL_Congruent_InverseEfficiency_E_post<-(df$SAya_FL_Congruent_InverseEfficiency_E_post)
SAya_FL_Incongruent_InverseEfficiency_E_post<-(df$SAya_FL_Incongruent_InverseEfficiency_E_post)
SAya_FL_Congruent_InverseEfficiency_Rest_T_pre<-(df$SAya_FL_Congruent_InverseEfficiency_Rest_T_pre)
SAya_FL_Incongruent_InverseEfficiency_Rest_T_pre<-(df$SAya_FL_Incongruent_InverseEfficiency_Rest_T_pre)
SAya_FL_Congruent_InverseEfficiency_Rest_T_post<-(df$SAya_FL_Congruent_InverseEfficiency_Rest_T_post)
SAya_FL_Incongruent_InverseEfficiency_Rest_T_post<-(df$SAya_FL_Incongruent_InverseEfficiency_Rest_T_post)
SAya_FL_Congruent_InverseEfficiency_T_pre<-(df$SAya_FL_Congruent_InverseEfficiency_T_pre)
SAya_FL_Incongruent_InverseEfficiency_T_pre<-(df$SAya_FL_Incongruent_InverseEfficiency_T_pre)
SAya_FL_Congruent_InverseEfficiency_T_post<-(df$SAya_FL_Congruent_InverseEfficiency_T_post)
SAya_FL_Incongruent_InverseEfficiency_T_post<-(df$SAya_FL_Incongruent_InverseEfficiency_T_post)

#M1 Difference in Inverse Efficiency Exercise vs. Rest
DiffPrePost_InverseEfficiency_R_Congruent<-SAya_FL_Congruent_InverseEfficiency_Rest_E_post-SAya_FL_Congruent_InverseEfficiency_Rest_E_pre
DiffPrePost_InverseEfficiency_R_Incongruent<-SAya_FL_Incongruent_InverseEfficiency_Rest_E_post-SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre
DiffPrePost_InverseEfficiency_E_Congruent<-SAya_FL_Congruent_InverseEfficiency_E_post-SAya_FL_Congruent_InverseEfficiency_E_pre
DiffPrePost_InverseEfficiency_E_Incongruent<-SAya_FL_Incongruent_InverseEfficiency_E_post-SAya_FL_Incongruent_InverseEfficiency_E_pre
M1_DiffPrePost_InverseEfficiency.df <- data.frame(SubjectID, DiffPrePost_InverseEfficiency_R_Congruent, DiffPrePost_InverseEfficiency_R_Incongruent, 
                                                  DiffPrePost_InverseEfficiency_E_Congruent, DiffPrePost_InverseEfficiency_E_Incongruent)
#Summarize
summary(M1_DiffPrePost_InverseEfficiency.df)
#melt dataframe
longformat.M1_DiffPrePost_InverseEfficiency.df <- melt(M1_DiffPrePost_InverseEfficiency.df, id = "SubjectID", variable.name = "Condition")
view(longformat.M1_DiffPrePost_InverseEfficiency.df)
#Visualize
ggplot(longformat.M1_DiffPrePost_InverseEfficiency.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Inverse Efficiency Difference Post-Pre for Model 1 Exercise vs. Rest", x ="Intervention", y = "Inverse Efficiency Difference")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.ACC_Model1_Diff_PrePost.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
M1_DiffPrePost_InverseEfficiency.df %>%
  shapiro_test(DiffPrePost_InverseEfficiency_R_Congruent, DiffPrePost_InverseEfficiency_R_Incongruent, 
               DiffPrePost_InverseEfficiency_E_Congruent, DiffPrePost_InverseEfficiency_E_Incongruent)
histogram(ACC_Model1_Diff_PrePost.df$Diff_PrePost_R_Congruent_ACC)
#Passed normality test! Parametric stats used
ggwithinstats(data = longformat.M1_DiffPrePost_InverseEfficiency.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_kendalls_w(0.03)

#M2 Difference in Inverse Efficiency Trier vs. Rest
DiffPrePost_InverseEfficiency_R_Congruent<-SAya_FL_Congruent_InverseEfficiency_Rest_T_post-SAya_FL_Congruent_InverseEfficiency_Rest_T_pre
DiffPrePost_InverseEfficiency_R_Incongruent<-SAya_FL_Incongruent_InverseEfficiency_Rest_T_post-SAya_FL_Incongruent_InverseEfficiency_Rest_T_pre
DiffPrePost_InverseEfficiency_T_Congruent<-SAya_FL_Congruent_InverseEfficiency_T_post-SAya_FL_Congruent_InverseEfficiency_T_pre
DiffPrePost_InverseEfficiency_T_Incongruent<-SAya_FL_Incongruent_InverseEfficiency_T_post-SAya_FL_Incongruent_InverseEfficiency_T_pre
M2_DiffPrePost_InverseEfficiency.df <- data.frame(SubjectID, DiffPrePost_InverseEfficiency_R_Congruent, DiffPrePost_InverseEfficiency_R_Incongruent, 
                                                  DiffPrePost_InverseEfficiency_T_Congruent, DiffPrePost_InverseEfficiency_T_Incongruent)
#Summarize
summary(M2_DiffPrePost_InverseEfficiency.df)
#melt dataframe
longformat.M2_DiffPrePost_InverseEfficiency.df <- melt(M2_DiffPrePost_InverseEfficiency.df, id = "SubjectID", variable.name = "Condition")
view(longformat.M2_DiffPrePost_InverseEfficiency.df)
#Visualize
ggplot(longformat.M2_DiffPrePost_InverseEfficiency.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Inverse Efficiency Difference Post-Pre for Model 1 Trier vs. Rest", x ="Intervention", y = "Inverse Efficiency Difference")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.ACC_Model2_Diff_PrePost.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
M2_DiffPrePost_InverseEfficiency.df %>%
  shapiro_test(DiffPrePost_InverseEfficiency_R_Congruent, DiffPrePost_InverseEfficiency_R_Incongruent, 
               DiffPrePost_InverseEfficiency_T_Congruent, DiffPrePost_InverseEfficiency_T_Incongruent)
histogram(ACC_Model2_Diff_PrePost.df$Diff_PrePost_R_Congruent_ACC)
#Did not pass normality test! Non-parametric used
ggwithinstats(data = longformat.M2_DiffPrePost_InverseEfficiency.df, x = Condition, y = value, type = "nonparametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_kendalls_w(0.03)

#M1: Flanker EZ diffusion model; Pc = response accuracy, VRT = response variance (SD^2), MRT = mean response time 
SubjectID<-(df$SubjectID)
Pc <- df$SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre
VRT <- (df$SAya_FL_Congruent_SD_RT_Rest_E_pre)^2
MRT <- df$SAya_FL_Congruent_MeanRT_Rest_E_pre

get.vaTer = function(Pc, VRT, MRT, s=0.1)
{
  # The default value for the scaling parameter s equals 0.1
  s2 = s^2
  if (Pc == 0)
    cat(Oops, Pc == 0!\n)
  if (Pc == 0.5)
    cat(Oops, Pc == .5!\n)
  if (Pc == 1)
    cat(Oops, Pc == 1!\n)
  # If Pc equals 0, .5, or 1, the method will not work, and
  # an edge correction is required.
  # The function “qlogis” calculates the logit.
  L = qlogis(Pc)
  # This gives drift rate.
  x = L*(L*Pc^2 -L*Pc + Pc - .5)/VRT
  v = sign(Pc-.5)*s*x^(1/4)
  # This gives boundary separation.
  a = s2*qlogis(Pc)/v
  # This gives nondecision time.
  y = -v*a/s2
  MDT = (a/(2*v)) * (1-exp(y))/(1+exp(y))
  Ter = MRT - MDT
  return(list(v, a, Ter))
}

Pc.Congruent.Rest.E.Post <- df$SAya_FL_Congruent_ResponseAccuracy_Rest_E_post
VRT.Congruent.Rest.E.Post <- (df$SAya_FL_Congruent_SD_RT_Rest_E_post)^2
MRT.Congruent.Rest.E.Post <- df$SAya_FL_Congruent_MeanRT_Rest_E_post

Pc.Incongruent.Rest.E.Pre <- df$SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre
VRT.Incongruent.Rest.E.Pre <- (df$SAya_FL_Incongruent_SD_RT_Rest_E_pre)^2
MRT.Incongruent.Rest.E.Pre <- df$SAya_FL_Incongruent_MeanRT_Rest_E_pre

Pc.Incongruent.Rest.E.Post <- df$SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post
VRT.Incongruent.Rest.E.Post <- (df$SAya_FL_Incongruent_SD_RT_Rest_E_post)^2
MRT.Incongruent.Rest.E.Post <- df$SAya_FL_Incongruent_MeanRT_Rest_E_post

Pc.Congruent.E.Pre <- df$SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre
VRT.Congruent.E.Pre <- (df$SAya_FL_Congruent_SD_RT_Rest_E_pre)^2
MRT.Congruent.E.Pre <- df$SAya_FL_Congruent_MeanRT_Rest_E_pre

Pc.Congruent.E.Post <- df$SAya_FL_Congruent_ResponseAccuracy_Rest_E_post
VRT.Congruent.E.Post <- (df$SAya_FL_Congruent_SD_RT_Rest_E_post)^2
MRT.Congruent.E.Post <- df$SAya_FL_Congruent_MeanRT_Rest_E_post

Pc.Incongruent.E.Pre <- df$SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre
VRT.Incongruent.E.Pre <- (df$SAya_FL_Incongruent_SD_RT_Rest_E_pre)^2
MRT.Incongruent.E.Pre <- df$SAya_FL_Incongruent_MeanRT_Rest_E_pre

Pc.Incongruent.E.Post <- df$SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post
VRT.Incongruent.E.Post <- (df$SAya_FL_Incongruent_SD_RT_Rest_E_post)^2
MRT.Incongruent.E.Post <- df$SAya_FL_Incongruent_MeanRT_Rest_E_post













