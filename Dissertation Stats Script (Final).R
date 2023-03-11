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
library(ggside)
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
library(GGally)
library(emmeans)

#Report 3 way repeated measures ANOVA: 
#https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/#post-hoc-tests-1
#Nonparametric test info: 
#https://yuzar-blog.netlify.app/posts/2022-02-08-friedman/
  
#Import Data
Dissertation_Database<- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/Dissertation_Database.xlsx")
#View(Dissertation_Database)

#Merge data
#Dissertation_Database<-merge(df1, df2, by = "SubjectID", all = TRUE)
#write_xlsx(Dissertation_Database, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//Dissertation_Database.xlsx') #export long format df

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
chisq.test(df$b_rand, df$householdincome, correct = FALSE)
ggplot(df, aes(x = householdincome, fill = b_rand_string, color = b_rand_string))+
  geom_bar(alpha = .5)+
  ylab("Count")+
  xlab("Household Income Category")+
  ggtitle("Income Category")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Randomization")+
  labs(color = "Randomization")

#Mother's Education, 1=did not complete HS, 2 = HS grad, 3 = some college, 4 = bachelor's, 5 = advanced degree 
tabyl(df, motherseducaion)
chisq.test(df$b_rand, df$motherseducaion, correct = FALSE)
ggplot(df, aes(x = motherseducaion, fill = b_rand_string, color = b_rand_string))+
  geom_bar(alpha = .5)+
  ylab("Count")+
  xlab("Mother's Education Category")+
  ggtitle("Mother's Education Category")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Randomization")+
  labs(color = "Randomization")

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
             linetype="dashed", linewidth=1)+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="VO2 Percentile by Sex", x ="VO2 Percentile (mL/kg/min)", y = "Frequency")
 
#BMI
mean(df$VO2_BMI, na.rm = TRUE)
sd(df$VO2_BMI, na.rm = TRUE)
summary(aov(b_rand~VO2_BMI, data = df))

#BMI class
tabyl(df, VO2_BMI_class)
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
SAA_1_Rest_E <- log10(df$SAA_1_Rest_E)
SAA_4_Rest_E <- log10(df$SAA_4_Rest_E)
SAA_6_Rest_E <- log10(df$SAA_6_Rest_E)
SAA_7_Rest_E <- log10(df$SAA_7_Rest_E)
SAA_1_Exercise <- log10(df$SAA_1_Exercise)
SAA_4_Exercise <- log10(df$SAA_4_Exercise)
SAA_6_Exercise <- log10(df$SAA_6_Exercise)
SAA_7_Exercise <- log10(df$SAA_7_Exercise)
SAA_1_Rest_T <- log10(df$SAA_1_Rest_T)
SAA_4_Rest_T <- log10(df$SAA_4_Rest_T)
SAA_6_Rest_T <- log10(df$SAA_6_Rest_T)
SAA_7_Rest_T <- log10(df$SAA_7_Rest_T)
SAA_1_Trier <- log10(df$SAA_1_Trier)
SAA_4_Trier <- log10(df$SAA_4_Trier)
SAA_6_Trier <- log10(df$SAA_6_Trier)
SAA_7_Trier <- log10(df$SAA_7_Trier)
SAA_Rest_E_mean<-data.frame(SAA_1_Rest_E, SAA_4_Rest_E, SAA_6_Rest_E, SAA_7_Rest_E)
SAA_Rest_E_mean<-rowMeans(SAA_Rest_E_mean)
SAA_Exercise_mean<-data.frame(SAA_1_Exercise, SAA_4_Exercise, SAA_6_Exercise, SAA_7_Exercise)
SAA_Exercise_mean<-rowMeans(SAA_Exercise_mean)
SAA_Rest_T_mean<-data.frame(SAA_1_Rest_T, SAA_4_Rest_T, SAA_6_Rest_T, SAA_7_Rest_T)
SAA_Rest_T_mean<-rowMeans(SAA_Rest_T_mean)
SAA_Trier_mean<-data.frame(SAA_1_Trier, SAA_4_Trier, SAA_6_Trier, SAA_7_Trier)
SAA_Trier_mean<-rowMeans(SAA_Trier_mean)

Cort_1_Rest_E <- log10(df$Cort_1_Rest_E)+2
Cort_4_Rest_E <- log10(df$Cort_4_Rest_E)+2
Cort_6_Rest_E <- log10(df$Cort_6_Rest_E)+2
Cort_7_Rest_E <- log10(df$Cort_7_Rest_E)+2
Cort_1_Exercise <- log10(df$Cort_1_Exercise)+2
Cort_4_Exercise <- log10(df$Cort_4_Exercise)+2
Cort_6_Exercise <- log10(df$Cort_6_Exercise)+2
Cort_7_Exercise <- log10(df$Cort_7_Exercise)+2
Cort_1_Rest_T <- log10(df$Cort_1_Rest_T)+2
Cort_4_Rest_T <- log10(df$Cort_4_Rest_T)+2
Cort_6_Rest_T <- log10(df$Cort_6_Rest_T)+2
Cort_7_Rest_T <- log10(df$Cort_7_Rest_T)+2
Cort_1_Trier <- log10(df$Cort_1_Trier)+2
Cort_4_Trier <- log10(df$Cort_4_Trier)+2
Cort_6_Trier <- log10(df$Cort_6_Trier)+2
Cort_7_Trier <- log10(df$Cort_7_Trier)+2
Cort_Rest_E_mean<-data.frame(Cort_1_Rest_E, Cort_4_Rest_E, Cort_6_Rest_E, Cort_7_Rest_E)
Cort_Rest_E_mean<-rowMeans(Cort_Rest_E_mean)
Cort_Exercise_mean<-data.frame(Cort_1_Exercise, Cort_4_Exercise, Cort_6_Exercise, Cort_7_Exercise)
Cort_Exercise_mean<-rowMeans(Cort_Exercise_mean)
Cort_Rest_T_mean<-data.frame(Cort_1_Rest_T, Cort_4_Rest_T, Cort_6_Rest_T, Cort_7_Rest_T)
Cort_Rest_T_mean<-rowMeans(Cort_Rest_T_mean)
Cort_Trier_mean<-data.frame(Cort_1_Trier, Cort_4_Trier, Cort_6_Trier, Cort_7_Trier)
Cort_Trier_mean<-rowMeans(Cort_Trier_mean)

Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
TOD_M1<-data.frame(Rest_E_TOD, E_TOD)
TOD_M1<-rowMeans(TOD_M1)
Rest_T_TOD<-df$r_end_time_T
T_TOD<-df$start_time
TOD_M2<-data.frame(Rest_T_TOD, T_TOD)
TOD_M2<-rowMeans(TOD_M2)

Age<-df$dob_years
Sex<-df$b_vo2_sex
Stress<-df$pss_total

#Is TOD related to sAA? 
summary(lm(SAA_Rest_E_mean~Rest_E_TOD, data = df))
summary(lm(SAA_Exercise_mean~E_TOD, data = df))
summary(lm(SAA_Rest_T_mean~Rest_T_TOD, data = df))
summary(lm(SAA_Trier_mean~T_TOD, data = df))

#is TOD related to cortisol? 
summary(lm(Cort_Rest_E_mean~Rest_E_TOD, data = df))
summary(lm(Cort_Exercise_mean~E_TOD, data = df))
summary(lm(Cort_Rest_T_mean~Rest_T_TOD, data = df))
summary(lm(Cort_Trier_mean~T_TOD, data = df))

#is age related to sAA? 
summary(lm(SAA_Rest_E_mean~Age, data = df))
summary(lm(SAA_Exercise_mean~Age, data = df))
summary(lm(SAA_Rest_T_mean~Age, data = df))
summary(lm(SAA_Trier_mean~Age, data = df))

#Is age related to cortisol? 
summary(lm(Cort_Rest_E_mean~Age, data = df))
summary(lm(Cort_Exercise_mean~Age, data = df))
summary(lm(Cort_Rest_T_mean~Age, data = df))
summary(lm(Cort_Trier_mean~Age, data = df))

#is Sex related to sAA? 
summary(lm(SAA_Rest_E_mean~Sex, data = df))
summary(lm(SAA_Exercise_mean~Sex, data = df))
summary(lm(SAA_Rest_T_mean~Sex, data = df))
summary(lm(SAA_Trier_mean~Sex, data = df))

#Is Sex related to cortisol? 
summary(lm(Cort_Rest_E_mean~Sex, data = df))
summary(lm(Cort_Exercise_mean~Sex, data = df))
summary(lm(Cort_Rest_T_mean~Sex, data = df))
summary(lm(Cort_Trier_mean~Sex, data = df))

#is stress related to sAA? 
summary(lm(SAA_Rest_E_mean~Stress, data = df))
summary(lm(SAA_Exercise_mean~Stress, data = df))
summary(lm(SAA_Rest_T_mean~Stress, data = df))
summary(lm(SAA_Trier_mean~Stress, data = df))

#is stress related to cortisol? 
summary(lm(Cort_Rest_E_mean~Stress, data = df))
summary(lm(Cort_Exercise_mean~Stress, data = df))
summary(lm(Cort_Rest_T_mean~Stress, data = df))
summary(lm(Cort_Trier_mean~Stress, data = df))

# SALIVARY OUTCOMES - SAA -------------------------------------------------
#Call variables 
SubjectID<-df$SubjectID
SAA_1_Rest_E <- log10(df$SAA_1_Rest_E)
SAA_4_Rest_E <- log10(df$SAA_4_Rest_E)
SAA_6_Rest_E <- log10(df$SAA_6_Rest_E)
SAA_7_Rest_E <- log10(df$SAA_7_Rest_E)
SAA_1_Exercise <- log10(df$SAA_1_Exercise)
SAA_4_Exercise <- log10(df$SAA_4_Exercise)
SAA_6_Exercise <- log10(df$SAA_6_Exercise)
SAA_7_Exercise <- log10(df$SAA_7_Exercise)
SAA_1_Rest_T <- log10(df$SAA_1_Rest_T)
SAA_4_Rest_T <- log10(df$SAA_4_Rest_T)
SAA_6_Rest_T <- log10(df$SAA_6_Rest_T)
SAA_7_Rest_T <- log10(df$SAA_7_Rest_T)
SAA_1_Trier <- log10(df$SAA_1_Trier)
SAA_4_Trier <- log10(df$SAA_4_Trier)
SAA_6_Trier <- log10(df$SAA_6_Trier)
SAA_7_Trier <- log10(df$SAA_7_Trier)

Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
TOD_M1<-data.frame(Rest_E_TOD, E_TOD)
TOD_M1<-rowMeans(TOD_M1)

Rest_T_TOD<-df$r_end_time_T
T_TOD<-df$start_time
TOD_M2<-data.frame(Rest_T_TOD, T_TOD)
TOD_M2<-rowMeans(TOD_M2)

#M1 Salivary Outcomes
#Create dataframe 
SAA.M1.df <- data.frame(SubjectID, TOD_M1, SAA_1_Rest_E, SAA_4_Rest_E, SAA_6_Rest_E, SAA_7_Rest_E, 
                        SAA_1_Exercise, SAA_4_Exercise, SAA_6_Exercise, SAA_7_Exercise)
#Melt dataframe into long format assumption testing  
longformat.SAA.M1.df <- melt(SAA.M1.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.SAA.M1.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Check for outliers 
longformat.SAA.M1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
SAA.M1.df %>%
  shapiro_test(SAA_1_Rest_E, SAA_4_Rest_E, SAA_6_Rest_E, SAA_7_Rest_E, 
               SAA_1_Exercise, SAA_4_Exercise, SAA_6_Exercise, SAA_7_Exercise)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.SAA.M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.longformat.SAA.M1.df.xlsx') 
RM.longformat.SAA.M1.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.longformat.SAA.M1.df.edited.xlsx")
View(RM.longformat.SAA.M1.df) 
#Computation
SAA.M1.aov<-anova_test(data = RM.longformat.SAA.M1.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint), type = 3, effect.size = "ges")
get_anova_table(SAA.M1.aov)
#PostHoc Computations
interaction.aov<- RM.longformat.SAA.M1.df %>%
  group_by(Intervention) %>%
  anova_test(dv = value, wid = SubjectID, within = Timepoint) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
interaction.aov

pairwise.aov <- RM.longformat.SAA.M1.df %>%
  group_by(Intervention) %>%
  pairwise_t_test(
    value ~ Timepoint, paired = TRUE,
    p.adjust.method = "bonferroni")
pairwise.aov

interaction.aov<- RM.longformat.SAA.M1.df %>%
  group_by(Timepoint) %>%
  anova_test(dv = value, wid = SubjectID, within = Intervention) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
interaction.aov

pairwise.aov <- RM.longformat.SAA.M1.df %>%
  group_by(Timepoint) %>%
  pairwise_t_test(
    value ~ Intervention, paired = TRUE,
    p.adjust.method = "bonferroni")
pairwise.aov
#Summarize for report 
RM.longformat.SAA.M1.df%>%
  group_by(Timepoint)%>%
  get_summary_stats(value, type = "mean_sd")

#Visualize
      #Load untransformed data, create and melt dataframe
      SAA.M1.df <- data.frame(SubjectID, SAA_1_Rest_E, SAA_4_Rest_E, SAA_6_Rest_E, SAA_7_Rest_E, 
                              SAA_1_Exercise, SAA_4_Exercise, SAA_6_Exercise, SAA_7_Exercise)
      #Export original data frame
      write_xlsx(SAA.M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.SAA.M1.df.xlsx')
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.SAA.M1.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.SAA.M1.df.edited.xlsx")
      #View to confirm correct format
      View(graph.SAA.M1.df.graph) 
      #Graph 
      ggplot(graph.SAA.M1.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("violetred1", "steelblue1"))+
        geom_point(size = 3, shape = 16)+
        theme_minimal()+
        labs(title="Salivary Alpha Amylase: Exercise vs. Rest", x ="Timepoint", y = "sAA (log10(U/mL))")+
        theme(plot.title = element_text(hjust = 0.5))+
        coord_cartesian(ylim=c(1.75, 2.4))


#M2 Salivary Outcomes
#Create dataframe 
SAA.M2.df <- data.frame(SubjectID, TOD_M2, SAA_1_Rest_T, SAA_4_Rest_T, SAA_6_Rest_T, SAA_7_Rest_T, 
                        SAA_1_Trier, SAA_4_Trier, SAA_6_Trier, SAA_7_Trier)
#Melt dataframe into long format assumption testing  
longformat.SAA.M2.df <- melt(SAA.M2.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.SAA.M2.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Check for outliers 
longformat.SAA.M2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
SAA.M2.df %>%
  shapiro_test(SAA_1_Rest_T, SAA_4_Rest_T, SAA_6_Rest_T, SAA_7_Rest_T, 
               SAA_1_Trier, SAA_4_Trier, SAA_6_Trier, SAA_7_Trier)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.SAA.M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.longformat.SAA.M2.df.xlsx') 
RM.longformat.SAA.M2.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.longformat.SAA.M2.df.edited.xlsx")
View(RM.longformat.SAA.M2.df) 
#Computation
SAA.M2.aov<-anova_test(data = RM.longformat.SAA.M2.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint), type = 3, effect.size = "pes")
get_anova_table(SAA.M2.aov)
#PostHoc Computations
interaction.aov<- RM.longformat.SAA.M2.df %>%
  group_by(Timepoint) %>%
  anova_test(dv = value, wid = SubjectID, within = Intervention) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
interaction.aov

pairwise.aov <- RM.longformat.SAA.M2.df %>%
  group_by(Timepoint) %>%
  pairwise_t_test(
    value ~ Intervention, paired = TRUE,
    p.adjust.method = "bonferroni")
pairwise.aov

interaction.aov<- RM.longformat.SAA.M2.df %>%
  group_by(Intervention) %>%
  anova_test(dv = value, wid = SubjectID, within = Timepoint) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
interaction.aov

pairwise.aov <- RM.longformat.SAA.M2.df %>%
  group_by(Intervention) %>%
  pairwise_t_test(
    value ~ Timepoint, paired = TRUE,
    p.adjust.method = "bonferroni")
pairwise.aov
#Summarize for report 
RM.longformat.SAA.M2.df%>%
  group_by(Timepoint)%>%
  get_summary_stats(value, type = "mean_sd")
#Visualize
    #Load untransformed data, create and melt dataframe
    SAA.M2.df <- data.frame(SubjectID, SAA_1_Rest_T, SAA_4_Rest_T, SAA_6_Rest_T, SAA_7_Rest_T, 
                            SAA_1_Trier, SAA_4_Trier, SAA_6_Trier, SAA_7_Trier)
    #Export original data frame
    write_xlsx(SAA.M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.SAA.M2.df.xlsx')
    #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
    graph.SAA.M2.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.SAA.M2.df.edited.xlsx")
    #View to confirm correct format
    View(graph.SAA.M2.df.graph) 
    #Graph 
    ggplot(graph.SAA.M2.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
      geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                    position=position_dodge(0.05)) +
      geom_line(linewidth = 1)+
      scale_color_manual(values=c("steelblue1", "violetred1"))+
      geom_point(size = 3, shape = 16)+
      theme_minimal()+
      labs(title="Salivary Alpha Amylase: Trier vs. Rest", x ="Timepoint", y = "sAA (log10(U/mL))")+
      theme(plot.title = element_text(hjust = 0.5))+
      coord_cartesian(ylim=c(1.75, 2.4))

# SALIVARY OUTCOMES - CORTISOL --------------------------------------------
#Call variables 
Cort_1_Rest_E <- log10(df$Cort_1_Rest_E)+2
Cort_4_Rest_E <- log10(df$Cort_4_Rest_E)+2
Cort_6_Rest_E <- log10(df$Cort_6_Rest_E)+2
Cort_7_Rest_E <- log10(df$Cort_7_Rest_E)+2
Cort_1_Exercise <- log10(df$Cort_1_Exercise)+2
Cort_4_Exercise <- log10(df$Cort_4_Exercise)+2
Cort_6_Exercise <- log10(df$Cort_6_Exercise)+2
Cort_7_Exercise <- log10(df$Cort_7_Exercise)+2
Cort_1_Rest_T <- log10(df$Cort_1_Rest_T)+2
Cort_4_Rest_T <- log10(df$Cort_4_Rest_T)+2
Cort_6_Rest_T <- log10(df$Cort_6_Rest_T)+2
Cort_7_Rest_T <- log10(df$Cort_7_Rest_T)+2
Cort_1_Trier <- log10(df$Cort_1_Trier)+2
Cort_4_Trier <- log10(df$Cort_4_Trier)+2
Cort_6_Trier <- log10(df$Cort_6_Trier)+2
Cort_7_Trier <- log10(df$Cort_7_Trier)+2

Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
TOD_M1<-data.frame(Rest_E_TOD, E_TOD)
TOD_M1<-rowMeans(TOD_M1)

Rest_T_TOD<-df$r_end_time_T
T_TOD<-df$start_time
TOD_M2<-data.frame(Rest_T_TOD, T_TOD)
TOD_M2<-rowMeans(TOD_M2)

SubjectID<-df$SubjectID

#M1 Salivary Outcomes
#Create dataframe 
CORT.M1.df <- data.frame(SubjectID, TOD_M1, Cort_1_Rest_E, Cort_4_Rest_E, Cort_6_Rest_E, Cort_7_Rest_E, 
                         Cort_1_Exercise, Cort_4_Exercise, Cort_6_Exercise, Cort_7_Exercise)
#Melt dataframe into long format assumption testing  
longformat.CORT.M1.df <- melt(CORT.M1.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.CORT.M1.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Check for outliers 
longformat.CORT.M1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
CORT.M1.df %>%
  shapiro_test(Cort_1_Rest_E, Cort_4_Rest_E, Cort_6_Rest_E, Cort_7_Rest_E, 
               Cort_1_Exercise, Cort_4_Exercise, Cort_6_Exercise, Cort_7_Exercise)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.CORT.M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.longformat.CORT.M1.df.xlsx') 
RM.longformat.CORT.M1.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.longformat.CORT.M1.df.edited.xlsx")
View(RM.longformat.CORT.M1.df) 
#Computation
CORT.M1.aov<-anova_test(data = RM.longformat.CORT.M1.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint), type = 3, effect.size = "pes", covariate = "TOD")
get_anova_table(CORT.M1.aov)
#PostHoc Computations
interaction.aov<- RM.longformat.CORT.M1.df %>%
  group_by(Intervention) %>%
  anova_test(dv = value, wid = SubjectID, within = Timepoint) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
interaction.aov

pairwise.aov <- RM.longformat.CORT.M1.df %>%
  group_by(Intervention) %>%
  pairwise_t_test(
    value ~ Timepoint, paired = TRUE,
    p.adjust.method = "bonferroni")
pairwise.aov
#Summarize for report
RM.longformat.CORT.M1.df%>%
  group_by(Timepoint)%>%
  get_summary_stats(value, type = "mean_sd")
#Visualize
      #Load untransformed data, create and melt dataframe
      CORT.M1.df <- data.frame(SubjectID, Cort_1_Rest_E, Cort_4_Rest_E, Cort_6_Rest_E, Cort_7_Rest_E, 
                               Cort_1_Exercise, Cort_4_Exercise, Cort_6_Exercise, Cort_7_Exercise)
      #Export original data frame
      write_xlsx(CORT.M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.CORT.M1.df.xlsx')
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.CORT.M1.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.CORT.M1.df.edited.xlsx")
      #View to confirm correct format
      View(graph.CORT.M1.df.graph) 
      #Graph 
      ggplot(graph.CORT.M1.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c( "violetred1", "steelblue1"))+
        geom_point(size = 3, shape = 16)+
        theme_minimal()+
        labs(title="Cortisol: Exercise vs. Rest", x ="Timepoint", y = "Cortisol (log(ug/dL))")+
        theme(plot.title = element_text(hjust = 0.5))+
        coord_cartesian(ylim=c(.75,2.5 ))

#M2 Salivary Outcomes
#Create dataframe 
CORT.M2.df <- data.frame(SubjectID, TOD_M2, Cort_1_Rest_T, Cort_4_Rest_T, Cort_6_Rest_T, Cort_7_Rest_T, 
                         Cort_1_Trier, Cort_4_Trier, Cort_6_Trier, Cort_7_Trier)
#Melt dataframe into long format assumption testing  
longformat.CORT.M2.df <- melt(CORT.M2.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.CORT.M2.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Check for outliers 
longformat.CORT.M2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
CORT.M2.df %>%
  shapiro_test(Cort_1_Rest_T, Cort_4_Rest_T, Cort_6_Rest_T, Cort_7_Rest_T, 
               Cort_1_Trier, Cort_4_Trier, Cort_6_Trier, Cort_7_Trier)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.CORT.M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.longformat.CORT.M2.df.xlsx') 
RM.longformat.CORT.M2.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.longformat.CORT.M2.df.edited.xlsx")
View(RM.longformat.CORT.M2.df) 
#Computation
CORT.M2.aov<-anova_test(data = RM.longformat.CORT.M2.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint), type = 3, effect.size = "ges", covariate = "TOD")
get_anova_table(CORT.M2.aov)
#PostHoc Computations
interaction.aov<- RM.longformat.CORT.M2.df %>%
  group_by(Timepoint) %>%
  anova_test(dv = value, wid = SubjectID, within = Intervention) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
interaction.aov

pairwise.aov <- RM.longformat.CORT.M2.df %>%
  group_by(Timepoint) %>%
  pairwise_t_test(
    value ~ Intervention, paired = TRUE,
    p.adjust.method = "bonferroni")
pairwise.aov

interaction.aov<- RM.longformat.CORT.M2.df %>%
  group_by(Intervention) %>%
  anova_test(dv = value, wid = SubjectID, within = Timepoint) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
interaction.aov

pairwise.aov <- RM.longformat.CORT.M2.df %>%
  group_by(Intervention) %>%
  pairwise_t_test(
    value ~ Timepoint, paired = TRUE,
    p.adjust.method = "bonferroni")
pairwise.aov
#Summarize for report
RM.longformat.CORT.M2.df%>%
  group_by(Timepoint)%>%
  get_summary_stats(value, type = "mean_sd")
#Visualize
#Load untransformed data, create and melt dataframe
CORT.M2.df <- data.frame(SubjectID, Cort_1_Rest_T, Cort_4_Rest_T, Cort_6_Rest_T, Cort_7_Rest_T, 
                         Cort_1_Trier, Cort_4_Trier, Cort_6_Trier, Cort_7_Trier)
#Export original data frame
write_xlsx(CORT.M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.CORT.M2.df.xlsx')
#Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
graph.CORT.M2.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.CORT.M2.df.edited.xlsx")
#View to confirm correct format
View(graph.CORT.M2.df.graph) 
#Graph 
ggplot(graph.CORT.M2.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                position=position_dodge(0.05)) +
  geom_line(linewidth = 1)+
  scale_color_manual(values=c("steelblue1","violetred1"))+
  geom_point(size = 3, shape = 16)+
  theme_minimal()+
  labs(title="Cortisol: Trier vs. Rest", x ="Timepoint", y = "Cortisol (log(ug/dL))")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(.75, 2.5))


# COGNITIVE OUTCOMES - Flanker Response Time --------------------------------------------------------------
#Call Variables
SubjectID <- (df$SubjectID)
Congruent_E_pre <- (df$ SAya_FL_Congruent_MeanRT_E_pre)
Congruent_Rest_E_pre <- (df$ SAya_FL_Congruent_MeanRT_Rest_E_pre)
Congruent_B_pre <- (df$ SAya_FL_Congruent_MeanRT_B_pre)
Incongruent_E_pre <- (df$ SAya_FL_Incongruent_MeanRT_E_pre)
Incongruent_Rest_E_pre <- (df$ SAya_FL_Incongruent_MeanRT_Rest_E_pre)
Incongruent_B_pre <- (df$ SAya_FL_Incongruent_MeanRT_B_pre)
Congruent_E_post <- (df$ SAya_FL_Congruent_MeanRT_E_post)
Congruent_Rest_E_post <- (df$ SAya_FL_Congruent_MeanRT_Rest_E_post)
Incongruent_E_post <- (df$ SAya_FL_Incongruent_MeanRT_E_post)
Incongruent_Rest_E_post <- (df$ SAya_FL_Incongruent_MeanRT_Rest_E_post)
Congruent_T_pre <- (df$ SAya_FL_Congruent_MeanRT_T_pre)
Congruent_Rest_T_pre <- (df$ SAya_FL_Congruent_MeanRT_Rest_T_pre)
Incongruent_T_pre <- (df$ SAya_FL_Incongruent_MeanRT_T_pre)
Incongruent_Rest_T_pre <- (df$ SAya_FL_Incongruent_MeanRT_Rest_T_pre)
Congruent_T_post <- (df$ SAya_FL_Congruent_MeanRT_T_post)
Congruent_Rest_T_post <- (df$ SAya_FL_Congruent_MeanRT_Rest_T_post)
Incongruent_T_post <- (df$ SAya_FL_Incongruent_MeanRT_T_post)
Incongruent_Rest_T_post <- (df$ SAya_FL_Incongruent_MeanRT_Rest_T_post)

#M1 All Conditions Computation 
#Create dataframe 
RT_M1.df<- data.frame(SubjectID, 
                      Congruent_Rest_E_pre, Incongruent_Rest_E_pre, Congruent_Rest_E_post, Incongruent_Rest_E_post, 
                      Congruent_E_pre, Incongruent_E_pre, Congruent_E_post, Incongruent_E_post)
#Melt dataframe into long format assumption testing  
longformat.RT_M1.df <- melt(RT_M1.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.RT_M1.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.RT_M1.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean Response Time: Exercise vs. Rest", x ="Intervention", y = "Response Time (ms)")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(250,510))
#Check for outliers 
longformat.RT_M1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
RT_M1.df %>%
  shapiro_test(Congruent_Rest_E_pre, Incongruent_Rest_E_pre, Congruent_Rest_E_post, Incongruent_Rest_E_post, 
               Congruent_E_pre, Incongruent_E_pre, Congruent_E_post, Incongruent_E_post)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.RT_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.RT_M1.df.xlsx') 
RM.RT_M1.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.RT_M1.df.edited.xlsx")
View(RM.RT_M1.df) 
#Computation
M1_RT.aov<-anova_test(data = RM.RT_M1.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint, Congruency), type = 3, effect.size = "ges")
get_anova_table(M1_RT.aov)
#PostHoc Computations
t.test(RM.RT_M1.df$value~RM.RT_M1.df$Congruency, paired = TRUE, p.adjust.method = "bonferroni")
#Summarize for report 
RM.RT_M1.df%>%
  group_by(Congruency)%>%
  get_summary_stats(value, type = "mean_sd")
#Visualize
      #Reload original data frame
      RT_M1.df<- data.frame(SubjectID, 
                      Congruent_Rest_E_pre, Incongruent_Rest_E_pre, Congruent_Rest_E_post, Incongruent_Rest_E_post, 
                      Congruent_E_pre, Incongruent_E_pre, Congruent_E_post, Incongruent_E_post)
      #Export original data frame
      write_xlsx(RT_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.RT_M1.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.RT_M1.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.RT_M1.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.RT_M1.df.graph) 
      #Graph 
      ggplot(graph.RT_M1.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
      geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                    position=position_dodge(0.05)) +
       geom_line(linewidth = 1)+
       scale_color_manual(values=c("violetred1", "steelblue1"))+
       geom_point(size = 2, shape = 16)+
      facet_wrap(~Congruency)+
      labs(title="Mean Response Time: Exercise vs. Rest", x ="Timepoint", y = "Mean RT (ms)")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Mean RT (ms)", limits=c(355, 450))
        
#M1 Difference in RT Exercise vs. Difference in Rest
#Create difference scores 
Diff_PrePost_R_Congruent_RT <- Congruent_Rest_E_post-Congruent_Rest_E_pre
Diff_PrePost_R_Incongruent_RT <- Incongruent_Rest_E_post-Incongruent_Rest_E_pre
Diff_PrePost_E_Congruent_RT <- Congruent_E_post-Incongruent_E_pre
Diff_PrePost_E_Incongruent_RT <- Incongruent_E_post-Incongruent_E_pre
#Create dataframe  
M1_RT_Diff.df <- data.frame(SubjectID, Diff_PrePost_R_Congruent_RT, Diff_PrePost_R_Incongruent_RT, 
                                        Diff_PrePost_E_Congruent_RT, Diff_PrePost_E_Incongruent_RT)
#Melt dataframe into long format assumption testing  
longformat.M1_RT_Diff.df <- melt(M1_RT_Diff.df, id = "SubjectID", variable.name = "Condition")     
#Summarize
summary(longformat.M1_RT_Diff.df)
#Visualize all conditions
ggplot(longformat.M1_RT_Diff.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean RT Difference Post-Pre for Model 1 Exercise vs. Rest", x ="Intervention", y = "Mean Response Time Difference (ms)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(-130, 55))      
#Check for outliers 
longformat.M1_RT_Diff.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
M1_RT_Diff.df %>%
  shapiro_test(Diff_PrePost_R_Congruent_RT, Diff_PrePost_R_Incongruent_RT, 
               Diff_PrePost_E_Congruent_RT, Diff_PrePost_E_Incongruent_RT)      
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.M1_RT_Diff.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.M1_RT_Diff.df.xlsx') 
RM.M1_RT_Diff.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.M1_RT_Diff.df.edited.xlsx") 
View(RM.M1_RT_Diff.df) 
#Computation
M1_RT_Diff.aov<-anova_test(data = RM.M1_RT_Diff.df, dv = value, wid = SubjectID, within =  c(Intervention, Congruency), type = 3, effect.size = "pes")
get_anova_table(M1_RT_Diff.aov)
#PostHoc Computations
t.test(RM.M1_RT_Diff.df$value~RM.M1_RT_Diff.df$Congruency, paired = TRUE, p.adjust.method = "bonferroni") 
t.test(RM.M1_RT_Diff.df$value~RM.M1_RT_Diff.df$Intervention, paired = TRUE, p.adjust.method = "bonferroni")


#M2 All Conditions Computation 
#Create dataframe 
RT_M2.df<- data.frame(SubjectID, 
                      Congruent_Rest_T_pre, Incongruent_Rest_T_pre, Congruent_Rest_T_post, Incongruent_Rest_T_post, 
                      Congruent_T_pre, Incongruent_T_pre, Congruent_T_post, Incongruent_T_post)
#Melt dataframe into long format assumption testing  
longformat.RT_M2.df <- melt(RT_M2.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.RT_M2.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.RT_M2.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean RT for Model 2Trier vs. Rest", x ="Intervention", y = "Response Time (ms)")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(250,625))
#Check for outliers 
longformat.RT_M2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
RT_M2.df %>%
  shapiro_test(Congruent_Rest_T_pre, Incongruent_Rest_T_pre, Congruent_Rest_T_post, Incongruent_Rest_T_post, 
               Congruent_T_pre, Incongruent_T_pre, Congruent_T_post, Incongruent_T_post)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.RT_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.RT_M2.df.xlsx') 
RM.RT_M2.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.RT_M2.df.edited.xlsx")
View(RM.RT_M2.df) 
#Computation
M2_RT.aov<-anova_test(data = RM.RT_M2.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint, Congruency), type = 3, effect.size = "ges")
get_anova_table(M2_RT.aov)
#PostHoc Computations
t.test(RM.RT_M2.df$value~RM.RT_M2.df$Congruency, paired = TRUE, p.adjust.method = "bonferroni")
t.test(RM.RT_M2.df$value~RM.RT_M2.df$Timepoint, paired = TRUE, p.adjust.method = "bonferroni")
#Summarize for report
RM.RT_M2.df%>%
  group_by(Timepoint)%>%
  get_summary_stats(value, type = "mean_sd")

RM.RT_M2.df%>%
  group_by(Congruency)%>%
  get_summary_stats(value, type = "mean_sd")


interpret_eta_squared(.003)
#Visualize
      #Reload original data frame
      RT_M2.df<- data.frame(SubjectID, 
                            Congruent_Rest_T_pre, Incongruent_Rest_T_pre, Congruent_Rest_T_post, Incongruent_Rest_T_post, 
                            Congruent_T_pre, Incongruent_T_pre, Congruent_T_post, Incongruent_T_post)
      #Export original data frame
      write_xlsx(RT_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.RT_M2.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.RT_M2.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.RT_M2.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.RT_M2.df.graph) 
      #Graph 
      ggplot(graph.RT_M2.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("steelblue1", "violetred1"))+
        geom_point(size = 2, shape = 16)+
        facet_wrap(~Congruency)+
        facet_wrap(~Congruency)+
        labs(title="Mean Response Time: Trier vs. Rest", x ="Timepoint", y = "Mean RT (ms)")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Mean RT (ms)", limits=c(355, 450))

#M2 Difference in RT Trier vs. Difference in Rest
#Create difference scores 
Diff_PrePost_R_Congruent_RT <- Congruent_Rest_T_post-Congruent_Rest_T_pre
Diff_PrePost_R_Incongruent_RT <- Incongruent_Rest_T_post-Incongruent_Rest_T_pre
Diff_PrePost_T_Congruent_RT <- Congruent_T_post-Incongruent_T_pre
Diff_PrePost_T_Incongruent_RT <- Incongruent_T_post-Incongruent_T_pre
#Create dataframe  
M2_RT_Diff.df <- data.frame(SubjectID, Diff_PrePost_R_Congruent_RT, Diff_PrePost_R_Incongruent_RT, 
                            Diff_PrePost_T_Congruent_RT, Diff_PrePost_T_Incongruent_RT)
#Melt dataframe into long format assumption testing  
longformat.M2_RT_Diff.df <- melt(M2_RT_Diff.df, id = "SubjectID", variable.name = "Condition")     
#Summarize
summary(longformat.M2_RT_Diff.df)
#Visualize all conditions
ggplot(longformat.M2_RT_Diff.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean RT Difference Post-Pre for Model 2Trier vs. Rest", x ="Intervention", y = "Mean Response Time Difference (ms)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(-130, 55))      
#Check for outliers 
longformat.M2_RT_Diff.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
M2_RT_Diff.df %>%
  shapiro_test(Diff_PrePost_R_Congruent_RT, Diff_PrePost_R_Incongruent_RT, 
               Diff_PrePost_T_Congruent_RT, Diff_PrePost_T_Incongruent_RT)      
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.M2_RT_Diff.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.M2_RT_Diff.df.xlsx') 
RM.M2_RT_Diff.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.M2_RT_Diff.df.edited.xlsx") 
View(RM.M2_RT_Diff.df) 
#Computation
M2_RT_Diff.aov<-anova_test(data = RM.M2_RT_Diff.df, dv = value, wid = SubjectID, within =  c(Intervention, Congruency), type = 3, effect.size = "pes")
get_anova_table(M2_RT_Diff.aov)
#PostHoc Computations
t.test(RM.M2_RT_Diff.df$value~RM.M2_RT_Diff.df$Congruency, paired = TRUE, p.adjust.method = "bonferroni")     
t.test(RM.M2_RT_Diff.df$value~RM.M2_RT_Diff.df$Timepoint, paired = TRUE, p.adjust.method = "bonferroni")     


# COGNITIVE OUTCOMES - Flanker Response Accuracy --------------------------
#Call Variables
SubjectID <- (df$SubjectID)
Congruent_E_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_pre)
Congruent_Rest_E_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre)
Congruent_B_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_B_pre)
Incongruent_E_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_pre)
Incongruent_Rest_E_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre)
Incongruent_B_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_B_pre)
Congruent_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_post)
Congruent_Rest_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_post)
Incongruent_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_post)
Incongruent_Rest_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
Congruent_T_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_T_pre)
Congruent_Rest_T_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_T_pre)
Incongruent_T_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_T_pre)
Incongruent_Rest_T_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_T_pre)
Congruent_T_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_T_post)
Congruent_Rest_T_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_T_post)
Incongruent_T_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_T_post)
Incongruent_Rest_T_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_T_post)

#M1 All Conditions Computation 
#Create dataframe 
ACC_M1.df<- data.frame(SubjectID, 
                       Congruent_Rest_E_pre, Incongruent_Rest_E_pre, Congruent_Rest_E_post, Incongruent_Rest_E_post, 
                       Congruent_E_pre, Incongruent_E_pre, Congruent_E_post, Incongruent_E_post)
#Melt dataframe into long format assumption testing  
longformat.ACC_M1.df <- melt(ACC_M1.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.ACC_M1.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.ACC_M1.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "jitter", alpha = 0.5)+
  labs(title="Mean ACC for Model 1 Exercise vs. Rest", x ="Intervention", y = "Response Time (ms)")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(60,100))
#Check for outliers 
longformat.ACC_M1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
ACC_M1.df %>%
  shapiro_test(Congruent_Rest_E_pre, Incongruent_Rest_E_pre, Congruent_Rest_E_post, Incongruent_Rest_E_post, 
               Congruent_E_pre, Incongruent_E_pre, Congruent_E_post, Incongruent_E_post)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.ACC_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.ACC_M1.df.xlsx') 
RM.ACC_M1.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.ACC_M1.df.edited.xlsx")
View(RM.ACC_M1.df) 
#Computation
#nonparametric test on SPSS
ggwithinstats(data = longformat.ACC_M1.df, x = Condition, y = value, type = "nonparametric", p.adjust.method = "bonferroni")
#PostHoc Computations
#nonparametric test on SPSS
#Summarize for report
RM.ACC_M1.df%>%
  group_by(Congruency)%>%
  get_summary_stats(value, type = "mean_sd")

interpret_kendalls_w(.64)
#Visualize
      #Reload original data frame
      ACC_M1.df<- data.frame(SubjectID, 
                             Congruent_Rest_E_pre, Incongruent_Rest_E_pre, Congruent_Rest_E_post, Incongruent_Rest_E_post, 
                             Congruent_E_pre, Incongruent_E_pre, Congruent_E_post, Incongruent_E_post)
      #Export original data frame
      write_xlsx(ACC_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.ACC_M1.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.ACC_M1.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.ACC_M1.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.ACC_M1.df.graph) 
      #Graph 
      ggplot(graph.ACC_M1.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("violetred1", "steelblue1"))+
        geom_point(size = 2, shape = 16)+
        facet_wrap(~Congruency)+
        labs(title="Mean Accuracy: Exercise vs. Rest", x ="Timepoint", y = "Mean ACC (%)")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Mean Accuracy (%)", limits=c(87, 100))

#M1 Difference in ACC Exercise vs. Difference in Rest
#Create difference scores 
Diff_PrePost_R_Congruent_ACC <- Congruent_Rest_E_post-Congruent_Rest_E_pre
Diff_PrePost_R_Incongruent_ACC <- Incongruent_Rest_E_post-Incongruent_Rest_E_pre
Diff_PrePost_E_Congruent_ACC <- Congruent_E_post-Incongruent_E_pre
Diff_PrePost_E_Incongruent_ACC <- Incongruent_E_post-Incongruent_E_pre
#Create dataframe  
M1_ACC_Diff.df <- data.frame(SubjectID, Diff_PrePost_R_Congruent_ACC, Diff_PrePost_R_Incongruent_ACC, 
                             Diff_PrePost_E_Congruent_ACC, Diff_PrePost_E_Incongruent_ACC)
#Melt dataframe into long format assumption testing  
longformat.M1_ACC_Diff.df <- melt(M1_ACC_Diff.df, id = "SubjectID", variable.name = "Condition")     
#Summarize
summary(longformat.M1_ACC_Diff.df)
#Visualize all conditions
ggplot(longformat.M1_ACC_Diff.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean ACC Difference Post-Pre for Model 1 Exercise vs. Rest", x ="Intervention", y = "Mean Response Time Difference (ms)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(-13, 22))      
#Check for outliers 
longformat.M1_ACC_Diff.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
M1_ACC_Diff.df %>%
  shapiro_test(Diff_PrePost_R_Congruent_ACC, Diff_PrePost_R_Incongruent_ACC, 
               Diff_PrePost_E_Congruent_ACC, Diff_PrePost_E_Incongruent_ACC)      
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.M1_ACC_Diff.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.M1_ACC_Diff.df.xlsx') 
RM.M1_ACC_Diff.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.M1_ACC_Diff.df.edited.xlsx") 
View(RM.M1_ACC_Diff.df) 
#Computation
M1_ACC_Diff.aov<-anova_test(data = RM.M1_ACC_Diff.df, dv = value, wid = SubjectID, within =  c(Intervention, Congruency), type = 3, effect.size = "pes")
get_anova_table(M1_ACC_Diff.aov)
#PostHoc Computations
t.test(RM.M1_ACC_Diff.df$value~RM.M1_ACC_Diff.df$Congruency, paired = TRUE, p.adjust.method = "bonferroni") 
t.test(RM.M1_ACC_Diff.df$value~RM.M1_ACC_Diff.df$Intervention, paired = TRUE, p.adjust.method = "bonferroni")     


#M2 All Conditions Computation 
#Create dataframe 
ACC_M2.df<- data.frame(SubjectID, 
                       Congruent_Rest_T_pre, Incongruent_Rest_T_pre, Congruent_Rest_T_post, Incongruent_Rest_T_post, 
                       Congruent_T_pre, Incongruent_T_pre, Congruent_T_post, Incongruent_T_post)
#Melt dataframe into long format assumption testing  
longformat.ACC_M2.df <- melt(ACC_M2.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.ACC_M2.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.ACC_M2.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "jitter", alpha = 0.5)+
  labs(title="Mean ACC for Model 2 Trier vs. Rest", x ="Intervention", y = "Response Time (ms)")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(60,100))
#Check for outliers 
longformat.ACC_M2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
ACC_M2.df %>%
  shapiro_test(Congruent_Rest_T_pre, Incongruent_Rest_T_pre, Congruent_Rest_T_post, Incongruent_Rest_T_post, 
               Congruent_T_pre, Incongruent_T_pre, Congruent_T_post, Incongruent_T_post)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.ACC_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.ACC_M2.df.xlsx') 
RM.ACC_M2.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.ACC_M2.df.edited.xlsx")
View(RM.ACC_M2.df) 
#Computation
#on SPSS
ggwithinstats(data = longformat.ACC_M2.df, x = Condition, y = value, type = "nonparametric", p.adjust.method = "bonferroni")
#PostHoc Computations
#on SPSS
#Summarize for report
RM.ACC_M2.df%>%
  group_by(Congruency)%>%
  get_summary_stats(value, type = "mean_sd")

interpret_kendalls_w(.61)
#Visualize
      #Reload original data frame
      ACC_M2.df<- data.frame(SubjectID, 
                             Congruent_Rest_T_pre, Incongruent_Rest_T_pre, Congruent_Rest_T_post, Incongruent_Rest_T_post, 
                             Congruent_T_pre, Incongruent_T_pre, Congruent_T_post, Incongruent_T_post)
      #Export original data frame
      write_xlsx(ACC_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.ACC_M2.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.ACC_M2.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.ACC_M2.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.ACC_M2.df.graph) 
      #Graph 
      ggplot(graph.ACC_M2.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("steelblue1", "violetred1"))+
        geom_point(size = 2, shape = 16)+
        facet_wrap(~Congruency)+
        labs(title="Mean Accuracy: Trier vs. Rest", x ="Timepoint", y = "Mean ACC (%)")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Mean Accuracy (%)", limits=c(87, 100))

#M2 Difference in ACC Trier vs. Difference in Rest
#Create difference scores 
Diff_PrePost_R_Congruent_ACC <- Congruent_Rest_T_post-Congruent_Rest_T_pre
Diff_PrePost_R_Incongruent_ACC <- Incongruent_Rest_T_post-Incongruent_Rest_T_pre
Diff_PrePost_T_Congruent_ACC <- Congruent_T_post-Incongruent_T_pre
Diff_PrePost_T_Incongruent_ACC <- Incongruent_T_post-Incongruent_T_pre
#Create dataframe  
M2_ACC_Diff.df <- data.frame(SubjectID, Diff_PrePost_R_Congruent_ACC, Diff_PrePost_R_Incongruent_ACC, 
                             Diff_PrePost_T_Congruent_ACC, Diff_PrePost_T_Incongruent_ACC)
#Melt dataframe into long format assumption testing  
longformat.M2_ACC_Diff.df <- melt(M2_ACC_Diff.df, id = "SubjectID", variable.name = "Condition")     
#Summarize
summary(longformat.M2_ACC_Diff.df)
#Visualize all conditions
ggplot(longformat.M2_ACC_Diff.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean ACC Difference Post-Pre for Model 2 Trier vs. Rest", x ="Intervention", y = "Mean Response Time Difference (ms)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(-13, 22))      
#Check for outliers 
longformat.M2_ACC_Diff.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
M2_ACC_Diff.df %>%
  shapiro_test(Diff_PrePost_R_Congruent_ACC, Diff_PrePost_R_Incongruent_ACC, 
               Diff_PrePost_T_Congruent_ACC, Diff_PrePost_T_Incongruent_ACC)      
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.M2_ACC_Diff.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.M2_ACC_Diff.df.xlsx') 
RM.M2_ACC_Diff.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.M2_ACC_Diff.df.edited.xlsx") 
View(RM.M2_ACC_Diff.df) 
#Computation
M2_ACC_Diff.aov<-anova_test(data = RM.M2_ACC_Diff.df, dv = value, wid = SubjectID, within =  c(Intervention, Congruency), type = 3, effect.size = "pes")
get_anova_table(M2_ACC_Diff.aov)
#PostHoc Computations
t.test(RM.M2_ACC_Diff.df$value~RM.M2_ACC_Diff.df$Congruency, paired = TRUE, p.adjust.method = "bonferroni")     
t.test(RM.M2_ACC_Diff.df$value~RM.M2_ACC_Diff.df$Intervention, paired = TRUE, p.adjust.method = "bonferroni")       


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
Sex<-df$b_vo2_sex

#Baseline congruent Flanker RT related to fitness?
summary(lm(SAya_FL_Congruent_MeanRT_B_pre~VO2 + Sex))
ggscatterstats(df, x=b_vo2_value, y=SAya_FL_Congruent_MeanRT_B_pre)

#Baseline incongruent Flanker RT related to fitness?
summary(lm(SAya_FL_Incongruent_MeanRT_B_pre~VO2+ Sex))
ggscatterstats(df, x=b_vo2_value, y=SAya_FL_Incongruent_MeanRT_B_pre)

#Baseline congruent flanker ACC realted to fitness? 
summary(lm(SAya_FL_Congruent_ResponseAccuracy_B_pre~VO2+ Sex))
ggscatterstats(df, x=b_vo2_value, y=SAya_FL_Congruent_ResponseAccuracy_B_pre)

#Baseline incongruent flanker ACC related to fitness? 
summary(lm(SAya_FL_Incongruent_ResponseAccuracy_B_pre~VO2+ Sex))
ggscatterstats(df, x=b_vo2_value, y=SAya_FL_Incongruent_ResponseAccuracy_B_pre)

#Baseline congruent Flanker IE related to fitness?
summary(lm(SAya_FL_Congruent_InverseEfficiency_B_pre~VO2+ Sex))
ggscatterstats(df, x=b_vo2_value, y=SAya_FL_Congruent_InverseEfficiency_B_pre)

#Baseline incongruent Flanker IE related to fitness?
summary(lm(SAya_FL_Incongruent_InverseEfficiency_B_pre~VO2+ Sex))
ggscatterstats(df, x=b_vo2_value, y=SAya_FL_Incongruent_InverseEfficiency_B_pre)

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

#M1 in P3 Latency between Exercise vs. Rest
#Average together ROI sites
Narrow.ROI_FC_Rest_E_pre_P3_Con_Latency.df <-data.frame(FC_Rest_E_pre_P3_Con_CPZ_Latency, FC_Rest_E_pre_P3_Con_CP1_Latency, FC_Rest_E_pre_P3_Con_CP2_Latency)
Congruent_Rest_E_pre <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Con_Latency.df)
Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Latency, FC_Rest_E_po_P3_Con_CP1_Latency, FC_Rest_E_po_P3_Con_CP2_Latency)
Congruent_Rest_E_post <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.df)
Narrow.ROI_FC_Rest_E_pre_P3_Inc_Latency.df <-data.frame(FC_Rest_E_pre_P3_Inc_CPZ_Latency, FC_Rest_E_pre_P3_Inc_CP1_Latency, FC_Rest_E_pre_P3_Inc_CP2_Latency)
Incongruent_Rest_E_pre <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Inc_Latency.df)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.df <-data.frame(FC_Rest_E_po_P3_Inc_CPZ_Latency, FC_Rest_E_po_P3_Inc_CP1_Latency, FC_Rest_E_po_P3_Inc_CP2_Latency)
Incongruent_Rest_E_post<- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.df)
Narrow.ROI_FC_E_pre_P3_Con_Latency.df <-data.frame(FC_E_pre_P3_Con_CPZ_Latency, FC_E_pre_P3_Con_CP1_Latency, FC_E_pre_P3_Con_CP2_Latency)
Congruent_E_pre <- rowMeans(Narrow.ROI_FC_E_pre_P3_Con_Latency.df)
Narrow.ROI_FC_E_po_P3_Con_Latency.df <-data.frame(FC_E_po_P3_Con_CPZ_Latency, FC_E_po_P3_Con_CP1_Latency, FC_E_po_P3_Con_CP2_Latency)
Congruent_E_post <- rowMeans(Narrow.ROI_FC_E_po_P3_Con_Latency.df)
Narrow.ROI_FC_E_pre_P3_Inc_Latency.df <-data.frame(FC_E_pre_P3_Inc_CPZ_Latency, FC_E_pre_P3_Inc_CP1_Latency, FC_E_pre_P3_Inc_CP2_Latency)
Incongruent_E_pre <- rowMeans(Narrow.ROI_FC_E_pre_P3_Inc_Latency.df)
Narrow.ROI_FC_E_po_P3_Inc_Latency.df <-data.frame(FC_E_po_P3_Inc_CPZ_Latency, FC_E_po_P3_Inc_CP1_Latency, FC_E_po_P3_Inc_CP2_Latency)
Incongruent_E_post <- rowMeans(Narrow.ROI_FC_E_po_P3_Inc_Latency.df)

#M1 All Conditions Computation 
#Create dataframe 
P3_Latency_M1.df<- data.frame(SubjectID, 
                              Congruent_Rest_E_pre, Incongruent_Rest_E_pre, Congruent_Rest_E_post, Incongruent_Rest_E_post, 
                              Congruent_E_pre, Incongruent_E_pre, Congruent_E_post, Incongruent_E_post)
#Melt dataframe into long format assumption testing  
longformat.P3_Latency_M1.df <- melt(P3_Latency_M1.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.P3_Latency_M1.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.P3_Latency_M1.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean P3_Latency for Model 1 Exercise vs. Rest", x ="Intervention", y = "P3 Latency (ms)")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(250,600))
#Check for outliers 
longformat.P3_Latency_M1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
P3_Latency_M1.df %>%
  shapiro_test(Congruent_Rest_E_pre, Incongruent_Rest_E_pre, Congruent_Rest_E_post, Incongruent_Rest_E_post, 
               Congruent_E_pre, Incongruent_E_pre, Congruent_E_post, Incongruent_E_post)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.P3_Latency_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.P3_Latency_M1.df.xlsx') 
RM.P3_Latency_M1.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.P3_Latency_M1.df.edited.xlsx")
View(RM.P3_Latency_M1.df) 
#Computation
M1_P3_Latency.aov<-anova_test(data = RM.P3_Latency_M1.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint, Congruency), type = 3, effect.size = "ges")
get_anova_table(M1_P3_Latency.aov)
#PostHoc Computations
t.test(RM.P3_Latency_M1.df$value~RM.P3_Latency_M1.df$Congruency, paired = TRUE, p.adjust.method = "bonferroni")
#Summarize for report 
RM.P3_Latency_M1.df%>%
  group_by(Congruency)%>%
  get_summary_stats(value, type = "mean_sd")
#Visualize
      #Reload original data frame
      P3_Latency_M1.df<- data.frame(SubjectID, 
                                    Congruent_Rest_E_pre, Incongruent_Rest_E_pre, Congruent_Rest_E_post, Incongruent_Rest_E_post, 
                                    Congruent_E_pre, Incongruent_E_pre, Congruent_E_post, Incongruent_E_post)
      #Export original data frame
      write_xlsx(P3_Latency_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.P3_Latency_M1.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.P3_Latency_M1.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.P3_Latency_M1.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.P3_Latency_M1.df.graph) 
      #Graph 
      ggplot(graph.P3_Latency_M1.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("violetred1", "steelblue1"))+
        geom_point(size = 2, shape = 16)+
        facet_wrap(~Congruency)+
        labs(title="Mean P3 Latency: Exercise vs. Rest", x ="Timepoint", y = "Mean P3_Latency (ms)")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Mean Latency (ms)", limits=c(340, 421))

#M1 Difference in P3_Latency Exercise vs. Difference in Rest
#Create difference scores 
Diff_PrePost_R_Congruent_P3_Latency <- Congruent_Rest_E_post-Congruent_Rest_E_pre
Diff_PrePost_R_Incongruent_P3_Latency <- Incongruent_Rest_E_post-Incongruent_Rest_E_pre
Diff_PrePost_E_Congruent_P3_Latency <- Congruent_E_post-Incongruent_E_pre
Diff_PrePost_E_Incongruent_P3_Latency <- Incongruent_E_post-Incongruent_E_pre
#Create dataframe  
M1_P3_Latency_Diff.df <- data.frame(SubjectID, Diff_PrePost_R_Congruent_P3_Latency, Diff_PrePost_R_Incongruent_P3_Latency, 
                                    Diff_PrePost_E_Congruent_P3_Latency, Diff_PrePost_E_Incongruent_P3_Latency)
#Melt dataframe into long format assumption testing  
longformat.M1_P3_Latency_Diff.df <- melt(M1_P3_Latency_Diff.df, id = "SubjectID", variable.name = "Condition")     
#Summarize
summary(longformat.M1_P3_Latency_Diff.df)
#Visualize all conditions
ggplot(longformat.M1_P3_Latency_Diff.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean P3 Latency Difference Post-Pre: Exercise vs. Rest", x ="Intervention", y = "Mean P3 Latency Difference (ms)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(-130, 55))      
#Check for outliers 
longformat.M1_P3_Latency_Diff.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
M1_P3_Latency_Diff.df %>%
  shapiro_test(Diff_PrePost_R_Congruent_P3_Latency, Diff_PrePost_R_Incongruent_P3_Latency, 
               Diff_PrePost_E_Congruent_P3_Latency, Diff_PrePost_E_Incongruent_P3_Latency)      
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.M1_P3_Latency_Diff.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.M1_P3_Latency_Diff.df.xlsx') 
RM.M1_P3_Latency_Diff.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.M1_P3_Latency_Diff.df.edited.xlsx") 
View(RM.M1_P3_Latency_Diff.df) 
#Computation
M1_P3_Latency_Diff.aov<-anova_test(data = RM.M1_P3_Latency_Diff.df, dv = value, wid = SubjectID, within =  c(Intervention, Congruency), type = 3, effect.size = "pes")
get_anova_table(M1_P3_Latency_Diff.aov)
#PostHoc Computations
t.test(RM.M1_P3_Latency_Diff.df$value~RM.M1_P3_Latency_Diff.df$Congruency, paired = TRUE, p.adjust.method = "bonferroni")     
t.test(RM.M1_P3_Latency_Diff.df$value~RM.M1_P3_Latency_Diff.df$Intervention, paired = TRUE, p.adjust.method = "bonferroni")      


#M2 in P3 Latency between Trier vs. Rest
#Average together ROI sites
Narrow.ROI_FC_Rest_T_pre_P3_Con_Latency.df <-data.frame(FC_Rest_T_pre_P3_Con_CPZ_Latency, FC_Rest_T_pre_P3_Con_CP1_Latency, FC_Rest_T_pre_P3_Con_CP2_Latency)
Congruent_Rest_T_pre <- rowMeans(Narrow.ROI_FC_Rest_T_pre_P3_Con_Latency.df)
Narrow.ROI_FC_Rest_T_po_P3_Con_Latency.df <-data.frame(FC_Rest_T_po_P3_Con_CPZ_Latency, FC_Rest_T_po_P3_Con_CP1_Latency, FC_Rest_T_po_P3_Con_CP2_Latency)
Congruent_Rest_T_post <- rowMeans(Narrow.ROI_FC_Rest_T_po_P3_Con_Latency.df)
Narrow.ROI_FC_Rest_T_pre_P3_Inc_Latency.df <-data.frame(FC_Rest_T_pre_P3_Inc_CPZ_Latency, FC_Rest_T_pre_P3_Inc_CP1_Latency, FC_Rest_T_pre_P3_Inc_CP2_Latency)
Incongruent_Rest_T_pre <- rowMeans(Narrow.ROI_FC_Rest_T_pre_P3_Inc_Latency.df)
Narrow.ROI_FC_Rest_T_po_P3_Inc_Latency.df <-data.frame(FC_Rest_T_po_P3_Inc_CPZ_Latency, FC_Rest_T_po_P3_Inc_CP1_Latency, FC_Rest_T_po_P3_Inc_CP2_Latency)
Incongruent_Rest_T_post<- rowMeans(Narrow.ROI_FC_Rest_T_po_P3_Inc_Latency.df)
Narrow.ROI_FC_T_pre_P3_Con_Latency.df <-data.frame(FC_T_pre_P3_Con_CPZ_Latency, FC_T_pre_P3_Con_CP1_Latency, FC_T_pre_P3_Con_CP2_Latency)
Congruent_T_pre <- rowMeans(Narrow.ROI_FC_T_pre_P3_Con_Latency.df)
Narrow.ROI_FC_T_po_P3_Con_Latency.df <-data.frame(FC_T_po_P3_Con_CPZ_Latency, FC_T_po_P3_Con_CP1_Latency, FC_T_po_P3_Con_CP2_Latency)
Congruent_T_post <- rowMeans(Narrow.ROI_FC_T_po_P3_Con_Latency.df)
Narrow.ROI_FC_T_pre_P3_Inc_Latency.df <-data.frame(FC_T_pre_P3_Inc_CPZ_Latency, FC_T_pre_P3_Inc_CP1_Latency, FC_T_pre_P3_Inc_CP2_Latency)
Incongruent_T_pre <- rowMeans(Narrow.ROI_FC_T_pre_P3_Inc_Latency.df)
Narrow.ROI_FC_T_po_P3_Inc_Latency.df <-data.frame(FC_T_po_P3_Inc_CPZ_Latency, FC_T_po_P3_Inc_CP1_Latency, FC_T_po_P3_Inc_CP2_Latency)
Incongruent_T_post <- rowMeans(Narrow.ROI_FC_T_po_P3_Inc_Latency.df)

#M2 All Conditions Computation 
#Create dataframe 
P3_Latency_M2.df<- data.frame(SubjectID, 
                              Congruent_Rest_T_pre, Incongruent_Rest_T_pre, Congruent_Rest_T_post, Incongruent_Rest_T_post, 
                              Congruent_T_pre, Incongruent_T_pre, Congruent_T_post, Incongruent_T_post)
#Melt dataframe into long format assumption testing  
longformat.P3_Latency_M2.df <- melt(P3_Latency_M2.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.P3_Latency_M2.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.P3_Latency_M2.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean P3_Latency for Model 2 Trier vs. Rest", x ="Intervention", y = "P3 Latency (ms)")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(250,600))
#Check for outliers 
longformat.P3_Latency_M2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
P3_Latency_M2.df %>%
  shapiro_test(Congruent_Rest_T_pre, Incongruent_Rest_T_pre, Congruent_Rest_T_post, Incongruent_Rest_T_post, 
               Congruent_T_pre, Incongruent_T_pre, Congruent_T_post, Incongruent_T_post)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.P3_Latency_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.P3_Latency_M2.df.xlsx') 
RM.P3_Latency_M2.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.P3_Latency_M2.df.edited.xlsx")
View(RM.P3_Latency_M2.df) 
#Computation
M2_P3_Latency.aov<-anova_test(data = RM.P3_Latency_M2.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint, Congruency), type = 3, effect.size = "ges")
get_anova_table(M2_P3_Latency.aov)
#PostHoc Computations
t.test(RM.P3_Latency_M2.df$value~RM.P3_Latency_M2.df$Congruency, paired = TRUE, p.adjust.method = "bonferroni") #main effect congruency 
#Summarize for report 
RM.P3_Latency_M2.df%>%
  group_by(Congruency)%>%
  get_summary_stats(value, type = "mean_sd")
#Visualize
      #Reload original data frame
      P3_Latency_M2.df<- data.frame(SubjectID, 
                                    Congruent_Rest_T_pre, Incongruent_Rest_T_pre, Congruent_Rest_T_post, Incongruent_Rest_T_post, 
                                    Congruent_T_pre, Incongruent_T_pre, Congruent_T_post, Incongruent_T_post)
      #Export original data frame
      write_xlsx(P3_Latency_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.P3_Latency_M2.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.P3_Latency_M2.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.P3_Latency_M2.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.P3_Latency_M2.df.graph) 
      #Graph 
      ggplot(graph.P3_Latency_M2.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c( "steelblue1", "violetred1"))+
        geom_point(size = 2, shape = 16)+
        facet_wrap(~Congruency)+
        labs(title="Mean P3 Latency: Trier vs. Rest", x ="Timepoint", y = "Mean P3_Latency (ms)")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Mean Latency (ms)", limits=c(340, 421))
        

#M2 Difference in P3_Latency Trier vs. Difference in Rest
#Create difference scores 
Diff_PrePost_R_Congruent_P3_Latency <- Congruent_Rest_T_post-Congruent_Rest_T_pre
Diff_PrePost_R_Incongruent_P3_Latency <- Incongruent_Rest_T_post-Incongruent_Rest_T_pre
Diff_PrePost_T_Congruent_P3_Latency <- Congruent_T_post-Incongruent_T_pre
Diff_PrePost_T_Incongruent_P3_Latency <- Incongruent_T_post-Incongruent_T_pre
#Create dataframe  
M2_P3_Latency_Diff.df <- data.frame(SubjectID, Diff_PrePost_R_Congruent_P3_Latency, Diff_PrePost_R_Incongruent_P3_Latency, 
                                    Diff_PrePost_T_Congruent_P3_Latency, Diff_PrePost_T_Incongruent_P3_Latency)
#Melt dataframe into long format assumption testing  
longformat.M2_P3_Latency_Diff.df <- melt(M2_P3_Latency_Diff.df, id = "SubjectID", variable.name = "Condition")     
#Summarize
summary(longformat.M2_P3_Latency_Diff.df)
#Visualize all conditions
ggplot(longformat.M2_P3_Latency_Diff.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean P3 Latency Difference Post-Pre for Model 2 Trier vs. Rest", x ="Intervention", y = "Mean P3 Latency Difference (ms)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(-130, 90))      
#Check for outliers 
longformat.M2_P3_Latency_Diff.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
M2_P3_Latency_Diff.df %>%
  shapiro_test(Diff_PrePost_R_Congruent_P3_Latency, Diff_PrePost_R_Incongruent_P3_Latency, 
               Diff_PrePost_T_Congruent_P3_Latency, Diff_PrePost_T_Incongruent_P3_Latency)      
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.M2_P3_Latency_Diff.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.M2_P3_Latency_Diff.df.xlsx') 
RM.M2_P3_Latency_Diff.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.M2_P3_Latency_Diff.df.edited.xlsx") 
View(RM.M2_P3_Latency_Diff.df) 
#Computation
M2_P3_Latency_Diff.aov<-anova_test(data = RM.M2_P3_Latency_Diff.df, dv = value, wid = SubjectID, within =  c(Intervention, Congruency), type = 3, effect.size = "pes")
get_anova_table(M2_P3_Latency_Diff.aov)
#PostHoc Computations
t.test(RM.M2_P3_Latency_Diff.df$value~RM.M2_P3_Latency_Diff.df$Congruency, paired = TRUE, p.adjust.method = "bonferroni")     
t.test(RM.M2_P3_Latency_Diff.df$value~RM.M2_P3_Latency_Diff.df$Intervention, paired = TRUE, p.adjust.method = "bonferroni")       


# BRAIN FUNCTION OUTCOMES - Flanker P3 Amplitude ------------------------------------
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

#M1 in P3 Amplitude between Exercise vs. Rest
#Average together ROI sites
Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_pre_P3_Con_CPZ_Amplitude, FC_Rest_E_pre_P3_Con_CP1_Amplitude, FC_Rest_E_pre_P3_Con_CP2_Amplitude)
Congruent_Rest_E_pre <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.df)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Amplitude, FC_Rest_E_po_P3_Con_CP1_Amplitude, FC_Rest_E_po_P3_Con_CP2_Amplitude)
Congruent_Rest_E_post <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df)
Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.df <-data.frame(FC_Rest_E_pre_P3_Inc_CPZ_Amplitude, FC_Rest_E_pre_P3_Inc_CP1_Amplitude, FC_Rest_E_pre_P3_Inc_CP2_Amplitude)
Incongruent_Rest_E_pre <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.df)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Inc_CPZ_Amplitude, FC_Rest_E_po_P3_Inc_CP1_Amplitude, FC_Rest_E_po_P3_Inc_CP2_Amplitude)
Incongruent_Rest_E_post<- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df)
Narrow.ROI_FC_E_pre_P3_Con_Amplitude.df <-data.frame(FC_E_pre_P3_Con_CPZ_Amplitude, FC_E_pre_P3_Con_CP1_Amplitude, FC_E_pre_P3_Con_CP2_Amplitude)
Congruent_E_pre <- rowMeans(Narrow.ROI_FC_E_pre_P3_Con_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.df <-data.frame(FC_E_po_P3_Con_CPZ_Amplitude, FC_E_po_P3_Con_CP1_Amplitude, FC_E_po_P3_Con_CP2_Amplitude)
Congruent_E_post <- rowMeans(Narrow.ROI_FC_E_po_P3_Con_Amplitude.df)
Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.df <-data.frame(FC_E_pre_P3_Inc_CPZ_Amplitude, FC_E_pre_P3_Inc_CP1_Amplitude, FC_E_pre_P3_Inc_CP2_Amplitude)
Incongruent_E_pre <- rowMeans(Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Inc_Amplitude.df <-data.frame(FC_E_po_P3_Inc_CPZ_Amplitude, FC_E_po_P3_Inc_CP1_Amplitude, FC_E_po_P3_Inc_CP2_Amplitude)
Incongruent_E_post <- rowMeans(Narrow.ROI_FC_E_po_P3_Inc_Amplitude.df)

#M1 All Conditions Computation 
#Create dataframe 
P3_Amplitude_M1.df<- data.frame(SubjectID, 
                                Congruent_Rest_E_pre, Incongruent_Rest_E_pre, Congruent_Rest_E_post, Incongruent_Rest_E_post, 
                                Congruent_E_pre, Incongruent_E_pre, Congruent_E_post, Incongruent_E_post)
#Melt dataframe into long format assumption testing  
longformat.P3_Amplitude_M1.df <- melt(P3_Amplitude_M1.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.P3_Amplitude_M1.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.P3_Amplitude_M1.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean P3_Amplitude for Model 1 Exercise vs. Rest", x ="Intervention", y = "P3 Amplitude (ms)")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(1, 40))
#Check for outliers 
longformat.P3_Amplitude_M1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
P3_Amplitude_M1.df %>%
  shapiro_test(Congruent_Rest_E_pre, Incongruent_Rest_E_pre, Congruent_Rest_E_post, Incongruent_Rest_E_post, 
               Congruent_E_pre, Incongruent_E_pre, Congruent_E_post, Incongruent_E_post)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.P3_Amplitude_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.P3_Amplitude_M1.df.xlsx') 
RM.P3_Amplitude_M1.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.P3_Amplitude_M1.df.edited.xlsx")
View(RM.P3_Amplitude_M1.df) 
#Computation
M1_P3_Amplitude.aov<-anova_test(data = RM.P3_Amplitude_M1.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint, Congruency), type = 3, effect.size = "ges")
get_anova_table(M1_P3_Amplitude.aov)
#Visualize
      #Reload original data frame
      P3_Amplitude_M1.df<- data.frame(SubjectID, 
                                      Congruent_Rest_E_pre, Incongruent_Rest_E_pre, Congruent_Rest_E_post, Incongruent_Rest_E_post, 
                                      Congruent_E_pre, Incongruent_E_pre, Congruent_E_post, Incongruent_E_post)
      #Export original data frame
      write_xlsx(P3_Amplitude_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.P3_Amplitude_M1.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.P3_Amplitude_M1.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.P3_Amplitude_M1.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.P3_Amplitude_M1.df.graph) 
      #Graph 
      ggplot(graph.P3_Amplitude_M1.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("violetred1", "steelblue1"))+
        geom_point(size = 2, shape = 16)+
        facet_wrap(~Congruency)+
        labs(title="Mean P3 Amplitude: Exercise vs. Rest", x ="Timepoint", y = "Mean P3 Amplitude (mV)")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Mean P3 Amplitude (mV)", limits=c(8, 15))

#M1 Difference in P3_Amplitude Exercise vs. Difference in Rest
#Create difference scores 
Diff_PrePost_R_Congruent_P3_Amplitude <- Congruent_Rest_E_post-Congruent_Rest_E_pre
Diff_PrePost_R_Incongruent_P3_Amplitude <- Incongruent_Rest_E_post-Incongruent_Rest_E_pre
Diff_PrePost_E_Congruent_P3_Amplitude <- Congruent_E_post-Incongruent_E_pre
Diff_PrePost_E_Incongruent_P3_Amplitude <- Incongruent_E_post-Incongruent_E_pre
#Create dataframe  
M1_P3_Amplitude_Diff.df <- data.frame(SubjectID, Diff_PrePost_R_Congruent_P3_Amplitude, Diff_PrePost_R_Incongruent_P3_Amplitude, 
                                      Diff_PrePost_E_Congruent_P3_Amplitude, Diff_PrePost_E_Incongruent_P3_Amplitude)
#Melt dataframe into long format assumption testing  
longformat.M1_P3_Amplitude_Diff.df <- melt(M1_P3_Amplitude_Diff.df, id = "SubjectID", variable.name = "Condition")     
#Summarize
summary(longformat.M1_P3_Amplitude_Diff.df)
#Visualize all conditions
ggplot(longformat.M1_P3_Amplitude_Diff.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean P3 Amplitude Difference Post-Pre for Model 1 Exercise vs. Rest", x ="Intervention", y = "Mean P3 Amplitude Difference (mV)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(-130, 55))      
#Check for outliers 
longformat.M1_P3_Amplitude_Diff.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
M1_P3_Amplitude_Diff.df %>%
  shapiro_test(Diff_PrePost_R_Congruent_P3_Amplitude, Diff_PrePost_R_Incongruent_P3_Amplitude, 
               Diff_PrePost_E_Congruent_P3_Amplitude, Diff_PrePost_E_Incongruent_P3_Amplitude)      
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.M1_P3_Amplitude_Diff.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.M1_P3_Amplitude_Diff.df.xlsx') 
RM.M1_P3_Amplitude_Diff.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.M1_P3_Amplitude_Diff.df.edited.xlsx") 
View(RM.M1_P3_Amplitude_Diff.df) 
#Computation
M1_P3_Amplitude_Diff.aov<-anova_test(data = RM.M1_P3_Amplitude_Diff.df, dv = value, wid = SubjectID, within =  c(Intervention, Congruency), type = 3, effect.size = "pes")
get_anova_table(M1_P3_Amplitude_Diff.aov)


#M2 in P3 Amplitude between Trier vs. Rest
#Average together ROI sites
Narrow.ROI_FC_Rest_T_pre_P3_Con_Amplitude.df <-data.frame(FC_Rest_T_pre_P3_Con_CPZ_Amplitude, FC_Rest_T_pre_P3_Con_CP1_Amplitude, FC_Rest_T_pre_P3_Con_CP2_Amplitude)
Congruent_Rest_T_pre <- rowMeans(Narrow.ROI_FC_Rest_T_pre_P3_Con_Amplitude.df)
Narrow.ROI_FC_Rest_T_po_P3_Con_Amplitude.df <-data.frame(FC_Rest_T_po_P3_Con_CPZ_Amplitude, FC_Rest_T_po_P3_Con_CP1_Amplitude, FC_Rest_T_po_P3_Con_CP2_Amplitude)
Congruent_Rest_T_post <- rowMeans(Narrow.ROI_FC_Rest_T_po_P3_Con_Amplitude.df)
Narrow.ROI_FC_Rest_T_pre_P3_Inc_Amplitude.df <-data.frame(FC_Rest_T_pre_P3_Inc_CPZ_Amplitude, FC_Rest_T_pre_P3_Inc_CP1_Amplitude, FC_Rest_T_pre_P3_Inc_CP2_Amplitude)
Incongruent_Rest_T_pre <- rowMeans(Narrow.ROI_FC_Rest_T_pre_P3_Inc_Amplitude.df)
Narrow.ROI_FC_Rest_T_po_P3_Inc_Amplitude.df <-data.frame(FC_Rest_T_po_P3_Inc_CPZ_Amplitude, FC_Rest_T_po_P3_Inc_CP1_Amplitude, FC_Rest_T_po_P3_Inc_CP2_Amplitude)
Incongruent_Rest_T_post<- rowMeans(Narrow.ROI_FC_Rest_T_po_P3_Inc_Amplitude.df)
Narrow.ROI_FC_T_pre_P3_Con_Amplitude.df <-data.frame(FC_T_pre_P3_Con_CPZ_Amplitude, FC_T_pre_P3_Con_CP1_Amplitude, FC_T_pre_P3_Con_CP2_Amplitude)
Congruent_T_pre <- rowMeans(Narrow.ROI_FC_T_pre_P3_Con_Amplitude.df)
Narrow.ROI_FC_T_po_P3_Con_Amplitude.df <-data.frame(FC_T_po_P3_Con_CPZ_Amplitude, FC_T_po_P3_Con_CP1_Amplitude, FC_T_po_P3_Con_CP2_Amplitude)
Congruent_T_post <- rowMeans(Narrow.ROI_FC_T_po_P3_Con_Amplitude.df)
Narrow.ROI_FC_T_pre_P3_Inc_Amplitude.df <-data.frame(FC_T_pre_P3_Inc_CPZ_Amplitude, FC_T_pre_P3_Inc_CP1_Amplitude, FC_T_pre_P3_Inc_CP2_Amplitude)
Incongruent_T_pre <- rowMeans(Narrow.ROI_FC_T_pre_P3_Inc_Amplitude.df)
Narrow.ROI_FC_T_po_P3_Inc_Amplitude.df <-data.frame(FC_T_po_P3_Inc_CPZ_Amplitude, FC_T_po_P3_Inc_CP1_Amplitude, FC_T_po_P3_Inc_CP2_Amplitude)
Incongruent_T_post <- rowMeans(Narrow.ROI_FC_T_po_P3_Inc_Amplitude.df)

#M2 All Conditions Computation 
#Create dataframe 
P3_Amplitude_M2.df<- data.frame(SubjectID, 
                                Congruent_Rest_T_pre, Incongruent_Rest_T_pre, Congruent_Rest_T_post, Incongruent_Rest_T_post, 
                                Congruent_T_pre, Incongruent_T_pre, Congruent_T_post, Incongruent_T_post)
#Melt dataframe into long format assumption testing  
longformat.P3_Amplitude_M2.df <- melt(P3_Amplitude_M2.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.P3_Amplitude_M2.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.P3_Amplitude_M2.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean P3_Amplitude for Model 2 Trier vs. Rest", x ="Intervention", y = "P3 Amplitude (mV)")+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.P3_Amplitude_M2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
P3_Amplitude_M2.df %>%
  shapiro_test(Congruent_Rest_T_pre, Incongruent_Rest_T_pre, Congruent_Rest_T_post, Incongruent_Rest_T_post, 
               Congruent_T_pre, Incongruent_T_pre, Congruent_T_post, Incongruent_T_post)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.P3_Amplitude_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.P3_Amplitude_M2.df.xlsx') 
RM.P3_Amplitude_M2.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.P3_Amplitude_M2.df.edited.xlsx")
View(RM.P3_Amplitude_M2.df) 
#Computation
M2_P3_Amplitude.aov<-anova_test(data = RM.P3_Amplitude_M2.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint, Congruency), type = 3, effect.size = "pes")
get_anova_table(M2_P3_Amplitude.aov)
#PostHoc Computations
t.test(RM.P3_Amplitude_M2.df$value~RM.P3_Amplitude_M2.df$Timepoint, paired = TRUE, p.adjust.method = "bonferroni")
#Summarize for report
RM.P3_Amplitude_M2.df%>%
  group_by(Timepoint)%>%
  get_summary_stats(value, type = "mean_sd")
#Visualize
      #Reload original data frame
      P3_Amplitude_M2.df<- data.frame(SubjectID, 
                                      Congruent_Rest_T_pre, Incongruent_Rest_T_pre, Congruent_Rest_T_post, Incongruent_Rest_T_post, 
                                      Congruent_T_pre, Incongruent_T_pre, Congruent_T_post, Incongruent_T_post)
      #Export original data frame
      write_xlsx(P3_Amplitude_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.P3_Amplitude_M2.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.P3_Amplitude_M2.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.P3_Amplitude_M2.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.P3_Amplitude_M2.df.graph) 
      #Graph 
      ggplot(graph.P3_Amplitude_M2.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("steelblue1", "violetred1"))+
        geom_point(size = 2, shape = 16)+
        facet_wrap(~Congruency)+
        labs(title="Mean P3 Amplitude: Trier vs. Rest", x ="Timepoint", y = "Mean P3 Amplitude (mV)")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Mean P3 Amplitude (mV)", limits=c(8, 15))

#M2 Difference in P3_Amplitude Trier vs. Difference in Rest
#Create difference scores 
Diff_PrePost_R_Congruent_P3_Amplitude <- Congruent_Rest_T_post-Congruent_Rest_T_pre
Diff_PrePost_R_Incongruent_P3_Amplitude <- Incongruent_Rest_T_post-Incongruent_Rest_T_pre
Diff_PrePost_T_Congruent_P3_Amplitude <- Congruent_T_post-Incongruent_T_pre
Diff_PrePost_T_Incongruent_P3_Amplitude <- Incongruent_T_post-Incongruent_T_pre
#Create dataframe  
M2_P3_Amplitude_Diff.df <- data.frame(SubjectID, Diff_PrePost_R_Congruent_P3_Amplitude, Diff_PrePost_R_Incongruent_P3_Amplitude, 
                                      Diff_PrePost_T_Congruent_P3_Amplitude, Diff_PrePost_T_Incongruent_P3_Amplitude)
#Melt dataframe into long format assumption testing  
longformat.M2_P3_Amplitude_Diff.df <- melt(M2_P3_Amplitude_Diff.df, id = "SubjectID", variable.name = "Condition")     
#Summarize
summary(longformat.M2_P3_Amplitude_Diff.df)
#Visualize all conditions
ggplot(longformat.M2_P3_Amplitude_Diff.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Mean P3 Amplitude Difference Post-Pre for Model 2 Trier vs. Rest", x ="Intervention", y = "Mean P3 Amplitude Difference (ms)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(-130, 90))      
#Check for outliers 
longformat.M2_P3_Amplitude_Diff.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
M2_P3_Amplitude_Diff.df %>%
  shapiro_test(Diff_PrePost_R_Congruent_P3_Amplitude, Diff_PrePost_R_Incongruent_P3_Amplitude, 
               Diff_PrePost_T_Congruent_P3_Amplitude, Diff_PrePost_T_Incongruent_P3_Amplitude)      
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.M2_P3_Amplitude_Diff.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.M2_P3_Amplitude_Diff.df.xlsx') 
RM.M2_P3_Amplitude_Diff.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.M2_P3_Amplitude_Diff.df.edited.xlsx") 
View(RM.M2_P3_Amplitude_Diff.df) 
#Computation
M2_P3_Amplitude_Diff.aov<-anova_test(data = RM.M2_P3_Amplitude_Diff.df, dv = value, wid = SubjectID, within =  c(Intervention, Congruency), type = 3, effect.size = "pes")
get_anova_table(M2_P3_Amplitude_Diff.aov)
#PostHoc Computations

# RESTING BRAIN FUNCTION OUTCOMES - Resting Posterior Alpha_EO -----------------------
#Call variables
P1_Alpha_EO_E_Pre <- df$P1_Alpha_EO_E_Pre
PZ_Alpha_EO_E_Pre <- df$PZ_Alpha_EO_E_Pre
P2_Alpha_EO_E_Pre <- df$P2_Alpha_EO_E_Pre
PO3_Alpha_EO_E_Pre <- df$PO3_Alpha_EO_E_Pre
POZ_Alpha_EO_E_Pre <- df$POZ_Alpha_EO_E_Pre
PO4_Alpha_EO_E_Pre <- df$PO4_Alpha_EO_E_Pre
P1_Alpha_EO_E_Post <- df$P1_Alpha_EO_E_Post
PZ_Alpha_EO_E_Post <- df$PZ_Alpha_EO_E_Post
P2_Alpha_EO_E_Post <- df$P2_Alpha_EO_E_Post
PO3_Alpha_EO_E_Post <- df$PO3_Alpha_EO_E_Post
POZ_Alpha_EO_E_Post <- df$POZ_Alpha_EO_E_Post
PO4_Alpha_EO_E_Post <- df$PO4_Alpha_EO_E_Post
P1_Alpha_EO_Rest_E_Pre <- df$P1_Alpha_EO_Rest_E_Pre
PZ_Alpha_EO_Rest_E_Pre <- df$PZ_Alpha_EO_Rest_E_Pre
P2_Alpha_EO_Rest_E_Pre <- df$P2_Alpha_EO_Rest_E_Pre
PO3_Alpha_EO_Rest_E_Pre <- df$PO3_Alpha_EO_Rest_E_Pre
POZ_Alpha_EO_Rest_E_Pre <- df$POZ_Alpha_EO_Rest_E_Pre
PO4_Alpha_EO_Rest_E_Pre <- df$PO4_Alpha_EO_Rest_E_Pre
P1_Alpha_EO_Rest_E_Post <- df$P1_Alpha_EO_Rest_E_Post
PZ_Alpha_EO_Rest_E_Post <- df$PZ_Alpha_EO_Rest_E_Post
P2_Alpha_EO_Rest_E_Post <- df$P2_Alpha_EO_Rest_E_Post
PO3_Alpha_EO_Rest_E_Post <- df$PO3_Alpha_EO_Rest_E_Post
POZ_Alpha_EO_Rest_E_Post <- df$POZ_Alpha_EO_Rest_E_Post
PO4_Alpha_EO_Rest_E_Post <- df$PO4_Alpha_EO_Rest_E_Post
P1_Alpha_EO_T_Pre <- df$P1_Alpha_EO_T_Pre
PZ_Alpha_EO_T_Pre <- df$PZ_Alpha_EO_T_Pre
P2_Alpha_EO_T_Pre <- df$P2_Alpha_EO_T_Pre
PO3_Alpha_EO_T_Pre <- df$PO3_Alpha_EO_T_Pre
POZ_Alpha_EO_T_Pre <- df$POZ_Alpha_EO_T_Pre
PO4_Alpha_EO_T_Pre <- df$PO4_Alpha_EO_T_Pre
P1_Alpha_EO_T_Post <- df$P1_Alpha_EO_T_Post
PZ_Alpha_EO_T_Post <- df$PZ_Alpha_EO_T_Post
P2_Alpha_EO_T_Post <- df$P2_Alpha_EO_T_Post
PO3_Alpha_EO_T_Post <- df$PO3_Alpha_EO_T_Post
POZ_Alpha_EO_T_Post <- df$POZ_Alpha_EO_T_Post
PO4_Alpha_EO_T_Post <- df$PO4_Alpha_EO_T_Post
P1_Alpha_EO_Rest_T_Pre <- df$P1_Alpha_EO_Rest_T_Pre
PZ_Alpha_EO_Rest_T_Pre <- df$PZ_Alpha_EO_Rest_T_Pre
P2_Alpha_EO_Rest_T_Pre <- df$P2_Alpha_EO_Rest_T_Pre
PO3_Alpha_EO_Rest_T_Pre <- df$PO3_Alpha_EO_Rest_T_Pre
POZ_Alpha_EO_Rest_T_Pre <- df$POZ_Alpha_EO_Rest_T_Pre
PO4_Alpha_EO_Rest_T_Pre <- df$PO4_Alpha_EO_Rest_T_Pre
P1_Alpha_EO_Rest_T_Post <- df$P1_Alpha_EO_Rest_T_Post
PZ_Alpha_EO_Rest_T_Post <- df$PZ_Alpha_EO_Rest_T_Post
P2_Alpha_EO_Rest_T_Post <- df$P2_Alpha_EO_Rest_T_Post
PO3_Alpha_EO_Rest_T_Post <- df$PO3_Alpha_EO_Rest_T_Post
POZ_Alpha_EO_Rest_T_Post <- df$POZ_Alpha_EO_Rest_T_Post
PO4_Alpha_EO_Rest_T_Post <- df$PO4_Alpha_EO_Rest_T_Post

#M1 in Resting Alpha_EO between Exercise vs. Rest
#Average together ROI sites
Rest_E_pre_Alpha.df <-data.frame(P1_Alpha_EO_Rest_E_Pre, PZ_Alpha_EO_Rest_E_Pre, P2_Alpha_EO_Rest_E_Pre, PO3_Alpha_EO_Rest_E_Pre, POZ_Alpha_EO_Rest_E_Pre, PO4_Alpha_EO_Rest_E_Pre)
Rest_E_pre_Alpha <- rowMeans(Rest_E_pre_Alpha.df)
Rest_E_post_Alpha.df <-data.frame(P1_Alpha_EO_Rest_E_Post, PZ_Alpha_EO_Rest_E_Post, P2_Alpha_EO_Rest_E_Post, PO3_Alpha_EO_Rest_E_Post, POZ_Alpha_EO_Rest_E_Post, PO4_Alpha_EO_Rest_E_Post)
Rest_E_post_Alpha <- rowMeans(Rest_E_post_Alpha.df)
E_pre_Alpha.df <-data.frame(P1_Alpha_EO_E_Pre, PZ_Alpha_EO_E_Pre, P2_Alpha_EO_E_Pre, PO3_Alpha_EO_E_Pre, POZ_Alpha_EO_E_Pre, PO4_Alpha_EO_E_Pre)
E_pre_Alpha <- rowMeans(E_pre_Alpha.df)
E_post_Alpha.df <-data.frame(P1_Alpha_EO_E_Post, PZ_Alpha_EO_E_Post, P2_Alpha_EO_E_Post, PO3_Alpha_EO_E_Post, POZ_Alpha_EO_E_Pre, PO4_Alpha_EO_E_Post)
E_post_Alpha <- rowMeans(E_post_Alpha.df)

#M1 All Conditions Computation 
#Create dataframe 
Alpha_EO_M1.df<- data.frame(SubjectID, Rest_E_pre_Alpha, Rest_E_post_Alpha, E_pre_Alpha, E_post_Alpha)
#Melt dataframe into long format assumption testing  
longformat.Alpha_EO_M1.df <- melt(Alpha_EO_M1.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.Alpha_EO_M1.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.Alpha_EO_M1.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EO Alpha: Exercise vs. Rest", x ="Intervention", y = "Log Power")+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.Alpha_EO_M1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
Alpha_EO_M1.df %>%
  shapiro_test(Rest_E_pre_Alpha, Rest_E_post_Alpha, E_pre_Alpha, E_post_Alpha)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.Alpha_EO_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.Alpha_EO_M1.df.xlsx') 
RM.longformat.Alpha_EO_M1.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.Alpha_EO_M1.df.edited.xlsx")
View(RM.longformat.Alpha_EO_M1.df) 
#Computation
Alpha_EO_M1.aov<-anova_test(data = RM.longformat.Alpha_EO_M1.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint), type = 3, effect.size = "ges")
get_anova_table(Alpha_EO_M1.aov)
#PostHoc Computations
t.test(RM.longformat.Alpha_EO_M1.df$value~RM.longformat.Alpha_EO_M1.df$Timepoint, paired = TRUE, p.adjust.method = "bonferroni")
#Summarize for report 
RM.longformat.Alpha_EO_M1.df%>%
  group_by(Timepoint)%>%
  get_summary_stats(value, type = "mean_sd")
#Visualize
      #Reload original data frame
      Alpha_EO_M1.df<- data.frame(SubjectID, Rest_E_pre_Alpha, Rest_E_post_Alpha, E_pre_Alpha, E_post_Alpha)
      #Export original data frame
      write_xlsx(Alpha_EO_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.Alpha_EO_M1.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.Alpha_EO_M1.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.Alpha_EO_M1.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.Alpha_EO_M1.df.graph) 
      #Graph 
      ggplot(graph.Alpha_EO_M1.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("violetred1", "steelblue1"))+
        geom_point(size = 2, shape = 16)+
        labs(title="Resting EO Alpha: Exercise vs. Rest", x ="Timepoint", y = "Log Power")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Log Power Spectral Density (10*log10(uV^2/Hz))", limits=c(3.1, 3.7))

#M1 Difference in Resting Alpha_EO Exercise vs. Rest Difference
#Create difference scores 
Diff_PrePost_R_Alpha_EO <- Rest_E_post_Alpha-Rest_E_pre_Alpha
Diff_PrePost_E_Alpha_EO <- E_post_Alpha-E_pre_Alpha
#Create dataframe  
M1_Alpha_EO_Diff.df <- data.frame(SubjectID, Diff_PrePost_R_Alpha_EO, Diff_PrePost_E_Alpha_EO)
#Melt dataframe into long format assumption testing  
longformat.M1_Alpha_EO_Diff.df <- melt(M1_Alpha_EO_Diff.df, id = "SubjectID", variable.name = "Condition")     
#Summarize
summary(longformat.M1_Alpha_EO_Diff.df)
#Visualize all conditions
ggplot(longformat.M1_Alpha_EO_Diff.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EO Alpha Power Difference Post-Pre for Model 1 Exercise vs. Rest", x ="Intervention", y = "Log Power")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.M1_Alpha_EO_Diff.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
M1_Alpha_EO_Diff.df <- data.frame(Diff_PrePost_R_Congruent_Alpha_EO, Diff_PrePost_R_Incongruent_Alpha_EO, 
                                  Diff_PrePost_E_Congruent_Alpha_EO, Diff_PrePost_E_Incongruent_Alpha_EO)     
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.M1_Alpha_EO_Diff.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.M1_Alpha_EO_Diff.df.xlsx') 
RM.M1_Alpha_EO_Diff.df<- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.M1_Alpha_EO_Diff.df.edited.xlsx") 
View(RM.M1_Alpha_EO_Diff.df) 
#Computation
t.test(RM.M1_Alpha_EO_Diff.df$value~RM.M1_Alpha_EO_Diff.df$Intervention, paired = TRUE, p.adjust.method = "bonferroni")      


#M2 in Resting Alpha_EO between Trier vs. Rest
#Average together ROI sites
Rest_T_pre_Alpha.df <-data.frame(P1_Alpha_EO_Rest_T_Pre, PZ_Alpha_EO_Rest_T_Pre, P2_Alpha_EO_Rest_T_Pre, PO3_Alpha_EO_Rest_T_Pre, POZ_Alpha_EO_Rest_T_Pre, PO4_Alpha_EO_Rest_T_Pre)
Rest_T_pre_Alpha <- rowMeans(Rest_T_pre_Alpha.df)
Rest_T_post_Alpha.df <-data.frame(P1_Alpha_EO_Rest_T_Post, PZ_Alpha_EO_Rest_T_Post, P2_Alpha_EO_Rest_T_Post, PO3_Alpha_EO_Rest_T_Post, POZ_Alpha_EO_Rest_T_Post, PO4_Alpha_EO_Rest_T_Post)
Rest_T_post_Alpha <- rowMeans(Rest_T_post_Alpha.df)
T_pre_Alpha.df <-data.frame(P1_Alpha_EO_T_Pre, PZ_Alpha_EO_T_Pre, P2_Alpha_EO_T_Pre, PO3_Alpha_EO_T_Pre, POZ_Alpha_EO_T_Pre, PO4_Alpha_EO_T_Pre)
T_pre_Alpha <- rowMeans(T_pre_Alpha.df)
T_post_Alpha.df <-data.frame(P1_Alpha_EO_T_Post, PZ_Alpha_EO_T_Post, P2_Alpha_EO_T_Post, PO3_Alpha_EO_T_Post, POZ_Alpha_EO_T_Pre, PO4_Alpha_EO_T_Post)
T_post_Alpha <- rowMeans(T_post_Alpha.df)

#M2 All Conditions Computation 
#Create dataframe 
Alpha_EO_M2.df<- data.frame(SubjectID, Rest_T_pre_Alpha, Rest_T_post_Alpha, T_pre_Alpha, T_post_Alpha)
#Melt dataframe into long format assumption testing  
longformat.Alpha_EO_M2.df <- melt(Alpha_EO_M2.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.Alpha_EO_M2.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.Alpha_EO_M2.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EO Alpha for Model 2 Trier vs. Rest", x ="Intervention", y = "Log Power")+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.Alpha_EO_M2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
Alpha_EO_M2.df %>%
  shapiro_test(Rest_T_pre_Alpha, Rest_T_post_Alpha, T_pre_Alpha, T_post_Alpha)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.Alpha_EO_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.Alpha_EO_M2.df.xlsx') 
RM.longformat.Alpha_EO_M2.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.Alpha_EO_M2.df.edited.xlsx")
View(RM.longformat.Alpha_EO_M2.df) 
#Computation
Alpha_EO_M2.aov<-anova_test(data = RM.longformat.Alpha_EO_M2.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint), type = 3, effect.size = "ges")
get_anova_table(Alpha_EO_M2.aov)
#PostHoc Computations
t.test(RM.longformat.Alpha_EO_M2.df$value~RM.longformat.Alpha_EO_M2.df$Timepoint, paired = TRUE, p.adjust.method = "bonferroni")
#Summarize for report 
RM.longformat.Alpha_EO_M2.df%>%
  group_by(Timepoint)%>%
  get_summary_stats(value, type = "mean_sd")
#Visualize
      #Reload original data frame
      Alpha_EO_M2.df<- data.frame(SubjectID, Rest_T_pre_Alpha, Rest_T_post_Alpha, T_pre_Alpha, T_post_Alpha)
      #Export original data frame
      write_xlsx(Alpha_EO_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.Alpha_EO_M2.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.Alpha_EO_M2.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.Alpha_EO_M2.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.Alpha_EO_M2.df.graph) 
      #Graph 
      ggplot(graph.Alpha_EO_M2.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("steelblue1", "violetred1"))+
        geom_point(size = 2, shape = 16)+
        labs(title="Resting EO Alpha: Trier vs. Rest", x ="Timepoint", y = "Log Power")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Log Power Spectral Density (10*log10(uV^2/Hz))", limits=c(3.1, 3.7))

#M2 Difference in Resting Alpha_EO Trier vs. Rest Difference
#Create difference scores 
Diff_PrePost_R_Alpha_EO <- Rest_T_post_Alpha-Rest_T_pre_Alpha
Diff_PrePost_T_Alpha_EO <- T_post_Alpha-T_pre_Alpha
#Create dataframe  
M2_Alpha_EO_Diff.df <- data.frame(SubjectID, Diff_PrePost_R_Alpha_EO, Diff_PrePost_T_Alpha_EO)
#Melt dataframe into long format assumption testing  
longformat.M2_Alpha_EO_Diff.df <- melt(M2_Alpha_EO_Diff.df, id = "SubjectID", variable.name = "Condition")     
#Summarize
summary(longformat.M2_Alpha_EO_Diff.df)
#Visualize all conditions
ggplot(longformat.M2_Alpha_EO_Diff.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EO Alpha Power Difference Post-Pre for Model 2 Trier vs. Rest", x ="Intervention", y = "Log Power")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.M2_Alpha_EO_Diff.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
M2_Alpha_EO_Diff.df <- data.frame(Diff_PrePost_R_Congruent_Alpha_EO, Diff_PrePost_R_Incongruent_Alpha_EO, 
                                  Diff_PrePost_T_Congruent_Alpha_EO, Diff_PrePost_T_Incongruent_Alpha_EO)     
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.M2_Alpha_EO_Diff.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.M2_Alpha_EO_Diff.df.xlsx') 
RM.M2_Alpha_EO_Diff.df<- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.M2_Alpha_EO_Diff.df.edited.xlsx") 
View(RM.M2_Alpha_EO_Diff.df) 
#Computation
t.test(RM.M2_Alpha_EO_Diff.df$value~RM.M2_Alpha_EO_Diff.df$Intervention, paired = TRUE, p.adjust.method = "bonferroni")      

# RESTING BRAIN FUNCTION OUTCOMES - Resting Posterior Alpha_EC -----------------------
#Call variables
P1_Alpha_EC_E_Pre <- df$P1_Alpha_EC_E_Pre
PZ_Alpha_EC_E_Pre <- df$PZ_Alpha_EC_E_Pre
P2_Alpha_EC_E_Pre <- df$P2_Alpha_EC_E_Pre
PO3_Alpha_EC_E_Pre <- df$PO3_Alpha_EC_E_Pre
POZ_Alpha_EC_E_Pre <- df$POZ_Alpha_EC_E_Pre
PO4_Alpha_EC_E_Pre <- df$PO4_Alpha_EC_E_Pre
P1_Alpha_EC_E_Post <- df$P1_Alpha_EC_E_Post
PZ_Alpha_EC_E_Post <- df$PZ_Alpha_EC_E_Post
P2_Alpha_EC_E_Post <- df$P2_Alpha_EC_E_Post
PO3_Alpha_EC_E_Post <- df$PO3_Alpha_EC_E_Post
POZ_Alpha_EC_E_Post <- df$POZ_Alpha_EC_E_Post
PO4_Alpha_EC_E_Post <- df$PO4_Alpha_EC_E_Post
P1_Alpha_EC_Rest_E_Pre <- df$P1_Alpha_EC_Rest_E_Pre
PZ_Alpha_EC_Rest_E_Pre <- df$PZ_Alpha_EC_Rest_E_Pre
P2_Alpha_EC_Rest_E_Pre <- df$P2_Alpha_EC_Rest_E_Pre
PO3_Alpha_EC_Rest_E_Pre <- df$PO3_Alpha_EC_Rest_E_Pre
POZ_Alpha_EC_Rest_E_Pre <- df$POZ_Alpha_EC_Rest_E_Pre
PO4_Alpha_EC_Rest_E_Pre <- df$PO4_Alpha_EC_Rest_E_Pre
P1_Alpha_EC_Rest_E_Post <- df$P1_Alpha_EC_Rest_E_Post
PZ_Alpha_EC_Rest_E_Post <- df$PZ_Alpha_EC_Rest_E_Post
P2_Alpha_EC_Rest_E_Post <- df$P2_Alpha_EC_Rest_E_Post
PO3_Alpha_EC_Rest_E_Post <- df$PO3_Alpha_EC_Rest_E_Post
POZ_Alpha_EC_Rest_E_Post <- df$POZ_Alpha_EC_Rest_E_Post
PO4_Alpha_EC_Rest_E_Post <- df$PO4_Alpha_EC_Rest_E_Post
P1_Alpha_EC_T_Pre <- df$P1_Alpha_EC_T_Pre
PZ_Alpha_EC_T_Pre <- df$PZ_Alpha_EC_T_Pre
P2_Alpha_EC_T_Pre <- df$P2_Alpha_EC_T_Pre
PO3_Alpha_EC_T_Pre <- df$PO3_Alpha_EC_T_Pre
POZ_Alpha_EC_T_Pre <- df$POZ_Alpha_EC_T_Pre
PO4_Alpha_EC_T_Pre <- df$PO4_Alpha_EC_T_Pre
P1_Alpha_EC_T_Post <- df$P1_Alpha_EC_T_Post
PZ_Alpha_EC_T_Post <- df$PZ_Alpha_EC_T_Post
P2_Alpha_EC_T_Post <- df$P2_Alpha_EC_T_Post
PO3_Alpha_EC_T_Post <- df$PO3_Alpha_EC_T_Post
POZ_Alpha_EC_T_Post <- df$POZ_Alpha_EC_T_Post
PO4_Alpha_EC_T_Post <- df$PO4_Alpha_EC_T_Post
P1_Alpha_EC_Rest_T_Pre <- df$P1_Alpha_EC_Rest_T_Pre
PZ_Alpha_EC_Rest_T_Pre <- df$PZ_Alpha_EC_Rest_T_Pre
P2_Alpha_EC_Rest_T_Pre <- df$P2_Alpha_EC_Rest_T_Pre
PO3_Alpha_EC_Rest_T_Pre <- df$PO3_Alpha_EC_Rest_T_Pre
POZ_Alpha_EC_Rest_T_Pre <- df$POZ_Alpha_EC_Rest_T_Pre
PO4_Alpha_EC_Rest_T_Pre <- df$PO4_Alpha_EC_Rest_T_Pre
P1_Alpha_EC_Rest_T_Post <- df$P1_Alpha_EC_Rest_T_Post
PZ_Alpha_EC_Rest_T_Post <- df$PZ_Alpha_EC_Rest_T_Post
P2_Alpha_EC_Rest_T_Post <- df$P2_Alpha_EC_Rest_T_Post
PO3_Alpha_EC_Rest_T_Post <- df$PO3_Alpha_EC_Rest_T_Post
POZ_Alpha_EC_Rest_T_Post <- df$POZ_Alpha_EC_Rest_T_Post
PO4_Alpha_EC_Rest_T_Post <- df$PO4_Alpha_EC_Rest_T_Post

#M1 in Resting Alpha_EC between Exercise vs. Rest
#Average together ROI sites
Rest_E_pre_Alpha.df <-data.frame(P1_Alpha_EC_Rest_E_Pre, PZ_Alpha_EC_Rest_E_Pre, P2_Alpha_EC_Rest_E_Pre, PO3_Alpha_EC_Rest_E_Pre, POZ_Alpha_EC_Rest_E_Pre, PO4_Alpha_EC_Rest_E_Pre)
Rest_E_pre_Alpha <- rowMeans(Rest_E_pre_Alpha.df)
Rest_E_post_Alpha.df <-data.frame(P1_Alpha_EC_Rest_E_Post, PZ_Alpha_EC_Rest_E_Post, P2_Alpha_EC_Rest_E_Post, PO3_Alpha_EC_Rest_E_Post, POZ_Alpha_EC_Rest_E_Post, PO4_Alpha_EC_Rest_E_Post)
Rest_E_post_Alpha <- rowMeans(Rest_E_post_Alpha.df)
E_pre_Alpha.df <-data.frame(P1_Alpha_EC_E_Pre, PZ_Alpha_EC_E_Pre, P2_Alpha_EC_E_Pre, PO3_Alpha_EC_E_Pre, POZ_Alpha_EC_E_Pre, PO4_Alpha_EC_E_Pre)
E_pre_Alpha <- rowMeans(E_pre_Alpha.df)
E_post_Alpha.df <-data.frame(P1_Alpha_EC_E_Post, PZ_Alpha_EC_E_Post, P2_Alpha_EC_E_Post, PO3_Alpha_EC_E_Post, POZ_Alpha_EC_E_Pre, PO4_Alpha_EC_E_Post)
E_post_Alpha <- rowMeans(E_post_Alpha.df)

#M1 All Conditions Computation 
#Create dataframe 
Alpha_EC_M1.df<- data.frame(SubjectID, Rest_E_pre_Alpha, Rest_E_post_Alpha, E_pre_Alpha, E_post_Alpha)
#Melt dataframe into long format assumption testing  
longformat.Alpha_EC_M1.df <- melt(Alpha_EC_M1.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.Alpha_EC_M1.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.Alpha_EC_M1.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EC Alpha for Model 1 Exercise vs. Rest", x ="Intervention", y = "Log Power")+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.Alpha_EC_M1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
Alpha_EC_M1.df %>%
  shapiro_test(Rest_E_pre_Alpha, Rest_E_post_Alpha, E_pre_Alpha, E_post_Alpha)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.Alpha_EC_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.Alpha_EC_M1.df.xlsx') 
RM.longformat.Alpha_EC_M1.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.Alpha_EC_M1.df.edited.xlsx")
View(RM.longformat.Alpha_EC_M1.df) 
#Computation
Alpha_EC_M1.aov<-anova_test(data = RM.longformat.Alpha_EC_M1.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint), type = 3, effect.size = "ges")
get_anova_table(Alpha_EC_M1.aov)
#Visualize
      #Reload original data frame
      Alpha_EC_M1.df<- data.frame(SubjectID, Rest_E_pre_Alpha, Rest_E_post_Alpha, E_pre_Alpha, E_post_Alpha)
      #Export original data frame
      write_xlsx(Alpha_EC_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.Alpha_EC_M1.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.Alpha_EC_M1.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.Alpha_EC_M1.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.Alpha_EC_M1.df.graph) 
      #Graph 
      ggplot(graph.Alpha_EC_M1.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("violetred1", "steelblue1"))+
        geom_point(size = 2, shape = 16)+
        labs(title="Resting EC Alpha: Exercise vs. Rest", x ="Timepoint", y = "Log Power")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Log Power Spectral Density (10*log10(uV^2/Hz))", limits=c(3.1, 4.6))

#M1 Difference in Resting Alpha_EC Exercise vs. Rest Difference
#Create difference scores 
Diff_PrePost_R_Alpha_EC <- Rest_E_post_Alpha-Rest_E_pre_Alpha
Diff_PrePost_E_Alpha_EC <- E_post_Alpha-E_pre_Alpha
#Create dataframe  
M1_Alpha_EC_Diff.df <- data.frame(SubjectID, Diff_PrePost_R_Alpha_EC, Diff_PrePost_E_Alpha_EC)
#Melt dataframe into long format assumption testing  
longformat.M1_Alpha_EC_Diff.df <- melt(M1_Alpha_EC_Diff.df, id = "SubjectID", variable.name = "Condition")     
#Summarize
summary(longformat.M1_Alpha_EC_Diff.df)
#Visualize all conditions
ggplot(longformat.M1_Alpha_EC_Diff.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EC Alpha Power Difference Post-Pre for Model 1 Exercise vs. Rest", x ="Intervention", y = "Log Power")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.M1_Alpha_EC_Diff.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
M1_Alpha_EC_Diff.df <- data.frame(Diff_PrePost_R_Alpha_EC, Diff_PrePost_E_Alpha_EC)     
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.M1_Alpha_EC_Diff.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.M1_Alpha_EC_Diff.df.xlsx') 
RM.M1_Alpha_EC_Diff.df<- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.M1_Alpha_EC_Diff.df.edited.xlsx") 
View(RM.M1_Alpha_EC_Diff.df) 
#Computation
t.test(RM.M1_Alpha_EC_Diff.df$value~RM.M1_Alpha_EC_Diff.df$Intervention, paired = TRUE, p.adjust.method = "bonferroni")      


#M2 in Resting Alpha_EC between Trier vs. Rest
#Average together ROI sites
Rest_T_pre_Alpha.df <-data.frame(P1_Alpha_EC_Rest_T_Pre, PZ_Alpha_EC_Rest_T_Pre, P2_Alpha_EC_Rest_T_Pre, PO3_Alpha_EC_Rest_T_Pre, POZ_Alpha_EC_Rest_T_Pre, PO4_Alpha_EC_Rest_T_Pre)
Rest_T_pre_Alpha <- rowMeans(Rest_T_pre_Alpha.df)
Rest_T_post_Alpha.df <-data.frame(P1_Alpha_EC_Rest_T_Post, PZ_Alpha_EC_Rest_T_Post, P2_Alpha_EC_Rest_T_Post, PO3_Alpha_EC_Rest_T_Post, POZ_Alpha_EC_Rest_T_Post, PO4_Alpha_EC_Rest_T_Post)
Rest_T_post_Alpha <- rowMeans(Rest_T_post_Alpha.df)
T_pre_Alpha.df <-data.frame(P1_Alpha_EC_T_Pre, PZ_Alpha_EC_T_Pre, P2_Alpha_EC_T_Pre, PO3_Alpha_EC_T_Pre, POZ_Alpha_EC_T_Pre, PO4_Alpha_EC_T_Pre)
T_pre_Alpha <- rowMeans(T_pre_Alpha.df)
T_post_Alpha.df <-data.frame(P1_Alpha_EC_T_Post, PZ_Alpha_EC_T_Post, P2_Alpha_EC_T_Post, PO3_Alpha_EC_T_Post, POZ_Alpha_EC_T_Pre, PO4_Alpha_EC_T_Post)
T_post_Alpha <- rowMeans(T_post_Alpha.df)

#M2 All Conditions Computation 
#Create dataframe 
Alpha_EC_M2.df<- data.frame(SubjectID, Rest_T_pre_Alpha, Rest_T_post_Alpha, T_pre_Alpha, T_post_Alpha)
#Melt dataframe into long format assumption testing  
longformat.Alpha_EC_M2.df <- melt(Alpha_EC_M2.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.Alpha_EC_M2.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.Alpha_EC_M2.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EC Alpha for Model 2 Trier vs. Rest", x ="Intervention", y = "Log Power")+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.Alpha_EC_M2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
Alpha_EC_M2.df %>%
  shapiro_test(Rest_T_pre_Alpha, Rest_T_post_Alpha, T_pre_Alpha, T_post_Alpha)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.Alpha_EC_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.Alpha_EC_M2.df.xlsx') 
RM.longformat.Alpha_EC_M2.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.Alpha_EC_M2.df.edited.xlsx")
View(RM.longformat.Alpha_EC_M2.df) 
#Computation
Alpha_EC_M2.aov<-anova_test(data = RM.longformat.Alpha_EC_M2.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint), type = 3, effect.size = "ges")
get_anova_table(Alpha_EC_M2.aov)
#PostHoc Computations
interaction.aov<- RM.longformat.Alpha_EC_M2.df %>%
  group_by(Intervention) %>%
  anova_test(dv = value, wid = SubjectID, within = Timepoint) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
interaction.aov

pairwise.aov <- RM.longformat.Alpha_EC_M2.df %>%
  group_by(Intervention) %>%
  pairwise_t_test(
    value ~ Timepoint, paired = TRUE,
    p.adjust.method = "bonferroni")
pairwise.aov

#Visualize
      #Reload original data frame
      Alpha_EC_M2.df<- data.frame(SubjectID, Rest_T_pre_Alpha, Rest_T_post_Alpha, T_pre_Alpha, T_post_Alpha)
      #Export original data frame
      write_xlsx(Alpha_EC_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.Alpha_EC_M2.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.Alpha_EC_M2.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.Alpha_EC_M2.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.Alpha_EC_M2.df.graph) 
      #Graph 
      ggplot(graph.Alpha_EC_M2.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("steelblue1", "violetred1"))+
        geom_point(size = 2, shape = 16)+
        labs(title="Resting EC Alpha: Trier vs. Rest", x ="Timepoint", y = "Log Power")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Log Power Spectral Density (10*log10(uV^2/Hz))", limits=c(3.1, 4.6))

#M2 Difference in Resting Alpha_EC Trier vs. Rest Difference
#Create difference scores 
Diff_PrePost_R_Alpha_EC <- Rest_T_post_Alpha-Rest_T_pre_Alpha
Diff_PrePost_T_Alpha_EC <- T_post_Alpha-T_pre_Alpha
#Create dataframe  
M2_Alpha_EC_Diff.df <- data.frame(SubjectID, Diff_PrePost_R_Alpha_EC, Diff_PrePost_T_Alpha_EC)
#Melt dataframe into long format assumption testing  
longformat.M2_Alpha_EC_Diff.df <- melt(M2_Alpha_EC_Diff.df, id = "SubjectID", variable.name = "Condition")     
#Summarize
summary(longformat.M2_Alpha_EC_Diff.df)
#Visualize all conditions
ggplot(longformat.M2_Alpha_EC_Diff.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EC Alpha Power Difference Post-Pre for Model 2 Trier vs. Rest", x ="Intervention", y = "Log Power")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.M2_Alpha_EC_Diff.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
M2_Alpha_EC_Diff.df <- data.frame(Diff_PrePost_R_Alpha_EC, Diff_PrePost_T_Alpha_EC)     
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.M2_Alpha_EC_Diff.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.M2_Alpha_EC_Diff.df.xlsx') 
RM.M2_Alpha_EC_Diff.df<- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.M2_Alpha_EC_Diff.df.edited.xlsx") 
View(RM.M2_Alpha_EC_Diff.df) 
#Computation
t.test(RM.M2_Alpha_EC_Diff.df$value~RM.M2_Alpha_EC_Diff.df$Intervention, paired = TRUE, p.adjust.method = "bonferroni")      


# RESTING BRAIN FUNCTION OUTCOMES - Resting Posterior Alpha_EO vs Alpha_EC------
#Call variables
P1_Alpha_EO_E_Pre <- df$P1_Alpha_EO_E_Pre
PZ_Alpha_EO_E_Pre <- df$PZ_Alpha_EO_E_Pre
P2_Alpha_EO_E_Pre <- df$P2_Alpha_EO_E_Pre
PO3_Alpha_EO_E_Pre <- df$PO3_Alpha_EO_E_Pre
POZ_Alpha_EO_E_Pre <- df$POZ_Alpha_EO_E_Pre
PO4_Alpha_EO_E_Pre <- df$PO4_Alpha_EO_E_Pre
P1_Alpha_EO_E_Post <- df$P1_Alpha_EO_E_Post
PZ_Alpha_EO_E_Post <- df$PZ_Alpha_EO_E_Post
P2_Alpha_EO_E_Post <- df$P2_Alpha_EO_E_Post
PO3_Alpha_EO_E_Post <- df$PO3_Alpha_EO_E_Post
POZ_Alpha_EO_E_Post <- df$POZ_Alpha_EO_E_Post
PO4_Alpha_EO_E_Post <- df$PO4_Alpha_EO_E_Post
P1_Alpha_EO_Rest_E_Pre <- df$P1_Alpha_EO_Rest_E_Pre
PZ_Alpha_EO_Rest_E_Pre <- df$PZ_Alpha_EO_Rest_E_Pre
P2_Alpha_EO_Rest_E_Pre <- df$P2_Alpha_EO_Rest_E_Pre
PO3_Alpha_EO_Rest_E_Pre <- df$PO3_Alpha_EO_Rest_E_Pre
POZ_Alpha_EO_Rest_E_Pre <- df$POZ_Alpha_EO_Rest_E_Pre
PO4_Alpha_EO_Rest_E_Pre <- df$PO4_Alpha_EO_Rest_E_Pre
P1_Alpha_EO_Rest_E_Post <- df$P1_Alpha_EO_Rest_E_Post
PZ_Alpha_EO_Rest_E_Post <- df$PZ_Alpha_EO_Rest_E_Post
P2_Alpha_EO_Rest_E_Post <- df$P2_Alpha_EO_Rest_E_Post
PO3_Alpha_EO_Rest_E_Post <- df$PO3_Alpha_EO_Rest_E_Post
POZ_Alpha_EO_Rest_E_Post <- df$POZ_Alpha_EO_Rest_E_Post
PO4_Alpha_EO_Rest_E_Post <- df$PO4_Alpha_EO_Rest_E_Post
P1_Alpha_EO_T_Pre <- df$P1_Alpha_EO_T_Pre
PZ_Alpha_EO_T_Pre <- df$PZ_Alpha_EO_T_Pre
P2_Alpha_EO_T_Pre <- df$P2_Alpha_EO_T_Pre
PO3_Alpha_EO_T_Pre <- df$PO3_Alpha_EO_T_Pre
POZ_Alpha_EO_T_Pre <- df$POZ_Alpha_EO_T_Pre
PO4_Alpha_EO_T_Pre <- df$PO4_Alpha_EO_T_Pre
P1_Alpha_EO_T_Post <- df$P1_Alpha_EO_T_Post
PZ_Alpha_EO_T_Post <- df$PZ_Alpha_EO_T_Post
P2_Alpha_EO_T_Post <- df$P2_Alpha_EO_T_Post
PO3_Alpha_EO_T_Post <- df$PO3_Alpha_EO_T_Post
POZ_Alpha_EO_T_Post <- df$POZ_Alpha_EO_T_Post
PO4_Alpha_EO_T_Post <- df$PO4_Alpha_EO_T_Post
P1_Alpha_EO_Rest_T_Pre <- df$P1_Alpha_EO_Rest_T_Pre
PZ_Alpha_EO_Rest_T_Pre <- df$PZ_Alpha_EO_Rest_T_Pre
P2_Alpha_EO_Rest_T_Pre <- df$P2_Alpha_EO_Rest_T_Pre
PO3_Alpha_EO_Rest_T_Pre <- df$PO3_Alpha_EO_Rest_T_Pre
POZ_Alpha_EO_Rest_T_Pre <- df$POZ_Alpha_EO_Rest_T_Pre
PO4_Alpha_EO_Rest_T_Pre <- df$PO4_Alpha_EO_Rest_T_Pre
P1_Alpha_EO_Rest_T_Post <- df$P1_Alpha_EO_Rest_T_Post
PZ_Alpha_EO_Rest_T_Post <- df$PZ_Alpha_EO_Rest_T_Post
P2_Alpha_EO_Rest_T_Post <- df$P2_Alpha_EO_Rest_T_Post
PO3_Alpha_EO_Rest_T_Post <- df$PO3_Alpha_EO_Rest_T_Post
POZ_Alpha_EO_Rest_T_Post <- df$POZ_Alpha_EO_Rest_T_Post
PO4_Alpha_EO_Rest_T_Post <- df$PO4_Alpha_EO_Rest_T_Post

P1_Alpha_EC_E_Pre <- df$P1_Alpha_EC_E_Pre
PZ_Alpha_EC_E_Pre <- df$PZ_Alpha_EC_E_Pre
P2_Alpha_EC_E_Pre <- df$P2_Alpha_EC_E_Pre
PO3_Alpha_EC_E_Pre <- df$PO3_Alpha_EC_E_Pre
POZ_Alpha_EC_E_Pre <- df$POZ_Alpha_EC_E_Pre
PO4_Alpha_EC_E_Pre <- df$PO4_Alpha_EC_E_Pre
P1_Alpha_EC_E_Post <- df$P1_Alpha_EC_E_Post
PZ_Alpha_EC_E_Post <- df$PZ_Alpha_EC_E_Post
P2_Alpha_EC_E_Post <- df$P2_Alpha_EC_E_Post
PO3_Alpha_EC_E_Post <- df$PO3_Alpha_EC_E_Post
POZ_Alpha_EC_E_Post <- df$POZ_Alpha_EC_E_Post
PO4_Alpha_EC_E_Post <- df$PO4_Alpha_EC_E_Post
P1_Alpha_EC_Rest_E_Pre <- df$P1_Alpha_EC_Rest_E_Pre
PZ_Alpha_EC_Rest_E_Pre <- df$PZ_Alpha_EC_Rest_E_Pre
P2_Alpha_EC_Rest_E_Pre <- df$P2_Alpha_EC_Rest_E_Pre
PO3_Alpha_EC_Rest_E_Pre <- df$PO3_Alpha_EC_Rest_E_Pre
POZ_Alpha_EC_Rest_E_Pre <- df$POZ_Alpha_EC_Rest_E_Pre
PO4_Alpha_EC_Rest_E_Pre <- df$PO4_Alpha_EC_Rest_E_Pre
P1_Alpha_EC_Rest_E_Post <- df$P1_Alpha_EC_Rest_E_Post
PZ_Alpha_EC_Rest_E_Post <- df$PZ_Alpha_EC_Rest_E_Post
P2_Alpha_EC_Rest_E_Post <- df$P2_Alpha_EC_Rest_E_Post
PO3_Alpha_EC_Rest_E_Post <- df$PO3_Alpha_EC_Rest_E_Post
POZ_Alpha_EC_Rest_E_Post <- df$POZ_Alpha_EC_Rest_E_Post
PO4_Alpha_EC_Rest_E_Post <- df$PO4_Alpha_EC_Rest_E_Post
P1_Alpha_EC_T_Pre <- df$P1_Alpha_EC_T_Pre
PZ_Alpha_EC_T_Pre <- df$PZ_Alpha_EC_T_Pre
P2_Alpha_EC_T_Pre <- df$P2_Alpha_EC_T_Pre
PO3_Alpha_EC_T_Pre <- df$PO3_Alpha_EC_T_Pre
POZ_Alpha_EC_T_Pre <- df$POZ_Alpha_EC_T_Pre
PO4_Alpha_EC_T_Pre <- df$PO4_Alpha_EC_T_Pre
P1_Alpha_EC_T_Post <- df$P1_Alpha_EC_T_Post
PZ_Alpha_EC_T_Post <- df$PZ_Alpha_EC_T_Post
P2_Alpha_EC_T_Post <- df$P2_Alpha_EC_T_Post
PO3_Alpha_EC_T_Post <- df$PO3_Alpha_EC_T_Post
POZ_Alpha_EC_T_Post <- df$POZ_Alpha_EC_T_Post
PO4_Alpha_EC_T_Post <- df$PO4_Alpha_EC_T_Post
P1_Alpha_EC_Rest_T_Pre <- df$P1_Alpha_EC_Rest_T_Pre
PZ_Alpha_EC_Rest_T_Pre <- df$PZ_Alpha_EC_Rest_T_Pre
P2_Alpha_EC_Rest_T_Pre <- df$P2_Alpha_EC_Rest_T_Pre
PO3_Alpha_EC_Rest_T_Pre <- df$PO3_Alpha_EC_Rest_T_Pre
POZ_Alpha_EC_Rest_T_Pre <- df$POZ_Alpha_EC_Rest_T_Pre
PO4_Alpha_EC_Rest_T_Pre <- df$PO4_Alpha_EC_Rest_T_Pre
P1_Alpha_EC_Rest_T_Post <- df$P1_Alpha_EC_Rest_T_Post
PZ_Alpha_EC_Rest_T_Post <- df$PZ_Alpha_EC_Rest_T_Post
P2_Alpha_EC_Rest_T_Post <- df$P2_Alpha_EC_Rest_T_Post
PO3_Alpha_EC_Rest_T_Post <- df$PO3_Alpha_EC_Rest_T_Post
POZ_Alpha_EC_Rest_T_Post <- df$POZ_Alpha_EC_Rest_T_Post
PO4_Alpha_EC_Rest_T_Post <- df$PO4_Alpha_EC_Rest_T_Post

#M1 in Resting Posterior Alpha between Exercise vs. Rest 
##Average together ROI sites
EO_Rest_E_pre.df <-data.frame(P1_Alpha_EO_Rest_E_Pre, PZ_Alpha_EO_Rest_E_Pre, P2_Alpha_EO_Rest_E_Pre, PO3_Alpha_EO_Rest_E_Pre, POZ_Alpha_EO_Rest_E_Pre, PO4_Alpha_EO_Rest_E_Pre)
EO_Rest_E_pre <- rowMeans(EO_Rest_E_pre.df)
EO_Rest_E_post.df <-data.frame(P1_Alpha_EO_Rest_E_Post, PZ_Alpha_EO_Rest_E_Post, P2_Alpha_EO_Rest_E_Post, PO3_Alpha_EO_Rest_E_Post, POZ_Alpha_EO_Rest_E_Post, PO4_Alpha_EO_Rest_E_Post)
EO_Rest_E_post <- rowMeans(EO_Rest_E_post.df)
EO_E_pre.df <-data.frame(P1_Alpha_EO_E_Pre, PZ_Alpha_EO_E_Pre, P2_Alpha_EO_E_Pre, PO3_Alpha_EO_E_Pre, POZ_Alpha_EO_E_Pre, PO4_Alpha_EO_E_Pre)
EO_E_pre <- rowMeans(EO_E_pre.df)
EO_E_post.df <-data.frame(P1_Alpha_EO_E_Post, PZ_Alpha_EO_E_Post, P2_Alpha_EO_E_Post, PO3_Alpha_EO_E_Post, POZ_Alpha_EO_E_Pre, PO4_Alpha_EO_E_Post)
EO_E_post <- rowMeans(EO_E_post.df)

EC_Rest_E_pre.df <-data.frame(P1_Alpha_EC_Rest_E_Pre, PZ_Alpha_EC_Rest_E_Pre, P2_Alpha_EC_Rest_E_Pre, PO3_Alpha_EC_Rest_E_Pre, POZ_Alpha_EC_Rest_E_Pre, PO4_Alpha_EC_Rest_E_Pre)
EC_Rest_E_pre <- rowMeans(EC_Rest_E_pre.df)
EC_Rest_E_post.df <-data.frame(P1_Alpha_EC_Rest_E_Post, PZ_Alpha_EC_Rest_E_Post, P2_Alpha_EC_Rest_E_Post, PO3_Alpha_EC_Rest_E_Post, POZ_Alpha_EC_Rest_E_Post, PO4_Alpha_EC_Rest_E_Post)
EC_Rest_E_post <- rowMeans(EC_Rest_E_post.df)
EC_E_pre.df <-data.frame(P1_Alpha_EC_E_Pre, PZ_Alpha_EC_E_Pre, P2_Alpha_EC_E_Pre, PO3_Alpha_EC_E_Pre, POZ_Alpha_EC_E_Pre, PO4_Alpha_EC_E_Pre)
EC_E_pre <- rowMeans(EC_E_pre.df)
EC_E_post.df <-data.frame(P1_Alpha_EC_E_Post, PZ_Alpha_EC_E_Post, P2_Alpha_EC_E_Post, PO3_Alpha_EC_E_Post, POZ_Alpha_EC_E_Pre, PO4_Alpha_EC_E_Post)
EC_E_post <- rowMeans(EC_E_post.df)

#M1 All Conditions Computation 
#Create dataframe 
Alpha_EO_EC_M1.df<- data.frame(SubjectID, EO_Rest_E_pre, EC_Rest_E_pre, EO_Rest_E_post, EC_Rest_E_post, 
                               EO_E_pre, EC_E_pre, EO_E_post, EC_E_post)
#Melt dataframe into long format assumption testing  
longformat.Alpha_EO_EC_M1.df <- melt(Alpha_EO_EC_M1.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.Alpha_EO_EC_M1.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.Alpha_EO_EC_M1.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EO & EC Alpha: Exercise vs. Rest", x ="Intervention", y = "Log Power Spectral Density (10*log10(uV^2/Hz))")+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.Alpha_EO_EC_M1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
Alpha_EO_EC_M1.df %>%
  shapiro_test(EO_Rest_E_pre, EC_Rest_E_pre, EO_Rest_E_post, EC_Rest_E_post, 
               EO_E_pre, EC_E_pre, EO_E_post, EC_E_post)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.Alpha_EO_EC_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.Alpha_EO_EC_M1.df.xlsx') 
RM.longformat.Alpha_EO_EC_M1.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.Alpha_EO_EC_M1.df.edited.xlsx")
View(RM.longformat.Alpha_EO_EC_M1.df) 
#Computation
Alpha_EO_EC_M1.aov<-anova_test(data = RM.longformat.Alpha_EO_EC_M1.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint, EC_EO), type = 3, effect.size = "ges")
get_anova_table(Alpha_EO_EC_M1.aov)
#Post Hoc tests
t.test(RM.longformat.Alpha_EO_EC_M1.df$value~RM.longformat.Alpha_EO_EC_M1.df$Timepoint, paired = TRUE, p.adjust.method = "bonferroni")      
t.test(RM.longformat.Alpha_EO_EC_M1.df$value~RM.longformat.Alpha_EO_EC_M1.df$EC_EO, paired = TRUE, p.adjust.method = "bonferroni")      
#Summarize for report 
RM.longformat.Alpha_EO_EC_M1.df%>%
  group_by(Timepoint)%>%
  get_summary_stats(value, type = "mean_sd")

RM.longformat.Alpha_EO_EC_M1.df%>%
  group_by(EC_EO)%>%
  get_summary_stats(value, type = "mean_sd")

#Visualize
      #Reload original data frame
      Alpha_EO_EC_M1.df<- data.frame(SubjectID, EO_Rest_E_pre, EC_Rest_E_pre, EO_Rest_E_post, EC_Rest_E_post, 
                                     EO_E_pre, EC_E_pre, EO_E_post, EC_E_post)
      #Export original data frame
      write_xlsx(Alpha_EO_EC_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.Alpha_EO_EC_M1.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.Alpha_EO_EC_M1.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.Alpha_EO_EC_M1.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.Alpha_EO_EC_M1.df.graph) 
      #Graph 
      ggplot(graph.Alpha_EO_EC_M1.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("violetred1", "steelblue1"))+
        geom_point(size = 2, shape = 16)+
        facet_wrap(~Eye_Status)+
        labs(title="Eyes Open vs. Eyes Closed Alpha Power: Exercise vs. Rest", x ="Timepoint", y = "Log Power Spectral Density (10*log10(uV^2/Hz))")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Log Power Spectral Density (10*log10(uV^2/Hz))", limits=c(3.12, 4.53))


#M2 in Resting Posterior Alpha between Trier vs. Rest 
##Average together ROI sites
EO_Rest_T_pre.df <-data.frame(P1_Alpha_EO_Rest_T_Pre, PZ_Alpha_EO_Rest_T_Pre, P2_Alpha_EO_Rest_T_Pre, PO3_Alpha_EO_Rest_T_Pre, POZ_Alpha_EO_Rest_T_Pre, PO4_Alpha_EO_Rest_T_Pre)
EO_Rest_T_pre <- rowMeans(EO_Rest_T_pre.df)
EO_Rest_T_post.df <-data.frame(P1_Alpha_EO_Rest_T_Post, PZ_Alpha_EO_Rest_T_Post, P2_Alpha_EO_Rest_T_Post, PO3_Alpha_EO_Rest_T_Post, POZ_Alpha_EO_Rest_T_Post, PO4_Alpha_EO_Rest_T_Post)
EO_Rest_T_post <- rowMeans(EO_Rest_T_post.df)
EO_T_pre.df <-data.frame(P1_Alpha_EO_T_Pre, PZ_Alpha_EO_T_Pre, P2_Alpha_EO_T_Pre, PO3_Alpha_EO_T_Pre, POZ_Alpha_EO_T_Pre, PO4_Alpha_EO_T_Pre)
EO_T_pre <- rowMeans(EO_T_pre.df)
EO_T_post.df <-data.frame(P1_Alpha_EO_T_Post, PZ_Alpha_EO_T_Post, P2_Alpha_EO_T_Post, PO3_Alpha_EO_T_Post, POZ_Alpha_EO_T_Pre, PO4_Alpha_EO_T_Post)
EO_T_post <- rowMeans(EO_T_post.df)

EC_Rest_T_pre.df <-data.frame(P1_Alpha_EC_Rest_T_Pre, PZ_Alpha_EC_Rest_T_Pre, P2_Alpha_EC_Rest_T_Pre, PO3_Alpha_EC_Rest_T_Pre, POZ_Alpha_EC_Rest_T_Pre, PO4_Alpha_EC_Rest_T_Pre)
EC_Rest_T_pre <- rowMeans(EC_Rest_T_pre.df)
EC_Rest_T_post.df <-data.frame(P1_Alpha_EC_Rest_T_Post, PZ_Alpha_EC_Rest_T_Post, P2_Alpha_EC_Rest_T_Post, PO3_Alpha_EC_Rest_T_Post, POZ_Alpha_EC_Rest_T_Post, PO4_Alpha_EC_Rest_T_Post)
EC_Rest_T_post <- rowMeans(EC_Rest_T_post.df)
EC_T_pre.df <-data.frame(P1_Alpha_EC_T_Pre, PZ_Alpha_EC_T_Pre, P2_Alpha_EC_T_Pre, PO3_Alpha_EC_T_Pre, POZ_Alpha_EC_T_Pre, PO4_Alpha_EC_T_Pre)
EC_T_pre <- rowMeans(EC_T_pre.df)
EC_T_post.df <-data.frame(P1_Alpha_EC_T_Post, PZ_Alpha_EC_T_Post, P2_Alpha_EC_T_Post, PO3_Alpha_EC_T_Post, POZ_Alpha_EC_T_Pre, PO4_Alpha_EC_T_Post)
EC_T_post <- rowMeans(EC_T_post.df)

#M2 All Conditions Computation 
#Create dataframe 
Alpha_EO_EC_M2.df<- data.frame(SubjectID, EO_Rest_T_pre, EC_Rest_T_pre, EO_Rest_T_post, EC_Rest_T_post, 
                               EO_T_pre, EC_T_pre, EO_T_post, EC_T_post)
#Melt dataframe into long format assumption testing  
longformat.Alpha_EO_EC_M2.df <- melt(Alpha_EO_EC_M2.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.Alpha_EO_EC_M2.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.Alpha_EO_EC_M2.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EO & EC Alpha: Trier vs. Rest", x ="Intervention", y = "Log Power Spectral Density (10*log10(uV^2/Hz))")+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.Alpha_EO_EC_M2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
Alpha_EO_EC_M2.df %>%
  shapiro_test(EO_Rest_T_pre, EC_Rest_T_pre, EO_Rest_T_post, EC_Rest_T_post, 
               EO_T_pre, EC_T_pre, EO_T_post, EC_T_post)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.Alpha_EO_EC_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.Alpha_EO_EC_M2.df.xlsx') 
RM.longformat.Alpha_EO_EC_M2.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.Alpha_EO_EC_M2.df.edited.xlsx")
View(RM.longformat.Alpha_EO_EC_M2.df) 
#Computation
Alpha_EO_EC_M2.aov<-anova_test(data = RM.longformat.Alpha_EO_EC_M2.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint, EC_EO), type = 3, effect.size = "ges")
get_anova_table(Alpha_EO_EC_M2.aov)
#Post Hoc tests
t.test(RM.longformat.Alpha_EO_EC_M2.df$value~RM.longformat.Alpha_EO_EC_M2.df$EC_EO, paired = TRUE, p.adjust.method = "bonferroni")  
#Summarize for report
RM.longformat.Alpha_EO_EC_M2.df%>%
  group_by(EC_EO)%>%
  get_summary_stats(value, type = "mean_sd")
#Visualize
      #Reload original data frame
Alpha_EO_EC_M2.df<- data.frame(SubjectID, EO_Rest_T_pre, EC_Rest_T_pre, EO_Rest_T_post, EC_Rest_T_post, 
                               EO_T_pre, EC_T_pre, EO_T_post, EC_T_post)
      #Export original data frame
      write_xlsx(Alpha_EO_EC_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.Alpha_EO_EC_M2.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.Alpha_EO_EC_M2.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.Alpha_EO_EC_M2.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.Alpha_EO_EC_M2.df.graph) 
      #Graph 
      ggplot(graph.Alpha_EO_EC_M2.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("steelblue1","violetred1"))+
        geom_point(size = 2, shape = 16)+
        facet_wrap(~Eye_Status)+
        labs(title="Eyes Open vs. Eyes Closed Alpha Power: Trier vs. Rest", x ="Timepoint", y = "Log Power Spectral Density (10*log10(uV^2/Hz))")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Log Power Spectral Density (10*log10(uV^2/Hz))", limits=c(3.12, 4.53))

# RESTING BRAIN FUNCTION OUTCOMES - Resting Posterior Theta_EO -----------------------
#Call variables
SubjectID <- (df$SubjectID)
F1_Theta_EO_E_Pre <- df$F1_Theta_EO_E_Pre
FZ_Theta_EO_E_Pre <- df$FZ_Theta_EO_E_Pre
F2_Theta_EO_E_Pre <- df$F2_Theta_EO_E_Pre
FC1_Theta_EO_E_Pre <- df$FC1_Theta_EO_E_Pre
FCZ_Theta_EO_E_Pre <- df$FCZ_Theta_EO_E_Pre
FC2_Theta_EO_E_Pre <- df$FC2_Theta_EO_E_Pre
C1_Theta_EO_E_Pre <- df$C1_Theta_EO_E_Pre
CZ_Theta_EO_E_Pre <- df$CZ_Theta_EO_E_Pre
C2_Theta_EO_E_Pre <- df$C2_Theta_EO_E_Pre
F1_Theta_EO_E_Post <- df$F1_Theta_EO_E_Post
FZ_Theta_EO_E_Post <- df$FZ_Theta_EO_E_Post
F2_Theta_EO_E_Post <- df$F2_Theta_EO_E_Post
FC1_Theta_EO_E_Post <- df$FC1_Theta_EO_E_Post
FCZ_Theta_EO_E_Post <- df$FCZ_Theta_EO_E_Post
FC2_Theta_EO_E_Post <- df$FC2_Theta_EO_E_Post
C1_Theta_EO_E_Post <- df$C1_Theta_EO_E_Post
CZ_Theta_EO_E_Post <- df$CZ_Theta_EO_E_Post
C2_Theta_EO_E_Post <- df$C2_Theta_EO_E_Post
F1_Theta_EO_Rest_E_Pre <- df$F1_Theta_EO_Rest_E_Pre
FZ_Theta_EO_Rest_E_Pre <- df$FZ_Theta_EO_Rest_E_Pre
F2_Theta_EO_Rest_E_Pre <- df$F2_Theta_EO_Rest_E_Pre
FC1_Theta_EO_Rest_E_Pre <- df$FC1_Theta_EO_Rest_E_Pre
FCZ_Theta_EO_Rest_E_Pre <- df$FCZ_Theta_EO_Rest_E_Pre
FC2_Theta_EO_Rest_E_Pre <- df$FC2_Theta_EO_Rest_E_Pre
C1_Theta_EO_Rest_E_Pre <- df$C1_Theta_EO_Rest_E_Pre
CZ_Theta_EO_Rest_E_Pre <- df$CZ_Theta_EO_Rest_E_Pre
C2_Theta_EO_Rest_E_Pre <- df$C2_Theta_EO_Rest_E_Pre
F1_Theta_EO_Rest_E_Post <- df$F1_Theta_EO_Rest_E_Post
FZ_Theta_EO_Rest_E_Post <- df$FZ_Theta_EO_Rest_E_Post
F2_Theta_EO_Rest_E_Post <- df$F2_Theta_EO_Rest_E_Post
FC1_Theta_EO_Rest_E_Post <- df$FC1_Theta_EO_Rest_E_Post
FCZ_Theta_EO_Rest_E_Post <- df$FCZ_Theta_EO_Rest_E_Post
FC2_Theta_EO_Rest_E_Post <- df$FC2_Theta_EO_Rest_E_Post
C1_Theta_EO_Rest_E_Post <- df$C1_Theta_EO_Rest_E_Post
CZ_Theta_EO_Rest_E_Post <- df$CZ_Theta_EO_Rest_E_Post
C2_Theta_EO_Rest_E_Post <- df$C2_Theta_EO_Rest_E_Post
F1_Theta_EO_T_Pre <- df$F1_Theta_EO_T_Pre
FZ_Theta_EO_T_Pre <- df$FZ_Theta_EO_T_Pre
F2_Theta_EO_T_Pre <- df$F2_Theta_EO_T_Pre
FC1_Theta_EO_T_Pre <- df$FC1_Theta_EO_T_Pre
FCZ_Theta_EO_T_Pre <- df$FCZ_Theta_EO_T_Pre
FC2_Theta_EO_T_Pre <- df$FC2_Theta_EO_T_Pre
C1_Theta_EO_T_Pre <- df$C1_Theta_EO_T_Pre
CZ_Theta_EO_T_Pre <- df$CZ_Theta_EO_T_Pre
C2_Theta_EO_T_Pre <- df$C2_Theta_EO_T_Pre
F1_Theta_EO_T_Post <- df$F1_Theta_EO_T_Post
FZ_Theta_EO_T_Post <- df$FZ_Theta_EO_T_Post
F2_Theta_EO_T_Post <- df$F2_Theta_EO_T_Post
FC1_Theta_EO_T_Post <- df$FC1_Theta_EO_T_Post
FCZ_Theta_EO_T_Post <- df$FCZ_Theta_EO_T_Post
FC2_Theta_EO_T_Post <- df$FC2_Theta_EO_T_Post
C1_Theta_EO_T_Post <- df$C1_Theta_EO_T_Post
CZ_Theta_EO_T_Post <- df$CZ_Theta_EO_T_Post
C2_Theta_EO_T_Post <- df$C2_Theta_EO_T_Post
F1_Theta_EO_Rest_T_Pre <- df$F1_Theta_EO_Rest_T_Pre
FZ_Theta_EO_Rest_T_Pre <- df$FZ_Theta_EO_Rest_T_Pre
F2_Theta_EO_Rest_T_Pre <- df$F2_Theta_EO_Rest_T_Pre
FC1_Theta_EO_Rest_T_Pre <- df$FC1_Theta_EO_Rest_T_Pre
FCZ_Theta_EO_Rest_T_Pre <- df$FCZ_Theta_EO_Rest_T_Pre
FC2_Theta_EO_Rest_T_Pre <- df$FC2_Theta_EO_Rest_T_Pre
C1_Theta_EO_Rest_T_Pre <- df$C1_Theta_EO_Rest_T_Pre
CZ_Theta_EO_Rest_T_Pre <- df$CZ_Theta_EO_Rest_T_Pre
C2_Theta_EO_Rest_T_Pre <- df$C2_Theta_EO_Rest_T_Pre
F1_Theta_EO_Rest_T_Post <- df$F1_Theta_EO_Rest_T_Post
FZ_Theta_EO_Rest_T_Post <- df$FZ_Theta_EO_Rest_T_Post
F2_Theta_EO_Rest_T_Post <- df$F2_Theta_EO_Rest_T_Post
FC1_Theta_EO_Rest_T_Post <- df$FC1_Theta_EO_Rest_T_Post
FCZ_Theta_EO_Rest_T_Post <- df$FCZ_Theta_EO_Rest_T_Post
FC2_Theta_EO_Rest_T_Post <- df$FC2_Theta_EO_Rest_T_Post
C1_Theta_EO_Rest_T_Post <- df$C1_Theta_EO_Rest_T_Post
CZ_Theta_EO_Rest_T_Post <- df$CZ_Theta_EO_Rest_T_Post
C2_Theta_EO_Rest_T_Post <- df$C2_Theta_EO_Rest_T_Post


#M1 in Resting Theta_EO between Exercise vs. Rest
#Average together ROI sites
Wide.ROI_FC_E_pre_Theta.df <-data.frame(F1_Theta_EO_E_Pre, FZ_Theta_EO_E_Pre, F2_Theta_EO_E_Pre, FC1_Theta_EO_E_Pre, FCZ_Theta_EO_E_Pre, FC2_Theta_EO_E_Pre, C1_Theta_EO_E_Pre, CZ_Theta_EO_E_Pre, C2_Theta_EO_E_Pre)
E_pre_Theta <- rowMeans(Wide.ROI_FC_E_pre_Theta.df)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_EO_E_Post, FZ_Theta_EO_E_Post, F2_Theta_EO_E_Post, FC1_Theta_EO_E_Post, FCZ_Theta_EO_E_Post, FC2_Theta_EO_E_Post, C1_Theta_EO_E_Post, CZ_Theta_EO_E_Post, C2_Theta_EO_E_Post)
E_post_Theta <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_pre_Theta.df <-data.frame(F1_Theta_EO_Rest_E_Pre, FZ_Theta_EO_Rest_E_Pre, F2_Theta_EO_Rest_E_Pre, FC1_Theta_EO_Rest_E_Pre, FCZ_Theta_EO_Rest_E_Pre, FC2_Theta_EO_Rest_E_Pre, C1_Theta_EO_Rest_E_Pre, CZ_Theta_EO_Rest_E_Pre, C2_Theta_EO_Rest_E_Pre)
Rest_E_pre_Theta <- rowMeans(Wide.ROI_FC_Rest_E_pre_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_EO_Rest_E_Post, FZ_Theta_EO_Rest_E_Post, F2_Theta_EO_Rest_E_Post, FC1_Theta_EO_Rest_E_Post, FCZ_Theta_EO_Rest_E_Post, FC2_Theta_EO_Rest_E_Post, C1_Theta_EO_Rest_E_Post, CZ_Theta_EO_Rest_E_Post, C2_Theta_EO_Rest_E_Post)
Rest_E_post_Theta <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)

#M1 All Conditions Computation 
#Create dataframe 
Theta_EO_M1.df<- data.frame(SubjectID, Rest_E_pre_Theta, Rest_E_post_Theta, E_pre_Theta, E_post_Theta)
#Melt dataframe into long format assumption testing  
longformat.Theta_EO_M1.df <- melt(Theta_EO_M1.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.Theta_EO_M1.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.Theta_EO_M1.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EO Theta for Model 1 Exercise vs. Rest", x ="Intervention", y = "Log Power")+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.Theta_EO_M1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
Theta_EO_M1.df %>%
  shapiro_test(Rest_E_pre_Theta, Rest_E_post_Theta, E_pre_Theta, E_post_Theta)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.Theta_EO_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.Theta_EO_M1.df.xlsx') 
RM.longformat.Theta_EO_M1.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.Theta_EO_M1.df.edited.xlsx")
View(RM.longformat.Theta_EO_M1.df) 
#Computation
Theta_EO_M1.aov<-anova_test(data = RM.longformat.Theta_EO_M1.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint), type = 3, effect.size = "Ges")
get_anova_table(Theta_EO_M1.aov)
#Post Hoc Computations
t.test(RM.longformat.Theta_EO_M1.df$value~RM.longformat.Theta_EO_M1.df$Timepoint, paired = TRUE, p.adjust.method = "bonferroni")
#Summarize for report 
RM.longformat.Theta_EO_M1.df%>%
  group_by(Timepoint)%>%
  get_summary_stats(value, type = "mean_sd")
#Visualize
      #Reload original data frame
      Theta_EO_M1.df<- data.frame(SubjectID, Rest_E_pre_Theta, Rest_E_post_Theta, E_pre_Theta, E_post_Theta)
      #Export original data frame
      write_xlsx(Theta_EO_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.Theta_EO_M1.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.Theta_EO_M1.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.Theta_EO_M1.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.Theta_EO_M1.df.graph) 
      #Graph 
      ggplot(graph.Theta_EO_M1.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("violetred1", "steelblue1"))+
        geom_point(size = 2, shape = 16)+
        labs(title="Resting EO Theta: Exercise vs. Rest", x ="Timepoint", y = "Log Power")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Log Power Spectral Density (10*log10(uV^2/Hz))", limits=c(2.83, 3.2))

#M1 Difference in Resting Theta_EO Exercise vs. Rest Difference
#Create difference scores 
Diff_PrePost_R_Theta_EO <- Rest_E_post_Theta-Rest_E_pre_Theta
Diff_PrePost_E_Theta_EO <- E_post_Theta-E_pre_Theta
#Create dataframe  
M1_Theta_EO_Diff.df <- data.frame(SubjectID, Diff_PrePost_R_Theta_EO, Diff_PrePost_E_Theta_EO)
#Melt dataframe into long format assumption testing  
longformat.M1_Theta_EO_Diff.df <- melt(M1_Theta_EO_Diff.df, id = "SubjectID", variable.name = "Condition")     
#Summarize
summary(longformat.M1_Theta_EO_Diff.df)
#Visualize all conditions
ggplot(longformat.M1_Theta_EO_Diff.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EO Theta Power Difference Post-Pre for Model 1 Exercise vs. Rest", x ="Intervention", y = "Log Power")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.M1_Theta_EO_Diff.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
M1_Theta_EO_Diff.df <- data.frame(Diff_PrePost_R_Theta_EO, Diff_PrePost_E_Theta_EO)     
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.M1_Theta_EO_Diff.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.M1_Theta_EO_Diff.df.xlsx') 
RM.M1_Theta_EO_Diff.df<- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.M1_Theta_EO_Diff.df.edited.xlsx") 
View(RM.M1_Theta_EO_Diff.df) 
#Computation
t.test(RM.M1_Theta_EO_Diff.df$value~RM.M1_Theta_EO_Diff.df$Intervention, paired = TRUE, p.adjust.method = "bonferroni")      

#M2 in Resting Theta_EO between Trier vs. Rest
#Average together ROI sites
Wide.ROI_FC_T_pre_Theta.df <-data.frame(F1_Theta_EO_T_Pre, FZ_Theta_EO_T_Pre, F2_Theta_EO_T_Pre, FC1_Theta_EO_T_Pre, FCZ_Theta_EO_T_Pre, FC2_Theta_EO_T_Pre, C1_Theta_EO_T_Pre, CZ_Theta_EO_T_Pre, C2_Theta_EO_T_Pre)
T_pre_Theta <- rowMeans(Wide.ROI_FC_T_pre_Theta.df)
Wide.ROI_FC_T_post_Theta.df <-data.frame(F1_Theta_EO_T_Post, FZ_Theta_EO_T_Post, F2_Theta_EO_T_Post, FC1_Theta_EO_T_Post, FCZ_Theta_EO_T_Post, FC2_Theta_EO_T_Post, C1_Theta_EO_T_Post, CZ_Theta_EO_T_Post, C2_Theta_EO_T_Post)
T_post_Theta <- rowMeans(Wide.ROI_FC_T_post_Theta.df)
Wide.ROI_FC_Rest_T_pre_Theta.df <-data.frame(F1_Theta_EO_Rest_T_Pre, FZ_Theta_EO_Rest_T_Pre, F2_Theta_EO_Rest_T_Pre, FC1_Theta_EO_Rest_T_Pre, FCZ_Theta_EO_Rest_T_Pre, FC2_Theta_EO_Rest_T_Pre, C1_Theta_EO_Rest_T_Pre, CZ_Theta_EO_Rest_T_Pre, C2_Theta_EO_Rest_T_Pre)
Rest_T_pre_Theta <- rowMeans(Wide.ROI_FC_Rest_T_pre_Theta.df)
Wide.ROI_FC_Rest_T_post_Theta.df <-data.frame(F1_Theta_EO_Rest_T_Post, FZ_Theta_EO_Rest_T_Post, F2_Theta_EO_Rest_T_Post, FC1_Theta_EO_Rest_T_Post, FCZ_Theta_EO_Rest_T_Post, FC2_Theta_EO_Rest_T_Post, C1_Theta_EO_Rest_T_Post, CZ_Theta_EO_Rest_T_Post, C2_Theta_EO_Rest_T_Post)
Rest_T_post_Theta <- rowMeans(Wide.ROI_FC_Rest_T_post_Theta.df)

#M2 All Conditions Computation 
#Create dataframe 
Theta_EO_M2.df<- data.frame(SubjectID, Rest_T_pre_Theta, Rest_T_post_Theta, T_pre_Theta, T_post_Theta)
#Melt dataframe into long format assumption testing  
longformat.Theta_EO_M2.df <- melt(Theta_EO_M2.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.Theta_EO_M2.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.Theta_EO_M2.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EO Theta for Model 2 Trier vs. Rest", x ="Intervention", y = "Log Power")+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.Theta_EO_M2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
Theta_EO_M2.df %>%
  shapiro_test(Rest_T_pre_Theta, Rest_T_post_Theta, T_pre_Theta, T_post_Theta)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.Theta_EO_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.Theta_EO_M2.df.xlsx') 
RM.longformat.Theta_EO_M2.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.Theta_EO_M2.df.edited.xlsx")
View(RM.longformat.Theta_EO_M2.df) 
#Computation
Theta_EO_M2.aov<-anova_test(data = RM.longformat.Theta_EO_M2.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint), type = 3, effect.size = "ges")
get_anova_table(Theta_EO_M2.aov)
#Post Hoc Computations
t.test(RM.longformat.Theta_EO_M2.df$value~RM.longformat.Theta_EO_M2.df$Timepoint, paired = TRUE, p.adjust.method = "bonferroni")
#Summarize for report 
RM.longformat.Theta_EO_M2.df%>%
  group_by(Timepoint)%>%
  get_summary_stats(value, type = "mean_sd")
#Visualize
      #Reload original data frame
      Theta_EO_M2.df<- data.frame(SubjectID, Rest_T_pre_Theta, Rest_T_post_Theta, T_pre_Theta, T_post_Theta)
      #Export original data frame
      write_xlsx(Theta_EO_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.Theta_EO_M2.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.Theta_EO_M2.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.Theta_EO_M2.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.Theta_EO_M2.df.graph) 
      #Graph 
      ggplot(graph.Theta_EO_M2.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("steelblue1", "violetred1"))+
        geom_point(size = 2, shape = 16)+
        labs(title="Resting EO Theta: Trier vs. Rest", x ="Timepoint", y = "Log Power")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Log Power Spectral Density (10*log10(uV^2/Hz))", limits=c(2.83, 3.2))

#M2 Difference in Resting Theta_EO Trier vs. Rest Difference
#Create difference scores 
Diff_PrePost_R_Theta_EO <- Rest_T_post_Theta-Rest_T_pre_Theta
Diff_PrePost_T_Theta_EO <- T_post_Theta-T_pre_Theta
#Create dataframe  
M2_Theta_EO_Diff.df <- data.frame(SubjectID, Diff_PrePost_R_Theta_EO, Diff_PrePost_T_Theta_EO)
#Melt dataframe into long format assumption testing  
longformat.M2_Theta_EO_Diff.df <- melt(M2_Theta_EO_Diff.df, id = "SubjectID", variable.name = "Condition")     
#Summarize
summary(longformat.M2_Theta_EO_Diff.df)
#Visualize all conditions
ggplot(longformat.M2_Theta_EO_Diff.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EO Theta Power Difference Post-Pre for Model 2 Trier vs. Rest", x ="Intervention", y = "Log Power")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.M2_Theta_EO_Diff.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
M2_Theta_EO_Diff.df <- data.frame(Diff_PrePost_R_Theta_EO, Diff_PrePost_T_Theta_EO)     
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.M2_Theta_EO_Diff.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.M2_Theta_EO_Diff.df.xlsx') 
RM.M2_Theta_EO_Diff.df<- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.M2_Theta_EO_Diff.df.edited.xlsx") 
View(RM.M2_Theta_EO_Diff.df) 
#Computation
t.test(RM.M2_Theta_EO_Diff.df$value~RM.M2_Theta_EO_Diff.df$Intervention, paired = TRUE, p.adjust.method = "bonferroni")      

# RESTING BRAIN FUNCTION OUTCOMES - Resting Posterior Theta_EC -----------------------
#Call variables
SubjectID <- (df$SubjectID)
F1_Theta_EC_E_Pre <- df$F1_Theta_EC_E_Pre
FZ_Theta_EC_E_Pre <- df$FZ_Theta_EC_E_Pre
F2_Theta_EC_E_Pre <- df$F2_Theta_EC_E_Pre
FC1_Theta_EC_E_Pre <- df$FC1_Theta_EC_E_Pre
FCZ_Theta_EC_E_Pre <- df$FCZ_Theta_EC_E_Pre
FC2_Theta_EC_E_Pre <- df$FC2_Theta_EC_E_Pre
C1_Theta_EC_E_Pre <- df$C1_Theta_EC_E_Pre
CZ_Theta_EC_E_Pre <- df$CZ_Theta_EC_E_Pre
C2_Theta_EC_E_Pre <- df$C2_Theta_EC_E_Pre
F1_Theta_EC_E_Post <- df$F1_Theta_EC_E_Post
FZ_Theta_EC_E_Post <- df$FZ_Theta_EC_E_Post
F2_Theta_EC_E_Post <- df$F2_Theta_EC_E_Post
FC1_Theta_EC_E_Post <- df$FC1_Theta_EC_E_Post
FCZ_Theta_EC_E_Post <- df$FCZ_Theta_EC_E_Post
FC2_Theta_EC_E_Post <- df$FC2_Theta_EC_E_Post
C1_Theta_EC_E_Post <- df$C1_Theta_EC_E_Post
CZ_Theta_EC_E_Post <- df$CZ_Theta_EC_E_Post
C2_Theta_EC_E_Post <- df$C2_Theta_EC_E_Post
F1_Theta_EC_Rest_E_Pre <- df$F1_Theta_EC_Rest_E_Pre
FZ_Theta_EC_Rest_E_Pre <- df$FZ_Theta_EC_Rest_E_Pre
F2_Theta_EC_Rest_E_Pre <- df$F2_Theta_EC_Rest_E_Pre
FC1_Theta_EC_Rest_E_Pre <- df$FC1_Theta_EC_Rest_E_Pre
FCZ_Theta_EC_Rest_E_Pre <- df$FCZ_Theta_EC_Rest_E_Pre
FC2_Theta_EC_Rest_E_Pre <- df$FC2_Theta_EC_Rest_E_Pre
C1_Theta_EC_Rest_E_Pre <- df$C1_Theta_EC_Rest_E_Pre
CZ_Theta_EC_Rest_E_Pre <- df$CZ_Theta_EC_Rest_E_Pre
C2_Theta_EC_Rest_E_Pre <- df$C2_Theta_EC_Rest_E_Pre
F1_Theta_EC_Rest_E_Post <- df$F1_Theta_EC_Rest_E_Post
FZ_Theta_EC_Rest_E_Post <- df$FZ_Theta_EC_Rest_E_Post
F2_Theta_EC_Rest_E_Post <- df$F2_Theta_EC_Rest_E_Post
FC1_Theta_EC_Rest_E_Post <- df$FC1_Theta_EC_Rest_E_Post
FCZ_Theta_EC_Rest_E_Post <- df$FCZ_Theta_EC_Rest_E_Post
FC2_Theta_EC_Rest_E_Post <- df$FC2_Theta_EC_Rest_E_Post
C1_Theta_EC_Rest_E_Post <- df$C1_Theta_EC_Rest_E_Post
CZ_Theta_EC_Rest_E_Post <- df$CZ_Theta_EC_Rest_E_Post
C2_Theta_EC_Rest_E_Post <- df$C2_Theta_EC_Rest_E_Post
F1_Theta_EC_T_Pre <- df$F1_Theta_EC_T_Pre
FZ_Theta_EC_T_Pre <- df$FZ_Theta_EC_T_Pre
F2_Theta_EC_T_Pre <- df$F2_Theta_EC_T_Pre
FC1_Theta_EC_T_Pre <- df$FC1_Theta_EC_T_Pre
FCZ_Theta_EC_T_Pre <- df$FCZ_Theta_EC_T_Pre
FC2_Theta_EC_T_Pre <- df$FC2_Theta_EC_T_Pre
C1_Theta_EC_T_Pre <- df$C1_Theta_EC_T_Pre
CZ_Theta_EC_T_Pre <- df$CZ_Theta_EC_T_Pre
C2_Theta_EC_T_Pre <- df$C2_Theta_EC_T_Pre
F1_Theta_EC_T_Post <- df$F1_Theta_EC_T_Post
FZ_Theta_EC_T_Post <- df$FZ_Theta_EC_T_Post
F2_Theta_EC_T_Post <- df$F2_Theta_EC_T_Post
FC1_Theta_EC_T_Post <- df$FC1_Theta_EC_T_Post
FCZ_Theta_EC_T_Post <- df$FCZ_Theta_EC_T_Post
FC2_Theta_EC_T_Post <- df$FC2_Theta_EC_T_Post
C1_Theta_EC_T_Post <- df$C1_Theta_EC_T_Post
CZ_Theta_EC_T_Post <- df$CZ_Theta_EC_T_Post
C2_Theta_EC_T_Post <- df$C2_Theta_EC_T_Post
F1_Theta_EC_Rest_T_Pre <- df$F1_Theta_EC_Rest_T_Pre
FZ_Theta_EC_Rest_T_Pre <- df$FZ_Theta_EC_Rest_T_Pre
F2_Theta_EC_Rest_T_Pre <- df$F2_Theta_EC_Rest_T_Pre
FC1_Theta_EC_Rest_T_Pre <- df$FC1_Theta_EC_Rest_T_Pre
FCZ_Theta_EC_Rest_T_Pre <- df$FCZ_Theta_EC_Rest_T_Pre
FC2_Theta_EC_Rest_T_Pre <- df$FC2_Theta_EC_Rest_T_Pre
C1_Theta_EC_Rest_T_Pre <- df$C1_Theta_EC_Rest_T_Pre
CZ_Theta_EC_Rest_T_Pre <- df$CZ_Theta_EC_Rest_T_Pre
C2_Theta_EC_Rest_T_Pre <- df$C2_Theta_EC_Rest_T_Pre
F1_Theta_EC_Rest_T_Post <- df$F1_Theta_EC_Rest_T_Post
FZ_Theta_EC_Rest_T_Post <- df$FZ_Theta_EC_Rest_T_Post
F2_Theta_EC_Rest_T_Post <- df$F2_Theta_EC_Rest_T_Post
FC1_Theta_EC_Rest_T_Post <- df$FC1_Theta_EC_Rest_T_Post
FCZ_Theta_EC_Rest_T_Post <- df$FCZ_Theta_EC_Rest_T_Post
FC2_Theta_EC_Rest_T_Post <- df$FC2_Theta_EC_Rest_T_Post
C1_Theta_EC_Rest_T_Post <- df$C1_Theta_EC_Rest_T_Post
CZ_Theta_EC_Rest_T_Post <- df$CZ_Theta_EC_Rest_T_Post
C2_Theta_EC_Rest_T_Post <- df$C2_Theta_EC_Rest_T_Post

#M1 in Resting Theta_EC between Exercise vs. Rest
#Average together ROI sites
Wide.ROI_FC_E_pre_Theta.df <-data.frame(F1_Theta_EC_E_Pre, FZ_Theta_EC_E_Pre, F2_Theta_EC_E_Pre, FC1_Theta_EC_E_Pre, FCZ_Theta_EC_E_Pre, FC2_Theta_EC_E_Pre, C1_Theta_EC_E_Pre, CZ_Theta_EC_E_Pre, C2_Theta_EC_E_Pre)
E_pre_Theta <- rowMeans(Wide.ROI_FC_E_pre_Theta.df)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_EC_E_Post, FZ_Theta_EC_E_Post, F2_Theta_EC_E_Post, FC1_Theta_EC_E_Post, FCZ_Theta_EC_E_Post, FC2_Theta_EC_E_Post, C1_Theta_EC_E_Post, CZ_Theta_EC_E_Post, C2_Theta_EC_E_Post)
E_post_Theta <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_pre_Theta.df <-data.frame(F1_Theta_EC_Rest_E_Pre, FZ_Theta_EC_Rest_E_Pre, F2_Theta_EC_Rest_E_Pre, FC1_Theta_EC_Rest_E_Pre, FCZ_Theta_EC_Rest_E_Pre, FC2_Theta_EC_Rest_E_Pre, C1_Theta_EC_Rest_E_Pre, CZ_Theta_EC_Rest_E_Pre, C2_Theta_EC_Rest_E_Pre)
Rest_E_pre_Theta <- rowMeans(Wide.ROI_FC_Rest_E_pre_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_EC_Rest_E_Post, FZ_Theta_EC_Rest_E_Post, F2_Theta_EC_Rest_E_Post, FC1_Theta_EC_Rest_E_Post, FCZ_Theta_EC_Rest_E_Post, FC2_Theta_EC_Rest_E_Post, C1_Theta_EC_Rest_E_Post, CZ_Theta_EC_Rest_E_Post, C2_Theta_EC_Rest_E_Post)
Rest_E_post_Theta <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)

#M1 All Conditions Computation 
#Create dataframe 
Theta_EC_M1.df<- data.frame(SubjectID, Rest_E_pre_Theta, Rest_E_post_Theta, E_pre_Theta, E_post_Theta)
#Melt dataframe into long format assumption testing  
longformat.Theta_EC_M1.df <- melt(Theta_EC_M1.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.Theta_EC_M1.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.Theta_EC_M1.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EC Theta for Model 1 Exercise vs. Rest", x ="Intervention", y = "Log Power")+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.Theta_EC_M1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
Theta_EC_M1.df %>%
  shapiro_test(Rest_E_pre_Theta, Rest_E_post_Theta, E_pre_Theta, E_post_Theta)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.Theta_EC_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.Theta_EC_M1.df.xlsx') 
RM.longformat.Theta_EC_M1.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.Theta_EC_M1.df.edited.xlsx")
View(RM.longformat.Theta_EC_M1.df) 
#Computation
Theta_EC_M1.aov<-anova_test(data = RM.longformat.Theta_EC_M1.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint), type = 3, effect.size = "ges")
get_anova_table(Theta_EC_M1.aov)
#Post hoc tests
t.test(RM.longformat.Theta_EC_M1.df$value~RM.longformat.Theta_EC_M1.df$Timepoint, paired = TRUE, p.adjust.method = "bonferroni")
#Summarize for report 
RM.longformat.Theta_EC_M1.df%>%
  group_by(Timepoint)%>%
  get_summary_stats(value, type = "mean_sd")
#Visualize
      #Reload original data frame
      Theta_EC_M1.df<- data.frame(SubjectID, Rest_E_pre_Theta, Rest_E_post_Theta, E_pre_Theta, E_post_Theta)
      #Export original data frame
      write_xlsx(Theta_EC_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.Theta_EC_M1.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.Theta_EC_M1.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.Theta_EC_M1.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.Theta_EC_M1.df.graph) 
      #Graph 
      ggplot(graph.Theta_EC_M1.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("violetred1", "steelblue1" ))+
        geom_point(size = 2, shape = 16)+
        labs(title="Resting EC Theta: Exercise vs. Rest", x ="Timepoint", y = "Log Power")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Log Power Spectral Density (10*log10(uV^2/Hz))", limits=c(3.17, 3.65))

#M1 Difference in Resting Theta_EC Exercise vs. Rest Difference
#Create difference scores 
Diff_PrePost_R_Theta_EC <- Rest_E_post_Theta-Rest_E_pre_Theta
Diff_PrePost_E_Theta_EC <- E_post_Theta-E_pre_Theta
#Create dataframe  
M1_Theta_EC_Diff.df <- data.frame(SubjectID, Diff_PrePost_R_Theta_EC, Diff_PrePost_E_Theta_EC)
#Melt dataframe into long format assumption testing  
longformat.M1_Theta_EC_Diff.df <- melt(M1_Theta_EC_Diff.df, id = "SubjectID", variable.name = "Condition")     
#Summarize
summary(longformat.M1_Theta_EC_Diff.df)
#Visualize all conditions
ggplot(longformat.M1_Theta_EC_Diff.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EC Theta Power Difference Post-Pre for Model 1 Exercise vs. Rest", x ="Intervention", y = "Log Power")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.M1_Theta_EC_Diff.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
M1_Theta_EC_Diff.df <- data.frame(Diff_PrePost_R_Theta_EC, Diff_PrePost_E_Theta_EC)     
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.M1_Theta_EC_Diff.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.M1_Theta_EC_Diff.df.xlsx') 
RM.M1_Theta_EC_Diff.df<- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.M1_Theta_EC_Diff.df.edited.xlsx") 
View(RM.M1_Theta_EC_Diff.df) 
#Computation
t.test(RM.M1_Theta_EC_Diff.df$value~RM.M1_Theta_EC_Diff.df$Intervention, paired = TRUE, p.adjust.method = "bonferroni")      


#M2 in Resting Theta_EC between Trier vs. Rest
#Average together ROI sites
Wide.ROI_FC_T_pre_Theta.df <-data.frame(F1_Theta_EC_T_Pre, FZ_Theta_EC_T_Pre, F2_Theta_EC_T_Pre, FC1_Theta_EC_T_Pre, FCZ_Theta_EC_T_Pre, FC2_Theta_EC_T_Pre, C1_Theta_EC_T_Pre, CZ_Theta_EC_T_Pre, C2_Theta_EC_T_Pre)
T_pre_Theta <- rowMeans(Wide.ROI_FC_T_pre_Theta.df)
Wide.ROI_FC_T_post_Theta.df <-data.frame(F1_Theta_EC_T_Post, FZ_Theta_EC_T_Post, F2_Theta_EC_T_Post, FC1_Theta_EC_T_Post, FCZ_Theta_EC_T_Post, FC2_Theta_EC_T_Post, C1_Theta_EC_T_Post, CZ_Theta_EC_T_Post, C2_Theta_EC_T_Post)
T_post_Theta <- rowMeans(Wide.ROI_FC_T_post_Theta.df)
Wide.ROI_FC_Rest_T_pre_Theta.df <-data.frame(F1_Theta_EC_Rest_T_Pre, FZ_Theta_EC_Rest_T_Pre, F2_Theta_EC_Rest_T_Pre, FC1_Theta_EC_Rest_T_Pre, FCZ_Theta_EC_Rest_T_Pre, FC2_Theta_EC_Rest_T_Pre, C1_Theta_EC_Rest_T_Pre, CZ_Theta_EC_Rest_T_Pre, C2_Theta_EC_Rest_T_Pre)
Rest_T_pre_Theta <- rowMeans(Wide.ROI_FC_Rest_T_pre_Theta.df)
Wide.ROI_FC_Rest_T_post_Theta.df <-data.frame(F1_Theta_EC_Rest_T_Post, FZ_Theta_EC_Rest_T_Post, F2_Theta_EC_Rest_T_Post, FC1_Theta_EC_Rest_T_Post, FCZ_Theta_EC_Rest_T_Post, FC2_Theta_EC_Rest_T_Post, C1_Theta_EC_Rest_T_Post, CZ_Theta_EC_Rest_T_Post, C2_Theta_EC_Rest_T_Post)
Rest_T_post_Theta <- rowMeans(Wide.ROI_FC_Rest_T_post_Theta.df)
#M2 All Conditions Computation 
#Create dataframe 
Theta_EC_M2.df<- data.frame(SubjectID, Rest_T_pre_Theta, Rest_T_post_Theta, T_pre_Theta, T_post_Theta)
#Melt dataframe into long format assumption testing  
longformat.Theta_EC_M2.df <- melt(Theta_EC_M2.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.Theta_EC_M2.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.Theta_EC_M2.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EC Theta for Model 2 Trier vs. Rest", x ="Intervention", y = "Log Power")+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.Theta_EC_M2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
Theta_EC_M2.df %>%
  shapiro_test(Rest_T_pre_Theta, Rest_T_post_Theta, T_pre_Theta, T_post_Theta)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.Theta_EC_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.Theta_EC_M2.df.xlsx') 
RM.longformat.Theta_EC_M2.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.Theta_EC_M2.df.edited.xlsx")
View(RM.longformat.Theta_EC_M2.df) 
#Computation
Theta_EC_M2.aov<-anova_test(data = RM.longformat.Theta_EC_M2.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint), type = 3, effect.size = "ges")
get_anova_table(Theta_EC_M2.aov)
#PostHoc Computations
t.test(RM.longformat.Theta_EC_M2.df$value~RM.longformat.Theta_EC_M2.df$Timepoint, paired = TRUE, p.adjust.method = "bonferroni")
#Summarize for report 
RM.longformat.Theta_EC_M2.df%>%
  group_by(Timepoint)%>%
  get_summary_stats(value, type = "mean_sd")
#Visualize
      #Reload original data frame
      Theta_EC_M2.df<- data.frame(SubjectID, Rest_T_pre_Theta, Rest_T_post_Theta, T_pre_Theta, T_post_Theta)
      #Export original data frame
      write_xlsx(Theta_EC_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.Theta_EC_M2.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.Theta_EC_M2.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.Theta_EC_M2.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.Theta_EC_M2.df.graph) 
      #Graph 
      ggplot(graph.Theta_EC_M2.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("steelblue1", "violetred1"))+
        geom_point(size = 2, shape = 16)+
        labs(title="Resting EC Theta: Trier vs. Rest", x ="Timepoint", y = "Log Power")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Log Power Spectral Density (10*log10(uV^2/Hz))", limits=c(3.17, 3.65))

#M2 Difference in Resting Theta_EC Trier vs. Rest Difference
#Create difference scores 
Diff_PrePost_R_Theta_EC <- Rest_T_post_Theta-Rest_T_pre_Theta
Diff_PrePost_T_Theta_EC <- T_post_Theta-T_pre_Theta
#Create dataframe  
M2_Theta_EC_Diff.df <- data.frame(SubjectID, Diff_PrePost_R_Theta_EC, Diff_PrePost_T_Theta_EC)
#Melt dataframe into long format assumption testing  
longformat.M2_Theta_EC_Diff.df <- melt(M2_Theta_EC_Diff.df, id = "SubjectID", variable.name = "Condition")     
#Summarize
summary(longformat.M2_Theta_EC_Diff.df)
#Visualize all conditions
ggplot(longformat.M2_Theta_EC_Diff.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EC Theta Power Difference Post-Pre for Model 2 Trier vs. Rest", x ="Intervention", y = "Log Power")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#Check for outliers 
longformat.M2_Theta_EC_Diff.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
M2_Theta_EC_Diff.df <- data.frame(Diff_PrePost_R_Theta_EC, Diff_PrePost_T_Theta_EC)     
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.M2_Theta_EC_Diff.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.M2_Theta_EC_Diff.df.xlsx') 
RM.M2_Theta_EC_Diff.df<- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.M2_Theta_EC_Diff.df.edited.xlsx") 
View(RM.M2_Theta_EC_Diff.df) 
#Computation
t.test(RM.M2_Theta_EC_Diff.df$value~RM.M2_Theta_EC_Diff.df$Intervention, paired = TRUE, p.adjust.method = "bonferroni")      


# RESTING BRAIN FUNCTION OUTCOMES - Resting Posterior Theta_EO vs Theta_EC------
#Call variables
SubjectID <- (df$SubjectID)
F1_Theta_EO_E_Pre <- df$F1_Theta_EO_E_Pre
FZ_Theta_EO_E_Pre <- df$FZ_Theta_EO_E_Pre
F2_Theta_EO_E_Pre <- df$F2_Theta_EO_E_Pre
FC1_Theta_EO_E_Pre <- df$FC1_Theta_EO_E_Pre
FCZ_Theta_EO_E_Pre <- df$FCZ_Theta_EO_E_Pre
FC2_Theta_EO_E_Pre <- df$FC2_Theta_EO_E_Pre
C1_Theta_EO_E_Pre <- df$C1_Theta_EO_E_Pre
CZ_Theta_EO_E_Pre <- df$CZ_Theta_EO_E_Pre
C2_Theta_EO_E_Pre <- df$C2_Theta_EO_E_Pre
F1_Theta_EO_E_Post <- df$F1_Theta_EO_E_Post
FZ_Theta_EO_E_Post <- df$FZ_Theta_EO_E_Post
F2_Theta_EO_E_Post <- df$F2_Theta_EO_E_Post
FC1_Theta_EO_E_Post <- df$FC1_Theta_EO_E_Post
FCZ_Theta_EO_E_Post <- df$FCZ_Theta_EO_E_Post
FC2_Theta_EO_E_Post <- df$FC2_Theta_EO_E_Post
C1_Theta_EO_E_Post <- df$C1_Theta_EO_E_Post
CZ_Theta_EO_E_Post <- df$CZ_Theta_EO_E_Post
C2_Theta_EO_E_Post <- df$C2_Theta_EO_E_Post
F1_Theta_EO_Rest_E_Pre <- df$F1_Theta_EO_Rest_E_Pre
FZ_Theta_EO_Rest_E_Pre <- df$FZ_Theta_EO_Rest_E_Pre
F2_Theta_EO_Rest_E_Pre <- df$F2_Theta_EO_Rest_E_Pre
FC1_Theta_EO_Rest_E_Pre <- df$FC1_Theta_EO_Rest_E_Pre
FCZ_Theta_EO_Rest_E_Pre <- df$FCZ_Theta_EO_Rest_E_Pre
FC2_Theta_EO_Rest_E_Pre <- df$FC2_Theta_EO_Rest_E_Pre
C1_Theta_EO_Rest_E_Pre <- df$C1_Theta_EO_Rest_E_Pre
CZ_Theta_EO_Rest_E_Pre <- df$CZ_Theta_EO_Rest_E_Pre
C2_Theta_EO_Rest_E_Pre <- df$C2_Theta_EO_Rest_E_Pre
F1_Theta_EO_Rest_E_Post <- df$F1_Theta_EO_Rest_E_Post
FZ_Theta_EO_Rest_E_Post <- df$FZ_Theta_EO_Rest_E_Post
F2_Theta_EO_Rest_E_Post <- df$F2_Theta_EO_Rest_E_Post
FC1_Theta_EO_Rest_E_Post <- df$FC1_Theta_EO_Rest_E_Post
FCZ_Theta_EO_Rest_E_Post <- df$FCZ_Theta_EO_Rest_E_Post
FC2_Theta_EO_Rest_E_Post <- df$FC2_Theta_EO_Rest_E_Post
C1_Theta_EO_Rest_E_Post <- df$C1_Theta_EO_Rest_E_Post
CZ_Theta_EO_Rest_E_Post <- df$CZ_Theta_EO_Rest_E_Post
C2_Theta_EO_Rest_E_Post <- df$C2_Theta_EO_Rest_E_Post
F1_Theta_EO_T_Pre <- df$F1_Theta_EO_T_Pre
FZ_Theta_EO_T_Pre <- df$FZ_Theta_EO_T_Pre
F2_Theta_EO_T_Pre <- df$F2_Theta_EO_T_Pre
FC1_Theta_EO_T_Pre <- df$FC1_Theta_EO_T_Pre
FCZ_Theta_EO_T_Pre <- df$FCZ_Theta_EO_T_Pre
FC2_Theta_EO_T_Pre <- df$FC2_Theta_EO_T_Pre
C1_Theta_EO_T_Pre <- df$C1_Theta_EO_T_Pre
CZ_Theta_EO_T_Pre <- df$CZ_Theta_EO_T_Pre
C2_Theta_EO_T_Pre <- df$C2_Theta_EO_T_Pre
F1_Theta_EO_T_Post <- df$F1_Theta_EO_T_Post
FZ_Theta_EO_T_Post <- df$FZ_Theta_EO_T_Post
F2_Theta_EO_T_Post <- df$F2_Theta_EO_T_Post
FC1_Theta_EO_T_Post <- df$FC1_Theta_EO_T_Post
FCZ_Theta_EO_T_Post <- df$FCZ_Theta_EO_T_Post
FC2_Theta_EO_T_Post <- df$FC2_Theta_EO_T_Post
C1_Theta_EO_T_Post <- df$C1_Theta_EO_T_Post
CZ_Theta_EO_T_Post <- df$CZ_Theta_EO_T_Post
C2_Theta_EO_T_Post <- df$C2_Theta_EO_T_Post
F1_Theta_EO_Rest_T_Pre <- df$F1_Theta_EO_Rest_T_Pre
FZ_Theta_EO_Rest_T_Pre <- df$FZ_Theta_EO_Rest_T_Pre
F2_Theta_EO_Rest_T_Pre <- df$F2_Theta_EO_Rest_T_Pre
FC1_Theta_EO_Rest_T_Pre <- df$FC1_Theta_EO_Rest_T_Pre
FCZ_Theta_EO_Rest_T_Pre <- df$FCZ_Theta_EO_Rest_T_Pre
FC2_Theta_EO_Rest_T_Pre <- df$FC2_Theta_EO_Rest_T_Pre
C1_Theta_EO_Rest_T_Pre <- df$C1_Theta_EO_Rest_T_Pre
CZ_Theta_EO_Rest_T_Pre <- df$CZ_Theta_EO_Rest_T_Pre
C2_Theta_EO_Rest_T_Pre <- df$C2_Theta_EO_Rest_T_Pre
F1_Theta_EO_Rest_T_Post <- df$F1_Theta_EO_Rest_T_Post
FZ_Theta_EO_Rest_T_Post <- df$FZ_Theta_EO_Rest_T_Post
F2_Theta_EO_Rest_T_Post <- df$F2_Theta_EO_Rest_T_Post
FC1_Theta_EO_Rest_T_Post <- df$FC1_Theta_EO_Rest_T_Post
FCZ_Theta_EO_Rest_T_Post <- df$FCZ_Theta_EO_Rest_T_Post
FC2_Theta_EO_Rest_T_Post <- df$FC2_Theta_EO_Rest_T_Post
C1_Theta_EO_Rest_T_Post <- df$C1_Theta_EO_Rest_T_Post
CZ_Theta_EO_Rest_T_Post <- df$CZ_Theta_EO_Rest_T_Post
C2_Theta_EO_Rest_T_Post <- df$C2_Theta_EO_Rest_T_Post

F1_Theta_EC_E_Pre <- df$F1_Theta_EC_E_Pre
FZ_Theta_EC_E_Pre <- df$FZ_Theta_EC_E_Pre
F2_Theta_EC_E_Pre <- df$F2_Theta_EC_E_Pre
FC1_Theta_EC_E_Pre <- df$FC1_Theta_EC_E_Pre
FCZ_Theta_EC_E_Pre <- df$FCZ_Theta_EC_E_Pre
FC2_Theta_EC_E_Pre <- df$FC2_Theta_EC_E_Pre
C1_Theta_EC_E_Pre <- df$C1_Theta_EC_E_Pre
CZ_Theta_EC_E_Pre <- df$CZ_Theta_EC_E_Pre
C2_Theta_EC_E_Pre <- df$C2_Theta_EC_E_Pre
F1_Theta_EC_E_Post <- df$F1_Theta_EC_E_Post
FZ_Theta_EC_E_Post <- df$FZ_Theta_EC_E_Post
F2_Theta_EC_E_Post <- df$F2_Theta_EC_E_Post
FC1_Theta_EC_E_Post <- df$FC1_Theta_EC_E_Post
FCZ_Theta_EC_E_Post <- df$FCZ_Theta_EC_E_Post
FC2_Theta_EC_E_Post <- df$FC2_Theta_EC_E_Post
C1_Theta_EC_E_Post <- df$C1_Theta_EC_E_Post
CZ_Theta_EC_E_Post <- df$CZ_Theta_EC_E_Post
C2_Theta_EC_E_Post <- df$C2_Theta_EC_E_Post
F1_Theta_EC_Rest_E_Pre <- df$F1_Theta_EC_Rest_E_Pre
FZ_Theta_EC_Rest_E_Pre <- df$FZ_Theta_EC_Rest_E_Pre
F2_Theta_EC_Rest_E_Pre <- df$F2_Theta_EC_Rest_E_Pre
FC1_Theta_EC_Rest_E_Pre <- df$FC1_Theta_EC_Rest_E_Pre
FCZ_Theta_EC_Rest_E_Pre <- df$FCZ_Theta_EC_Rest_E_Pre
FC2_Theta_EC_Rest_E_Pre <- df$FC2_Theta_EC_Rest_E_Pre
C1_Theta_EC_Rest_E_Pre <- df$C1_Theta_EC_Rest_E_Pre
CZ_Theta_EC_Rest_E_Pre <- df$CZ_Theta_EC_Rest_E_Pre
C2_Theta_EC_Rest_E_Pre <- df$C2_Theta_EC_Rest_E_Pre
F1_Theta_EC_Rest_E_Post <- df$F1_Theta_EC_Rest_E_Post
FZ_Theta_EC_Rest_E_Post <- df$FZ_Theta_EC_Rest_E_Post
F2_Theta_EC_Rest_E_Post <- df$F2_Theta_EC_Rest_E_Post
FC1_Theta_EC_Rest_E_Post <- df$FC1_Theta_EC_Rest_E_Post
FCZ_Theta_EC_Rest_E_Post <- df$FCZ_Theta_EC_Rest_E_Post
FC2_Theta_EC_Rest_E_Post <- df$FC2_Theta_EC_Rest_E_Post
C1_Theta_EC_Rest_E_Post <- df$C1_Theta_EC_Rest_E_Post
CZ_Theta_EC_Rest_E_Post <- df$CZ_Theta_EC_Rest_E_Post
C2_Theta_EC_Rest_E_Post <- df$C2_Theta_EC_Rest_E_Post
F1_Theta_EC_T_Pre <- df$F1_Theta_EC_T_Pre
FZ_Theta_EC_T_Pre <- df$FZ_Theta_EC_T_Pre
F2_Theta_EC_T_Pre <- df$F2_Theta_EC_T_Pre
FC1_Theta_EC_T_Pre <- df$FC1_Theta_EC_T_Pre
FCZ_Theta_EC_T_Pre <- df$FCZ_Theta_EC_T_Pre
FC2_Theta_EC_T_Pre <- df$FC2_Theta_EC_T_Pre
C1_Theta_EC_T_Pre <- df$C1_Theta_EC_T_Pre
CZ_Theta_EC_T_Pre <- df$CZ_Theta_EC_T_Pre
C2_Theta_EC_T_Pre <- df$C2_Theta_EC_T_Pre
F1_Theta_EC_T_Post <- df$F1_Theta_EC_T_Post
FZ_Theta_EC_T_Post <- df$FZ_Theta_EC_T_Post
F2_Theta_EC_T_Post <- df$F2_Theta_EC_T_Post
FC1_Theta_EC_T_Post <- df$FC1_Theta_EC_T_Post
FCZ_Theta_EC_T_Post <- df$FCZ_Theta_EC_T_Post
FC2_Theta_EC_T_Post <- df$FC2_Theta_EC_T_Post
C1_Theta_EC_T_Post <- df$C1_Theta_EC_T_Post
CZ_Theta_EC_T_Post <- df$CZ_Theta_EC_T_Post
C2_Theta_EC_T_Post <- df$C2_Theta_EC_T_Post
F1_Theta_EC_Rest_T_Pre <- df$F1_Theta_EC_Rest_T_Pre
FZ_Theta_EC_Rest_T_Pre <- df$FZ_Theta_EC_Rest_T_Pre
F2_Theta_EC_Rest_T_Pre <- df$F2_Theta_EC_Rest_T_Pre
FC1_Theta_EC_Rest_T_Pre <- df$FC1_Theta_EC_Rest_T_Pre
FCZ_Theta_EC_Rest_T_Pre <- df$FCZ_Theta_EC_Rest_T_Pre
FC2_Theta_EC_Rest_T_Pre <- df$FC2_Theta_EC_Rest_T_Pre
C1_Theta_EC_Rest_T_Pre <- df$C1_Theta_EC_Rest_T_Pre
CZ_Theta_EC_Rest_T_Pre <- df$CZ_Theta_EC_Rest_T_Pre
C2_Theta_EC_Rest_T_Pre <- df$C2_Theta_EC_Rest_T_Pre
F1_Theta_EC_Rest_T_Post <- df$F1_Theta_EC_Rest_T_Post
FZ_Theta_EC_Rest_T_Post <- df$FZ_Theta_EC_Rest_T_Post
F2_Theta_EC_Rest_T_Post <- df$F2_Theta_EC_Rest_T_Post
FC1_Theta_EC_Rest_T_Post <- df$FC1_Theta_EC_Rest_T_Post
FCZ_Theta_EC_Rest_T_Post <- df$FCZ_Theta_EC_Rest_T_Post
FC2_Theta_EC_Rest_T_Post <- df$FC2_Theta_EC_Rest_T_Post
C1_Theta_EC_Rest_T_Post <- df$C1_Theta_EC_Rest_T_Post
CZ_Theta_EC_Rest_T_Post <- df$CZ_Theta_EC_Rest_T_Post
C2_Theta_EC_Rest_T_Post <- df$C2_Theta_EC_Rest_T_Post

#M1 in Resting Posterior Theta between Exercise vs. Rest 
##Average together ROI sites
Wide.ROI_EC_E_pre_Theta.df <-data.frame(F1_Theta_EC_E_Pre, FZ_Theta_EC_E_Pre, F2_Theta_EC_E_Pre, FC1_Theta_EC_E_Pre, FCZ_Theta_EC_E_Pre, FC2_Theta_EC_E_Pre, C1_Theta_EC_E_Pre, CZ_Theta_EC_E_Pre, C2_Theta_EC_E_Pre)
EC_E_pre <- rowMeans(Wide.ROI_EC_E_pre_Theta.df)
Wide.ROI_EC_E_post_Theta.df <-data.frame(F1_Theta_EC_E_Post, FZ_Theta_EC_E_Post, F2_Theta_EC_E_Post, FC1_Theta_EC_E_Post, FCZ_Theta_EC_E_Post, FC2_Theta_EC_E_Post, C1_Theta_EC_E_Post, CZ_Theta_EC_E_Post, C2_Theta_EC_E_Post)
EC_E_post <- rowMeans(Wide.ROI_EC_E_post_Theta.df)
Wide.ROI_EC_Rest_E_pre_Theta.df <-data.frame(F1_Theta_EC_Rest_E_Pre, FZ_Theta_EC_Rest_E_Pre, F2_Theta_EC_Rest_E_Pre, FC1_Theta_EC_Rest_E_Pre, FCZ_Theta_EC_Rest_E_Pre, FC2_Theta_EC_Rest_E_Pre, C1_Theta_EC_Rest_E_Pre, CZ_Theta_EC_Rest_E_Pre, C2_Theta_EC_Rest_E_Pre)
EC_Rest_E_pre <- rowMeans(Wide.ROI_EC_Rest_E_pre_Theta.df)
Wide.ROI_EC_Rest_E_post_Theta.df <-data.frame(F1_Theta_EC_Rest_E_Post, FZ_Theta_EC_Rest_E_Post, F2_Theta_EC_Rest_E_Post, FC1_Theta_EC_Rest_E_Post, FCZ_Theta_EC_Rest_E_Post, FC2_Theta_EC_Rest_E_Post, C1_Theta_EC_Rest_E_Post, CZ_Theta_EC_Rest_E_Post, C2_Theta_EC_Rest_E_Post)
EC_Rest_E_post <- rowMeans(Wide.ROI_EC_Rest_E_post_Theta.df)

Wide.ROI_EO_E_pre_Theta.df <-data.frame(F1_Theta_EO_E_Pre, FZ_Theta_EO_E_Pre, F2_Theta_EO_E_Pre, FC1_Theta_EO_E_Pre, FCZ_Theta_EO_E_Pre, FC2_Theta_EO_E_Pre, C1_Theta_EO_E_Pre, CZ_Theta_EO_E_Pre, C2_Theta_EO_E_Pre)
EO_E_pre <- rowMeans(Wide.ROI_EO_E_pre_Theta.df)
Wide.ROI_EO_E_post_Theta.df <-data.frame(F1_Theta_EO_E_Post, FZ_Theta_EO_E_Post, F2_Theta_EO_E_Post, FC1_Theta_EO_E_Post, FCZ_Theta_EO_E_Post, FC2_Theta_EO_E_Post, C1_Theta_EO_E_Post, CZ_Theta_EO_E_Post, C2_Theta_EO_E_Post)
EO_E_post <- rowMeans(Wide.ROI_EO_E_post_Theta.df)
Wide.ROI_EO_Rest_E_pre_Theta.df <-data.frame(F1_Theta_EO_Rest_E_Pre, FZ_Theta_EO_Rest_E_Pre, F2_Theta_EO_Rest_E_Pre, FC1_Theta_EO_Rest_E_Pre, FCZ_Theta_EO_Rest_E_Pre, FC2_Theta_EO_Rest_E_Pre, C1_Theta_EO_Rest_E_Pre, CZ_Theta_EO_Rest_E_Pre, C2_Theta_EO_Rest_E_Pre)
EO_Rest_E_pre <- rowMeans(Wide.ROI_EO_Rest_E_pre_Theta.df)
Wide.ROI_EO_Rest_E_post_Theta.df <-data.frame(F1_Theta_EO_Rest_E_Post, FZ_Theta_EO_Rest_E_Post, F2_Theta_EO_Rest_E_Post, FC1_Theta_EO_Rest_E_Post, FCZ_Theta_EO_Rest_E_Post, FC2_Theta_EO_Rest_E_Post, C1_Theta_EO_Rest_E_Post, CZ_Theta_EO_Rest_E_Post, C2_Theta_EO_Rest_E_Post)
EO_Rest_E_post <- rowMeans(Wide.ROI_EO_Rest_E_post_Theta.df)

#M1 All Conditions Computation 
#Create dataframe 
Theta_EO_EC_M1.df<- data.frame(SubjectID, EO_Rest_E_pre, EC_Rest_E_pre, EO_Rest_E_post, EC_Rest_E_post, 
                               EO_E_pre, EC_E_pre, EO_E_post, EC_E_post)
#Melt dataframe into long format assumption testing  
longformat.Theta_EO_EC_M1.df <- melt(Theta_EO_EC_M1.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.Theta_EO_EC_M1.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.Theta_EO_EC_M1.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EO & EC Theta: Exercise vs. Rest", x ="Intervention", y = "Log Power Spectral Density (10*log10(uV^2/Hz))")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(name="Log Power Spectral Density (10*log10(uV^2/Hz))", limits=c(0, 5.3))
#Check for outliers 
longformat.Theta_EO_EC_M1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
Theta_EO_EC_M1.df %>%
  shapiro_test(EO_Rest_E_pre, EC_Rest_E_pre, EO_Rest_E_post, EC_Rest_E_post, 
               EO_E_pre, EC_E_pre, EO_E_post, EC_E_post)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.Theta_EO_EC_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.Theta_EO_EC_M1.df.xlsx') 
RM.longformat.Theta_EO_EC_M1.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.Theta_EO_EC_M1.df.edited.xlsx")
View(RM.longformat.Theta_EO_EC_M1.df) 
#Computation
Theta_EO_EC_M1.aov<-anova_test(data = RM.longformat.Theta_EO_EC_M1.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint, EO_EC), type = 3, effect.size = "ges")
get_anova_table(Theta_EO_EC_M1.aov)
#Post Hoc tests
t.test(RM.longformat.Theta_EO_EC_M1.df$value~RM.longformat.Theta_EO_EC_M1.df$Timepoint, paired = TRUE, p.adjust.method = "bonferroni")      
t.test(RM.longformat.Theta_EO_EC_M1.df$value~RM.longformat.Theta_EO_EC_M1.df$EO_EC, paired = TRUE, p.adjust.method = "bonferroni")      

interaction.aov<- RM.longformat.Theta_EO_EC_M1.df %>%
  group_by(EO_EC, Intervention) %>%
  anova_test(dv = value, wid = SubjectID, within = Timepoint) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
interaction.aov

pairwise.aov <- RM.longformat.Theta_EO_EC_M1.df %>%
  group_by(EO_EC, Intervention) %>%
  pairwise_t_test(
    value ~ Timepoint, paired = TRUE,
    p.adjust.method = "bonferroni")
pairwise.aov

interaction.aov<- RM.longformat.Theta_EO_EC_M1.df %>%
  group_by(Timepoint, Intervention) %>%
  anova_test(dv = value, wid = SubjectID, within = EO_EC) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
interaction.aov

pairwise.aov <- RM.longformat.Theta_EO_EC_M1.df %>%
  group_by(Timepoint, Intervention) %>%
  pairwise_t_test(
    value ~ EO_EC, paired = TRUE,
    p.adjust.method = "bonferroni")
pairwise.aov
#Summarize for report 
RM.longformat.Theta_EO_EC_M1.df%>%
  group_by(Timepoint)%>%
  get_summary_stats(value, type = "mean_sd")

RM.longformat.Theta_EO_EC_M1.df%>%
  group_by(EO_EC)%>%
  get_summary_stats(value, type = "mean_sd")
#Visualize
      #Reload original data frame
      Theta_EO_EC_M1.df<- data.frame(SubjectID, EO_Rest_E_pre, EC_Rest_E_pre, EO_Rest_E_post, EC_Rest_E_post, 
                              EO_E_pre, EC_E_pre, EO_E_post, EC_E_post)
      #Export original data frame
      write_xlsx(Theta_EO_EC_M1.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.Theta_EO_EC_M1.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.Theta_EO_EC_M1.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.Theta_EO_EC_M1.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.Theta_EO_EC_M1.df.graph) 
      #Graph 
      ggplot(graph.Theta_EO_EC_M1.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("violetred1", "steelblue1"))+
        geom_point(size = 2, shape = 16)+
        facet_wrap(~Eye_Status)+
        labs(title="Eyes Open vs. Eyes Closed Theta Power: Exercise vs. Rest", x ="Timepoint", y = "Log Power Spectral Density (10*log10(uV^2/Hz))")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Log Power Spectral Density (10*log10(uV^2/Hz))", limits=c(2.5, 4.5))
      
#M2 in Resting Posterior Theta between Trier vs. Rest 
##Average together ROI sites
Wide.ROI_FC_T_pre_Theta.df <-data.frame(F1_Theta_EO_T_Pre, FZ_Theta_EO_T_Pre, F2_Theta_EO_T_Pre, FC1_Theta_EO_T_Pre, FCZ_Theta_EO_T_Pre, FC2_Theta_EO_T_Pre, C1_Theta_EO_T_Pre, CZ_Theta_EO_T_Pre, C2_Theta_EO_T_Pre)
EO_T_pre <- rowMeans(Wide.ROI_FC_T_pre_Theta.df)
Wide.ROI_FC_T_post_Theta.df <-data.frame(F1_Theta_EO_T_Post, FZ_Theta_EO_T_Post, F2_Theta_EO_T_Post, FC1_Theta_EO_T_Post, FCZ_Theta_EO_T_Post, FC2_Theta_EO_T_Post, C1_Theta_EO_T_Post, CZ_Theta_EO_T_Post, C2_Theta_EO_T_Post)
EO_T_post <- rowMeans(Wide.ROI_FC_T_post_Theta.df)
Wide.ROI_FC_Rest_T_pre_Theta.df <-data.frame(F1_Theta_EO_Rest_T_Pre, FZ_Theta_EO_Rest_T_Pre, F2_Theta_EO_Rest_T_Pre, FC1_Theta_EO_Rest_T_Pre, FCZ_Theta_EO_Rest_T_Pre, FC2_Theta_EO_Rest_T_Pre, C1_Theta_EO_Rest_T_Pre, CZ_Theta_EO_Rest_T_Pre, C2_Theta_EO_Rest_T_Pre)
EO_Rest_T_pre<- rowMeans(Wide.ROI_FC_Rest_T_pre_Theta.df)
Wide.ROI_FC_Rest_T_post_Theta.df <-data.frame(F1_Theta_EO_Rest_T_Post, FZ_Theta_EO_Rest_T_Post, F2_Theta_EO_Rest_T_Post, FC1_Theta_EO_Rest_T_Post, FCZ_Theta_EO_Rest_T_Post, FC2_Theta_EO_Rest_T_Post, C1_Theta_EO_Rest_T_Post, CZ_Theta_EO_Rest_T_Post, C2_Theta_EO_Rest_T_Post)
EO_Rest_T_post<- rowMeans(Wide.ROI_FC_Rest_T_post_Theta.df)

Wide.ROI_FC_T_pre_Theta.df <-data.frame(F1_Theta_EC_T_Pre, FZ_Theta_EC_T_Pre, F2_Theta_EC_T_Pre, FC1_Theta_EC_T_Pre, FCZ_Theta_EC_T_Pre, FC2_Theta_EC_T_Pre, C1_Theta_EC_T_Pre, CZ_Theta_EC_T_Pre, C2_Theta_EC_T_Pre)
EC_T_pre<- rowMeans(Wide.ROI_FC_T_pre_Theta.df)
Wide.ROI_FC_T_post_Theta.df <-data.frame(F1_Theta_EC_T_Post, FZ_Theta_EC_T_Post, F2_Theta_EC_T_Post, FC1_Theta_EC_T_Post, FCZ_Theta_EC_T_Post, FC2_Theta_EC_T_Post, C1_Theta_EC_T_Post, CZ_Theta_EC_T_Post, C2_Theta_EC_T_Post)
EC_T_post <- rowMeans(Wide.ROI_FC_T_post_Theta.df)
Wide.ROI_FC_Rest_T_pre_Theta.df <-data.frame(F1_Theta_EC_Rest_T_Pre, FZ_Theta_EC_Rest_T_Pre, F2_Theta_EC_Rest_T_Pre, FC1_Theta_EC_Rest_T_Pre, FCZ_Theta_EC_Rest_T_Pre, FC2_Theta_EC_Rest_T_Pre, C1_Theta_EC_Rest_T_Pre, CZ_Theta_EC_Rest_T_Pre, C2_Theta_EC_Rest_T_Pre)
EC_Rest_T_pre <- rowMeans(Wide.ROI_FC_Rest_T_pre_Theta.df)
Wide.ROI_FC_Rest_T_post_Theta.df <-data.frame(F1_Theta_EC_Rest_T_Post, FZ_Theta_EC_Rest_T_Post, F2_Theta_EC_Rest_T_Post, FC1_Theta_EC_Rest_T_Post, FCZ_Theta_EC_Rest_T_Post, FC2_Theta_EC_Rest_T_Post, C1_Theta_EC_Rest_T_Post, CZ_Theta_EC_Rest_T_Post, C2_Theta_EC_Rest_T_Post)
EC_Rest_T_post <- rowMeans(Wide.ROI_FC_Rest_T_post_Theta.df)

#M2 All Conditions Computation 
#Create dataframe 
Theta_EO_EC_M2.df<- data.frame(SubjectID, EO_Rest_T_pre, EC_Rest_T_pre, EO_Rest_T_post, EC_Rest_T_post, 
                               EO_T_pre, EC_T_pre, EO_T_post, EC_T_post)
#Melt dataframe into long format assumption testing  
longformat.Theta_EO_EC_M2.df <- melt(Theta_EO_EC_M2.df, id = "SubjectID", variable.name = "Condition")
#Summarize
longformat.Theta_EO_EC_M2.df %>%
  group_by(Condition) %>%
  get_summary_stats(value, type = "mean_sd")
#Visualize all conditions
ggplot(longformat.Theta_EO_EC_M2.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Resting EO & EC Theta: Trier vs. Rest", x ="Intervention", y = "Log Power Spectral Density (10*log10(uV^2/Hz))")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(name="Log Power Spectral Density (10*log10(uV^2/Hz))", limits=c(0, 5.3))
#Check for outliers 
longformat.Theta_EO_EC_M2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#Check for normality
Theta_EO_EC_M2.df %>%
  shapiro_test(EO_Rest_T_pre, EC_Rest_T_pre, EO_Rest_T_post, EC_Rest_T_post, 
               EO_T_pre, EC_T_pre, EO_T_post, EC_T_post)
#Manually reformat for RM ANOVA (2x2x2)
write_xlsx(longformat.Theta_EO_EC_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//RM.Theta_EO_EC_M2.df.xlsx') 
RM.longformat.Theta_EO_EC_M2.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/RM.Theta_EO_EC_M2.df.edited.xlsx")
View(RM.longformat.Theta_EO_EC_M2.df) 
#Computation
Theta_EO_EC_M2.aov<-anova_test(data = RM.longformat.Theta_EO_EC_M2.df, dv = value, wid = SubjectID, within =  c(Intervention, Timepoint, EO_EC), type = 3, effect.size = "ges")
get_anova_table(Theta_EO_EC_M2.aov)
#Post Hoc tests
t.test(RM.longformat.Theta_EO_EC_M2.df$value~RM.longformat.Theta_EO_EC_M2.df$EO_EC, paired = TRUE, p.adjust.method = "bonferroni")       
t.test(RM.longformat.Theta_EO_EC_M2.df$value~RM.longformat.Theta_EO_EC_M2.df$Timepoint, paired = TRUE, p.adjust.method = "bonferroni")       
#Summarize for report 
RM.longformat.Theta_EO_EC_M2.df%>%
  group_by(Timepoint)%>%
  get_summary_stats(value, type = "mean_sd")
RM.longformat.Theta_EO_EC_M2.df%>%
  group_by(EO_EC)%>%
  get_summary_stats(value, type = "mean_sd")
#Visualize
      #Reload original data frame
Theta_EO_EC_M2.df<- data.frame(SubjectID, EO_Rest_T_pre, EC_Rest_T_pre, EO_Rest_T_post, EC_Rest_T_post, 
                               EO_T_pre, EC_T_pre, EO_T_post, EC_T_post)
      #Export original data frame
      write_xlsx(Theta_EO_EC_M2.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//graph.Theta_EO_EC_M2.df.xlsx') 
      #Calculate means and SE in small df, repload as .edited; mutate for logical Pre Post
      graph.Theta_EO_EC_M2.df.graph <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/graph.Theta_EO_EC_M2.df.edited.xlsx")  %>%
        mutate(Timepoint = fct_relevel(Timepoint, "Pre", "Post")) 
      #View to confirm correct format
      View(graph.Theta_EO_EC_M2.df.graph) 
      #Graph 
      ggplot(graph.Theta_EO_EC_M2.df.graph, aes(x=Timepoint, y=Mean, group=Intervention, color=Intervention)) + 
        geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, 
                      position=position_dodge(0.05)) +
        geom_line(linewidth = 1)+
        scale_color_manual(values=c("steelblue1","violetred1"))+
        geom_point(size = 2, shape = 16)+
        facet_wrap(~Eye_Status)+
        labs(title="Eyes Open vs. Eyes Closed Theta Power: Trier vs. Rest", x ="Timepoint", y = "Log Power Spectral Density (10*log10(uV^2/Hz))")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="Log Power Spectral Density (10*log10(uV^2/Hz))", limits=c(2.5, 4.5))
      
      
# COGNITIVE AND RESTING BRAIN FUNCTION OUTCOMES - Alpha_EO and P3 Peak ----------------
#Call Variables 
SubjectID<-df$SubjectID
P1_Alpha_EO_E_Pre <- df$P1_Alpha_EO_E_Pre
PZ_Alpha_EO_E_Pre <- df$PZ_Alpha_EO_E_Pre
P2_Alpha_EO_E_Pre <- df$P2_Alpha_EO_E_Pre
PO3_Alpha_EO_E_Pre <- df$PO3_Alpha_EO_E_Pre
POZ_Alpha_EO_E_Pre <- df$POZ_Alpha_EO_E_Pre
PO4_Alpha_EO_E_Pre <- df$PO4_Alpha_EO_E_Pre
P1_Alpha_EO_E_Post <- df$P1_Alpha_EO_E_Post
PZ_Alpha_EO_E_Post <- df$PZ_Alpha_EO_E_Post
P2_Alpha_EO_E_Post <- df$P2_Alpha_EO_E_Post
PO3_Alpha_EO_E_Post <- df$PO3_Alpha_EO_E_Post
POZ_Alpha_EO_E_Post <- df$POZ_Alpha_EO_E_Post
PO4_Alpha_EO_E_Post <- df$PO4_Alpha_EO_E_Post
P1_Alpha_EO_Rest_E_Pre <- df$P1_Alpha_EO_Rest_E_Pre
PZ_Alpha_EO_Rest_E_Pre <- df$PZ_Alpha_EO_Rest_E_Pre
P2_Alpha_EO_Rest_E_Pre <- df$P2_Alpha_EO_Rest_E_Pre
PO3_Alpha_EO_Rest_E_Pre <- df$PO3_Alpha_EO_Rest_E_Pre
POZ_Alpha_EO_Rest_E_Pre <- df$POZ_Alpha_EO_Rest_E_Pre
PO4_Alpha_EO_Rest_E_Pre <- df$PO4_Alpha_EO_Rest_E_Pre
P1_Alpha_EO_Rest_E_Post <- df$P1_Alpha_EO_Rest_E_Post
PZ_Alpha_EO_Rest_E_Post <- df$PZ_Alpha_EO_Rest_E_Post
P2_Alpha_EO_Rest_E_Post <- df$P2_Alpha_EO_Rest_E_Post
PO3_Alpha_EO_Rest_E_Post <- df$PO3_Alpha_EO_Rest_E_Post
POZ_Alpha_EO_Rest_E_Post <- df$POZ_Alpha_EO_Rest_E_Post
PO4_Alpha_EO_Rest_E_Post <- df$PO4_Alpha_EO_Rest_E_Post
P1_Alpha_EO_T_Pre <- df$P1_Alpha_EO_T_Pre
PZ_Alpha_EO_T_Pre <- df$PZ_Alpha_EO_T_Pre
P2_Alpha_EO_T_Pre <- df$P2_Alpha_EO_T_Pre
PO3_Alpha_EO_T_Pre <- df$PO3_Alpha_EO_T_Pre
POZ_Alpha_EO_T_Pre <- df$POZ_Alpha_EO_T_Pre
PO4_Alpha_EO_T_Pre <- df$PO4_Alpha_EO_T_Pre
P1_Alpha_EO_T_Post <- df$P1_Alpha_EO_T_Post
PZ_Alpha_EO_T_Post <- df$PZ_Alpha_EO_T_Post
P2_Alpha_EO_T_Post <- df$P2_Alpha_EO_T_Post
PO3_Alpha_EO_T_Post <- df$PO3_Alpha_EO_T_Post
POZ_Alpha_EO_T_Post <- df$POZ_Alpha_EO_T_Post
PO4_Alpha_EO_T_Post <- df$PO4_Alpha_EO_T_Post
P1_Alpha_EO_Rest_T_Pre <- df$P1_Alpha_EO_Rest_T_Pre
PZ_Alpha_EO_Rest_T_Pre <- df$PZ_Alpha_EO_Rest_T_Pre
P2_Alpha_EO_Rest_T_Pre <- df$P2_Alpha_EO_Rest_T_Pre
PO3_Alpha_EO_Rest_T_Pre <- df$PO3_Alpha_EO_Rest_T_Pre
POZ_Alpha_EO_Rest_T_Pre <- df$POZ_Alpha_EO_Rest_T_Pre
PO4_Alpha_EO_Rest_T_Pre <- df$PO4_Alpha_EO_Rest_T_Pre
P1_Alpha_EO_Rest_T_Post <- df$P1_Alpha_EO_Rest_T_Post
PZ_Alpha_EO_Rest_T_Post <- df$PZ_Alpha_EO_Rest_T_Post
P2_Alpha_EO_Rest_T_Post <- df$P2_Alpha_EO_Rest_T_Post
PO3_Alpha_EO_Rest_T_Post <- df$PO3_Alpha_EO_Rest_T_Post
POZ_Alpha_EO_Rest_T_Post <- df$POZ_Alpha_EO_Rest_T_Post
PO4_Alpha_EO_Rest_T_Post <- df$PO4_Alpha_EO_Rest_T_Post

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
ROI_FC_Rest_E_pre_Alpha.df <-data.frame(P1_Alpha_EO_Rest_E_Pre, PZ_Alpha_EO_Rest_E_Pre, P2_Alpha_EO_Rest_E_Pre, PO3_Alpha_EO_Rest_E_Pre, POZ_Alpha_EO_Rest_E_Pre, PO4_Alpha_EO_Rest_E_Pre)
ROI_FC_Rest_E_pre_Alpha.mean <- rowMeans(ROI_FC_Rest_E_pre_Alpha.df)
ROI_FC_Rest_E_post_Alpha.df <-data.frame(P1_Alpha_EO_Rest_E_Post, PZ_Alpha_EO_Rest_E_Post, P2_Alpha_EO_Rest_E_Post, PO3_Alpha_EO_Rest_E_Post, POZ_Alpha_EO_Rest_E_Post, PO4_Alpha_EO_Rest_E_Post)
ROI_FC_Rest_E_post_Alpha.mean <- rowMeans(ROI_FC_Rest_E_post_Alpha.df)
ROI_FC_E_pre_Alpha.df <-data.frame(P1_Alpha_EO_E_Pre, PZ_Alpha_EO_E_Pre, P2_Alpha_EO_E_Pre, PO3_Alpha_EO_E_Pre, POZ_Alpha_EO_E_Pre, PO4_Alpha_EO_E_Pre)
ROI_FC_E_pre_Alpha.mean <- rowMeans(ROI_FC_E_pre_Alpha.df)
ROI_FC_E_post_Alpha.df <-data.frame(P1_Alpha_EO_E_Post, PZ_Alpha_EO_E_Post, P2_Alpha_EO_E_Post, PO3_Alpha_EO_E_Post, POZ_Alpha_EO_E_Pre, PO4_Alpha_EO_E_Post)
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
M1_Alpha_EO_x_P3.df <- data.frame(SubjectID, ROI_FC_Rest_E_pre_Alpha.mean, ROI_FC_Rest_E_post_Alpha.mean, ROI_FC_E_pre_Alpha.mean, ROI_FC_E_post_Alpha.mean, 
                             Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean, Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean, Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean, Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean, 
                             Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean,Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean)
#melt
longformat.M1_Alpha_EO_x_P3.df <- melt(M1_Alpha_EO_x_P3.df, id = "SubjectID", variable.name = "Condition")
#Check for outliers 
longformat.M1_Alpha_EO_x_P3.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#normality test 
M1_Alpha_EO_x_P3.df%>%
shapiro_test(ROI_FC_Rest_E_pre_Alpha.mean, ROI_FC_Rest_E_post_Alpha.mean, ROI_FC_E_pre_Alpha.mean, ROI_FC_E_post_Alpha.mean, 
             Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean, Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean, Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean, Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean, 
             Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean,Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean)

#Computation - Pre_E_Congruent
summary(lm(formula = Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean~ROI_FC_E_pre_Alpha.mean))
ggplot(M1_Alpha_EO_x_P3.df, aes(x=ROI_FC_E_pre_Alpha.mean, y=Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_E_Incongruent
summary(lm(formula = Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean~ROI_FC_E_pre_Alpha.mean, data = M1_Alpha_EO_x_P3.df))
ggplot(M1_Alpha_EO_x_P3.df, aes(x=ROI_FC_E_pre_Alpha.mean, y=Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Congruent
summary(lm(formula = Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean~ROI_FC_Rest_E_pre_Alpha.mean, data = M1_Alpha_EO_x_P3.df))
ggplot(M1_Alpha_EO_x_P3.df, aes(x=ROI_FC_Rest_E_pre_Alpha.mean, y=Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Incongruent
summary(lm(formula = Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean~ROI_FC_Rest_E_pre_Alpha.mean, data = M1_Alpha_EO_x_P3.df))
ggplot(M1_Alpha_EO_x_P3.df, aes(x=ROI_FC_Rest_E_pre_Alpha.mean, y=Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Congruent
summary(lm(formula = Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean~ROI_FC_E_post_Alpha.mean, data = M1_Alpha_EO_x_P3.df))
ggplot(M1_Alpha_EO_x_P3.df, aes(x=ROI_FC_E_post_Alpha.mean, y=Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Incongruent
summary(lm(formula = Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean~ROI_FC_E_post_Alpha.mean, data = M1_Alpha_EO_x_P3.df))
ggplot(M1_Alpha_EO_x_P3.df, aes(x=ROI_FC_E_post_Alpha.mean, y=Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Congruent
summary(lm(formula = Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean~ROI_FC_Rest_E_post_Alpha.mean, data = M1_Alpha_EO_x_P3.df))
ggplot(M1_Alpha_EO_x_P3.df, aes(x=ROI_FC_Rest_E_post_Alpha.mean, y=Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Incongruent
summary(lm(formula = Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean~ROI_FC_Rest_E_post_Alpha.mean, data = M1_Alpha_EO_x_P3.df))
ggplot(M1_Alpha_EO_x_P3.df, aes(x=ROI_FC_Rest_E_post_Alpha.mean, y=Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))


# COGNITIVE AND RESTING BRAIN FUNCTION OUTCOMES - Alpha_EC and P3 Peak ----------------
#Call Variables 
SubjectID<-df$SubjectID
P1_Alpha_EC_E_Pre <- df$P1_Alpha_EC_E_Pre
PZ_Alpha_EC_E_Pre <- df$PZ_Alpha_EC_E_Pre
P2_Alpha_EC_E_Pre <- df$P2_Alpha_EC_E_Pre
PO3_Alpha_EC_E_Pre <- df$PO3_Alpha_EC_E_Pre
POZ_Alpha_EC_E_Pre <- df$POZ_Alpha_EC_E_Pre
PO4_Alpha_EC_E_Pre <- df$PO4_Alpha_EC_E_Pre
P1_Alpha_EC_E_Post <- df$P1_Alpha_EC_E_Post
PZ_Alpha_EC_E_Post <- df$PZ_Alpha_EC_E_Post
P2_Alpha_EC_E_Post <- df$P2_Alpha_EC_E_Post
PO3_Alpha_EC_E_Post <- df$PO3_Alpha_EC_E_Post
POZ_Alpha_EC_E_Post <- df$POZ_Alpha_EC_E_Post
PO4_Alpha_EC_E_Post <- df$PO4_Alpha_EC_E_Post
P1_Alpha_EC_Rest_E_Pre <- df$P1_Alpha_EC_Rest_E_Pre
PZ_Alpha_EC_Rest_E_Pre <- df$PZ_Alpha_EC_Rest_E_Pre
P2_Alpha_EC_Rest_E_Pre <- df$P2_Alpha_EC_Rest_E_Pre
PO3_Alpha_EC_Rest_E_Pre <- df$PO3_Alpha_EC_Rest_E_Pre
POZ_Alpha_EC_Rest_E_Pre <- df$POZ_Alpha_EC_Rest_E_Pre
PO4_Alpha_EC_Rest_E_Pre <- df$PO4_Alpha_EC_Rest_E_Pre
P1_Alpha_EC_Rest_E_Post <- df$P1_Alpha_EC_Rest_E_Post
PZ_Alpha_EC_Rest_E_Post <- df$PZ_Alpha_EC_Rest_E_Post
P2_Alpha_EC_Rest_E_Post <- df$P2_Alpha_EC_Rest_E_Post
PO3_Alpha_EC_Rest_E_Post <- df$PO3_Alpha_EC_Rest_E_Post
POZ_Alpha_EC_Rest_E_Post <- df$POZ_Alpha_EC_Rest_E_Post
PO4_Alpha_EC_Rest_E_Post <- df$PO4_Alpha_EC_Rest_E_Post
P1_Alpha_EC_T_Pre <- df$P1_Alpha_EC_T_Pre
PZ_Alpha_EC_T_Pre <- df$PZ_Alpha_EC_T_Pre
P2_Alpha_EC_T_Pre <- df$P2_Alpha_EC_T_Pre
PO3_Alpha_EC_T_Pre <- df$PO3_Alpha_EC_T_Pre
POZ_Alpha_EC_T_Pre <- df$POZ_Alpha_EC_T_Pre
PO4_Alpha_EC_T_Pre <- df$PO4_Alpha_EC_T_Pre
P1_Alpha_EC_T_Post <- df$P1_Alpha_EC_T_Post
PZ_Alpha_EC_T_Post <- df$PZ_Alpha_EC_T_Post
P2_Alpha_EC_T_Post <- df$P2_Alpha_EC_T_Post
PO3_Alpha_EC_T_Post <- df$PO3_Alpha_EC_T_Post
POZ_Alpha_EC_T_Post <- df$POZ_Alpha_EC_T_Post
PO4_Alpha_EC_T_Post <- df$PO4_Alpha_EC_T_Post
P1_Alpha_EC_Rest_T_Pre <- df$P1_Alpha_EC_Rest_T_Pre
PZ_Alpha_EC_Rest_T_Pre <- df$PZ_Alpha_EC_Rest_T_Pre
P2_Alpha_EC_Rest_T_Pre <- df$P2_Alpha_EC_Rest_T_Pre
PO3_Alpha_EC_Rest_T_Pre <- df$PO3_Alpha_EC_Rest_T_Pre
POZ_Alpha_EC_Rest_T_Pre <- df$POZ_Alpha_EC_Rest_T_Pre
PO4_Alpha_EC_Rest_T_Pre <- df$PO4_Alpha_EC_Rest_T_Pre
P1_Alpha_EC_Rest_T_Post <- df$P1_Alpha_EC_Rest_T_Post
PZ_Alpha_EC_Rest_T_Post <- df$PZ_Alpha_EC_Rest_T_Post
P2_Alpha_EC_Rest_T_Post <- df$P2_Alpha_EC_Rest_T_Post
PO3_Alpha_EC_Rest_T_Post <- df$PO3_Alpha_EC_Rest_T_Post
POZ_Alpha_EC_Rest_T_Post <- df$POZ_Alpha_EC_Rest_T_Post
PO4_Alpha_EC_Rest_T_Post <- df$PO4_Alpha_EC_Rest_T_Post

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
ROI_FC_Rest_E_pre_Alpha.df <-data.frame(P1_Alpha_EC_Rest_E_Pre, PZ_Alpha_EC_Rest_E_Pre, P2_Alpha_EC_Rest_E_Pre, PO3_Alpha_EC_Rest_E_Pre, POZ_Alpha_EC_Rest_E_Pre, PO4_Alpha_EC_Rest_E_Pre)
ROI_FC_Rest_E_pre_Alpha.mean <- rowMeans(ROI_FC_Rest_E_pre_Alpha.df)
ROI_FC_Rest_E_post_Alpha.df <-data.frame(P1_Alpha_EC_Rest_E_Post, PZ_Alpha_EC_Rest_E_Post, P2_Alpha_EC_Rest_E_Post, PO3_Alpha_EC_Rest_E_Post, POZ_Alpha_EC_Rest_E_Post, PO4_Alpha_EC_Rest_E_Post)
ROI_FC_Rest_E_post_Alpha.mean <- rowMeans(ROI_FC_Rest_E_post_Alpha.df)
ROI_FC_E_pre_Alpha.df <-data.frame(P1_Alpha_EC_E_Pre, PZ_Alpha_EC_E_Pre, P2_Alpha_EC_E_Pre, PO3_Alpha_EC_E_Pre, POZ_Alpha_EC_E_Pre, PO4_Alpha_EC_E_Pre)
ROI_FC_E_pre_Alpha.mean <- rowMeans(ROI_FC_E_pre_Alpha.df)
ROI_FC_E_post_Alpha.df <-data.frame(P1_Alpha_EC_E_Post, PZ_Alpha_EC_E_Post, P2_Alpha_EC_E_Post, PO3_Alpha_EC_E_Post, POZ_Alpha_EC_E_Pre, PO4_Alpha_EC_E_Post)
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
M1_Alpha_EC_x_P3.df <- data.frame(SubjectID, ROI_FC_Rest_E_pre_Alpha.mean, ROI_FC_Rest_E_post_Alpha.mean, ROI_FC_E_pre_Alpha.mean, ROI_FC_E_post_Alpha.mean, 
                                  Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean, Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean, Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean, Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean, 
                                  Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean,Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean)
#melt
longformat.M1_Alpha_EC_x_P3.df <- melt(M1_Alpha_EC_x_P3.df, id = "SubjectID", variable.name = "Condition")
#Check for outliers 
longformat.M1_Alpha_EC_x_P3.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#normality test 
M1_Alpha_EC_x_P3.df%>%
  shapiro_test(ROI_FC_Rest_E_pre_Alpha.mean, ROI_FC_Rest_E_post_Alpha.mean, ROI_FC_E_pre_Alpha.mean, ROI_FC_E_post_Alpha.mean, 
               Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean, Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean, Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean, Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean, 
               Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean,Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean)

#Computation - Pre_E_Congruent
summary(lm(formula = Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean~ROI_FC_E_pre_Alpha.mean))
ggplot(M1_Alpha_EC_x_P3.df, aes(x=ROI_FC_E_pre_Alpha.mean, y=Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_E_Incongruent
summary(lm(formula = Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean~ROI_FC_E_pre_Alpha.mean, data = M1_Alpha_EC_x_P3.df))
ggplot(M1_Alpha_EC_x_P3.df, aes(x=ROI_FC_E_pre_Alpha.mean, y=Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Congruent
summary(lm(formula = Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean~ROI_FC_Rest_E_pre_Alpha.mean, data = M1_Alpha_EC_x_P3.df))
ggplot(M1_Alpha_EC_x_P3.df, aes(x=ROI_FC_Rest_E_pre_Alpha.mean, y=Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Incongruent
summary(lm(formula = Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean~ROI_FC_Rest_E_pre_Alpha.mean, data = M1_Alpha_EC_x_P3.df))
ggplot(M1_Alpha_EC_x_P3.df, aes(x=ROI_FC_Rest_E_pre_Alpha.mean, y=Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Congruent
summary(lm(formula = Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean~ROI_FC_E_post_Alpha.mean, data = M1_Alpha_EC_x_P3.df))
ggplot(M1_Alpha_EC_x_P3.df, aes(x=ROI_FC_E_post_Alpha.mean, y=Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Incongruent
summary(lm(formula = Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean~ROI_FC_E_post_Alpha.mean, data = M1_Alpha_EC_x_P3.df))
ggplot(M1_Alpha_EC_x_P3.df, aes(x=ROI_FC_E_post_Alpha.mean, y=Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Congruent
summary(lm(formula = Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean~ROI_FC_Rest_E_post_Alpha.mean, data = M1_Alpha_EC_x_P3.df))
ggplot(M1_Alpha_EC_x_P3.df, aes(x=ROI_FC_Rest_E_post_Alpha.mean, y=Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Incongruent
summary(lm(formula = Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean~ROI_FC_Rest_E_post_Alpha.mean, data = M1_Alpha_EC_x_P3.df))
ggplot(M1_Alpha_EC_x_P3.df, aes(x=ROI_FC_Rest_E_post_Alpha.mean, y=Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean))+
  geom_point()+
  xlab("Alpha Log Power")+
  ylab("P3 Amplitude")+
  ggtitle("Alpha Power x P3 Amplitude Peak")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))


# COGNITIVE AND RESTING BRAIN FUNCTION OUTCOMES - Theta_EO and Flanker ACC -----------
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

F1_Theta_EO_E_Pre <- df$F1_Theta_EO_E_Pre
FZ_Theta_EO_E_Pre <- df$FZ_Theta_EO_E_Pre
F2_Theta_EO_E_Pre <- df$F2_Theta_EO_E_Pre
FC1_Theta_EO_E_Pre <- df$FC1_Theta_EO_E_Pre
FCZ_Theta_EO_E_Pre <- df$FCZ_Theta_EO_E_Pre
FC2_Theta_EO_E_Pre <- df$FC2_Theta_EO_E_Pre
C1_Theta_EO_E_Pre <- df$C1_Theta_EO_E_Pre
CZ_Theta_EO_E_Pre <- df$CZ_Theta_EO_E_Pre
C2_Theta_EO_E_Pre <- df$C2_Theta_EO_E_Pre
F1_Theta_EO_E_Post <- df$F1_Theta_EO_E_Post
FZ_Theta_EO_E_Post <- df$FZ_Theta_EO_E_Post
F2_Theta_EO_E_Post <- df$F2_Theta_EO_E_Post
FC1_Theta_EO_E_Post <- df$FC1_Theta_EO_E_Post
FCZ_Theta_EO_E_Post <- df$FCZ_Theta_EO_E_Post
FC2_Theta_EO_E_Post <- df$FC2_Theta_EO_E_Post
C1_Theta_EO_E_Post <- df$C1_Theta_EO_E_Post
CZ_Theta_EO_E_Post <- df$CZ_Theta_EO_E_Post
C2_Theta_EO_E_Post <- df$C2_Theta_EO_E_Post
F1_Theta_EO_Rest_E_Pre <- df$F1_Theta_EO_Rest_E_Pre
FZ_Theta_EO_Rest_E_Pre <- df$FZ_Theta_EO_Rest_E_Pre
F2_Theta_EO_Rest_E_Pre <- df$F2_Theta_EO_Rest_E_Pre
FC1_Theta_EO_Rest_E_Pre <- df$FC1_Theta_EO_Rest_E_Pre
FCZ_Theta_EO_Rest_E_Pre <- df$FCZ_Theta_EO_Rest_E_Pre
FC2_Theta_EO_Rest_E_Pre <- df$FC2_Theta_EO_Rest_E_Pre
C1_Theta_EO_Rest_E_Pre <- df$C1_Theta_EO_Rest_E_Pre
CZ_Theta_EO_Rest_E_Pre <- df$CZ_Theta_EO_Rest_E_Pre
C2_Theta_EO_Rest_E_Pre <- df$C2_Theta_EO_Rest_E_Pre
F1_Theta_EO_Rest_E_Post <- df$F1_Theta_EO_Rest_E_Post
FZ_Theta_EO_Rest_E_Post <- df$FZ_Theta_EO_Rest_E_Post
F2_Theta_EO_Rest_E_Post <- df$F2_Theta_EO_Rest_E_Post
FC1_Theta_EO_Rest_E_Post <- df$FC1_Theta_EO_Rest_E_Post
FCZ_Theta_EO_Rest_E_Post <- df$FCZ_Theta_EO_Rest_E_Post
FC2_Theta_EO_Rest_E_Post <- df$FC2_Theta_EO_Rest_E_Post
C1_Theta_EO_Rest_E_Post <- df$C1_Theta_EO_Rest_E_Post
CZ_Theta_EO_Rest_E_Post <- df$CZ_Theta_EO_Rest_E_Post
C2_Theta_EO_Rest_E_Post <- df$C2_Theta_EO_Rest_E_Post

#M1 Resting Posterior Theta x Flanker ACC in Exercise vs. Rest 
##Average together Resting Theta ROI site
Wide.ROI_FC_E_pre_Theta.df <-data.frame(F1_Theta_EO_E_Pre, FZ_Theta_EO_E_Pre, F2_Theta_EO_E_Pre, FC1_Theta_EO_E_Pre, FCZ_Theta_EO_E_Pre, FC2_Theta_EO_E_Pre, C1_Theta_EO_E_Pre, CZ_Theta_EO_E_Pre, C2_Theta_EO_E_Pre)
Wide.ROI_FC_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_E_pre_Theta.df)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_EO_E_Post, FZ_Theta_EO_E_Post, F2_Theta_EO_E_Post, FC1_Theta_EO_E_Post, FCZ_Theta_EO_E_Post, FC2_Theta_EO_E_Post, C1_Theta_EO_E_Post, CZ_Theta_EO_E_Post, C2_Theta_EO_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_pre_Theta.df <-data.frame(F1_Theta_EO_Rest_E_Pre, FZ_Theta_EO_Rest_E_Pre, F2_Theta_EO_Rest_E_Pre, FC1_Theta_EO_Rest_E_Pre, FCZ_Theta_EO_Rest_E_Pre, FC2_Theta_EO_Rest_E_Pre, C1_Theta_EO_Rest_E_Pre, CZ_Theta_EO_Rest_E_Pre, C2_Theta_EO_Rest_E_Pre)
Wide.ROI_FC_Rest_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_pre_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_EO_Rest_E_Post, FZ_Theta_EO_Rest_E_Post, F2_Theta_EO_Rest_E_Post, FC1_Theta_EO_Rest_E_Post, FCZ_Theta_EO_Rest_E_Post, FC2_Theta_EO_Rest_E_Post, C1_Theta_EO_Rest_E_Post, CZ_Theta_EO_Rest_E_Post, C2_Theta_EO_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)

#create dataframe 
M1_Theta_EO_x_ACC.df <- data.frame(SubjectID,Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
                                SAya_FL_Congruent_ResponseAccuracy_E_pre, SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre, SAya_FL_Incongruent_ResponseAccuracy_E_pre, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre, 
                                SAya_FL_Congruent_ResponseAccuracy_E_post, SAya_FL_Congruent_ResponseAccuracy_Rest_E_post, SAya_FL_Incongruent_ResponseAccuracy_E_post, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
#melt
longformat.M1_Theta_EO_x_ACC.df <- melt(M1_Theta_EO_x_ACC.df, id = "SubjectID", variable.name = "Condition")
#Check for outliers 
longformat.M1_Theta_EO_x_ACC.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#normality test 
M1_Theta_EO_x_ACC.df%>%
  shapiro_test(Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
               SAya_FL_Congruent_ResponseAccuracy_E_pre, SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre, SAya_FL_Incongruent_ResponseAccuracy_E_pre, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre, 
               SAya_FL_Congruent_ResponseAccuracy_E_post, SAya_FL_Congruent_ResponseAccuracy_Rest_E_post, SAya_FL_Incongruent_ResponseAccuracy_E_post, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
#Computation - Pre_E_Congruent
summary(lm(formula = SAya_FL_Congruent_ResponseAccuracy_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_EO_x_ACC.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Congruent_ResponseAccuracy_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_ResponseAccuracy_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_EO_x_ACC.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Incongruent_ResponseAccuracy_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Congruent
summary(lm(formula = SAya_FL_Congruent_ResponseAccuracy_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_EO_x_ACC.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Congruent_ResponseAccuracy_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_ResponseAccuracy_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_EO_x_ACC.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Incongruent_ResponseAccuracy_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_EO_x_ACC.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_EO_x_ACC.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_EO_x_ACC.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_EO_x_ACC.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))

# COGNITIVE AND RESTING BRAIN FUNCTION OUTCOMES - Theta_EC and Flanker ACC -----------
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

F1_Theta_EC_E_Pre <- df$F1_Theta_EC_E_Pre
FZ_Theta_EC_E_Pre <- df$FZ_Theta_EC_E_Pre
F2_Theta_EC_E_Pre <- df$F2_Theta_EC_E_Pre
FC1_Theta_EC_E_Pre <- df$FC1_Theta_EC_E_Pre
FCZ_Theta_EC_E_Pre <- df$FCZ_Theta_EC_E_Pre
FC2_Theta_EC_E_Pre <- df$FC2_Theta_EC_E_Pre
C1_Theta_EC_E_Pre <- df$C1_Theta_EC_E_Pre
CZ_Theta_EC_E_Pre <- df$CZ_Theta_EC_E_Pre
C2_Theta_EC_E_Pre <- df$C2_Theta_EC_E_Pre
F1_Theta_EC_E_Post <- df$F1_Theta_EC_E_Post
FZ_Theta_EC_E_Post <- df$FZ_Theta_EC_E_Post
F2_Theta_EC_E_Post <- df$F2_Theta_EC_E_Post
FC1_Theta_EC_E_Post <- df$FC1_Theta_EC_E_Post
FCZ_Theta_EC_E_Post <- df$FCZ_Theta_EC_E_Post
FC2_Theta_EC_E_Post <- df$FC2_Theta_EC_E_Post
C1_Theta_EC_E_Post <- df$C1_Theta_EC_E_Post
CZ_Theta_EC_E_Post <- df$CZ_Theta_EC_E_Post
C2_Theta_EC_E_Post <- df$C2_Theta_EC_E_Post
F1_Theta_EC_Rest_E_Pre <- df$F1_Theta_EC_Rest_E_Pre
FZ_Theta_EC_Rest_E_Pre <- df$FZ_Theta_EC_Rest_E_Pre
F2_Theta_EC_Rest_E_Pre <- df$F2_Theta_EC_Rest_E_Pre
FC1_Theta_EC_Rest_E_Pre <- df$FC1_Theta_EC_Rest_E_Pre
FCZ_Theta_EC_Rest_E_Pre <- df$FCZ_Theta_EC_Rest_E_Pre
FC2_Theta_EC_Rest_E_Pre <- df$FC2_Theta_EC_Rest_E_Pre
C1_Theta_EC_Rest_E_Pre <- df$C1_Theta_EC_Rest_E_Pre
CZ_Theta_EC_Rest_E_Pre <- df$CZ_Theta_EC_Rest_E_Pre
C2_Theta_EC_Rest_E_Pre <- df$C2_Theta_EC_Rest_E_Pre
F1_Theta_EC_Rest_E_Post <- df$F1_Theta_EC_Rest_E_Post
FZ_Theta_EC_Rest_E_Post <- df$FZ_Theta_EC_Rest_E_Post
F2_Theta_EC_Rest_E_Post <- df$F2_Theta_EC_Rest_E_Post
FC1_Theta_EC_Rest_E_Post <- df$FC1_Theta_EC_Rest_E_Post
FCZ_Theta_EC_Rest_E_Post <- df$FCZ_Theta_EC_Rest_E_Post
FC2_Theta_EC_Rest_E_Post <- df$FC2_Theta_EC_Rest_E_Post
C1_Theta_EC_Rest_E_Post <- df$C1_Theta_EC_Rest_E_Post
CZ_Theta_EC_Rest_E_Post <- df$CZ_Theta_EC_Rest_E_Post
C2_Theta_EC_Rest_E_Post <- df$C2_Theta_EC_Rest_E_Post


#M1 Resting Posterior Theta x Flanker ACC in Exercise vs. Rest 
##Average together Resting Theta ROI site
Wide.ROI_FC_E_pre_Theta.df <-data.frame(F1_Theta_EC_E_Pre, FZ_Theta_EC_E_Pre, F2_Theta_EC_E_Pre, FC1_Theta_EC_E_Pre, FCZ_Theta_EC_E_Pre, FC2_Theta_EC_E_Pre, C1_Theta_EC_E_Pre, CZ_Theta_EC_E_Pre, C2_Theta_EC_E_Pre)
Wide.ROI_FC_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_E_pre_Theta.df)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_EC_E_Post, FZ_Theta_EC_E_Post, F2_Theta_EC_E_Post, FC1_Theta_EC_E_Post, FCZ_Theta_EC_E_Post, FC2_Theta_EC_E_Post, C1_Theta_EC_E_Post, CZ_Theta_EC_E_Post, C2_Theta_EC_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_pre_Theta.df <-data.frame(F1_Theta_EC_Rest_E_Pre, FZ_Theta_EC_Rest_E_Pre, F2_Theta_EC_Rest_E_Pre, FC1_Theta_EC_Rest_E_Pre, FCZ_Theta_EC_Rest_E_Pre, FC2_Theta_EC_Rest_E_Pre, C1_Theta_EC_Rest_E_Pre, CZ_Theta_EC_Rest_E_Pre, C2_Theta_EC_Rest_E_Pre)
Wide.ROI_FC_Rest_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_pre_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_EC_Rest_E_Post, FZ_Theta_EC_Rest_E_Post, F2_Theta_EC_Rest_E_Post, FC1_Theta_EC_Rest_E_Post, FCZ_Theta_EC_Rest_E_Post, FC2_Theta_EC_Rest_E_Post, C1_Theta_EC_Rest_E_Post, CZ_Theta_EC_Rest_E_Post, C2_Theta_EC_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#create dataframe 
M1_Theta_EC_x_ACC.df <- data.frame(SubjectID,Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
                                   SAya_FL_Congruent_ResponseAccuracy_E_pre, SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre, SAya_FL_Incongruent_ResponseAccuracy_E_pre, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre, 
                                   SAya_FL_Congruent_ResponseAccuracy_E_post, SAya_FL_Congruent_ResponseAccuracy_Rest_E_post, SAya_FL_Incongruent_ResponseAccuracy_E_post, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
#melt
longformat.M1_Theta_EC_x_ACC.df <- melt(M1_Theta_EC_x_ACC.df, id = "SubjectID", variable.name = "Condition")
#Check for outliers 
longformat.M1_Theta_EC_x_ACC.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#normality test 
M1_Theta_EC_x_ACC.df%>%
  shapiro_test(Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
               SAya_FL_Congruent_ResponseAccuracy_E_pre, SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre, SAya_FL_Incongruent_ResponseAccuracy_E_pre, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre, 
               SAya_FL_Congruent_ResponseAccuracy_E_post, SAya_FL_Congruent_ResponseAccuracy_Rest_E_post, SAya_FL_Incongruent_ResponseAccuracy_E_post, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
#Computation - Pre_E_Congruent
summary(lm(formula = SAya_FL_Congruent_ResponseAccuracy_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_EC_x_ACC.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Congruent_ResponseAccuracy_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_ResponseAccuracy_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_EC_x_ACC.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Incongruent_ResponseAccuracy_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Congruent
summary(lm(formula = SAya_FL_Congruent_ResponseAccuracy_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_EC_x_ACC.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Congruent_ResponseAccuracy_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_ResponseAccuracy_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_EC_x_ACC.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Incongruent_ResponseAccuracy_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_EC_x_ACC.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_EC_x_ACC.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_EC_x_ACC.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_EC_x_ACC.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker ACC")+
  ggtitle("Theta Power x Flanker ACC")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))

# COGNITIVE AND RESTING BRAIN FUNCTION OUTCOMES - Theta_EO and Flanker RT ------------
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

F1_Theta_EO_E_Pre <- df$F1_Theta_EO_E_Pre
FZ_Theta_EO_E_Pre <- df$FZ_Theta_EO_E_Pre
F2_Theta_EO_E_Pre <- df$F2_Theta_EO_E_Pre
FC1_Theta_EO_E_Pre <- df$FC1_Theta_EO_E_Pre
FCZ_Theta_EO_E_Pre <- df$FCZ_Theta_EO_E_Pre
FC2_Theta_EO_E_Pre <- df$FC2_Theta_EO_E_Pre
C1_Theta_EO_E_Pre <- df$C1_Theta_EO_E_Pre
CZ_Theta_EO_E_Pre <- df$CZ_Theta_EO_E_Pre
C2_Theta_EO_E_Pre <- df$C2_Theta_EO_E_Pre
F1_Theta_EO_E_Post <- df$F1_Theta_EO_E_Post
FZ_Theta_EO_E_Post <- df$FZ_Theta_EO_E_Post
F2_Theta_EO_E_Post <- df$F2_Theta_EO_E_Post
FC1_Theta_EO_E_Post <- df$FC1_Theta_EO_E_Post
FCZ_Theta_EO_E_Post <- df$FCZ_Theta_EO_E_Post
FC2_Theta_EO_E_Post <- df$FC2_Theta_EO_E_Post
C1_Theta_EO_E_Post <- df$C1_Theta_EO_E_Post
CZ_Theta_EO_E_Post <- df$CZ_Theta_EO_E_Post
C2_Theta_EO_E_Post <- df$C2_Theta_EO_E_Post
F1_Theta_EO_Rest_E_Pre <- df$F1_Theta_EO_Rest_E_Pre
FZ_Theta_EO_Rest_E_Pre <- df$FZ_Theta_EO_Rest_E_Pre
F2_Theta_EO_Rest_E_Pre <- df$F2_Theta_EO_Rest_E_Pre
FC1_Theta_EO_Rest_E_Pre <- df$FC1_Theta_EO_Rest_E_Pre
FCZ_Theta_EO_Rest_E_Pre <- df$FCZ_Theta_EO_Rest_E_Pre
FC2_Theta_EO_Rest_E_Pre <- df$FC2_Theta_EO_Rest_E_Pre
C1_Theta_EO_Rest_E_Pre <- df$C1_Theta_EO_Rest_E_Pre
CZ_Theta_EO_Rest_E_Pre <- df$CZ_Theta_EO_Rest_E_Pre
C2_Theta_EO_Rest_E_Pre <- df$C2_Theta_EO_Rest_E_Pre
F1_Theta_EO_Rest_E_Post <- df$F1_Theta_EO_Rest_E_Post
FZ_Theta_EO_Rest_E_Post <- df$FZ_Theta_EO_Rest_E_Post
F2_Theta_EO_Rest_E_Post <- df$F2_Theta_EO_Rest_E_Post
FC1_Theta_EO_Rest_E_Post <- df$FC1_Theta_EO_Rest_E_Post
FCZ_Theta_EO_Rest_E_Post <- df$FCZ_Theta_EO_Rest_E_Post
FC2_Theta_EO_Rest_E_Post <- df$FC2_Theta_EO_Rest_E_Post
C1_Theta_EO_Rest_E_Post <- df$C1_Theta_EO_Rest_E_Post
CZ_Theta_EO_Rest_E_Post <- df$CZ_Theta_EO_Rest_E_Post
C2_Theta_EO_Rest_E_Post <- df$C2_Theta_EO_Rest_E_Post


#M1 Resting Posterior Theta x Flanker RT in Exercise vs. Rest 
##Average together Resting Theta ROI site
Wide.ROI_FC_E_pre_Theta.df <-data.frame(F1_Theta_EO_E_Pre, FZ_Theta_EO_E_Pre, F2_Theta_EO_E_Pre, FC1_Theta_EO_E_Pre, FCZ_Theta_EO_E_Pre, FC2_Theta_EO_E_Pre, C1_Theta_EO_E_Pre, CZ_Theta_EO_E_Pre, C2_Theta_EO_E_Pre)
Wide.ROI_FC_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_E_pre_Theta.df)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_EO_E_Post, FZ_Theta_EO_E_Post, F2_Theta_EO_E_Post, FC1_Theta_EO_E_Post, FCZ_Theta_EO_E_Post, FC2_Theta_EO_E_Post, C1_Theta_EO_E_Post, CZ_Theta_EO_E_Post, C2_Theta_EO_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_pre_Theta.df <-data.frame(F1_Theta_EO_Rest_E_Pre, FZ_Theta_EO_Rest_E_Pre, F2_Theta_EO_Rest_E_Pre, FC1_Theta_EO_Rest_E_Pre, FCZ_Theta_EO_Rest_E_Pre, FC2_Theta_EO_Rest_E_Pre, C1_Theta_EO_Rest_E_Pre, CZ_Theta_EO_Rest_E_Pre, C2_Theta_EO_Rest_E_Pre)
Wide.ROI_FC_Rest_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_pre_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_EO_Rest_E_Post, FZ_Theta_EO_Rest_E_Post, F2_Theta_EO_Rest_E_Post, FC1_Theta_EO_Rest_E_Post, FCZ_Theta_EO_Rest_E_Post, FC2_Theta_EO_Rest_E_Post, C1_Theta_EO_Rest_E_Post, CZ_Theta_EO_Rest_E_Post, C2_Theta_EO_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#create dataframe 
M1_Theta_EO_x_RT.df <- data.frame(SubjectID, Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
                               SAya_FL_Congruent_MeanRT_E_pre, SAya_FL_Congruent_MeanRT_Rest_E_pre, SAya_FL_Incongruent_MeanRT_E_pre, SAya_FL_Incongruent_MeanRT_Rest_E_pre, 
                               SAya_FL_Congruent_MeanRT_E_post, SAya_FL_Congruent_MeanRT_Rest_E_post, SAya_FL_Incongruent_MeanRT_E_post, SAya_FL_Incongruent_MeanRT_Rest_E_post)
#melt
longformat.M1_Theta_EO_x_RT.df.df <- melt(M1_Theta_EO_x_RT.df, id = "SubjectID", variable.name = "Condition")
#Check for outliers 
longformat.M1_Theta_EO_x_RT.df.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#normality test 
M1_Theta_EO_x_RT.df%>%
  shapiro_test(Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
               SAya_FL_Congruent_MeanRT_E_pre, SAya_FL_Congruent_MeanRT_Rest_E_pre, SAya_FL_Incongruent_MeanRT_E_pre, SAya_FL_Incongruent_MeanRT_Rest_E_pre, 
               SAya_FL_Congruent_MeanRT_E_post, SAya_FL_Congruent_MeanRT_Rest_E_post, SAya_FL_Incongruent_MeanRT_E_post, SAya_FL_Incongruent_MeanRT_Rest_E_post)
#Computation - Pre_E_Congruent
summary(lm(formula = SAya_FL_Congruent_MeanRT_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_EO_x_RT.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Congruent_MeanRT_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_MeanRT_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_EO_x_RT.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Incongruent_MeanRT_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Congruent
summary(lm(formula = SAya_FL_Congruent_MeanRT_E_post~Wide.ROI_FC_E_post_Theta.mean))
ggplot(M1_Theta_EO_x_RT.df, aes(x=Wide.ROI_FC_E_post_Theta.mean, y=SAya_FL_Congruent_MeanRT_E_post))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_MeanRT_E_post~Wide.ROI_FC_E_post_Theta.mean))
ggplot(M1_Theta_EO_x_RT.df, aes(x=Wide.ROI_FC_E_post_Theta.mean, y=SAya_FL_Incongruent_MeanRT_E_post))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_MeanRT_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_EO_x_RT.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Congruent_MeanRT_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_MeanRT_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_EO_x_RT.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Incongruent_MeanRT_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_MeanRT_Rest_E_post~Wide.ROI_FC_Rest_E_post_Theta.mean))
ggplot(M1_Theta_EO_x_RT.df, aes(x=Wide.ROI_FC_Rest_E_post_Theta.mean, y=SAya_FL_Congruent_MeanRT_Rest_E_post))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_MeanRT_Rest_E_post~Wide.ROI_FC_Rest_E_post_Theta.mean))
ggplot(M1_Theta_EO_x_RT.df, aes(x=Wide.ROI_FC_Rest_E_post_Theta.mean, y=SAya_FL_Incongruent_MeanRT_Rest_E_post))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))


# COGNITIVE AND RESTING BRAIN FUNCTION OUTCOMES - Theta_EC and Flanker RT ------------
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

F1_Theta_EC_E_Pre <- df$F1_Theta_EC_E_Pre
FZ_Theta_EC_E_Pre <- df$FZ_Theta_EC_E_Pre
F2_Theta_EC_E_Pre <- df$F2_Theta_EC_E_Pre
FC1_Theta_EC_E_Pre <- df$FC1_Theta_EC_E_Pre
FCZ_Theta_EC_E_Pre <- df$FCZ_Theta_EC_E_Pre
FC2_Theta_EC_E_Pre <- df$FC2_Theta_EC_E_Pre
C1_Theta_EC_E_Pre <- df$C1_Theta_EC_E_Pre
CZ_Theta_EC_E_Pre <- df$CZ_Theta_EC_E_Pre
C2_Theta_EC_E_Pre <- df$C2_Theta_EC_E_Pre
F1_Theta_EC_E_Post <- df$F1_Theta_EC_E_Post
FZ_Theta_EC_E_Post <- df$FZ_Theta_EC_E_Post
F2_Theta_EC_E_Post <- df$F2_Theta_EC_E_Post
FC1_Theta_EC_E_Post <- df$FC1_Theta_EC_E_Post
FCZ_Theta_EC_E_Post <- df$FCZ_Theta_EC_E_Post
FC2_Theta_EC_E_Post <- df$FC2_Theta_EC_E_Post
C1_Theta_EC_E_Post <- df$C1_Theta_EC_E_Post
CZ_Theta_EC_E_Post <- df$CZ_Theta_EC_E_Post
C2_Theta_EC_E_Post <- df$C2_Theta_EC_E_Post
F1_Theta_EC_Rest_E_Pre <- df$F1_Theta_EC_Rest_E_Pre
FZ_Theta_EC_Rest_E_Pre <- df$FZ_Theta_EC_Rest_E_Pre
F2_Theta_EC_Rest_E_Pre <- df$F2_Theta_EC_Rest_E_Pre
FC1_Theta_EC_Rest_E_Pre <- df$FC1_Theta_EC_Rest_E_Pre
FCZ_Theta_EC_Rest_E_Pre <- df$FCZ_Theta_EC_Rest_E_Pre
FC2_Theta_EC_Rest_E_Pre <- df$FC2_Theta_EC_Rest_E_Pre
C1_Theta_EC_Rest_E_Pre <- df$C1_Theta_EC_Rest_E_Pre
CZ_Theta_EC_Rest_E_Pre <- df$CZ_Theta_EC_Rest_E_Pre
C2_Theta_EC_Rest_E_Pre <- df$C2_Theta_EC_Rest_E_Pre
F1_Theta_EC_Rest_E_Post <- df$F1_Theta_EC_Rest_E_Post
FZ_Theta_EC_Rest_E_Post <- df$FZ_Theta_EC_Rest_E_Post
F2_Theta_EC_Rest_E_Post <- df$F2_Theta_EC_Rest_E_Post
FC1_Theta_EC_Rest_E_Post <- df$FC1_Theta_EC_Rest_E_Post
FCZ_Theta_EC_Rest_E_Post <- df$FCZ_Theta_EC_Rest_E_Post
FC2_Theta_EC_Rest_E_Post <- df$FC2_Theta_EC_Rest_E_Post
C1_Theta_EC_Rest_E_Post <- df$C1_Theta_EC_Rest_E_Post
CZ_Theta_EC_Rest_E_Post <- df$CZ_Theta_EC_Rest_E_Post
C2_Theta_EC_Rest_E_Post <- df$C2_Theta_EC_Rest_E_Post

#M1 Resting Posterior Theta x Flanker RT in Exercise vs. Rest 
##Average together Resting Theta ROI site
Wide.ROI_FC_E_pre_Theta.df <-data.frame(F1_Theta_EC_E_Pre, FZ_Theta_EC_E_Pre, F2_Theta_EC_E_Pre, FC1_Theta_EC_E_Pre, FCZ_Theta_EC_E_Pre, FC2_Theta_EC_E_Pre, C1_Theta_EC_E_Pre, CZ_Theta_EC_E_Pre, C2_Theta_EC_E_Pre)
Wide.ROI_FC_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_E_pre_Theta.df)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_EC_E_Post, FZ_Theta_EC_E_Post, F2_Theta_EC_E_Post, FC1_Theta_EC_E_Post, FCZ_Theta_EC_E_Post, FC2_Theta_EC_E_Post, C1_Theta_EC_E_Post, CZ_Theta_EC_E_Post, C2_Theta_EC_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_pre_Theta.df <-data.frame(F1_Theta_EC_Rest_E_Pre, FZ_Theta_EC_Rest_E_Pre, F2_Theta_EC_Rest_E_Pre, FC1_Theta_EC_Rest_E_Pre, FCZ_Theta_EC_Rest_E_Pre, FC2_Theta_EC_Rest_E_Pre, C1_Theta_EC_Rest_E_Pre, CZ_Theta_EC_Rest_E_Pre, C2_Theta_EC_Rest_E_Pre)
Wide.ROI_FC_Rest_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_pre_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_EC_Rest_E_Post, FZ_Theta_EC_Rest_E_Post, F2_Theta_EC_Rest_E_Post, FC1_Theta_EC_Rest_E_Post, FCZ_Theta_EC_Rest_E_Post, FC2_Theta_EC_Rest_E_Post, C1_Theta_EC_Rest_E_Post, CZ_Theta_EC_Rest_E_Post, C2_Theta_EC_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#create dataframe 
M1_Theta_EC_x_RT.df <- data.frame(SubjectID, Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
                                  SAya_FL_Congruent_MeanRT_E_pre, SAya_FL_Congruent_MeanRT_Rest_E_pre, SAya_FL_Incongruent_MeanRT_E_pre, SAya_FL_Incongruent_MeanRT_Rest_E_pre, 
                                  SAya_FL_Congruent_MeanRT_E_post, SAya_FL_Congruent_MeanRT_Rest_E_post, SAya_FL_Incongruent_MeanRT_E_post, SAya_FL_Incongruent_MeanRT_Rest_E_post)
#melt
longformat.M1_Theta_EC_x_RT.df.df <- melt(M1_Theta_EC_x_RT.df, id = "SubjectID", variable.name = "Condition")
#Check for outliers 
longformat.M1_Theta_EC_x_RT.df.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#normality test 
M1_Theta_EC_x_RT.df%>%
  shapiro_test(Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
               SAya_FL_Congruent_MeanRT_E_pre, SAya_FL_Congruent_MeanRT_Rest_E_pre, SAya_FL_Incongruent_MeanRT_E_pre, SAya_FL_Incongruent_MeanRT_Rest_E_pre, 
               SAya_FL_Congruent_MeanRT_E_post, SAya_FL_Congruent_MeanRT_Rest_E_post, SAya_FL_Incongruent_MeanRT_E_post, SAya_FL_Incongruent_MeanRT_Rest_E_post)
#Computation - Pre_E_Congruent
summary(lm(formula = SAya_FL_Congruent_MeanRT_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_EC_x_RT.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Congruent_MeanRT_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_MeanRT_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_EC_x_RT.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Incongruent_MeanRT_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Congruent
summary(lm(formula = SAya_FL_Congruent_MeanRT_E_post~Wide.ROI_FC_E_post_Theta.mean))
ggplot(M1_Theta_EC_x_RT.df, aes(x=Wide.ROI_FC_E_post_Theta.mean, y=SAya_FL_Congruent_MeanRT_E_post))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_MeanRT_E_post~Wide.ROI_FC_E_post_Theta.mean))
ggplot(M1_Theta_EC_x_RT.df, aes(x=Wide.ROI_FC_E_post_Theta.mean, y=SAya_FL_Incongruent_MeanRT_E_post))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_MeanRT_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_EC_x_RT.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Congruent_MeanRT_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_MeanRT_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggplot(M1_Theta_EC_x_RT.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Incongruent_MeanRT_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_MeanRT_Rest_E_post~Wide.ROI_FC_Rest_E_post_Theta.mean))
ggplot(M1_Theta_EC_x_RT.df, aes(x=Wide.ROI_FC_Rest_E_post_Theta.mean, y=SAya_FL_Congruent_MeanRT_Rest_E_post))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_MeanRT_Rest_E_post~Wide.ROI_FC_Rest_E_post_Theta.mean))
ggplot(M1_Theta_EC_x_RT.df, aes(x=Wide.ROI_FC_Rest_E_post_Theta.mean, y=SAya_FL_Incongruent_MeanRT_Rest_E_post))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker RT")+
  ggtitle("Theta Power x Flanker RT")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))


# MEDIATIONS - COGNITIVE OUTCOMES - Flanker Response Time  --------
#M1 Mediation - X = intervention group, M = sAA at Timepoint 4, Y = post intervention Congruent Flanker RT
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
SAya_FL_Congruent_MeanRT_E_post <- (df$ SAya_FL_Congruent_MeanRT_E_post)
SAya_FL_Congruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Congruent_MeanRT_Rest_E_post)
#create dataframe 
postRT_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise,SAya_FL_Congruent_MeanRT_E_post, SAya_FL_Congruent_MeanRT_Rest_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(postRT_Congruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postRT_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postRT_Congruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postRT_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
SAya_FL_Incongruent_MeanRT_E_post <- (df$ SAya_FL_Incongruent_MeanRT_E_post)
SAya_FL_Incongruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Incongruent_MeanRT_Rest_E_post)
#create dataframe 
postRT_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, SAya_FL_Incongruent_MeanRT_E_post, SAya_FL_Incongruent_MeanRT_Rest_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(postRT_Incongruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postRT_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postRT_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/postRT_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
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
write_xlsx(changeRT_Congruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeRT_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeRT_Congruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeRT_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
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
write_xlsx(changeRT_Incongruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeRT_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeRT_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeRT_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
SAya_FL_Congruent_MeanRT_E_post <- (df$ SAya_FL_Congruent_MeanRT_E_post)
SAya_FL_Congruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Congruent_MeanRT_Rest_E_post)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#create dataframe 
changeSAA_PostRT_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Congruent_MeanRT_E_post, SAya_FL_Congruent_MeanRT_Rest_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostRT_Congruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostRT_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostRT_Congruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostRT_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
SAya_FL_Incongruent_MeanRT_E_post <- (df$ SAya_FL_Incongruent_MeanRT_E_post)
SAya_FL_Incongruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Incongruent_MeanRT_Rest_E_post)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#create dataframe 
changeSAA_PostRT_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Incongruent_MeanRT_E_post, SAya_FL_Incongruent_MeanRT_Rest_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostRT_Incongruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostRT_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostRT_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostRT_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
SAya_FL_Congruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_post)
SAya_FL_Congruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_post)
#create dataframe 
postACC_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, SAya_FL_Congruent_ResponseAccuracy_Rest_E_post, SAya_FL_Congruent_ResponseAccuracy_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(postACC_Congruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postACC_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postACC_Congruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/postACC_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
SAya_FL_Incongruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_post)
#create dataframe 
postACC_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post, SAya_FL_Incongruent_ResponseAccuracy_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(postACC_Incongruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postACC_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postACC_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/postACC_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
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
write_xlsx(changeACC_Congruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeACC_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeACC_Congruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeACC_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
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
write_xlsx(changeACC_Incongruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeACC_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeACC_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeACC_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
SAya_FL_Congruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_post)
SAya_FL_Congruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_post)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#create dataframe 
changeSAA_PostACC_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Congruent_ResponseAccuracy_E_post, SAya_FL_Congruent_ResponseAccuracy_Rest_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostACC_Congruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostACC_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostACC_Congruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostACC_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
SAya_FL_Incongruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_post)
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#create dataframe 
changeSAA_PostACC_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Incongruent_ResponseAccuracy_E_post, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostACC_Incongruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostACC_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostACC_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostACC_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostACC_Incongruent_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostACC_Incongruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostACC_Incongruent_Model1_Mediation.df)
model.Y.changeSAA_PostACC_Incongruent_Model1_Mediation<- lm(PostACC~Intervention + SAA_Change, longformat.changeSAA_PostACC_Incongruent_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostACC_Incongruent_Model1_Mediation<- mediate(model.M.changeSAA_PostACC_Incongruent_Model1_Mediation, model.Y.changeSAA_PostACC_Incongruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostACC_Incongruent_Model1_Mediation)

# MEDIATIONS - BRAIN FUNCTION OUTCOMES - Flanker P3 Latency ----------------
#M1 Mediation - X = intervention group, M = sAA at Timepoint 4, Y = post-intervention congruent P3 Latency
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Latency, FC_Rest_E_po_P3_Con_CP1_Latency, FC_Rest_E_po_P3_Con_CP2_Latency)
Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.df)
Narrow.ROI_FC_E_po_P3_Con_Latency.df <-data.frame(FC_E_po_P3_Con_CPZ_Latency, FC_E_po_P3_Con_CP1_Latency, FC_E_po_P3_Con_CP2_Latency)
Narrow.ROI_FC_E_po_P3_Con_Latency.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Con_Latency.df)
#create dataframe 
postP3Lat_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, Narrow.ROI_FC_Rest_E_po_P3_Con_Latency.mean, Narrow.ROI_FC_E_po_P3_Con_Latency.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(postP3Lat_Congruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postP3Lat_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postP3Lat_Congruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/postP3Lat_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.df <-data.frame(FC_Rest_E_po_P3_Inc_CPZ_Latency, FC_Rest_E_po_P3_Inc_CP1_Latency, FC_Rest_E_po_P3_Inc_CP2_Latency)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.df)
Narrow.ROI_FC_E_po_P3_Inc_Latency.df <-data.frame(FC_E_po_P3_Inc_CPZ_Latency, FC_E_po_P3_Inc_CP1_Latency, FC_E_po_P3_Inc_CP2_Latency)
Narrow.ROI_FC_E_po_P3_Inc_Latency.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Inc_Latency.df)
#create dataframe 
postP3Lat_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, Narrow.ROI_FC_Rest_E_po_P3_Inc_Latency.mean, Narrow.ROI_FC_E_po_P3_Inc_Latency.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(postP3Lat_Incongruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postP3Lat_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postP3Lat_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/postP3Lat_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
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
write_xlsx(changeP3Lat_Congruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeP3Lat_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeP3Lat_Congruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeP3Lat_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
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
write_xlsx(changeP3Lat_Incongruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeP3Lat_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeP3Lat_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeP3Lat_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
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
write_xlsx(changeSAA_PostP3Lat_Congruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostP3Lat_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostP3Lat_Congruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostP3Lat_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
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
write_xlsx(changeSAA_PostP3Lat_Incongruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostP3Lat_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostP3Lat_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostP3Lat_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostP3Lat_Incongruent_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostP3Lat_Incongruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostP3Lat_Incongruent_Model1_Mediation.df)
model.Y.changeSAA_PostP3Lat_Incongruent_Model1_Mediation<- lm(PostP3Lat~Intervention + SAA_Change, longformat.changeSAA_PostP3Lat_Incongruent_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostP3Lat_Incongruent_Model1_Mediation<- mediate(model.M.changeSAA_PostP3Lat_Incongruent_Model1_Mediation, model.Y.changeSAA_PostP3Lat_Incongruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostP3Lat_Incongruent_Model1_Mediation)

# MEDIATIONS - BRAIN FUNCTION OUTCOMES - Flanker P3 Amplitude --------------
#M1 Mediation - X = intervention group, M = sAA at Timepoint 4, Y = post-intervention congruent P3 Amplitude
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Amplitude, FC_Rest_E_po_P3_Con_CP1_Amplitude, FC_Rest_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.df <-data.frame(FC_E_po_P3_Con_CPZ_Amplitude, FC_E_po_P3_Con_CP1_Amplitude, FC_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Con_Amplitude.df)
#create dataframe 
postP3Amp_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(postP3Amp_Congruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postP3Amp_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postP3Amp_Congruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/postP3Amp_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_4_Exercise<-log10(df$SAA_4_Exercise)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Inc_CPZ_Amplitude, FC_Rest_E_po_P3_Inc_CP1_Amplitude, FC_Rest_E_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Inc_Amplitude.df <-data.frame(FC_E_po_P3_Inc_CPZ_Amplitude, FC_E_po_P3_Inc_CP1_Amplitude, FC_E_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Inc_Amplitude.df)
#create dataframe 
postP3Amp_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Exercise, SAA_4_Rest_E, Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(postP3Amp_Incongruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postP3Amp_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postP3Amp_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/postP3Amp_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
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
write_xlsx(changeP3Amp_Congruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeP3Amp_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeP3Amp_Congruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeP3Amp_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeP3Amp_Congruent_Model1_Mediation.df)
#mediation computation
model.M.changeP3Amp_Congruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeP3Amp_Congruent_Model1_Mediation.df)
model.Y.changeP3Amp_Congruent_Model1_Mediation<- lm(MeanACC_Change~Intervention + SAA_Change, longformat.changeP3Amp_Congruent_Model1_Mediation.df)
set.seed(123)
results.changeP3Amp_Congruent_Model1_Mediation<- mediate(model.M.changeP3Amp_Congruent_Model1_Mediation, model.Y.changeP3Amp_Congruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeP3Amp_Congruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = change in Incongruent Flanker P3 Amplitude
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.df <-data.frame(FC_Rest_E_pre_P3_Inc_CPZ_Amplitude, FC_Rest_E_pre_P3_Inc_CP1_Amplitude, FC_Rest_E_pre_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.df)
Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.df <-data.frame(FC_E_pre_P3_Inc_CPZ_Amplitude, FC_E_pre_P3_Inc_CP1_Amplitude, FC_E_pre_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.df)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Inc_CPZ_Amplitude, FC_Rest_E_po_P3_Inc_CP1_Amplitude, FC_Rest_E_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Inc_Amplitude.df <-data.frame(FC_E_po_P3_Inc_CPZ_Amplitude, FC_E_po_P3_Inc_CP1_Amplitude, FC_E_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Inc_Amplitude.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
SAya_FL_Incongruent_P3Amp_Rest_E_Change<- Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean-Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean
SAya_FL_Incongruent_P3Amp_E_Change<- Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean-Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean
#create dataframe 
changeP3Amp_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, SAya_FL_Incongruent_P3Amp_E_Change, SAya_FL_Incongruent_P3Amp_Rest_E_Change)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeP3Amp_Incongruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeP3Amp_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeP3Amp_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeP3Amp_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeP3Amp_Incongruent_Model1_Mediation.df)
#mediation computation
model.M.changeP3Amp_Incongruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeP3Amp_Incongruent_Model1_Mediation.df)
model.Y.changeP3Amp_Incongruent_Model1_Mediation<- lm(MeanACC_Change~Intervention + SAA_Change, longformat.changeP3Amp_Incongruent_Model1_Mediation.df)
set.seed(123)
results.changeP3Amp_Incongruent_Model1_Mediation<- mediate(model.M.changeP3Amp_Incongruent_Model1_Mediation, model.Y.changeP3Amp_Incongruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeP3Amp_Incongruent_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = Post Congruent Flanker P3 Amplitude
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
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
write_xlsx(changeSAA_PostP3Amp_Congruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostP3Amp_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostP3Amp_Congruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostP3Amp_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
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
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
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
write_xlsx(changeSAA_PostP3Amp_Incongruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostP3Amp_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostP3Amp_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostP3Amp_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostP3Amp_Incongruent_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostP3Amp_Incongruent_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostP3Amp_Incongruent_Model1_Mediation.df)
model.Y.changeSAA_PostP3Amp_Incongruent_Model1_Mediation<- lm(PostP3Amp~Intervention + SAA_Change, longformat.changeSAA_PostP3Amp_Incongruent_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostP3Amp_Incongruent_Model1_Mediation<- mediate(model.M.changeSAA_PostP3Amp_Incongruent_Model1_Mediation, model.Y.changeSAA_PostP3Amp_Incongruent_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostP3Amp_Incongruent_Model1_Mediation)

# MEDIATIONS - BRAIN FUNCTION OUTCOMES - Resting Posterior Alpha_EO ---------------
#M1 Mediation - X = intervention group, M = sAA at Timepoint 4, Y = post-intervention resting alpha
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
ROI_FC_Rest_E_post_Alpha.df <-data.frame(P1_Alpha_EO_Rest_E_Post, PZ_Alpha_EO_Rest_E_Post, P2_Alpha_EO_Rest_E_Post, PO3_Alpha_EO_Rest_E_Post, POZ_Alpha_EO_Rest_E_Post, PO4_Alpha_EO_Rest_E_Post)
ROI_FC_Rest_E_post_Alpha.mean <- rowMeans(ROI_FC_Rest_E_post_Alpha.df)
ROI_FC_E_post_Alpha.df <-data.frame(P1_Alpha_EO_E_Post, PZ_Alpha_EO_E_Post, P2_Alpha_EO_E_Post, PO3_Alpha_EO_E_Post, POZ_Alpha_EO_E_Pre, PO4_Alpha_EO_E_Post)
ROI_FC_E_post_Alpha.mean <- rowMeans(ROI_FC_E_post_Alpha.df)
#create dataframe 
postAlpha_EO_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, ROI_FC_Rest_E_post_Alpha.mean, ROI_FC_E_post_Alpha.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(postAlpha_EO_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postAlpha_EO_Model1_Mediation.df.xlsx') #export long format df
longformat.postAlpha_EO_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/postAlpha_EO_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postAlpha_EO_Model1_Mediation.df) 
#mediation computation 
model.M.postAlpha_EO_Model1_Mediation.df<- lm(SAA~Intervention, longformat.postAlpha_EO_Model1_Mediation.df)
model.Y.postAlpha_EO_Model1_Mediation.df<- lm(Post_Alpha~Intervention + SAA, longformat.postAlpha_EO_Model1_Mediation.df)
set.seed(123)
results.postAlpha_EO_Model1_Mediation<- mediate(model.M.postAlpha_EO_Model1_Mediation.df, model.Y.postAlpha_EO_Model1_Mediation.df, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA', boot = TRUE, sims = 1000)
summary(results.postAlpha_EO_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = change in alpha 
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
ROI_FC_Rest_E_pre_Alpha.df <-data.frame(P1_Alpha_EO_Rest_E_Pre, PZ_Alpha_EO_Rest_E_Pre, P2_Alpha_EO_Rest_E_Pre, PO3_Alpha_EO_Rest_E_Pre, POZ_Alpha_EO_Rest_E_Pre, PO4_Alpha_EO_Rest_E_Pre)
ROI_FC_Rest_E_pre_Alpha.mean <- rowMeans(ROI_FC_Rest_E_pre_Alpha.df)
ROI_FC_E_pre_Alpha.df <-data.frame(P1_Alpha_EO_E_Pre, PZ_Alpha_EO_E_Pre, P2_Alpha_EO_E_Pre, PO3_Alpha_EO_E_Pre, POZ_Alpha_EO_E_Pre, PO4_Alpha_EO_E_Pre)
ROI_FC_E_pre_Alpha.mean <- rowMeans(ROI_FC_E_pre_Alpha.df)
ROI_FC_Rest_E_post_Alpha.df <-data.frame(P1_Alpha_EO_Rest_E_Post, PZ_Alpha_EO_Rest_E_Post, P2_Alpha_EO_Rest_E_Post, PO3_Alpha_EO_Rest_E_Post, POZ_Alpha_EO_Rest_E_Post, PO4_Alpha_EO_Rest_E_Post)
ROI_FC_Rest_E_post_Alpha.mean <- rowMeans(ROI_FC_Rest_E_post_Alpha.df)
ROI_FC_E_post_Alpha.df <-data.frame(P1_Alpha_EO_E_Post, PZ_Alpha_EO_E_Post, P2_Alpha_EO_E_Post, PO3_Alpha_EO_E_Post, POZ_Alpha_EO_E_Pre, PO4_Alpha_EO_E_Post)
ROI_FC_E_post_Alpha.mean <- rowMeans(ROI_FC_E_post_Alpha.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
ROI_Alpha_EO_Rest_E_Change<- ROI_FC_Rest_E_post_Alpha.mean-ROI_FC_Rest_E_pre_Alpha.mean
ROI_Alpha_EO_E_Change<- ROI_FC_E_post_Alpha.mean-ROI_FC_E_pre_Alpha.mean
#Create dataframe
changeAlpha_EO_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, ROI_Alpha_EO_Rest_E_Change, ROI_Alpha_EO_E_Change)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeAlpha_EO_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeAlpha_EO_Model1_Mediation.df.xlsx') #export long format df
longformat.changeAlpha_EO_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeAlpha_EO_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeAlpha_EO_Model1_Mediation.df)
#mediation computation
model.M.changeAlpha_EO_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeAlpha_EO_Model1_Mediation.df)
model.Y.changeAlpha_EO_Model1_Mediation<- lm(Alpha_EO_Change~Intervention + SAA_Change, longformat.changeAlpha_EO_Model1_Mediation.df)
set.seed(123)
results.changeAlpha_EO_Model1_Mediation<- mediate(model.M.changeAlpha_EO_Model1_Mediation, model.Y.changeAlpha_EO_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeAlpha_EO_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = post alpha 
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
ROI_FC_Rest_E_post_Alpha.df <-data.frame(P1_Alpha_EO_Rest_E_Post, PZ_Alpha_EO_Rest_E_Post, P2_Alpha_EO_Rest_E_Post, PO3_Alpha_EO_Rest_E_Post, POZ_Alpha_EO_Rest_E_Post, PO4_Alpha_EO_Rest_E_Post)
ROI_FC_Rest_E_post_Alpha.mean <- rowMeans(ROI_FC_Rest_E_post_Alpha.df)
ROI_FC_E_post_Alpha.df <-data.frame(P1_Alpha_EO_E_Post, PZ_Alpha_EO_E_Post, P2_Alpha_EO_E_Post, PO3_Alpha_EO_E_Post, POZ_Alpha_EO_E_Pre, PO4_Alpha_EO_E_Post)
ROI_FC_E_post_Alpha.mean <- rowMeans(ROI_FC_E_post_Alpha.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#Create dataframe
changeSAA_PostAlpha_EO_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, ROI_FC_Rest_E_post_Alpha.mean, ROI_FC_E_post_Alpha.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostAlpha_EO_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostAlpha_EO_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostAlpha_EO_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostAlpha_EO_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostAlpha_EO_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostAlpha_EO_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostAlpha_EO_Model1_Mediation.df)
model.Y.changeSAA_PostAlpha_EO_Model1_Mediation<- lm(PostAlpha~Intervention + SAA_Change, longformat.changeSAA_PostAlpha_EO_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostAlpha_EO_Model1_Mediation<- mediate(model.M.changeSAA_PostAlpha_EO_Model1_Mediation, model.Y.changeSAA_PostAlpha_EO_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostAlpha_EO_Model1_Mediation)

# MEDIATIONS - BRAIN FUNCTION OUTCOMES - Resting Posterior Alpha_EC ---------------
#M1 Mediation - X = intervention group, M = sAA at Timepoint 4, Y = post-intervention resting alpha
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
ROI_FC_Rest_E_post_Alpha.df <-data.frame(P1_Alpha_EC_Rest_E_Post, PZ_Alpha_EC_Rest_E_Post, P2_Alpha_EC_Rest_E_Post, PO3_Alpha_EC_Rest_E_Post, POZ_Alpha_EC_Rest_E_Post, PO4_Alpha_EC_Rest_E_Post)
ROI_FC_Rest_E_post_Alpha.mean <- rowMeans(ROI_FC_Rest_E_post_Alpha.df)
ROI_FC_E_post_Alpha.df <-data.frame(P1_Alpha_EC_E_Post, PZ_Alpha_EC_E_Post, P2_Alpha_EC_E_Post, PO3_Alpha_EC_E_Post, POZ_Alpha_EC_E_Pre, PO4_Alpha_EC_E_Post)
ROI_FC_E_post_Alpha.mean <- rowMeans(ROI_FC_E_post_Alpha.df)
#create dataframe 
postAlpha_EC_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, ROI_FC_Rest_E_post_Alpha.mean, ROI_FC_E_post_Alpha.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(postAlpha_EC_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postAlpha_EC_Model1_Mediation.df.xlsx') #export long format df
longformat.postAlpha_EC_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/postAlpha_EC_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postAlpha_EC_Model1_Mediation.df) 
#mediation computation 
model.M.postAlpha_EC_Model1_Mediation.df<- lm(SAA~Intervention, longformat.postAlpha_EC_Model1_Mediation.df)
model.Y.postAlpha_EC_Model1_Mediation.df<- lm(Post_Alpha~Intervention + SAA, longformat.postAlpha_EC_Model1_Mediation.df)
set.seed(123)
results.postAlpha_EC_Model1_Mediation<- mediate(model.M.postAlpha_EC_Model1_Mediation.df, model.Y.postAlpha_EC_Model1_Mediation.df, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA', boot = TRUE, sims = 1000)
summary(results.postAlpha_EC_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = change in alpha 
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
ROI_FC_Rest_E_pre_Alpha.df <-data.frame(P1_Alpha_EC_Rest_E_Pre, PZ_Alpha_EC_Rest_E_Pre, P2_Alpha_EC_Rest_E_Pre, PO3_Alpha_EC_Rest_E_Pre, POZ_Alpha_EC_Rest_E_Pre, PO4_Alpha_EC_Rest_E_Pre)
ROI_FC_Rest_E_pre_Alpha.mean <- rowMeans(ROI_FC_Rest_E_pre_Alpha.df)
ROI_FC_E_pre_Alpha.df <-data.frame(P1_Alpha_EC_E_Pre, PZ_Alpha_EC_E_Pre, P2_Alpha_EC_E_Pre, PO3_Alpha_EC_E_Pre, POZ_Alpha_EC_E_Pre, PO4_Alpha_EC_E_Pre)
ROI_FC_E_pre_Alpha.mean <- rowMeans(ROI_FC_E_pre_Alpha.df)
ROI_FC_Rest_E_post_Alpha.df <-data.frame(P1_Alpha_EC_Rest_E_Post, PZ_Alpha_EC_Rest_E_Post, P2_Alpha_EC_Rest_E_Post, PO3_Alpha_EC_Rest_E_Post, POZ_Alpha_EC_Rest_E_Post, PO4_Alpha_EC_Rest_E_Post)
ROI_FC_Rest_E_post_Alpha.mean <- rowMeans(ROI_FC_Rest_E_post_Alpha.df)
ROI_FC_E_post_Alpha.df <-data.frame(P1_Alpha_EC_E_Post, PZ_Alpha_EC_E_Post, P2_Alpha_EC_E_Post, PO3_Alpha_EC_E_Post, POZ_Alpha_EC_E_Pre, PO4_Alpha_EC_E_Post)
ROI_FC_E_post_Alpha.mean <- rowMeans(ROI_FC_E_post_Alpha.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
ROI_Alpha_EC_Rest_E_Change<- ROI_FC_Rest_E_post_Alpha.mean-ROI_FC_Rest_E_pre_Alpha.mean
ROI_Alpha_EC_E_Change<- ROI_FC_E_post_Alpha.mean-ROI_FC_E_pre_Alpha.mean
#Create dataframe
changeAlpha_EC_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, ROI_Alpha_EC_Rest_E_Change, ROI_Alpha_EC_E_Change)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeAlpha_EC_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeAlpha_EC_Model1_Mediation.df.xlsx') #export long format df
longformat.changeAlpha_EC_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeAlpha_EC_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeAlpha_EC_Model1_Mediation.df)
#mediation computation
model.M.changeAlpha_EC_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeAlpha_EC_Model1_Mediation.df)
model.Y.changeAlpha_EC_Model1_Mediation<- lm(Alpha_EC_Change~Intervention + SAA_Change, longformat.changeAlpha_EC_Model1_Mediation.df)
set.seed(123)
results.changeAlpha_EC_Model1_Mediation<- mediate(model.M.changeAlpha_EC_Model1_Mediation, model.Y.changeAlpha_EC_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeAlpha_EC_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = post alpha 
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
ROI_FC_Rest_E_post_Alpha.df <-data.frame(P1_Alpha_EC_Rest_E_Post, PZ_Alpha_EC_Rest_E_Post, P2_Alpha_EC_Rest_E_Post, PO3_Alpha_EC_Rest_E_Post, POZ_Alpha_EC_Rest_E_Post, PO4_Alpha_EC_Rest_E_Post)
ROI_FC_Rest_E_post_Alpha.mean <- rowMeans(ROI_FC_Rest_E_post_Alpha.df)
ROI_FC_E_post_Alpha.df <-data.frame(P1_Alpha_EC_E_Post, PZ_Alpha_EC_E_Post, P2_Alpha_EC_E_Post, PO3_Alpha_EC_E_Post, POZ_Alpha_EC_E_Pre, PO4_Alpha_EC_E_Post)
ROI_FC_E_post_Alpha.mean <- rowMeans(ROI_FC_E_post_Alpha.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#Create dataframe
changeSAA_PostAlpha_EC_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, ROI_FC_Rest_E_post_Alpha.mean, ROI_FC_E_post_Alpha.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostAlpha_EC_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostAlpha_EC_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostAlpha_EC_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostAlpha_EC_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostAlpha_EC_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostAlpha_EC_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostAlpha_EC_Model1_Mediation.df)
model.Y.changeSAA_PostAlpha_EC_Model1_Mediation<- lm(PostAlpha~Intervention + SAA_Change, longformat.changeSAA_PostAlpha_EC_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostAlpha_EC_Model1_Mediation<- mediate(model.M.changeSAA_PostAlpha_EC_Model1_Mediation, model.Y.changeSAA_PostAlpha_EC_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostAlpha_EC_Model1_Mediation)

# MEDIATIONS - BRAIN FUNCTION OUTCOMES - Resting Midfrontal Theta_EO ---------
#M1 Mediation - X = intervention group, M = sAA at Timepoint 4, Y = post-intervention resting theta
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_EO_E_Post, FZ_Theta_EO_E_Post, F2_Theta_EO_E_Post, FC1_Theta_EO_E_Post, FCZ_Theta_EO_E_Post, FC2_Theta_EO_E_Post, C1_Theta_EO_E_Post, CZ_Theta_EO_E_Post, C2_Theta_EO_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_EO_Rest_E_Post, FZ_Theta_EO_Rest_E_Post, F2_Theta_EO_Rest_E_Post, FC1_Theta_EO_Rest_E_Post, FCZ_Theta_EO_Rest_E_Post, FC2_Theta_EO_Rest_E_Post, C1_Theta_EO_Rest_E_Post, CZ_Theta_EO_Rest_E_Post, C2_Theta_EO_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#create dataframe 
postTheta_EO_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(postTheta_EO_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postTheta_EO_Model1_Mediation.df.xlsx') #export long format df
longformat.postTheta_EO_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/postTheta_EO_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postTheta_EO_Model1_Mediation.df)
#mediation computation 
model.M.postTheta_EO_Model1_Mediation.df<- lm(SAA~Intervention, longformat.postTheta_EO_Model1_Mediation.df)
model.Y.postTheta_EO_Model1_Mediation.df<- lm(Post_Theta~Intervention + SAA, longformat.postTheta_EO_Model1_Mediation.df)
set.seed(123)
results.postTheta_EO_Model1_Mediation<- mediate(model.M.postTheta_EO_Model1_Mediation.df, model.Y.postTheta_EO_Model1_Mediation.df, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA', boot = TRUE, sims = 1000)
summary(results.postTheta_EO_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = change in theta 
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
Wide.ROI_FC_E_pre_Theta.df <-data.frame(F1_Theta_EO_E_Pre, FZ_Theta_EO_E_Pre, F2_Theta_EO_E_Pre, FC1_Theta_EO_E_Pre, FCZ_Theta_EO_E_Pre, FC2_Theta_EO_E_Pre, C1_Theta_EO_E_Pre, CZ_Theta_EO_E_Pre, C2_Theta_EO_E_Pre)
Wide.ROI_FC_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_E_pre_Theta.df)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_EO_E_Post, FZ_Theta_EO_E_Post, F2_Theta_EO_E_Post, FC1_Theta_EO_E_Post, FCZ_Theta_EO_E_Post, FC2_Theta_EO_E_Post, C1_Theta_EO_E_Post, CZ_Theta_EO_E_Post, C2_Theta_EO_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_pre_Theta.df <-data.frame(F1_Theta_EO_Rest_E_Pre, FZ_Theta_EO_Rest_E_Pre, F2_Theta_EO_Rest_E_Pre, FC1_Theta_EO_Rest_E_Pre, FCZ_Theta_EO_Rest_E_Pre, FC2_Theta_EO_Rest_E_Pre, C1_Theta_EO_Rest_E_Pre, CZ_Theta_EO_Rest_E_Pre, C2_Theta_EO_Rest_E_Pre)
Wide.ROI_FC_Rest_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_pre_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_EO_Rest_E_Post, FZ_Theta_EO_Rest_E_Post, F2_Theta_EO_Rest_E_Post, FC1_Theta_EO_Rest_E_Post, FCZ_Theta_EO_Rest_E_Post, FC2_Theta_EO_Rest_E_Post, C1_Theta_EO_Rest_E_Post, CZ_Theta_EO_Rest_E_Post, C2_Theta_EO_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)

#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
ROI_Theta_EO_Rest_E_Change<- Wide.ROI_FC_Rest_E_post_Theta.mean-Wide.ROI_FC_Rest_E_pre_Theta.mean
ROI_Theta_EO_E_Change<- Wide.ROI_FC_E_post_Theta.mean-Wide.ROI_FC_E_pre_Theta.mean
#Create dataframe
changeTheta_EO_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, ROI_Theta_EO_Rest_E_Change, ROI_Theta_EO_E_Change)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeTheta_EO_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeTheta_EO_Model1_Mediation.df.xlsx') #export long format df
longformat.changeTheta_EO_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeTheta_EO_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeTheta_EO_Model1_Mediation.df)
#mediation computation
model.M.changeTheta_EO_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeTheta_EO_Model1_Mediation.df)
model.Y.changeTheta_EO_Model1_Mediation<- lm(Theta_EO_Change~Intervention + SAA_Change, longformat.changeTheta_EO_Model1_Mediation.df)
set.seed(123)
results.changeTheta_EO_Model1_Mediation<- mediate(model.M.changeTheta_EO_Model1_Mediation, model.Y.changeTheta_EO_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeTheta_EO_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = post Theta 
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-df$SAA_4_Rest_E
SAA_4_Exercise<- df$SAA_4_Exercise
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_EO_E_Post, FZ_Theta_EO_E_Post, F2_Theta_EO_E_Post, FC1_Theta_EO_E_Post, FCZ_Theta_EO_E_Post, FC2_Theta_EO_E_Post, C1_Theta_EO_E_Post, CZ_Theta_EO_E_Post, C2_Theta_EO_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_EO_Rest_E_Post, FZ_Theta_EO_Rest_E_Post, F2_Theta_EO_Rest_E_Post, FC1_Theta_EO_Rest_E_Post, FCZ_Theta_EO_Rest_E_Post, FC2_Theta_EO_Rest_E_Post, C1_Theta_EO_Rest_E_Post, CZ_Theta_EO_Rest_E_Post, C2_Theta_EO_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#Create dataframe
changeSAA_PostTheta_EO_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, Wide.ROI_FC_Rest_E_post_Theta.mean, Wide.ROI_FC_E_post_Theta.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostTheta_EO_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostTheta_EO_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostTheta_EO_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostTheta_EO_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostTheta_EO_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostTheta_EO_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostTheta_EO_Model1_Mediation.df)
model.Y.changeSAA_PostTheta_EO_Model1_Mediation<- lm(PostTheta~Intervention + SAA_Change, longformat.changeSAA_PostTheta_EO_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostTheta_EO_Model1_Mediation<- mediate(model.M.changeSAA_PostTheta_EO_Model1_Mediation, model.Y.changeSAA_PostTheta_EO_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostTheta_EO_Model1_Mediation)

# MEDIATIONS - BRAIN FUNCTION OUTCOMES - Resting Midfrontal Theta_EC ---------
#M1 Mediation - X = intervention group, M = sAA at Timepoint 4, Y = post-intervention resting theta
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_4_Exercise<-log10(df$SAA_4_Exercise)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_EC_E_Post, FZ_Theta_EC_E_Post, F2_Theta_EC_E_Post, FC1_Theta_EC_E_Post, FCZ_Theta_EC_E_Post, FC2_Theta_EC_E_Post, C1_Theta_EC_E_Post, CZ_Theta_EC_E_Post, C2_Theta_EC_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_EC_Rest_E_Post, FZ_Theta_EC_Rest_E_Post, F2_Theta_EC_Rest_E_Post, FC1_Theta_EC_Rest_E_Post, FCZ_Theta_EC_Rest_E_Post, FC2_Theta_EC_Rest_E_Post, C1_Theta_EC_Rest_E_Post, CZ_Theta_EC_Rest_E_Post, C2_Theta_EC_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#create dataframe 
postTheta_EC_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_4_Rest_E, SAA_4_Exercise, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(postTheta_EC_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postTheta_EC_Model1_Mediation.df.xlsx') #export long format df
longformat.postTheta_EC_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/postTheta_EC_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postTheta_EC_Model1_Mediation.df)
#mediation computation 
model.M.postTheta_EC_Model1_Mediation.df<- lm(SAA~Intervention, longformat.postTheta_EC_Model1_Mediation.df)
model.Y.postTheta_EC_Model1_Mediation.df<- lm(Post_Theta~Intervention + SAA, longformat.postTheta_EC_Model1_Mediation.df)
set.seed(123)
results.postTheta_EC_Model1_Mediation<- mediate(model.M.postTheta_EC_Model1_Mediation.df, model.Y.postTheta_EC_Model1_Mediation.df, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA', boot = TRUE, sims = 1000)
summary(results.postTheta_EC_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = change in theta 
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
Wide.ROI_FC_E_pre_Theta.df <-data.frame(F1_Theta_EC_E_Pre, FZ_Theta_EC_E_Pre, F2_Theta_EC_E_Pre, FC1_Theta_EC_E_Pre, FCZ_Theta_EC_E_Pre, FC2_Theta_EC_E_Pre, C1_Theta_EC_E_Pre, CZ_Theta_EC_E_Pre, C2_Theta_EC_E_Pre)
Wide.ROI_FC_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_E_pre_Theta.df)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_EC_E_Post, FZ_Theta_EC_E_Post, F2_Theta_EC_E_Post, FC1_Theta_EC_E_Post, FCZ_Theta_EC_E_Post, FC2_Theta_EC_E_Post, C1_Theta_EC_E_Post, CZ_Theta_EC_E_Post, C2_Theta_EC_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_pre_Theta.df <-data.frame(F1_Theta_EC_Rest_E_Pre, FZ_Theta_EC_Rest_E_Pre, F2_Theta_EC_Rest_E_Pre, FC1_Theta_EC_Rest_E_Pre, FCZ_Theta_EC_Rest_E_Pre, FC2_Theta_EC_Rest_E_Pre, C1_Theta_EC_Rest_E_Pre, CZ_Theta_EC_Rest_E_Pre, C2_Theta_EC_Rest_E_Pre)
Wide.ROI_FC_Rest_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_pre_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_EC_Rest_E_Post, FZ_Theta_EC_Rest_E_Post, F2_Theta_EC_Rest_E_Post, FC1_Theta_EC_Rest_E_Post, FCZ_Theta_EC_Rest_E_Post, FC2_Theta_EC_Rest_E_Post, C1_Theta_EC_Rest_E_Post, CZ_Theta_EC_Rest_E_Post, C2_Theta_EC_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
ROI_Theta_EC_Rest_E_Change<- Wide.ROI_FC_Rest_E_post_Theta.mean-Wide.ROI_FC_Rest_E_pre_Theta.mean
ROI_Theta_EC_E_Change<- Wide.ROI_FC_E_post_Theta.mean-Wide.ROI_FC_E_pre_Theta.mean
#Create dataframe
changeTheta_EC_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, ROI_Theta_EC_Rest_E_Change, ROI_Theta_EC_E_Change)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeTheta_EC_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeTheta_EC_Model1_Mediation.df.xlsx') #export long format df
longformat.changeTheta_EC_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeTheta_EC_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeTheta_EC_Model1_Mediation.df)
#mediation computation
model.M.changeTheta_EC_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeTheta_EC_Model1_Mediation.df)
model.Y.changeTheta_EC_Model1_Mediation<- lm(Theta_EC_Change~Intervention + SAA_Change, longformat.changeTheta_EC_Model1_Mediation.df)
set.seed(123)
results.changeTheta_EC_Model1_Mediation<- mediate(model.M.changeTheta_EC_Model1_Mediation, model.Y.changeTheta_EC_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeTheta_EC_Model1_Mediation)

#M1 Mediation - X = intervention group, M = change in SAA from timepoint 1 to 4, Y = post Theta 
#Call Variables #may need to load variables in P3 section 
SubjectID<- df$SubjectID
b_rand_string<-df$b_rand_string
Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
SAA_1_Rest_E<-log10(df$SAA_1_Rest_E)
SAA_4_Rest_E<-log10(df$SAA_4_Rest_E)
SAA_1_Exercise<- log10(df$SAA_1_Exercise)
SAA_4_Exercise<- log10(df$SAA_4_Exercise)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_EC_E_Post, FZ_Theta_EC_E_Post, F2_Theta_EC_E_Post, FC1_Theta_EC_E_Post, FCZ_Theta_EC_E_Post, FC2_Theta_EC_E_Post, C1_Theta_EC_E_Post, CZ_Theta_EC_E_Post, C2_Theta_EC_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_EC_Rest_E_Post, FZ_Theta_EC_Rest_E_Post, F2_Theta_EC_Rest_E_Post, FC1_Theta_EC_Rest_E_Post, FCZ_Theta_EC_Rest_E_Post, FC2_Theta_EC_Rest_E_Post, C1_Theta_EC_Rest_E_Post, CZ_Theta_EC_Rest_E_Post, C2_Theta_EC_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#Create change scores 
SAA_Rest_E_Change<- SAA_4_Rest_E-SAA_1_Rest_E
SAA_Exercise_Change <- SAA_4_Exercise-SAA_1_Exercise
#Create dataframe
changeSAA_PostTheta_EC_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Rest_E_TOD, E_TOD, SAA_Rest_E_Change, SAA_Exercise_Change, Wide.ROI_FC_Rest_E_post_Theta.mean, Wide.ROI_FC_E_post_Theta.mean)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeSAA_PostTheta_EC_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostTheta_EC_Model1_Mediation.df.xlsx') #export long format df
longformat.changeSAA_PostTheta_EC_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeSAA_PostTheta_EC_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeSAA_PostTheta_EC_Model1_Mediation.df)
#mediation computation
model.M.changeSAA_PostTheta_EC_Model1_Mediation<- lm(SAA_Change~Intervention, longformat.changeSAA_PostTheta_EC_Model1_Mediation.df)
model.Y.changeSAA_PostTheta_EC_Model1_Mediation<- lm(PostTheta~Intervention + SAA_Change, longformat.changeSAA_PostTheta_EC_Model1_Mediation.df)
set.seed(123)
results.changeSAA_PostTheta_EC_Model1_Mediation<- mediate(model.M.changeSAA_PostTheta_EC_Model1_Mediation, model.Y.changeSAA_PostTheta_EC_Model1_Mediation, covariates = 'TOD', treat = 'Intervention', mediator = 'SAA_Change', boot = TRUE, sims = 1000)
summary(results.changeSAA_PostTheta_EC_Model1_Mediation)

# Exploratory Analyses - Saliva COVID Interaction ------------------------------------------------
Cort_1_Rest_E <- df$Cort_1_Rest_E
Cort_1_Exercise <- (df$Cort_1_Exercise)
Cort_1_Rest_T <- (df$Cort_1_Rest_T)
Cort_1_Trier <- (df$Cort_1_Trier)

CORT_1_interventionmean_M1<-data.frame(Cort_1_Rest_T, Cort_1_Trier)
CORT_1_interventionmean_M1 <- rowMeans(CORT_1_interventionmean_M1)

Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
TOD_M1<-data.frame(Rest_E_TOD, E_TOD)
TOD_M1<-rowMeans(TOD_M1)

Rest_T_TOD<-df$r_end_time_T
T_TOD<-df$start_time
TOD_M2<-data.frame(Rest_T_TOD, T_TOD)
TOD_M2<-rowMeans(TOD_M2)

COVID_Status<-df$COVID_Status
SubjectID<-df$SubjectID

df.COVID<-data.frame(SubjectID, COVID_Status, CORT_1_interventionmean_M2, TOD_M1)

ggplot(df.COVID, aes(x=COVID_Status, y=CORT_1_interventionmean_M1, fill=COVID_Status)) + 
  geom_violin()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(y = "Cortisol", x="COVID Status", title = "Cortisol Levels by Lockdown Status", fill = "Lockdown Status")+
  theme(plot.title = element_text(hjust=0.5))



SAA_1_Rest_E <- log10(df$Cort_1_Rest_E)+2
SAA_1_Exercise <- log10(df$Cort_1_Exercise)+2
SAA_1_Rest_T <- log10(df$Cort_1_Rest_T)+2
SAA_1_Trier <- log10(df$Cort_1_Trier)+2

SAA_1_interventionmean_M1<-data.frame(SAA_1_Rest_E, SAA_1_Exercise)
SAA_1_interventionmean_M1 <- rowMeans(SAA_1_interventionmean_M1)

Rest_E_TOD<-df$r_end_time_E
E_TOD<-df$e_start_time
TOD_M1<-data.frame(Rest_E_TOD, E_TOD)
TOD_M1<-rowMeans(TOD_M1)

Rest_T_TOD<-df$r_end_time_T
T_TOD<-df$start_time
TOD_M2<-data.frame(Rest_T_TOD, T_TOD)
TOD_M2<-rowMeans(TOD_M2)

COVID_Status<-df$COVID_Status
SubjectID<-df$SubjectID

df.COVID<-data.frame(SubjectID, COVID_Status, SAA_1_interventionmean_M1, TOD_M1)

ggplot(df.COVID, aes(x=COVID_Status, y=SAA_1_interventionmean_M1, fill=COVID_Status)) + 
  geom_violin()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(y = "SAA", x="COVID Status", title = "SAA Levels by Lockdown Status", fill = "Lockdown Status")+
  theme(plot.title = element_text(hjust=0.5))
# Exploratory Analyses - COGNITIVE AND RESTING BRAIN FUNCTION OUTCOMES - Theta_EO and Flanker Inverse Efficiency --------
#Call Variables 
SAya_FL_Congruent_InverseEfficiency_Rest_E_pre<-(df$SAya_FL_Congruent_InverseEfficiency_Rest_E_pre)
SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre<-(df$SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre)
SAya_FL_Congruent_InverseEfficiency_Rest_E_post<-(df$SAya_FL_Congruent_InverseEfficiency_Rest_E_post)
SAya_FL_Incongruent_InverseEfficiency_Rest_E_post<-(df$SAya_FL_Incongruent_InverseEfficiency_Rest_E_post)
SAya_FL_Congruent_InverseEfficiency_E_pre<-(df$SAya_FL_Congruent_InverseEfficiency_E_pre)
SAya_FL_Incongruent_InverseEfficiency_E_pre<-(df$SAya_FL_Incongruent_InverseEfficiency_E_pre)
SAya_FL_Congruent_InverseEfficiency_E_post<-(df$SAya_FL_Congruent_InverseEfficiency_E_post)
SAya_FL_Incongruent_InverseEfficiency_E_post<-(df$SAya_FL_Incongruent_InverseEfficiency_E_post)

F1_Theta_EO_E_Pre <- df$F1_Theta_EO_E_Pre
FZ_Theta_EO_E_Pre <- df$FZ_Theta_EO_E_Pre
F2_Theta_EO_E_Pre <- df$F2_Theta_EO_E_Pre
FC1_Theta_EO_E_Pre <- df$FC1_Theta_EO_E_Pre
FCZ_Theta_EO_E_Pre <- df$FCZ_Theta_EO_E_Pre
FC2_Theta_EO_E_Pre <- df$FC2_Theta_EO_E_Pre
C1_Theta_EO_E_Pre <- df$C1_Theta_EO_E_Pre
CZ_Theta_EO_E_Pre <- df$CZ_Theta_EO_E_Pre
C2_Theta_EO_E_Pre <- df$C2_Theta_EO_E_Pre
F1_Theta_EO_E_Post <- df$F1_Theta_EO_E_Post
FZ_Theta_EO_E_Post <- df$FZ_Theta_EO_E_Post
F2_Theta_EO_E_Post <- df$F2_Theta_EO_E_Post
FC1_Theta_EO_E_Post <- df$FC1_Theta_EO_E_Post
FCZ_Theta_EO_E_Post <- df$FCZ_Theta_EO_E_Post
FC2_Theta_EO_E_Post <- df$FC2_Theta_EO_E_Post
C1_Theta_EO_E_Post <- df$C1_Theta_EO_E_Post
CZ_Theta_EO_E_Post <- df$CZ_Theta_EO_E_Post
C2_Theta_EO_E_Post <- df$C2_Theta_EO_E_Post
F1_Theta_EO_Rest_E_Pre <- df$F1_Theta_EO_Rest_E_Pre
FZ_Theta_EO_Rest_E_Pre <- df$FZ_Theta_EO_Rest_E_Pre
F2_Theta_EO_Rest_E_Pre <- df$F2_Theta_EO_Rest_E_Pre
FC1_Theta_EO_Rest_E_Pre <- df$FC1_Theta_EO_Rest_E_Pre
FCZ_Theta_EO_Rest_E_Pre <- df$FCZ_Theta_EO_Rest_E_Pre
FC2_Theta_EO_Rest_E_Pre <- df$FC2_Theta_EO_Rest_E_Pre
C1_Theta_EO_Rest_E_Pre <- df$C1_Theta_EO_Rest_E_Pre
CZ_Theta_EO_Rest_E_Pre <- df$CZ_Theta_EO_Rest_E_Pre
C2_Theta_EO_Rest_E_Pre <- df$C2_Theta_EO_Rest_E_Pre
F1_Theta_EO_Rest_E_Post <- df$F1_Theta_EO_Rest_E_Post
FZ_Theta_EO_Rest_E_Post <- df$FZ_Theta_EO_Rest_E_Post
F2_Theta_EO_Rest_E_Post <- df$F2_Theta_EO_Rest_E_Post
FC1_Theta_EO_Rest_E_Post <- df$FC1_Theta_EO_Rest_E_Post
FCZ_Theta_EO_Rest_E_Post <- df$FCZ_Theta_EO_Rest_E_Post
FC2_Theta_EO_Rest_E_Post <- df$FC2_Theta_EO_Rest_E_Post
C1_Theta_EO_Rest_E_Post <- df$C1_Theta_EO_Rest_E_Post
CZ_Theta_EO_Rest_E_Post <- df$CZ_Theta_EO_Rest_E_Post
C2_Theta_EO_Rest_E_Post <- df$C2_Theta_EO_Rest_E_Post

#M1 Resting Posterior Theta x Flanker RT in Exercise vs. Rest 
##Average together Resting Theta ROI site
Wide.ROI_FC_E_pre_Theta.df <-data.frame(F1_Theta_EO_E_Pre, FZ_Theta_EO_E_Pre, F2_Theta_EO_E_Pre, FC1_Theta_EO_E_Pre, FCZ_Theta_EO_E_Pre, FC2_Theta_EO_E_Pre, C1_Theta_EO_E_Pre, CZ_Theta_EO_E_Pre, C2_Theta_EO_E_Pre)
Wide.ROI_FC_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_E_pre_Theta.df)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_EO_E_Post, FZ_Theta_EO_E_Post, F2_Theta_EO_E_Post, FC1_Theta_EO_E_Post, FCZ_Theta_EO_E_Post, FC2_Theta_EO_E_Post, C1_Theta_EO_E_Post, CZ_Theta_EO_E_Post, C2_Theta_EO_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_pre_Theta.df <-data.frame(F1_Theta_EO_Rest_E_Pre, FZ_Theta_EO_Rest_E_Pre, F2_Theta_EO_Rest_E_Pre, FC1_Theta_EO_Rest_E_Pre, FCZ_Theta_EO_Rest_E_Pre, FC2_Theta_EO_Rest_E_Pre, C1_Theta_EO_Rest_E_Pre, CZ_Theta_EO_Rest_E_Pre, C2_Theta_EO_Rest_E_Pre)
Wide.ROI_FC_Rest_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_pre_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_EO_Rest_E_Post, FZ_Theta_EO_Rest_E_Post, F2_Theta_EO_Rest_E_Post, FC1_Theta_EO_Rest_E_Post, FCZ_Theta_EO_Rest_E_Post, FC2_Theta_EO_Rest_E_Post, C1_Theta_EO_Rest_E_Post, CZ_Theta_EO_Rest_E_Post, C2_Theta_EO_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#create dataframe 
M1_Theta_EO_x_IE.df <- data.frame(SubjectID,Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
                                  SAya_FL_Congruent_InverseEfficiency_Rest_E_pre, SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre, SAya_FL_Congruent_InverseEfficiency_Rest_E_post, SAya_FL_Incongruent_InverseEfficiency_Rest_E_post, 
                                  SAya_FL_Congruent_InverseEfficiency_E_pre, SAya_FL_Incongruent_InverseEfficiency_E_pre, SAya_FL_Congruent_InverseEfficiency_E_post, SAya_FL_Incongruent_InverseEfficiency_E_post)
#melt
longformat.M1_Theta_EO_x_IE.df <- melt(M1_Theta_EO_x_IE.df, id = "SubjectID", variable.name = "Condition")
#Check for outliers 
longformat.M1_Theta_EO_x_IE.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#normality test 
M1_Theta_EO_x_IE.df %>%
  shapiro_test(Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
               SAya_FL_Congruent_InverseEfficiency_Rest_E_pre, SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre, SAya_FL_Congruent_InverseEfficiency_Rest_E_post, SAya_FL_Incongruent_InverseEfficiency_Rest_E_post, 
               SAya_FL_Congruent_InverseEfficiency_E_pre, SAya_FL_Incongruent_InverseEfficiency_E_pre, SAya_FL_Congruent_InverseEfficiency_E_post, SAya_FL_Incongruent_InverseEfficiency_E_post)
#Computation - Pre_E_Congruent
summary(lm(formula = SAya_FL_Congruent_InverseEfficiency_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggplot(M1_Theta_EO_x_IE.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Congruent_InverseEfficiency_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_InverseEfficiency_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggscatterstats(data = M1_Theta_EO_x_IE.df, y = SAya_FL_Incongruent_InverseEfficiency_E_pre, x = Wide.ROI_FC_E_pre_Theta.mean, type = "parametric")
ggplot(M1_Theta_EO_x_IE.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Incongruent_InverseEfficiency_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Congruent
summary(lm(formula = SAya_FL_Congruent_InverseEfficiency_E_pre~Wide.ROI_FC_E_post_Theta.mean))
ggscatterstats(data = M1_Theta_EO_x_IE.df, y = SAya_FL_Congruent_InverseEfficiency_E_post, x = Wide.ROI_FC_E_post_Theta.mean, type = "parametric")
ggplot(M1_Theta_EO_x_IE.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Congruent_InverseEfficiency_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_InverseEfficiency_E_post~Wide.ROI_FC_E_post_Theta.mean))
ggscatterstats(data = M1_Theta_EO_x_IE.df, y = SAya_FL_Incongruent_InverseEfficiency_E_post, x = Wide.ROI_FC_E_post_Theta.mean, type = "parametric")
ggplot(M1_Theta_EO_x_IE.df, aes(x=Wide.ROI_FC_E_post_Theta.mean, y=SAya_FL_Incongruent_InverseEfficiency_E_post))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_InverseEfficiency_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggscatterstats(data = M1_Theta_EO_x_IE.df, y = SAya_FL_Congruent_InverseEfficiency_Rest_E_pre, x = Wide.ROI_FC_Rest_E_pre_Theta.mean, type = "parametric")
ggplot(M1_Theta_EO_x_IE.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Congruent_InverseEfficiency_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggscatterstats(data = M1_Theta_EO_x_IE.df, y = SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre, x = Wide.ROI_FC_Rest_E_pre_Theta.mean, type = "parametric")
ggplot(M1_Theta_EO_x_IE.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_InverseEfficiency_Rest_E_post~Wide.ROI_FC_Rest_E_post_Theta.mean))
ggscatterstats(data = M1_Theta_EO_x_IE.df, y = SAya_FL_Congruent_InverseEfficiency_Rest_E_post, x = Wide.ROI_FC_Rest_E_post_Theta.mean, type = "nonparametric")
ggplot(M1_Theta_EO_x_IE.df, aes(x=Wide.ROI_FC_Rest_E_post_Theta.mean, y=SAya_FL_Congruent_InverseEfficiency_Rest_E_post))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_InverseEfficiency_Rest_E_post~Wide.ROI_FC_Rest_E_post_Theta.mean))
ggscatterstats(data = M1_Theta_EO_x_IE.df, y = SAya_FL_Incongruent_InverseEfficiency_Rest_E_post, x = Wide.ROI_FC_Rest_E_post_Theta.mean, type = "nonparametric")
ggplot(M1_Theta_EO_x_IE.df, aes(x=Wide.ROI_FC_Rest_E_post_Theta.mean, y=SAya_FL_Incongruent_InverseEfficiency_Rest_E_post))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))

# Exploratory Analyses - COGNITIVE AND RESTING BRAIN FUNCTION OUTCOMES - Theta_EC and Flanker Inverse Efficiency --------
#Call Variables 
SAya_FL_Congruent_InverseEfficiency_Rest_E_pre<-(df$SAya_FL_Congruent_InverseEfficiency_Rest_E_pre)
SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre<-(df$SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre)
SAya_FL_Congruent_InverseEfficiency_Rest_E_post<-(df$SAya_FL_Congruent_InverseEfficiency_Rest_E_post)
SAya_FL_Incongruent_InverseEfficiency_Rest_E_post<-(df$SAya_FL_Incongruent_InverseEfficiency_Rest_E_post)
SAya_FL_Congruent_InverseEfficiency_E_pre<-(df$SAya_FL_Congruent_InverseEfficiency_E_pre)
SAya_FL_Incongruent_InverseEfficiency_E_pre<-(df$SAya_FL_Incongruent_InverseEfficiency_E_pre)
SAya_FL_Congruent_InverseEfficiency_E_post<-(df$SAya_FL_Congruent_InverseEfficiency_E_post)
SAya_FL_Incongruent_InverseEfficiency_E_post<-(df$SAya_FL_Incongruent_InverseEfficiency_E_post)

F1_Theta_EC_E_Pre <- df$F1_Theta_EC_E_Pre
FZ_Theta_EC_E_Pre <- df$FZ_Theta_EC_E_Pre
F2_Theta_EC_E_Pre <- df$F2_Theta_EC_E_Pre
FC1_Theta_EC_E_Pre <- df$FC1_Theta_EC_E_Pre
FCZ_Theta_EC_E_Pre <- df$FCZ_Theta_EC_E_Pre
FC2_Theta_EC_E_Pre <- df$FC2_Theta_EC_E_Pre
C1_Theta_EC_E_Pre <- df$C1_Theta_EC_E_Pre
CZ_Theta_EC_E_Pre <- df$CZ_Theta_EC_E_Pre
C2_Theta_EC_E_Pre <- df$C2_Theta_EC_E_Pre
F1_Theta_EC_E_Post <- df$F1_Theta_EC_E_Post
FZ_Theta_EC_E_Post <- df$FZ_Theta_EC_E_Post
F2_Theta_EC_E_Post <- df$F2_Theta_EC_E_Post
FC1_Theta_EC_E_Post <- df$FC1_Theta_EC_E_Post
FCZ_Theta_EC_E_Post <- df$FCZ_Theta_EC_E_Post
FC2_Theta_EC_E_Post <- df$FC2_Theta_EC_E_Post
C1_Theta_EC_E_Post <- df$C1_Theta_EC_E_Post
CZ_Theta_EC_E_Post <- df$CZ_Theta_EC_E_Post
C2_Theta_EC_E_Post <- df$C2_Theta_EC_E_Post
F1_Theta_EC_Rest_E_Pre <- df$F1_Theta_EC_Rest_E_Pre
FZ_Theta_EC_Rest_E_Pre <- df$FZ_Theta_EC_Rest_E_Pre
F2_Theta_EC_Rest_E_Pre <- df$F2_Theta_EC_Rest_E_Pre
FC1_Theta_EC_Rest_E_Pre <- df$FC1_Theta_EC_Rest_E_Pre
FCZ_Theta_EC_Rest_E_Pre <- df$FCZ_Theta_EC_Rest_E_Pre
FC2_Theta_EC_Rest_E_Pre <- df$FC2_Theta_EC_Rest_E_Pre
C1_Theta_EC_Rest_E_Pre <- df$C1_Theta_EC_Rest_E_Pre
CZ_Theta_EC_Rest_E_Pre <- df$CZ_Theta_EC_Rest_E_Pre
C2_Theta_EC_Rest_E_Pre <- df$C2_Theta_EC_Rest_E_Pre
F1_Theta_EC_Rest_E_Post <- df$F1_Theta_EC_Rest_E_Post
FZ_Theta_EC_Rest_E_Post <- df$FZ_Theta_EC_Rest_E_Post
F2_Theta_EC_Rest_E_Post <- df$F2_Theta_EC_Rest_E_Post
FC1_Theta_EC_Rest_E_Post <- df$FC1_Theta_EC_Rest_E_Post
FCZ_Theta_EC_Rest_E_Post <- df$FCZ_Theta_EC_Rest_E_Post
FC2_Theta_EC_Rest_E_Post <- df$FC2_Theta_EC_Rest_E_Post
C1_Theta_EC_Rest_E_Post <- df$C1_Theta_EC_Rest_E_Post
CZ_Theta_EC_Rest_E_Post <- df$CZ_Theta_EC_Rest_E_Post
C2_Theta_EC_Rest_E_Post <- df$C2_Theta_EC_Rest_E_Post

#M1 Resting Posterior Theta x Flanker RT in Exercise vs. Rest 
##Average together Resting Theta ROI site
Wide.ROI_FC_E_pre_Theta.df <-data.frame(F1_Theta_EC_E_Pre, FZ_Theta_EC_E_Pre, F2_Theta_EC_E_Pre, FC1_Theta_EC_E_Pre, FCZ_Theta_EC_E_Pre, FC2_Theta_EC_E_Pre, C1_Theta_EC_E_Pre, CZ_Theta_EC_E_Pre, C2_Theta_EC_E_Pre)
Wide.ROI_FC_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_E_pre_Theta.df)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_EC_E_Post, FZ_Theta_EC_E_Post, F2_Theta_EC_E_Post, FC1_Theta_EC_E_Post, FCZ_Theta_EC_E_Post, FC2_Theta_EC_E_Post, C1_Theta_EC_E_Post, CZ_Theta_EC_E_Post, C2_Theta_EC_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_pre_Theta.df <-data.frame(F1_Theta_EC_Rest_E_Pre, FZ_Theta_EC_Rest_E_Pre, F2_Theta_EC_Rest_E_Pre, FC1_Theta_EC_Rest_E_Pre, FCZ_Theta_EC_Rest_E_Pre, FC2_Theta_EC_Rest_E_Pre, C1_Theta_EC_Rest_E_Pre, CZ_Theta_EC_Rest_E_Pre, C2_Theta_EC_Rest_E_Pre)
Wide.ROI_FC_Rest_E_pre_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_pre_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_EC_Rest_E_Post, FZ_Theta_EC_Rest_E_Post, F2_Theta_EC_Rest_E_Post, FC1_Theta_EC_Rest_E_Post, FCZ_Theta_EC_Rest_E_Post, FC2_Theta_EC_Rest_E_Post, C1_Theta_EC_Rest_E_Post, CZ_Theta_EC_Rest_E_Post, C2_Theta_EC_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)

#create dataframe 
M1_Theta_EC_x_IE.df <- data.frame(SubjectID,Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
                                  SAya_FL_Congruent_InverseEfficiency_Rest_E_pre, SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre, SAya_FL_Congruent_InverseEfficiency_Rest_E_post, SAya_FL_Incongruent_InverseEfficiency_Rest_E_post, 
                                  SAya_FL_Congruent_InverseEfficiency_E_pre, SAya_FL_Incongruent_InverseEfficiency_E_pre, SAya_FL_Congruent_InverseEfficiency_E_post, SAya_FL_Incongruent_InverseEfficiency_E_post)
#melt
longformat.M1_Theta_EC_x_IE.df <- melt(M1_Theta_EC_x_IE.df, id = "SubjectID", variable.name = "Condition")
#Check for outliers 
longformat.M1_Theta_EC_x_IE.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#normality test 
M1_Theta_EC_x_IE.df %>%
  shapiro_test(Wide.ROI_FC_E_pre_Theta.mean, Wide.ROI_FC_Rest_E_pre_Theta.mean, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean,
               SAya_FL_Congruent_InverseEfficiency_Rest_E_pre, SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre, SAya_FL_Congruent_InverseEfficiency_Rest_E_post, SAya_FL_Incongruent_InverseEfficiency_Rest_E_post, 
               SAya_FL_Congruent_InverseEfficiency_E_pre, SAya_FL_Incongruent_InverseEfficiency_E_pre, SAya_FL_Congruent_InverseEfficiency_E_post, SAya_FL_Incongruent_InverseEfficiency_E_post)
#Computation - Pre_E_Congruent
summary(lm(formula = SAya_FL_Congruent_InverseEfficiency_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggscatterstats(data = M1_Theta_EC_x_IE.df, y = SAya_FL_Congruent_InverseEfficiency_E_pre, x = Wide.ROI_FC_E_pre_Theta.mean, type = "nonparametric")
ggplot(M1_Theta_EC_x_IE.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Congruent_InverseEfficiency_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_InverseEfficiency_E_pre~Wide.ROI_FC_E_pre_Theta.mean))
ggscatterstats(data = M1_Theta_EC_x_IE.df, y = SAya_FL_Incongruent_InverseEfficiency_E_pre, x = Wide.ROI_FC_E_pre_Theta.mean, type = "parametric")
ggplot(M1_Theta_EC_x_IE.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Incongruent_InverseEfficiency_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Congruent
summary(lm(formula = SAya_FL_Congruent_InverseEfficiency_E_pre~Wide.ROI_FC_E_post_Theta.mean))
ggscatterstats(data = M1_Theta_EC_x_IE.df, y = SAya_FL_Congruent_InverseEfficiency_E_post, x = Wide.ROI_FC_E_post_Theta.mean, type = "parametric")
ggplot(M1_Theta_EC_x_IE.df, aes(x=Wide.ROI_FC_E_pre_Theta.mean, y=SAya_FL_Congruent_InverseEfficiency_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_InverseEfficiency_E_post~Wide.ROI_FC_E_post_Theta.mean))
ggscatterstats(data = M1_Theta_EC_x_IE.df, y = SAya_FL_Incongruent_InverseEfficiency_E_post, x = Wide.ROI_FC_E_post_Theta.mean, type = "parametric")
ggplot(M1_Theta_EC_x_IE.df, aes(x=Wide.ROI_FC_E_post_Theta.mean, y=SAya_FL_Incongruent_InverseEfficiency_E_post))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_InverseEfficiency_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggscatterstats(data = M1_Theta_EC_x_IE.df, y = SAya_FL_Congruent_InverseEfficiency_Rest_E_pre, x = Wide.ROI_FC_Rest_E_pre_Theta.mean, type = "nonparametric")
ggplot(M1_Theta_EC_x_IE.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Congruent_InverseEfficiency_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Pre_Rest_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre~Wide.ROI_FC_Rest_E_pre_Theta.mean))
ggscatterstats(data = M1_Theta_EC_x_IE.df, y = SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre, x = Wide.ROI_FC_Rest_E_pre_Theta.mean, type = "nonparametric")
ggplot(M1_Theta_EC_x_IE.df, aes(x=Wide.ROI_FC_Rest_E_pre_Theta.mean, y=SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Congruent
summary(lm(formula = SAya_FL_Congruent_InverseEfficiency_Rest_E_post~Wide.ROI_FC_Rest_E_post_Theta.mean))
ggscatterstats(data = M1_Theta_EC_x_IE.df, y = SAya_FL_Congruent_InverseEfficiency_Rest_E_post, x = Wide.ROI_FC_Rest_E_post_Theta.mean, type = "parametric")
ggplot(M1_Theta_EC_x_IE.df, aes(x=Wide.ROI_FC_Rest_E_post_Theta.mean, y=SAya_FL_Congruent_InverseEfficiency_Rest_E_post))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))
#Computation - Post_Rest_E_Incongruent
summary(lm(formula = SAya_FL_Incongruent_InverseEfficiency_Rest_E_post~Wide.ROI_FC_Rest_E_post_Theta.mean))
ggscatterstats(data = M1_Theta_EC_x_IE.df, y = SAya_FL_Incongruent_InverseEfficiency_Rest_E_post, x = Wide.ROI_FC_Rest_E_post_Theta.mean, type = "parametric")
ggplot(M1_Theta_EC_x_IE.df, aes(x=Wide.ROI_FC_Rest_E_post_Theta.mean, y=SAya_FL_Incongruent_InverseEfficiency_Rest_E_post))+
  geom_point()+
  xlab("Theta Log Power")+
  ylab("Flanker InverseEfficiency")+
  ggtitle("Theta Power x Flanker InverseEfficiency")+
  geom_smooth(method='lm', formula= y~x)+
  theme(plot.title = element_text(hjust = 0.5))

# Exploratory Analyses - CONGITIVE OUTCOMES - Inverse Efficiency ----------------------------------------------------
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

#M1 All conditions computation 
IE_M1.df<- data.frame(SubjectID, 
                      SAya_FL_Congruent_InverseEfficiency_Rest_E_pre, SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre, 
                      SAya_FL_Congruent_InverseEfficiency_Rest_E_post, SAya_FL_Incongruent_InverseEfficiency_Rest_E_post, 
                      SAya_FL_Congruent_InverseEfficiency_E_pre, SAya_FL_Incongruent_InverseEfficiency_E_pre,
                      SAya_FL_Congruent_InverseEfficiency_E_post, SAya_FL_Incongruent_InverseEfficiency_E_post)
longformat.IE_M1.df <- melt(IE_M1.df, id = "SubjectID", variable.name = "Condition")
#check for outliers 
longformat.IE_M1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality
IE_M1.df %>%
  shapiro_test(SAya_FL_Congruent_InverseEfficiency_Rest_E_pre, SAya_FL_Incongruent_InverseEfficiency_Rest_E_pre, 
               SAya_FL_Congruent_InverseEfficiency_Rest_E_post, SAya_FL_Incongruent_InverseEfficiency_Rest_E_post, 
               SAya_FL_Congruent_InverseEfficiency_E_pre, SAya_FL_Incongruent_InverseEfficiency_E_pre,
               SAya_FL_Congruent_InverseEfficiency_E_post, SAya_FL_Incongruent_InverseEfficiency_E_post)
ggwithinstats(data = longformat.IE_M1.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni", ggplot.component = theme(axis.text.x = element_text(angle = 30)))

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

#M2 All conditions computation 
IE_M2.df<- data.frame(SubjectID, 
                      SAya_FL_Congruent_InverseEfficiency_Rest_T_pre, SAya_FL_Incongruent_InverseEfficiency_Rest_T_pre, 
                      SAya_FL_Congruent_InverseEfficiency_Rest_T_post, SAya_FL_Incongruent_InverseEfficiency_Rest_T_post, 
                      SAya_FL_Congruent_InverseEfficiency_T_pre, SAya_FL_Incongruent_InverseEfficiency_T_pre,
                      SAya_FL_Congruent_InverseEfficiency_T_post, SAya_FL_Incongruent_InverseEfficiency_T_post)
longformat.IE_M2.df <- melt(IE_M2.df, id = "SubjectID", variable.name = "Condition")
#check for outliers 
longformat.IE_M2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality
IE_M2.df %>%
  shapiro_test(SAya_FL_Congruent_InverseEfficiency_Rest_T_pre, SAya_FL_Incongruent_InverseEfficiency_Rest_T_pre, 
               SAya_FL_Congruent_InverseEfficiency_Rest_T_post, SAya_FL_Incongruent_InverseEfficiency_Rest_T_post, 
               SAya_FL_Congruent_InverseEfficiency_T_pre, SAya_FL_Incongruent_InverseEfficiency_T_pre,
               SAya_FL_Congruent_InverseEfficiency_T_post, SAya_FL_Incongruent_InverseEfficiency_T_post)
ggwithinstats(data = longformat.IE_M2.df, x = Condition, y = value, type = "nonparametric", p.adjust.method = "bonferroni", ggplot.component = theme(axis.text.x = element_text(angle = 30)))

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


# Exploratory Analyses - COGNITIVE OUTCOMES - EZ Diffusion Model -------------------------------

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
  # The function "qlogis” calculates the logit.
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



# Exploratory Analyses - COGNITIVE OUTCOMES - Flanker Response Time Interference Scores --------
#Call Variables
SubjectID <- (df$SubjectID)
SAya_FL_Congruent_MeanRT_E_pre <- (df$ SAya_FL_Congruent_MeanRT_E_pre)
SAya_FL_Congruent_MeanRT_Rest_E_pre <- (df$ SAya_FL_Congruent_MeanRT_Rest_E_pre)
SAya_FL_Incongruent_MeanRT_E_pre <- (df$ SAya_FL_Incongruent_MeanRT_E_pre)
SAya_FL_Incongruent_MeanRT_Rest_E_pre <- (df$ SAya_FL_Incongruent_MeanRT_Rest_E_pre)
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

#M1
#Generate Interference Scores (Incongruent - Congruent)
Interference_Rest_E_pre<-SAya_FL_Incongruent_MeanRT_Rest_E_pre-SAya_FL_Congruent_MeanRT_Rest_E_pre
Interference_E_pre<-SAya_FL_Incongruent_MeanRT_E_pre-SAya_FL_Congruent_MeanRT_E_pre
Interference_Rest_E_post<-SAya_FL_Incongruent_MeanRT_Rest_E_post-SAya_FL_Congruent_MeanRT_Rest_E_post
Interference_E_post<-SAya_FL_Incongruent_MeanRT_E_post-SAya_FL_Congruent_MeanRT_E_post

#M1 All conditions computation 
Interference_M1.df<- data.frame(SubjectID,Interference_Rest_E_pre, Interference_E_pre, Interference_Rest_E_post, Interference_E_post)
longformat.Interference_M1.df <- melt(Interference_M1.df, id = "SubjectID", variable.name = "Condition")
#check for outliers 
longformat.Interference_M1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality
Interference_M1.df %>%
  shapiro_test(Interference_Rest_E_pre, Interference_E_pre, Interference_Rest_E_post, Interference_E_post)
ggwithinstats(data = longformat.Interference_M1.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Generate difference scores of Interference Scores (post-pre)
DiffInterference_Rest_E<-Interference_Rest_E_post-Interference_Rest_E_pre
DiffInterference_E<-Interference_E_post-Interference_E_pre
#create dataframe
DiffInterference_Model1.df <- data.frame(SubjectID, DiffInterference_Rest_E, DiffInterference_E)
#Summarize
summary(DiffInterference_Model1.df)
#melt data frame
longformat.DiffInterference_Model1.df <- melt(DiffInterference_Model1.df, id = "SubjectID", variable.name = "Condition")
#Visualize
ggplot(longformat.DiffInterference_Model1.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Difference in Interference Scores Model 1 Exercise vs. Rest", x ="Intervention", y = "Interference Difference")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.DiffInterference_Model1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
DiffInterference_Model1.df %>%
  shapiro_test(DiffInterference_Rest_E, DiffInterference_E)
histogram(DiffInterference_Model1.df$DiffInterference_E)
#Passed normality test! Parametric stats used 
ggwithinstats(data = longformat.DiffInterference_Model1.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_hedges_g(0.02)


#M2
#Generate Interference Scores (Incongruent - Congruent)
Interference_Rest_T_pre<-SAya_FL_Incongruent_MeanRT_Rest_T_pre-SAya_FL_Congruent_MeanRT_Rest_T_pre
Interference_T_pre<-SAya_FL_Incongruent_MeanRT_T_pre-SAya_FL_Congruent_MeanRT_T_pre
Interference_Rest_T_post<-SAya_FL_Incongruent_MeanRT_Rest_T_post-SAya_FL_Congruent_MeanRT_Rest_T_post
Interference_T_post<-SAya_FL_Incongruent_MeanRT_T_post-SAya_FL_Congruent_MeanRT_T_post

#M2 All conditions computation 
Interference_M2.df<- data.frame(SubjectID,Interference_Rest_T_pre, Interference_T_pre, Interference_Rest_T_post, Interference_T_post)
longformat.Interference_M2.df <- melt(Interference_M2.df, id = "SubjectID", variable.name = "Condition")
#check for outliers 
longformat.Interference_M2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality
Interference_M2.df %>%
  shapiro_test(Interference_Rest_T_pre, Interference_T_pre, Interference_Rest_T_post, Interference_T_post)
ggwithinstats(data = longformat.Interference_M2.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Generate difference scores of Interference Scores (post-pre)
DiffInterference_Rest_T<-Interference_Rest_T_post-Interference_Rest_T_pre
DiffInterference_T<-Interference_T_post-Interference_T_pre
#create dataframe
DiffInterference_Model2.df <- data.frame(SubjectID, DiffInterference_Rest_T, DiffInterference_T)
#Summarize
summary(DiffInterference_Model2.df)
#melt data frame
longformat.DiffInterference_Model2.df <- melt(DiffInterference_Model2.df, id = "SubjectID", variable.name = "Condition")
#Visualize
ggplot(longformat.DiffInterference_Model2.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Difference in Interference Scores Model 2 Trier vs. Rest", x ="Intervention", y = "Interference Difference")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.DiffInterference_Model2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
DiffInterference_Model2.df %>%
  shapiro_test(DiffInterference_Rest_T, DiffInterference_T)
histogram(DiffInterference_Model2.df$DiffInterference_T)
#Passed normality test! Parametric stats used 
ggwithinstats(data = longformat.DiffInterference_Model2.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_hedges_g(0.30)

# Exploratory Analyses - COGNITIVE OUTCOMES - Flanker Accuracy Interference Scores --------
#Call Variables
SubjectID <- (df$SubjectID)
SAya_FL_Congruent_ResponseAccuracy_E_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_pre)
SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre)
SAya_FL_Incongruent_ResponseAccuracy_E_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_pre)
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre)
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

#M1
#Generate Interference Scores (Incongruent - Congruent)
Interference_Rest_E_pre<-SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre-SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre
Interference_E_pre<-SAya_FL_Incongruent_ResponseAccuracy_E_pre-SAya_FL_Congruent_ResponseAccuracy_E_pre
Interference_Rest_E_post<-SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post-SAya_FL_Congruent_ResponseAccuracy_Rest_E_post
Interference_E_post<-SAya_FL_Incongruent_ResponseAccuracy_E_post-SAya_FL_Congruent_ResponseAccuracy_E_post
#Generate difference scores of Interference Scores (post-pre)
DiffInterference_Rest_E<-Interference_Rest_E_post-Interference_Rest_E_pre
DiffInterference_E<-Interference_E_post-Interference_E_pre
#create dataframe
DiffInterference_Model1.df <- data.frame(SubjectID, DiffInterference_Rest_E, DiffInterference_E)
#Summarize
summary(DiffInterference_Model1.df)
#melt data frame
longformat.DiffInterference_Model1.df <- melt(DiffInterference_Model1.df, id = "SubjectID", variable.name = "Condition")
#Visualize
ggplot(longformat.DiffInterference_Model1.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Difference in Interference Scores Model 1 Exercise vs. Rest", x ="Intervention", y = "Interference Difference")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.DiffInterference_Model1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
DiffInterference_Model1.df %>%
  shapiro_test(DiffInterference_Rest_E, DiffInterference_E)
histogram(DiffInterference_Model1.df$DiffInterference_E)
#Passed normality test! Parametric stats used 
ggwithinstats(data = longformat.DiffInterference_Model1.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_hedges_g(0.11)


#M2
#Generate Interference Scores (Incongruent - Congruent)
Interference_Rest_T_pre<-SAya_FL_Incongruent_ResponseAccuracy_Rest_T_pre-SAya_FL_Congruent_ResponseAccuracy_Rest_T_pre
Interference_T_pre<-SAya_FL_Incongruent_ResponseAccuracy_T_pre-SAya_FL_Congruent_ResponseAccuracy_T_pre
Interference_Rest_T_post<-SAya_FL_Incongruent_ResponseAccuracy_Rest_T_post-SAya_FL_Congruent_ResponseAccuracy_Rest_T_post
Interference_T_post<-SAya_FL_Incongruent_ResponseAccuracy_T_post-SAya_FL_Congruent_ResponseAccuracy_T_post
#Generate difference scores of Interference Scores (post-pre)
DiffInterference_Rest_T<-Interference_Rest_T_post-Interference_Rest_T_pre
DiffInterference_T<-Interference_T_post-Interference_T_pre
#create dataframe
DiffInterference_Model2.df <- data.frame(SubjectID, DiffInterference_Rest_T, DiffInterference_T)
#Summarize
summary(DiffInterference_Model2.df)
#melt data frame
longformat.DiffInterference_Model2.df <- melt(DiffInterference_Model2.df, id = "SubjectID", variable.name = "Condition")
#Visualize
ggplot(longformat.DiffInterference_Model2.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Difference in Interference Scores Model 2 Trier vs. Rest", x ="Intervention", y = "Interference Difference")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.DiffInterference_Model2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
DiffInterference_Model2.df %>%
  shapiro_test(DiffInterference_Rest_T, DiffInterference_T)
histogram(DiffInterference_Model2.df$DiffInterference_T)
#Passed normality test! Parametric stats used 
ggwithinstats(data = longformat.DiffInterference_Model2.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_hedges_g(0.30)



# Exploratory Analyses - COGNITIVE OUTCOMES - Flanker Response Time Standard Deviation --------
#Call variables 
SubejctID<-df$SubjectID
SAya_FL_Congruent_SD_RT_B_pre<-df$SAya_FL_Congruent_SD_RT_B_pre
SAya_FL_Congruent_SD_RT_E_pre<-df$SAya_FL_Congruent_SD_RT_E_pre
SAya_FL_Congruent_SD_RT_E_post<-df$SAya_FL_Congruent_SD_RT_E_post
SAya_FL_Congruent_SD_RT_Rest_E_pre<-df$SAya_FL_Congruent_SD_RT_Rest_E_pre
SAya_FL_Congruent_SD_RT_Rest_E_post<-df$SAya_FL_Congruent_SD_RT_Rest_E_post
SAya_FL_Congruent_SD_RT_T_pre<-df$SAya_FL_Congruent_SD_RT_T_pre
SAya_FL_Congruent_SD_RT_T_post<-df$SAya_FL_Congruent_SD_RT_T_post
SAya_FL_Congruent_SD_RT_Rest_T_pre<-df$SAya_FL_Congruent_SD_RT_Rest_T_pre
SAya_FL_Congruent_SD_RT_Rest_T_post<-df$SAya_FL_Congruent_SD_RT_Rest_T_post
SAya_FL_Incongruent_SD_RT_B_pre<-df$SAya_FL_Incongruent_SD_RT_B_pre
SAya_FL_Incongruent_SD_RT_E_pre<-df$SAya_FL_Incongruent_SD_RT_E_pre
SAya_FL_Incongruent_SD_RT_E_post<-df$SAya_FL_Incongruent_SD_RT_E_post
SAya_FL_Incongruent_SD_RT_Rest_E_pre<-df$SAya_FL_Incongruent_SD_RT_Rest_E_pre
SAya_FL_Incongruent_SD_RT_Rest_E_post<-df$SAya_FL_Incongruent_SD_RT_Rest_E_post
SAya_FL_Incongruent_SD_RT_T_pre<-df$SAya_FL_Incongruent_SD_RT_T_pre
SAya_FL_Incongruent_SD_RT_T_post<-df$SAya_FL_Incongruent_SD_RT_T_post
SAya_FL_Incongruent_SD_RT_Rest_T_pre<-df$SAya_FL_Incongruent_SD_RT_Rest_T_pre
SAya_FL_Incongruent_SD_RT_Rest_T_post<-df$SAya_FL_Incongruent_SD_RT_Rest_T_post

#M1
#Visualize all conditions
RTSD_Model1.df <- data.frame(SubjectID, SAya_FL_Congruent_SD_RT_B_pre, SAya_FL_Congruent_SD_RT_E_pre, SAya_FL_Congruent_SD_RT_E_post, SAya_FL_Congruent_SD_RT_Rest_E_pre, SAya_FL_Congruent_SD_RT_Rest_E_post, 
                           SAya_FL_Incongruent_SD_RT_B_pre, SAya_FL_Incongruent_SD_RT_E_pre,SAya_FL_Incongruent_SD_RT_E_post, SAya_FL_Incongruent_SD_RT_Rest_E_pre, SAya_FL_Incongruent_SD_RT_Rest_E_post)
longformat.RTSD_Model1.df <- melt(RTSD_Model1.df, id = "SubjectID", variable.name = "Condition")
ggplot(longformat.RTSD_Model1.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "jitter", alpha = 0.5)+
  labs(title="Response Time SD for Model 1 Exercise vs. Rest", x ="Intervention", y = "SD Response Time")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#Exploratory all conditions stats
#ggwithinstats(data = longformat.RTSD_Model1.df, x = Condition, y = value, type = "nonparametric", p.adjust.method = "bonferroni")

#M1 Difference in RT SD Exercise vs. Difference in Rest
Diff_R_Congruent_RTSD <- SAya_FL_Congruent_SD_RT_Rest_E_post-SAya_FL_Congruent_SD_RT_Rest_E_pre
Diff_R_Incongruent_RTSD <-SAya_FL_Incongruent_SD_RT_Rest_E_post-SAya_FL_Incongruent_SD_RT_Rest_E_pre
Diff_E_Congruent_RTSD <- SAya_FL_Congruent_SD_RT_E_post-SAya_FL_Congruent_SD_RT_E_pre
Diff_E_Incongruent_RTSD <- SAya_FL_Incongruent_SD_RT_E_post-SAya_FL_Incongruent_SD_RT_E_pre
Diff_RTSD_Model1.df <- data.frame(SubjectID, Diff_R_Congruent_RTSD, Diff_R_Incongruent_RTSD, Diff_E_Congruent_RTSD, Diff_E_Incongruent_RTSD)
#Summarize
summary(Diff_RTSD_Model1.df)
#Melt dataframe
longformat.Diff_RTSD_Model1.df <- melt(Diff_RTSD_Model1.df, id = "SubjectID", variable.name = "Condition")
View(longformat.Diff_RTSD_Model1.df)
#Visualize
ggplot(longformat.Diff_RTSD_Model1.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Response Time SD Difference Post-Pre for Model 1 Exercise vs. Rest", x ="Intervention", y = "SD Difference (ms)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.Diff_RTSD_Model1.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
Diff_RTSD_Model1.df %>%
  shapiro_test(Diff_R_Congruent_RTSD, Diff_R_Incongruent_RTSD, Diff_E_Congruent_RTSD, Diff_E_Incongruent_RTSD)
histogram(Diff_RTSD_Model1.df$Diff_R_Congruent_RTSD)
#Passed normality test! Parametric stats used 
ggwithinstats(data = longformat.Diff_RTSD_Model1.df, x = Condition, y = value, type = "nonparametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_kendalls_w(0.46)

#M2
#Visualize all conditions
RTSD_Model2.df <- data.frame(SubjectID, SAya_FL_Congruent_SD_RT_B_pre, SAya_FL_Congruent_SD_RT_T_pre, SAya_FL_Congruent_SD_RT_T_post, SAya_FL_Congruent_SD_RT_Rest_T_pre, SAya_FL_Congruent_SD_RT_Rest_T_post, 
                             SAya_FL_Incongruent_SD_RT_B_pre, SAya_FL_Incongruent_SD_RT_T_pre,SAya_FL_Incongruent_SD_RT_T_post, SAya_FL_Incongruent_SD_RT_Rest_T_pre, SAya_FL_Incongruent_SD_RT_Rest_T_post)
longformat.RTSD_Model2.df <- melt(RTSD_Model2.df, id = "SubjectID", variable.name = "Condition")
ggplot(longformat.RTSD_Model2.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "jitter", alpha = 0.5)+
  labs(title="Response Time SD for Model 1 Trier vs. Rest", x ="Intervention", y = "SD Response Time")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#Exploratory all conditions stats
ggwithinstats(data = longformat.RTSD_Model2.df, x = Condition, y = value, type = "nonparametric", p.adjust.method = "bonferroni")
#M2 Difference in RT SD Trier vs. Difference in Rest
Diff_R_Congruent_RTSD <- SAya_FL_Congruent_SD_RT_Rest_T_post-SAya_FL_Congruent_SD_RT_Rest_T_pre
Diff_R_Incongruent_RTSD <-SAya_FL_Incongruent_SD_RT_Rest_T_post-SAya_FL_Incongruent_SD_RT_Rest_T_pre
Diff_T_Congruent_RTSD <- SAya_FL_Congruent_SD_RT_T_post-SAya_FL_Congruent_SD_RT_T_pre
Diff_T_Incongruent_RTSD <- SAya_FL_Incongruent_SD_RT_T_post-SAya_FL_Incongruent_SD_RT_T_pre
Diff_RTSD_Model2.df <- data.frame(SubjectID, Diff_R_Congruent_RTSD, Diff_R_Incongruent_RTSD, Diff_T_Congruent_RTSD, Diff_T_Incongruent_RTSD)
#Summarize
summary(Diff_RTSD_Model2.df)
#Melt dataframe
longformat.Diff_RTSD_Model2.df <- melt(Diff_RTSD_Model2.df, id = "SubjectID", variable.name = "Condition")
View(longformat.Diff_RTSD_Model2.df)
#Visualize
ggplot(longformat.Diff_RTSD_Model2.df, aes( x=Condition, y = value, fill=Condition)) + 
  geom_bar(position="dodge", stat="summary")+
  geom_point(position = "dodge", alpha = 0.5)+
  labs(title="Response Time SD Difference Post-Pre for Model 2 Trier vs. Rest", x ="Intervention", y = "SD Difference (ms)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
#check for outliers
longformat.Diff_RTSD_Model2.df %>%
  group_by(Condition) %>%
  identify_outliers(value)
#check for normality using wide format
Diff_RTSD_Model2.df %>%
  shapiro_test(Diff_R_Congruent_RTSD, Diff_R_Incongruent_RTSD, Diff_T_Congruent_RTSD, Diff_T_Incongruent_RTSD)
histogram(Diff_RTSD_Model2.df$Diff_R_Congruent_RTSD)
#Passed normality test! Parametric stats used 
ggwithinstats(data = longformat.Diff_RTSD_Model2.df, x = Condition, y = value, type = "nonparametric", p.adjust.method = "bonferroni")
#Take effect size and see how big it is!
interpret_kendalls_w(0.46)


# Exploratory Analyses - COGNITIVE & FITNESS OUTCOME - Post-E Flanker Performance and VO2 --------
#Post Exercise congruent Flanker RT related to fitness?
summary(lm(SAya_FL_Congruent_MeanRT_E_post~VO2 + Sex))

#Post Exercise incongruent Flanker RT related to fitness?
summary(lm(SAya_FL_Incongruent_MeanRT_E_post~VO2+ Sex))

#Post Exercise congruent flanker ACC related to fitness? 
summary(lm(SAya_FL_Congruent_ResponseAccuracy_E_post~VO2+ Sex))
ggplot(df, aes(x = VO2, y = SAya_FL_Congruent_ResponseAccuracy_E_post))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)
#Post Exercise incongruent flanker ACC related to fitness? 
summary(lm(SAya_FL_Incongruent_ResponseAccuracy_E_post~VO2+ Sex))

#Post Exercise congruent Flanker IE related to fitness?
summary(lm(SAya_FL_Congruent_InverseEfficiency_E_post~VO2+ Sex))

#Post Exercise incongruent Flanker IE related to fitness?
summary(lm(SAya_FL_Incongruent_InverseEfficiency_E_post~VO2+ Sex))


# Exploratory Analyses - COGNITIVE & FITNESS OUTCOME - Flanker Response Time SD and VO2 --------
#Call Variables 
SubjectID <- (df$SubjectID)
SAya_FL_Congruent_SD_RT_B_pre<-df$SAya_FL_Congruent_SD_RT_B_pre
SAya_FL_Congruent_SD_RT_E_pre<-df$SAya_FL_Congruent_SD_RT_E_pre
SAya_FL_Congruent_SD_RT_E_post<-df$SAya_FL_Congruent_SD_RT_E_post
SAya_FL_Congruent_SD_RT_Rest_E_pre<-df$SAya_FL_Congruent_SD_RT_Rest_E_pre
SAya_FL_Congruent_SD_RT_Rest_E_post<-df$SAya_FL_Congruent_SD_RT_Rest_E_post
SAya_FL_Incongruent_SD_RT_B_pre<-df$SAya_FL_Incongruent_SD_RT_B_pre
SAya_FL_Incongruent_SD_RT_E_pre<-df$SAya_FL_Incongruent_SD_RT_E_pre
SAya_FL_Incongruent_SD_RT_E_post<-df$SAya_FL_Incongruent_SD_RT_E_post
SAya_FL_Incongruent_SD_RT_Rest_E_pre<-df$SAya_FL_Incongruent_SD_RT_Rest_E_pre
SAya_FL_Incongruent_SD_RT_Rest_E_post<-df$SAya_FL_Incongruent_SD_RT_Rest_E_post
VO2<-df$b_vo2_value
Sex<-df$b_vo2_sex

#Baseline congruent Flanker RT related to fitness?
summary(lm(SAya_FL_Congruent_SD_RT_B_pre~VO2 + Sex))

#Baseline incongruent Flanker RT related to fitness?
summary(lm(SAya_FL_Incongruent_SD_RT_B_pre~VO2+ Sex))

#Post Exercise congruent Flanker RT related to fitness?
summary(lm(SAya_FL_Congruent_SD_RT_E_post~VO2 + Sex))

#Post Exercise incongruent Flanker RT related to fitness?
summary(lm(SAya_FL_Incongruent_SD_RT_E_post~VO2+ Sex))

# Exploratory Analyses - MEDIATION - COGNITIVE OUTCOME - P3 Amplitude --> P3 Accuracy --------
#May need to run P3 Amplitude and Flanker Accuracy call variable sections prior to running
#Call Variables 
SubjectID<-df$SubjectID
b_rand_string<-df$b_rand_string
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Amplitude, FC_Rest_E_po_P3_Con_CP1_Amplitude, FC_Rest_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.df <-data.frame(FC_E_po_P3_Con_CPZ_Amplitude, FC_E_po_P3_Con_CP1_Amplitude, FC_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Con_Amplitude.df)
SAya_FL_Congruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_post)
SAya_FL_Congruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_post)
#M1 Mediation - X = intervention group, M = post-intervention congruent P3 Amplitude, Y = post-intervention congruent P3 Accuracy
#create dataframe 
postP3Amp_postP3ACC_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string,Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean, SAya_FL_Congruent_ResponseAccuracy_Rest_E_post, SAya_FL_Congruent_ResponseAccuracy_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(postP3Amp_postP3ACC_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postP3Amp_postP3ACC_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postP3Amp_postP3ACC_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/postP3Amp_postP3ACC_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postP3Amp_postP3ACC_Model1_Mediation.df)
#mediation computation 
model.M.postP3Amp_postP3ACC_Model1_Mediation<- lm(PostP3Amp~Intervention, longformat.postP3Amp_postP3ACC_Model1_Mediation.df)
model.Y.postP3Amp_postP3ACC_Model1_Mediation<- lm(PostP3ACC~Intervention + PostP3Amp, longformat.postP3Amp_postP3ACC_Model1_Mediation.df)
set.seed(123)
results.postP3Amp_postP3ACC_Model1_Mediation<- mediate(model.M.postP3Amp_postP3ACC_Model1_Mediation, model.Y.postP3Amp_postP3ACC_Model1_Mediation, treat = 'Intervention', mediator = 'PostP3Amp', boot = TRUE, sims = 1000)
summary(results.postP3Amp_postP3ACC_Model1_Mediation)
#exploratory associations
summary(lm(SAya_FL_Congruent_ResponseAccuracy_Rest_E_post~Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean))
summary(lm(SAya_FL_Congruent_ResponseAccuracy_E_post~Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean))

#M1 Mediation - X = intervention group, M = post-intervention incongruent P3 Amplitude, Y = post-intervention incongruent Accuracy
#May need to run P3 Amplitude and Flanker Accuracy call variable sections prior to running
#Call Variables 
SubjectID<-df$SubjectID
b_rand_string<-df$b_rand_string
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Inc_CPZ_Amplitude, FC_Rest_E_po_P3_Inc_CP1_Amplitude, FC_Rest_E_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df)
Narrow.ROI_FC_E_po_P3_Inc_Amplitude.df <-data.frame(FC_E_po_P3_Inc_CPZ_Amplitude, FC_E_po_P3_Inc_CP1_Amplitude, FC_E_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_E_po_P3_Inc_Amplitude.df)
SAya_FL_Incongruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_post)
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
#M1 Mediation - X = intervention group, M = post-intervention Incongruent P3 Amplitude, Y = post-intervention Incongruent Accuracy
#create dataframe 
postP3Amp_postP3ACC_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string,Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean, Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post, SAya_FL_Incongruent_ResponseAccuracy_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(postP3Amp_postP3ACC_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postP3Amp_postP3ACC_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.postP3Amp_postP3ACC_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/postP3Amp_postP3ACC_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postP3Amp_postP3ACC_Model1_Mediation.df)
#mediation computation 
model.M.postP3Amp_postP3ACC_Model1_Mediation<- lm(PostP3Amp~Intervention, longformat.postP3Amp_postP3ACC_Model1_Mediation.df)
model.Y.postP3Amp_postP3ACC_Model1_Mediation<- lm(PostP3ACC~Intervention + PostP3Amp, longformat.postP3Amp_postP3ACC_Model1_Mediation.df)
set.seed(123)
results.postP3Amp_postP3ACC_Model1_Mediation<- mediate(model.M.postP3Amp_postP3ACC_Model1_Mediation, model.Y.postP3Amp_postP3ACC_Model1_Mediation, treat = 'Intervention', mediator = 'PostP3Amp', boot = TRUE, sims = 1000)
summary(results.postP3Amp_postP3ACC_Model1_Mediation)
#exploratory associations
summary(lm(SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post~Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean))
summary(lm(SAya_FL_Incongruent_ResponseAccuracy_E_post~Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean))

#M1 Mediation - X = intervention group, M = change in Congruent P3 Amplitude, Y = change Congruent Accuracy
#Call variables
SubjectID<-df$SubjectID
b_rand_string<-df$b_rand_string
Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_pre_P3_Con_CPZ_Amplitude, FC_Rest_E_pre_P3_Con_CP1_Amplitude, FC_Rest_E_pre_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.df)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Amplitude, FC_Rest_E_po_P3_Con_CP1_Amplitude, FC_Rest_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df)
SAya_FL_Congruent_ResponseAccuracy_E_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_pre)
SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre)
SAya_FL_Congruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_post)
SAya_FL_Congruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_post)
#Create change scores 
Diff_Rest_E_P3_Con_Amplitude<- Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean- Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean
Diff_E_P3_Con_Amplitude<- Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean- Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean
Diff_Rest_E_Accuracy<- SAya_FL_Congruent_ResponseAccuracy_Rest_E_post- SAya_FL_Congruent_ResponseAccuracy_Rest_E_pre
Diff_E_Accuracy<- SAya_FL_Congruent_ResponseAccuracy_E_post-SAya_FL_Congruent_ResponseAccuracy_E_pre
#Create dataframe
changeP3Amp_changeACC_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Diff_Rest_E_P3_Con_Amplitude, Diff_E_P3_Con_Amplitude, Diff_Rest_E_Accuracy, Diff_E_Accuracy)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeP3Amp_changeACC_Congruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeP3Amp_changeACC_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeP3Amp_changeACC_Congruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/changeP3Amp_changeACC_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeP3Amp_changeACC_Congruent_Model1_Mediation.df)
#mediation computation 
model.M.changeP3Amp_changeACC_Congruent_Model1_Mediation<- lm(ChangeP3Amp~Intervention, longformat.changeP3Amp_changeACC_Congruent_Model1_Mediation.df)
model.Y.changeP3Amp_changeACC_Congruent_Model1_Mediation<- lm(ChangeP3ACC~Intervention + ChangeP3Amp, longformat.changeP3Amp_changeACC_Congruent_Model1_Mediation.df)
set.seed(123)
results.changeP3Amp_changeACC_Congruent_Model1_Mediation<- mediate(model.M.changeP3Amp_changeACC_Congruent_Model1_Mediation, model.Y.changeP3Amp_changeACC_Congruent_Model1_Mediation, treat = 'Intervention', mediator = 'ChangeP3Amp', boot = TRUE, sims = 1000)
summary(results.changeP3Amp_changeACC_Congruent_Model1_Mediation)
#exploratory associations
summary(lm(Diff_Rest_E_Accuracy~Diff_Rest_E_P3_Con_Amplitude))
summary(lm(Diff_E_Accuracy~Diff_E_P3_Con_Amplitude))

#M1 Mediation - X = intervention group, M = change in Incongruent P3 Amplitude, Y = change Incongruent Accuracy
#Call variables
SubjectID<-df$SubjectID
b_rand_string<-df$b_rand_string
Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.df <-data.frame(FC_Rest_E_pre_P3_Inc_CPZ_Amplitude, FC_Rest_E_pre_P3_Inc_CP1_Amplitude, FC_Rest_E_pre_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.df)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Inc_CPZ_Amplitude, FC_Rest_E_po_P3_Inc_CP1_Amplitude, FC_Rest_E_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df)
SAya_FL_Incongruent_ResponseAccuracy_E_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_pre)
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre)
SAya_FL_Incongruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_post)
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
#Create change scores 
Diff_Rest_E_P3_Inc_Amplitude<- Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean- Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean
Diff_E_P3_Inc_Amplitude<- Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean- Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean
Diff_Rest_E_Accuracy<- SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post- SAya_FL_Incongruent_ResponseAccuracy_Rest_E_pre
Diff_E_Accuracy<- SAya_FL_Incongruent_ResponseAccuracy_E_post-SAya_FL_Incongruent_ResponseAccuracy_E_pre
#Create dataframe
changeP3Amp_changeACC_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Diff_Rest_E_P3_Inc_Amplitude, Diff_E_P3_Inc_Amplitude, Diff_Rest_E_Accuracy, Diff_E_Accuracy)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeP3Amp_changeACC_Incongruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeP3Amp_changeACC_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeP3Amp_changeACC_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/changeP3Amp_changeACC_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeP3Amp_changeACC_Incongruent_Model1_Mediation.df)
#mediation computation 
model.M.changeP3Amp_changeACC_Incongruent_Model1_Mediation<- lm(ChangeP3Amp~Intervention, longformat.changeP3Amp_changeACC_Incongruent_Model1_Mediation.df)
model.Y.changeP3Amp_changeACC_Incongruent_Model1_Mediation<- lm(ChangeP3ACC~Intervention + ChangeP3Amp, longformat.changeP3Amp_changeACC_Incongruent_Model1_Mediation.df)
set.seed(123)
results.changeP3Amp_changeACC_Incongruent_Model1_Mediation<- mediate(model.M.changeP3Amp_changeACC_Incongruent_Model1_Mediation, model.Y.changeP3Amp_changeACC_Incongruent_Model1_Mediation, treat = 'Intervention', mediator = 'ChangeP3Amp', boot = TRUE, sims = 1000)
summary(results.changeP3Amp_changeACC_Incongruent_Model1_Mediation)
#exploratory associations
summary(lm(Diff_Rest_E_Accuracy~Diff_Rest_E_P3_Inc_Amplitude))
summary(lm(Diff_E_Accuracy~Diff_E_P3_Inc_Amplitude))

#M1 Mediation - X = intervention group, M = change in Congruent P3 Amplitude, Y = post Congruent Accuracy
#Call variables
SubjectID<-df$SubjectID
b_rand_string<-df$b_rand_string
Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_pre_P3_Con_CPZ_Amplitude, FC_Rest_E_pre_P3_Con_CP1_Amplitude, FC_Rest_E_pre_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.df)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Con_CPZ_Amplitude, FC_Rest_E_po_P3_Con_CP1_Amplitude, FC_Rest_E_po_P3_Con_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.df)
SAya_FL_Congruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_E_post)
SAya_FL_Congruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Congruent_ResponseAccuracy_Rest_E_post)
#Create change scores 
Diff_Rest_E_P3_Con_Amplitude<- Narrow.ROI_FC_Rest_E_po_P3_Con_Amplitude.mean- Narrow.ROI_FC_Rest_E_pre_P3_Con_Amplitude.mean
Diff_E_P3_Con_Amplitude<- Narrow.ROI_FC_E_po_P3_Con_Amplitude.mean- Narrow.ROI_FC_E_pre_P3_Con_Amplitude.mean
#Create dataframe
changeP3Amp_postACC_Congruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Diff_Rest_E_P3_Con_Amplitude, Diff_E_P3_Con_Amplitude, SAya_FL_Congruent_ResponseAccuracy_E_post, SAya_FL_Congruent_ResponseAccuracy_Rest_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeP3Amp_postACC_Congruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeP3Amp_postACC_Congruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeP3Amp_postACC_Congruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/changeP3Amp_postACC_Congruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeP3Amp_postACC_Congruent_Model1_Mediation.df)
#mediation computation 
model.M.changeP3Amp_postACC_Congruent_Model1_Mediation<- lm(ChangeP3Amp~Intervention, longformat.changeP3Amp_postACC_Congruent_Model1_Mediation.df)
model.Y.changeP3Amp_postACC_Congruent_Model1_Mediation<- lm(PostP3ACC~Intervention + ChangeP3Amp, longformat.changeP3Amp_postACC_Congruent_Model1_Mediation.df)
set.seed(123)
results.changeP3Amp_postACC_Congruent_Model1_Mediation<- mediate(model.M.changeP3Amp_postACC_Congruent_Model1_Mediation, model.Y.changeP3Amp_postACC_Congruent_Model1_Mediation, treat = 'Intervention', mediator = 'ChangeP3Amp', boot = TRUE, sims = 1000)
summary(results.changeP3Amp_postACC_Congruent_Model1_Mediation)
#exploratory associations
summary(lm(SAya_FL_Congruent_ResponseAccuracy_Rest_E_post~Diff_Rest_E_P3_Con_Amplitude))
summary(lm(SAya_FL_Congruent_ResponseAccuracy_E_post~Diff_E_P3_Con_Amplitude))

#M1 Mediation - X = intervention group, M = change in Incongruent P3 Amplitude, Y = post Incongruent Accuracy
#Call variables
SubjectID<-df$SubjectID
b_rand_string<-df$b_rand_string
Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.df <-data.frame(FC_Rest_E_pre_P3_Inc_CPZ_Amplitude, FC_Rest_E_pre_P3_Inc_CP1_Amplitude, FC_Rest_E_pre_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.df)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df <-data.frame(FC_Rest_E_po_P3_Inc_CPZ_Amplitude, FC_Rest_E_po_P3_Inc_CP1_Amplitude, FC_Rest_E_po_P3_Inc_CP2_Amplitude)
Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean <- rowMeans(Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.df)
SAya_FL_Incongruent_ResponseAccuracy_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_E_post)
SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post <- (df$ SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
#Create change scores 
Diff_Rest_E_P3_Inc_Amplitude<- Narrow.ROI_FC_Rest_E_po_P3_Inc_Amplitude.mean- Narrow.ROI_FC_Rest_E_pre_P3_Inc_Amplitude.mean
Diff_E_P3_Inc_Amplitude<- Narrow.ROI_FC_E_po_P3_Inc_Amplitude.mean- Narrow.ROI_FC_E_pre_P3_Inc_Amplitude.mean
#Create dataframe
changeP3Amp_postACC_Incongruent_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, Diff_Rest_E_P3_Inc_Amplitude, Diff_E_P3_Inc_Amplitude, SAya_FL_Incongruent_ResponseAccuracy_E_post, SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post)
#export for manual manipulation to prepare for mediation 
write_xlsx(changeP3Amp_postACC_Incongruent_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//changeP3Amp_postACC_Incongruent_Model1_Mediation.df.xlsx') #export long format df
longformat.changeP3Amp_postACC_Incongruent_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data/changeP3Amp_postACC_Incongruent_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.changeP3Amp_postACC_Incongruent_Model1_Mediation.df)
#mediation computation 
model.M.changeP3Amp_postACC_Incongruent_Model1_Mediation<- lm(ChangeP3Amp~Intervention, longformat.changeP3Amp_postACC_Incongruent_Model1_Mediation.df)
model.Y.changeP3Amp_postACC_Incongruent_Model1_Mediation<- lm(PostP3ACC~Intervention + ChangeP3Amp, longformat.changeP3Amp_postACC_Incongruent_Model1_Mediation.df)
set.seed(123)
results.changeP3Amp_postACC_Incongruent_Model1_Mediation<- mediate(model.M.changeP3Amp_postACC_Incongruent_Model1_Mediation, model.Y.changeP3Amp_postACC_Incongruent_Model1_Mediation, treat = 'Intervention', mediator = 'ChangeP3Amp', boot = TRUE, sims = 1000)
summary(results.changeP3Amp_postACC_Incongruent_Model1_Mediation)
#exploratory associations
summary(lm(SAya_FL_Incongruent_ResponseAccuracy_Rest_E_post~Diff_Rest_E_P3_Inc_Amplitude))
summary(lm(SAya_FL_Incongruent_ResponseAccuracy_E_post~Diff_E_P3_Inc_Amplitude))

# Exploratory Analyses - DATA QUALITY - P3 @ each electrode site in ROI --------
#Amplitude M1
#Call Variables at Rest_E Pre Congruent
SubjectID<-df$SubjectID
FC_Rest_E_pre_P3_Con_CPZ_Amplitude<- df$FC_Rest_E_pre_P3_Con_CPZ_Amplitude
FC_Rest_E_pre_P3_Con_CP1_Amplitude<- df$FC_Rest_E_pre_P3_Con_CP1_Amplitude
FC_Rest_E_pre_P3_Con_CP2_Amplitude<- df$FC_Rest_E_pre_P3_Con_CP2_Amplitude
FC_Rest_E_pre_P3_Con_P1_Amplitude<- df$FC_Rest_E_pre_P3_Con_P1_Amplitude
FC_Rest_E_pre_P3_Con_PZ_Amplitude<- df$FC_Rest_E_pre_P3_Con_PZ_Amplitude
FC_Rest_E_pre_P3_Con_P2_Amplitude<- df$FC_Rest_E_pre_P3_Con_P2_Amplitude
FC_Rest_E_pre_P3_Con_C1_Amplitude<- df$FC_Rest_E_pre_P3_Con_C1_Amplitude
FC_Rest_E_pre_P3_Con_CZ_Amplitude<- df$FC_Rest_E_pre_P3_Con_CZ_Amplitude
FC_Rest_E_pre_P3_Con_C2_Amplitude<- df$FC_Rest_E_pre_P3_Con_C2_Amplitude
#create dataframe 
Rest_E_pre_Con_P3_QC.df<-data.frame(SubjectID, FC_Rest_E_pre_P3_Con_CPZ_Amplitude,FC_Rest_E_pre_P3_Con_CP1_Amplitude,FC_Rest_E_pre_P3_Con_CP2_Amplitude, FC_Rest_E_pre_P3_Con_P1_Amplitude, FC_Rest_E_pre_P3_Con_PZ_Amplitude, FC_Rest_E_pre_P3_Con_P2_Amplitude,FC_Rest_E_pre_P3_Con_C1_Amplitude,FC_Rest_E_pre_P3_Con_CZ_Amplitude,FC_Rest_E_pre_P3_Con_C2_Amplitude)
#melt dataframe
longformat.Rest_E_pre_Con_P3_QC.df <- melt(Rest_E_pre_Con_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.Rest_E_pre_Con_P3_QC.df)
#computation
ggwithinstats(data = longformat.Rest_E_pre_Con_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at Rest_E Po Congruent
SubjectID<-df$SubjectID
FC_Rest_E_po_P3_Con_CPZ_Amplitude<- df$FC_Rest_E_po_P3_Con_CPZ_Amplitude
FC_Rest_E_po_P3_Con_CP1_Amplitude<- df$FC_Rest_E_po_P3_Con_CP1_Amplitude
FC_Rest_E_po_P3_Con_CP2_Amplitude<- df$FC_Rest_E_po_P3_Con_CP2_Amplitude
FC_Rest_E_po_P3_Con_P1_Amplitude<- df$FC_Rest_E_po_P3_Con_P1_Amplitude
FC_Rest_E_po_P3_Con_PZ_Amplitude<- df$FC_Rest_E_po_P3_Con_PZ_Amplitude
FC_Rest_E_po_P3_Con_P2_Amplitude<- df$FC_Rest_E_po_P3_Con_P2_Amplitude
FC_Rest_E_po_P3_Con_C1_Amplitude<- df$FC_Rest_E_po_P3_Con_C1_Amplitude
FC_Rest_E_po_P3_Con_CZ_Amplitude<- df$FC_Rest_E_po_P3_Con_CZ_Amplitude
FC_Rest_E_po_P3_Con_C2_Amplitude<- df$FC_Rest_E_po_P3_Con_C2_Amplitude
#create dataframe 
Rest_E_po_Con_P3_QC.df<-data.frame(SubjectID, FC_Rest_E_po_P3_Con_CPZ_Amplitude,FC_Rest_E_po_P3_Con_CP1_Amplitude,FC_Rest_E_po_P3_Con_CP2_Amplitude, FC_Rest_E_po_P3_Con_P1_Amplitude, 
                                   FC_Rest_E_po_P3_Con_PZ_Amplitude, FC_Rest_E_po_P3_Con_P2_Amplitude,FC_Rest_E_po_P3_Con_C1_Amplitude,FC_Rest_E_po_P3_Con_CZ_Amplitude,FC_Rest_E_po_P3_Con_C2_Amplitude)
#melt dataframe
longformat.Rest_E_po_Con_P3_QC.df <- melt(Rest_E_po_Con_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.Rest_E_po_Con_P3_QC.df)
#computation
ggwithinstats(data = longformat.Rest_E_po_Con_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at Rest_E Pre Incongruent
SubjectID<-df$SubjectID
FC_Rest_E_pre_P3_Inc_CPZ_Amplitude<- df$FC_Rest_E_pre_P3_Inc_CPZ_Amplitude
FC_Rest_E_pre_P3_Inc_CP1_Amplitude<- df$FC_Rest_E_pre_P3_Inc_CP1_Amplitude
FC_Rest_E_pre_P3_Inc_CP2_Amplitude<- df$FC_Rest_E_pre_P3_Inc_CP2_Amplitude
FC_Rest_E_pre_P3_Inc_P1_Amplitude<- df$FC_Rest_E_pre_P3_Inc_P1_Amplitude
FC_Rest_E_pre_P3_Inc_PZ_Amplitude<- df$FC_Rest_E_pre_P3_Inc_PZ_Amplitude
FC_Rest_E_pre_P3_Inc_P2_Amplitude<- df$FC_Rest_E_pre_P3_Inc_P2_Amplitude
FC_Rest_E_pre_P3_Inc_C1_Amplitude<- df$FC_Rest_E_pre_P3_Inc_C1_Amplitude
FC_Rest_E_pre_P3_Inc_CZ_Amplitude<- df$FC_Rest_E_pre_P3_Inc_CZ_Amplitude
FC_Rest_E_pre_P3_Inc_C2_Amplitude<- df$FC_Rest_E_pre_P3_Inc_C2_Amplitude
#create dataframe 
Rest_E_pre_Inc_P3_QC.df<-data.frame(SubjectID, FC_Rest_E_pre_P3_Inc_CPZ_Amplitude,FC_Rest_E_pre_P3_Inc_CP1_Amplitude,FC_Rest_E_pre_P3_Inc_CP2_Amplitude, FC_Rest_E_pre_P3_Inc_P1_Amplitude, FC_Rest_E_pre_P3_Inc_PZ_Amplitude, FC_Rest_E_pre_P3_Inc_P2_Amplitude,FC_Rest_E_pre_P3_Inc_C1_Amplitude,FC_Rest_E_pre_P3_Inc_CZ_Amplitude,FC_Rest_E_pre_P3_Inc_C2_Amplitude)
#melt dataframe
longformat.Rest_E_pre_Inc_P3_QC.df <- melt(Rest_E_pre_Inc_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.Rest_E_pre_Inc_P3_QC.df)
#computation
ggwithinstats(data = longformat.Rest_E_pre_Inc_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at Rest_E Po Incongruent
SubjectID<-df$SubjectID
FC_Rest_E_po_P3_Inc_CPZ_Amplitude<- df$FC_Rest_E_po_P3_Inc_CPZ_Amplitude
FC_Rest_E_po_P3_Inc_CP1_Amplitude<- df$FC_Rest_E_po_P3_Inc_CP1_Amplitude
FC_Rest_E_po_P3_Inc_CP2_Amplitude<- df$FC_Rest_E_po_P3_Inc_CP2_Amplitude
FC_Rest_E_po_P3_Inc_P1_Amplitude<- df$FC_Rest_E_po_P3_Inc_P1_Amplitude
FC_Rest_E_po_P3_Inc_PZ_Amplitude<- df$FC_Rest_E_po_P3_Inc_PZ_Amplitude
FC_Rest_E_po_P3_Inc_P2_Amplitude<- df$FC_Rest_E_po_P3_Inc_P2_Amplitude
FC_Rest_E_po_P3_Inc_C1_Amplitude<- df$FC_Rest_E_po_P3_Inc_C1_Amplitude
FC_Rest_E_po_P3_Inc_CZ_Amplitude<- df$FC_Rest_E_po_P3_Inc_CZ_Amplitude
FC_Rest_E_po_P3_Inc_C2_Amplitude<- df$FC_Rest_E_po_P3_Inc_C2_Amplitude
#create dataframe 
Rest_E_po_Inc_P3_QC.df<-data.frame(SubjectID, FC_Rest_E_po_P3_Inc_CPZ_Amplitude,FC_Rest_E_po_P3_Inc_CP1_Amplitude,FC_Rest_E_po_P3_Inc_CP2_Amplitude, FC_Rest_E_po_P3_Inc_P1_Amplitude, 
                                   FC_Rest_E_po_P3_Inc_PZ_Amplitude, FC_Rest_E_po_P3_Inc_P2_Amplitude,FC_Rest_E_po_P3_Inc_C1_Amplitude,FC_Rest_E_po_P3_Inc_CZ_Amplitude,FC_Rest_E_po_P3_Inc_C2_Amplitude)
#melt dataframe
longformat.Rest_E_po_Inc_P3_QC.df <- melt(Rest_E_po_Inc_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.Rest_E_po_Inc_P3_QC.df)
#computation
ggwithinstats(data = longformat.Rest_E_po_Inc_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at E Pre Congruent
SubjectID<-df$SubjectID
FC_E_pre_P3_Con_CPZ_Amplitude<- df$FC_E_pre_P3_Con_CPZ_Amplitude
FC_E_pre_P3_Con_CP1_Amplitude<- df$FC_E_pre_P3_Con_CP1_Amplitude
FC_E_pre_P3_Con_CP2_Amplitude<- df$FC_E_pre_P3_Con_CP2_Amplitude
FC_E_pre_P3_Con_P1_Amplitude<- df$FC_E_pre_P3_Con_P1_Amplitude
FC_E_pre_P3_Con_PZ_Amplitude<- df$FC_E_pre_P3_Con_PZ_Amplitude
FC_E_pre_P3_Con_P2_Amplitude<- df$FC_E_pre_P3_Con_P2_Amplitude
FC_E_pre_P3_Con_C1_Amplitude<- df$FC_E_pre_P3_Con_C1_Amplitude
FC_E_pre_P3_Con_CZ_Amplitude<- df$FC_E_pre_P3_Con_CZ_Amplitude
FC_E_pre_P3_Con_C2_Amplitude<- df$FC_E_pre_P3_Con_C2_Amplitude
#create dataframe 
E_pre_Con_P3_QC.df<-data.frame(SubjectID, FC_E_pre_P3_Con_CPZ_Amplitude,FC_E_pre_P3_Con_CP1_Amplitude,FC_E_pre_P3_Con_CP2_Amplitude, FC_E_pre_P3_Con_P1_Amplitude, FC_E_pre_P3_Con_PZ_Amplitude, FC_E_pre_P3_Con_P2_Amplitude,FC_E_pre_P3_Con_C1_Amplitude,FC_E_pre_P3_Con_CZ_Amplitude,FC_E_pre_P3_Con_C2_Amplitude)
#melt dataframe
longformat.E_pre_Con_P3_QC.df <- melt(E_pre_Con_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.E_pre_Con_P3_QC.df)
#computation
ggwithinstats(data = longformat.E_pre_Con_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at E Po Congruent
SubjectID<-df$SubjectID
FC_E_po_P3_Con_CPZ_Amplitude<- df$FC_E_po_P3_Con_CPZ_Amplitude
FC_E_po_P3_Con_CP1_Amplitude<- df$FC_E_po_P3_Con_CP1_Amplitude
FC_E_po_P3_Con_CP2_Amplitude<- df$FC_E_po_P3_Con_CP2_Amplitude
FC_E_po_P3_Con_P1_Amplitude<- df$FC_E_po_P3_Con_P1_Amplitude
FC_E_po_P3_Con_PZ_Amplitude<- df$FC_E_po_P3_Con_PZ_Amplitude
FC_E_po_P3_Con_P2_Amplitude<- df$FC_E_po_P3_Con_P2_Amplitude
FC_E_po_P3_Con_C1_Amplitude<- df$FC_E_po_P3_Con_C1_Amplitude
FC_E_po_P3_Con_CZ_Amplitude<- df$FC_E_po_P3_Con_CZ_Amplitude
FC_E_po_P3_Con_C2_Amplitude<- df$FC_E_po_P3_Con_C2_Amplitude
#create dataframe 
E_po_Con_P3_QC.df<-data.frame(SubjectID, FC_E_po_P3_Con_CPZ_Amplitude,FC_E_po_P3_Con_CP1_Amplitude,FC_E_po_P3_Con_CP2_Amplitude, FC_E_po_P3_Con_P1_Amplitude, 
                              FC_E_po_P3_Con_PZ_Amplitude, FC_E_po_P3_Con_P2_Amplitude,FC_E_po_P3_Con_C1_Amplitude,FC_E_po_P3_Con_CZ_Amplitude,FC_E_po_P3_Con_C2_Amplitude)
#melt dataframe
longformat.E_po_Con_P3_QC.df <- melt(E_po_Con_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.E_po_Con_P3_QC.df)
#computation
ggwithinstats(data = longformat.E_po_Con_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at E Pre Incongruent
SubjectID<-df$SubjectID
FC_E_pre_P3_Inc_CPZ_Amplitude<- df$FC_E_pre_P3_Inc_CPZ_Amplitude
FC_E_pre_P3_Inc_CP1_Amplitude<- df$FC_E_pre_P3_Inc_CP1_Amplitude
FC_E_pre_P3_Inc_CP2_Amplitude<- df$FC_E_pre_P3_Inc_CP2_Amplitude
FC_E_pre_P3_Inc_P1_Amplitude<- df$FC_E_pre_P3_Inc_P1_Amplitude
FC_E_pre_P3_Inc_PZ_Amplitude<- df$FC_E_pre_P3_Inc_PZ_Amplitude
FC_E_pre_P3_Inc_P2_Amplitude<- df$FC_E_pre_P3_Inc_P2_Amplitude
FC_E_pre_P3_Inc_C1_Amplitude<- df$FC_E_pre_P3_Inc_C1_Amplitude
FC_E_pre_P3_Inc_CZ_Amplitude<- df$FC_E_pre_P3_Inc_CZ_Amplitude
FC_E_pre_P3_Inc_C2_Amplitude<- df$FC_E_pre_P3_Inc_C2_Amplitude
#create dataframe 
E_pre_Inc_P3_QC.df<-data.frame(SubjectID, FC_E_pre_P3_Inc_CPZ_Amplitude,FC_E_pre_P3_Inc_CP1_Amplitude,FC_E_pre_P3_Inc_CP2_Amplitude, FC_E_pre_P3_Inc_P1_Amplitude, FC_E_pre_P3_Inc_PZ_Amplitude, FC_E_pre_P3_Inc_P2_Amplitude,FC_E_pre_P3_Inc_C1_Amplitude,FC_E_pre_P3_Inc_CZ_Amplitude,FC_E_pre_P3_Inc_C2_Amplitude)
#melt dataframe
longformat.E_pre_Inc_P3_QC.df <- melt(E_pre_Inc_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.E_pre_Inc_P3_QC.df)
#computation
ggwithinstats(data = longformat.E_pre_Inc_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at E Po Incongruent
SubjectID<-df$SubjectID
FC_E_po_P3_Inc_CPZ_Amplitude<- df$FC_E_po_P3_Inc_CPZ_Amplitude
FC_E_po_P3_Inc_CP1_Amplitude<- df$FC_E_po_P3_Inc_CP1_Amplitude
FC_E_po_P3_Inc_CP2_Amplitude<- df$FC_E_po_P3_Inc_CP2_Amplitude
FC_E_po_P3_Inc_P1_Amplitude<- df$FC_E_po_P3_Inc_P1_Amplitude
FC_E_po_P3_Inc_PZ_Amplitude<- df$FC_E_po_P3_Inc_PZ_Amplitude
FC_E_po_P3_Inc_P2_Amplitude<- df$FC_E_po_P3_Inc_P2_Amplitude
FC_E_po_P3_Inc_C1_Amplitude<- df$FC_E_po_P3_Inc_C1_Amplitude
FC_E_po_P3_Inc_CZ_Amplitude<- df$FC_E_po_P3_Inc_CZ_Amplitude
FC_E_po_P3_Inc_C2_Amplitude<- df$FC_E_po_P3_Inc_C2_Amplitude
#create dataframe 
E_po_Inc_P3_QC.df<-data.frame(SubjectID, FC_E_po_P3_Inc_CPZ_Amplitude,FC_E_po_P3_Inc_CP1_Amplitude,FC_E_po_P3_Inc_CP2_Amplitude, FC_E_po_P3_Inc_P1_Amplitude, 
                              FC_E_po_P3_Inc_PZ_Amplitude, FC_E_po_P3_Inc_P2_Amplitude,FC_E_po_P3_Inc_C1_Amplitude,FC_E_po_P3_Inc_CZ_Amplitude,FC_E_po_P3_Inc_C2_Amplitude)
#melt dataframe
longformat.E_po_Inc_P3_QC.df <- melt(E_po_Inc_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.E_po_Inc_P3_QC.df)
#computation
ggwithinstats(data = longformat.E_po_Inc_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Amplitude M2
#Call Variables at Rest_T Pre Congruent
SubjectID<-df$SubjectID
FC_Rest_T_pre_P3_Con_CPZ_Amplitude<- df$FC_Rest_T_pre_P3_Con_CPZ_Amplitude
FC_Rest_T_pre_P3_Con_CP1_Amplitude<- df$FC_Rest_T_pre_P3_Con_CP1_Amplitude
FC_Rest_T_pre_P3_Con_CP2_Amplitude<- df$FC_Rest_T_pre_P3_Con_CP2_Amplitude
FC_Rest_T_pre_P3_Con_P1_Amplitude<- df$FC_Rest_T_pre_P3_Con_P1_Amplitude
FC_Rest_T_pre_P3_Con_PZ_Amplitude<- df$FC_Rest_T_pre_P3_Con_PZ_Amplitude
FC_Rest_T_pre_P3_Con_P2_Amplitude<- df$FC_Rest_T_pre_P3_Con_P2_Amplitude
FC_Rest_T_pre_P3_Con_C1_Amplitude<- df$FC_Rest_T_pre_P3_Con_C1_Amplitude
FC_Rest_T_pre_P3_Con_CZ_Amplitude<- df$FC_Rest_T_pre_P3_Con_CZ_Amplitude
FC_Rest_T_pre_P3_Con_C2_Amplitude<- df$FC_Rest_T_pre_P3_Con_C2_Amplitude
#create dataframe 
Rest_T_pre_Con_P3_QC.df<-data.frame(SubjectID, FC_Rest_T_pre_P3_Con_CPZ_Amplitude,FC_Rest_T_pre_P3_Con_CP1_Amplitude,FC_Rest_T_pre_P3_Con_CP2_Amplitude, FC_Rest_T_pre_P3_Con_P1_Amplitude, FC_Rest_T_pre_P3_Con_PZ_Amplitude, FC_Rest_T_pre_P3_Con_P2_Amplitude,FC_Rest_T_pre_P3_Con_C1_Amplitude,FC_Rest_T_pre_P3_Con_CZ_Amplitude,FC_Rest_T_pre_P3_Con_C2_Amplitude)
#melt dataframe
longformat.Rest_T_pre_Con_P3_QC.df <- melt(Rest_T_pre_Con_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.Rest_T_pre_Con_P3_QC.df)
#computation
ggwithinstats(data = longformat.Rest_T_pre_Con_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at Rest_T Po Congruent
SubjectID<-df$SubjectID
FC_Rest_T_po_P3_Con_CPZ_Amplitude<- df$FC_Rest_T_po_P3_Con_CPZ_Amplitude
FC_Rest_T_po_P3_Con_CP1_Amplitude<- df$FC_Rest_T_po_P3_Con_CP1_Amplitude
FC_Rest_T_po_P3_Con_CP2_Amplitude<- df$FC_Rest_T_po_P3_Con_CP2_Amplitude
FC_Rest_T_po_P3_Con_P1_Amplitude<- df$FC_Rest_T_po_P3_Con_P1_Amplitude
FC_Rest_T_po_P3_Con_PZ_Amplitude<- df$FC_Rest_T_po_P3_Con_PZ_Amplitude
FC_Rest_T_po_P3_Con_P2_Amplitude<- df$FC_Rest_T_po_P3_Con_P2_Amplitude
FC_Rest_T_po_P3_Con_C1_Amplitude<- df$FC_Rest_T_po_P3_Con_C1_Amplitude
FC_Rest_T_po_P3_Con_CZ_Amplitude<- df$FC_Rest_T_po_P3_Con_CZ_Amplitude
FC_Rest_T_po_P3_Con_C2_Amplitude<- df$FC_Rest_T_po_P3_Con_C2_Amplitude
#create dataframe 
Rest_T_po_Con_P3_QC.df<-data.frame(SubjectID, FC_Rest_T_po_P3_Con_CPZ_Amplitude,FC_Rest_T_po_P3_Con_CP1_Amplitude,FC_Rest_T_po_P3_Con_CP2_Amplitude, FC_Rest_T_po_P3_Con_P1_Amplitude, 
                                   FC_Rest_T_po_P3_Con_PZ_Amplitude, FC_Rest_T_po_P3_Con_P2_Amplitude,FC_Rest_T_po_P3_Con_C1_Amplitude,FC_Rest_T_po_P3_Con_CZ_Amplitude,FC_Rest_T_po_P3_Con_C2_Amplitude)
#melt dataframe
longformat.Rest_T_po_Con_P3_QC.df <- melt(Rest_T_po_Con_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.Rest_T_po_Con_P3_QC.df)
#computation
ggwithinstats(data = longformat.Rest_T_po_Con_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at Rest_T Pre Incongruent
SubjectID<-df$SubjectID
FC_Rest_T_pre_P3_Inc_CPZ_Amplitude<- df$FC_Rest_T_pre_P3_Inc_CPZ_Amplitude
FC_Rest_T_pre_P3_Inc_CP1_Amplitude<- df$FC_Rest_T_pre_P3_Inc_CP1_Amplitude
FC_Rest_T_pre_P3_Inc_CP2_Amplitude<- df$FC_Rest_T_pre_P3_Inc_CP2_Amplitude
FC_Rest_T_pre_P3_Inc_P1_Amplitude<- df$FC_Rest_T_pre_P3_Inc_P1_Amplitude
FC_Rest_T_pre_P3_Inc_PZ_Amplitude<- df$FC_Rest_T_pre_P3_Inc_PZ_Amplitude
FC_Rest_T_pre_P3_Inc_P2_Amplitude<- df$FC_Rest_T_pre_P3_Inc_P2_Amplitude
FC_Rest_T_pre_P3_Inc_C1_Amplitude<- df$FC_Rest_T_pre_P3_Inc_C1_Amplitude
FC_Rest_T_pre_P3_Inc_CZ_Amplitude<- df$FC_Rest_T_pre_P3_Inc_CZ_Amplitude
FC_Rest_T_pre_P3_Inc_C2_Amplitude<- df$FC_Rest_T_pre_P3_Inc_C2_Amplitude
#create dataframe 
Rest_T_pre_Inc_P3_QC.df<-data.frame(SubjectID, FC_Rest_T_pre_P3_Inc_CPZ_Amplitude,FC_Rest_T_pre_P3_Inc_CP1_Amplitude,FC_Rest_T_pre_P3_Inc_CP2_Amplitude, FC_Rest_T_pre_P3_Inc_P1_Amplitude, FC_Rest_T_pre_P3_Inc_PZ_Amplitude, FC_Rest_T_pre_P3_Inc_P2_Amplitude,FC_Rest_T_pre_P3_Inc_C1_Amplitude,FC_Rest_T_pre_P3_Inc_CZ_Amplitude,FC_Rest_T_pre_P3_Inc_C2_Amplitude)
#melt dataframe
longformat.Rest_T_pre_Inc_P3_QC.df <- melt(Rest_T_pre_Inc_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.Rest_T_pre_Inc_P3_QC.df)
#computation
ggwithinstats(data = longformat.Rest_T_pre_Inc_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at Rest_T Po Incongruent
SubjectID<-df$SubjectID
FC_Rest_T_po_P3_Inc_CPZ_Amplitude<- df$FC_Rest_T_po_P3_Inc_CPZ_Amplitude
FC_Rest_T_po_P3_Inc_CP1_Amplitude<- df$FC_Rest_T_po_P3_Inc_CP1_Amplitude
FC_Rest_T_po_P3_Inc_CP2_Amplitude<- df$FC_Rest_T_po_P3_Inc_CP2_Amplitude
FC_Rest_T_po_P3_Inc_P1_Amplitude<- df$FC_Rest_T_po_P3_Inc_P1_Amplitude
FC_Rest_T_po_P3_Inc_PZ_Amplitude<- df$FC_Rest_T_po_P3_Inc_PZ_Amplitude
FC_Rest_T_po_P3_Inc_P2_Amplitude<- df$FC_Rest_T_po_P3_Inc_P2_Amplitude
FC_Rest_T_po_P3_Inc_C1_Amplitude<- df$FC_Rest_T_po_P3_Inc_C1_Amplitude
FC_Rest_T_po_P3_Inc_CZ_Amplitude<- df$FC_Rest_T_po_P3_Inc_CZ_Amplitude
FC_Rest_T_po_P3_Inc_C2_Amplitude<- df$FC_Rest_T_po_P3_Inc_C2_Amplitude
#create dataframe 
Rest_T_po_Inc_P3_QC.df<-data.frame(SubjectID, FC_Rest_T_po_P3_Inc_CPZ_Amplitude,FC_Rest_T_po_P3_Inc_CP1_Amplitude,FC_Rest_T_po_P3_Inc_CP2_Amplitude, FC_Rest_T_po_P3_Inc_P1_Amplitude, 
                                   FC_Rest_T_po_P3_Inc_PZ_Amplitude, FC_Rest_T_po_P3_Inc_P2_Amplitude,FC_Rest_T_po_P3_Inc_C1_Amplitude,FC_Rest_T_po_P3_Inc_CZ_Amplitude,FC_Rest_T_po_P3_Inc_C2_Amplitude)
#melt dataframe
longformat.Rest_T_po_Inc_P3_QC.df <- melt(Rest_T_po_Inc_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.Rest_T_po_Inc_P3_QC.df)
#computation
ggwithinstats(data = longformat.Rest_T_po_Inc_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at T Pre Congruent
SubjectID<-df$SubjectID
FC_T_pre_P3_Con_CPZ_Amplitude<- df$FC_T_pre_P3_Con_CPZ_Amplitude
FC_T_pre_P3_Con_CP1_Amplitude<- df$FC_T_pre_P3_Con_CP1_Amplitude
FC_T_pre_P3_Con_CP2_Amplitude<- df$FC_T_pre_P3_Con_CP2_Amplitude
FC_T_pre_P3_Con_P1_Amplitude<- df$FC_T_pre_P3_Con_P1_Amplitude
FC_T_pre_P3_Con_PZ_Amplitude<- df$FC_T_pre_P3_Con_PZ_Amplitude
FC_T_pre_P3_Con_P2_Amplitude<- df$FC_T_pre_P3_Con_P2_Amplitude
FC_T_pre_P3_Con_C1_Amplitude<- df$FC_T_pre_P3_Con_C1_Amplitude
FC_T_pre_P3_Con_CZ_Amplitude<- df$FC_T_pre_P3_Con_CZ_Amplitude
FC_T_pre_P3_Con_C2_Amplitude<- df$FC_T_pre_P3_Con_C2_Amplitude
#create dataframe 
E_pre_Con_P3_QC.df<-data.frame(SubjectID, FC_T_pre_P3_Con_CPZ_Amplitude,FC_T_pre_P3_Con_CP1_Amplitude,FC_T_pre_P3_Con_CP2_Amplitude, FC_T_pre_P3_Con_P1_Amplitude, FC_T_pre_P3_Con_PZ_Amplitude, FC_T_pre_P3_Con_P2_Amplitude,FC_T_pre_P3_Con_C1_Amplitude,FC_T_pre_P3_Con_CZ_Amplitude,FC_T_pre_P3_Con_C2_Amplitude)
#melt dataframe
longformat.E_pre_Con_P3_QC.df <- melt(E_pre_Con_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.E_pre_Con_P3_QC.df)
#computation
ggwithinstats(data = longformat.E_pre_Con_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at T Po Congruent
SubjectID<-df$SubjectID
FC_T_po_P3_Con_CPZ_Amplitude<- df$FC_T_po_P3_Con_CPZ_Amplitude
FC_T_po_P3_Con_CP1_Amplitude<- df$FC_T_po_P3_Con_CP1_Amplitude
FC_T_po_P3_Con_CP2_Amplitude<- df$FC_T_po_P3_Con_CP2_Amplitude
FC_T_po_P3_Con_P1_Amplitude<- df$FC_T_po_P3_Con_P1_Amplitude
FC_T_po_P3_Con_PZ_Amplitude<- df$FC_T_po_P3_Con_PZ_Amplitude
FC_T_po_P3_Con_P2_Amplitude<- df$FC_T_po_P3_Con_P2_Amplitude
FC_T_po_P3_Con_C1_Amplitude<- df$FC_T_po_P3_Con_C1_Amplitude
FC_T_po_P3_Con_CZ_Amplitude<- df$FC_T_po_P3_Con_CZ_Amplitude
FC_T_po_P3_Con_C2_Amplitude<- df$FC_T_po_P3_Con_C2_Amplitude
#create dataframe 
E_po_Con_P3_QC.df<-data.frame(SubjectID, FC_T_po_P3_Con_CPZ_Amplitude,FC_T_po_P3_Con_CP1_Amplitude,FC_T_po_P3_Con_CP2_Amplitude, FC_T_po_P3_Con_P1_Amplitude, 
                              FC_T_po_P3_Con_PZ_Amplitude, FC_T_po_P3_Con_P2_Amplitude,FC_T_po_P3_Con_C1_Amplitude,FC_T_po_P3_Con_CZ_Amplitude,FC_T_po_P3_Con_C2_Amplitude)
#melt dataframe
longformat.E_po_Con_P3_QC.df <- melt(E_po_Con_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.E_po_Con_P3_QC.df)
#computation
ggwithinstats(data = longformat.E_po_Con_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at T Pre Incongruent
SubjectID<-df$SubjectID
FC_T_pre_P3_Inc_CPZ_Amplitude<- df$FC_T_pre_P3_Inc_CPZ_Amplitude
FC_T_pre_P3_Inc_CP1_Amplitude<- df$FC_T_pre_P3_Inc_CP1_Amplitude
FC_T_pre_P3_Inc_CP2_Amplitude<- df$FC_T_pre_P3_Inc_CP2_Amplitude
FC_T_pre_P3_Inc_P1_Amplitude<- df$FC_T_pre_P3_Inc_P1_Amplitude
FC_T_pre_P3_Inc_PZ_Amplitude<- df$FC_T_pre_P3_Inc_PZ_Amplitude
FC_T_pre_P3_Inc_P2_Amplitude<- df$FC_T_pre_P3_Inc_P2_Amplitude
FC_T_pre_P3_Inc_C1_Amplitude<- df$FC_T_pre_P3_Inc_C1_Amplitude
FC_T_pre_P3_Inc_CZ_Amplitude<- df$FC_T_pre_P3_Inc_CZ_Amplitude
FC_T_pre_P3_Inc_C2_Amplitude<- df$FC_T_pre_P3_Inc_C2_Amplitude
#create dataframe 
E_pre_Inc_P3_QC.df<-data.frame(SubjectID, FC_T_pre_P3_Inc_CPZ_Amplitude,FC_T_pre_P3_Inc_CP1_Amplitude,FC_T_pre_P3_Inc_CP2_Amplitude, FC_T_pre_P3_Inc_P1_Amplitude, FC_T_pre_P3_Inc_PZ_Amplitude, FC_T_pre_P3_Inc_P2_Amplitude,FC_T_pre_P3_Inc_C1_Amplitude,FC_T_pre_P3_Inc_CZ_Amplitude,FC_T_pre_P3_Inc_C2_Amplitude)
#melt dataframe
longformat.E_pre_Inc_P3_QC.df <- melt(E_pre_Inc_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.E_pre_Inc_P3_QC.df)
#computation
ggwithinstats(data = longformat.E_pre_Inc_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at T Po Incongruent
SubjectID<-df$SubjectID
FC_T_po_P3_Inc_CPZ_Amplitude<- df$FC_T_po_P3_Inc_CPZ_Amplitude
FC_T_po_P3_Inc_CP1_Amplitude<- df$FC_T_po_P3_Inc_CP1_Amplitude
FC_T_po_P3_Inc_CP2_Amplitude<- df$FC_T_po_P3_Inc_CP2_Amplitude
FC_T_po_P3_Inc_P1_Amplitude<- df$FC_T_po_P3_Inc_P1_Amplitude
FC_T_po_P3_Inc_PZ_Amplitude<- df$FC_T_po_P3_Inc_PZ_Amplitude
FC_T_po_P3_Inc_P2_Amplitude<- df$FC_T_po_P3_Inc_P2_Amplitude
FC_T_po_P3_Inc_C1_Amplitude<- df$FC_T_po_P3_Inc_C1_Amplitude
FC_T_po_P3_Inc_CZ_Amplitude<- df$FC_T_po_P3_Inc_CZ_Amplitude
FC_T_po_P3_Inc_C2_Amplitude<- df$FC_T_po_P3_Inc_C2_Amplitude
#create dataframe 
E_po_Inc_P3_QC.df<-data.frame(SubjectID, FC_T_po_P3_Inc_CPZ_Amplitude,FC_T_po_P3_Inc_CP1_Amplitude,FC_T_po_P3_Inc_CP2_Amplitude, FC_T_po_P3_Inc_P1_Amplitude, 
                              FC_T_po_P3_Inc_PZ_Amplitude, FC_T_po_P3_Inc_P2_Amplitude,FC_T_po_P3_Inc_C1_Amplitude,FC_T_po_P3_Inc_CZ_Amplitude,FC_T_po_P3_Inc_C2_Amplitude)
#melt dataframe
longformat.E_po_Inc_P3_QC.df <- melt(E_po_Inc_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.E_po_Inc_P3_QC.df)
#computation
ggwithinstats(data = longformat.E_po_Inc_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Latency M1
#Call Variables at Rest_E Pre Congruent
SubjectID<-df$SubjectID
FC_Rest_E_pre_P3_Con_CPZ_Latency<- df$FC_Rest_E_pre_P3_Con_CPZ_Latency
FC_Rest_E_pre_P3_Con_CP1_Latency<- df$FC_Rest_E_pre_P3_Con_CP1_Latency
FC_Rest_E_pre_P3_Con_CP2_Latency<- df$FC_Rest_E_pre_P3_Con_CP2_Latency
FC_Rest_E_pre_P3_Con_P1_Latency<- df$FC_Rest_E_pre_P3_Con_P1_Latency
FC_Rest_E_pre_P3_Con_PZ_Latency<- df$FC_Rest_E_pre_P3_Con_PZ_Latency
FC_Rest_E_pre_P3_Con_P2_Latency<- df$FC_Rest_E_pre_P3_Con_P2_Latency
FC_Rest_E_pre_P3_Con_C1_Latency<- df$FC_Rest_E_pre_P3_Con_C1_Latency
FC_Rest_E_pre_P3_Con_CZ_Latency<- df$FC_Rest_E_pre_P3_Con_CZ_Latency
FC_Rest_E_pre_P3_Con_C2_Latency<- df$FC_Rest_E_pre_P3_Con_C2_Latency
#create dataframe 
Rest_E_pre_Con_P3_QC.df<-data.frame(SubjectID, FC_Rest_E_pre_P3_Con_CPZ_Latency,FC_Rest_E_pre_P3_Con_CP1_Latency,FC_Rest_E_pre_P3_Con_CP2_Latency, FC_Rest_E_pre_P3_Con_P1_Latency, FC_Rest_E_pre_P3_Con_PZ_Latency, FC_Rest_E_pre_P3_Con_P2_Latency,FC_Rest_E_pre_P3_Con_C1_Latency,FC_Rest_E_pre_P3_Con_CZ_Latency,FC_Rest_E_pre_P3_Con_C2_Latency)
#melt dataframe
longformat.Rest_E_pre_Con_P3_QC.df <- melt(Rest_E_pre_Con_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.Rest_E_pre_Con_P3_QC.df)
#computation
ggwithinstats(data = longformat.Rest_E_pre_Con_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at Rest_E Po Congruent
SubjectID<-df$SubjectID
FC_Rest_E_po_P3_Con_CPZ_Latency<- df$FC_Rest_E_po_P3_Con_CPZ_Latency
FC_Rest_E_po_P3_Con_CP1_Latency<- df$FC_Rest_E_po_P3_Con_CP1_Latency
FC_Rest_E_po_P3_Con_CP2_Latency<- df$FC_Rest_E_po_P3_Con_CP2_Latency
FC_Rest_E_po_P3_Con_P1_Latency<- df$FC_Rest_E_po_P3_Con_P1_Latency
FC_Rest_E_po_P3_Con_PZ_Latency<- df$FC_Rest_E_po_P3_Con_PZ_Latency
FC_Rest_E_po_P3_Con_P2_Latency<- df$FC_Rest_E_po_P3_Con_P2_Latency
FC_Rest_E_po_P3_Con_C1_Latency<- df$FC_Rest_E_po_P3_Con_C1_Latency
FC_Rest_E_po_P3_Con_CZ_Latency<- df$FC_Rest_E_po_P3_Con_CZ_Latency
FC_Rest_E_po_P3_Con_C2_Latency<- df$FC_Rest_E_po_P3_Con_C2_Latency
#create dataframe 
Rest_E_po_Con_P3_QC.df<-data.frame(SubjectID, FC_Rest_E_po_P3_Con_CPZ_Latency,FC_Rest_E_po_P3_Con_CP1_Latency,FC_Rest_E_po_P3_Con_CP2_Latency, FC_Rest_E_po_P3_Con_P1_Latency, 
                                   FC_Rest_E_po_P3_Con_PZ_Latency, FC_Rest_E_po_P3_Con_P2_Latency,FC_Rest_E_po_P3_Con_C1_Latency,FC_Rest_E_po_P3_Con_CZ_Latency,FC_Rest_E_po_P3_Con_C2_Latency)
#melt dataframe
longformat.Rest_E_po_Con_P3_QC.df <- melt(Rest_E_po_Con_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.Rest_E_po_Con_P3_QC.df)
#computation
ggwithinstats(data = longformat.Rest_E_po_Con_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at Rest_E Pre Incongruent
SubjectID<-df$SubjectID
FC_Rest_E_pre_P3_Inc_CPZ_Latency<- df$FC_Rest_E_pre_P3_Inc_CPZ_Latency
FC_Rest_E_pre_P3_Inc_CP1_Latency<- df$FC_Rest_E_pre_P3_Inc_CP1_Latency
FC_Rest_E_pre_P3_Inc_CP2_Latency<- df$FC_Rest_E_pre_P3_Inc_CP2_Latency
FC_Rest_E_pre_P3_Inc_P1_Latency<- df$FC_Rest_E_pre_P3_Inc_P1_Latency
FC_Rest_E_pre_P3_Inc_PZ_Latency<- df$FC_Rest_E_pre_P3_Inc_PZ_Latency
FC_Rest_E_pre_P3_Inc_P2_Latency<- df$FC_Rest_E_pre_P3_Inc_P2_Latency
FC_Rest_E_pre_P3_Inc_C1_Latency<- df$FC_Rest_E_pre_P3_Inc_C1_Latency
FC_Rest_E_pre_P3_Inc_CZ_Latency<- df$FC_Rest_E_pre_P3_Inc_CZ_Latency
FC_Rest_E_pre_P3_Inc_C2_Latency<- df$FC_Rest_E_pre_P3_Inc_C2_Latency
#create dataframe 
Rest_E_pre_Inc_P3_QC.df<-data.frame(SubjectID, FC_Rest_E_pre_P3_Inc_CPZ_Latency,FC_Rest_E_pre_P3_Inc_CP1_Latency,FC_Rest_E_pre_P3_Inc_CP2_Latency, FC_Rest_E_pre_P3_Inc_P1_Latency, FC_Rest_E_pre_P3_Inc_PZ_Latency, FC_Rest_E_pre_P3_Inc_P2_Latency,FC_Rest_E_pre_P3_Inc_C1_Latency,FC_Rest_E_pre_P3_Inc_CZ_Latency,FC_Rest_E_pre_P3_Inc_C2_Latency)
#melt dataframe
longformat.Rest_E_pre_Inc_P3_QC.df <- melt(Rest_E_pre_Inc_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.Rest_E_pre_Inc_P3_QC.df)
#computation
ggwithinstats(data = longformat.Rest_E_pre_Inc_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at Rest_E Po Incongruent
SubjectID<-df$SubjectID
FC_Rest_E_po_P3_Inc_CPZ_Latency<- df$FC_Rest_E_po_P3_Inc_CPZ_Latency
FC_Rest_E_po_P3_Inc_CP1_Latency<- df$FC_Rest_E_po_P3_Inc_CP1_Latency
FC_Rest_E_po_P3_Inc_CP2_Latency<- df$FC_Rest_E_po_P3_Inc_CP2_Latency
FC_Rest_E_po_P3_Inc_P1_Latency<- df$FC_Rest_E_po_P3_Inc_P1_Latency
FC_Rest_E_po_P3_Inc_PZ_Latency<- df$FC_Rest_E_po_P3_Inc_PZ_Latency
FC_Rest_E_po_P3_Inc_P2_Latency<- df$FC_Rest_E_po_P3_Inc_P2_Latency
FC_Rest_E_po_P3_Inc_C1_Latency<- df$FC_Rest_E_po_P3_Inc_C1_Latency
FC_Rest_E_po_P3_Inc_CZ_Latency<- df$FC_Rest_E_po_P3_Inc_CZ_Latency
FC_Rest_E_po_P3_Inc_C2_Latency<- df$FC_Rest_E_po_P3_Inc_C2_Latency
#create dataframe 
Rest_E_po_Inc_P3_QC.df<-data.frame(SubjectID, FC_Rest_E_po_P3_Inc_CPZ_Latency,FC_Rest_E_po_P3_Inc_CP1_Latency,FC_Rest_E_po_P3_Inc_CP2_Latency, FC_Rest_E_po_P3_Inc_P1_Latency, 
                                   FC_Rest_E_po_P3_Inc_PZ_Latency, FC_Rest_E_po_P3_Inc_P2_Latency,FC_Rest_E_po_P3_Inc_C1_Latency,FC_Rest_E_po_P3_Inc_CZ_Latency,FC_Rest_E_po_P3_Inc_C2_Latency)
#melt dataframe
longformat.Rest_E_po_Inc_P3_QC.df <- melt(Rest_E_po_Inc_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.Rest_E_po_Inc_P3_QC.df)
#computation
ggwithinstats(data = longformat.Rest_E_po_Inc_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at E Pre Congruent
SubjectID<-df$SubjectID
FC_E_pre_P3_Con_CPZ_Latency<- df$FC_E_pre_P3_Con_CPZ_Latency
FC_E_pre_P3_Con_CP1_Latency<- df$FC_E_pre_P3_Con_CP1_Latency
FC_E_pre_P3_Con_CP2_Latency<- df$FC_E_pre_P3_Con_CP2_Latency
FC_E_pre_P3_Con_P1_Latency<- df$FC_E_pre_P3_Con_P1_Latency
FC_E_pre_P3_Con_PZ_Latency<- df$FC_E_pre_P3_Con_PZ_Latency
FC_E_pre_P3_Con_P2_Latency<- df$FC_E_pre_P3_Con_P2_Latency
FC_E_pre_P3_Con_C1_Latency<- df$FC_E_pre_P3_Con_C1_Latency
FC_E_pre_P3_Con_CZ_Latency<- df$FC_E_pre_P3_Con_CZ_Latency
FC_E_pre_P3_Con_C2_Latency<- df$FC_E_pre_P3_Con_C2_Latency
#create dataframe 
E_pre_Con_P3_QC.df<-data.frame(SubjectID, FC_E_pre_P3_Con_CPZ_Latency,FC_E_pre_P3_Con_CP1_Latency,FC_E_pre_P3_Con_CP2_Latency, FC_E_pre_P3_Con_P1_Latency, FC_E_pre_P3_Con_PZ_Latency, FC_E_pre_P3_Con_P2_Latency,FC_E_pre_P3_Con_C1_Latency,FC_E_pre_P3_Con_CZ_Latency,FC_E_pre_P3_Con_C2_Latency)
#melt dataframe
longformat.E_pre_Con_P3_QC.df <- melt(E_pre_Con_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.E_pre_Con_P3_QC.df)
#computation
ggwithinstats(data = longformat.E_pre_Con_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at E Po Congruent
SubjectID<-df$SubjectID
FC_E_po_P3_Con_CPZ_Latency<- df$FC_E_po_P3_Con_CPZ_Latency
FC_E_po_P3_Con_CP1_Latency<- df$FC_E_po_P3_Con_CP1_Latency
FC_E_po_P3_Con_CP2_Latency<- df$FC_E_po_P3_Con_CP2_Latency
FC_E_po_P3_Con_P1_Latency<- df$FC_E_po_P3_Con_P1_Latency
FC_E_po_P3_Con_PZ_Latency<- df$FC_E_po_P3_Con_PZ_Latency
FC_E_po_P3_Con_P2_Latency<- df$FC_E_po_P3_Con_P2_Latency
FC_E_po_P3_Con_C1_Latency<- df$FC_E_po_P3_Con_C1_Latency
FC_E_po_P3_Con_CZ_Latency<- df$FC_E_po_P3_Con_CZ_Latency
FC_E_po_P3_Con_C2_Latency<- df$FC_E_po_P3_Con_C2_Latency
#create dataframe 
E_po_Con_P3_QC.df<-data.frame(SubjectID, FC_E_po_P3_Con_CPZ_Latency,FC_E_po_P3_Con_CP1_Latency,FC_E_po_P3_Con_CP2_Latency, FC_E_po_P3_Con_P1_Latency, 
                              FC_E_po_P3_Con_PZ_Latency, FC_E_po_P3_Con_P2_Latency,FC_E_po_P3_Con_C1_Latency,FC_E_po_P3_Con_CZ_Latency,FC_E_po_P3_Con_C2_Latency)
#melt dataframe
longformat.E_po_Con_P3_QC.df <- melt(E_po_Con_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.E_po_Con_P3_QC.df)
#computation
ggwithinstats(data = longformat.E_po_Con_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at E Pre Incongruent
SubjectID<-df$SubjectID
FC_E_pre_P3_Inc_CPZ_Latency<- df$FC_E_pre_P3_Inc_CPZ_Latency
FC_E_pre_P3_Inc_CP1_Latency<- df$FC_E_pre_P3_Inc_CP1_Latency
FC_E_pre_P3_Inc_CP2_Latency<- df$FC_E_pre_P3_Inc_CP2_Latency
FC_E_pre_P3_Inc_P1_Latency<- df$FC_E_pre_P3_Inc_P1_Latency
FC_E_pre_P3_Inc_PZ_Latency<- df$FC_E_pre_P3_Inc_PZ_Latency
FC_E_pre_P3_Inc_P2_Latency<- df$FC_E_pre_P3_Inc_P2_Latency
FC_E_pre_P3_Inc_C1_Latency<- df$FC_E_pre_P3_Inc_C1_Latency
FC_E_pre_P3_Inc_CZ_Latency<- df$FC_E_pre_P3_Inc_CZ_Latency
FC_E_pre_P3_Inc_C2_Latency<- df$FC_E_pre_P3_Inc_C2_Latency
#create dataframe 
E_pre_Inc_P3_QC.df<-data.frame(SubjectID, FC_E_pre_P3_Inc_CPZ_Latency,FC_E_pre_P3_Inc_CP1_Latency,FC_E_pre_P3_Inc_CP2_Latency, FC_E_pre_P3_Inc_P1_Latency, FC_E_pre_P3_Inc_PZ_Latency, FC_E_pre_P3_Inc_P2_Latency,FC_E_pre_P3_Inc_C1_Latency,FC_E_pre_P3_Inc_CZ_Latency,FC_E_pre_P3_Inc_C2_Latency)
#melt dataframe
longformat.E_pre_Inc_P3_QC.df <- melt(E_pre_Inc_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.E_pre_Inc_P3_QC.df)
#computation
ggwithinstats(data = longformat.E_pre_Inc_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at E Po Incongruent
SubjectID<-df$SubjectID
FC_E_po_P3_Inc_CPZ_Latency<- df$FC_E_po_P3_Inc_CPZ_Latency
FC_E_po_P3_Inc_CP1_Latency<- df$FC_E_po_P3_Inc_CP1_Latency
FC_E_po_P3_Inc_CP2_Latency<- df$FC_E_po_P3_Inc_CP2_Latency
FC_E_po_P3_Inc_P1_Latency<- df$FC_E_po_P3_Inc_P1_Latency
FC_E_po_P3_Inc_PZ_Latency<- df$FC_E_po_P3_Inc_PZ_Latency
FC_E_po_P3_Inc_P2_Latency<- df$FC_E_po_P3_Inc_P2_Latency
FC_E_po_P3_Inc_C1_Latency<- df$FC_E_po_P3_Inc_C1_Latency
FC_E_po_P3_Inc_CZ_Latency<- df$FC_E_po_P3_Inc_CZ_Latency
FC_E_po_P3_Inc_C2_Latency<- df$FC_E_po_P3_Inc_C2_Latency
#create dataframe 
E_po_Inc_P3_QC.df<-data.frame(SubjectID, FC_E_po_P3_Inc_CPZ_Latency,FC_E_po_P3_Inc_CP1_Latency,FC_E_po_P3_Inc_CP2_Latency, FC_E_po_P3_Inc_P1_Latency, 
                              FC_E_po_P3_Inc_PZ_Latency, FC_E_po_P3_Inc_P2_Latency,FC_E_po_P3_Inc_C1_Latency,FC_E_po_P3_Inc_CZ_Latency,FC_E_po_P3_Inc_C2_Latency)
#melt dataframe
longformat.E_po_Inc_P3_QC.df <- melt(E_po_Inc_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.E_po_Inc_P3_QC.df)
#computation
ggwithinstats(data = longformat.E_po_Inc_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Latency M2
#Call Variables at Rest_T Pre Congruent
SubjectID<-df$SubjectID
FC_Rest_T_pre_P3_Con_CPZ_Latency<- df$FC_Rest_T_pre_P3_Con_CPZ_Latency
FC_Rest_T_pre_P3_Con_CP1_Latency<- df$FC_Rest_T_pre_P3_Con_CP1_Latency
FC_Rest_T_pre_P3_Con_CP2_Latency<- df$FC_Rest_T_pre_P3_Con_CP2_Latency
FC_Rest_T_pre_P3_Con_P1_Latency<- df$FC_Rest_T_pre_P3_Con_P1_Latency
FC_Rest_T_pre_P3_Con_PZ_Latency<- df$FC_Rest_T_pre_P3_Con_PZ_Latency
FC_Rest_T_pre_P3_Con_P2_Latency<- df$FC_Rest_T_pre_P3_Con_P2_Latency
FC_Rest_T_pre_P3_Con_C1_Latency<- df$FC_Rest_T_pre_P3_Con_C1_Latency
FC_Rest_T_pre_P3_Con_CZ_Latency<- df$FC_Rest_T_pre_P3_Con_CZ_Latency
FC_Rest_T_pre_P3_Con_C2_Latency<- df$FC_Rest_T_pre_P3_Con_C2_Latency
#create dataframe 
Rest_T_pre_Con_P3_QC.df<-data.frame(SubjectID, FC_Rest_T_pre_P3_Con_CPZ_Latency,FC_Rest_T_pre_P3_Con_CP1_Latency,FC_Rest_T_pre_P3_Con_CP2_Latency, FC_Rest_T_pre_P3_Con_P1_Latency, FC_Rest_T_pre_P3_Con_PZ_Latency, FC_Rest_T_pre_P3_Con_P2_Latency,FC_Rest_T_pre_P3_Con_C1_Latency,FC_Rest_T_pre_P3_Con_CZ_Latency,FC_Rest_T_pre_P3_Con_C2_Latency)
#melt dataframe
longformat.Rest_T_pre_Con_P3_QC.df <- melt(Rest_T_pre_Con_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.Rest_T_pre_Con_P3_QC.df)
#computation
ggwithinstats(data = longformat.Rest_T_pre_Con_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at Rest_T Po Congruent
SubjectID<-df$SubjectID
FC_Rest_T_po_P3_Con_CPZ_Latency<- df$FC_Rest_T_po_P3_Con_CPZ_Latency
FC_Rest_T_po_P3_Con_CP1_Latency<- df$FC_Rest_T_po_P3_Con_CP1_Latency
FC_Rest_T_po_P3_Con_CP2_Latency<- df$FC_Rest_T_po_P3_Con_CP2_Latency
FC_Rest_T_po_P3_Con_P1_Latency<- df$FC_Rest_T_po_P3_Con_P1_Latency
FC_Rest_T_po_P3_Con_PZ_Latency<- df$FC_Rest_T_po_P3_Con_PZ_Latency
FC_Rest_T_po_P3_Con_P2_Latency<- df$FC_Rest_T_po_P3_Con_P2_Latency
FC_Rest_T_po_P3_Con_C1_Latency<- df$FC_Rest_T_po_P3_Con_C1_Latency
FC_Rest_T_po_P3_Con_CZ_Latency<- df$FC_Rest_T_po_P3_Con_CZ_Latency
FC_Rest_T_po_P3_Con_C2_Latency<- df$FC_Rest_T_po_P3_Con_C2_Latency
#create dataframe 
Rest_T_po_Con_P3_QC.df<-data.frame(SubjectID, FC_Rest_T_po_P3_Con_CPZ_Latency,FC_Rest_T_po_P3_Con_CP1_Latency,FC_Rest_T_po_P3_Con_CP2_Latency, FC_Rest_T_po_P3_Con_P1_Latency, 
                                   FC_Rest_T_po_P3_Con_PZ_Latency, FC_Rest_T_po_P3_Con_P2_Latency,FC_Rest_T_po_P3_Con_C1_Latency,FC_Rest_T_po_P3_Con_CZ_Latency,FC_Rest_T_po_P3_Con_C2_Latency)
#melt dataframe
longformat.Rest_T_po_Con_P3_QC.df <- melt(Rest_T_po_Con_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.Rest_T_po_Con_P3_QC.df)
#computation
ggwithinstats(data = longformat.Rest_T_po_Con_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at Rest_T Pre Incongruent
SubjectID<-df$SubjectID
FC_Rest_T_pre_P3_Inc_CPZ_Latency<- df$FC_Rest_T_pre_P3_Inc_CPZ_Latency
FC_Rest_T_pre_P3_Inc_CP1_Latency<- df$FC_Rest_T_pre_P3_Inc_CP1_Latency
FC_Rest_T_pre_P3_Inc_CP2_Latency<- df$FC_Rest_T_pre_P3_Inc_CP2_Latency
FC_Rest_T_pre_P3_Inc_P1_Latency<- df$FC_Rest_T_pre_P3_Inc_P1_Latency
FC_Rest_T_pre_P3_Inc_PZ_Latency<- df$FC_Rest_T_pre_P3_Inc_PZ_Latency
FC_Rest_T_pre_P3_Inc_P2_Latency<- df$FC_Rest_T_pre_P3_Inc_P2_Latency
FC_Rest_T_pre_P3_Inc_C1_Latency<- df$FC_Rest_T_pre_P3_Inc_C1_Latency
FC_Rest_T_pre_P3_Inc_CZ_Latency<- df$FC_Rest_T_pre_P3_Inc_CZ_Latency
FC_Rest_T_pre_P3_Inc_C2_Latency<- df$FC_Rest_T_pre_P3_Inc_C2_Latency
#create dataframe 
Rest_T_pre_Inc_P3_QC.df<-data.frame(SubjectID, FC_Rest_T_pre_P3_Inc_CPZ_Latency,FC_Rest_T_pre_P3_Inc_CP1_Latency,FC_Rest_T_pre_P3_Inc_CP2_Latency, FC_Rest_T_pre_P3_Inc_P1_Latency, FC_Rest_T_pre_P3_Inc_PZ_Latency, FC_Rest_T_pre_P3_Inc_P2_Latency,FC_Rest_T_pre_P3_Inc_C1_Latency,FC_Rest_T_pre_P3_Inc_CZ_Latency,FC_Rest_T_pre_P3_Inc_C2_Latency)
#melt dataframe
longformat.Rest_T_pre_Inc_P3_QC.df <- melt(Rest_T_pre_Inc_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.Rest_T_pre_Inc_P3_QC.df)
#computation
ggwithinstats(data = longformat.Rest_T_pre_Inc_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at Rest_T Po Incongruent
SubjectID<-df$SubjectID
FC_Rest_T_po_P3_Inc_CPZ_Latency<- df$FC_Rest_T_po_P3_Inc_CPZ_Latency
FC_Rest_T_po_P3_Inc_CP1_Latency<- df$FC_Rest_T_po_P3_Inc_CP1_Latency
FC_Rest_T_po_P3_Inc_CP2_Latency<- df$FC_Rest_T_po_P3_Inc_CP2_Latency
FC_Rest_T_po_P3_Inc_P1_Latency<- df$FC_Rest_T_po_P3_Inc_P1_Latency
FC_Rest_T_po_P3_Inc_PZ_Latency<- df$FC_Rest_T_po_P3_Inc_PZ_Latency
FC_Rest_T_po_P3_Inc_P2_Latency<- df$FC_Rest_T_po_P3_Inc_P2_Latency
FC_Rest_T_po_P3_Inc_C1_Latency<- df$FC_Rest_T_po_P3_Inc_C1_Latency
FC_Rest_T_po_P3_Inc_CZ_Latency<- df$FC_Rest_T_po_P3_Inc_CZ_Latency
FC_Rest_T_po_P3_Inc_C2_Latency<- df$FC_Rest_T_po_P3_Inc_C2_Latency
#create dataframe 
Rest_T_po_Inc_P3_QC.df<-data.frame(SubjectID, FC_Rest_T_po_P3_Inc_CPZ_Latency,FC_Rest_T_po_P3_Inc_CP1_Latency,FC_Rest_T_po_P3_Inc_CP2_Latency, FC_Rest_T_po_P3_Inc_P1_Latency, 
                                   FC_Rest_T_po_P3_Inc_PZ_Latency, FC_Rest_T_po_P3_Inc_P2_Latency,FC_Rest_T_po_P3_Inc_C1_Latency,FC_Rest_T_po_P3_Inc_CZ_Latency,FC_Rest_T_po_P3_Inc_C2_Latency)
#melt dataframe
longformat.Rest_T_po_Inc_P3_QC.df <- melt(Rest_T_po_Inc_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.Rest_T_po_Inc_P3_QC.df)
#computation
ggwithinstats(data = longformat.Rest_T_po_Inc_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at T Pre Congruent
SubjectID<-df$SubjectID
FC_T_pre_P3_Con_CPZ_Latency<- df$FC_T_pre_P3_Con_CPZ_Latency
FC_T_pre_P3_Con_CP1_Latency<- df$FC_T_pre_P3_Con_CP1_Latency
FC_T_pre_P3_Con_CP2_Latency<- df$FC_T_pre_P3_Con_CP2_Latency
FC_T_pre_P3_Con_P1_Latency<- df$FC_T_pre_P3_Con_P1_Latency
FC_T_pre_P3_Con_PZ_Latency<- df$FC_T_pre_P3_Con_PZ_Latency
FC_T_pre_P3_Con_P2_Latency<- df$FC_T_pre_P3_Con_P2_Latency
FC_T_pre_P3_Con_C1_Latency<- df$FC_T_pre_P3_Con_C1_Latency
FC_T_pre_P3_Con_CZ_Latency<- df$FC_T_pre_P3_Con_CZ_Latency
FC_T_pre_P3_Con_C2_Latency<- df$FC_T_pre_P3_Con_C2_Latency
#create dataframe 
E_pre_Con_P3_QC.df<-data.frame(SubjectID, FC_T_pre_P3_Con_CPZ_Latency,FC_T_pre_P3_Con_CP1_Latency,FC_T_pre_P3_Con_CP2_Latency, FC_T_pre_P3_Con_P1_Latency, FC_T_pre_P3_Con_PZ_Latency, FC_T_pre_P3_Con_P2_Latency,FC_T_pre_P3_Con_C1_Latency,FC_T_pre_P3_Con_CZ_Latency,FC_T_pre_P3_Con_C2_Latency)
#melt dataframe
longformat.E_pre_Con_P3_QC.df <- melt(E_pre_Con_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.E_pre_Con_P3_QC.df)
#computation
ggwithinstats(data = longformat.E_pre_Con_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at T Po Congruent
SubjectID<-df$SubjectID
FC_T_po_P3_Con_CPZ_Latency<- df$FC_T_po_P3_Con_CPZ_Latency
FC_T_po_P3_Con_CP1_Latency<- df$FC_T_po_P3_Con_CP1_Latency
FC_T_po_P3_Con_CP2_Latency<- df$FC_T_po_P3_Con_CP2_Latency
FC_T_po_P3_Con_P1_Latency<- df$FC_T_po_P3_Con_P1_Latency
FC_T_po_P3_Con_PZ_Latency<- df$FC_T_po_P3_Con_PZ_Latency
FC_T_po_P3_Con_P2_Latency<- df$FC_T_po_P3_Con_P2_Latency
FC_T_po_P3_Con_C1_Latency<- df$FC_T_po_P3_Con_C1_Latency
FC_T_po_P3_Con_CZ_Latency<- df$FC_T_po_P3_Con_CZ_Latency
FC_T_po_P3_Con_C2_Latency<- df$FC_T_po_P3_Con_C2_Latency
#create dataframe 
E_po_Con_P3_QC.df<-data.frame(SubjectID, FC_T_po_P3_Con_CPZ_Latency,FC_T_po_P3_Con_CP1_Latency,FC_T_po_P3_Con_CP2_Latency, FC_T_po_P3_Con_P1_Latency, 
                              FC_T_po_P3_Con_PZ_Latency, FC_T_po_P3_Con_P2_Latency,FC_T_po_P3_Con_C1_Latency,FC_T_po_P3_Con_CZ_Latency,FC_T_po_P3_Con_C2_Latency)
#melt dataframe
longformat.E_po_Con_P3_QC.df <- melt(E_po_Con_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.E_po_Con_P3_QC.df)
#computation
ggwithinstats(data = longformat.E_po_Con_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at T Pre Incongruent
SubjectID<-df$SubjectID
FC_T_pre_P3_Inc_CPZ_Latency<- df$FC_T_pre_P3_Inc_CPZ_Latency
FC_T_pre_P3_Inc_CP1_Latency<- df$FC_T_pre_P3_Inc_CP1_Latency
FC_T_pre_P3_Inc_CP2_Latency<- df$FC_T_pre_P3_Inc_CP2_Latency
FC_T_pre_P3_Inc_P1_Latency<- df$FC_T_pre_P3_Inc_P1_Latency
FC_T_pre_P3_Inc_PZ_Latency<- df$FC_T_pre_P3_Inc_PZ_Latency
FC_T_pre_P3_Inc_P2_Latency<- df$FC_T_pre_P3_Inc_P2_Latency
FC_T_pre_P3_Inc_C1_Latency<- df$FC_T_pre_P3_Inc_C1_Latency
FC_T_pre_P3_Inc_CZ_Latency<- df$FC_T_pre_P3_Inc_CZ_Latency
FC_T_pre_P3_Inc_C2_Latency<- df$FC_T_pre_P3_Inc_C2_Latency
#create dataframe 
E_pre_Inc_P3_QC.df<-data.frame(SubjectID, FC_T_pre_P3_Inc_CPZ_Latency,FC_T_pre_P3_Inc_CP1_Latency,FC_T_pre_P3_Inc_CP2_Latency, FC_T_pre_P3_Inc_P1_Latency, FC_T_pre_P3_Inc_PZ_Latency, FC_T_pre_P3_Inc_P2_Latency,FC_T_pre_P3_Inc_C1_Latency,FC_T_pre_P3_Inc_CZ_Latency,FC_T_pre_P3_Inc_C2_Latency)
#melt dataframe
longformat.E_pre_Inc_P3_QC.df <- melt(E_pre_Inc_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.E_pre_Inc_P3_QC.df)
#computation
ggwithinstats(data = longformat.E_pre_Inc_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")

#Call Variables at T Po Incongruent
SubjectID<-df$SubjectID
FC_T_po_P3_Inc_CPZ_Latency<- df$FC_T_po_P3_Inc_CPZ_Latency
FC_T_po_P3_Inc_CP1_Latency<- df$FC_T_po_P3_Inc_CP1_Latency
FC_T_po_P3_Inc_CP2_Latency<- df$FC_T_po_P3_Inc_CP2_Latency
FC_T_po_P3_Inc_P1_Latency<- df$FC_T_po_P3_Inc_P1_Latency
FC_T_po_P3_Inc_PZ_Latency<- df$FC_T_po_P3_Inc_PZ_Latency
FC_T_po_P3_Inc_P2_Latency<- df$FC_T_po_P3_Inc_P2_Latency
FC_T_po_P3_Inc_C1_Latency<- df$FC_T_po_P3_Inc_C1_Latency
FC_T_po_P3_Inc_CZ_Latency<- df$FC_T_po_P3_Inc_CZ_Latency
FC_T_po_P3_Inc_C2_Latency<- df$FC_T_po_P3_Inc_C2_Latency
#create dataframe 
E_po_Inc_P3_QC.df<-data.frame(SubjectID, FC_T_po_P3_Inc_CPZ_Latency,FC_T_po_P3_Inc_CP1_Latency,FC_T_po_P3_Inc_CP2_Latency, FC_T_po_P3_Inc_P1_Latency, 
                              FC_T_po_P3_Inc_PZ_Latency, FC_T_po_P3_Inc_P2_Latency,FC_T_po_P3_Inc_C1_Latency,FC_T_po_P3_Inc_CZ_Latency,FC_T_po_P3_Inc_C2_Latency)
#melt dataframe
longformat.E_po_Inc_P3_QC.df <- melt(E_po_Inc_P3_QC.df, id = "SubjectID", variable.name = "Condition")
view(longformat.E_po_Inc_P3_QC.df)
#computation
ggwithinstats(data = longformat.E_po_Inc_P3_QC.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni")




# Exploratory Analyses - MEDIATION - COGNITIVE OUTCOME - post exercise theta EC & post exercise congruent flanker RT--------
#M1 Mediation - X = intervention group, M = EC Theta Post_E, Y = post intervention Congruent Flanker RT
#Call Variables
SubjectID <- (df$SubjectID)
b_rand_string<-df$b_rand_string
SAya_FL_Congruent_MeanRT_E_post <- (df$ SAya_FL_Congruent_MeanRT_E_post)
SAya_FL_Congruent_MeanRT_Rest_E_post <- (df$ SAya_FL_Congruent_MeanRT_Rest_E_post)
Wide.ROI_FC_E_post_Theta.df <-data.frame(F1_Theta_EC_E_Post, FZ_Theta_EC_E_Post, F2_Theta_EC_E_Post, FC1_Theta_EC_E_Post, FCZ_Theta_EC_E_Post, FC2_Theta_EC_E_Post, C1_Theta_EC_E_Post, CZ_Theta_EC_E_Post, C2_Theta_EC_E_Post)
Wide.ROI_FC_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_E_post_Theta.df)
Wide.ROI_FC_Rest_E_post_Theta.df <-data.frame(F1_Theta_EC_Rest_E_Post, FZ_Theta_EC_Rest_E_Post, F2_Theta_EC_Rest_E_Post, FC1_Theta_EC_Rest_E_Post, FCZ_Theta_EC_Rest_E_Post, FC2_Theta_EC_Rest_E_Post, C1_Theta_EC_Rest_E_Post, CZ_Theta_EC_Rest_E_Post, C2_Theta_EC_Rest_E_Post)
Wide.ROI_FC_Rest_E_post_Theta.mean <- rowMeans(Wide.ROI_FC_Rest_E_post_Theta.df)
#create dataframe 
postRT_Congruent_Theta_Model1_Mediation.df<-data.frame(SubjectID, b_rand_string, SAya_FL_Congruent_MeanRT_E_post, SAya_FL_Congruent_MeanRT_Rest_E_post, Wide.ROI_FC_E_post_Theta.mean, Wide.ROI_FC_Rest_E_post_Theta.mean )
#export for manual manipulation to prepare for mediation 
write_xlsx(postRT_Congruent_Theta_Model1_Mediation.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postRT_Congruent_Theta_Model1_Mediation.df.xlsx') #export long format df
longformat.postRT_Congruent_Theta_Model1_Mediation.df <- read_excel("/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//postRT_Congruent_Theta_Model1_Mediation.df.edited.xlsx") #reupload new df
View(longformat.postRT_Congruent_Theta_Model1_Mediation.df)
#mediation computation
model.M.postRT_Congruent_Theta_Model1_Mediation.df<- lm(Post_Theta~Intervention, longformat.postRT_Congruent_Theta_Model1_Mediation.df)
model.Y.postRT_Congruent_Theta_Model1_Mediation.df<- lm(Post_RT~Intervention + Post_Theta, longformat.postRT_Congruent_Theta_Model1_Mediation.df)
set.seed(123)
results.postRT_Congruent_Theta_Model1_Mediation.df<- mediate(model.M.postRT_Congruent_Theta_Model1_Mediation.df, model.Y.postRT_Congruent_Theta_Model1_Mediation.df, treat = 'Intervention', mediator = 'Post_Theta', boot = TRUE, sims = 1000)
summary(results.postRT_Congruent_Theta_Model1_Mediation.df)

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
pr_E_Congruent <-data.frame(CPZ_pr_E_Congruent, CP1_pr_E_Congruent, CP2_pr_E_Congruent)
pr_E_Congruent <- rowMeans(pr_E_Congruent,c(CPZ_pr_E_Congruent, CP1_pr_E_Congruent, CP2_pr_E_Congruent))
pr_E_Incongruent<-data.frame(CPZ_pr_E_Incongruent, CP1_pr_E_Incongruent, CP2_pr_E_Incongruent)
pr_E_Incongruent <- rowMeans(pr_E_Incongruent,c(CPZ_pr_E_Incongruent, CP1_pr_E_Incongruent, CP2_pr_E_Incongruent))
po_E_Congruent <- data.frame(CPZ_po_E_Congruent, CP1_po_E_Congruent, CP2_po_E_Congruent)
po_E_Congruent <- rowMeans(po_E_Congruent,c(CPZ_po_E_Congruent, CP1_po_E_Congruent, CP2_po_E_Congruent))
po_E_Incongruent <- data.frame(CPZ_po_E_Incongruent, CP1_po_E_Incongruent, CP2_po_E_Incongruent)
po_E_Incongruent <- rowMeans(po_E_Incongruent,c(CPZ_po_E_Incongruent, CP1_po_E_Incongruent, CP2_po_E_Incongruent))
pr_Rest_E_Congruent <-data.frame(CPZ_pr_Rest_E_Congruent, CP1_pr_Rest_E_Congruent, CP2_pr_Rest_E_Congruent)
pr_Rest_E_Congruent <- rowMeans(pr_Rest_E_Congruent,c(CPZ_pr_Rest_E_Congruent, CP1_pr_Rest_E_Congruent, CP2_pr_Rest_E_Congruent))
pr_Rest_E_Incongruent <- data.frame(CPZ_pr_Rest_E_Incongruent, CP1_pr_Rest_E_Incongruent, CP2_pr_Rest_E_Incongruent)
pr_Rest_E_Incongruent <- rowMeans(pr_Rest_E_Incongruent,c(CPZ_pr_Rest_E_Incongruent, CP1_pr_Rest_E_Incongruent, CP2_pr_Rest_E_Incongruent))
po_Rest_E_Congruent <- data.frame(CPZ_po_Rest_E_Congruent, CP1_po_Rest_E_Congruent, CP2_po_Rest_E_Congruent)
po_Rest_E_Congruent <- rowMeans(po_Rest_E_Congruent,c(CPZ_po_Rest_E_Congruent, CP1_po_Rest_E_Congruent, CP2_po_Rest_E_Congruent))
po_Rest_E_Incongruent <- data.frame(CPZ_po_Rest_E_Incongruent, CP1_po_Rest_E_Incongruent, CP2_po_Rest_E_Incongruent)
po_Rest_E_Incongruent <- rowMeans(po_Rest_E_Incongruent,c(CPZ_po_Rest_E_Incongruent, CP1_po_Rest_E_Incongruent, CP2_po_Rest_E_Incongruent))

ROI.Narrow.df <- data.frame(time, pr_E_Congruent, po_E_Congruent, pr_E_Incongruent, po_E_Incongruent,  
                            pr_Rest_E_Congruent, po_Rest_E_Congruent, pr_Rest_E_Incongruent, po_Rest_E_Incongruent)
longformat.ROI.Narrow.df<- melt(ROI.Narrow.df , id = "time", variable.name = "Condition", variable.type = numeric)
longformat.ROI.Narrow.df$value<-as.numeric(as.character(longformat.ROI.Narrow.df$value))

ggplot(longformat.ROI.Narrow.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(13, -5)+
  xlim(-200, 1000)+
  xlab("Latency (ms)")+
  ylab("Amplitude (mV)")+
  geom_line(linewidth = .75)+
  ggtitle("Flanker ERP: Exercise vs. Rest")+
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
pr_T_Congruent <-data.frame(CPZ_pr_T_Congruent, CP1_pr_T_Congruent, CP2_pr_T_Congruent)
pr_T_Congruent <- rowMeans(pr_T_Congruent,c(CPZ_pr_T_Congruent, CP1_pr_T_Congruent, CP2_pr_T_Congruent))
pr_T_Incongruent<-data.frame(CPZ_pr_T_Incongruent, CP1_pr_T_Incongruent, CP2_pr_T_Incongruent)
pr_T_Incongruent <- rowMeans(pr_T_Incongruent,c(CPZ_pr_T_Incongruent, CP1_pr_T_Incongruent, CP2_pr_T_Incongruent))
po_T_Congruent <- data.frame(CPZ_po_T_Congruent, CP1_po_T_Congruent, CP2_po_T_Congruent)
po_T_Congruent <- rowMeans(po_T_Congruent,c(CPZ_po_T_Congruent, CP1_po_T_Congruent, CP2_po_T_Congruent))
po_T_Incongruent <- data.frame(CPZ_po_T_Incongruent, CP1_po_T_Incongruent, CP2_po_T_Incongruent)
po_T_Incongruent <- rowMeans(po_T_Incongruent,c(CPZ_po_T_Incongruent, CP1_po_T_Incongruent, CP2_po_T_Incongruent))
pr_Rest_T_Congruent <-data.frame(CPZ_pr_Rest_T_Congruent, CP1_pr_Rest_T_Congruent, CP2_pr_Rest_T_Congruent)
pr_Rest_T_Congruent <- rowMeans(pr_Rest_T_Congruent,c(CPZ_pr_Rest_T_Congruent, CP1_pr_Rest_T_Congruent, CP2_pr_Rest_T_Congruent))
pr_Rest_T_Incongruent <- data.frame(CPZ_pr_Rest_T_Incongruent, CP1_pr_Rest_T_Incongruent, CP2_pr_Rest_T_Incongruent)
pr_Rest_T_Incongruent <- rowMeans(pr_Rest_T_Incongruent,c(CPZ_pr_Rest_T_Incongruent, CP1_pr_Rest_T_Incongruent, CP2_pr_Rest_T_Incongruent))
po_Rest_T_Congruent <- data.frame(CPZ_po_Rest_T_Congruent, CP1_po_Rest_T_Congruent, CP2_po_Rest_T_Congruent)
po_Rest_T_Congruent <- rowMeans(po_Rest_T_Congruent,c(CPZ_po_Rest_T_Congruent, CP1_po_Rest_T_Congruent, CP2_po_Rest_T_Congruent))
po_Rest_T_Incongruent <- data.frame(CPZ_po_Rest_T_Incongruent, CP1_po_Rest_T_Incongruent, CP2_po_Rest_T_Incongruent)
po_Rest_T_Incongruent <- rowMeans(po_Rest_T_Incongruent,c(CPZ_po_Rest_T_Incongruent, CP1_po_Rest_T_Incongruent, CP2_po_Rest_T_Incongruent))

ROI.Narrow.df <- data.frame(time, pr_T_Congruent, po_T_Congruent, pr_T_Incongruent, po_T_Incongruent,  
                            pr_Rest_T_Congruent, po_Rest_T_Congruent, pr_Rest_T_Incongruent, po_Rest_T_Incongruent)
longformat.ROI.Narrow.df<- melt(ROI.Narrow.df , id = "time", variable.name = "Condition", variable.type = numeric)
longformat.ROI.Narrow.df$value<-as.numeric(as.character(longformat.ROI.Narrow.df$value))

ggplot(longformat.ROI.Narrow.df, aes(x=time, y=value, color = Condition))+
  geom_line()+
  ylim(13, -5)+
  xlim(-200, 1000)+
  xlab("Latency (ms)")+
  ylab("Amplitude (mV)")+
  geom_line(linewidth = .75)+
  ggtitle("Flanker ERP: Trier vs. Rest")+
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


# Old stats ------------------------------------------------
#COGNITIVE OUTCOMES STATS


#ggwithinstats(data = longformat.RT_M1.df, x = Condition, y = value, type = "parametric", p.adjust.method = "bonferroni", ggplot.component = theme(axis.text.x = element_text(angle = 30)), title = "Flanker Response Time in M1: Exercise vs. Rest", ggsignif.args = list(textsize = 2.5, tip_length = 0.01))


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
write_xlsx(longformat.M1_RT.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//longformat.M1_RT.df.xlsx') #export long format df
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
M1_RT.aov <- anova_test(data = longformat.M1_RT.df.edited, dv = Average.RT.ms, wid = SubjectID, within =  Congruency, type = 3)
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
write_xlsx(longformat.M2_RT.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//longformat.M2_RT.df.xlsx') #export long format df
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
write_xlsx(longformat.M1_ACC.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//longformat.M1_ACC.df.xlsx') #export long format df
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
write_xlsx(longformat.M2_ACC.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//longformat.M2_ACC.df.xlsx') #export long format df
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
write_xlsx(longformat.M1_InverseEfficiency.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//longformat.M1_InverseEfficiency.df.xlsx') #export long format df
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
write_xlsx(longformat.M2_InverseEfficiency.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//longformat.M2_InverseEfficiency.df.xlsx') #export long format df
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
write_xlsx(longformat.M1_P3.Amplitude.df, '/Volumes/Data/KatherineM/SNEACY/SNEACY_YA/Dissertation_Data//longformat.M1_P3.Amplitude.df.xlsx') #export long format df
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
































