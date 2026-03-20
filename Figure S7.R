# library
library(readxl)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidyverse)
library(survival)
library(survminer)
library(ggprism)
library(ggpubr)
library(gridExtra)
library(pammtools)
library(flextable)
library(ggplot2)
library(ggsignif)
library(readr)
library(stats)
library(rstatix)
library(foreign)
library(gdata)
library(tidyr)
library(ggfortify)
library(ggpubr)
library(dplyr)
library(ggprism)
library(haven)
library(dplyr)
## data
data_f6= read_excel("data/Figure S7")
# Recode the variable
# Or specify factor levels
data_f6$Pfk13_status_622_675_num <- factor(data_f6$Pfk13_status_622_675_num, 
                                           levels = c(0, 1),
                                           labels = c("Wild type", "Mutant"))

# site specific data

data_ab= data_f6 %>% filter(site_tes=="Abobo")
data_as= data_f6 %>% filter(site_tes=="Asayita")
data_ass= data_f6 %>% filter(site_tes=="Assosa")
data_mak= data_f6 %>% filter(site_tes=="Makesegnit")
data_Miz= data_f6 %>% filter(site_tes=="Mizan")

## For All site
###  To estimate an HR, you must use a Cox proportional hazards model
cox_model <- coxph(
  Surv(time, status) ~ Pfk13_status_622_675_num,
  data = data_f6
)

summary(cox_model)


# Base KM plot
km_plotall <- ggsurvplot(
  survfit(Surv(time, status) ~ Pfk13_status_622_675_num, data = data_f6),
  data = data_f6,
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.y.text = F,
  risk.table.y.text.col = T,
  xlab = "",
  ylab = "Probability of parasite positivity",
  title = "All site",
  legend = "none",
  tables.theme = theme_cleantable(),
  ggtheme = theme_pubr(),
  break.x.by = 1,
  xlim = c(0, 7),
  conf.int.alpha = 0.1,
  palette = c( "#56B4E9", "#B00")  
)

# Comprehensive styling of both line and CI
km_plotall$plot <- km_plotall$plot +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 7)) +
  scale_color_manual(values = c( "#56B4E9", "#B00")) +
  scale_fill_manual(values = c( "#56B4E9", "#B00")) +
  annotate("text", x = 3.2, y = 0.7, 
           label = "HR = 0.44 (95% CI:1.0.35-0.57\np<0.0001",
           #label="LR=50.8,p<0.0001",
           size = 4, hjust = 0)

km_plotall

## Abobo
# 1. Primary Cox Model
cox_model_ab <- coxph(Surv(time, status) ~ Pfk13_status_622_675_num, data = data_ab1)
# Key outputs to report:
# - Hazard Ratio (exp(coef)) for log_baseline
# - 95% Confidence Interval
# - p-value

# Test proportional hazards assumption
cox_zph <- cox.zph(cox_model_ab)
print(cox_zph)
plot(cox_zph)
##
#  KM plot
km_plotabk13 <- ggsurvplot(
  survfit(Surv(time, status) ~ Pfk13_status_622_675_num, data = data_ab),
  data = data_ab,
  conf.int = TRUE,
  risk.table = TRUE,
  
  risk.table.y.text = F,
  risk.table.y.text.col = T,
  xlab = "",
  ylab = "Probability of parasite positivity",
  title = "Abobo",
  legend = "none",
  tables.theme = theme_cleantable(),
  ggtheme = theme_pubr(),
  break.x.by = 1,
  xlim = c(0, 7),
  conf.int.alpha = 0.1,
  palette = c( "#56B4E9", "#B00")  
)

# Comprehensive styling of both line and CI
km_plotabk13$plot <- km_plotabk13$plot +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 7)) +
  scale_color_manual(values = c( "#56B4E9", "#B00")) +
  scale_fill_manual(values = c( "#56B4E9", "#B00")) +
  annotate("text", x = 3.2, y = 0.7, 
           label = "HR = 0.36 (95% CI:0.16-0.81)\np=0.013",
           #label="LR=11.3,p<0.0001",
           size = 4, hjust = 0)

km_plotabk13


#### Asayita
# 1. Primary Cox Model
cox_model_ask13 <- coxph(Surv(time, status) ~ Pfk13_status_622_675_num, data = data_as)
# Key outputs to report:
# - Hazard Ratio (exp(coef)) for log_baseline
# - 95% Confidence Interval
# - p-value

# Test proportional hazards assumption
cox_zph <- cox.zph(cox_model_ask13)
print(cox_zph)
plot(cox_zph)
##
km_plotask13 <- ggsurvplot(
  survfit(Surv(time, status) ~ Pfk13_status_622_675_num, data = data_as),
  data = data_as,
  conf.int = TRUE,
  
  risk.table = TRUE,
  risk.table.y.text = F,
  risk.table.y.text.col = T,
  xlab = "",
  ylab = "Probability of parasite positivity",
  title = "Asayita",
  legend = "none",
  tables.theme = theme_cleantable(),
  ggtheme = theme_pubr(),
  break.x.by = 1,
  xlim = c(0, 7),
  conf.int.alpha = 0.08,
  palette = c( "#56B4E9", "#B00")  
)

# Then add the other customizations
km_plotask13$plot <- km_plotask13$plot +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 7)) +
  annotate("text", x = 3.2, y = 0.5, 
           label = "HR = 0.66 (95% CI: 0.38-1.16)\np = 0.149",
           #label="LR=2.4,p=0.100",
           size = 4, hjust = 0)

km_plotask13

#### Asosa
# 1. Primary Cox Model
cox_model_assk13 <- coxph(Surv(time, status) ~ Pfk13_status_622_675_num, data = data_ass)
# Key outputs to report:
# - Hazard Ratio (exp(coef)) for log_baseline
# - 95% Confidence Interval
# - p-value

# Test proportional hazards assumption
cox_zph <- cox.zph(cox_model_ass)
print(cox_zph)
plot(cox_zph)
# Create Kaplan-Meier plot with Cox results and specific x-axis labels
km_plotassk13 <- ggsurvplot(
  survfit(Surv(time, status) ~ Pfk13_status_622_675_num, data = data_ass),  # Overall survival curve
  data = data_ass,
  conf.int = TRUE,
  
  risk.table = TRUE,
  risk.table.y.text = F,
  risk.table.y.text.col = T,
  xlab = "Days ",
  ylab = "Probability of parasite positivity",
  title = "Asosa",
  legend = "none",
  tables.theme = theme_cleantable(),
  ggtheme = theme_pubr(),
  break.x.by = 1, 
  xlim = c(0, 7),  
  conf.int.alpha = 0.08,
  palette = c( "#56B4E9", "#B00")
)

# Customize x-axis to show specific labels
km_plotassk13$plot <- km_plotassk13$plot +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 7)) +
  annotate("text", x = 3.2, y = 0.5, 
           label = "HR = 0.47 (95% CI: 0.28-0.76)\np=0.002",
           #label="LR=8.1,p=0.004",
           size = 4, hjust = 0)

# Display the plot
km_plotassk13
#### Maksegnit
# 1. Primary Cox Model
cox_model_mkk13 <- coxph(Surv(time, status) ~ Pfk13_status_622_675_num, data = data_mak)
summary(cox_model_mkk13)

# Key outputs to report:
# - Hazard Ratio (exp(coef)) for log_baseline
# - 95% Confidence Interval
# - p-value

# Test proportional hazards assumption
cox_zph <- cox.zph(cox_model_mk)
print(cox_zph)
plot(cox_zph)

# Create Kaplan-Meier plot with Cox results and specific x-axis labels
km_plotmkk13 <- ggsurvplot(
  survfit(Surv(time, status) ~ Pfk13_status_622_675_num, data = data_mak),  # Overall survival curve
  data = data_mak,
  risk.table = TRUE,
  risk.table.y.text = F,
  risk.table.y.text.col = T,
  conf.int = TRUE,
  xlab = "Days ",
  ylab = "Probability of parasite positivity",
  title = "Maksegnit",
  legend = "none",
  tables.theme = theme_cleantable(),
  ggtheme = theme_pubr(),
  break.x.by = 1,  
  xlim = c(0, 7) ,  
  conf.int.alpha = 0.08,
  palette = c( "#56B4E9", "#B00")
)

# Customize x-axis to show specific labels
km_plotmkk13$plot <- km_plotmkk13$plot +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 7)) +
  annotate("text", x = 3.2, y = 0.5, 
           label = "HR = 0.29 (95% CI: 0.17-0.49)\np<0.0001",
           #label="LR=23.5,p<0.0001",
           size = 4, hjust = 0)

# Display the plot
km_plotmkk13
#### Mizan
# 1. Primary Cox Model
cox_model_mzk13 <- coxph(Surv(time, status) ~ Pfk13_status_622_675_num, data = data_Miz)

# Key outputs to report:
# - Hazard Ratio (exp(coef)) for log_baseline
# - 95% Confidence Interval
# - p-value

# Test proportional hazards assumption
cox_zph <- cox.zph(cox_model_mz)
print(cox_zph)
plot(cox_zph)
##
# Base KM plot
km_plotmzk13 <- ggsurvplot(
  survfit(Surv(time, status) ~ Pfk13_status_622_675_num, data = data_Miz),  # Overall survival curve
  data = data_Miz,
  risk.table = TRUE,
  risk.table.y.text = F,
  risk.table.y.text.col = T,
  risk.table.title = NULL, 
  #risk.table.col = c("#56B4E9", "#B00"),  # colors for risk table strata
  conf.int = TRUE,
  xlab = "Days",
  ylab = "Probability of parasite positivity",
  title = "Mizan",
  legend = "none",
  # tables.theme = theme_cleantable(),
  tables.theme = theme_cleantable() + 
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),   
          axis.ticks.y = element_blank()), 
  ggtheme = theme_pubr(),
  break.x.by = 1,
  xlim = c(0, 7),
  conf.int.alpha = 0.08,
  palette = c( "#56B4E9", "#B00")
)

# Customize x-axis to show specific labels
km_plotmzk13$plot <- km_plotmzk13$plot +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 7)) +
  annotate("text", x = 3.2, y = 0.6, 
           label = "HR = 0.24 (95% CI: 0.06-1.02)\np=0.054",
           # label="LR=8.5,p=0.003",
           size = 4, hjust = 0)

km_plotmzk13
####
km_plotall$table <- km_plotall$table + labs(title = NULL)
km_plotabk13$table <-km_plotabk13$table + labs(title = NULL)
km_plotask13$table<-  km_plotask13$table + labs(title = NULL)
km_plotassk13$table <- km_plotassk13$table + labs(title = NULL)
km_plotmkk13$table <- km_plotmkk13$table + labs(title = NULL)
km_plotmzk13$table <- km_plotmzk13$table + labs(title = NULL)
####
#ggarrange(km_plotab,km_plotas,km_plotass,km_plotmk,km_plotmz)
ggarrange( km_plotall$plot,
           km_plotabk13$plot,
           km_plotask13$plot,
           km_plotall$table,
           km_plotabk13$table,
           km_plotask13$table,
           km_plotassk13$plot,
           km_plotmkk13$plot,
           km_plotmzk13$plot,
           km_plotassk13$table,
           km_plotmkk13$table,
           km_plotmzk13$table,
           
           ncol = 3,           # or 2 if you prefer side by side
           nrow = 4,
           heights = c(2, 0.5, 2, 0.5),   # plots get twice the height of tables
           align = "v"                    # vertical alignment of panels
)


