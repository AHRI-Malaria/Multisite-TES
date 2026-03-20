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
data_f6= read_excel("data/Figure S6")

# site specific data
Abobok13= data_f6 %>% filter(site_tes=="Abobo")
Asayita13= data_f6 %>% filter(site_tes=="Asayita")
Asosak13= data_f6 %>% filter(site_tes=="Assosa")
Maksegnitk13= data_f6 %>% filter(site_tes=="Makesegnit")
Mizank13= data_f6 %>% filter(site_tes=="Mizan")
###### Abobo
# Plot for k13 genotype with missing values

k13_plotab = ggplot(Abobok13, aes(
  x = day,
  y = log10(par_density_avarage),
  group = id,
  color = k13_plot_status
)) +
  # Points for all data (including missing)
  geom_point(alpha = 0.3, size = 1) +
  geom_line(alpha = 0.3, size = 0.2) +

  # Median lines only for non-missing data
  stat_summary(data = subset(Abobok13, k13_plot_status != "Missing"),
               aes(group = k13_plot_status), 
               fun = median, geom = "line", 
               size = 0.7, alpha = 0.8) +
  
  # Errorbars only for non-missing data
  stat_summary(data = subset(Abobok13, k13_plot_status != "Missing"),
               aes(group = k13_plot_status), 
               fun.data = ~data.frame(
                 y = median(.x),
                 ymin = quantile(.x, 0.25),
                 ymax = quantile(.x, 0.75)
               ), 
               geom = "errorbar", 
               width = 0.2, size = 0.25, alpha = 0.8) +
  
  labs(
    x = "Day",
    y = expression(paste("Asexual parasite density/μL (log10)")),
    color = "PfK13 mutations "
  ) +
  #facet_wrap(~ Site) +
  ggtitle("Abobo") +
  theme_pubr(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid = element_blank()
  ) +
  theme(legend.position = "bottom") +
  scale_color_manual(
    values = c("Wild type" = "#56B4E9", "Mutant" = "#B00", "Missing" = "black"),
    na.value = "black"
  )

k13_plotab
## Asayita
# Plot for k13 genotype with missing values
k13_plotas = ggplot(Asayita13, aes(
  x = day,
  y = log10(par_density_avarage),
  group = id,
  color = k13_plot_status
)) +
  # Points for all data (including missing)
  geom_line(alpha = 0.3, size = 0.2) +
  geom_point(alpha = 0.3, size = 1) +
  
  # Median lines only for non-missing data
  stat_summary(data = subset(Asayita13, k13_plot_status != "Missing"),
               aes(group = k13_plot_status), 
               fun = median, geom = "line", 
               size = 0.7, alpha = 0.8) +
  
  # Errorbars only for non-missing data
  stat_summary(data = subset(Asayita13, k13_plot_status != "Missing"),
               aes(group = k13_plot_status), 
               fun.data = ~data.frame(
                 y = median(.x),
                 ymin = quantile(.x, 0.25),
                 ymax = quantile(.x, 0.75)
               ), 
               geom = "errorbar", 
               width = 0.2, size = 0.25, alpha = 0.8) +
  
  labs(
    x = "Day",
    y = expression(paste("Asexual parasite density/μL (log10)")),
    color = "PfK13 mutations "
  ) +
  #facet_wrap(~ Site) +
  ggtitle("Asayita") +
  theme_pubr(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid = element_blank()
  ) +
  theme(legend.position = "bottom") +
  scale_color_manual(
    values = c("Wild type" = "#56B4E9", "Mutant" = "#B00", "Missing" = "black"),
    na.value = "black"
  )

k13_plotas
## Assosa
# Plot for k13 genotype with missing values
k13_plotass = ggplot(Asosak13, aes(
  x = day,
  y = log10(par_density_avarage),
  group = id,
  color = k13_plot_status
)) +
  # Points for all data (including missing)
  geom_line(alpha = 0.3, size = 0.2) +
  geom_point(alpha = 0.3, size = 1) +
  
  # Median lines only for non-missing data
  stat_summary(data = subset(Asosak13, k13_plot_status != "Missing"),
               aes(group = k13_plot_status), 
               fun = median, geom = "line", 
               size = 0.7, alpha = 0.8) +
  
  # Errorbars only for non-missing data
  stat_summary(data = subset(Asosak13, k13_plot_status != "Missing"),
               aes(group = k13_plot_status), 
               fun.data = ~data.frame(
                 y = median(.x),
                 ymin = quantile(.x, 0.25),
                 ymax = quantile(.x, 0.75)
               ), 
               geom = "errorbar", 
               width = 0.2, size = 0.25, alpha = 0.8) +
  
  labs(
    x = "Day",
    y = expression(paste("Asexual parasite density/μL (log10)")),
    color = "PfK13 mutations "
  ) +
  #facet_wrap(~ Site) +
  ggtitle("ASosa") +
  theme_pubr(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid = element_blank()
  ) +
  theme(legend.position = "bottom") +
  scale_color_manual(
    values = c("Wild type" = "#56B4E9", "Mutant" = "#B00", "Missing" = "black"),
    na.value = "black"
  )

k13_plotass
## Maksegnite
# Plot for k13 genotype with missing values
k13_plotmk = ggplot(Maksegnitk13, aes(
  x = day,
  y = log10(par_density_avarage),
  group = id,
  color = k13_plot_status
)) +
  # Points for all data (including missing)
  geom_line(alpha = 0.3, size = 0.2) +
  geom_point(alpha = 0.3, size = 1) +
  
  # Median lines only for non-missing data
  stat_summary(data = subset(Maksegnitk13, k13_plot_status != "Missing"),
               aes(group = k13_plot_status), 
               fun = median, geom = "line", 
               size = 0.7, alpha = 0.8) +
  
  # Errorbars only for non-missing data
  stat_summary(data = subset(Maksegnitk13, k13_plot_status != "Missing"),
               aes(group = k13_plot_status), 
               fun.data = ~data.frame(
                 y = median(.x),
                 ymin = quantile(.x, 0.25),
                 ymax = quantile(.x, 0.75)
               ), 
               geom = "errorbar", 
               width = 0.2, size = 0.25, alpha = 0.8) +
  
  labs(
    x = "Day",
    y = expression(paste("Asexual parasite density/μL (log10)")),
    color = "PfK13 mutations "
  ) +
  #facet_wrap(~ Site) +
  ggtitle("Maksegnit") +
  theme_pubr(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid = element_blank()
  ) +
  theme(legend.position = "bottom") +
  scale_color_manual(
    values = c("Wild type" = "#56B4E9", "Mutant" = "#B00", "Missing" = "black"),
    na.value = "black"
  )

k13_plotmk
## Mizan
# Plot for k13 genotype with missing values
k13_plotmz = ggplot(Mizank13, aes(
  x = day,
  y = log10(par_density_avarage),
  group = id,
  color = k13_plot_status
)) +
  # Points for all data (including missing)
  geom_line(alpha = 0.3, size = 0.2) +
  geom_point(alpha = 0.3, size = 1) +
  
  # Median lines only for non-missing data
  stat_summary(data = subset(Mizank13, k13_plot_status != "Missing"),
               aes(group = k13_plot_status), 
               fun = median, geom = "line", 
               size = 0.7, alpha = 0.8) +
  
  # Errorbars only for non-missing data
  stat_summary(data = subset(Mizank13, k13_plot_status != "Missing"),
               aes(group = k13_plot_status), 
               fun.data = ~data.frame(
                 y = median(.x),
                 ymin = quantile(.x, 0.25),
                 ymax = quantile(.x, 0.75)
               ), 
               geom = "errorbar", 
               width = 0.2, size = 0.25, alpha = 0.8) +
  
  labs(
    x = "",
    y = expression(paste("Asexual parasite density/μL (log10)")),
    color = "PfK13 mutations "
  ) +
  #facet_wrap(~ Site) +
  ggtitle("Mizan")+
  theme_pubr(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid = element_blank()
  ) +
  theme(legend.position = "bottom") +
  scale_color_manual(
    values = c("Wild type" = "#56B4E9", "Mutant" = "#B00", "Missing" = "black"),
    na.value = "black"
  )

k13_plotmz
#### For All site
##
k13_plotall = ggplot(data_f6, aes(
  x = day,
  y = log10(par_density_avarage),
  group = id,
  color = k13_plot_status
)) +
  # Points for all data (including missing)
  geom_line(alpha = 0.3, size = 0.2) +
  geom_point(alpha = 0.3, size = 1) +
  
  # Median lines only for non-missing data
  stat_summary(data = subset(data_f6, k13_plot_status != "Missing"),
               aes(group = k13_plot_status), 
               fun = median, geom = "line", 
               size = 0.7, alpha = 0.8) +
  
  # Errorbars only for non-missing data
  stat_summary(data = subset(data_f6, k13_plot_status != "Missing"),
               aes(group = k13_plot_status), 
               fun.data = ~data.frame(
                 y = median(.x),
                 ymin = quantile(.x, 0.25),
                 ymax = quantile(.x, 0.75)
               ), 
               geom = "errorbar", 
               width = 0.2, size = 0.25, alpha = 0.8) +
  
  labs(
    x = "",
    y = expression(paste("Asexual parasite density/μL (log10)")),
    color = "PfK13 mutations "
  ) +
  #facet_wrap(~ Site) +
  ggtitle("All site")+
  theme_pubr(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid = element_blank()
  ) +
  theme(legend.position = "bottom") +
  scale_color_manual(
    values = c("Wild type" = "#56B4E9", "Mutant" = "#B00", "Missing" = "black"),
    na.value = "black"
  )

k13_plotall
##
# Install and load patchwork if not already installed
ggarrange(k13_plotall,k13_plotab,k13_plotas,k13_plotass,k13_plotmk,k13_plotmz,common.legend = T)

