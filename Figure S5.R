# library
library(readxl)
library(tidyverse)
library(ggprism)
library(ggpubr)
library(gridExtra)
library(ggplot2)
library(ggsignif)
library(rstatix)
library(foreign)
library(haven)
library(dplyr)
## data
####Supplemental figure 5: Fever Clearance and Anemia Recovery Rates During Follow-Up

temp <- read_excel("data/Figure S5.xlsx", sheet = "Fever")
temp= temp %>%
  filter(day %in% c("1","2","3","7","14","21","28"))
temp$day= as.factor(temp$day)

temp_update <- ggplot(temp, aes(x = day, y = prop_clearance)) +
  geom_bar(position = "stack", stat = "identity", fill = "#E00") +
  labs(title = "",
       x = "Follow up days",
       y = "Fever clearance (%)") +
 theme(
    axis.line = element_line(size = 0.5),  
    legend.position = "bottom"  
  ) +
  
  theme_pubr() +

  theme(
    # plot.title = element_text(family = "Calibri", size = 10), 
    axis.text.x = element_text(family = "Calibri", size = 10),
    axis.text.y = element_text(family = "Calibri", size = 10),
    axis.title.x = element_text(family = "Calibri", size = 10),
    axis.title.y = element_text(family = "Calibri", size = 10),
    axis.ticks = element_line(size = 0.1),
    axis.ticks.length = unit(0.05, "cm"),
    axis.line = element_line(size = 0.15),
    strip.background = element_blank(),
    legend.position = "none"  # No legend needed
   
  )
temp_update
## homoglobine
hgb= read_excel("data/Figure S5.xlsx", sheet = "Animic")
hgb= hgb %>%
  filter(day_an %in% c("7", "14", "21", "28"))
hgb$day_an= as.factor(hgb$day_an)


# Plotting the proportions with modified x-axis labels
hgbl=ggplot(hgb, aes(x = day_an, y = prop), color="#E00") +
  geom_bar(position = "stack", stat = "identity",fill="#E00") +
  labs(title = "",
       x = "Follow up days",
       y = "Anemia recovery (%)") +
 
  ylim(0,100)+
  theme(
    axis.line = element_line(size = 0.5),  
    legend.position = "bottom"  
  ) +
  theme_pubr() +
  theme(
    # plot.title = element_text(family = "Calibri", size = 10), 
    axis.text.x = element_text(family = "Calibri", size = 10),
    axis.text.y = element_text(family = "Calibri", size = 10),
    axis.title.x = element_text(family = "Calibri", size = 10),
    axis.title.y = element_text(family = "Calibri", size = 10),
    axis.ticks = element_line(size = 0.1),
    axis.ticks.length = unit(0.05, "cm"),
    axis.line = element_line(size = 0.15),
    strip.background = element_blank(),
    legend.position = "none"  
  )
hgbl
ggarrange(temp_update,hgbl)