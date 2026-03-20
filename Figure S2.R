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

data_f2=read.xlsx("data/Figure S2.xlsx")
##### creating new variable
data_f2 = data_f2 %>%
  mutate(mic_gam_pos = ifelse(mic_pos == "Positive" & mic_gam_expert== "Negative", "Asexual Positive only",
                              ifelse(mic_gam_expert == "Positive" & mic_pos== "Negative", "Gameteocyte Positive only",
                                     ifelse(mic_pos == "Positive" & mic_gam_expert== "Positive", "Asexual and Gameteocyte Positive",NA))))


data_f2 <- data_f2 %>%
  filter(!is.na(mic_gam_pos))
###
cor.test(
  data_f2$asexual_parasite_expert,
  data_f2$pf_par_ul,
  method = "spearman",
  use = "complete.obs"
)
# Create the ggplot scatter plot with NA values excluded
Figuree_S2 <- ggplot(data_f2, aes(x = log10(count_asexual_parasite_expert + 1), y = log10(pf_par_ul + 1), color = mic_gam_pos, size = 1)) +
  geom_point(size = 1,alpha = 0.5) +
  scale_color_manual(values = c("Asexual Positive only" = "red", "Gameteocyte Positive only" = "#0072b5","Asexual and Gameteocyte Positive"="#BC3C29FF")) +
  xlab("Microscopic parasitemia, Log10") +
  ylab("Pf18S qPCR parasitemia, Log10") +
  theme_pubr() +
  theme(axis.line = element_line(size = 0.25),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.direction = "horizontal",
        legend.justification = c(0.5, 1),
        legend.box.just = "top",
  ) +
  theme(
    axis.ticks = element_line(size = 0.1),  
    axis.ticks.length = unit(0.05, "cm"), 
    axis.line = element_line(size = 0.15),
    axis.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),  
    axis.text = element_text(size=8),
    legend.title = element_text(size=8),
    strip.background = element_blank(),
    strip.text = element_blank()) +
  scale_x_continuous(limits = c(0, 6), breaks = c(0, 1, 2, 3, 5, 6)) +
  scale_y_continuous(limits = c(0, 6), breaks = c(0, 1, 2, 3, 4, 5, 6)) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +   
  guides(color = guide_legend(override.aes = list(size = 1))) +
  annotate("text", x = 0, y = 6, 
           label = "ρ=0.87, p<0.001", 
           hjust = 0, vjust = 1, 
           size = 4, color = "black")

Figuree_S2