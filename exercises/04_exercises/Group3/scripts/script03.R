# Project: The fast and the sorrow
# Author: Timo Roettger
# Date: 12/12/2025
# Group 3


##############
## code book #
##############

## vpX_data            data frames for individual subjects
# var_001:             picture category: sad vs. happy 
# var_002:             unique id of experimental images (n = 64)
# var_003:             date of data collection
# var_004:             assigned list of trials sequence 
# var_005:             dependent variable: words per minute
# var_006              dependent variable: syllables per minute
# var_007:             dependent variable: phonemes per second
# var_008:             sadness rating (1-7)
# vp_id:               unique id of subject (n = 20)

## vp_info
# vp_id:               unique id of subject (n = 20)
# vp_002:              subject age (ranging from 19-31)
# vp_003:              subject gender; levels: M vs. W
# vp_004:              subjects major; levels: phil(osophy), ling(uistics), lit(erature), psych(ology)


if (!require("pacman")) install.packages("pacman")
pacman::p_load(readbulk, rstudioapi, tidyverse, lme4)

current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))                              

raw_data <- read_bulk(directory = "../raw/", 
                      extension = ".csv")                 

subject_info <- read_csv("../data/vp_info.csv")               

data <- full_join(raw_data, subject_info)                  

data <- data[data$vp_002 != "31",]                       

data_trimmed <- data |> 
  group_by(vp_id, var_001) |> 
  mutate(avg = mean(var_007), stdev = sd(var_007)) |> 
  filter(var_007 <= 2 * stdev + avg) |> 
  as.data.frame()

xmdl <- lmer(var_007 ~ var_001 * vp_003 +
               (1 | vp_id),
             data_trimmed) 

xmdl_red <- lmer(var_007 ~ var_001 + vp_003 +
                   (1 | vp_id),
                 data_trimmed) 

model_comp <- 
  anova(xmdl, xmdl_red)

save(xmdl, xmdl_red, model_comp,
     file = "../data/model_output.RData")

Figure <- data |> 
  group_by(var_001, vp_003) |> 
  summarise(mean = mean(var_007)) |> 
  ggplot(aes(x = var_001, y = mean, colour = vp_003, group = vp_003)) +
  geom_point(pch = 15, size = 3) +
  geom_line(lty = c(1,1,2,2)) +
  scale_colour_manual("snt",
                      guide = guide_legend(title = "Subject sex"),
                      values = c("#0072B2", "#D55E00")) +
  scale_y_continuous(expand = c(0, 0), breaks = (c(6.8,7,7.2,7.4,7.6)), limits = c(6.8,7.6)) +
  labs(title = "Speech rate is influenced by mood and gender\n",
       y = "Phonemes per second\n",
       x = "\nMood") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.line = element_blank())

ggsave(filename = "../plots/Figure.png", 
       plot = Figure,
       width = 150, 
       height = 100,
       units = "mm",
       dpi = 300)
