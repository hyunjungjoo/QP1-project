library(ggplot2)
library(lme4)
library(lmerTest)
library(tidyr)
library(tidyverse)
data<-read.csv("C:\\Users\\joohj\\OneDrive\\바탕 화면\\Exp_result_new\\collect_data_final.csv")

data$tone <- ifelse(data$key_resp_4.keys=="d",1,0)
data <- data %>%
  separate(sound, c("item1", "item2"), sep="/") %>%
  separate(item2, c("item", "factor", "step"), sep = "_")

data$step <- gsub(".wav", "", data$step)
data$step <- gsub("step", "", data$step)
data <- data[,-c(1:3)]

# exclude an outlier
data<-data[!data$participant %in% c("NF01"),]


## Plot
align<-data[data$factor=="peak",]
shape<-data[data$factor=="shape",]
df <- data[data$factor=="shape"|data$factor=="peak",]

align %>%
  ggplot() +
  aes(x = as.numeric(step), y = tone) +
  stat_summary(fun = mean, geom = "line", color = "blueviolet", size=1.3)+
  stat_summary(fun = mean_cl_normal,  geom = "ribbon", fill = "blueviolet", alpha=.1)+
  stat_summary(fun = mean, geom = "point", size = 5, fill='blueviolet', color = "white",
               stroke =2.3, pch = 21) +
  coord_cartesian(ylim = c(0, 1))+
  theme(axis.text.x = element_text(size=14,color = "black"), 
        axis.text.y = element_text(size=12,color = "black"),
        plot.title = element_text(size=16, face = "bold", hjust=0.5), 
        axis.title.x = element_text(size=14,face="bold"), 
        axis.title.y = element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 13, face = "bold"))+
  labs(x = "Step", y = "Tone") + 
  ggtitle("Alignment") 

shape %>%
  ggplot() +
  aes(x = as.numeric(step), y = tone) +
  stat_summary(fun = mean, geom = "line", color = "violetred1", size=1.3)+
  stat_summary(fun = mean_cl_normal,  geom = "ribbon", fill = "violetred1", alpha=.1)+
  stat_summary(fun = mean, geom = "point", size = 5, fill='violetred1', color = "white",
               stroke =2.3, pch = 21) +
  coord_cartesian(ylim = c(0, 1))+
  theme(axis.text.x = element_text(size=14,color = "black"), 
        axis.text.y = element_text(size=12,color = "black"),
        plot.title = element_text(size=16, face = "bold", hjust=0.5), 
        axis.title.x = element_text(size=14,face="bold"), 
        axis.title.y = element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 13, face = "bold"))+
  labs(x = "Step", y = "Tone") + 
  ggtitle("Rise shape") 

df %>%
  group_by(factor, step) %>%
  summarize(avg = mean(tone),
            sd = sd(tone),
            min = min(tone),
            max = max(tone))

## Stat_01
df_wide <- df %>%
  pivot_wider(names_from = factor, 
              values_from = step)
df_wide$peak <- as.numeric(df_wide$peak)
df_wide$shape <- as.numeric(df_wide$shape)

mod_1 <- glmer(tone ~ peak + (1|participant), family = "binomial", df_wide)
mod_2 <- glmer(tone ~ shape + (1+shape|participant), family = "binomial", df_wide)
summary(mod_1)
summary(mod_2)

## Stat_02
df$step <- as.numeric(df$step)
mod_0 <- glmer (tone ~ factor * step + (1+factor|participant), family = "binomial", data = df)
summary(mod_0)

# df$factor <- as.factor(df$factor)
# df$factor <- relevel(df$factor, ref="shape") 
# mod_3 <- glm(tone~factor*step, family="binomial", data=df)
# summary(mod_3)