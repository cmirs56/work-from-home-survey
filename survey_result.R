
# road the libraries and data ---------------------------------------------

library(tidyverse)
library(readr)
library(car)
library(ggplot2)

read.csv.any <- function(text, sep = "", ...) {
  encoding <- as.character(guess_encoding(text)[1,1])
  setting <- as.character(tools::file_ext(text))
  if(sep != "" | !(setting  %in% c("csv", "txt")) ) setting <- "custom"
  separate <- list(csv = ",", txt = "\n", custom = sep)
  result <- read.table(text, sep = separate[[setting]], fileEncoding = encoding, ...)
  return(result)
}
df <- read.csv.any("c:/Users/R/survey_telework/survey_num.csv", header = TRUE, na.strings = c("NA", ""))
df$efficiency <- recode(df$efficiency, "'100' = 3 ; '100~80' =  2 ; '150~100' = 4")
df$efficiency <- as.numeric(df$efficiency)


# basic information of the data -------------------------------------------

str(df)
head(df)
summary(df)


# 정규분포 검정_meaningless b/c the 4 variables are all 서열변수 not 연속변수
# Ho: normal / H1: non-normal 

shapiro.test(df$satisfaction)
shapiro.test(df$efficiency)
shapiro.test(df$cooperation)
shapiro.test(df$privacy)

hist(df$efficiency, breaks = 20)
hist(df$efficiency, freq = FALSE, breaks = 20, main = "Kernel Density Plot of df$harassment")
lines(density(df$efficiency), col = "blue", lwd = 3)

qqnorm(df$efficiency)
qqline(df$efficiency)


# Kruskal Wallis H test ---------------------------------------------------

kruskal.test(df$satisfaction ~ df$days, data = df) 
kruskal.test(df$satisfaction ~ df$affiliate, data = df)
kruskal.test(df$satisfaction ~ df$position, data = df)
kruskal.test(df$satisfaction ~ df$responsibility, data = df)

kruskal.test(df$efficiency ~ df$days, data = df) 
kruskal.test(df$efficiency ~ df$affiliate, data = df)
kruskal.test(df$efficiency ~ df$position, data = df)
kruskal.test(df$efficiency ~ df$responsibility, data = df)

kruskal.test(df$cooperation ~ df$days, data = df) 
kruskal.test(df$cooperation ~ df$affiliate, data = df)
kruskal.test(df$cooperation ~ df$position, data = df)
kruskal.test(df$cooperation ~ df$responsibility, data = df)

kruskal.test(df$privacy ~ df$days, data = df) 
kruskal.test(df$privacy ~ df$affiliate, data = df)
kruskal.test(df$privacy ~ df$position, data = df)
kruskal.test(df$privacy ~ df$responsibility, data = df)



# post test ---------------------------------------------------------------

library(userfriendlyscience)

df_noNAstis <- na.omit(df, cols = satisfaction) 
df_noNAeffi <- na.omit(df, cols = efficiency)
df_noNAcoop <- na.omit(df, cols = cooperation)
df_noNApriv <- na.omit(df, cols = privacy)

posthocTGH(df_noNAstis$days, y = df_noNAstis$satisfaction, method = 'games-howell')
posthocTGH(df_noNAstis$affiliate, y = df_noNAstis$satisfaction, method = 'games-howell')
posthocTGH(df_noNAstis$position, y = df_noNAstis$satisfaction, method = 'games-howell')
posthocTGH(df_noNAstis$responsibility, y = df_noNAstis$satisfaction, method = 'games-howell')

posthocTGH(df_noNAeffi$days, y = df_noNAeffi$efficiency, method = 'games-howell')
posthocTGH(df_noNAeffi$affiliate, y = df_noNAeffi$efficiency, method = 'games-howell')

posthocTGH(df_noNAcoop$days, y = df_noNAcoop$cooperation, method = 'games-howell')

posthocTGH(df_noNAstis$days, y = df_noNAstis$privacy, method = 'games-howell')
posthocTGH(df_noNAstis$affiliate, y = df_noNAstis$privacy, method = 'games-howell')
posthocTGH(df_noNAstis$position, y = df_noNAstis$privacy, method = 'games-howell')
posthocTGH(df_noNAstis$responsibility, y = df_noNAstis$privacy, method = 'games-howell')



# monnBook  ---------------------------------------------------------------

library(moonBook)

?mytable
mytable(privacy ~ ., data = df)


# mean of subsets ---------------------------------------------------------

df_stfc <- df %>% 
  group_by(position, responsibility) %>% 
  summarise(mean_stfc = mean(satisfaction, na.rm = TRUE))
df_stfc

df_prvc <- df %>% 
  group_by(position, responsibility) %>% 
  summarise(mean_prvc = mean(privacy, na.rm = TRUE))
df_prvc

df_effi <- df %>% 
  group_by(position, responsibility) %>% 
  summarise(mean_effi = mean(efficiency, na.rm = TRUE))
df_effi

df_effi_days <- df %>% 
  group_by(days, position, responsibility) %>% 
  summarise(mean_effi = mean(efficiency, na.rm = TRUE))
df_effi_days

df_coop <- df %>% 
  group_by(position, responsibility) %>% 
  summarise(mean_coop = mean(cooperation))
df_coop

df_coop_days <- df %>% 
  group_by(days, position, responsibility) %>% 
  summarise(mean_coop = mean(cooperation))
df_coop_days


# the four major variables ------------------------------------------------

df_all <- tibble(
  `satisfaction` = mean(df_stfc$mean_stfc), 
  `efficiency` = mean(df_effi$mean_effi), 
  `cooperation` = mean(df_coop$mean_coop),
  `privacy` = mean(df_prvc$mean_prvc) 
) # mean of means != mean (this calculation is useless) 
df_all
df_all_t <- t(df_all) # switch row & column 
df_all_t
str(df_all_t)
df_all_df <- as.data.frame(df_all_t)

ggplot(df_all_df, aes([1,1],V1))+
  geom_col(position = "dodge")  # not solved


# level of cooperation & efficiency by position/responsibility ------------

ggplot(df_coop, aes(position, mean_coop, fill = responsibility))+
  geom_col(position = "dodge")

ggplot(df_coop_days, aes(days, mean_coop, fill = position))+
  geom_col(position = "dodge")

ggplot(df_effi, aes(position, mean_effi, fill = responsibility))+
  geom_col(position = "dodge")

ggplot(df_effi_days, aes(days, mean_effi, fill = position))+
  geom_col(position = "dodge")


# the relation between efficiency and cooperation -------------------------

ggplot(df, aes(efficiency, cooperation))+
  geom_point(mapping = aes(color = days, shpae = days))+
  geom_smooth(color = 'black', fill = 'grey')+
  facet_wrap(~position)


# the level of efficiency/cooperation by position, responsibility, --------

ggplot(df_coop_days, aes(days, mean_coop))+
  geom_point(mapping = aes(color = position, shape = responsibility))+
  geom_boxplot()+
  theme_bw()

ggplot(df_effi_days, aes(days, mean_effi))+
  geom_point(mapping = aes(color = position, shape = responsibility))+
  geom_boxplot()+
  theme_bw()


# what should be improved  ------------------------------------------------

df_imprv <- df %>% 
  select(improvement, responsibility) 
df_imprv  

ggplot(df_imprv, aes(improvement, fill = responsibility))+
  geom_bar(position = "dodge")+
  coord_flip()


# correlations between the four variables ---------------------------------

df_narm <- na.omit(df)
cor(df[,c(3:6)],method=c("pearson"))

library(PerformanceAnalytics)
chart.Correlation(df_narm[,c(3:6)],pch=19)
