

# road the libraries and data ---------------------------------------------

library(tidyverse)
library(readr)
library(car)
read.csv.any <- function(text, sep = "", ...) {
  encoding <- as.character(guess_encoding(text)[1,1])
  setting <- as.character(tools::file_ext(text))
  if(sep != "" | !(setting  %in% c("csv", "txt")) ) setting <- "custom"
  separate <- list(csv = ",", txt = "\n", custom = sep)
  result <- read.table(text, sep = separate[[setting]], fileEncoding = encoding, ...)
  return(result)
}
df <- read.csv.any("c:/Users/MC/R/survey_telework/survey_num.csv", header = TRUE, na.strings = c("NA", ""))
df$efficiency <- recode(df$efficiency, "'100' = 3 ; '100~80' =  2 ; '150~100' = 4")
df$efficiency <- as.numeric(df$efficiency)

#### outline of the dataframe  
str(df)
head(df)
summary(df)

#### mean subset
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

#### overall 
df_all <- tibble(
  `satisfaction` = mean(df_stfc$mean_stfc), 
  `efficiency` = mean(df_effi$mean_effi), 
  `cooperation` = mean(df_coop$mean_coop),
  `privacy` = mean(df_prvc$mean_prvc) 
)
df_all
df_all_t <- t(df_all) #switching_row & column 
df_all_t
str(df_all_t)
df_all_df <- as.data.frame(df_all_t)

ggplot(df_all_df, aes([1,1],V1))+
  geom_col(position = "dodge")     #unsolved

#### mean_coop/effi & position & responsibility 
ggplot(df_coop, aes(position, mean_coop, fill = responsibility))+
  geom_col(position = "dodge")

ggplot(df_coop_days, aes(days, mean_coop, fill = position))+
  geom_col(position = "dodge")

ggplot(df_effi, aes(position, mean_effi, fill = responsibility))+
  geom_col(position = "dodge")

ggplot(df_effi_days, aes(days, mean_effi, fill = position))+
  geom_col(position = "dodge")

#### the relation between efficiency and cooperation 
ggplot(df, aes(efficiency, cooperation))+
  geom_point(mapping = aes(color = days, shpae = days))+
  geom_smooth(color = 'black', fill = 'grey')+
  facet_wrap(~position)

#### days & position & respinsibility & efficiency/cooperation
ggplot(df_coop_days, aes(days, mean_coop))+
  geom_point(mapping = aes(color = position, shape = responsibility))+
  geom_boxplot()+
  theme_bw()

ggplot(df_effi_days, aes(days, mean_effi))+
  geom_point(mapping = aes(color = position, shape = responsibility))+
  geom_boxplot()+
  theme_bw()




#### improvement 
df_imprv <- df %>% 
  select(improvement, responsibility) 
df_imprv  

ggplot(df_imprv, aes(improvement, fill = responsibility))+
  geom_bar(position = "dodge")+
  coord_flip()

# 상관분석
df_narm <- na.omit(df)
cor(df[,c(3:6)],method=c("pearson"))

library(PerformanceAnalytics)
chart.Correlation(df_narm[,c(3:6)],pch=19)

df_0days <- df %>% 
filter(df$days == "0일")
df_0days
nrow(df_0days)           
