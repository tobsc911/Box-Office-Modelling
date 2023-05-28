library(dplyr)
library(tidyverse)
library(forecast)
library(ggplot2)
library(strex)

MovieData<-read.csv("/Downloads/BoxOffice2010.csv", header = TRUE, stringsAsFactors = FALSE)
df <- MovieData[, c(1, 2, 3, 4:31)]
view(df)

df_long <- df %>%
  pivot_longer(cols = 4:31, names_to = "week", values_to = "revenue")
df_long$week <- str_extract_numbers(df_long$week) 
str(df_long$week)
View(df_long)
df_long$RevenueX <- c(df_long$revenue[-28], 0)
df_long$RevenueX[seq(28, nrow(df_long), by=28)] <- 0
df_long$RevenueY <- c(0, df_long$revenue[-1])
ggplot(df_long, aes(x = as.numeric(week), y = revenue, color = Movie_Name)) +
  geom_line() +
  labs(x = "Week", y = "Revenue", title = "Box Office Revenue 2010 by Movie")

# Time series plot for G-rated movies
df_g <- df_long %>%
  filter(Rating == "G")
ggplot(df_g, aes(x = as.numeric(week), y = revenue, color = Movie_Name)) +
  geom_line() +
  labs(x = "Week", y = "Revenue", title = "Box Office Revenue 2010 for G-rated Movies")

# Time series plot for PG-rated movies
df_PG <- df_long %>%
  filter(Rating == "PG")
ggplot(df_PG, aes(x = as.numeric(week), y = revenue, color = Movie_Name)) +
  geom_line() +
  labs(x = "Week", y = "Revenue", title = "Box Office Revenue 2010 for PG-rated Movies")

# Time series plot for G-rated movies
df_PG13 <- df_long %>%
  filter(Rating == "PG-13")
ggplot(df_PG13, aes(x = as.numeric(week), y = revenue, color = Movie_Name)) +
  geom_line() +
  labs(x = "Week", y = "Revenue", title = "Box Office Revenue 2010 for PG-13 rated Movies")

# Time series plot for G-rated movies
df_R <- df_long %>%
  filter(Rating == "R")
ggplot(df_R, aes(x = as.numeric(week), y = revenue, color = Movie_Name)) +
  geom_line() +
  labs(x = "Week", y = "Revenue", title = "Box Office Revenue 2010 for R-rated Movies")

# Time series plot by Ratingss
ggplot(df_long, aes(x = as.numeric(week), y = revenue, color = Movie_Name)) +
  geom_line() +
  facet_wrap(~ Rating) +
  labs(x = "Week", y = "Revenue", title = "Box Office Revenue 2010 by Movie Rating")

# Time series plot for movies released in Fall
df_Fall <- df_long %>%
  filter(Season == "Fall")
ggplot(df_Fall, aes(x = as.numeric(week), y = revenue, color = Movie_Name)) +
  geom_line() +
  labs(x = "Week", y = "Revenue", title = "Box Office Revenue 2010 for Fall Movies")

# Time series plot for movies released in Winter
df_Winter <- df_long %>%
  filter(Season == "Winter")
ggplot(df_Winter, aes(x = as.numeric(week), y = revenue, color = Movie_Name)) +
  geom_line() +
  labs(x = "Week", y = "Revenue", title = "Box Office Revenue 2010 for Winter Movies")

# Time series plot for movies released in Spring
df_Spring <- df_long %>%
  filter(Season == "Spring")
ggplot(df_Spring, aes(x = as.numeric(week), y = revenue, color = Movie_Name)) +
  geom_line() +
  labs(x = "Week", y = "Revenue", title = "Box Office Revenue 2010 for Spring Movies")

# Time series plot for movies released in Summer
df_Summer <- df_long %>%
  filter(Season == "Summer")
ggplot(df_Summer, aes(x = as.numeric(week), y = revenue, color = Movie_Name)) +
  geom_line() +
  labs(x = "Week", y = "Revenue", title = "Box Office Revenue 2010 for Summer Movies")

# Time series plot by Seasons
ggplot(df_long, aes(x = as.numeric(week), y = revenue, color = Movie_Name)) +
  geom_line() +
  facet_wrap(~ Season) +
  labs(x = "Week", y = "Revenue", title = "Box Office Revenue 2010 by Season")

ggplot(df_long, aes(x = as.numeric(week))) +
  geom_line(aes(y = RevenueX, color = Movie_Name), linetype = "solid") +
  geom_line(aes(y = RevenueY, color = Movie_Name), linetype = "dotted") +
  labs(x = "Week", y = "Revenue", title = "Box Office Revenue 2010 by Movie") +
  scale_color_discrete(name = "Movie Name")