#METHDOLOGY AND ANALYSIS
#loading of packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(viridis)
library(wordcloud)
library(RColorBrewer)
library(magrittr)
library(lubridate)
library(RPostgreSQL)
library(plotly)
library(jsonlite)
library(htmltools)
library(glmnet)
library(epitools)
library(lme4)
library(sjPlot)
library(pscl)
#loading of dataset
VIDEOGS <- read.csv("D:/john/Videogs_2016.csv")
VIDEOGS

head(VIDEOGS)
glimpse(VIDEOGS)
summary(VIDEOGS)
names(VIDEOGS)
dim(VIDEOGS)
#class of some variables
class(VIDEOGS$Name)
class(VIDEOGS$Platform)
class(VIDEOGS$Publisher)
class(VIDEOGS$Year_of_Release)
class(VIDEOGS$Genre)

#load package
library(psych)
describe(VIDEOGS)
VIDEOGS%>%
  group_by(Genre) %>%
  summarise(Count = n()) %>%
  plot_ly(x = ~Genre,
          y = ~Count,
          type = "bar", col="black")
VIDEOGS%>%
  group_by(Platform) %>%
  summarise(Count = n()) %>%
  plot_ly(x = ~Platform,
          y = ~Count,
          type = "bar")
VIDEOGS %>%
  gather("Region", "Value", c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")) %>%
  group_by(Region, Name) %>%
  summarize(Sales = sum(Value)) %>%
  top_n(n = 3) %>%
  ggplot(aes(x = Region, y = Sales, group = Region, fill = Name)) +
  geom_col(position = "stack") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Top Genre by Sales per Region")

VIDEOGS %>%
  gather("Region", "Value", c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")) %>%
  group_by(Region, Genre) %>%
  summarize(Sales = sum(Value)) %>%
  top_n(n = 3) %>%
  ggplot(aes(x = Region, y = Sales, group = Region, fill = Genre)) +
  geom_col(position = "stack") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Top Genre by Sales per Region")
#top sales by Publisher
VIDEOGS %>%
  gather("Region", "Value", c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")) %>%
  group_by(Region, Platform) %>%
  summarize(Sales = sum(Value)) %>%
  top_n(n = 3) %>%
  ggplot(aes(x = Region, y = Sales, group = Region, fill = Platform)) +
  geom_col(position = "stack") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Top Genre by Sales per Region")
#number of games released per
#the output revealed that the highest number of games was produced in 2008 with a total of 1427 games which was followed by the year 2007 with 1426.
VIDEOGS %>%
  group_by(Year_of_Release) %>%
  summarize(Number_of_games_each_year = n())
#PLOT
VIDEOGS %>%
  group_by(Year_of_Release) %>%
  summarize(Number_of_games_each_year = n()) %>%
  ggplot(aes(x = Year_of_Release, y = Number_of_games_each_year)) +
  geom_col(fill = "red") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Games released per Year", x = "Year", y = "Sales (units)")
install.packages("corrplot")
library(corrplot)
VIDEOGS[, c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")] %>%
  cor(method = "pearson") %>%
  corrplot::corrplot(addCoef.col = "white", type="upper")

# relationship between sales and Genre and Rating
#The result revealed a strong relationship between sales and Genre. 
VIDEOGS %>%
  ggplot(aes(x = Rating, y = Genre, col = Genre)) +
  geom_jitter(alpha = 0.6, pch = 25) +
  theme(legend.position = "none") +
  scale_color_viridis(discrete = TRUE)
