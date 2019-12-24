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
#data inspection
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

#Distrubution of sales around the regions of the world according to platforms, genres, names and publishers.
#Sales in North America
boxplot(VIDEOGS$NA_Sales, main="Sales in North America", xlab="Sales", ylab="frequency", vertical = TRUE)
summary(VIDEOGS$NA_Sales)
#sales in Europe
boxplot(VIDEOGS$EU_Sales, main="Sales in Europe", xlab="Sales", ylab="frequency", vertical = TRUE)
summary(VIDEOGS$EU_Sales)

#sales in Japan
boxplot(VIDEOGS$JP_Sales, main="Sales in Japan", xlab="Sales", ylab="frequency", vertical = TRUE)
summary(VIDEOGS$JP_Sales)

#Global sales
boxplot(VIDEOGS$Global_Sales, main="Sales in Japan", xlab="Sales", ylab="frequency", vertical = TRUE)
summary(VIDEOGS$Global_Sales)

#Popular Genre by sales at global level

VIDEOGS%>%
  group_by(Genre) %>%
  summarise(Count = n()) %>%
  plot_ly(x = ~Genre,
          y = ~Count,
          type = "bar", col="black")
#From the bar chart above, Action Genre games yielded the highest sales globally.
##DC and PS were the platforms that benefitted with the highest sales at the global level. Although X360 and WiiU also strived at the global the global sales.

VIDEOGS%>%
  group_by(Platform) %>%
  summarise(Count = n()) %>%
  plot_ly(x = ~Platform,
          y = ~Count,
          type = "bar")

#Sales by publishers
#The bar chart revealed that Electronic Arts was the publisher with the highest sales.
VIDEOGS%>%
  group_by(Publisher) %>%
  summarise(Count = n()) %>%
  plot_ly(x = ~Publisher,
          y = ~Count,
          type = "bar")


#top sales by Platforms by region
#The top platforms by sales in different region were DS, PS, PS2, PS3, WiiU and X360
VIDEOGS %>%
  gather("Region", "Value", c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")) %>%
  group_by(Region, Platform) %>%
  summarize(Sales = sum(Value)) %>%
  top_n(n = 3) %>%
  ggplot(aes(x = Region, y = Sales, group = Region, fill = Platform)) +
  geom_col(position = "stack") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Top Genre by Sales per Region")

#Top sold games in the regions of the world
VIDEOGS %>%
  gather("Region", "Value", c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")) %>%
  group_by(Region, Name) %>%
  summarize(Sales = sum(Value)) %>%
  top_n(n = 3) %>%
  ggplot(aes(x = Region, y = Sales, group = Region, fill = Name)) +
  geom_col(position = "stack") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Top Genre by Sales per Region")

#Top sold genres according to regions
#Action, Role-playing, Shooter and Sport
VIDEOGS %>%
  gather("Region", "Value", c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")) %>%
  group_by(Region, Genre) %>%
  summarize(Sales = sum(Value)) %>%
  top_n(n = 3) %>%
  ggplot(aes(x = Region, y = Sales, group = Region, fill = Genre)) +
  geom_col(position = "stack") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Top Genre by Sales per Region")


#total sales per year at global level
tot_year <- aggregate(VIDEOGS$Global_Sales, by=list(Year=VIDEOGS$Year), sum)
plot(tot_year)
data_frame(tot_year)

#Global Sales per genre
Glb_sales <- aggregate(VIDEOGS$Global_Sales, by=list(Genre=VIDEOGS$Genre), sum)
data.table(Glb_sales)
plot(Glb_sales)

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

#a simple correlation matrix was carried out between sales
install.packages("corrplot")
library(corrplot)
VIDEOGS[, c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")] %>%
  cor(method = "pearson") %>%
  corrplot::corrplot(addCoef.col = "white", type="upper")

#We considered the relationship between Sales in other regions against the global sales.
par(mfrow=c(1,4))
with(VIDEOGS, plot(NA_Sales, Global_Sales))
with(VIDEOGS, plot(EU_Sales, Global_Sales))
with(VIDEOGS, plot(JP_Sales, Global_Sales))
with(VIDEOGS, plot(Other_Sales, Global_Sales))
#The result revealed that there is linear relationship between regional sales and global sales.
#we further considered linear models between sales in all the regions and genres in the study since action Genres have dorminated sales.
#North America
fit <- lm( NA_Sales ~ Genre, VIDEOGS)
summary(fit)
plot(fit)
#linear relationship between sales in Europe and Genre
fit <- lm( EU_Sales ~ Genre, VIDEOGS)
summary(fit)
plot(fit)

#linear relationship between sales in Japan and Genre
fit <- lm( JP_Sales ~ Genre, VIDEOGS)
summary(fit)
plot(fit)

#linear relationship between other sales and Genre
fit <- lm(Other_Sales ~ Genre, VIDEOGS)
summary(fit)
plot(fit)

#linear relationship between global sales and Genre
fit <- lm(Global_Sales ~ Genre, VIDEOGS)
summary(fit)
plot(fit)
#with reference to the fitted plots, it is obvious that genres has no influence on the sales of games in all the regions

#we looked at the relationship between sales and Genre and Rating
#The result revealed a strong relationship between sales and Genre. 
VIDEOGS %>%
  ggplot(aes(x = Rating, y = Genre, col = Genre)) +
  geom_jitter(alpha = 0.6, pch = 25) +
  theme(legend.position = "none") +
  scale_color_viridis(discrete = TRUE)

