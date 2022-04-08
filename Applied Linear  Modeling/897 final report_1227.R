setwd("~/Desktop/R Workfile/final project/R working files")
#install.packages("RSocrata")
#install.packages('lubridate')
library(RSocrata)
library(dplyr)
library(plyr)
library(lubridate)
library(ggplot2)

##############################################
########## Crime Data Retrieve ############
##############################################

# 2019
# Retrieve the data from 2019.01.01 to 2019.04.30
df_2019 <- read.socrata("https://data.cityofchicago.org/resource/6zsd-86xi.json?$where=date between '2019-01-01T0:00:00' and '2019-04-30T23:59:59'",
                        app_token = "bQUYVD7927yqZviZ6wCzgtixH",
                        email= "ylcao@upenn.edu",
                        password= "Cyl174387!")

# Extract the date (Y-M-D) from column 'date' and change the type to timestamp
df_2019$day <- format(df_2019$date,'%Y-%m-%d')
df_2019$day <- as.Date(df_2019$day)

# 2020
# Retrieve the data from 2020.01.01 to 2020.04.30
df_2020 <- read.socrata("https://data.cityofchicago.org/resource/6zsd-86xi.json?$where=date between '2020-01-01T0:00:00' and '2020-04-30T23:59:59'",
                        app_token = "bQUYVD7927yqZviZ6wCzgtixH",
                        email= "ylcao@upenn.edu",
                        password= "Cyl174387!")

# Extract the date (Y-M-D) from column 'date' and change the type to timestamp
df_2020$day <- format(df_2020$date,'%Y-%m-%d')
df_2020$day <- as.Date(df_2020$day)

##############################################
########## Crime Data Aggregation ############
##############################################

# group_by community and week and merge two years of data
## 2019
df_2019_id <- na.omit(df_2019[,c("id","date","day","community_area")])
df_2019_id$week = lubridate::week(ymd(df_2019_id$day))
df_2019_id$week

community_2019_w <- na.omit(df_2019_id) %>%
  group_by(community_area, week) %>%
  dplyr::summarize(crime_n=n())
community_2019_w$year <- 0

## 2020
df_2020_id <- na.omit(df_2020[,c("id","date","day","community_area")])
df_2020_id$week = lubridate::week(ymd(df_2020_id$day))

community_2020_w <- na.omit(df_2020_id) %>%
  group_by(community_area, week) %>%
  dplyr::summarize(crime_n=n())
community_2020_w$year <- 1

community_df_w <- rbind(community_2019_w, community_2020_w)

# treat
community_df_w$treat<- ifelse(community_df_w$week>=10,1,0)

##############################################
##########  Merge other variables ############
##############################################

pop_2019 <- read.csv('community_2019pop.csv')
pop_2020 <- read.csv('community_2020pop.csv')
medinc_raw <- read.csv('community_medinc.csv')

pop_2019$year <- 0
pop_2019$NON_WHITE <- (1-pop_2019$WHITE_PCT)
pop_2019_nonwhite <- pop_2019[c('GEOID','GEOG','year','NON_WHITE')]
names(pop_2019_nonwhite)[names(pop_2019_nonwhite) == 'GEOID'] <- 'community_area'

pop_2020$year <- 1
pop_2020$NON_WHITE <- (1-pop_2020$WHITE_PCT)
pop_2020_nonwhite <- pop_2020[c('GEOID','GEOG','year','NON_WHITE')]
names(pop_2020_nonwhite)[names(pop_2020_nonwhite) == 'GEOID'] <- 'community_area'

# df that will be used
pop <- rbind(pop_2019_nonwhite, pop_2020_nonwhite)
medinc <- medinc_raw[c('GEOID','MEDINC')] # %>% rename(community_area = GEOID)
names(medinc)[names(medinc) == 'GEOID'] <- 'community_area'

# merge everything together
community_df_w <- merge(community_df_w, pop, by = c('community_area','year'))
community_df_w <- merge(community_df_w, medinc, by = 'community_area')

# did intersection 
community_df_w$did <- community_df_w$year * community_df_w$treat

# Variable explanation:
colnames(community_df_w)
# community_area: community id
# year: 0 represents 2019, 1 represents 2021.
# week: represents the order of the week of the year
# treat: 0 represent before lockdown, 1 represent after
# crime_n: how many crime occurred
# GEOG: community name
# NON_WHITE: Pct of non white population
# MEDINC: Median Income of this community


########################################
############### Why DiD ################
########################################

community_df_w$group = ifelse(community_df_w$year == 0, "Non_treat (2019)","Treated(2020)")
community_df_w$time = ifelse(community_df_w$treat == 0, "Before (Week 1-9)","After(Week 10-18)")

# Fig 1 & Fig 2
hist.before <- community_df_w %>%
  filter(treat == 0) %>%
  ggplot(aes(crime_n, fill = group)) +
  geom_histogram(aes(y=..count../sum(..count..)*100),
                 alpha=0.5, position = "dodge", bins = 20) +
  labs(title = "Week 1 - Week 9", x = "Crime Rate", y = "Percent", fill = "") +
  ylim(0,20) + theme_minimal()

hist.before

hist.after <- community_df_w %>%
  filter(treat == 1) %>%
  ggplot(aes(crime_n, fill = group)) +
  geom_histogram(aes(y=..count../sum(..count..)*100),
                 alpha=0.5, position = "dodge", bins = 20) +
  labs(title = "Week 10 - Week 18", x = "Crime Rate", y = "Percent", fill = "") +
  ylim(0,20) + theme_minimal()

hist.after








##############################################
########   Descriptive Statistics   ##########
##############################################

# Table 1
stargazer(community_df_w[,c("crime_n",
                            "year",
                            "treat",
                            "NON_WHITE",
                            "MEDINC")], type = 'html', title = 'Table 1: Descriptive Statistics', 
          align = TRUE, out = 'table1.doc')


# Table 2
stargazer(cor(as.matrix(community_df_w[,c("crime_n",
                                "year",
                                "treat",
                                "NON_WHITE",
                                "MEDINC")]), use = "complete"), type = 'html',title = 'Table 2: Correlation Matrix',
          align = TRUE, out = 'table2.doc')


##############################################
##########   Initial regression   ############
##############################################

# Table 3
lm1 <- lm(crime_n ~ year:treat, data = community_df_w)
summary(lm1)

lm2 <- lm(crime_n ~ year + treat + year:treat, data = community_df_w)
summary(lm2)

lm3 <- lm(crime_n ~ year + treat + year:treat + NON_WHITE, data = community_df_w)
summary(lm3)

lm4 <- lm(crime_n ~ year + treat + year:treat + NON_WHITE + MEDINC, data = community_df_w)
summary(lm4)

stargazer(lm1, lm2, lm3, lm4, type = 'html' ,title = "Table 3: Regression Results", 
          ci = TRUE, ci.level = 0.95, omit.stat ='ser', align = TRUE, out = 'table3.doc')


##############################################
##########  Assumption Diagnostics ###########
##############################################

# 1. Linearity

# Fig 3
ggplot(community_df_w, aes(x = MEDINC, y = crime_n))+
  geom_point(size = 0.6)+ 
  xlab("Community's Median Income") +
  ylab("Number of Crimes in the Community")+
  theme_bw()+ 
  geom_smooth(method = "loess") +
  ggtitle("Crime Rate vs Median Income")

# Fig 4
ggplot(community_df_w, aes(x = NON_WHITE, y = crime_n))+
  geom_point(size = 0.6)+ 
  xlab("Community's Non White Pct") +
  ylab("Number of Crimes in the Community")+
  theme_bw()+ 
  geom_smooth(method = "loess") +
  ggtitle("Crime Rate vs Non_white")


# Homoscedasticity
# Fig 5
community.res<- resid(lm4)
fitted.res<-fitted(lm4)
plot(fitted.res,community.res, main = 'Residuals vs Fitted, model 4')
abline(0, 0, col= "red") 
lines(lowess(lm4$residuals ~ lm4$fitted.values), col="green")   

# Normality of Residuals
# Fig 6
hist(community.res,150, main = 'Histogram of residuals, model 4')
# Fig 7
boxplot(community.res, main="Boxplot of residuals, model 4") 
# Fig 8
plot(lm4, main = "Q-Q Plot, model 4", which = 2)

# Ourliers
# Table 4
community_df_w[c(613, 627, 628), c("crime_n","year","treat","NON_WHITE","MEDINC")]

# Fig 9
plot(lm4, main = "Cook's distance, model 4", which = 4)
# Fig 10
plot(lm4, main = "Residuals vs Leverage, model 4", which = 5)
# Table 5
community_df_w[c(2698, 2703, 2714), c("crime_n","year","treat","NON_WHITE","MEDINC")]



##############################################
################  Correction  ################
##############################################


## Variable Transformations
# Create a new log faminc variable
# values less than or equal to 1 are zero, values greater than 1 are logged
community_df_w$loginc <- ifelse(community_df_w$MEDINC <= 1, 0, 
                          ifelse(community_df_w$MEDINC > 1, log(community_df_w$MEDINC), NA))


# Run a model with the new transformations
lm5 <- lm(crime_n ~ year + treat + year:treat + NON_WHITE + loginc, data = community_df_w)
summary(lm5)
# Table 6
stargazer(lm5, type = 'html' ,title = "Table 6: Regression Results - Model 5", 
          ci = TRUE, ci.level = 0.95, omit.stat ='ser', align = TRUE, out = 'table6.doc')


# Check for AIC
library(AICcmodavg)
models <- list(lm4, lm5)
#specify model names
mod.names <- c("Original","Log")
#calculate AIC of each model
aictab(cand.set = models, modnames = mod.names)
# The original one before log transformation has a lower AIC value, but we will still need to check for diagnostics.


# Check for linearity
# Fig 11
ggplot(community_df_w, aes(x = loginc, y = crime_n)) +
  geom_point(size = 0.6) +
  xlab("Log Income") +
  ylab("Crime rate") +
  theme_bw() +
  geom_smooth(method = "loess") +
  ggtitle("Crime Rate vs Loginc")


# Residuals vs. fitted values
# Fig 12
par(mfrow=c(1,1))
plot(lm5$fitted.values,lm5$residuals, main = "Residuals vs Fitted, model 5")
abline(0, 0, col= "red") 
lines(lowess(lm5$residuals ~ lm5$fitted.values), col="green") 

# Normality of residuals:
# Fig 13
hist(lm5$residuals,150)
# Fig 14
plot(lm5, main = "Q-Q Plot, model 5", which = 2)

## Outliers and multicollinearity
# Fig 15
plot(lm5, main = "Cook's distance, model 5", which = 4)
# Fig 16
plot(lm5, main = "Residuals vs Leverage, model 5", which = 5)
# Table 7
community_df_w[c(2698, 2703, 2714), c("crime_n","year","treat","NON_WHITE","loginc","GEOG")]


# Create a new data frame where we can store all of our outlier information
outliers <- community_df_w
outliers$cd <- cooks.distance(lm5)


# Cook's D:
4/2767

# New dataset:
community_df_new <- outliers %>% filter (abs(outliers$cd) < 4/2767)
# New regression
lm6<-lm(crime_n ~ year + treat + year:treat + NON_WHITE + loginc, data= community_df_new)
summary(lm6)
# Table 8
stargazer(lm6, type = 'html' ,title = "Table 8: Regression Result - Model 6", 
          ci = TRUE, ci.level = 0.95, omit.stat ='ser', align = TRUE, out = 'table8.doc')

## Multicollinearity 
# load the car package
library(car)
# Assess whether model 3 has a multicollinearity problem 
vif(lm6)



# Compare the two models
# Check for AIC
models_2 <- list(lm5, lm6)
#specify model names
mod.names_2 <- c("lm5","lm6")
#calculate AIC of each model
aictab(cand.set = models_2, modnames = mod.names_2)


# Table 9
stargazer(lm5, lm6, type = 'html' ,title = "Table 9: Comparison of lm5 and lm6", 
          ci = TRUE, ci.level = 0.95, omit.stat ='ser', align = TRUE, out = 'table9.doc')





##############################################
################    Results   ################
##############################################

# Table 10: Differences

### First differences
differences <- community_df_new %>%
  group_by(time, group) %>%
  dplyr::summarize(crime_n=mean(crime_n))

# Control group (2019) after treatment
after_2019 <- differences[1,3]

# Treatment group (2020) after treatment
after_2020 <- differences[2,3]

# Control group (2019) before treatment
before_2019 <- differences[3,3]

# Treatment group (2020) before treatment
before_2020 <- differences[4,3]


### ATT effect
(after_2020-after_2019)-(before_2020-before_2019)



### Visualization

# Calculate counterfactual outcome
counterfactual_2020 <- tibble(
  time = c("Before (Week 1-9)","After(Week 10-18)"), 
  group = c("2020 (Counterfactual)","2020 (Counterfactual)"),
  crime_n = as.numeric(c(before_2020, before_2020-(before_2019-after_2019)))
) 

# Data points for treatment event
intervention <- tibble(
  time = c("Intervention", "Intervention", "Intervention"),
  group = c("Treated(2020)", "Non_treat (2019)", "2020 (Counterfactual)"),
  crime_n = c(49.00205, 47.98805, 49.00205)
) 

# Combine data
did_plotdata <- bind_rows(differences, 
                          counterfactual_2020, 
                          intervention)
did_plotdata$time <- factor(did_plotdata$time,levels = c("Before (Week 1-9)","Intervention","After(Week 10-18)"))

# Fig 18
did_plotdata %>%
  ggplot(aes(x=time,y=crime_n, group=group)) +
  geom_line(aes(color=group), size=1.2) +
  geom_vline(xintercept = "Intervention", linetype="dotted", 
             color = "black", size=1.1) + 
  scale_color_brewer(palette = "Accent") +
  scale_y_continuous(limits = c(35,60)) +
  labs(x="", y="Community Crime Rate (mean)") +
  ggtitle("Difference-in-Difference Effect") +
  theme_minimal()

