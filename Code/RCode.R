####################################################################################
#                                Social Network Analysis MidTerm
#                                Name  -   Sonal Mendiratta MSBA20B
#                                ID    -   64446813
###################################################################################

#Load relevant packages

#install.packages("psych")
#install.packages("sqldf")
#install.packages('rlang')
#install.packages('corrplot')
#install.packages("gridExtra")
#install.packages("Matching")
#install.packages("optmatch")
library(igraph)
library(dplyr)  
library("psych")
library(sqldf)
library(ggplot2)
library(corrplot)
library("Hmisc")
library(MatchIt)
library(gridExtra)
library(pROC)
library(reshape)

#setting working directory
setwd("/Users/sonal/Desktop/MSBA/COURSES/QUARTER3/CustomerSocialAnalytics/MidTerm")

#=================================================================================#
####                               DATA EXPLORATION                            ####
#=================================================================================#

#Importing the file
highnote_data <- data.frame(read.csv("HighNoteDataMidterm.csv"))

#Dimensions of the data
dim(highnote_data)
#[1] 43827    16

#Class of each variable
sapply(highnote_data,class)
#ID                   age                  male            friend_cnt        avg_friend_age 
#"integer"             "integer"             "integer"             "integer"             "numeric" 
#avg_friend_male    friend_country_cnt subscriber_friend_cnt         songsListened           lovedTracks 
#"numeric"             "integer"             "integer"             "integer"             "integer" 
#posts             playlists                shouts               adopter                tenure 
#"integer"             "integer"             "integer"             "integer"             "integer" 
#good_country 
#"integer" 

head(highnote_data)

#Checking if the data has any missing values
indx <-lapply(highnote_data, function(x){any(is.na(x))} )
indx #No missing values in the data

attach(highnote_data)
table(adopter)

##################################################################################
##                                  QUESTION 1                                  ##
##################################################################################

#=================================================================================#
#                                  DESCRIPTIVE STATISTICS                         #
#=================================================================================#


#creating a new table without the ID, male and good_country as they are identifiers 
high_without_id <-subset(highnote_data,select = -ID)

dim(high_without_id)
#[1] 43827    15

# Descriptive statistics by adopter
stat_all <- describeBy(high_without_id, high_without_id$adopter)
print(stat_all)

# Getting the count of total number of users in both the groups
user_count<- sqldf("select adopter, count(distinct ID) as users
                    from highnote_data 
                    group by adopter")

user_count

user_pct <- mutate(user_count, 
                   user_pct = users * 100/ sum(users))
user_pct
#Plotting the percentatge of Free and Premium users in the data
barplot(user_pct[,3], names.arg=c("Free", "Premium"), main="Percentage of Users",col="light blue")

names(highnote_data)

#=================================================================================#
#                                  T TEST FOR DIFFERENCE OF MEANS                 #
#=================================================================================#

# t tests for difference of means

#age
df <- highnote_data %>%
  filter(adopter == 1 | adopter == 0) %>%
  select(age, adopter)
t.test(age ~ adopter, data = df)

#male
df <- highnote_data %>%
  filter(adopter == 1 | adopter == 0) %>%
  select(male, adopter)
t.test(male ~ adopter, data = df)

#friend_cnt
df <- highnote_data %>%
  filter(adopter == 1 | adopter == 0) %>%
  select(friend_cnt, adopter)
t.test(friend_cnt ~ adopter, data = df)

#avg_friend_age
df <- highnote_data %>%
  filter(adopter == 1 | adopter == 0) %>%
  select(avg_friend_age, adopter)
t.test(avg_friend_age ~ adopter, data = df)

#avg_friend_male
df <- highnote_data %>%
  filter(adopter == 1 | adopter == 0) %>%
  select(avg_friend_male, adopter)
t.test(avg_friend_male ~ adopter, data = df)

#friend_country_cnt
df <- highnote_data %>%
  filter(adopter == 1 | adopter == 0) %>%
  select(friend_country_cnt, adopter)
t.test(friend_country_cnt ~ adopter, data = df)

#subscriber_friend_cnt
df <- highnote_data %>%
  filter(adopter == 1 | adopter == 0) %>%
  select(subscriber_friend_cnt, adopter)
t.test(subscriber_friend_cnt ~ adopter, data = df)

#songsListened
df <- highnote_data %>%
  filter(adopter == 1 | adopter == 0) %>%
  select(songsListened, adopter)
t.test(songsListened ~ adopter, data = df)

#lovedTracks
df <- highnote_data %>%
  filter(adopter == 1 | adopter == 0) %>%
  select(lovedTracks, adopter)
t.test(lovedTracks ~ adopter, data = df)

#posts
df <- highnote_data %>%
  filter(adopter == 1 | adopter == 0) %>%
  select(posts, adopter)
t.test(posts ~ adopter, data = df)

#playlists
df <- highnote_data %>%
  filter(adopter == 1 | adopter == 0) %>%
  select(playlists, adopter)
t.test(playlists ~ adopter, data = df)

#shouts
df <- highnote_data %>%
  filter(adopter == 1 | adopter == 0) %>%
  select(shouts, adopter)
t.test(shouts ~ adopter, data = df)

#tenure
df <- highnote_data %>%
  filter(adopter == 1 | adopter == 0) %>%
  select(tenure, adopter)
t.test(tenure ~ adopter, data = df)

#good_country
df <- highnote_data %>%
  filter(adopter == 1 | adopter == 0) %>%
  select(good_country, adopter)
t.test(good_country ~ adopter, data = df)

head(highnote_data)

#one step t test --- did not work
highnote_cov <- c('age', 'male', 'friend_cnt', 'avg_friend_age', 'avg_friend_male','friend_country_cnt',
              'subscriber_friend_cnt','songsListened','lovedTracks','posts','playlists',
              'shouts','tenure','good_country','adopter')

highnote_data %>%
  group_by(adopter) %>%
  select(one_of(highnote_cov)) %>%
  summarise_all(funs(mean))
head(highnote_data)
lapply(highnote_cov, function(v) {
  t.test(highnote_data[, v] ~ highnote_data$adopter)
})


##################################################################################
##                                  QUESTION 2                                  ##
##################################################################################

#=================================================================================#
####                               VISUALIZATIONS                              ####
#=================================================================================#

#Chart 1 & 2 - Variables male and good_country
# Getting the count of total number of users in both the groups
user_count<- sqldf("select adopter, sum(male) as males,sum(good_country) as good_country
                    from highnote_data 
                    group by adopter")

user_count
#creating new variables to calculate of the total number of users in the variable of interest, how many lie
#in the particular group of adopters
user_pct <- mutate(user_count, 
                   male_pct = males * 100/ sum(males),
                   goodcountry_pct = good_country * 100/ sum(good_country))
user_pct

#Plotting the percentage of users within these two groups
barplot(user_pct[,5], names.arg=c("Free", "Premium"), main="Percentage of Males",col="light blue")
barplot(user_pct[,6], names.arg=c("Free", "Premium"), main="Percentage of Users from Good Countries",col="light blue")

#Chart 3 - age
png( 'biage.png' )
bi.bars(highnote_data,"age","adopter",ylab="Age",main="Age by Premium and Free Users")
dev.off()
aa<- sqldf("select adopter,age,count(distinct ID) as users
           from highnote_data
           group by adopter,age")
aa

#Chart 3 - age - backup code
labs <- paste("Type of Users - ", c("Premium", "Free"))
highnote_data %>%
  mutate(adopter = ifelse(adopter == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = age)) +
  geom_histogram(color = "white") +
  facet_wrap(~adopter) +
  xlab("Distribution of Age of the users") +
  theme_bw()

#Chart 3 - age
p<-ggplot(highnote_data,aes(x=adopter,y=age,fill=age,group=adopter))+
  geom_violin() +
  theme(legend.position="none")+
  ggtitle("Distribution of Age of Free and Premium Users")+
  theme(plot.title=element_text(hjust=0.5))+
  geom_boxplot(width=0.1,fill="light blue")
p

#Chart 4 - avg_friend_age
p<-ggplot(highnote_data,aes(x=adopter,y=avg_friend_age,fill=avg_friend_age,group=adopter))+
  geom_violin() +
  theme(legend.position="none")+
  ggtitle("Distribution of Age of Friends of Free and Premium Users")+
  theme(plot.title=element_text(hjust=0.5))+
  geom_boxplot(width=0.1,fill="light blue")
p

#Chart 5 friend_cnt

bp<-ggplot(highnote_data, aes(x=adopter,y=friend_cnt,group=adopter,fill=adopter))+
  geom_boxplot(alpha=0.5,varwidth=TRUE)+
  theme(legend.position="none")+
  ggtitle("Number of friends of Free and Premium Users")+
  theme(plot.title=element_text(hjust=0.5))+
  stat_summary(fun.y=mean,geom='point',shape=20,size=5,color='red',fill='red')+
  theme(
    panel.background = element_rect(fill='light blue',colour='white',
                                    size=0.5,linetype='solid'),
    panel.grid.major =element_line(colour='white',
                                   size=0.5,linetype='solid'),
    panel.grid.minor=element_line(colour='white',
                                  size=0.25,linetype='solid'))
bp+theme(plot.background=element_rect(fill='green'))
bp

#Chart 6 subscriber_friend_cnt


bp<-ggplot(highnote_data, aes(x=adopter,y=subscriber_friend_cnt,group=adopter,fill=adopter))+
  geom_boxplot(alpha=0.5,varwidth=TRUE)+
  theme(legend.position="none")+
  ggtitle("Number of Premium friends of Free and Premium Users")+
  theme(plot.title=element_text(hjust=0.5))+
  stat_summary(fun.y=mean,geom='point',shape=20,size=5,color='red',fill='red')+
  theme(
    panel.background = element_rect(fill='light blue',colour='white',
                                    size=0.5,linetype='solid'),
    panel.grid.major =element_line(colour='white',
                                   size=0.5,linetype='solid'),
    panel.grid.minor=element_line(colour='white',
                                  size=0.25,linetype='solid'))
bp+theme(plot.background=element_rect(fill='green'))
bp

#Chart 6 avg_friend_male
ggplot(highnote_data,aes(x=adopter,y=avg_friend_male))+
  geom_bar(stat = 'summary', fun.y = 'mean',fill = 'light blue',col='black')+
  ggtitle("Average Proportion of Male Free and Premium Users")



#Chart 6 friend_country_cnt
bp<-ggplot(highnote_data, aes(x=adopter,y=friend_country_cnt,group=adopter,fill=adopter))+
  geom_boxplot(alpha=0.5,varwidth=TRUE)+
  theme(legend.position="none")+
  ggtitle("Number of Different countries that the friends of Free and Premiums users are from")+
  theme(plot.title=element_text(hjust=0.5))+
  stat_summary(fun.y=mean,geom='point',shape=20,size=5,color='red',fill='red')+
  theme(
    panel.background = element_rect(colour='white',
                                    size=0.5,linetype='solid'),
    panel.grid.major =element_line(colour='white',
                                   size=0.5,linetype='solid'),
    panel.grid.minor=element_line(colour='white',
                                  size=0.25,linetype='solid'))
bp+theme(plot.background=element_rect(fill='green'))
bp



#Chart 7 songsListened
p<-ggplot(highnote_data,aes(x=adopter,y=songsListened,fill=songsListened,group=adopter))+
  geom_violin() +
  theme(legend.position="none")+
  ggtitle("Distribution of Number of Songs Listened by Free and Premium Users")+
  theme(plot.title=element_text(hjust=0.5))+
  geom_boxplot(width=0.1,fill="light blue")
p

#Chart 8 lovedTracks

labs <- paste("Type of Users - ", c("Premium", "Free"))
highnote_data %>%
  mutate(adopter = ifelse(adopter == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = lovedTracks)) +
  geom_histogram(color = "white") +
  facet_wrap(~adopter) +
  xlab("Distribution of Number of Songs Loved by the users") +
  theme_bw()

#Chart 9 posts

p<-ggplot(highnote_data,aes(x=adopter,y=posts,fill=posts,group=adopter))+
  geom_violin() +
  theme(legend.position="none")+
  ggtitle("Distribution of Number of Posts made by Free and Premium Users")+
  theme(plot.title=element_text(hjust=0.5))+
  geom_boxplot(width=0.1,fill="light blue")
p

#Chart 10 playlists

labs <- paste("Type of Users - ", c("Premium", "Free"))
highnote_data %>%
  mutate(adopter = ifelse(adopter == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = playlists)) +
  geom_histogram(color = "white") +
  facet_wrap(~adopter) +
  xlab("Distribution of Playlists made by the users") +
  theme_bw()

#Chart 11 shouts


bp<-ggplot(highnote_data, aes(x=adopter,y=shouts,group=adopter,fill=adopter))+
  geom_boxplot(alpha=0.5,varwidth=TRUE)+
  theme(legend.position="none")+
  ggtitle("Number of Shouts received by Free and Premiums users")+
  theme(plot.title=element_text(hjust=0.5))+
  stat_summary(fun.y=mean,geom='point',shape=20,size=5,color='red',fill='red')+
  theme(
    panel.background = element_rect(colour='white',
                                    size=0.5,linetype='solid'),
    panel.grid.major =element_line(colour='white',
                                   size=0.5,linetype='solid'),
    panel.grid.minor=element_line(colour='white',
                                  size=0.25,linetype='solid'))
bp+theme(plot.background=element_rect(fill='green'))
bp


#Chart 12 tenure
labs <- paste("Type of Users - ", c("Premium", "Free"))
highnote_data %>%
  mutate(adopter = ifelse(adopter == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = tenure),fill=adopter) +
  geom_histogram(color = "plum") +
  facet_wrap(~adopter) +
  xlab("Distribution of Number of Months Users have been on the site") +
  theme_bw()

#Chart 13 - Relation between songs listened and tenure
ggplot(data=highnote_data,aes(x=tenure,y=songsListened,fill=adopter))+
  geom_point( color="plum", shape=23)+
  geom_smooth(method=lm, se=FALSE)+
  facet_wrap(highnote_data$adopter)+
  labs(x="Number of Months the user has been on the site",y= "Number of Songs Listened",
       title = "Relationship b/w Tenure and Number of Songs listened")

#Chart 14 - Relation between friend_cnt and friend_country_cnt
ggplot(data=highnote_data,aes(x=friend_cnt,y=subscriber_friend_cnt,fill=adopter))+
  geom_point( color="plum", shape=23)+
  geom_smooth(method=lm, se=FALSE)+
  facet_wrap(highnote_data$adopter)+
  labs(x="Number of friends",y= "Number of Premium friends",
       title = "Relationship b/w Number of Friends and Number of Premium Friends")

#Chart 15
#Understanding the pattern of user engagement over tenure
#taking the following variables here - posts, songsListened,playlists,lovedTracks
#core idea is to get the percentage of users who performed an activity in that tenure where records for activity are filtered
# by the logic that atleast one instance of the activity is performed in that tenure

#posts
df_post<- sqldf("select tenure,count(distinct ID) as users_posting
              from highnote_data
              where posts>=1
              group by tenure
              ")
df_post <- mutate(df_post, 
                  users_posting_pct = users_posting * 100/ sum(users_posting))
#songsListened
df_songs<- sqldf("select tenure,count(distinct ID) as users_listening
              from highnote_data
              where songsListened>=1
              group by tenure
              ")
df_songs <- mutate(df_songs, 
                   users_listening_pct = users_listening * 100/ sum(users_listening))
#playlists
df_playlists<- sqldf("select tenure,count(distinct ID) as users_playlists
              from highnote_data
              where playlists>=1
              group by tenure
              ")
df_playlists <- mutate(df_playlists, 
                       users_playlists_pct = users_playlists * 100/ sum(users_playlists))
#lovedTracks
df_loved<- sqldf("select tenure,count(distinct ID) as users_lovetracks
              from highnote_data
              where lovedTracks>=1
              group by tenure
              ")
df_loved <- mutate(df_loved, 
                   users_loved_pct = users_lovetracks * 100/ sum(users_lovetracks))
#merging datasets one by one
a <- merge(x = df_post, y = df_songs, by = "tenure")
b <- merge(x = a,y = df_playlists, by = "tenure")
c <- merge(x = b,y = df_loved, by = "tenure")

df_all<- sqldf("select tenure,
                        users_posting_pct as pct_users_posting,
                        users_listening_pct as pct_users_listening,
                        users_playlists_pct as pct_users_playlisting,
                        users_loved_pct as pct_users_loving
                from c")

#Reshaping the dataset
Molten <- melt(df_all, id.vars = "tenure")

#plotting the graph
ggplot(Molten, aes(x = tenure, y = value, colour = variable)) + geom_line()+
  labs(x="Tenure",y= "Percentage", title = "User Engagement as a Function of Tenure")


#Chart 16 - Pairwise scatter plots ---> will move this under regression category ie. Question 4
pairs(~friend_cnt + subscriber_friend_cnt + friend_country_cnt + playlists + 
       songsListened + lovedTracks + posts + shouts + tenure + adopter,
      data=highnote_data, pch=20, main = "Scatterplot Matrix")

#Chart 17 and 18 - Pairwise scatter plots for Free and Premium Users
names(highnote_data)
adopters <- filter(highnote_data,highnote_data$adopter==1)
pairs(~age+friend_cnt + subscriber_friend_cnt + friend_country_cnt + playlists + 
        songsListened + lovedTracks + posts + shouts + tenure ,
      data=adopters, pch=20, cex=0.2 , main = "Scatterplot Matrix For Premium Users")

nonadopters <- filter(highnote_data,highnote_data$adopter<1)
pairs(~age+friend_cnt + subscriber_friend_cnt + friend_country_cnt + playlists + 
        songsListened + lovedTracks + posts + shouts + tenure ,
      data=nonadopters, pch=20, cex=0.2 , main = "Scatterplot Matrix For Free Users")

#Chart 16

# Male and good_country in both the groups
names(highnote_data)

#good county
labs <- paste("Type of Users - ", c("Premium", "Free"))
highnote_data %>%
  mutate(adopter = ifelse(adopter == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = good_country),fill=adopter) +
  geom_bar(color = "plum",fill='light blue') +
  facet_wrap(~adopter) +
  xlab("Distribution of Number of Users from US, UK or Germany") +
  theme_bw()

#male

labs <- paste("Type of Users - ", c("Premium", "Free"))
highnote_data %>%
  mutate(adopter = ifelse(adopter == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = male),fill=adopter) +
  geom_bar(color = "plum",fill='light blue') +
  facet_wrap(~adopter) +
  xlab("Distribution of Number of Males") +
  theme_bw()

#=================================================================================#
####                               CORRELATIONS                                ####
#=================================================================================#

#Overall Correlation
png('corplot.png')
#Expand the plot section on the R studio if you want to get the plot displayed
corPlot(highnote_data,numbers=TRUE,upper=FALSE,diag=FALSE,main="Correlation Matrix")
dev.off()

#Correlatin for Premium Users
adopters <- filter(highnote_data,highnote_data$adopter==1)
png('corplot.png')
#Expand the plot section on the R studio if you want to get the plot displayed
corPlot(adopters,numbers=TRUE,upper=FALSE,diag=FALSE,main="Correlation Matrix for Premium Users")
dev.off()

#Correlatin for Free Users

nonadopters <- filter(highnote_data,highnote_data$adopter<1)
png('corplot.png')
#Expand the plot section on the R studio if you want to get the plot displayed
corPlot(nonadopters,numbers=TRUE,upper=FALSE,diag=FALSE,main="Correlation Matrix for Free Users")
dev.off()




##################################################################################
##                                  QUESTION 3                                  ##
##################################################################################

#=================================================================================#
####                          PROPENSITY SCORE MATCHING                        ####
#=================================================================================#

#=================================================================================#
####                          ITERATION a) - NON-LOGGED VARIABLES               ####
#=================================================================================#

#Creating treatment and control variables first
#defined the columns as treatement = 1 if subscriber_friend_cnt >=1  else 0
names(highnote_data)
highnote_data_tc <- mutate(highnote_data, treatment = ifelse(subscriber_friend_cnt >=1,1,0))
head(highnote_data_tc)

attach(highnote_data_tc)
table(treatment)
#treatment
#0     1 
#34004  9823 

# Pre-analysis using non-matched data

highnote_data_tc %>%
  group_by(treatment) %>%
  summarise(mean_math = mean(adopter))

#getting mean of adopter variable in both the groups

highnote_data_tc %>%
  group_by(treatment) %>%
  select(adopter,treatment) %>%
  summarise_all(funs(mean(., na.rm = T))) #same result

# t test on the treatment column
with(highnote_data_tc, t.test(adopter ~ treatment))

# Difference-in-means: pre-treatment covariates

names(highnote_data_tc)

# Let's calculate the mean for each covariate by treatment status

highnote_cov <- c('age', 'male', 'friend_cnt', 'avg_friend_age', 'avg_friend_male','friend_country_cnt',
                  'subscriber_friend_cnt','songsListened','lovedTracks','posts','playlists',
                  'shouts','tenure','good_country','adopter','treatment')

highnote_data_tc %>%
  group_by(treatment) %>%
  select(one_of(highnote_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))


# Propensity score estimation
# We estimate the propensity score by running a logit model (probit also works) 
# where the outcome variable is a binary variable indicating treatment status. 

#transforming the variable songsListened
highnote_data_tc <- highnote_data_tc %>% mutate(songsListened_1k = songsListened / 1000)
m_ps <- glm(treatment ~ age + male + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt +
             songsListened_1k + lovedTracks + posts + playlists +
            shouts + tenure + good_country,family = binomial(), data = highnote_data_tc)
summary(m_ps)


# Using this model, we can now calculate the propensity score for each user 
# It is simply the user’s predicted probability of being Treated, 
# given the estimates from the logit model.

prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     treatment = m_ps$model$treatment)
head(prs_df)

# Examining the region of common support
# After estimating the propensity score, it is useful to plot histograms of the estimated propensity scores by treatment status

labs <- paste("Number of Premium friends of User", c(">=1", "=0"))
prs_df %>%
  mutate(treatment = ifelse(treatment == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~treatment) +
  xlab("Probability of having atleast one subscriber friend") +
  theme_bw()



# The method we use below is to find pairs of observations that have very similar propensity scores, 
# but that differ in their treatment status. We use the package MatchIt for this. 
# This package estimates the propensity score in the background and then matches observations based 
# on the method of choice (“nearest” in this case).

highnote_data_tc_nomiss <- highnote_data_tc %>%  # MatchIt does not allow missing values
  select(adopter, treatment, one_of(highnote_cov)) %>%
  na.omit()

mod_match <- matchit(treatment ~ age + male + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt +
                       songsListened + lovedTracks + posts + playlists +
                       shouts + tenure + good_country,
                     method = "nearest", data = highnote_data_tc_nomiss)

# We can get some information about how successful the matching was using summary(mod_match) and plot(mod_match)

summary(mod_match)
plot(mod_match)

# To create a dataframe containing only the matched observations, use the match.data() function

dta_m <- match.data(mod_match)
dim(dta_m)
# The final dataset contains a variable called distance, which is the propensity score.
# Examining covariate balance in the matched sample
# Visual Inspection

fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  if (variable == 'songsListened') dta$variable <- dta$variable / 10^3
  dta$treatment <- as.factor(dta$treatment)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = treatment)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

#renaming columns for plotting
dta_m$Age <- dta_m$age
dta_m$Male <- dta_m$male
dta_m$Number_of_Friends <- dta_m$friend_cnt
dta_m$Average_Age_of_Friends <- dta_m$avg_friend_age
dta_m$Proportion_of_Male_Friends <- dta_m$avg_friend_male
dta_m$Number_of_Countries_Friends <- dta_m$friend_country_cnt
dta_m$Songs_Listened <- dta_m$songsListened
dta_m$Loved_Tracks <- dta_m$lovedTracks
dta_m$Posts <- dta_m$posts
dta_m$Playlists <- dta_m$playlists
dta_m$Shouts <- dta_m$shouts
dta_m$Tenure <- dta_m$tenure
dta_m$US_UK_Germany <- dta_m$good_country

#The graphs displayed using this function are not interpretable 
#therefore plotting 2 at a time
# grid.arrange(
#   fn_bal(dta_m, "Age"),
#   fn_bal(dta_m, "Male") + theme(legend.position = "none"),
#   fn_bal(dta_m, "Number_of_Friends"),
#   fn_bal(dta_m, "Average_Age_of_Friends") + theme(legend.position = "none"),
#   fn_bal(dta_m, "Proportion_of_Male_Friends"),
#   fn_bal(dta_m, "Number_of_Countries_Friends") + theme(legend.position = "none"),
#   fn_bal(dta_m, "Songs_Listened"),
#   fn_bal(dta_m, "Loved_Tracks") + theme(legend.position = "none"),
#   fn_bal(dta_m, "Posts"),
#   fn_bal(dta_m, "Playlists") + theme(legend.position = "none"),
#   fn_bal(dta_m, "Shouts"),
#   fn_bal(dta_m, "Tenure") + theme(legend.position = "none"),
#   fn_bal(dta_m, "US_UK_Germany"),
#   nrow = 7
# )

#grid 1
grid.arrange(
  fn_bal(dta_m, "Age"),
  fn_bal(dta_m, "Male") + theme(legend.position = "none"),
  nrow = 2 
)
#grid 2
grid.arrange(
  fn_bal(dta_m, "Number_of_Friends"),
  fn_bal(dta_m, "Average_Age_of_Friends") + theme(legend.position = "none"),
  nrow = 2 
)
#grid 3
grid.arrange(
  fn_bal(dta_m, "Proportion_of_Male_Friends"),
  fn_bal(dta_m, "Number_of_Countries_Friends") + theme(legend.position = "none"),
  nrow = 2 
)

#grid 4
grid.arrange(
  fn_bal(dta_m, "Songs_Listened"),
  fn_bal(dta_m, "Loved_Tracks") + theme(legend.position = "none"),
  nrow = 2 
)

#grid 5
grid.arrange(
  fn_bal(dta_m, "Posts"),
  fn_bal(dta_m, "Playlists") + theme(legend.position = "none"),
  nrow = 2 
)

#grid 6
grid.arrange(
  fn_bal(dta_m, "Shouts"),
  fn_bal(dta_m, "Tenure") + theme(legend.position = "none"),
  nrow = 2 
)

#grid 7
fn_bal(dta_m, "US_UK_Germany")


# Difference of means
names(dta_m)

dta_m %>%
  group_by(treatment) %>%
  select(one_of(highnote_cov)) %>%
  summarise_all(funs(mean))

lapply(highnote_cov, function(v) {
  t.test(dta_m[, v] ~ dta_m$treatment)
}) ## does not work for me - i will just use my own t test code which takes one variable at a time
head(dta_m)


## add code for t test

# t tests for difference of means

#age
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(age, treatment)
t.test(age ~ treatment, data = df)

#male
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(male, treatment)
t.test(male ~ treatment, data = df)

#friend_cnt
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(friend_cnt, treatment)
t.test(friend_cnt ~ treatment, data = df)

#avg_friend_age
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(avg_friend_age, treatment)
t.test(avg_friend_age ~ treatment, data = df)

#avg_friend_male
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(avg_friend_male, treatment)
t.test(avg_friend_male ~ treatment, data = df)

#friend_country_cnt
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(friend_country_cnt, treatment)
t.test(friend_country_cnt ~ treatment, data = df)

#songsListened
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(songsListened, treatment)
t.test(songsListened ~ treatment, data = df)

#lovedTracks
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(lovedTracks, treatment)
t.test(lovedTracks ~ treatment, data = df)

#posts
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(posts, treatment)
t.test(posts ~ treatment, data = df)

#playlists
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(playlists, treatment)
t.test(playlists ~ treatment, data = df)

#shouts
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(shouts, treatment)
t.test(shouts ~ treatment, data = df)

#tenure
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(tenure, treatment)
t.test(tenure ~ treatment, data = df)

#good_country
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(good_country, treatment)
t.test(good_country ~ treatment, data = df)

names(dta_m)

# Estimating treatment effects

# Estimating the treatment effect is simple once we have 
# a matched sample that we are happy with. We can use a t-test:

with(dta_m, t.test(adopter ~ treatment)) #now the data is matched


dta_m %>%
  group_by(treatment) %>%
  select(adopter,treatment) %>%
  summarise_all(funs(mean(., na.rm = T)))

dta_m %>%
  group_by(treatment) %>%
  select(one_of(highnote_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

# Or we can use OLS with or without covariates:

lm_treat1 <- lm(adopter ~ treatment, data = dta_m)
summary(lm_treat1)

highnote_cov <- c('age', 'male', 'friend_cnt', 'avg_friend_age', 'avg_friend_male','friend_country_cnt',
                  'subscriber_friend_cnt','songsListened','lovedTracks','posts','playlists',
                  'shouts','tenure','good_country','adopter')

lm_treat2 <- lm(adopter ~ treatment + age + male + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt
                   + lovedTracks + posts + playlists +
                  shouts + tenure + good_country +
                  I(songsListened / 10^3) , data = dta_m)
summary(lm_treat2)

#propensity desnity after matching

m_ps <- glm(treatment ~ age + male + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt +
              songsListened + lovedTracks + posts + playlists +
              shouts + tenure + good_country,family = binomial(), data = dta_m)
summary(m_ps)


# Using this model, we can now calculate the propensity score for each student. 
# It is simply the student’s predicted probability of being Treated, 
# given the estimates from the logit model.

prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     treatment = m_ps$model$treatment)
head(prs_df)

labs <- paste("Number of Premium friends of User", c(">=1", "=0"))
prs_df %>%
  mutate(treatment = ifelse(treatment == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~treatment) +
  xlab("Probability of having atleast one subscriber friend") +
  theme_bw()


#=================================================================================#
####                          ITERATION b - LOGGED VARIABLES                   ####
#=================================================================================#


#Creating treatment and control variables first
#defined the columns as treatement = 1 if subscriber_friend_cnt >=1  else 0
names(highnote_data)
highnote_data_tc <- mutate(highnote_data, treatment = ifelse(subscriber_friend_cnt >=1,1,0))
head(highnote_data_tc)

attach(highnote_data_tc)
table(treatment)
#treatment
#0     1 
#34004  9823 

# Pre-analysis using non-matched data

highnote_data_tc %>%
  group_by(treatment) %>%
  summarise(mean_math = mean(adopter))

#getting mean of adopter variable in both the groups

highnote_data_tc %>%
  group_by(treatment) %>%
  select(adopter,treatment) %>%
  summarise_all(funs(mean(., na.rm = T))) #same result

# t test on the treatment column
with(highnote_data_tc, t.test(adopter ~ treatment))

# Difference-in-means: pre-treatment covariates

names(highnote_data_tc)

# Let's calculate the mean for each covariate by treatment status

highnote_cov <- c('age', 'male', 'friend_cnt', 'avg_friend_age', 'avg_friend_male','friend_country_cnt',
                  'subscriber_friend_cnt','songsListened','lovedTracks','posts','playlists',
                  'shouts','tenure','good_country','adopter','treatment')

highnote_data_tc %>%
  group_by(treatment) %>%
  select(one_of(highnote_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))


# Propensity score estimation
# We estimate the propensity score by running a logit model (probit also works) 
# where the outcome variable is a binary variable indicating treatment status. 

#transforming the variable songsListened
highnote_data_tc <- highnote_data_tc %>% mutate(songsListened_1k = songsListened / 1000)
m_ps <- glm(treatment ~ age + male + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt +
              songsListened_1k + lovedTracks + posts + playlists +
              shouts + tenure + good_country,family = binomial(), data = highnote_data_tc)
summary(m_ps)


# Using this model, we can now calculate the propensity score for each user 
# It is simply the user’s predicted probability of being Treated, 
# given the estimates from the logit model.

prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     treatment = m_ps$model$treatment)
head(prs_df)

# Examining the region of common support
# After estimating the propensity score, it is useful to plot histograms of the estimated propensity scores by treatment status

labs <- paste("Number of Premium friends of User", c(">=1", "=0"))
prs_df %>%
  mutate(treatment = ifelse(treatment == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~treatment) +
  xlab("Probability of having atleast one subscriber friend") +
  theme_bw()



# The method we use below is to find pairs of observations that have very similar propensity scores, 
# but that differ in their treatment status. We use the package MatchIt for this. 
# This package estimates the propensity score in the background and then matches observations based 
# on the method of choice (“nearest” in this case).

highnote_data_tc_nomiss <- highnote_data_tc %>%  # MatchIt does not allow missing values
  select(adopter, treatment, one_of(highnote_cov)) %>%
  na.omit()

#taking logarithmic transformations - from here onwards the iterations are different
highnote_data_tc_nomiss$log_friend_cnt <- log(highnote_data_tc_nomiss$friend_cnt + 1)
highnote_data_tc_nomiss$log_friend_country_cnt <- log(highnote_data_tc_nomiss$friend_country_cnt + 1)
highnote_data_tc_nomiss$log_songsListened <- log(highnote_data_tc_nomiss$songsListened + 1)
highnote_data_tc_nomiss$log_lovedTracks <- log(highnote_data_tc_nomiss$lovedTracks + 1)
highnote_data_tc_nomiss$log_posts <- log(highnote_data_tc_nomiss$posts + 1)
highnote_data_tc_nomiss$log_playlists <- log(highnote_data_tc_nomiss$playlists + 1)
highnote_data_tc_nomiss$log_shouts <- log(highnote_data_tc_nomiss$shouts + 1)
highnote_data_tc_nomiss$log_age <- log(highnote_data_tc_nomiss$age + 1)
highnote_data_tc_nomiss$log_avg_friend_age <- log(highnote_data_tc_nomiss$avg_friend_age + 1)


mod_match <- matchit(treatment ~ log_age + male + log_friend_cnt + log_avg_friend_age + avg_friend_male +
                                log_friend_country_cnt + log_songsListened + log_lovedTracks + log_posts + 
                       log_playlists + log_shouts + tenure + good_country,
                     method = "nearest", data = highnote_data_tc_nomiss)

# We can get some information about how successful the matching was using summary(mod_match) and plot(mod_match)

summary(mod_match)

plot(mod_match)

# To create a dataframe containing only the matched observations, use the match.data() function

dta_m <- match.data(mod_match)
dim(dta_m)
head(dta_m)
# The final dataset contains a variable called distance, which is the propensity score.
# Examining covariate balance in the matched sample
# Visual Inspection

fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  if (variable == 'songsListened') dta$variable <- dta$variable / 10^3
  dta$treatment <- as.factor(dta$treatment)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = treatment)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}


#The graphs displayed using this function are not interpretable 
#therefore plotting 2 at a time
# grid.arrange(
#   fn_bal(dta_m, "Age"),
#   fn_bal(dta_m, "Male") + theme(legend.position = "none"),
#   fn_bal(dta_m, "Number_of_Friends"),
#   fn_bal(dta_m, "Average_Age_of_Friends") + theme(legend.position = "none"),
#   fn_bal(dta_m, "Proportion_of_Male_Friends"),
#   fn_bal(dta_m, "Number_of_Countries_Friends") + theme(legend.position = "none"),
#   fn_bal(dta_m, "Songs_Listened"),
#   fn_bal(dta_m, "Loved_Tracks") + theme(legend.position = "none"),
#   fn_bal(dta_m, "Posts"),
#   fn_bal(dta_m, "Playlists") + theme(legend.position = "none"),
#   fn_bal(dta_m, "Shouts"),
#   fn_bal(dta_m, "Tenure") + theme(legend.position = "none"),
#   fn_bal(dta_m, "US_UK_Germany"),
#   nrow = 7
# )
names(dta_m)
#grid 1
grid.arrange(
  fn_bal(dta_m, "log_age"),
  fn_bal(dta_m, "male") + theme(legend.position = "none"),
  nrow = 2 
)
#grid 2
grid.arrange(
  fn_bal(dta_m, "log_friend_cnt"),
  fn_bal(dta_m, "log_avg_friend_age") + theme(legend.position = "none"),
  nrow = 2 
)
#grid 3
grid.arrange(
  fn_bal(dta_m, "avg_friend_male"),
  fn_bal(dta_m, "log_friend_country_cnt") + theme(legend.position = "none"),
  nrow = 2 
)

#grid 4
grid.arrange(
  fn_bal(dta_m, "log_songsListened"),
  fn_bal(dta_m, "log_lovedTracks") + theme(legend.position = "none"),
  nrow = 2 
)

#grid 5
grid.arrange(
  fn_bal(dta_m, "log_posts"),
  fn_bal(dta_m, "log_playlists") + theme(legend.position = "none"),
  nrow = 2 
)

#grid 6
grid.arrange(
  fn_bal(dta_m, "log_shouts"),
  fn_bal(dta_m, "tenure") + theme(legend.position = "none"),
  nrow = 2 
)

#grid 7
fn_bal(dta_m, "good_country")

new_cov <- c('log_age', 'male', 'log_friend_cnt', 'log_avg_friend_age', 'avg_friend_male','log_friend_country_cnt',
             'log_songsListened','log_lovedTracks','log_posts','log_playlists',
                  'log_shouts','tenure','good_country','treatment')

# Difference of means
names(dta_m)

dta_m %>%
  group_by(treatment) %>%
  select(one_of(new_cov)) %>%
  summarise_all(funs(mean))

lapply(new_cov, function(v) {
  t.test(dta_m[, v] ~ dta_m$treatment)
}) ## does not work for me - i will just use my own t test code which takes one variable at a time
head(dta_m)


## add code for t test

# t tests for difference of means

new_cov <- c('log_age', 'male', 'log_friend_cnt', 'log_avg_friend_age', 'avg_friend_male','log_friend_country_cnt',
             'log_songsListened','log_lovedTracks','log_posts','log_playlists',
             'log_shouts','tenure','good_country','treatment')

#age
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(log_age, treatment)
t.test(log_age ~ treatment, data = df)

#male
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(male, treatment)
t.test(male ~ treatment, data = df)

#friend_cnt
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(log_friend_cnt, treatment)
t.test(log_friend_cnt ~ treatment, data = df)

#avg_friend_age
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(log_avg_friend_age, treatment)
t.test(log_avg_friend_age ~ treatment, data = df)

#avg_friend_male
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(avg_friend_male, treatment)
t.test(avg_friend_male ~ treatment, data = df)

#friend_country_cnt
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(log_friend_country_cnt, treatment)
t.test(log_friend_country_cnt ~ treatment, data = df)

#songsListened
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(log_songsListened, treatment)
t.test(log_songsListened ~ treatment, data = df)

#lovedTracks
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(log_lovedTracks, treatment)
t.test(log_lovedTracks ~ treatment, data = df)

#posts
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(log_posts, treatment)
t.test(log_posts ~ treatment, data = df)

#playlists
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(log_playlists, treatment)
t.test(log_playlists ~ treatment, data = df)

#shouts
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(log_shouts, treatment)
t.test(log_shouts ~ treatment, data = df)

#tenure
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(tenure, treatment)
t.test(tenure ~ treatment, data = df)

#good_country
df <- dta_m %>%
  filter(treatment == 1 | treatment == 0) %>%
  select(good_country, treatment)
t.test(good_country ~ treatment, data = df)



# Estimating treatment effects

# Estimating the treatment effect is simple once we have 
# a matched sample that we are happy with. We can use a t-test:

with(dta_m, t.test(adopter ~ treatment)) #now the data is matched


dta_m %>%
  group_by(treatment) %>%
  select(adopter,treatment) %>%
  summarise_all(funs(mean(., na.rm = T)))

# Or we can use OLS with or without covariates:

lm_treat1 <- lm(adopter ~ treatment, data = dta_m)
summary(lm_treat1)

new_cov <- c('log_age', 'male', 'log_friend_cnt', 'log_avg_friend_age', 'avg_friend_male','log_friend_country_cnt',
             'log_songsListened','log_lovedTracks','log_posts','log_playlists',
             'log_shouts','tenure','good_country','treatment')

lm_treat2 <- lm(adopter ~ treatment + log_age + male + log_friend_cnt + log_avg_friend_age + avg_friend_male + 
                  log_friend_country_cnt + log_songsListened
                + log_lovedTracks + log_posts + log_playlists +
                  log_shouts + tenure + good_country , data = dta_m)
summary(lm_treat2)

#propensity desnity after matching
m_ps <- glm(treatment ~ log_age + male + log_friend_cnt + log_avg_friend_age + avg_friend_male + 
              log_friend_country_cnt + log_songsListened
            + log_lovedTracks + log_posts + log_playlists +
              log_shouts + tenure + good_country,family = binomial(), data = dta_m)
summary(m_ps)


# Using this model, we can now calculate the propensity score for each student. 
# It is simply the student’s predicted probability of being Treated, 
# given the estimates from the logit model.

prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     treatment = m_ps$model$treatment)
head(prs_df)

labs <- paste("Number of Premium friends of User", c(">=1", "=0"))
prs_df %>%
  mutate(treatment = ifelse(treatment == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~treatment) +
  xlab("Probability of having atleast one subscriber friend") +
  theme_bw()


##################################################################################
##                                  QUESTION 4                                  ##
##################################################################################

#=================================================================================#
####                          LOGISTIC REGRESSION MODEL                        ####
#=================================================================================#

names(dta_m)
#[1] "adopter"                "treatment"              "age"                    "male"                   "friend_cnt"            
#[6] "avg_friend_age"         "avg_friend_male"        "friend_country_cnt"     "subscriber_friend_cnt"  "songsListened"         
#[11] "lovedTracks"            "posts"                  "playlists"              "shouts"                 "tenure"                
#[16] "good_country"           "log_friend_cnt"         "log_friend_country_cnt" "log_songsListened"      "log_lovedTracks"       
#[21] "log_posts"              "log_playlists"          "log_shouts"             "log_age"                "log_avg_friend_age"    
#[26] "distance"               "weights"  

#plotting histogram of all variables together
hist.data.frame(dta_m)

#considering the same variables that i used for matching along with treatment column (instead of subscriber_friend_cnt column)

#iteration 1

model_4_a <- glm(adopter ~ log_age + male + log_friend_cnt + log_avg_friend_age + avg_friend_male + log_friend_country_cnt +
                   log_songsListened + log_lovedTracks + log_posts + log_playlists + treatment + log_shouts + tenure 
                 + good_country,
                 data=dta_m, family=binomial())

summary(model_4_a)

table(dta_m$adopter)
#### MODEL 2 - Dropping Insignificant Variables from Model 1 and calculating offset value

### ofset value

# Comparing the effectiveness of advertising

# Since the analysis is conducted on a choice based sample, this biases the estimation of the constant term, while keeping the other coefficients unbiased
# We need to correct this bias in order to compute marginal effects and ROIs.
# We use an offset factor = log[((1-purchaserate_population)/purchaserate_population)/((1-purchaserate_sample)/purchaserate_sample))]

populateRate = 0.030603
purchaseRate = mean(dta_m$adopter)
purchaseRate
dta_m$offsetvalue <- log(((1 - populateRate)/populateRate)/((1 - purchaseRate)/purchaseRate))

model_4_b <- glm(adopter ~ log_age + male  + log_avg_friend_age   +
             log_songsListened + log_lovedTracks + log_posts + log_playlists + treatment + log_shouts + tenure 
           + good_country,
           data=dta_m,family=binomial(), offset = dta_m$offsetvalue)
summary(model_4_b)


### Comparison of these two models - I can compare since they are nested
anova(model_4_a, model_4_b, test="Chisq")
#p value > alpha therefore no difference in the models

# Proceeding with the second model now

#AOC curve
prob=predict(model_4_b,type=c("response"))
dta_m$prob=prob
ROC <- roc(adopter ~ prob, data = dta_m,print.auc = TRUE,plot=TRUE)
plot(ROC)  

as.numeric(ROC$auc)

###ADDITIONAL MARGINAL EFFECT CALCULATION



# Comparing the effectiveness of advertising

# Since the analysis is conducted on a choice based sample, this biases the estimation of the constant term, while keeping the other coefficients unbiased
# We need to correct this bias in order to compute marginal effects and ROIs.
# We use an offset factor = log[((1-purchaserate_population)/purchaserate_population)/((1-purchaserate_sample)/purchaserate_sample))]


#glm(formula = adopter ~ log_age + male + log_avg_friend_age + 
 #     log_songsListened + log_lovedTracks + log_posts + log_playlists + 
 #     treatment + log_shouts + tenure + good_country, family = binomial(), 
 #   data = dta_m, offset = dta_m$offsetvalue)
# Note that the only difference from the previous regression is in the magnitude of the constant term

star2 = dta_m
star2$offsetvalue=0
#This is to ensure that predictions correspond to the population purchase rate

pred = predict(model_4_b,newdata=star2,type="response") #predicted purchase probabilities
star3 = star2
star3$tenure = star3$tenure  + 0.01*mean(star3$tenure )

#increase impressions by 1% of mean value

pred_15 = predict(model_4_b,newdata=star3,type="response")

#predicted purchase probabilities with increased impressions

marginal_15 = (pred_15 - pred)/(star3$tenure - star2$tenure)
mean(marginal_15[star3$adopter==1]) #Marginal effect for sites 1-5

# ROI comparison
# Cost per thousand impressions are $25 and $20 for Sites 1-5 and Site 6, respoectively; 
# Cost per impression is $0.025 and $0.020
# Marginal effect of one impression on purchase = 0.000039 and 0.0000361
# Contribution per purchase is $1200
# Incremental contribution due to one impression = Marginal Effect * Contribution per purchase: 0.04680 and 0l.04325
# ROI = (Inc cont - Cost)/Cost: 87.2% for Sites 1-5 and 116.3% for Site 6
star=dta_m
head(star)
star$log_age_t = star$log_age * star$treatment
star$log_avg_friend_age_t = star$log_avg_friend_age * star$treatment
star$log_songsListened_t = star$log_songsListened * star$treatment
star$log_lovedTracks_t = star$log_lovedTracks *star$treatment
star$log_posts_t = star$log_posts * star$treatment
star$log_playlists_t = star$log_playlists * star$treatment
star$log_shouts_t = star$log_shouts* star$treatment
star$tenure_t = star$tenure * star$treatment


# Comparing the effectiveness of advertising

# Since the analysis is conducted on a choice based sample, this biases the estimation of the constant term, while keeping the other coefficients unbiased
# We need to correct this bias in order to compute marginal effects and ROIs.
# We use an offset factor = log[((1-purchaserate_population)/purchaserate_population)/((1-purchaserate_sample)/purchaserate_sample))]

# we are told that the purchase rate in the population is 0.153%, whereas the purchase rate in the sample is 50.29%

populateRate = 0.030603
purchaseRate = mean(purchase)

star$offsetvalue = log(((1 - populateRate)/populateRate)/((1 - purchaseRate)/purchaseRate))
reg5 = glm(adopter ~ log_age + male + log_avg_friend_age + 
             log_songsListened + log_lovedTracks + log_posts + log_playlists + 
             treatment + log_shouts + tenure + good_country
           + log_age_t + log_avg_friend_age_t + log_songsListened_t + log_lovedTracks_t +
             log_posts_t +log_playlists_t + log_shouts_t + tenure_t, family = "binomial",
           data = star, offset = offsetvalue)
summary(reg5)

# Note that the only difference from the previous regression is in the magnitude of the constant term

star2 = star
star2$offsetvalue=0
#This is to ensure that predictions correspond to the population purchase rate

pred = predict(reg5,newdata=star2,type="response") #predicted purchase probabilities
star3 = star2
star3$log_age_t = star3$log_age_t + 0.01*mean(star3$log_age_t)

#increase impressions by 1% of mean value

pred_15 = predict(reg5,newdata=star3,type="response")

#predicted purchase probabilities with increased impressions

marginal_15 = (pred_15 - pred)/(star3$log_age_t - star2$log_age_t)
mean(marginal_15[star3$treatment==1]) #Marginal effect for sites 1-5





