require(ggplot2)
require(tm)
require(lubridate)
library(dplyr)
library(stringr)

trim.whitespace<-function(x){gsub("^\\s+|\\s+$","",x)}
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
setwd("Y:././Laura/AMIA_Twitter/")

#################################
## Load in Data and Format
#################################

symplur_amia2011_clean <- read.delim("symplur_amia2011_clean.txt", header=F, quote="")
names(symplur_amia2011_clean)<-c("date","user","tweet")
## Adjust Date/Time
symplur_amia2011_clean$date<-trim.whitespace(symplur_amia2011_clean$date)
symplur_amia2011_clean$date<-gsub("PDT ","",symplur_amia2011_clean$date)
symplur_amia2011_clean$date<-as.POSIXlt(as.character(symplur_amia2011_clean$date), format="%a %b %d %H:%M:%S %Y",tz="America/Los_Angeles")
symplur_amia2011_clean$date<-with_tz(symplur_amia2011_clean$date,tzone="America/New_York")
# Add Year placeholder
symplur_amia2011_clean$year<-rep("2011",nrow(symplur_amia2011_clean))

symplur_amia2012_clean <- read.delim("symplur_amia2012_clean.txt", header=F, quote="")
names(symplur_amia2012_clean)<-c("date","user","tweet")
## Adjust Date/Time
symplur_amia2012_clean$date<-trim.whitespace(symplur_amia2012_clean$date)
symplur_amia2012_clean$date<-gsub("PDT ","",symplur_amia2012_clean$date)
symplur_amia2012_clean$date<-as.POSIXlt(as.character(symplur_amia2012_clean$date), format="%a %b %d %H:%M:%S %Y",tz="America/Los_Angeles")
symplur_amia2012_clean$date<-with_tz(symplur_amia2012_clean$date,tzone="America/Chicago")
# Add Year placeholder
symplur_amia2012_clean$year<-rep("2012",nrow(symplur_amia2012_clean))

symplur_amia2013_clean <- read.delim("symplur_amia2013_clean.txt", header=F, quote="")
names(symplur_amia2013_clean)<-c("user","tweet","date")
# Reorder to be the same as 2011 and 2012
symplur_amia2013_clean<-symplur_amia2013_clean[,c("date","user","tweet")]
##Adjust Date/Time
symplur_amia2013_clean$date<-trim.whitespace(symplur_amia2013_clean$date)
symplur_amia2013_clean$date<-gsub("PST ","",symplur_amia2013_clean$date)
symplur_amia2013_clean$date<-as.POSIXlt(as.character(symplur_amia2013_clean$date), format="%a %b %d %H:%M:%S %Y",tz="America/Los_Angeles")
symplur_amia2013_clean$date<-with_tz(symplur_amia2013_clean$date,tzone="America/New_York")
# Add Year Placeholder
symplur_amia2013_clean$year<-rep("2013",nrow(symplur_amia2013_clean))

amia_tweets<-rbind(symplur_amia2011_clean,symplur_amia2012_clean,symplur_amia2013_clean)
amia_tweets$year<-as.factor(amia_tweets$year)
amia_tweets$user<-as.factor(amia_tweets$user)
amia_tweets$date<-as.POSIXct(amia_tweets$date)
amia_tweets$tweetid<-c(1:nrow(amia_tweets))
amia_tweets_df<-tbl_df(amia_tweets)

#################################
## Content Type
#################################

# RT's at start of tweet - assume no additional comment
# 4,840 tweets
amia_tweets_rt_start<-amia_tweets_df %>% filter(grepl('^RT |^rt ',tweet))
amia_tweets_df<- amia_tweets_df %>% mutate(rt_start=grepl('^RT |^rt ',tweet))
ggplot(amia_tweets_df,aes(x=year,fill=rt_start))+geom_histogram(position="dodge")+scale_fill_manual(values=cbbPalette[2:4],name="",breaks=c("TRUE", "FALSE"),labels=c("Retweet Only", "New or Modified Content"))+theme(legend.position="bottom")

# New Content Only
amia_tweets_no_rmt<-amia_tweets_df %>% filter(!grepl('^RT |^rt | RT | rt |^MT |^mt | MT | mt ',tweet))
amia_tweets_df<- amia_tweets_df %>% mutate(no_rmt=!grepl('^RT |^rt | RT | rt |^MT |^mt | MT | mt ',tweet))
ggplot(amia_tweets_df,aes(x=year,fill=no_rmt))+geom_histogram(position="dodge")+scale_fill_manual(values=cbbPalette[2:4],name="",breaks=c("TRUE", "FALSE"),labels=c("New Content", "RT's or MT's"))+theme(legend.position="bottom")

# By type
amia_tweets_df<-amia_tweets_df %>% mutate(content_type=ifelse(no_rmt==TRUE,yes = "no_rmt",no = ifelse(rt_start==TRUE,yes = "rt_only",no = "modified_rmt")))
amia_tweets_df$content_type<-factor(amia_tweets_df$content_type,levels=c("modified_rmt","rt_only","no_rmt"))
ggplot(amia_tweets_df,aes(x=year,fill=content_type))+geom_histogram(position="dodge")+scale_fill_manual(values=cbbPalette[2:4],name="",breaks=c( "modified_rmt","rt_only","no_rmt"),labels=c("Modified Content","Retweeted Content","New Content"))+theme(legend.position="bottom")

  

#################################
## Discussions
#################################

amia_tweets_discussion<-amia_tweets_df %>% filter(no_rmt==TRUE) %>% mutate(tweet=as.character(tweet)) %>%  mutate(user_mention=grepl('@[0-9,a-z,A-Z]+ ',tweet),user_mention_count=str_count(string = tweet,pattern = '@[0-9,a-z,A-Z]+ |@[0-9,a-z,A-Z]+$')) %>% mutate(user_mention=factor(user_mention,levels=c(TRUE,FALSE)))

ggplot(amia_tweets_discussion,aes(x=year,fill=user_mention))+geom_histogram(position="dodge")+scale_fill_manual(values=cbbPalette[2:4],name="",breaks=c("TRUE", "FALSE"),labels=c("Mentions User", "No mentions"))+theme(legend.position="bottom")+ggtitle("New Content Only")


#################################
## Weblinks
#################################

amia_tweets_links<-amia_tweets_df %>% filter(rt_start==F) %>% mutate(tweet=as.character(tweet))  %>% mutate(weblink=grepl('http|www|t.co',tweet))
  

