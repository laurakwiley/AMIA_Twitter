#################################
## Libraries and Functions
#################################

## Libraries
require(wordcloud)
require(ggplot2)
require(tm)
require(lubridate)
require(dplyr)

## Functions
trim.whitespace<-function(x){gsub("^\\s+|\\s+$","",x)}

#################################
## Load in Data and Format
#################################

symplur_amia2011_clean <- read.delim("symplur_amia2011_clean.txt", header=F, quote="")
names(symplur_amia2011_clean)<-c("date","user","tweet")
symplur_amia2012_clean <- read.delim("symplur_amia2012_clean.txt", header=F, quote="")
names(symplur_amia2012_clean)<-c("date","user","tweet")
symplur_amia2013_clean <- read.delim("symplur_amia2013_clean.txt", header=F, quote="")
names(symplur_amia2013_clean)<-c("user","tweet","date")
symplur_amia2013_clean<-symplur_amia2013_clean[,c("date","user","tweet")]

## Adjust to correct date

# Remove Whitespace
symplur_amia2011_clean$date<-trim.whitespace(symplur_amia2011_clean$date)
symplur_amia2012_clean$date<-trim.whitespace(symplur_amia2012_clean$date)
symplur_amia2013_clean$date<-trim.whitespace(symplur_amia2013_clean$date)

# Split by timezone
symplur_amia2012_clean_pdt<-symplur_amia2012_clean[grepl(pattern="PDT",symplur_amia2012_clean$date),]
symplur_amia2012_clean_pst<-symplur_amia2012_clean[grepl(pattern="PST",symplur_amia2012_clean$date),]

# Remove timezone notation
symplur_amia2011_clean$date<-gsub("PDT ","",symplur_amia2011_clean$date)
symplur_amia2012_clean_pdt$date<-gsub("PDT ","",symplur_amia2012_clean_pdt$date)
symplur_amia2012_clean_pst$date<-gsub("PST ","",symplur_amia2012_clean_pst$date)
symplur_amia2013_clean$date<-gsub("PST ","",symplur_amia2013_clean$date)

# Format to Posix - First set time to PDT
symplur_amia2011_clean$date<-as.POSIXct(symplur_amia2011_clean$date,format="%a %b %d %H:%M:%S %Y",tz="America/New_York")+(3*60^2)
symplur_amia2012_clean_pdt$date<-as.POSIXct(symplur_amia2012_clean_pdt$date,format="%a %b %d %H:%M:%S %Y",tz="America/Chicago")+(2*60^2)
symplur_amia2012_clean_pst$date<-as.POSIXct(symplur_amia2012_clean_pst$date,format="%a %b %d %H:%M:%S %Y",tz="America/Chicago")+(3*60^2)
symplur_amia2013_clean$date<-as.POSIXct(symplur_amia2013_clean$date,format="%a %b %d %H:%M:%S %Y",tz="America/New_York")+(4*60^2)

# Remerge 2012 Data
symplur_amia2012_clean<-rbind(symplur_amia2012_clean_pdt,symplur_amia2012_clean_pst)




