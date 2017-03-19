library(foreign)
library(dplyr)
library(magrittr)
library(tidyr)
library(lubridate)

accid <- read.dbf("accid.DBF")

osha <- read.dbf("osha.DBF")

glimpse(osha)

head(osha)

table(osha$ACTIVITYNO)

length(unique(osha$ACTIVITYNO))

label1 <- read.dbf("lookups/scc.dbf")

head(label1)

glimpse(label1)

label1$VALUE[label1$CATEGORY == "PART-BODY"]


dates <- osha$OPENDATE[1:20]

dates[1:10]

dates<- ymd(dates)



if(sum(accid$SITESTATE == "MA") == dim(accid)[1]) {accid %<>% select(-SITESTATE)} 

glimpse(accid)


parts <- label1[label1$CATEGORY == "PART-BODY",]

parts <- select(parts, CODE ,VALUE)

parts


colnames(parts) <- c("VALUE", "BODYPART")

str(parts)


d1 <- left_join(accid , parts, by = "BODYPART")

head(d1)


save(parts , file = "parts.txt")

