library(foreign)
library(dplyr)
library(magrittr)
library(tidyr)
library(lubridate)

osha <- read.dbf("osha.DBF")

accid<- read.dbf("accid.dbf")

str(accid)
# 2147 obs

length(unique(accid$ACTIVITYNO))

# 1570 unique activity NO

#examine the non-unique ones and see what we can do about it

accid$ACTIVITYNO[duplicated(accid$ACTIVITYNO)]

noneunique<- subset(accid, duplicated(accid$ACTIVITYNO) ==TRUE)

str(noneunique)

head(noneunique,20)

# for the first four record, all the entry is exactly the same

head(noneunique,10)

# but if we examine the other record, not all the entries with the same activity number are the same
# the fact that this may occur because there may be multiple individuals involved in any accident and they are injured in the same exact way
# we cannot simply removed the duplicated records

# a very important indicator of the seriouness of an accident is the degree. 1 for fatality, 2 for hospitality and 3 for non-hospitalized injury
# it will probably be worthwhile to see if the same activityno also entails the same degree of seriousness 
#if they are duplicated in the exact same way, the above statement is true

table(duplicated(accid$ACTIVITYNO) == duplicated(accid$DEGREE))

# this table tells us that it is not

# one way we can eleminate the duplication record is to accumulate the degree of seriousness and number of individuals involved in each incident.

uniqueaccid <- accid %>% group_by(ACTIVITYNO)

uniqueaccid$DEGREE %<>% as.character() %>% as.numeric()

uniqueaccid %<>% summarise(cumulativeDegree = sum(DEGREE), count = n())

head(uniqueaccid,20)
uniqueaccid %<>% mutate(averageDegree = round(cumulativeDegree/count,3))


#just to test to see if we do it correctly
uniqueaccid[uniqueaccid$ACTIVITYNO == "10096592",]

#now we have the list of unique accidents and the cumulative seriouness and the associated number of individuals involved.
# the address information is stored in osha and we can retrieve the information by matching the activty number


#in the osha documentation, the following are the address info, state are all MA, i prefer to keep state info even it seems unnecessary
#SITE STREET		Street Address
#SITE STATE			State Code (alpha postal abbreviation)
#SITE ZIP				United States Postal Zip Code
#SITE CITY CODE	Department of Commerce City Code
#SITE CNTY CODE

# since the primary purpose of this task is to retrieve the relevant information, NAs and duplications are no less of a concern here. we will worry about
# it later


glimpse(osha)
#osha  %<>% select(ACTIVITYNO,SITEZIP,SITECITY,SITECNTY)

#we may want to keep the date information, lets examine all the dates avaliable

str(osha$OSHA1MOD)
str(osha$OPENDATE)
str(osha$CLOSEDT)
str(osha$CLOSEDT2)
str(osha$CLOSEDATE)
str(osha$CLOSEDATE2)
str(osha$OPENDT)

# the opendate and opendt seems the same, lets see

datetest1 <- ymd(osha$OPENDT)

datetest2 <- ymd(osha$OPENDATE)

table(datetest1 == datetest2)

# they are the same. apply the same test for closedate

datetest1 <- ymd(osha$CLOSEDT)

datetest2 <- ymd(osha$CLOSEDATE)
    