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

accid$DEGREE%<>% as.character() %>% as.numeric()

uniqueaccid <- accid %>% group_by(ACTIVITYNO)


test3 <- accid %>% group_by(ACTIVITYNO) %>% filter(DEGREE == 3)
test2 <- accid %>% group_by(ACTIVITYNO) %>% filter(DEGREE == 2)
test1 <- accid %>% group_by(ACTIVITYNO) %>% filter(DEGREE == 1)

#fatality
test1 %<>% summarise(count = n())
colnames(test1) <- c("ACTIVITYNO", "Fatality")

#hospitality
test2 %<>% summarise(count = n())
colnames(test2) <- c("ACTIVITYNO", "Hospitality")

#non-hospitality
test3 %<>% summarise(count = n())
colnames(test3) <- c("ACTIVITYNO", "Non-hospitality")
uniqueaccid$DEGREE <- as.numeric(as.character(uniqueaccid$DEGREE))

uniqueaccid %<>% summarise(cumulativeDegree = sum(DEGREE), count = n())

#join them one by one
uniqueaccid <- uniqueaccid %>% left_join(test1, by = "ACTIVITYNO")
uniqueaccid <- uniqueaccid %>% left_join(test2, by = "ACTIVITYNO")
uniqueaccid <- uniqueaccid %>% left_join(test3, by = "ACTIVITYNO")

head(uniqueaccid,20)
uniqueaccid %<>% mutate(averageDegree = round(cumulativeDegree/count,3))

uniqueaccid[is.na(uniqueaccid)] <-0
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


oshaaddress <- osha[,c("ACTIVITYNO" , "SITEADD", "SITESTATE", "SITEZIP", "SITECITY", "SITECNTY" ) ]

uniqueaccid %<>% left_join(oshaaddress,by = "ACTIVITYNO")

head(uniqueaccid,30)
     
summarise(group_by(uniqueaccid,SITEZIP),count= n())



# may be we can add the city name and county name instead of the encoding

scc <- read.dbf("lookups/scc.dbf")

#by looking at scc and do some googling, the names are city names
glimpse(scc)
head(scc)

colnames(scc) <- c("TYPE", "SITESTATE", "SITECNTY", "SITECITY", "NAME")

testname <- scc[,c("SITESTATE","SITECITY","NAME")]


uniqueaccid  %<>%  left_join(testname,by = c("SITESTATE","SITECITY"))

head(uniqueaccid)

dangerouscity<-summarise(group_by(uniqueaccid,NAME),incidence = n(), total= sum(count))

arrange(dangerouscity,desc(total))


#lets map all incidents
library(ggmap)
library(ggplot2)

qmap("masschusettes")



mapdata <- uniqueaccid[,c("ACTIVITYNO", "count","Fatality","SITESTATE","NAME")]

head(mapdata)

mapdata$EXACT <- paste(mapdata$SITESTATE,mapdata$NAME)

test <- summarise(group_by(mapdata,EXACT), total = sum(count))

ma.location<- geocode(test$EXACT)

# in case we lost the data
write.table(ma.location, file = "MA_Location")
x <- read.table("MA_Location")
head(x)

test$lon <- x$lon
test$lat <- x$lat


mass<- get_map("Masschusetts")

normalize<- test$total

ggmap(mass) +geom_point(aes(x=lon, y=lat), data=test, col="orange", alpha=0.2, size=test$total)
#lets only concern about fatality.

map_fatality <- summarise(group_by(mapdata,EXACT), total = sum(Fatality))

map_fatality$lon <- x$lon
map_fatality$lat <- x$lat

ggmap(mass)+geom_point(aes(x=lon, y=lat), data=map_fatality, col="red", alpha=0.2, size=map_fatality$total)



#testing street level

testinfor <- uniqueaccid[,c( "ACTIVITYNO", "SITEADD", "SITESTATE", "NAME" )]

head(testinfor)

testinfor <- subset(testinfor, testinfor$NAME == "BOSTON")

head(testinfor)

testinfor$EXACT <- paste(testinfor$SITEADD,testinfor$NAME)

head(testinfor)

testinfo <- summarise(group_by(testinfor,EXACT), count = n())

### this piece of code takes some time to run and you may want to just read the file I saved in advance.
boston_location <- geocode(testinfo$EXACT)

write.table(boston_location, file = "Boston_location")
###



bos <- read.table("Boston_location")

testinfo$lon <- bos$lon
testinfo$lat<- bos$lat

boston <- get_map("Boston", zoom =13)

ggmap(boston) + geom_point(aes(x = lon, y= lat), data = testinfo, col = "red" , alpha = .5 , size = testinfo$count*5) + ggtitle("OSHA accidents in Boston")
 

# bar graph for jury types

table(osha$JOBTITLE)

acc<- read.dbf("lookups/acc.dbf")

ok <- subset(acc, acc$CATEGORY == "SOURC-INJ")

unique(ok$VALUE)

accid$SOURCE

head(ok)

colnames(ok) <- c("CATEGORY" ,"SOURCE", "VALUE")

ok$CATEGORY <- NULL

accid_injury<- left_join(accid, ok, by = "SOURCE")

head(accid_injury)

accid_injury<- accid_injury[,c("ACTIVITYNO","VALUE")]

accid_injury<- summarise(group_by(accid_injury,VALUE),count = n())

accid_injury$VALUE <- droplevels(accid_injury$VALUE)

ggplot(data = accid_injury, aes (count, fill = VALUE), ylab = "injury") + geom_bar(width =5) + coord_flip() + ggtitle("Types of injury summary in Mass")

