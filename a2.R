#### A1 ####
### Marriage Licenses ###
require(plyr)
wed <- read.csv("marriage.csv", head=T)
str(wed)

wed <- within(wed, {
  Year <- factor(gsub("-.*", "", TIME_PERIOD))
  Month <- factor(month.abb[as.numeric(gsub(".*-", "", TIME_PERIOD))], levels = month.abb)
})
wed.agg <- ddply(wed, .(Year, Month), summarize, MarriageLic = sum(MARRIAGE_LICENSES))

# 1D table
wed.tab1 <- with(wed, tapply(MARRIAGE_LICENSES, CIVIC_CENTRE, sum)); wed.tab1

# Barplot
wed.bar <- arrange(ddply(wed, "CIVIC_CENTRE", summarize, MarriageLic = sum(MARRIAGE_LICENSES)), -MarriageLic)
with(wed.bar, barplot(MarriageLic, names.arg=CIVIC_CENTRE, main="Marriage license count by Region"))

# 2D table
wed.tab2 <- with(wed, tapply(MARRIAGE_LICENSES, list(Year, Month), sum)); wed.tab2

# Stacked bar
wed.bar2 <- ddply(wed, c("CIVIC_CENTRE", "Month"), summarize, MarriageLic = sum(MARRIAGE_LICENSES))
require(ggplot2) # I like these plots better than base R graphics. Also easier to code
ggplot(wed.bar2, aes(x= Month, y= MarriageLic, fill= CIVIC_CENTRE)) + geom_bar(stat="identity") + labs(y= "Marriage Licenses")
# barplot(xtabs(MarriageLic ~ CIVIC_CENTRE + Month, data=wed.bar))
# barplot(with(wed, tapply(MARRIAGE_LICENSES, list(CIVIC_CENTRE, Month), sum)), col=1:4)
# legend("topright", legend= levels(wed$CIVIC_CENTRE), fill= 1:4, title= "Civic Centres")

### Business Licenses ###
biz <- read.csv("businessLicences.csv", head=T, stringsAsFactors = F)
str(biz)

require(lubridate) # Nice package for dealing with dates, although you don't need it
biz <- within(biz, {
  #   Date <- as.Date(Issued, format = ifelse(nchar(Issued) > 8, "%d/%m/%Y", "%d/%m/%y"))
  Date <- as.Date(Issued, format = "%d/%m/%y")
  Year <- factor(year(Date))
  Month <- factor(month.abb[month(Date)], levels = month.abb)
})
biz <- subset(biz, Year %in% unique(wed$Year), select=c(Month, Year)) # Doing it here to save RAM, but could be done in join instead
biz <- droplevels(biz) # Gets rid of unused factor levels
biz.agg <- ddply(biz, .(Year, Month), summarize, BusinessLic = length(Month))

### Regression with combined data ###
licenses <- join(wed.agg, biz.agg, by = c("Year", "Month"))
rm(wed, wed.agg, biz, biz.agg, wed.bar, wed.bar2, wed.tab1, wed.tab2)

with(licenses, plot(BusinessLic, MarriageLic))
fit <- lm(MarriageLic ~ BusinessLic, data=licenses)
summary(fit)
confint(fit, level = 0.88)
newData <- data.frame(BusinessLic = 550)
predict(fit, newData, interval="prediction")

#### A2 ####
### Building Permits ###
require(plyr)
require(lubridate)
require(ggplot2)

build.raw <- read.csv("activepermits.csv", head= T)
str(build.raw)

# QA1
build <- within(build.raw, {
  Date.App <- as.Date(APPLICATION_DATE, format = "%Y/%m/%d")
  Date.Iss <- as.Date(ISSUED_DATE, format = "%Y/%m/%d")

  Year <- factor(year(Date.App))
  Month <- factor(month.abb[month(Date.App)], levels = month.abb)
  
  Days.Applied.to.Issued <- as.numeric(Date.Iss - Date.App)
  CURRENT_USE  <- toupper(CURRENT_USE)
  PROPOSED_USE <- toupper(PROPOSED_USE)
  
  CURRENT_USE_cleaned <- CURRENT_USE
})
build <- subset(build, Days.Applied.to.Issued >= 0, # & Days.Applied.to.Issued < 3*365, 
                select=c(Year, Month, Date.App, Days.Applied.to.Issued, STATUS, POSTAL,
                         CURRENT_USE, CURRENT_USE_cleaned, PROPOSED_USE, DWELLING_UNITS_CREATED, DWELLING_UNITS_LOST))
str(build)
summary(build)

# QA2
sum(as.character(build$CURRENT_USE) != as.character(build$PROPOSED_USE))

tab <- with(build, table(CURRENT_USE))
tab <- tab[order(tab)] # Put in ascending order
tab[tab >= 30] # At least 30 records

build <- within(build, {
  CURRENT_USE_cleaned[grepl("(APT)|(ARTMENT)|(CONDO)",   CURRENT_USE)] <- "Apartment"
  CURRENT_USE_cleaned[grepl("(UNIV)|(COLLEGE)|(SCHOOL)", CURRENT_USE)] <- "Educational"
  CURRENT_USE_cleaned[grepl("(RESTAURANT)|(REST)",       CURRENT_USE)] <- "Restaurant"
  CURRENT_USE_cleaned[grepl("PARKING",                   CURRENT_USE)] <- "Parking Lot"
  CURRENT_USE_cleaned[grepl("(RETAIL)|(RET)",            CURRENT_USE)] <- "Retail"
  CURRENT_USE_cleaned[grepl("(OFFICE)|(OFF)",            CURRENT_USE)] <- "Office"
  CURRENT_USE_cleaned[grepl("USE",                       CURRENT_USE)] <- "Mixed"
  CURRENT_USE_cleaned[grepl("LAB",                       CURRENT_USE)] <- "Laboratory"
  CURRENT_USE_cleaned[grepl("(IND)|(INDUSTRIAL)|(PLANT)|(MANUF)|(LUMBER)", CURRENT_USE)] <- "Industrial"
  CURRENT_USE_cleaned[grepl("(RES)|(RESIDENTIAL)",       CURRENT_USE)] <- "Residential"
  CURRENT_USE_cleaned[grepl("(SUBWAY)|(TRANSIT)|(UNION)",CURRENT_USE)] <- "Transit Station"
  CURRENT_USE_cleaned[grepl("(CHURCH)|(WORSHIP)",        CURRENT_USE)] <- "Place of Worship"
  CURRENT_USE_cleaned[grepl("(VACANT)|(VACNT)",          CURRENT_USE)] <- "Vacant"
  CURRENT_USE_cleaned[grepl("(N/A)|(NOT KNOWN)",         CURRENT_USE)] <- ""
  
  SFD <- grepl("(SFD)|(SINGLE)|(SF RES)", CURRENT_USE)
  Det <- grepl("(DETACH)|(DET)|(DETCAHED)|(DETATCHED)", CURRENT_USE)
  Sem <- grepl("SEMI", CURRENT_USE)
  Row <- grepl("(ROW)|(TOWN)", CURRENT_USE)
  
  CURRENT_USE_cleaned <- ifelse(Sem, "SFD Semi-Detached", ifelse(Det, "SFD Detached", ifelse(Row, "SFD Rowhouse", ifelse(SFD, "SFD", CURRENT_USE_cleaned))))
  
  CURRENT_USE_cleaned[grepl("(UNIT)|(TRIPLEX)|(DUPLEX)", CURRENT_USE)] <- "Multi-Unit"
  CURRENT_USE_cleaned[grepl("(GROUP)|(NURSING)|(AGED)|(LONG TERM)",  CURRENT_USE)] <- "Nursing Home"
  CURRENT_USE_cleaned[grepl("(ARENA)|(PARK)|(BOWLING)|(FITNESS)|(THEATRE)|(STADIUM)|(RECREAT)|(CLUB)",  CURRENT_USE)] <- "Nursing Home"
})
## --- Testing --- ##
# tab2 <- with(build, table(CURRENT_USE_cleaned))
# tab2 <- tab2[order(tab2)] # Put in ascending order
# tab2[tab2 >= 30] # At least 30 records
dfQA2a <- arrange(ddply(build, "CURRENT_USE_cleaned", nrow), -V1)
names(dfQA2a) <- c("Usage", "Count")
subset(dfQA2a, Count >= 30)

# QA2b
hist(build$Days.Applied.to.Issued, main="Toronto Building Permit Delays", xlab="Days")
# QA2c
build$log_days <- log(build$Days.Applied.to.Issued + 1)
hist(build$log_days, main="Toronto Building Permit Delays", xlab="log(Days)")
build$res <- factor(ifelse(build$DWELLING_UNITS_CREATED > 0, "Increase", ifelse(build$DWELLING_UNITS_LOST > 0, "Decrease", "No Change")))
summary(build$res)
# QA2d
ggplot(subset(build, !is.na(res)), aes(x= log_days, fill= res)) + geom_histogram()
# QA2e
ggplot(subset(build, !is.na(res)), aes(y= log_days, x= res)) + geom_boxplot()
# QA2f
dfQA2f <- arrange(ddply(build, "STATUS", summarize, AvgDelay = round(mean(Days.Applied.to.Issued), 0)), -AvgDelay)
dfQA2f[1:10,]

## QA3
## Word Cloud ##
# install.packages('wordcloud')
# install.packages('tagcloud')
library(wordcloud)
# library(tm)
# library(tagcloud)
tags <- ddply(subset(build, POSTAL == "M5G"), "CURRENT_USE_cleaned", nrow)
wordcloud(tags$CURRENT_USE, tags$V1, max.words=20, random.order=FALSE, rot.per=.30)

# QA4
build.sub <- subset(build, Date.App >= '2011-01-01' & Date.App <= '2015-08-31', select=c(Year, Month))
build.agg <- ddply(build.sub, .(Year, Month), summarize, BuildingPerms = length(Month))
toronto <- join(licenses, build.agg, by = c("Year", "Month"))
# a
with(toronto, plot(BuildingPerms, BusinessLic))
fit <- lm(BusinessLic ~ BuildingPerms, data= toronto)
abline(coef(fit), col="red")

# b - Residual plots
par(mfrow=c(1,2))
plot(fit, 1); plot(fit, 2)



### QB1 - A1 Submission times ###
times <- read.csv("a2data.csv", head= T)
times <- within(times, {
  time <- ifelse(MTroom %in% c("SS 2117", "SS 1069"), 17, 10) - a1hours - a1minutes/60 + 24*(15 - a1Date)
  a1 <- as.numeric(a1)
  a1_sq <- a1^2
  log_time <- log(time + 1)
})
# times[!is.na(times$time),]
str(times)
times <- subset(times, !is.na(time) & time >= 0, select=c(MTroom, a1, time, a1_sq, log_time, date.ambiguity))
par(mfrow=c(1,2))
with(times, hist(time, breaks = 30))
with(times, hist(a1,   breaks = 30))
with(times, hist(log_time, breaks = 30))
with(times, hist(a1_sq,   breaks = 30))

# SLR model
fit <- lm(a1 ~ time, data= times)
plot(fit, 1); plot(fit, 2)
par(mfrow=c(1,1))
with(times, plot(time, a1))
abline(coef(fit), col="red")

par(mfrow=c(1,2))
plot(fit, 1); plot(fit, 2)


# Transformed model
fitT <- lm(a1_sq ~ log_time, data= times)
with(times, plot(log_time, a1_sq))
abline(coef(fitT), col="red")
par(mfrow=c(1,2))
plot(fitT, 1); plot(fitT, 2)


sqrt(predict(fitT, data.frame(log_time = log(12 + 1)), interval= "confidence"))

times <- arrange(times, MTroom, time)
write.csv(subset(times, select=c(MTroom, a1, time, date.ambiguity)), "A1times.csv", row.names= F)

## Use this or not ??? ##
#### TTC ####
require(XLConnect)
require(reshape2)
require(plyr)
wb <- loadWorkbook("PM_TTC.xls")

# Riders
ttc <- readWorksheet(wb, sheet = "Data", startRow = 27, endRow = 34, rownames = T)
names(ttc) <- c("Year", month.abb)
ttc$Year <- factor(gsub("[^0-9]*", "", ttc$Year))
ttc.1 <- melt(ttc, id.vars = "Year", value.name = "TTCriders", variable.name = "Month")

# Riders target
ttc <- readWorksheet(wb, sheet = "Data", startRow = 36, endRow = 42, rownames = T, head = F)
names(ttc) <- c("Year", month.abb)
ttc$Year <- factor(gsub("[^0-9]*", "", ttc$Year))
ttc.2 <- melt(ttc, id.vars = "Year", value.name = "TTCtarget", variable.name = "Month")

# Money
ttc <- readWorksheet(wb, sheet = "Data", startRow = 64, endRow = 71, rownames = T)
names(ttc) <- c("Year", month.abb)
ttc$Year <- factor(gsub("[^0-9]*", "", ttc$Year))
ttc.3 <- melt(ttc, id.vars = "Year", value.name = "Dollars_Millions", variable.name = "Month")

# Money budget
ttc <- readWorksheet(wb, sheet = "Data", startRow = 73, endRow = 79, rownames = T, head = F)
names(ttc) <- c("Year", month.abb)
ttc$Year <- factor(gsub("[^0-9]*", "", ttc$Year))
ttc.4 <- melt(ttc, id.vars = "Year", value.name = "Budget_Millions", variable.name = "Month")

# Join together
ttc.agg <- join(ttc.1, ttc.2, by = c("Year", "Month"))
ttc.agg <- join(ttc.agg, ttc.3, by = c("Year", "Month"))
ttc.agg <- join(ttc.agg, ttc.4, by = c("Year", "Month"))
toronto <- join(toronto, ttc.agg, by = c("Year", "Month"))
rm(ttc.1, ttc.2, ttc.3, ttc.4, ttc.agg)

with(toronto, plot(TTCriders, Dollars_Millions))
with(toronto, plot(TTCtarget, TTCriders))

with(toronto, plot(BuildingPerms, Dollars_Millions))

fit <- lm(Dollars_Millions ~ TTCriders, data=toronto)
summary(fit)


fit <- lm(Dollars_Millions ~ TTCriders, data=toronto)
summary(fit)


# ITEM NAME  DESCRIPTION	FORMAT
# PERMIT_NUM	Last two digits of calendar year, plus IBMS-generated sequence	99 999999
# REVISION_NUM	Two digit number identifying revisions to permit application made after permit issuance 	99
# PERMIT_TYPE	Text field describing the type of permit.	Eg Small Residential Projects; Building Addtions/Alterations
# STRUCTURE_TYPE	Identifies that type of structure the application relates to.	Eg SFD- Detached; Apartment Building
# WORK	Overall description of the type of work covered by application.	
# STREET_NUM	Address – street number	99
# STREET_NAME	Address – street name	Upper Case
# STREET_TYPE	Address – street type	Eg AVE, ST, DR, RD
# STREET_DIRECTION	Address – street direction	Eg E, W, N, S
# POSTAL	First 3 digits of postal code	
# GEO_ID	City-defined, unique identifier for Property Address	
# APPLICATION_DATE	Date that the application was received and entered into IBMS	YYYY/MM/DD
# ISSUED_DATE	Date that the permit was issued.	YYYY/MM/DD
# COMPLETED_DATE	Date work is complete and permit is cleared	N/A for this dataset, included for compatibility purpose
# STATUS	Current status of application / permit	Eg Permit Issued; Inspection; Closed
# DESCRIPTION	Description of work proposed in application	
# CURRENT USE	Use of the property at the time of application submission	
# PROPOSED_USE	Use of the property after completion of work covered by permit	
# DWELLING_UNITS_CREATED	Number of residential dwelling units created by completion of permit work. 	
# DWELLING_UNITS_LOST	Number of residential dwelling units lost by completion of permit work.	
# ASSEMBLY	Assembly Occupancy area (in sq metres) covered by permit work. (eg Restaurant, Library, Theatre)	
# INSTITUTIONAL	Institutional Occupancy area (in sq metres) covered by permit work (eg Hospital, Nursing Home)	
# RESIDENTIAL	Residential Occupancy area (in sq metres) covered by permit work	
# BUSINESS_AND_PERSONAL_SERVICES	Business and Personal Services Occupancy area (in sq metres) covered by permit work (Office, Bank, Medical Clinic)	
# MERCANTILE	Mercantile Occupancy area (in sq metres) covered by permit work (eg Department Store, Supermarket)	
# INDUSTRIAL	Industrial Occupancy area (in sq metres) covered by permit work (eg Warehouse, Gas Station)	
# INTERIOR ALTERATIONS	Floor area (in sq metres) covered by permit work	
# DEMOLITION	Floor area (in sq metres) covered by permit work	




