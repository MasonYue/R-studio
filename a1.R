#### A1 ####
### Marriage Licenses ###
require(plyr)
wed <- read.csv("C:\\Users\\Lenovo\\Desktop\\marriage.csv")
wed
str(wed)

wed <- within(wed, {
  Year <- factor(gsub("-.*", "", TIME_PERIOD))
  Month <- factor(month.abb[as.numeric(gsub(".*-", "", TIME_PERIOD))], levels = month.abb)
})
within(wed, {Year <- factor(gsub("-.*", "", TIME_PERIOD))})

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