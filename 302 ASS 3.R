states <- as.data.frame(state.x77)
names(states)[names(states) == "Life Exp"] <- "Life.Exp"
names(states)[names(states) == "HS Grad"] <- "HS.Grad"

states <- within(states,Density<-1000*Population/Area)#Create density
states <- within(states,Region<-state.division)#create region

matrix(c(cor(states$Life.Exp,states$Density),cor(states$Life.Exp,states$Income),cor(states$Life.Exp,states$Murder),cor(states$Life.Exp,states$HS.Grad),cor(states$Life.Exp,states$Frost)),dimnames = list(c("Density", "Income", "Murder", "HS Grad", "Frost"),c("Life Expectancy")))
#create matrix of covirance 

with(states,hist(Life.Exp))
with(states,hist(Density))
with(states,hist(Income))
with(states,hist(Murder))
with(states,hist(HS.Grad))
with(states,hist(Frost))# creat 6 histogram of each variable

with(states,plot(Density,Life.Exp))
with(states,plot(Income,Life.Exp))
with(states,plot(Murder,Life.Exp))
with(states,plot(HS.Grad,Life.Exp))
with(states,plot(Frost,Life.Exp))
with(states,boxplot(Life.Exp~Region)) # create 5 plot and 1 box plot

with(states,plot(log(Density),Life.Exp))# using log(Density)

states <- within(states,East_South_Central <- factor(ifelse((Region == "East South Central"),1,0)))
states <- within(states,Pacific <- factor(ifelse((Region == "Pacific"),1,0)))
states <- within(states,Mountain <- factor(ifelse((Region == "Mountain"),1,0)))
states <- within(states,West_South_Central <- factor(ifelse((Region == "West South Central"),1,0)))
states <- within(states,New_England <- factor(ifelse((Region == "New England"),1,0)))
states <- within(states,South_Atlantic <- factor(ifelse((Region == "South Atlantic"),1,0)))
states <- within(states,East_North_Central <- factor(ifelse((Region == "East North Central"),1,0)))
states <- within(states,West_North_Central <- factor(ifelse((Region == "West North Central"),1,0)))
states <- within(states,Middle_Atlantic <- factor(ifelse((Region == "Middle Atlantic"),1,0)))
#Create 9 indicator variables

fit.full <- lm(Life.Exp ~ Income + Murder + HS.Grad + Frost + log(Density) + East_South_Central +Pacific+Mountain+West_South_Central+New_England+South_Atlantic+East_North_Central+West_North_Central+Middle_Atlantic,data=states)
summary(fit.full)# full test

fit.full1 <- lm(Life.Exp ~ Income + Murder + HS.Grad + Frost + log(Density) + East_South_Central +Pacific+Mountain+West_South_Central+New_England+South_Atlantic+East_North_Central+West_North_Central+Middle_Atlantic,data=states)
anova(fit.full1) #full test without intercept


fit.reduce <- lm(Life.Exp ~ Income + Murder + HS.Grad + Frost + log(Density) + West_North_Central ,data=states)
summary(fit.reduce)#reduce test

anova(fit.reduce,fit.full)# anova



fit.reduce3 <- lm(Life.Exp ~ Murder + HS.Grad +Frost+ West_North_Central,data=states)
anova(fit.reduce3,fit.reduce)#another partial test and anova

fit.reduce4 <- lm(Life.Exp ~ Murder + HS.Grad + Frost +West_North_Central, data=states)
fit.reduce4#same as above

par(mfrow=c(1,1))#show 2*1 plots
plot(fit.reduce4, 1); plot(fit.reduce4, 2)# residuals vs. fitted values and a Normal QQ plot

plot(abs(dffits(fit.reduce4)),hatvalues(fit.reduce4))#leverage vs. DFFITs(absolute value)
require(calibrate)
text(abs(dffits(fit.reduce4)),hatvalues(fit.reduce4),row.names(states))#label each point with its state name



#####################################################################################################

require(xlsx)
tnf100M <- read.xlsx2("C:\\Users\\Lenovo\\Desktop\\100mM.xlsx",1)
tnf100W <- read.xlsx2("C:\\Users\\Lenovo\\Desktop\\100mW.xlsx", 1)#read excel

tnf100M <- within(tnf100M,indicator<- rep(c(1),2639))#create indicator variables
tnf100W <- within(tnf100W,indicator<- rep(c(0),1918))#create indicator variables

athelet <- rbind(tnf100M,tnf100W)#bind them

athelet$time[grepl("A",athelet$time )]<-"10.00A"#change any time contains "A" to a specific time
athelet2 <- subset(athelet, time!="10.00A")#delete all these times

athelet3 <- subset(athelet2,select=c("country","date","wind","birth","time","indicator"))#select these columns

athelet3$date <- as.Date(athelet3$date,"%d.%m.%Y" )
athelet3$birth <- as.Date(athelet3$birth,"%d.%m.%y")# Change the format
athelet3$birth <- as.Date(ifelse(athelet3$birth > Sys.Date(), format(athelet3$birth, "19%y-%m-%d"), format(athelet3$birth)))
#change some wrong time to correct times

athelet4 <- within(athelet3, Year <- factor(gsub("-.*", "", date)))#change year format
athelet4 <- within(athelet4,Month <- factor(month.abb[as.numeric(gsub(".*-", "", factor(substr(athelet3$date,1,7))))]), levels = month.abb)
#change month format
athelet4 <- within(athelet4, age<- as.numeric(round((date-birth)/365.25,4)))
#calculate age
athelet4 <- subset(athelet4,age>0)
# choose only age > 0 

athelet4$wind[grepl(",",athelet4$wind)]<-"."
athelet4$wind[grepl("0.0",athelet4$wind )]<-"0"# change those to formal format

athelet5 <- subset(athelet4,wind!="")#delect empty wind
athelet5 <- within(athelet5,time<- ifelse(indicator==1,as.numeric(as.character(time))+0.05*as.numeric(as.character(wind)),as.numeric(as.character(time))+0.06*as.numeric(as.character(time))))
#calculate new time
tnf1500M<-read.csv("C:\\Users\\Lenovo\\Desktop\\1500mM.csv")
tnf1500W<-read.csv("C:\\Users\\Lenovo\\Desktop\\1500mW.csv")

tnf1500M <- within(tnf1500M,indicator<- rep(c(1),7666))
tnf1500W <- within(tnf1500W,indicator<- rep(c(0),2275))#create indicator variables

athe <- rbind(tnf1500M,tnf1500W)#bind them

athe$time[grepl("A",athe$time )]<-"4:04.97A"
athe2 <- subset(athe, time!="4:04.97A")# change all "A" in specific time and delete

athe3 <- subset(athe2,select=c("country","date","birth","time","indicator"))#select those columns

athe3$date <- as.Date(athe3$date,"%d.%m.%Y" )
athe3$birth <- as.Date(athe3$birth,"%d.%m.%y")#change format
athe3$birth <- as.Date(ifelse(athe3$birth > Sys.Date(), format(athe3$birth, "19%y-%m-%d"), format(athe3$birth)))
#give correct form of date
athe4 <- within(athe3, Year <- factor(gsub("-.*", "", date)))
athe4 <- within(athe4,Month <- factor(month.abb[as.numeric(gsub(".*-", "", factor(substr((sapply(athe3$date,as.character)),1,7))))]), levels = month.abb)
#change format
athe4 <- within(athe4, age<-as.numeric(round((date-birth)/365.25,2)))
athe4 <- subset(athe4,age>0)
#calculate age and choose >0


athe4<-within(athe4,time<-as.numeric(substr(athe4$time,1,2))*60+as.numeric(substr(athe4$time,4,8)))
#change age to seconds
athe4<-subset(athe4,time>0)#select > 0 

athe5<-within(athe4,type<-rep(c("1500M"),9077))#create type
athelet6<-within(athelet5,type<-rep(c("100M"),4268))#create type
athelet6<-subset(athelet6,select = c("country","date","birth","time","indicator","Year","Month","age","type"))
#select those columns
Race <- rbind(athelet6,athe5)#bind them
summary(Race)#summary

Race2 <- within(Race,x<-rep(1,13345))

Race2 <- within(Race2,sex<-(ifelse(indicator==1,"Male","Female")))
require(plyr)
Race_Month <- ddply(subset(Race2,type=="100M"), c("sex", "Month"), summarize, top_times = sum(x))
#100M month top times

Race_Month2$Month<-factor(Race_Month2$Month,levels=month.abb)#change level of factor
Race_Month3<-Race_Month2[order(Race_Month2$Month),]#change to monthly order


require(ggplot2)
ggplot(Race_Month3, aes(x= Month, y= top_times, fill= sex)) + geom_bar(stat="identity") + labs(y= "Top Score")
#graph 100M stack plot
Race_MonthA <- ddply(subset(Race2,type=="1500M"), c("sex", "Month"), summarize, top_times = sum(x))
Race_MonthA$ Month<-factor(Race_MonthA$Month,levels=month.abb)
ggplot(Race_Month2[order(Race_MonthA$Month),], aes(x= Month, y= top_times, fill= sex)) + geom_bar(stat="identity") + labs(y= "1500M Top Score")
#graph 1500M stack plot
table <- data.frame(with(Race2, tapply(x, list(country, sex), sum)))
#create 2*1 form
table[is.na(table)] <- 0#change NA to 0
table2<-within(table,sum<-as.numeric(Female)+as.numeric(Male))#sum male and female
table3<-table2[order(-table2$sum),]#decreasing order
head(table3,25)#choose top 25

Race_rela <-  ddply(subset(Race2,type=="100M"), c("country", "sex"), summarize, OneHundred_times = sum(x))
#100M top times
Race_rela2 <-  ddply(subset(Race2,type=="1500M"), c("country", "sex"), summarize, fifteenHundred_times = sum(x))
#1500M top times

Race_rela4<-merge(Race_rela,Race_rela2,by=c("country","sex"))
Race_rela4# give those != 0

Male<-subset(Race_rela4,sex=="Male")#subset of male
Female<-subset(Race_rela4,sex=="Female")#subset of female

with(Male,plot(log(OneHundred_times),log(fifteenHundred_times),col="red",pch = 16))#graph plot
abline(lm(log(Male$fifteenHundred_times) ~ log(Male$OneHundred_times)))#draw fit line
par(new=TRUE)
with(Female,points(log(OneHundred_times),log(fifteenHundred_times),col="green"))#draw new plot in same plot
abline(lm(log(Female$fifteenHundred_times) ~ log(Female$OneHundred_times)))#draw fit line

Race_rela5 <- within(Race_rela4,sex<-(ifelse(sex=="Male",1,0)))
fit2_full <- lm(log(fifteenHundred_times) ~ log(OneHundred_times) + sex,data=Race_rela5)
summary(fit2_full)

fit2_reduce <- lm(log(fifteenHundred_times) ~ log(OneHundred_times) ,data=Race_rela5)#reduce model 1
fit2_reduce
anova(fit2_reduce,fit2_full)

fit2_reduce2 <- lm(log(fifteenHundred_times) ~  sex,data=Race_rela5)#reduce model 2
anova(fit2_reduce2,fit2_full)
