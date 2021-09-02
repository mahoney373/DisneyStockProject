
DisMovies2=read.csv(file="Movies.csv")
DisStock=read.csv(file="DIS (2).csv")
DisMovies2$Date=as.Date(DisMovies2$Date)
DisStock$Date=as.Date(DisStock$Date,"%Y-%m-%d")
DisComp=merge.data.frame(DisMovies2,DisStock,by="Date",all=TRUE)
DisComp=DisComp[,-4]
DisComp=DisComp[,-4]
DisCompNA=na.omit(DisComp)



PercentChange=function(x){
  row=which(grepl(x,DisComp$Date))
  Opening1=DisComp$Open[DisComp$Date==x]
  week=row+7
  Day7=DisComp$Close[week]
  Percent=((Day7-Opening1)/Opening1)*100
  return(Percent)
}
PercentChange2=function(x){
  row=which(grepl(x,DisComp$Date))
  Opening1=DisComp$Open[DisComp$Date==x]
  week=row-7
  Day7=DisComp$Close[week]
  Percent=((Opening1-Day7)/Day7)*100
  return(Percent)
}
OpenClose=function(x){
  Opening=DisComp$Open[DisComp$Date==x]
  Closing=DisComp$Close[DisComp$Date==x]
  Percent=((Closing-Opening)/Opening)*100
  return(Percent)
}
PercentChange3=function(x){
  row=which(grepl(x,DisComp$Date))
  Opening1=DisComp$Open[DisComp$Date==x]
  week=row+30
  Day7=DisComp$Close[week]
  Percent=((Day7-Opening1)/Opening1)*100
  return(Percent)
}
df=as.data.frame(sapply(DisCompNA$Date,OpenClose))
DisFinal1=cbind(DisCompNA,df)
df2=as.data.frame(sapply(DisCompNA$Date,PercentChange))
DisFinal2=cbind(DisFinal1,df2)
df3=as.data.frame(sapply(DisCompNA$Dat,PercentChange2))
DisFinal=cbind(DisFinal2,df3)
df4=as.data.frame(sapply(DisCompNA$Date,PercentChange3))
DisFinal=cbind(DisFinal,df4)
colnames(DisFinal)=c("Date","Event Description","Title or Event","Open","High","Low","Close","Adj Close","Volume","Day Percent Change","PC Week After","PC Week Before","PC 30 Days After")
DisFinalB00=DisFinal[DisFinal$Date < "2000-01-01",]
DisFinalA00=DisFinal[DisFinal$Date > "2000-01-01",]



DisFinal %>% summarise("Average PC Week After"=mean(`PC Week After`),
                       "Average PC Week Before"=mean(`PC Week Before`),
                       "Average PC Day Of"=mean(`Day Percent Change`))
DisFinal %>% group_by(`Event Description`) %>% 
  dplyr::summarise("Average PC Week After"=mean(`PC Week After`),
            "Average PC Week Before"=mean(`PC Week Before`),
            "Average PC Day Of"=mean(`Day Percent Change`), n=n())
DisFinalB00 %>% summarise("Average PC Week After"=mean(`PC Week After`),
                          "Average PC Week Before"=mean(`PC Week Before`),
                          "Average PC Day Of"=mean(`Day Percent Change`))
DisFinalA00 %>% summarise("Average PC Week After"=mean(`PC Week After`),
                          "Average PC Week Before"=mean(`PC Week Before`),
                          "Average PC Day Of"=mean(`Day Percent Change`))



library(ggplot2)
DisPlus=DisFinal$`PC Week After`+ DisFinal$`PC Week Before`

ggplot(data=DisStock, aes(x=Date,y=Open)) +
  geom_point(alpha=0.5,size=0.5) + geom_smooth() + ggtitle("Daily Disney Stock Prices (1962 to 2021)")

ggplot(data=DisStock[DisStock$Date<"1990-12-31",], aes(x=Date,y=Open)) +
  geom_smooth() + geom_point(alpha=0.5,size=0.5)

ggplot(data=DisStock[DisStock$Date>"1990-12-31",], aes(x=Date,y=Open)) +
  geom_point(alpha=0.5,size=0.5) + geom_smooth()

ggplot(data=DisFinal, aes(x = `Event Description`, y = `PC Week After`)) +
  geom_point(position = position_jitter(width = 0.25),size=0.6) + 
  geom_boxplot(fill = "grey80", colour = "blue",outlier.colour = NA,alpha=.2) +
  scale_x_discrete() + xlab("Event") +
  ylab("Percent Change") + ggtitle("One Week After the Event")

ggplot(data=DisFinal, aes(x = `Event Description`, y = `PC Week Before`)) +
  geom_point(position = position_jitter(width = 0.25),size=0.5) + 
  geom_boxplot(fill = "grey80", colour = "blue",outlier.colour = NA,alpha=.2) +
  scale_x_discrete() + xlab("Event") +
  ylab("Percent Change in Stock the Week Before the Event")

ggplot(data=DisFinal, aes(x = `Event Description`, y = `Day Percent Change`)) +
  geom_point(position = position_jitter(width = 0.25),size=0.5) + 
  geom_boxplot(fill = "grey80", colour = "blue",outlier.colour = NA,alpha=.2) +
  scale_x_discrete() + xlab("Event") +
  ylab("Percent Change the Day of the Event")

DisFinal$`Title or Event`[DisFinal$`PC Week After`>10]


ggplot(data=DisFinalB00, aes(x = `Event Description`, y = `PC Week After`)) +
  geom_point(position = position_jitter(width = 0.25),size=0.5) + 
  geom_boxplot(fill = "grey80", colour = "blue",outlier.colour = NA,alpha=.2) +
  scale_x_discrete() + xlab("Event") +
  ylab("Percent Change in Stock After One Week") +
  ggtitle("Before Jan. 1st, 2000")

ggplot(data=DisFinalA00, aes(x = `Event Description`, y = `PC Week After`)) +
  geom_point(position = position_jitter(width = 0.25),size=0.5) + 
  geom_boxplot(fill = "grey80", colour = "blue",outlier.colour = NA,alpha=.2) +
  scale_x_discrete() + xlab("Event") +
  ylab("Percent Change in Stock After One Week") +
  ggtitle("After Jan. 1st, 2000")



library(Rmisc)

CI(DisFinal$`PC Week After`,ci=0.95)
CI(DisFinal$`PC Week Before`,ci=0.95)
CI(DisFinal$`Day Percent Change`,ci=0.95)
CI(DisFinalA00$`PC Week After`,ci=0.95)
CI(DisFinalA00$`PC Week Before`,ci=0.95)
CI(DisFinalA00$`Day Percent Change`,ci=0.95)
CI(DisFinalB00$`PC Week After`,ci=0.95)
CI(DisFinalB00$`PC Week Before`,ci=0.95)
CI(DisFinalB00$`Day Percent Change`,ci=0.95)

t.test(DisFinalA00$`PC Week After`,DisFinalB00$`PC Week After`,var.equal=TRUE)

var.test(DisFinal$`PC Week Before`,DisFinal$`PC Week After`)

t.test(DisFinal$`PC Week Before`,DisFinal$`PC Week After`,var.equal=TRUE)

#ANOVA for full data set
disaov<-aov(`PC Week After` ~ `Event Description`, data=DisFinal)
summary(disaov)
disaov2<-aov(`PC Week Before` ~ `Event Description`, data=DisFinal)
summary(disaov2)
disaov3<-aov(`Day Percent Change` ~ `Event Description`, data=DisFinal)
summary(disaov3)

#ANOVA for data after 2000
disaov4<-aov(`PC Week After` ~ `Event Description`, data=DisFinalA00)
summary(disaov4)
disaov5<-aov(`PC Week Before` ~ `Event Description`, data=DisFinalA00)
summary(disaov5)
disaov6<-aov(`Day Percent Change` ~ `Event Description`, data=DisFinalA00)
summary(disaov6)

#ANOVA for data before 2000
disaov7<-aov(`PC Week After` ~ `Event Description`, data=DisFinalB00)
summary(disaov7)
disaov8<-aov(`PC Week Before` ~ `Event Description`, data=DisFinalB00)
summary(disaov8)
disaov9<-aov(`Day Percent Change` ~ `Event Description`, data=DisFinalB00)
summary(disaov9)
