
library("plotrix")
print("Student Performance Analysis")

#Read data from file

studentData <- read.table(file.choose(),header=T,sep=",")
head(studentData)

#Cleaning data
summary(studentData)

class(studentData$SrNo)

class(studentData$name)

class(studentData$city)


class(studentData$gender)

class(studentData$SGPA)

class(studentData$college)


class(studentData$Freetime)

class(studentData$Social.Media)

class(studentData$Time.Table)

class(studentData$Friends)

# Removing outliers

#1. Considring sleep hours only less than 24hours

hist(studentData$Sleep)
summary(studentData$Sleep)

studentData_sub_sleep<-subset(studentData,Sleep<24)


hist(studentData_sub_sleep$Sleep)
boxplot(studentData_sub_sleep,horizontal = T)
summary(studentData_sub_sleep$Sleep)


#2. Considring study hours only less than 15 hours

hist(studentData_sub_sleep$Study.Hours)
summary(studentData_sub_sleep$Study.Hours)

studentData_sub_sleep_study<-subset(studentData_sub_sleep,Study.Hours<15)


hist(studentData_sub_sleep_study$Study.Hours)
boxplot(studentData_sub_sleep_study,horizontal = T)
summary(studentData_sub_sleep_study$Study.Hours)

##Males only

studentData_male<-subset(studentData_sub_sleep_study,gender=="M")
summary(studentData_male)

##Females only

studentData_female<-subset(studentData_sub_sleep_study,gender=="F")
summary(studentData_female)


#3.Replace all outliers with NAs

studentData$gender[studentData$gender!="M" & studentData$gender!="F"]<-NA
studentData$SGPA[studentData$SGPA>10]<-NA
studentData$Time.Table[studentData$Time.Table!="Yes" & studentData$Time.Table!="No"]<-NA
studentData$Sleep[studentData$Sleep>24]<-NA
studentData$Study.Hours[studentData$Study.Hours>15]<-NA

head(studentData)


#4.Remove NAs

mean(studentData$Sleep,na.rm = T)

summary(studentData)

studentData_removed_na<-na.omit(studentData)
head(studentData_removed_na)
summary(studentData_removed_na)
boxplot(studentData_removed_na,horizontal = T)


#Various graphs

##1. Bar charts
###Gender chart
count<-table(studentData_sub_sleep_study$gender)
count

barplot(count,names.arg = c("NA","FeMale","Male"),xlab="Gender",ylab="No of students",col = "red",
        main="Gender chart",border="black")

count1<-table(studentData_sub_sleep_study$college)
count1

barplot(count1 ,xlab="College",ylab="No of students",col = "red",main="College chart",border="black")

##2. Pie charts

###SGPA chart

a<-table(studentData_sub_sleep_study$SGPA>=9)["TRUE"]
b<-table(studentData_sub_sleep_study$SGPA<9 & studentData_sub_sleep_study$SGPA>=8)["TRUE"]
c<-table(studentData_sub_sleep_study$SGPA<8 & studentData_sub_sleep_study$SGPA>=7)["TRUE"]
d<-table(studentData_sub_sleep_study$SGPA<7 & studentData_sub_sleep_study$SGPA>=6)["TRUE"]
e<-table(studentData_sub_sleep_study$SGPA<6 & studentData_sub_sleep_study$SGPA>=5)["TRUE"]

x<-c(a,b,c,d,e)
piepercent<- round(100*x/sum(x), 1)

pie(x, labels = piepercent, main = "SGPA pie chart",col = rainbow(length(x)))
legend("topright", c("9 and above","8-9","7-8","6-7","5-6"), cex = 1,
       fill = rainbow(length(x)))

###3D pie chart of freetime activities
count2<-table(studentData_sub_sleep_study$Freetime)
count2
lbs<-c("Job","Movies","Play","Read","TV")
pie3D(x,labels=lbs,explode = 0.1,shade = 0.8, main = "Pie Chart of Freetime activities of students ")

#3.Scatter plots


plot(x=studentData_sub_sleep_study$SGPA,y=studentData_sub_sleep_study$Sleep,xlab="SGPA",ylab="Sleep hours",main = "Sleephours vs SGPA",col="red")


plot(x=studentData_sub_sleep_study$SGPA,y=studentData_sub_sleep_study$Study.Hours,xlab="SGPA",ylab="Study hours",main = "Studyhours vs SGPA",col="red")

sleep<-table(studentData_sub_sleep_study$Sleep)
sleep
plot(x=studentData_sub_sleep_study$SGPA,y=studentData_sub_sleep_study$Freetime,xlab="SGPA",ylab="Freetime activities",main = "Freetime activities vs SGPA",col="red")

#Simple linear regression


#SCatterplot
plot(studentData$Sleep,studentData$SGPA,main="Scatterplot")

#Linear model

##SGPA~Sleep


model1<-lm(studentData$SGPA~studentData$Sleep)
abline(model1)

plot(model1)

summary(model1)

##SGPA~Study

model2<-lm(studentData$SGPA~studentData$SrNo)
abline(model2)

plot(model2)

summary(model2)

