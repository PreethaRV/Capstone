Salary2011<- read.csv("state-of-california-2011.csv",header = TRUE,strip.white = TRUE)
Salary2014 <- read.csv("state-of-california-2014.csv",header = TRUE,strip.white = TRUE)
Salary2012 <- read.csv("state-of-california-2012.csv",header = TRUE,strip.white = TRUE)
Salary2013 <- read.csv("state-of-california-2013.csv",header = TRUE,strip.white = TRUE)

Salary2011$job_title<-trimws(Salary2011$job_title)
Salary2012$job_title<-trimws(Salary2012$job_title)
Salary2013$Job.Title<-trimws(Salary2013$Job.Title)
Salary2014$Job.Title<-trimws(Salary2014$Job.Title)



#Test if all strings are trimmed - The below string had a long white space even after strip.white = true
#Salary2014 %>% filter(Job.Title =='OFFICER, CALIFORNIA HIGHWAY PATROL')
#Salary2011 %>% filter(job_title == 'OFFICER, CALIFORNIA HIGHWAY PATROL')


length(Salary2011$job_title)

length(Salary2014$Job.Title)

length(Salary2011$job_title)-length(Salary2014$Job.Title)


length(unique(Salary2011$job_title))
length(unique(Salary2012$job_title))
length(unique(Salary2013$Job.Title))
length(unique(Salary2014$Job.Title))

str(Salary2014$Job.Title)

library(dplyr)
library(ggplot2)
library(tidyr)


#JobTitle2011 <- c(unique(as.character(Salary2011$job_title)))
#JobTitle2012 <- c(unique(as.character(Salary2012$job_title)))
#JobTitle2013 <- c(unique(as.character(Salary2013$Job.Title)))
#JobTitle2014 <- c(unique(as.character(Salary2014$Job.Title)))

#summary(Salary2011$job_title)

# create a dataframe to store  2011 Jobtitle and number of position.
Jobtitle2011byCount <- data.frame(table(Salary2011$job_title))


names(Jobtitle2011byCount)[1] <- paste("Job_Title")
names(Jobtitle2011byCount)[2] <- paste("Number_Of_Positions")

# create a dataframe to store  2012 Jobtitle and number of position.
Jobtitle2012byCount <- data.frame(table(Salary2012$job_title))


names(Jobtitle2012byCount)[1] <- paste("Job_Title")
names(Jobtitle2012byCount)[2] <- paste("Number_Of_Positions")

# create a dataframe to store  2013 Jobtitle and number of position.
Jobtitle2013byCount <- data.frame(table(Salary2013$Job.Title))


names(Jobtitle2013byCount)[1] <- paste("Job_Title")
names(Jobtitle2013byCount)[2] <- paste("Number_Of_Positions")

# create a dataframe to store  2014 Jobtitle and number of position.
Jobtitle2014byCount <- data.frame(table(Salary2014$Job.Title))


names(Jobtitle2014byCount)[1] <- paste("Job_Title")
names(Jobtitle2014byCount)[2] <- paste("Number_Of_Positions")

#Jobtitle2012byCount %>% filter(Job_Title %in% ('ACCOUNT CLERK II'))


#leftjoin2011 <- left_join(Jobtitle2011byCount,Jobtitle2014byCount,"Job_Title")
#names(leftjoin2011)[2] <- paste("2011Job_Title")
#names(leftjoin2011)[3] <- paste("2014Job_Title")

#Identify Job Titles and Position reduced/removed from 2011

#Join all jobtitles of 2011 and 2014
join20112014 <- full_join(Jobtitle2011byCount,Jobtitle2014byCount,"Job_Title")
names(join20112014)[2] <- paste("2011Positions")
names(join20112014)[3] <- paste("2014Positions")

join20112014$`2014Positions`[is.na(join20112014$`2014Positions`)] <- 0
join20112014$`2011Positions`[is.na(join20112014$`2011Positions`)] <- 0

#Jobs reduced from 2011
jobsreducedfrom2011 <- join20112014 %>% filter(`2011Positions` - `2014Positions`>10)

MajorJobCutFrom2011 <- join20112014 %>% filter(`2011Positions` - `2014Positions`>500)

MajorJobCutFrom2011$SJT <- strtrim(MajorJobCutFrom2011$Job_Title, 4)
MajorJobCutFrom2011$PositionsLost <- MajorJobCutFrom2011$`2011Positions` - MajorJobCutFrom2011$`2014Positions`

#Display count of jobs titles reduced from 2011.
ggplot(data=MajorJobCutFrom2011, aes(x=SJT, y=PositionsLost)) +
  geom_bar(stat="identity")+
ylab('Positions Lost from 2011')+
xlab('Job_Title in short form')

#Jobs titles that no longer exists from 2011

JobTitleNoLongerExists <- join20112014 %>% filter(`2014Positions` == 0)


#Jobs titles that no longer exists from 2011 which once had positions more than 100 Positions

MajorJobTitlescut2011 <- join20112014 %>% filter(`2014Positions` == 0 & `2011Positions` >100)
MajorJobTitlescut2011$SJT <- strtrim(MajorJobTitlescut2011$Job_Title, 7)

#Display jobs titles removed from 2011.
ggplot(data=MajorJobTitlescut2011, aes(x=SJT, y=`2011Positions`)) +
  geom_bar(stat="identity")+
  ylab('2011 Position No Longer Exists')+
  xlab('Job_Title in short form')


#Identify Job Titles and Position increased/newly added in 2014

#Jobs increased from 2011
jobsIncreasedfrom2011 <- join20112014 %>% filter(`2014Positions` - `2011Positions`>10)

MajorjobsIncreasedfrom2011 <- join20112014 %>% filter(`2014Positions` - `2011Positions`>500)


MajorjobsIncreasedfrom2011$SJT <- strtrim(MajorjobsIncreasedfrom2011$Job_Title, 4)
MajorjobsIncreasedfrom2011$PositionsIncreased <- MajorjobsIncreasedfrom2011$`2014Positions` - MajorjobsIncreasedfrom2011$`2011Positions`

#Display jobs titles increased from 2011.
ggplot(data=MajorjobsIncreasedfrom2011, aes(x=SJT, y=PositionsIncreased)) +
  geom_bar(stat="identity")+
ylab('Positions increased between 2011-2014')+
  xlab('Job_Title in short form')

#Jobs titles newly added after 2011

MajorJobTitlesIncreaedFrom2011 <- join20112014 %>% filter(`2011Positions` == 0 & `2014Positions` >100)
MajorJobTitlesIncreaedFrom2011$SJT <- strtrim(MajorJobTitlesIncreaedFrom2011$Job_Title, 8)

#Display new jobs titles added after 2011.
ggplot(data=MajorJobTitlesIncreaedFrom2011, aes(x=SJT, y=`2014Positions`)) +
  geom_bar(stat="identity")+
ylab('2014 job tiltes newly added since 2011-2014')+
  xlab('Job_Title in short form')


#Job Cut Density

    

MajorJobCutFrom2011 <- join20112014 %>% filter(`2011Positions` - `2014Positions`>200)
MajorJobCutFrom2011$SJT <- strtrim(MajorJobCutFrom2011$Job_Title, 4)
MajorJobCutFrom2011$PositionsLost <- MajorJobCutFrom2011$`2011Positions` - MajorJobCutFrom2011$`2014Positions`


names(Salary2011)[2]<- paste('Job_Title')
names(Salary2014)[2]<- paste('Job_Title')

JobCutDensity2011 <-  left_join(MajorJobCutFrom2011,Salary2011,"Job_Title")
JobCutDensity2011$Status <- 'NA'
#Ensure Year variable has value 2011
JobCutDensity2011$year <- 2011

JobCutDensity2014 <-  left_join(MajorJobCutFrom2011,Salary2014,"Job_Title")
#Ensure Year variable has value 2014
JobCutDensity2014$Year <- 2014

glimpse(JobCutDensity2014)
glimpse(JobCutDensity2011)

names(JobCutDensity2014)[6]<- paste('Employee_Name')
names(JobCutDensity2014)[7]<- paste('Base_Pay')
names(JobCutDensity2014)[8]<- paste('Overtime_Pay')
names(JobCutDensity2014)[9]<- paste('Other_Pay')
names(JobCutDensity2014)[10]<- paste('Benefits')
names(JobCutDensity2014)[11]<- paste('Total_Pay')
names(JobCutDensity2014)[12]<- paste('Total_Pay_Benefits')
names(JobCutDensity2014)[13]<- paste('Year')
names(JobCutDensity2014)[14]<- paste('Notes')
names(JobCutDensity2014)[15]<- paste('Agency')

names(JobCutDensity2011)[6]<- paste('Employee_Name')
names(JobCutDensity2011)[7]<- paste('Base_Pay')
names(JobCutDensity2011)[8]<- paste('Overtime_Pay')
names(JobCutDensity2011)[9]<- paste('Other_Pay')
names(JobCutDensity2011)[10]<- paste('Benefits')
names(JobCutDensity2011)[11]<- paste('Total_Pay')
names(JobCutDensity2011)[12]<- paste('Total_Pay_Benefits')
names(JobCutDensity2011)[13]<- paste('Year')
names(JobCutDensity2011)[14]<- paste('Notes')
names(JobCutDensity2011)[15]<- paste('Agency')
          

JobCutDensity20112014 <- rbind(JobCutDensity2011,JobCutDensity2014)

by(JobCutDensity20112014$Job_Title,JobCutDensity20112014$Year,summary)

ggplot(JobCutDensity20112014, aes(x=SJT)) + 
  geom_density(aes(group=Year, colour=Year))+
  ggtitle("Density of jobs lost")


ggplot(JobCutDensity20112014,aes(x=SJT,fill = Year))+
  stat_count(width = 0.5) +
  facet_wrap(~Year)+
  ggtitle("Positions Vs Year")+
  xlab("Job Titles")+
  ylab("Number of Positions")+
  labs(fill="Year")

#Density of Jobs Increased in 2014


MajorjobsIncreasedfrom2011 <- join20112014 %>% filter(`2014Positions` - `2011Positions`>200)
MajorjobsIncreasedfrom2011$SJT <- strtrim(MajorjobsIncreasedfrom2011$Job_Title, 4)
MajorjobsIncreasedfrom2011$PositionsIncreased <- MajorjobsIncreasedfrom2011$`2014Positions` - MajorjobsIncreasedfrom2011$`2011Positions`


names(Salary2011)[2]<- paste('Job_Title')
names(Salary2014)[2]<- paste('Job_Title')

JobIncreasedDensity2011 <-  left_join(MajorjobsIncreasedfrom2011,Salary2011,"Job_Title")
JobIncreasedDensity2011$Status <- 'NA'
#Ensure Year variable has value 2011
JobIncreasedDensity2011$year <- 2011

JobIncreasedDensity2014 <-  left_join(MajorjobsIncreasedfrom2011,Salary2014,"Job_Title")
#Ensure Year variable has value 2014
JobIncreasedDensity2014$Year <- 2014

glimpse(JobIncreasedDensity2011)
glimpse(JobIncreasedDensity2014)

names(JobIncreasedDensity2014)[6]<- paste('Employee_Name')
names(JobIncreasedDensity2014)[7]<- paste('Base_Pay')
names(JobIncreasedDensity2014)[8]<- paste('Overtime_Pay')
names(JobIncreasedDensity2014)[9]<- paste('Other_Pay')
names(JobIncreasedDensity2014)[10]<- paste('Benefits')
names(JobIncreasedDensity2014)[11]<- paste('Total_Pay')
names(JobIncreasedDensity2014)[12]<- paste('Total_Pay_Benefits')
names(JobIncreasedDensity2014)[13]<- paste('Year')
names(JobIncreasedDensity2014)[14]<- paste('Notes')
names(JobIncreasedDensity2014)[15]<- paste('Agency')

names(JobIncreasedDensity2011)[6]<- paste('Employee_Name')
names(JobIncreasedDensity2011)[7]<- paste('Base_Pay')
names(JobIncreasedDensity2011)[8]<- paste('Overtime_Pay')
names(JobIncreasedDensity2011)[9]<- paste('Other_Pay')
names(JobIncreasedDensity2011)[10]<- paste('Benefits')
names(JobIncreasedDensity2011)[11]<- paste('Total_Pay')
names(JobIncreasedDensity2011)[12]<- paste('Total_Pay_Benefits')
names(JobIncreasedDensity2011)[13]<- paste('Year')
names(JobIncreasedDensity2011)[14]<- paste('Notes')
names(JobIncreasedDensity2011)[15]<- paste('Agency')


JobIncreasedDensity20112014 <- rbind(JobIncreasedDensity2011,JobIncreasedDensity2014)

by(JobIncreasedDensity20112014$Job_Title,JobIncreasedDensity20112014$Year,summary)

ggplot(JobIncreasedDensity20112014, aes(x=SJT)) + 
  geom_density(aes(group=Year, colour=Year))


ggplot(JobIncreasedDensity20112014,aes(x=SJT,fill = Year))+
  stat_count(width = 0.5) +
  facet_wrap(~Year)+
  ggtitle("Positions Vs Year")+
  xlab("Job Titles")+
  ylab("Number of Positions")+
  labs(fill="Year")

#Job Density by job increased and reduced.

names(JobIncreasedDensity20112014)[5] <- paste('Difference_Position')
names(JobCutDensity20112014)[5] <- paste('Difference_Position')

glimpse(JobIncreasedDensity20112014)
glimpse(JobCutDensity20112014)

JobDensity <- rbind(JobIncreasedDensity20112014,JobCutDensity20112014)
JobDensity$Difference_Position <- (JobDensity$`2014Positions`-JobDensity$`2011Positions`)

#Density of number of positions per title VS Year
ggplot(JobDensity, aes(x=SJT)) + 
  geom_density(aes(group=Year, colour=Year))

#Positions per Job Title grouped by year
job20112014Plot <- ggplot(JobDensity,aes(x=Job_Title,fill = Year))+
  stat_count(width = 0.5) +
  facet_wrap(~Year)+
  ggtitle("Positions Vs Year")+
  xlab("Job Titles")+
  ylab("Number of Positions")+
  labs(fill="Year")

job20112014Plot+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

JobDensity$Base_Pay[is.na(JobDensity$Base_Pay)] <- 0

JobMeanPay <- JobDensity %>% group_by(factor(Job_Title),Difference_Position) %>%
  summarise(MeanPay=mean(Base_Pay))

names(JobMeanPay)[1] <- paste('Job_Title')



glimpse(JobMeanPay)

#Jobs lost and added in 2014(cpmpared to jobs in 2011)
#scale_x_log10()

ggplot(data=JobMeanPay, aes(x=MeanPay, y=Difference_Position)) +
  geom_bar(stat="identity",width = .1)+
  scale_y_continuous(limits = c(-3000, 4000), breaks = seq(-3000, 4000, 10)) +
  scale_x_continuous() +
  scale_x_log10()+
  ylim(c(-3000,4000))+
  ylab('<--Positions Lost from 2011 . Positions Added in 2014 -->')+
  xlab('Mean Pay')

#scale_x_sqrt()
ggplot(data=JobMeanPay, aes(x=MeanPay, y=Difference_Position)) +
  geom_bar(stat="identity",width = 50)+
  scale_y_continuous(limits = c(-3000, 4000), breaks = seq(-3000, 4000, 10)) +
  scale_x_sqrt()+
      ylim(c(-3000,4000))+
     ylab('<--Positions Lost from 2011 . Positions Added in 2014 -->')+
  xlab('Mean Pay')


#mean salaries of jobs for 2011 and 2014

MeanPay2011 <- Salary2011 %>% group_by(factor(job_title)) %>%
  summarise(mean=mean(total_pay), sd=sd(total_pay))
#MeanPay2011 <- Salary2011 %>% group_by(factor(job_title)) %>%
#summarise(mean=mean(total_pay))
#MeanPay2011$Year <- '2011'

names(MeanPay2011)[1] <- paste("Job_Title")
names(MeanPay2011)[2] <- paste("2011Mean")
names(MeanPay2011)[3] <- paste("2011SD")



MeanPay2014 <- Salary2014 %>% group_by(factor(Job.Title)) %>%
  summarise(mean=mean(Total.Pay), sd=sd(Total.Pay))

#MeanPay2014 <- Salary2014 %>% group_by(factor(Job.Title)) %>%
# summarise(mean=mean(Total.Pay))

names(MeanPay2014)[1] <- paste("Job_Title")
names(MeanPay2014)[2] <- paste("2014Mean")
names(MeanPay2014)[3] <- paste("2014SD")


MeanPay2011 <- left_join(Jobtitle2011byCount,MeanPay2011,"Job_Title")
MeanPay2014 <- left_join(Jobtitle2014byCount,MeanPay2014,"Job_Title")

names(MeanPay2011)[1] <- paste("Job_Title")
names(MeanPay2011)[2] <- paste("2011Positions")
names(MeanPay2011)[3] <- paste("2011Mean")
names(MeanPay2011)[4] <- paste("2011SD")
names(MeanPay2014)[1] <- paste("Job_Title")
names(MeanPay2014)[2] <- paste("2014Positions")
names(MeanPay2014)[3] <- paste("2014Mean")
names(MeanPay2014)[4] <- paste("2014SD")

JobsandMeanPay20112014 <- full_join(MeanPay2011,MeanPay2014,"Job_Title")

#update NA to 0 for all variables.

JobsandMeanPay20112014$`2011SD`[is.na(JobsandMeanPay20112014$`2011SD`)] <- 0
JobsandMeanPay20112014$`2014Positions`[is.na(JobsandMeanPay20112014$`2014Positions`)] <- 0
JobsandMeanPay20112014$`2014Mean`[is.na(JobsandMeanPay20112014$`2014Mean`)] <- 0
JobsandMeanPay20112014$`2014SD`[is.na(JobsandMeanPay20112014$`2014SD`)] <- 0

#More Jobs with less mean pay 
ggplot(aes(x = MeanPay2011$`2011Mean`), data = MeanPay2011) +
  geom_freqpoly( binwidth=20) + 
  scale_x_continuous(limits = c(40000, 522594), breaks = seq(40000, 522594, 20000)) + 
  xlab('Mean Pay') + 
  ylab('Number of JobTitles')

#Higer the basepay, lesser the positions
ggplot(data=MeanPay2011, aes(x=MeanPay2011$`2011Mean`, y=MeanPay2011$`2011Positions`)) +
  geom_bar(stat="identity")+
  scale_x_continuous(limits = c(400000, 522594), breaks = seq(400000, 522594, 10000)) +
  ylim(c(0,25))+
  ylab('Number of positions')+
  xlab('Mean Pay')