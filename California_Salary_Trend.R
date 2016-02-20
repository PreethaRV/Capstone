Salary2011<- read.csv("state-of-california-2011.csv",header = TRUE,strip.white = TRUE)
Salary2014 <- read.csv("state-of-california-2014.csv",header = TRUE,strip.white = TRUE)
Salary2012 <- read.csv("state-of-california-2012.csv",header = TRUE,strip.white = TRUE)
Salary2013 <- read.csv("state-of-california-2013.csv",header = TRUE,strip.white = TRUE)

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

#Join all jobtitles of 2011 and 2014
join20112014 <- full_join(Jobtitle2011byCount,Jobtitle2014byCount,"c")
names(join20112014)[2] <- paste("2011Positions")
names(join20112014)[3] <- paste("2014Positions")

join20112014$`2014Positions`[is.na(join20112014$`2014Positions`)] <- 0
join20112014$`2011Positions`[is.na(join20112014$`2011Positions`)] <- 0

#Jobs reduced from 2011
jobsreducedfrom2011 <- join20112014 %>% filter(`2011Positions` - `2014Positions`>10)

MajorJobCutFrom2011 <- join20112014 %>% filter(`2011Positions` - `2014Positions`>1000)

MajorJobCutFrom2011$SJT <- strtrim(MajorJobCutFrom2011$Job_Title, 4)
MajorJobCutFrom2011$PositionsLost <- MajorJobCutFrom2011$`2011Positions` - MajorJobCutFrom2011$`2014Positions`

#Display count of jobs titles reduced from 2011.
ggplot(data=MajorJobCutFrom2011, aes(x=SJT, y=PositionsLost)) +
  geom_bar(stat="identity")

