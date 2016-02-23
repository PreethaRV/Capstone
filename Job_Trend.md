---
title: "Job_Trend"
author: "Preetha"
date: "February 22, 2016"
output: word_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

#Load Files and trim white space.
```{r}
Salary2011<- read.csv("state-of-california-2011.csv",header = TRUE,strip.white = TRUE)
Salary2014 <- read.csv("state-of-california-2014.csv",header = TRUE,strip.white = TRUE)
Salary2012 <- read.csv("state-of-california-2012.csv",header = TRUE,strip.white = TRUE)
Salary2013 <- read.csv("state-of-california-2013.csv",header = TRUE,strip.white = TRUE)

Salary2011$job_title<-trimws(Salary2011$job_title)
Salary2012$job_title<-trimws(Salary2012$job_title)
Salary2013$Job.Title<-trimws(Salary2013$Job.Title)
Salary2014$Job.Title<-trimws(Salary2014$Job.Title)

```


#Load required libraries
```{r}
library(dplyr)
library(ggplot2)
library(tidyr)
```

# create a dataframe to store  2011 Jobtitle and number of position.
```{r}

Jobtitle2011byCount <- data.frame(table(Salary2011$job_title))

names(Jobtitle2011byCount)[1] <- paste("Job_Title")
names(Jobtitle2011byCount)[2] <- paste("Number_Of_Positions")

```

# create a dataframe to store  2012 Jobtitle and number of position.
```{r}

Jobtitle2012byCount <- data.frame(table(Salary2012$job_title))

names(Jobtitle2012byCount)[1] <- paste("Job_Title")
names(Jobtitle2012byCount)[2] <- paste("Number_Of_Positions")
```

# create a dataframe to store  2013 Jobtitle and number of position.
```{r}

Jobtitle2013byCount <- data.frame(table(Salary2013$Job.Title))

names(Jobtitle2013byCount)[1] <- paste("Job_Title")
names(Jobtitle2013byCount)[2] <- paste("Number_Of_Positions")
```

# create a dataframe to store  2014 Jobtitle and number of position.
```{r}

Jobtitle2014byCount <- data.frame(table(Salary2014$Job.Title))


names(Jobtitle2014byCount)[1] <- paste("Job_Title")
names(Jobtitle2014byCount)[2] <- paste("Number_Of_Positions")
```

#Join jobtitles of 2011 and 2014. Update positions with 0 if the job do not exist for the year(2011 or 2014)
```{r}

join20112014 <- full_join(Jobtitle2011byCount,Jobtitle2014byCount,"Job_Title")
names(join20112014)[2] <- paste("2011Positions")
names(join20112014)[3] <- paste("2014Positions")

join20112014$`2014Positions`[is.na(join20112014$`2014Positions`)] <- 0
join20112014$`2011Positions`[is.na(join20112014$`2011Positions`)] <- 0
```

#Compare jobs of 2014 with 2011 and identify the jobs cut since 2011.
# List of jobs reduced from 2011 with positions difference of 10 and 500.
```{r}

jobsreducedfrom2011 <- join20112014 %>% filter(`2011Positions` - `2014Positions`>10)

MajorJobCutFrom2011 <- join20112014 %>% filter(`2011Positions` - `2014Positions`>500)

MajorJobCutFrom2011$SJT <- strtrim(MajorJobCutFrom2011$Job_Title, 4)


```

#Find number of positions lost per job title.
```{r}
MajorJobCutFrom2011$PositionsLost <- MajorJobCutFrom2011$`2011Positions` - MajorJobCutFrom2011$`2014Positions`

```
#Plot number of positions reduced per jobs titles from 2011.

```{r}

ggplot(data=MajorJobCutFrom2011, aes(x=SJT, y=PositionsLost)) +
  geom_bar(stat="identity")+
ylab('Positions Lost from 2011')+
xlab('Job_Title in short form')
```

#Jobs titles that no longer exists from 2011
```{r}
JobTitleNoLongerExists <- join20112014 %>% filter(`2014Positions` == 0)
```

#Jobs titles that no longer exists from 2011 which once had positions more than 100 Positions
```{r}

MajorJobTitlescut2011 <- join20112014 %>% filter(`2014Positions` == 0 & `2011Positions` >100)
MajorJobTitlescut2011$SJT <- strtrim(MajorJobTitlescut2011$Job_Title, 7)

```


#Display jobs titles removed from 2011.

```{r}

ggplot(data=MajorJobTitlescut2011, aes(x=SJT, y=`2011Positions`)) +
  geom_bar(stat="identity")+
  ylab('2011 Position No Longer Exists')+
  xlab('Job_Title in short form')
  
```

#Identify Job Titles and Position increased/newly added in 2014

#Positions per job titles increased from 2011
```{r}
jobsIncreasedfrom2011 <- join20112014 %>% filter(`2014Positions` - `2011Positions`>10)

MajorjobsIncreasedfrom2011 <- join20112014 %>% filter(`2014Positions` - `2011Positions`>500)

MajorjobsIncreasedfrom2011$SJT <- strtrim(MajorjobsIncreasedfrom2011$Job_Title, 4)


MajorjobsIncreasedfrom2011$PositionsIncreased <- MajorjobsIncreasedfrom2011$`2014Positions` - MajorjobsIncreasedfrom2011$`2011Positions`

```

#Display jobs titles increased after 2011.
```{r}

ggplot(data=MajorjobsIncreasedfrom2011, aes(x=SJT, y=PositionsIncreased)) +
  geom_bar(stat="identity")+
ylab('Positions increased between 2011-2014')+
  xlab('Job_Title in short form')
```

#Jobs titles newly added after 2011 which had atleast positions more than 100
```{r}

MajorJobTitlesIncreaedFrom2011 <- join20112014 %>% filter(`2011Positions` == 0 & `2014Positions` >100)
MajorJobTitlesIncreaedFrom2011$SJT <- strtrim(MajorJobTitlesIncreaedFrom2011$Job_Title, 8)


```

#Display new jobs titles added after 2011.
```{r}

ggplot(data=MajorJobTitlesIncreaedFrom2011, aes(x=SJT, y=`2014Positions`)) +
  geom_bar(stat="identity")+
ylab('2014 job tiltes newly added since 2011-2014')+
  xlab('Job_Title in short form')


```

#Jobs cuts and their mean base pay

#Filter job titles that had atleast a cut of minimum 200 positions. 
```{r}


MajorJobCutFrom2011 <- join20112014 %>% filter(`2011Positions` - `2014Positions`>200)
MajorJobCutFrom2011$SJT <- strtrim(MajorJobCutFrom2011$Job_Title, 4)
MajorJobCutFrom2011$PositionsLost <- MajorJobCutFrom2011$`2011Positions` - MajorJobCutFrom2011$`2014Positions`


names(Salary2011)[2]<- paste('Job_Title')
names(Salary2014)[2]<- paste('Job_Title')
```

#Join tables to combine pay related informations.
#Ensure Year variable has value 2011
#Addd Status column as 2014 data has a new column 2014
```{r}
JobCutDensity2011 <-  left_join(MajorJobCutFrom2011,Salary2011,"Job_Title")
JobCutDensity2011$Status <- 'NA'
JobCutDensity2011$year <- 2011
```

#Repeat the above steps to 2014.Join tables to combine pay related informations for 2014.
#Ensure Year variable has value 2014
```{r}
JobCutDensity2014 <-  left_join(MajorJobCutFrom2011,Salary2014,"Job_Title")

JobCutDensity2014$Year <- 2014

glimpse(JobCutDensity2014)
glimpse(JobCutDensity2011)
```

#Ensure the columns match between JobCutDensity2014 and JobCutDensity2011
```{r}

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
          
```

#rbind the tables
```{r}
JobCutDensity20112014 <- rbind(JobCutDensity2011,JobCutDensity2014)

by(JobCutDensity20112014$Job_Title,JobCutDensity20112014$Year,summary)

```

#Plot the density of positions reduced grouped by year
```{r}
ggplot(JobCutDensity20112014, aes(x=SJT)) + 
  geom_density(aes(group=Year, colour=Year))+
  ggtitle("Density of jobs lost")

```

#Histogram of positions/job title per year 2011 and 2014
```{r}

ggplot(JobCutDensity20112014,aes(x=SJT,fill = Year))+
  stat_count(width = 0.5) +
  facet_wrap(~Year)+
  ggtitle("Positions Vs Year")+
  xlab("Job Titles")+
  ylab("Number of Positions")+
  labs(fill="Year")

```

#Jobs increased in 2014 and their mean base pay

#Filter job titles that had atleast a increase of minimum 200 positions.
```{r}

MajorjobsIncreasedfrom2011 <- join20112014 %>% filter(`2014Positions` - `2011Positions`>200)
MajorjobsIncreasedfrom2011$SJT <- strtrim(MajorjobsIncreasedfrom2011$Job_Title, 4)
MajorjobsIncreasedfrom2011$PositionsIncreased <- MajorjobsIncreasedfrom2011$`2014Positions` - MajorjobsIncreasedfrom2011$`2011Positions`


names(Salary2011)[2]<- paste('Job_Title')
names(Salary2014)[2]<- paste('Job_Title')

```

#Join tables to combine pay related informations.
#Ensure Year variable has value 2011
#Addd Status column as 2014 data has a new column 2014

```{r}
JobIncreasedDensity2011 <-  left_join(MajorjobsIncreasedfrom2011,Salary2011,"Job_Title")
JobIncreasedDensity2011$Status <- 'NA'
#Ensure Year variable has value 2011
JobIncreasedDensity2011$year <- 2011

JobIncreasedDensity2014 <-  left_join(MajorjobsIncreasedfrom2011,Salary2014,"Job_Title")
#Ensure Year variable has value 2014
JobIncreasedDensity2014$Year <- 2014

glimpse(JobIncreasedDensity2011)
glimpse(JobIncreasedDensity2014)

```

#Ensure the columns match between JobIncreasedDensity2014 and JobIncreasedDensity2011

```{r}
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

```


#rbind the tables

```{r}
JobIncreasedDensity20112014 <- rbind(JobIncreasedDensity2011,JobIncreasedDensity2014)

by(JobIncreasedDensity20112014$Job_Title,JobIncreasedDensity20112014$Year,summary)
```


#Plot the density of positions increased and grouped by year

```{r}
ggplot(JobIncreasedDensity20112014, aes(x=SJT)) + 
  geom_density(aes(group=Year, colour=Year))

```


#Histogram of positions increased per job title for year 2011 and 2014
```{r,echo=FALSE}
ggplot(JobIncreasedDensity20112014,aes(x=SJT,fill = Year))+
  stat_count(width = 0.5) +
  facet_wrap(~Year)+
  ggtitle("Positions Vs Year")+
  xlab("Job Titles")+
  ylab("Number of Positions")+
  labs(fill="Year")
```

