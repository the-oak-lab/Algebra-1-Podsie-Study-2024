# EDA-draft
title: "Algebra I Podsie Study"
author:
- Isha Patel
- ishap
date: "August 2 2024"
output:
  pdf_document:
    toc: yes
  word_document:
    toc: yes
  html_document:
    code_folding: show
    theme: cosmo
    toc: yes
    toc_float: yes
optin: TRUE
---

```{r, include=FALSE}
###########################
# STYLE EDITS: IGNORE THIS
###########################
knitr::opts_chunk$set(message = FALSE) # include this if you don't want markdown to knit messages
knitr::opts_chunk$set(warning = FALSE) # include this if you don't want markdown to knit warnings
knitr::opts_chunk$set(echo = TRUE) # set echo=FALSE to hide code from html output
```


```{r}
algebraData <- read.table("algebra.txt", header = TRUE, sep = "\t", quote = "\"", fill = TRUE, comment.char = "")
PostTest <- read.table("post-test.txt", header = TRUE, sep = "\t", quote = "\"", fill = TRUE, comment.char = "")
library("knitr")
PreTest <- read.table("pre-test.txt", header = TRUE, sep = "\t", quote = "\"", fill = TRUE, comment.char = "")
library("kableExtra")
library("pander")
library("readr")
library("magrittr")
library("car")
library("interactions")
library("leaps")
library("dplyr")

```

# Introduction

In this study we investigate the impact that spaced retrieval practice of Algebra I concepts has on learning compared to no practice. This is to understand if spaced retrieval practice of Algebra I in a computer-based system can improve learning.
10 learning objectives covered in class will be identity and 2 questions will be created. In the pre-test students will complete 20 questions and each will have 3 variations to ensure students do not memorize the answer.
For each student, 40% of the learning objectives will be randomly selected, and withheld from the Personal Deck. The questions for the remaining 60% of the learning objectives will be inserted into the Personal Deck for practice over the 4 weeks.
Every Tuesday and Thurday each student will be given 15 minutes to practice their personal deck which will follow SuperMemo2 algorithm to determine if a student should practice each specific question. At the end of 4 weeks, students will be given a post-test with 20 questions covering the 10 learning objectives.

# Exploratory Data Analysis

DATA: This data was conducted by a Algebra I teacher Ashley, who will be using Podsie with her 80 students over the past 5 weeks of the school year. The study will run from will from 2/26/2024 to 3/21/2024. There are 28 variables in this study which are called Row, Sample, StudentID, ProblemHierachy, ProblemName, ProblemView, StepName, StepStartTime, FirstTransactionTime, CorrectTransactionTime, StepEndTime, StepDuration, CorrectStepDuration, ErrorStepDuration, FirstAttempt, Incorrect, Hint, Correct, Condition, KCLO, OppurtunityLO, PredictedErrorRateLO, KCSingleKC, OppurtunitySingleKC, PredictedErrorRateSingleKC, KCUniqueStep, OppurtunityUniqueStep and PredictedErrorRateUniqueStep.


This is the structure of the data that will be used.
```{r}
str(algebraData)
```
'data.frame':	6028 obs. of  28 variables:
 $ Row                         : int  1 2 3 4 5 6 7 8 9 10 ...
 $ Sample                      : chr  "All Data" "All Data" "All Data" "All Data" ...
 $ StudentID                   : int  25940 25940 25940 25940 25940 25940 25940 25940 25940 25940 ...
 $ ProblemHierarchy            : chr  "LO LO 1" "LO LO 1" "LO LO 2" "LO LO 3" ...
 $ ProblemName                 : chr  "Simplify the expression.  \\left(9n-16\\right)+\\left(-1+3n\\right)" "Simplify: \\left(-3x+4\\right)-\\left(-7x-6\\right)" "Which of the following expressions is equivalent to 4x+4y ?" "Which of the following relations represent a function? Select all that apply." ...
 $ ProblemView                 : int  1 1 1 1 1 1 1 1 1 1 ...
 $ StepName                    : int  97183 98210 98211 98217 98219 98220 98221 98222 98238 98239 ...
 $ StepStartTime               : chr  "2024-03-05 15:37:10" "2024-03-05 15:38:12" "2024-03-05 15:39:10" "2024-03-05 15:41:31" ...
 $ FirstTransactionTime        : chr  "2024-03-05 15:38:12" "2024-03-05 15:39:10" "2024-03-05 15:39:33" "2024-03-05 15:42:53" ...
 $ CorrectTransactionTime      : chr  "2024-03-05 15:38:12" "" "2024-03-05 15:39:33" "2024-03-05 15:42:53" ...
 $ StepEndTime                 : chr  "2024-03-05 15:38:12" "2024-03-05 15:39:10" "2024-03-05 15:39:33" "2024-03-05 15:42:53" ...
 $ StepDuration                : chr  "62" "58" "23" "82" ...
 $ CorrectStepDuration         : chr  "62" "." "23" "82" ...
 $ ErrorStepDuration           : chr  "." "58" "." "." ...
 $ FirstAttempt                : chr  "correct" "incorrect" "correct" "correct" ...
 $ Incorrect                   : int  0 1 0 0 0 0 0 1 0 0 ...
 $ Hint                        : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Correct                     : int  1 0 1 1 1 1 1 0 2 1 ...
 $ Condition                   : chr  "without (podsie_personal_deck)" "without (podsie_personal_deck)" "with (podsie_personal_deck)" "with (podsie_personal_deck)" ...
 $ KCLO                        : chr  "LO 1" "LO 1" "LO 2" "LO 3" ...
 $ OpportunityLO               : int  1 2 1 1 1 2 1 2 1 2 ...
 $ PredictedErrorRateLO        : num  0.193 0.193 0.161 0.25 0.21 ...
 $ KCSingleKC                  : chr  "Single-KC" "Single-KC" "Single-KC" "Single-KC" ...
 $ OpportunitySingleKC         : int  1 2 3 4 5 6 7 8 9 10 ...
 $ PredictedErrorRateSingleKC  : num  0.347 0.345 0.343 0.34 0.338 ...
 $ KCUniqueStep                : chr  "KC9" "KC36" "KC65" "KC67" ...
 $ OpportunityUniqueStep       : int  1 1 1 1 1 1 1 1 1 1 ...
 $ PredictedErrorRateUniqueStep: num  0.108 0.372 0 0.437 0.425 ...

 Now we look at a summary of the data.
```{r}
summary(algebraData)
```
   Row          Sample            StudentID     ProblemHierarchy  
 Min.   :   1   Length:6028        Min.   :25940   Length:6028       
 1st Qu.:1508   Class :character   1st Qu.:25960   Class :character  
 Median :3014   Mode  :character   Median :26046   Mode  :character  
 Mean   :3014                      Mean   :26038                     
 3rd Qu.:4521                      3rd Qu.:26083                     
 Max.   :6028                      Max.   :26209                     
                                                                     
 ProblemName         ProblemView       StepName      StepStartTime     
 Length:6028        Min.   :1.000   Min.   : 97183   Length:6028       
 Class :character   1st Qu.:1.000   1st Qu.: 98245   Class :character  
 Mode  :character   Median :1.000   Median : 99020   Mode  :character  
                    Mean   :1.194   Mean   :101261                     
                    3rd Qu.:1.000   3rd Qu.:106887                     
                    Max.   :7.000   Max.   :106941                     
                                                                       
 FirstTransactionTime CorrectTransactionTime StepEndTime       
 Length:6028          Length:6028            Length:6028       
 Class :character     Class :character       Class :character  
 Mode  :character     Mode  :character       Mode  :character  

StepDuration       CorrectStepDuration ErrorStepDuration 
 Length:6028        Length:6028         Length:6028       
 Class :character   Class :character    Class :character  
 Mode  :character   Mode  :character    Mode  :character  
                                                                                         
                                                          
 FirstAttempt         Incorrect           Hint      Correct      
 Length:6028        Min.   :0.0000   Min.   :0   Min.   :0.0000  
 Class :character   1st Qu.:0.0000   1st Qu.:0   1st Qu.:1.0000  
 Mode  :character   Median :0.0000   Median :0   Median :1.0000  
                    Mean   :0.2381   Mean   :0   Mean   :0.8369  
                    3rd Qu.:0.0000   3rd Qu.:0   3rd Qu.:1.0000  
                    Max.   :4.0000   Max.   :0   Max.   :2.0000  
                                                                 
  Condition             KCLO           OpportunityLO  
 Length:6028        Length:6028        Min.   : 1.00  
 Class :character   Class :character   1st Qu.: 2.00  
 Mode  :character   Mode  :character   Median : 4.00  
                                       Mean   : 5.23  
                                       3rd Qu.: 7.00  
                                       Max.   :28.00 
PredictedErrorRateLO  KCSingleKC        OpportunitySingleKC
 Min.   :0.0126       Length:6028        Min.   :  1.00     
 1st Qu.:0.1083       Class :character   1st Qu.: 19.00     
 Median :0.1888       Mode  :character   Median : 38.00     
 Mean   :0.2113                          Mean   : 39.26     
 3rd Qu.:0.2947                          3rd Qu.: 57.00     
 Max.   :0.6903                          Max.   :125.00     
                                                            
 PredictedErrorRateSingleKC KCUniqueStep       OpportunityUniqueStep
 Min.   :0.0297             Length:6028        Min.   :1.000        
 1st Qu.:0.1325             Class :character   1st Qu.:1.000        
 Median :0.2029             Mode  :character   Median :1.000        
 Mean   :0.2113                                Mean   :1.179        
 3rd Qu.:0.2761                                3rd Qu.:1.000        
 Max.   :0.5827                                Max.   :6.000        
                                               NA's   :42           
 PredictedErrorRateUniqueStep
 Min.   :0.00000             
 1st Qu.:0.04965             
 Median :0.15370             
 Mean   :0.21049             
 3rd Qu.:0.31877             
 Max.   :0.92780             
 NA's   :42      

 It is important to also check for duplicates.

```{r}
duplicate_count <- sum(duplicated(algebraData$StudentID))
print(paste("Number of duplicate StudentID entries:", duplicate_count))
```

There are 5948 duplicates in StudentID, this can assist is figuring out how many students completed both the Pre-Test and Post-Test.

```{r}
table(PostTest$Sample)
table(PreTest$Sample)
```
Post-Test : 1600
Pre-Test : 1600
```{r}
table(PostTest$StudentId)
table(PreTest$Anon.Student.Id)
```
25940 25941 25942 25943 25944 25945 25946 25947 25948 25949 25950 25951 
   20    20    20    20    20    20    20    20    20    20    20    20 
25952 25953 25954 25955 25956 25957 25958 25959 25960 25961 25962 25963 
   20    20    20    20    20    20    20    20    20    20    20    20 
26030 26031 26032 26033 26034 26035 26036 26037 26038 26039 26040 26041 
   20    20    20    20    20    20    20    20    20    20    20    20 
26042 26043 26044 26045 26046 26047 26048 26049 26050 26051 26052 26053 
   20    20    20    20    20    20    20    20    20    20    20    20 
26054 26071 26072 26073 26074 26075 26076 26077 26078 26079 26080 26081 
   20    20    20    20    20    20    20    20    20    20    20    20 
26082 26083 26084 26085 26086 26087 26088 26089 26090 26091 26092 26093 
   20    20    20    20    20    20    20    20    20    20    20    20 
26094 26095 26096 26179 26185 26186 26208 26209 
   20    20    20    20    20    20    20    20

25940 25941 25942 25943 25944 25945 25946 25947 25948 25949 25950 25951 
   20    20    20    20    20    20    20    20    20    20    20    20 
25952 25953 25954 25955 25956 25957 25958 25959 25960 25961 25962 25963 
   20    20    20    20    20    20    20    20    20    20    20    20 
26030 26031 26032 26033 26034 26035 26036 26037 26038 26039 26040 26041 
   20    20    20    20    20    20    20    20    20    20    20    20 
26042 26043 26044 26045 26046 26047 26048 26049 26050 26051 26052 26053 
   20    20    20    20    20    20    20    20    20    20    20    20 
26054 26071 26072 26073 26074 26075 26076 26077 26078 26079 26080 26081 
   20    20    20    20    20    20    20    20    20    20    20    20 
26082 26083 26084 26085 26086 26087 26088 26089 26090 26091 26092 26093 
   20    20    20    20    20    20    20    20    20    20    20    20 
26094 26095 26096 26179 26185 26186 26208 26209 
   20    20    20    20    20    20    20    20 

The tables above informs us that each student completed the Pre-Test and Post-Test which is a total of 1600 total tests completed divided by 20 test per Student ID results in 80 students


Now it important to look for missing values in the dataset.
```{r}
MissingValues <- colSums(is.na(algebraData))
print(MissingValues)
```

                          Row                       Sample 
                           0                            0 
                   StudentID             ProblemHierarchy 
                           0                            0 
                 ProblemName                  ProblemView 
                           0                            0 
                    StepName                StepStartTime 
                           0                            0 
        FirstTransactionTime       CorrectTransactionTime 
                           0                            0 
                 StepEndTime                 StepDuration 
                           0                            0 
         CorrectStepDuration            ErrorStepDuration 
                           0                            0 
                FirstAttempt                    Incorrect 
                           0                            0 
                        Hint                      Correct 
                           0                            0 
                   Condition                         KCLO 
                           0                            0 
               OpportunityLO         PredictedErrorRateLO 
                           0                            0 
                  KCSingleKC          OpportunitySingleKC 
                           0                            0 
  PredictedErrorRateSingleKC                 KCUniqueStep 
                           0                            0 
       OpportunityUniqueStep PredictedErrorRateUniqueStep 
                          42                           42 

We must now calculate the spacing with timestamps.

```{r}
algebraData <- algebraData %>%
  arrange(StudentID, KCLO, OpportunityLO)

TimeDiff <- function(StartTime, EndTime) 
  as.numeric(difftime(strptime(EndTime, format="%Y-%m-%d %H:%M:%S"),
                      strptime(StartTime, format="%Y-%m-%d %H:%M:%S"), units="secs"))

algebraData <- algebraData %>%
  group_by(StudentID, KCLO) %>%
  mutate(Spacing = TimeDiff(lag(StepEndTime), StepStartTime))

average_spacing <- algebraData %>%
  group_by(KCLO) %>%
  summarize(AverageSpacing = mean(Spacing, na.rm = TRUE))

print(average_spacing)

```
Above is the spacing for each LO value. The total mean for all the values is 597087.13

```{r}
PreTest <- PreTest %>%
  arrange(Sample, KC..LO.)

PreTest <- PreTest %>%
  mutate(LOSegment = ntile(KC..LO., 10))

head(PreTest)

table(PreTest$LOSegment)


```

  1   2   3   4   5   6   7   8   9  10 
160 160 160 160 160 160 160 160 160 160 

```{r}
PostTest <- PostTest %>%
  arrange(Sample, KCLO)

PostTest <- PostTest %>%
  mutate(LOSegment = ntile(KCLO, 10))

head(PostTest)

table(PostTest$LOSegment)

```

  1   2   3   4   5   6   7   8   9  10 
160 160 160 160 160 160 160 160 160 160 

In order to begin the analysis, we look at each variable separately. To investigate the distribution of the quantitative variables, we utilize histogram and scatterplots while bar charts and tables for categorical variables. The response variable is FirstAttempt, which includes correct if they got the question right or incorrect if they got it wrong.

```{r}
table(algebraData$FirstAttempt)
```
correct incorrect 
     4754      1274 

This table shows that 4754 out of 6058 students got the question correct on the first attempt which is 78.87%

It is now important to learn how many students used Podsie.

```{r}
table(algebraData$Condition)
```
with (podsie_personal_deck) without (podsie_personal_deck) 
                          4747                           1281 

The table above shows how many students used Podise and how many didn't. 4747 students used Podsie out of 6028 which is 78.75% and 1281 did not which is 21.25% That number is very close to the number of students who got the question correct on the first attempt. However, according to the Pre-Registration, 40% should be witheld from the personal deck while 60% should be retained.
