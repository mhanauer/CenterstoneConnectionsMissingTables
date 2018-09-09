---
title: "ConnectionsTableData"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r, include=FALSE}
library(Amelia)
library(prettyR)
library(nlme)
library(descr)
library(foreign)
library(lme4)
library(sjstats)
library(MissMech)
library(BaylorEdPsych)
library(ggplot2)
library(HLMdiag)
library(psych)
library(stargazer)
library(MuMIn)

setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/ConnectionsPaperData")
GPRAAll = read.csv("ConnGPRA.csv", header = TRUE) 
# subet the data based on 1's for baseline and 2's for six-month.  Then write as CSV's, then merge together.
GPRAConBase = subset(GPRAAll, InterviewType ==1)
GPRAConMonth6 = subset(GPRAAll, InterviewType == 2)
GPRAAll = merge(GPRAConBase, GPRAConMonth6, by = "ClientID", all = TRUE)
write.csv(GPRAAll, "GPRAAll.csv", row.names = FALSE)
GPRAAll = read.csv("GPRAAll.csv", header = TRUE, na.strings = c(-9, -8, -7, -1, "", " ", "NULL", NULL, NA, "NA"))

GPRAAll$InterviewDate.x = as.Date(GPRAAll$InterviewDate.x, format = "%m/%d/%Y") 
GPRAAll$InterviewDate.y = as.Date(GPRAAll$InterviewDate.y, format = "%m/%d/%Y")

GPRAAll = subset(GPRAAll,LivingWhere.x !=4)
GPRAAll$LivingWhere.y = ifelse(GPRAAll$LivingWhere.y == 4, 1,0)
GPRAAll$LivingWhere.x = ifelse(GPRAAll$LivingWhere.x == 4, 1,0)

GPRAAll$Employment.x = ifelse(GPRAAll$Employment.x == 1, 1,0)
GPRAAll$Employment.y = ifelse(GPRAAll$Employment.y == 1, 1,0)
GPRAAll = subset(GPRAAll, Gender.x <= 2)
GPRAAll = subset(GPRAAll, Gender.y <= 2)
GPRAAll$Gender.x = ifelse(GPRAAll$Gender.x == 1, 1,0)
GPRAAll$Gender.y = ifelse(GPRAAll$Gender.y == 1, 1,0)
GPRAAll$EducationYears.x = ifelse(GPRAAll$EducationYears.x <= 12, 1, 0)
GPRAAll$EducationYears.y = ifelse(GPRAAll$EducationYears.y <= 12, 1, 0)
GPRAAll$County.x = ifelse(GPRAAll$County.x == "Monroe", 1, 0)
GPRAAll$County.y = ifelse(GPRAAll$County.y == "Monroe", 1, 0)

ConnPaper = data.frame(ClientID = GPRAAll$ClientID, InterviewDate.x = GPRAAll$InterviewDate.x, InterviewDate.y = GPRAAll$InterviewDate.y, Depression.x = GPRAAll$Depression.x, Depression.y = GPRAAll$Depression.y, 	Anxiety.x = GPRAAll$Anxiety.x, Anxiety.y = GPRAAll$Anxiety.y, Employment.x = GPRAAll$Employment.x, Employment.y = GPRAAll$Employment.y,ArrestedDays.x = GPRAAll$ArrestedDays.x, ArrestedDays.y = GPRAAll$ArrestedDays.y,LivingWhere.x = GPRAAll$LivingWhere.x, LivingWhere.y = GPRAAll$LivingWhere.y, HealthStatus.x = GPRAAll$HealthStatus.x,HealthStatus.y = GPRAAll$HealthStatus.y, IncomeWages.x = GPRAAll$IncomeWages.x, IncomeWages.y = GPRAAll$IncomeWages.y, Age.x = GPRAAll$Age.x, Age.y = GPRAAll$Age.y, EducationYears.x = GPRAAll$EducationYears.x, EducationYears.y = GPRAAll$EducationYears.y, Gender.x = GPRAAll$Gender.x, Gender.y = GPRAAll$Gender.y, DAUseIllegDrugsDays.x = GPRAAll$DAUseIllegDrugsDays.x, DAUseIllegDrugsDays.y = GPRAAll$DAUseIllegDrugsDays.y, County.x = GPRAAll$County.x, County.y = GPRAAll$County.y)

ConnPaper = subset(ConnPaper, InterviewDate.x < "2018-02-01")

PHQ9Base = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/PHQ9/PHQ9 Baseline.sav", to.data.frame = TRUE)
PHQ96month = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/PHQ9/PHQ9 6 Month.sav", to.data.frame = TRUE)
PHQ9All = merge(PHQ9Base, PHQ96month, by = "ParticipantID", all = TRUE)
GAD7Base = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/GAD7/GAD7 Baseline.sav", to.data.frame = TRUE)
GAD76month = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/GAD7/GAD7 6 Month.sav", to.data.frame = TRUE)
GAD7All = merge(GAD7Base, GAD76month, by = "ParticipantID", all = TRUE)

PHQ9Connections = data.frame(ClientID = PHQ9All$ParticipantID,PHQ9Base= PHQ9All$PHQ9Total.x, PHQ9Month6= PHQ9All$PHQ9Total.y)
GAD7Connections = data.frame(ClientID = GAD7All$ParticipantID, GAD7Base = GAD7All$GAD7Total.x, GAD7Month6 = GAD7All$GAD7Total.y)
PHQ9_GAD7 = merge(PHQ9Connections, GAD7Connections, by = "ClientID", all = TRUE)
head(PHQ9_GAD7)

PHQ9_GAD7 = merge(ConnPaper, PHQ9_GAD7, by = "ClientID", all = TRUE)
PHQ9_GAD7 = subset(PHQ9_GAD7, InterviewDate.x < "2018-02-01")
dim(PHQ9_GAD7)
head(PHQ9_GAD7)
sum(is.na(PHQ9_GAD7$LivingWhere.x))


PHQ9_GAD7Long = reshape(PHQ9_GAD7, varying = list(c("PHQ9Base", "PHQ9Month6"), c("GAD7Base", "GAD7Month6"), c("DAUseIllegDrugsDays.x", "DAUseIllegDrugsDays.y"), c("InterviewDate.x", "InterviewDate.y"), c("LivingWhere.x", "LivingWhere.y"), c("Employment.x", "Employment.y"), c("Depression.x", "Depression.y"), c("HealthStatus.x", "HealthStatus.y"), c("Gender.x", "Gender.y"), c("EducationYears.x", "EducationYears.y"), c("IncomeWages.x", "IncomeWages.y"), c("ArrestedDays.x", "ArrestedDays.y"), c("Anxiety.x", "Anxiety.y"), c("Age.x", "Age.y"), c("County.x", "County.y")), times = c(0,1), direction = "long")

```

Descriptives for base and 6month for complete data
```{r}
PHQ9_GAD7LongComplete = na.omit(PHQ9_GAD7Long)

PHQ9_GAD7LongBaseComplete = subset(PHQ9_GAD7LongComplete, PHQ9_GAD7LongComplete$time == 0)
PHQ9_GAD7LongMonth6Complete = subset(PHQ9_GAD7LongComplete, PHQ9_GAD7LongComplete$time == 1)

describe(PHQ9_GAD7LongBaseComplete)
describe(PHQ9_GAD7LongMonth6Complete)

```
Missing value descirptives for base
```{r}
write.csv(PHQ9_GAD7Long, "PHQ9_GAD7Long.csv", row.names = FALSE)
PHQ9_GAD7Long = read.csv("PHQ9_GAD7Long.csv", header = TRUE)
PHQ9_GAD7LongMissingDes = PHQ9_GAD7Long
PHQ9_GAD7LongMissingDes$InterviewDate.x = NULL
PHQ9_GAD7LongMissingDes$id = NULL
m = 10
PHQ9_GAD7LongMissingDesOut = amelia(m = 10, PHQ9_GAD7LongMissingDes, noms = c("Employment.x", "LivingWhere.x", "Gender.x", "EducationYears.x"), ords = c("Depression.x", "Anxiety.x","ArrestedDays.x", "DAUseIllegDrugsDays.x", "County.x"), ts = "time")


datAnalysisAll = lapply(1:m, function(x){PHQ9_GAD7LongMissingDesOut$imputations[[x]]})


datAnalysisAllDes = lapply(1:m, function(x){subset(datAnalysisAll[[x]], time == 0)})

nForBase = NULL

for(i in 1:m){
  nForBase[[i]] = dim(datAnalysisAllDes[[i]])[1]
}
nForBase


mean.out = NULL
for(i in 1:m) {
  mean.out[[i]] = apply(datAnalysisAllDes[[i]], 2, mean)
  mean.out = data.frame(mean.out)
}

descFun = function(x){
  x = data.frame(t(x))
}
mean.out = descFun(mean.out)

# now get sds
sd.out = NULL
for(i in 1:m) {
  sd.out[[i]] = apply(datAnalysisAllDes[[i]], 2, sd)
  sd.out = data.frame(sd.out)
}
sd.out = descFun(sd.out)
mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out

```
Now descriptives for 6-month
```{r}
datAnalysisAll = lapply(1:m, function(x){PHQ9_GAD7LongMissingDesOut$imputations[[x]]})


datAnalysisAllDes = lapply(1:m, function(x){subset(datAnalysisAll[[x]], time == 1)})

nForMonth6 = NULL

for(i in 1:m){
  nForMonth6[[i]] = dim(datAnalysisAllDes[[i]])[1]
}
nForMonth6

mean.out = NULL
for(i in 1:m) {
  mean.out[[i]] = apply(datAnalysisAllDes[[i]], 2, mean)
  mean.out = data.frame(mean.out)
}

descFun = function(x){
  x = data.frame(t(x))
}
mean.out = descFun(mean.out)

# now get sds
sd.out = NULL
for(i in 1:m) {
  sd.out[[i]] = apply(datAnalysisAllDes[[i]], 2, sd)
  sd.out = data.frame(sd.out)
}
sd.out = descFun(sd.out)
mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out

```

```{r, include=FALSE}
head(PHQ9_GAD7Long)

PHQ9_GAD7LongBinary = data.frame(ClientID = PHQ9_GAD7Long$ClientID, time = PHQ9_GAD7Long$time, Gender.x = PHQ9_GAD7Long$Gender.x, LivingWhere.x = PHQ9_GAD7Long$LivingWhere.x, Employment.x = PHQ9_GAD7Long$Employment.x, EducationYears.x = PHQ9_GAD7Long$EducationYears.x, DAUseIllegDrugsDays.x = PHQ9_GAD7Long$DAUseIllegDrugsDays.x, Depression.x = PHQ9_GAD7Long$Depression.x, InterviewDate.x = PHQ9_GAD7Long$InterviewDate.x, ArrestedDays.x = PHQ9_GAD7Long$ArrestedDays.x, Anxiety.x = PHQ9_GAD7Long$Anxiety.x, County.x = PHQ9_GAD7Long$County.x)

PHQ9_GAD7LongCon = data.frame(HealthStatus.x = PHQ9_GAD7Long$HealthStatus.x, IncomeWages.x = PHQ9_GAD7Long$IncomeWages.x, Age.x = PHQ9_GAD7Long$Age.x, PHQ9Base = PHQ9_GAD7Long$PHQ9Base, GAD7Base = PHQ9_GAD7Long$GAD7Base)
summary(PHQ9_GAD7LongCon)

write.csv(PHQ9_GAD7LongCon, "PHQ9_GAD7LongCon.csv", row.names = FALSE)
PHQ9_GAD7LongCon = read.csv("PHQ9_GAD7LongCon.csv", header = TRUE)

PHQ9_GAD7LongCon = scale(PHQ9_GAD7LongCon, center = TRUE, scale = FALSE)

PHQ9_GAD7LongCenter = data.frame(PHQ9_GAD7LongBinary, PHQ9_GAD7LongCon)
write.csv(PHQ9_GAD7LongCenter, "PHQ9_GAD7LongCenter.csv", row.names = FALSE)
PHQ9_GAD7LongCenter = read.csv("PHQ9_GAD7LongCenter.csv", header = TRUE)

summary(PHQ9_GAD7LongCenter)

head(PHQ9_GAD7LongCenter)
PHQ9_GAD7LongCenter$InterviewDate.x = NULL
m = 10
PHQ9_GAD7LongCenterOut = amelia(m = 10, PHQ9_GAD7LongCenter, noms = c("Employment.x", "LivingWhere.x", "Gender.x", "EducationYears.x", "County.x"), ords = c("Depression.x", "Anxiety.x","ArrestedDays.x", "DAUseIllegDrugsDays.x"), ts = "time")

summary(PHQ9_GAD7LongCenterOut)

compare.density(PHQ9_GAD7LongCenterOut, var = "HealthStatus.x")


PHQ9_GAD7LongCenter = lapply(1:m, function(x){PHQ9_GAD7LongCenterOut$imputations[[x]]})
head(PHQ9_GAD7LongCenter)

PHQ9_GAD7LongCenter = na.omit(PHQ9_GAD7LongCenter)
head(PHQ9_GAD7LongCenter[[1]])
dim(PHQ9_GAD7LongCenter[[1]])

```
Data Analysis: Factors with housing: Employment + Depression.x + County.x
```{r}
output = list()
coef_output =  NULL
se_output = NULL
df= NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = glmer(LivingWhere.x ~ Employment.x+ Depression.x + HealthStatus.x + Gender.x + EducationYears.x + Age.x + DAUseIllegDrugsDays.x + County.x + (1 | ClientID), data  = PHQ9_GAD7LongCenter[[i]], family = "binomial")
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
  df[[i]] = output[[i]]$AICtab[5]
}
coef_output = data.frame(coef_output)
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}

coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  options(scipen=999)
  p = round((2*pt(-abs(t_stat), df = df[1])),3)
  return(data.frame(coefs1, ses1, t_stat, p))
}
results = meldAllT_stat(coef_output, se_output)
round(results,3) 
```

