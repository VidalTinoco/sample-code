##### compilation of my codes #####
##### Vidal Humberto Mendoza Tinoco

#### Paper replication ####
#### Title: Civil War and Social Cohesion: Lab-in-the-Field Evidence from Nepal
#### Authors: Michael J. Gilligan,  Benjamin J. Pasquale, Cyrus Samii 
### Note: the data was taken from harvard dataverse and the original code is written in stata, the task was to replicate the paper but with original code made by me in R

### empty workspace
rm (list=ls ())

### installing new packages
install.packages("lmtest")

### libraries
library(dplyr)
library(ggplot2)
library(ggthemes) 
library(lfe)
library(readstata13)
library(readxl)
library(sf)
library(tidyverse)
library(tidyr)
library(units)
library(viridis)
library(miceadds)
library(stargazer)
library(lme4)
library(lmtest)


### Options
options (digits=4, scipen=8, show.signif.stars=FALSE, dplyr.width=Inf, tibble.print_max=Inf, tibble.print_min=1)

### Setting the working directory
setwd("/Users/vidal/Documents/MPA/Final")

### Data
nepgames <- read.dta13("dataverse_files/nep-games-data.dta")
summary(nepgames)
dim(nepgames)
view(nepgames)

nepsurvey <- read.dta13("dataverse_files/nep-survey-data.dta")
dim(nepsurvey)
summary(nepsurvey)
view(nepsurvey)

###Converting variables to factor
nepgames <- nepgames %>% mutate(cast_hh = factor(cast_hh),
                                district = factor(district),
                                sex_hh = factor(sex_hh),
                                vdc = factor(vdc))
summary(nepgames)


### Table 1

## Converting variables to factor
nepsurvey <- nepsurvey %>% mutate(matchstrat= factor(matchstrat))
summary(nepsurvey)

## fats1000 = 0
ns <- nepsurvey %>% filter(fats1000==0)

## Modeles
r1.1 <- felm(turnout~1, weights = ns$stratwgt, data=ns)
noviol_m <- coeftest(r1.1)[1,1] 
noviol_m <- round(noviol_m, 3)


r1.2 <- felm(turnout~fats1000|matchstrat|0|vdc_name, weights=nepsurvey$stratwgt, data=nepsurvey)

r1.3 <- felm(groupsx~1, weights=ns$stratwgt, data=ns)
noviol_m2<-coeftest(r1.3)[1,1]
noviol_m2<-round(noviol_m2, 3)

r1.4 <- felm(groupsx~fats1000|matchstrat|0|vdc_name, weight=nepsurvey$stratwgt, data=nepsurvey)

## Table
stargazer(r1.2,r1.4, 
          omit=c("matchstrat2", "matchstrat3", "matchstrat4", "matchstrat5", "matchstrat6", "matchstrat7",
                 "matchstrat8", "matchstrat9", "matchstrat10", "Constant"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Baseline (no violence)", noviol_m, noviol_m2)),
          dep.var.caption="",
          covariate.labels = c("Fatalities per 1,000"),
          title="TABLE 1 Effects of Violence on Collective Behavior",
          dep.var.labels=c("Voter Turnout", "Community Groups Index"), 
          notes.append = F,
          notes.align = "l",
          notes = c("Standard errors in parentheses. Weighted least squares with matched-pair block fixed effects. Robust standard errors clustered by",
                    "VDC. ∗p < 0.10, ∗∗p < 0.05, ∗∗∗p < 0.01 (two-sided tests)."),
          type="html", out="tyg/Final Tabla 1.html")




### Table 2
tabla2<-nepgames%>%
  select(lot_choice, dict_sent, cooperate, s_sent, sharereturn, Female, brahmin,
         chhetri, magarrai, Literate, age_hh)

neps<-nepgames%>%
  filter(vdctag==1)%>%
  select(lpop2, roaddist)

tabla2$lpop2<-replicate(252, 0)
tabla2$roaddist<-replicate(252, 0)
tabla2[1:12,12]<-neps[1:12,1]
tabla2[1:12,13]<-neps[1:12,2]
tabla2[,12][tabla2[,12]==0]<-NA
tabla2[,13][tabla2[,13]==0]<-NA


stargazer(tabla2, 
          digits=3,
          summary.stat=c("n","mean", "sd", "min","max"),
          align=T,
          covariate.labels=c("Lottery choice", "Amount sent in Rps. (dictator game)", "Cooperate (public-goods game)", "Amount sent in Rps. (trust game)", "Share returned (trust game)", "Female", "Brahmin", 
                             "Chhetri", "Magar/Rai", "Literate", "Age", "Log VDC population", "Distance to nearest road from VDC (km)"),
          title="TABLE 2 Summary Statistics",
          type="html", out = "tyg/Final Tabla 2.html")


### Table 3

## Converting variables to factor
nepgames <- nepgames %>% mutate(dist_block = factor(dist_block))

## violence = 0 
ng <- nepgames %>%  filter(violence==0)

## Modeles
#Lottery choice in risk game
r3.1 <- felm(lot_choice~1, weights = ng$vdc_weight, data=ng)
noviol_m31 <- coeftest(r3.1)[1,1] 
noviol_m31 <- round(noviol_m31, 2)
r3.2 <- felm(lot_choice~violence|dist_block|0|wardunique, weights=nepgames$vdc_weight, data=nepgames)

#Amount send in dictator game
r3.3 <- felm(dict_sent~1, weights = ng$vdc_weight, data=ng)
noviol_m33 <- coeftest(r3.3)[1,1] 
noviol_m33 <- round(noviol_m33, 2)
r3.4 <- felm(dict_sent~violence|dist_block|0|wardunique, weights=nepgames$vdc_weight, data=nepgames)

#Cooperate in obligation game
r3.5 <- felm(cooperate~1, weights = ng$vdc_weight, data=ng)
noviol_m35 <- coeftest(r3.5)[1,1] 
noviol_m35 <- round(noviol_m35, 2)
r3.6 <- felm(cooperate~violence|dist_block|0|wardunique, weights=nepgames$vdc_weight, data=nepgames)

#Amount sent in trust game
r3.7 <- felm(s_sent~1, weights = ng$vdc_weight, data=ng)
noviol_m37 <- coeftest(r3.7)[1,1] 
noviol_m37 <- round(noviol_m37, 2)
r3.8 <- felm(s_sent~violence|dist_block|0|wardunique, weights=nepgames$vdc_weight, data=nepgames)

#Share returned in trust game
r3.9 <- felm(sharereturn~1, weights = ng$vdc_weight, data=ng)
noviol_m39 <- coeftest(r3.9)[1,1] 
noviol_m39 <- round(noviol_m39, 2)
r3.10 <- felm(sharereturn~violence|dist_block|0|wardunique, weights=nepgames$vdc_weight, data=nepgames)

#Index
r3.11 <- felm(Index~1, weights = ng$vdc_weight, data=ng)
noviol_m311 <- coeftest(r3.11)[1,1] 
noviol_m311 <- round(noviol_m311, 2)
r3.12 <- felm(Index~violence|dist_block|0|wardunique, weights=nepgames$vdc_weight, data=nepgames)

##Table
stargazer(r3.2,r3.4,r3.6,r3.8,r3.10,r3.12, 
          keep.stat = c("n", "rsq"),
          digits=2,
          omit=c("dist_blockDolakha", "dist_blockDoti", "dist_blockPalpa/Syangja", "dist_blockRolpa", "dist_blockUdayapur", "Constant"),
          add.lines = list(c("Baseline (no violence)", noviol_m31, noviol_m33, noviol_m35, noviol_m37, noviol_m39, noviol_m311)),
          dep.var.caption="",
          covariate.labels = c("Violence"),
          title="TABLE 3 Main Results",
          dep.var.labels=c("Lottery Risk", "Dictator", "Cooperate", "Trust Sent", "Trust Return", "Soc. Index"), 
          notes.append = F,
          notes.align = "l",
          notes = c("Standard errors in parentheses. Weighted least squares with matched-pair block fixed effects. Robust standard errors clustered by",
                    "ward. Soc. Index is inverse-covariance weighted average of outcomes 2–5. ∗p < 0.10, ∗∗p < 0.05, ∗∗∗p < 0.01 (two-sided tests"),
          type="html", out="tyg/Final Tabla 3.html")




### Table 5

## creating new variables
mean_age = 52.2
nepgames <- nepgames %>% mutate(age_hhc = age_hh - mean_age,
                                ViolenceXAge = violence*age_hhc,
                                ViolenceXLiterate = violence*Literate,
                                AgeXRoad = age_hhc*roaddist,
                                LitXRoad = Literate*roaddist,
                                ViolXAgeXRoad = violence*age_hhc*roaddist,
                                ViolXLitXRoad = violence*Literate*roaddist)
summary(nepgames)


## violence = 0
#(By themselves these models are run)
ng <- nepgames %>% filter(violence == 0)

## Converting variables to factor
#(By themselves these models are run)
nepgames <- nepgames %>% mutate(dist_block = factor(dist_block))
nepgames <- nepgames %>% mutate(sex_hh = factor(sex_hh),
                                cast_hh = factor(cast_hh))


## Modeles 
# Index 1
r5.1 <- felm(Index~1, weights = ng$vdc_weight, data = ng)
noviol_m51 <- coeftest(r5.1)[1,1]
noviol_m51 <- round(noviol_m51, 2)
r5.2 <- felm(Index~violence+age_hhc+ViolenceXAge|dist_block|0|wardunique, weights = nepgames$vdc_weight, data = nepgames)

# Index 2
r5.3 <- felm(Index~1, weights = ng$vdc_weight, data = ng)
noviol_m53 <- coeftest(r5.3)[1,1]
noviol_m53 <- round(noviol_m53, 2)
r.54 <- felm(Index~violence+Literate+ViolenceXLiterate|dist_block|0|wardunique, weights = nepgames$vdc_weight, data = nepgames)

# Index 3
r5.5 <- felm(Index~1, weights = ng$vdc_weight, data = ng)
noviol_m55 <- coeftest(r5.5)[1,1]
noviol_m55 <- round(noviol_m55, 2)
r5.6 <- felm(Index~violence+age_hhc+ViolenceXAge+Literate+ViolenceXLiterate|dist_block|0|wardunique, weights = nepgames$vdc_weight, data = nepgames)

# Index 4
r5.7 <- felm(Index~1, weights = ng$vdc_weight, data = ng)
noviol_m57 <- coeftest(r5.7)[1,1]
noviol_m57 <- round(noviol_m57, 2)
r5.8 <- felm(Index~violence+age_hhc+ViolenceXAge+Literate+ViolenceXLiterate|sex_hh+cast_hh+dist_block|0|wardunique, weights = nepgames$vdc_weight, data = nepgames)

# Index 5
r5.9 <- felm(Index~1, weights = ng$vdc_weight, data = ng)
noviol_m59 <- coeftest(r5.9)[1,1]
noviol_m59 <- round(noviol_m59, 2)
r5.10 <- felm(Index~violence+age_hhc+ViolenceXAge+Literate+ViolenceXLiterate+roaddist|sex_hh+cast_hh+dist_block|0|wardunique, weights = nepgames$vdc_weight, data = nepgames)

# Index 6
r5.11 <- felm(Index~1, weights = ng$vdc_weight, data = ng)
noviol_m511 <- coeftest(r5.9)[1,1]
noviol_m511 <- round(noviol_m511, 2)
r5.12 <- felm(Index~violence+age_hhc+ViolenceXAge+Literate+ViolenceXLiterate+roaddist+violXroaddist|sex_hh+cast_hh+dist_block|0|wardunique, weights = nepgames$vdc_weight, data = nepgames)

## Table
stargazer(r5.2, r.54, r5.6, r5.8, r5.10, r5.12,
          keep.stat = c("n", "rsq"),
          digits = 2,
          omit = c("dist_blockDolakha", "dist_blockDoti", "dist_blockPalpa/Syangja", "dist_blockRolpa", "dist_blockUdayapur", "Constant"),
          add.lines = list(c("Baseline (no violence)", noviol_m51, noviol_m53, noviol_m55, noviol_m57, noviol_m59, noviol_m511)),
          dep.var.caption = "",
          covariate.labels = c("Violence", "Age", "Violence x Age", "Literate", "Violence x Literate", "Dist. to road (km)", "Viol. x Dist. to road"),
          title = "TABLE 5 Effects of Violence on the Human Capital and Sociality Relationship",
          dep.var.labels = c("Soc. Index"),
          notes.append = F,
          notes.align = "l",
          notes = c("Standard errors in parentheses. Weighted least squares with matched-pair block fixed effects. Robust standard errors clustered by ward. “Soc. Index” is inverse-covariance weighted average of outcomes 2–5. “Age” is centered on its mean (52). Models 4–6 control for caste- and gender-fixed effects. ∗p < 0.10, ∗∗p < 0.05, ∗∗∗p < 0.01 (two-sided tests)."),
          type = "html",
          out = "tyg/Final Tabla 5.html")


### teacher's instructions

## Table 1
#a) unweighted
r1.1.1a <- felm(turnout~1, data=ns)
noviol_m.1a <- coeftest(r1.1.1a)[1,1] 
noviol_m.1a <- round(noviol_m.1a, 3)


r1.2.1a <- felm(turnout~fats1000|matchstrat|0|vdc_name, data=nepsurvey)

r1.3.1a <- felm(groupsx~1, data=ns)
noviol_m2.1a<-coeftest(r1.3.1a)[1,1]
noviol_m2.1a<-round(noviol_m2.1a, 3)

r1.4.1a <- felm(groupsx~fats1000|matchstrat|0|vdc_name, data=nepsurvey)

#Table
stargazer(r1.2.1a,r1.4.1a, 
          omit=c("matchstrat2", "matchstrat3", "matchstrat4", "matchstrat5", "matchstrat6", "matchstrat7",
                 "matchstrat8", "matchstrat9", "matchstrat10", "Constant"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Baseline (no violence)", noviol_m.1a, noviol_m2.1a)),
          dep.var.caption="",
          covariate.labels = c("Fatalities per 1,000"),
          title="TABLE 1 Effects of Violence on Collective Behavior (SIN PONDERAR)",
          dep.var.labels=c("Voter Turnout", "Community Groups Index"), 
          notes.append = F,
          notes.align = "l",
          type="html", out="tyg/Final Tabla 1 (sin ponderar).html")

#b) without fixed effects
r1.1.1b <- felm(turnout~1, weights = ns$stratwgt, data=ns)
noviol_m.1b <- coeftest(r1.1.1b)[1,1] 
noviol_m.1b <- round(noviol_m.1b, 3)


r1.2.1b <- felm(turnout~fats1000|0|0|vdc_name, weights=nepsurvey$stratwgt, data=nepsurvey)

r1.3.1b <- felm(groupsx~1, weights=ns$stratwgt, data=ns)
noviol_m2.1b<-coeftest(r1.3.1b)[1,1]
noviol_m2.1b<-round(noviol_m2.1b, 3)

r1.4.1b <- felm(groupsx~fats1000|0|0|vdc_name, weight=nepsurvey$stratwgt, data=nepsurvey)

#Table
stargazer(r1.2.1b,r1.4.1b, 
          omit=c("matchstrat2", "matchstrat3", "matchstrat4", "matchstrat5", "matchstrat6", "matchstrat7",
                 "matchstrat8", "matchstrat9", "matchstrat10", "Constant"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Baseline (no violence)", noviol_m.1b, noviol_m2.1b)),
          dep.var.caption="",
          covariate.labels = c("Fatalities per 1,000"),
          title="TABLE 1 Effects of Violence on Collective Behavior (SIN EFECTOS FIJOS)",
          dep.var.labels=c("Voter Turnout", "Community Groups Index"), 
          notes.append = F,
          notes.align = "l",
          type="html", out="tyg/Final Tabla 1 (sin efectos fijos).html")

#c) unweighted and without fixed effects
r1.1.1c <- felm(turnout~1, data=ns)
noviol_m.1c <- coeftest(r1.1.1c)[1,1] 
noviol_m.1c <- round(noviol_m.1c, 3)


r1.2.1c <- felm(turnout~fats1000|0|0|vdc_name, data=nepsurvey)

r1.3.1c <- felm(groupsx~1, data=ns)
noviol_m2.1c<-coeftest(r1.3.1c)[1,1]
noviol_m2.1c<-round(noviol_m2.1c, 3)

r1.4.1c <- felm(groupsx~fats1000|0|0|vdc_name, data=nepsurvey)

#Table
stargazer(r1.2.1c,r1.4.1c, 
          omit=c("matchstrat2", "matchstrat3", "matchstrat4", "matchstrat5", "matchstrat6", "matchstrat7",
                 "matchstrat8", "matchstrat9", "matchstrat10", "Constant"),
          keep.stat = c("n", "rsq"),
          add.lines = list(c("Baseline (no violence)", noviol_m.1c, noviol_m2.1c)),
          dep.var.caption="",
          covariate.labels = c("Fatalities per 1,000"),
          title="TABLE 1 Effects of Violence on Collective Behavior (SIN EFECTOS FIJOS NI PONDERAR)",
          dep.var.labels=c("Voter Turnout", "Community Groups Index"), 
          notes.append = F,
          notes.align = "l",
          type="html", out="tyg/Final Tabla 1 (sin efectos fijos ni ponderar).html")

## Table 3, column 6

#a) unweighted
r3.11.a <- felm(Index~1, data=ng)
noviol_m311.a <- coeftest(r3.11.a)[1,1] 
noviol_m311.a <- round(noviol_m311.a, 2)
r3.12.a <- felm(Index~violence|dist_block|0|wardunique, data=nepgames)

#b) without fixed effects
r3.11.b <- felm(Index~1, weights = ng$vdc_weight, data=ng)
noviol_m311.b <- coeftest(r3.11.b)[1,1] 
noviol_m311.b <- round(noviol_m311.b, 2)
r3.12.b <- felm(Index~violence|0|0|wardunique, weights=nepgames$vdc_weight, data=nepgames)

#c) unweighted and without fixed effects
r3.11.c <- felm(Index~1, data=ng)
noviol_m311.c <- coeftest(r3.11.c)[1,1] 
noviol_m311.c <- round(noviol_m311.c, 2)
r3.12.c <- felm(Index~violence|0|0|wardunique, data=nepgames)

#Table 
#a)
stargazer(r3.12.a,
          keep.stat = c("n", "rsq"),
          digits=2,
          omit=c("dist_blockDolakha", "dist_blockDoti", "dist_blockPalpa/Syangja", "dist_blockRolpa", "dist_blockUdayapur", "Constant"),
          add.lines = list(c("Baseline (no violence)", noviol_m311.a)),
          dep.var.caption="",
          covariate.labels = c("Violence"),
          title="TABLE 3 Column 6 a)",
          dep.var.labels=c("Sin Ponderar"), 
          notes.append = F,
          notes.align = "l",
          type="html", out="tyg/Final Tabla 3 columna 6 a).html")

#b)
stargazer(r3.12.b,
          keep.stat = c("n", "rsq"),
          digits=2,
          omit=c("dist_blockDolakha", "dist_blockDoti", "dist_blockPalpa/Syangja", "dist_blockRolpa", "dist_blockUdayapur", "Constant"),
          add.lines = list(c("Baseline (no violence)", noviol_m311.b)),
          dep.var.caption="",
          covariate.labels = c("Violence"),
          title="TABLE 3 Column 6 b)",
          dep.var.labels=c("Sin Efectos Fijos"), 
          notes.append = F,
          notes.align = "l",
          type="html", out="tyg/Final Tabla 3 columna 6 b).html")

#c)
stargazer(r3.12.c,
          keep.stat = c("n", "rsq"),
          digits=2,
          omit=c("dist_blockDolakha", "dist_blockDoti", "dist_blockPalpa/Syangja", "dist_blockRolpa", "dist_blockUdayapur", "Constant"),
          add.lines = list(c("Baseline (no violence)", noviol_m311.c)),
          dep.var.caption="",
          covariate.labels = c("Violence"),
          title="TABLE 3 Column 6 c)",
          dep.var.labels=c("Sin Ponderar ni Efectos Fijos"), 
          notes.append = F,
          notes.align = "l",
          type="html", out="tyg/Final Tabla 3 columna 6 c).html")


### Graphs Table 1, model 1
##Pooled
(pool <- ggplot (nepsurvey, aes(x=fats1000, y=turnout))
  + geom_jitter (alpha=0.25, col="forestgreen")
  + geom_smooth (method="lm", se=F, col="indianred3")
  + ylab ("Voter Turnout")
  + xlab ("Fatalities per 1,000")
  + ggtitle ("Pooled")
  + theme_classic (base_size=14)
  + theme (legend.title=element_blank ()))
pool


##Within

summary(nepsurvey)
#average deviations
nepsurvey <- nepsurvey %>% group_by(matchstrat) %>% mutate(
  fats1000_mean = mean(fats1000, na.rm=T),
  fats1000_desv = (fats1000 - fats1000_mean),
  turnout_mean = mean(turnout, na.rm=T),
  turnout_desv = (turnout - turnout_mean)
)
summary(nepsurvey)

#Graph
(gwith <- ggplot (nepsurvey, aes (x=fats1000_desv, y=turnout_desv))
  + geom_jitter (height=0.1, width=0.1, alpha=0.25, col="forestgreen")
  + geom_smooth (method="lm", se=F, col="indianred3")
  + ylab ("Turnout\n(desvio respecto al promedio matchstrat)")
  + xlab ("Fatalities per 1,000\n(desvio respecto al promedio matchstrat)")
  + ggtitle ("Within")
  + theme_classic (base_size=14)
  + theme (legend.title=element_blank ()))


## Between
# new df, by matchstrat
neps_btw <- nepsurvey %>% group_by (matchstrat) %>% summarise (
  fats1000_mean = mean (fats1000, na.rm=T)
  , turnout_mean = mean (turnout, na.rm=T))
summary (neps_btw)

# Graph
(gbtw <- ggplot (neps_btw, aes (x=fats1000_mean, y=turnout_mean))
  + geom_text (aes (label=matchstrat), size=2.75)
  + geom_smooth (method="lm", se=F, col="indianred3")
  + ylab ("proporcion Turnout")
  + xlab ("proporcion Fatalities per 1,000")
  + ggtitle ("Between")
  + theme_classic (base_size=14)
  + theme (legend.title=element_blank ()))




### Graphs Table 1, model 2
##Pooled
(pool2 <- ggplot (nepsurvey, aes(x=fats1000, y=groupsx))
  + geom_jitter (alpha=0.25, col="forestgreen")
  + geom_smooth (method="lm", se=F, col="indianred3")
  + ylab ("Community Groups Index")
  + xlab ("Fatalities per 1,000")
  + ggtitle ("Pooled")
  + theme_classic (base_size=14)
  + theme (legend.title=element_blank ()))
pool2


##Within

summary(nepsurvey)
#average deviations
nepsurvey <- nepsurvey %>% group_by(matchstrat) %>% mutate(
  fats1000_mean = mean(fats1000, na.rm=T),
  fats1000_desv = (fats1000 - fats1000_mean),
  groupsx_mean = mean(groupsx, na.rm=T),
  groupsx_desv = (groupsx - groupsx_mean)
)
summary(nepsurvey)

#Graph
(gwith2 <- ggplot (nepsurvey, aes (x=fats1000_desv, y=groupsx_desv))
  + geom_jitter (height=0.1, width=0.1, alpha=0.25, col="forestgreen")
  + geom_smooth (method="lm", se=F, col="indianred3")
  + ylab ("Community Groups Index\n(desvio respecto al promedio matchstrat)")
  + xlab ("Fatalities per 1,000\n(desvio respecto al promedio matchstrat)")
  + ggtitle ("Within")
  + theme_classic (base_size=14)
  + theme (legend.title=element_blank ()))


## Between
# new df by matchstrat
neps_btw2 <- nepsurvey %>% group_by (matchstrat) %>% summarise (
  fats1000_mean = mean (fats1000, na.rm=T)
  , groupsx_mean = mean (groupsx, na.rm=T))
summary (neps_btw2)

# Graph
(gbtw2 <- ggplot (neps_btw2, aes (x=fats1000_mean, y=groupsx_mean))
  + geom_text (aes (label=matchstrat), size=2.75)
  + geom_smooth (method="lm", se=F, col="indianred3")
  + ylab ("proporcion Community Groups Index")
  + xlab ("proporcion Fatalities per 1,000")
  + ggtitle ("Between")
  + theme_classic (base_size=14)
  + theme (legend.title=element_blank ()))



### Table 3, model 6
summary(nepgames)
##Pooled
(pool3 <- ggplot (nepgames, aes(x=violence, y=Index))
  + geom_jitter (alpha=0.25, col="forestgreen")
  + geom_smooth (method="lm", se=F, col="indianred3")
  + ylab ("Index")
  + xlab ("Violence")
  + ggtitle ("Pooled")
  + theme_classic (base_size=14)
  + theme (legend.title=element_blank ()))
pool3



##Within

summary(nepsurvey)
#average deviations
nepgames <- nepgames %>% group_by(dist_block) %>% mutate(
  violence_mean = mean(violence, na.rm=T),
  violence_desv = (violence - violence_mean),
  Index_mean = mean(Index, na.rm=T),
  Index_desv = (Index - Index_mean)
)
summary(nepgames)

#Graph
(gwith3 <- ggplot (nepgames, aes (x=violence_desv, y=Index_desv))
  + geom_jitter (height=0.1, width=0.1, alpha=0.25, col="forestgreen")
  + geom_smooth (method="lm", se=F, col="indianred3")
  + ylab ("Social Index\n(desvio respecto al promedio matchstrat)")
  + xlab ("Violence\n(desvio respecto al promedio matchstrat)")
  + ggtitle ("Within")
  + theme_classic (base_size=14)
  + theme (legend.title=element_blank ()))


##Between
# new df by dist_block
nepg_btw <- nepgames %>% group_by (dist_block) %>% summarise (
  violence_mean = mean (violence, na.rm=T)
  , Index_mean = mean (Index, na.rm=T))
summary (nepg_btw)

# Graph
(gbtw3 <- ggplot (nepg_btw, aes (x=violence_mean, y=Index_mean))
  + geom_text (aes (label=dist_block), size=2.75)
  + geom_smooth (method="lm", se=F, col="indianred3")
  + ylab ("proporcion Social Index")
  + xlab ("proporcion Violence")
  + ggtitle ("Between")
  + theme_classic (base_size=14)
  + theme (legend.title=element_blank ()))






#### DPSS Capstone Project ####
#### The Political Legacy of the George Floyd Protests ####

#### empty workspace
rm (list=ls ())

#### libraries
library(sf)
library(tidyverse)
library(tidycensus)
library(dplyr)
library(ggplot2)
library(usmap)
library(lfe)
library(stargazer)


#### setting working directory
setwd("/Users/vidal/Documents/DPSS/Week 7")


##### 2. Using data from Ipsos, map the locations of protests. Also map several key features of these protests: size, escalated to violence, and whether they involved police violence.

#### Loadings and exploring data
gf_exports <- read_csv("george-floyd-exports-june-22.csv")
view(gf_exports)
summary(gf_exports)

gf_fips <- read_csv("gf_w_fips.csv")
view(gf_fips)
summary(gf_fips)


### Plotting with usmaps

p_size <- plot_usmap(data = gf_exports, regions = c("state"), values = "size") + labs(title = "Protest's Size", caption = "Data from IPSOS") + theme(legend.position = "bottom") + scale_fill_discrete(name = "Protest's Size")
p_size

p_escviol <- plot_usmap(data = gf_exports, regions = c("state"), values = "type_of_gathering") + labs(title = "Escalated to Violence", caption = "Data from IPSOS") + theme(legend.position = "bottom") + scale_fill_discrete(name = "Violence")
p_escviol

p_polviol <- plot_usmap(data = gf_exports, regions = c("state"), values = "police_altercation") + labs(title = "Police Violence", caption = "Data from IPSOS")+ theme(legend.position = "bottom") + scale_fill_discrete(name = "Police Violence")
p_polviol


##### 3. Collapse the protest data by county and generate several variables.

### Loading and exploring data
county <- read_csv("tl_2016_us_county/tl_2016_us_county/all_county_list.csv")
view(county)
summary(county)

county2 <- read_csv("tl_2016_us_county/tl_2016_us_county/county2dma.csv")
summary(county2)
view(county2)


### Joining
gf_fips_county <- left_join(county2, gf_fips, by = "fips")
view(gf_fips_county)
summary(gf_fips_county)



### Generating new variables

##factor
gf_fips_county <- gf_fips_county %>% mutate(`new uid` = factor(`new uid`),
                                            start_date = factor(start_date),
                                            city = factor(city),
                                            state = factor(state),
                                            `top 100 city?` = factor(`top 100 city?`),
                                            address = factor(address),
                                            size = factor(size),
                                            internaluse_type_of_gathering = factor(internaluse_type_of_gathering),
                                            type_of_gathering = factor(type_of_gathering),
                                            police_altercation = factor(police_altercation),
                                            damage_inflicted = factor(damage_inflicted),
                                            curfew_established = factor(curfew_established),
                                            march = factor(march),
                                            escalation = factor(escalation),
                                            state_deployed_national_guard = factor(state_deployed_national_guard),
                                            `auto formula (don't touch)` = factor(`auto formula (don't touch)`),
                                            NAME.x = factor(NAME.x))
summary(gf_fips_county)


## a) whether a protest occurred in the county
gf_fips_county <- gf_fips_county %>% group_by(NAME.x) %>%  mutate(dummy_protco = ifelse(internaluse_type_of_gathering == "Both" | internaluse_type_of_gathering == "protest" | internaluse_type_of_gathering == "Protests" |internaluse_type_of_gathering == "Protests with Observable Violence", 1, 0))

## b) how many protests took place
gf_fips_county <- gf_fips_county %>% group_by(NAME.x) %>% mutate(number_prot1 = sum(dummy_protco, na.rm = T))

## c) these protests (and how many) escalated to violence
gf_fips_county <- gf_fips_county %>% group_by(NAME.x) %>%  mutate(dummy_protviol = ifelse(internaluse_type_of_gathering == "Protests with Observable Violence", 1, 0))
gf_fips_county <- gf_fips_county %>% group_by(NAME.x) %>% mutate(number_prot2 = sum(dummy_protviol, na.rm = T)) 


### Regressions
# Your dependent variable will be Trump vote share in the election.

## Joining data
vote <- readxl::read_xlsx("vote2020.xlsx")
summary(vote)
view(vote)
vote <- vote %>% mutate(fips = county_fips)
reg_data <- left_join(gf_fips_county, vote, by = "fips")
view(reg_data)


##Regression (1) should include whether a protest occurred (0/1) and whether the protest (if it occurred) escalated to violence (0/1).
reg1 <- glm(per_gop ~ dummy_protco + dummy_protviol, family = binomial(), data = reg_data)
summary(reg1) 


##Regression (2) repeat this approach but use protest counts per county
reg_data <- reg_data %>% group_by(NAME.x)
reg2 <- lm(per_gop ~ number_prot1 + number_prot2, data = reg_data)
summary(reg2)

##Table
stargazer(reg1, reg2, 
          covariate.labels = c("Protest occurred in the county", "Escalated to Violence", "No. Protests in the County", "No. Protests Escalated to Violence"),
          dep.var.caption="Trump Vote Share",
          title = "Table 1 Effects of Protests in Trump Vote Share",
          type="html", out="Table 1.html")




##### 4. Now consider a fixed effects regression similar to the ones above.
reg3 <- glm(per_gop ~ dummy_protco + dummy_protviol + factor(STATEFP.x), family = binomial(), data = reg_data)
summary(reg3)

reg4 <- lm(per_gop ~ number_prot1 + number_prot2 + factor(STATEFP.x), data = reg_data)
summary(reg4)

## Table
stargazer(reg3, reg4,
          covariate.labels = c("Protest occurred in the county", "Escalated to Violence", "No. Protests in the County", "No. Protests Escalated to Violence"),
          dep.var.caption="Trump Vote Share",
          title = "Table 2 Effects of Protests in Trump Vote Share With Fixed Effects",
          type = "html", out = "Table 2.html")

