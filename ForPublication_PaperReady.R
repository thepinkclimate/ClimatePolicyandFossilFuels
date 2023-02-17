rm(list=ls(all=TRUE)) # Removes all previously created variables
gc() # frees up memory resources

setwd("/Users/francesca/Dropbox/UCL/Rystad_data/")
library(tidyverse)
library(xlsx)
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(openxlsx)
library(Hmisc)
library(plotly)
library(formattable)
library(data.table)
library(igraph)
library(parallel)
library(checkmate)
library(psych)
library(scales)
library(readxl)
library(ggsci)
library(gridExtra)
library(plm)
library(plyr)
library(BayesVarSel)
library(panelr)
library(ggpubr)
library(stringi)

#Build functions:
# These two would serve to build density and diameter of brown network. For now, I do not use them.
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Focus on country level
#Upload investments, production, OPEX, Subsidies, Government Take, emission data, GDP p.c.
investments <- read.xlsx("investments.xlsx")
names(investments)[2] <- "invest"
investments <- investments %>% filter(Oil.And.Gas.Category == "Gas" | Oil.And.Gas.Category == "Oil")
investments <- investments %>% filter(Economics.Category != "Exploration Capex")

#Descriptive statistics: measure top investors across the whole sample
topinvestors <- aggregate(invest~ Country, data = investments, sum)

options(scipen=999)
production <- read.xlsx("production.xlsx")
names(production)[2] <- "prod"
production <- production %>% filter(Oil.And.Gas.Category == "Gas" | Oil.And.Gas.Category == "Oil")

oilprod <- production %>% filter(Oil.And.Gas.Category == "Oil")
names(oilprod)[2] <- "oilprod"
gasprod <- production %>% filter(Oil.And.Gas.Category == "Gas")
names(gasprod)[2] <- "gasprod"

opecosts <- read.xlsx("OperationalCosts.xlsx")
names(opecosts)[3] <- "opex"
opecosts <- opecosts %>% filter(Oil.And.Gas.Category == "Gas" | Oil.And.Gas.Category == "Oil")

govetake <- read.xlsx("GovernmentTake_byTaxType.xlsx")
names(govetake)[3] <- "govex"
govetake <- govetake %>% filter(Oil.And.Gas.Category == "Gas" | Oil.And.Gas.Category == "Oil")

tetobe <- read.xlsx("Yet-to-be-find-resources.xlsx")
names(tetobe)[1] <- "oilres"
tetobeoil <- tetobe %>% filter(Oil.And.Gas.Category == "Oil")
tetobegas <- tetobe %>% filter(Oil.And.Gas.Category == "Gas")
names(tetobegas)[1] <- "gasres"

investments <- aggregate(invest ~ Year + Country + Continent + `OPEC.NON-OPEC` + 
                           Region + Continent, data = investments, sum)
oilprod <- aggregate(oilprod ~ Year + Country, data = oilprod, sum)
gasprod <- aggregate(gasprod ~ Year + Country, data = gasprod, sum)

opecosts <- aggregate(opex ~ Year + Country, data = opecosts, sum)

govetake <-aggregate(govex ~ Year + Country, data = govetake, sum)
tetobeoil <- aggregate(oilres ~ Year + Country, data = tetobeoil, sum)
tetobegas <- aggregate(gasres ~ Year + Country, data = tetobegas, sum)

#Merge all datasets from Rystad: they do not require any name change
masterfile2 <- full_join(investments, gasprod, by = c("Year", "Country"))
masterfile <- full_join(masterfile2, oilprod, by = c("Year", "Country"))
masterfile <- full_join(masterfile, opecosts, by = c("Year", "Country"))
masterfile <- full_join(masterfile, govetake, by = c("Year", "Country"))
masterfile <- full_join(masterfile, tetobeoil, by = c("Year", "Country"))
masterfile <- full_join(masterfile, tetobegas, by = c("Year", "Country"))


#Include controls: macro economic variables from WB and IMF
#GDP per capita
dpPop <- read.csv("GDPpc.csv")
dpPop <- pivot_longer(dpPop, -c(Country.Name, Country.Code, Indicator.Name), 
                      values_to = "GDPpc", names_to = "Year")
dpPop$Year <- str_sub(dpPop$Year, 2)
dpPop$Year <- as.numeric(as.character(dpPop$Year))

names(dpPop)[1] <- "Country"
dpPop$Country <- gsub("Brunei Darussalam", "Brunei", dpPop$Country)
dpPop$Country <- gsub("Bahamas, The", "Bahamas", dpPop$Country)
dpPop$Country <- gsub("Congo, Rep.", "Congo", dpPop$Country)
dpPop$Country <- gsub("Congo, Dem. Rep.", "Democratic Republic of Congo", dpPop$Country)
dpPop$Country <- gsub("Egypt, Arab Rep.", "Egypt", dpPop$Country)
dpPop$Country <- gsub("Iran, Islamic Rep. of", "Iran", dpPop$Country)
dpPop$Country <- gsub("Kyrgyz Republic", "Kyrgyzstan", dpPop$Country)
dpPop$Country <- gsub("Russian Federation", "Russia", dpPop$Country)
dpPop$Country <- gsub("Slovak Rep.", "Slovakia", dpPop$Country)
dpPop$Country <- gsub("Syrian Arab Republic", "Syria", dpPop$Country)
dpPop$Country <- gsub("United Arab Emirates", "UAE", dpPop$Country)
dpPop$Country <- gsub("Venezuela, RB", "Venezuela", dpPop$Country)
dpPop$Country <- gsub("Yemen, Rep.", "Yemen", dpPop$Country)
dpPop$Country <- gsub("Korea, Dem. People's Rep.", "North Korea", dpPop$Country)
dpPop$Country <- gsub("Korea, Rep.", "South Korea", dpPop$Country)
dpPop$Country <- gsub("Slovakiablic", "Slovakia", dpPop$Country)
dpPop$Country <- gsub("Turkiye", "Turkey", dpPop$Country)
dpPop$Country <- gsub("Iran, Islamic Rep. of", "Iran", dpPop$Country)
dpPop$Country <- gsub("Lao PDR", "Laos", dpPop$Country)
dpPop$Country <- gsub("Gambia, The", "Gambia", dpPop$Country)
dpPop$Country <- gsub("Iran, Islamic Rep.", "Iran", dpPop$Country)

pPop <- dpPop[,-3]
masterfile <- left_join(masterfile, dpPop, by = c("Year", "Country"))

tradelowcarbs <- read.csv("Trade_in_Low_Carbon_Technology_Products.csv")
tradelowcarbs <- pivot_longer(tradelowcarbs, -c(ObjectId, Country, ISO2, ISO3, Indicator, Code, Unit, Source, Trade_Flow, Scale), values_to = "exp", names_to = "Year")
tradelowcarbs$Year <- str_sub(tradelowcarbs$Year, 2)
tradelowcarbs$Year <- as.numeric(as.character(tradelowcarbs$Year))
tradelowcarbs <- tradelowcarbs %>% filter(Year > 1999 | Year < 2021)
tradelowcarbs <- tradelowcarbs %>% filter(Indicator == "Exports of low carbon technology products as percent of GDP")
tradelowcarbs <- aggregate(exp ~ Year + Country, data = tradelowcarbs, sum)
names(tradelowcarbs)[3] <- "explow_percent"

tradelowcarbs$Country <- gsub("Afghanistan, Islamic Rep. of", "Afghanistan", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Azerbaijan, Rep. of", "Azerbaijan", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Bahrain, Kingdom of", "Bahrain", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Brunei Darussalam", "Brunei", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("China, P.R.: Mainland", "China", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Congo, Rep. of", "Congo", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Congo, Dem. Rep. of the", "Democratic Republic of Congo", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Croatia, Rep. of", "Croatia", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Czech Rep.", "Czech Republic", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Egypt, Arab Rep.", "Egypt", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Egypt of", "Egypt", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Estonia, Rep. of", "Estonia", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Ethiopia, The Federal Dem. Rep. of", "Ethiopia", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Iran, Islamic Rep. of", "Iran", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Kazakhstan, Rep. of", "Kazakhstan", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Kyrgyz Rep.", "Kyrgyzstan", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Madagascar, Rep. of", "Madagascar", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Mauritania, Islamic Rep. of", "Mauritania", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Mozambique, Rep. of", "Mozambique", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Netherlands, The", "Netherlands", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Poland, Rep. of", "Poland", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Russian Federation", "Russia", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Serbia, Rep. of", "Serbia", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Slovak Rep.", "Slovakia", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Slovenia, Rep. of", "Slovenia", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Syrian Arab Rep.", "Syria", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Tajikistan, Rep. of", "Tajikistan", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Tanzania, United Rep. of", "Tanzania", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Timor-Leste, Dem. Rep. of", "Timor-Leste", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("United Arab Emirates", "UAE", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Uzbekistan, Rep. of", "Uzbekistan", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Venezuela, Rep. Bolivariana de", "Venezuela", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Yemen, Rep. of", "Yemen", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Bahamas, The", "Bahamas", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Dominican Rep.", "Dominican Republic", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Central African Rep.", "Central African Republic", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Lao People's Dem. Rep.", "Laos", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Palau, Rep. of", "Palau", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Moldova, Rep. of", "Moldova", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("South Korea of", "South Korea", tradelowcarbs$Country)
tradelowcarbs$Country <- gsub("Armenia, Rep. of", "Armenia", tradelowcarbs$Country)

masterfile <- left_join(masterfile, tradelowcarbs, by = c("Year", "Country"))
climatepolicy <- read.xlsx("CCPI.xlsx")
climatepolicy <- climatepolicy%>% filter(Year > 2006 & Year < 2021)
climatepolicy$Country <- gsub("Korea", "South Korea", climatepolicy$Country)
masterfile <- left_join(climatepolicy, masterfile, by = c("Year", "Country"))
masterfile$colorcodes <- as.factor(ifelse(masterfile$Country == "United States", 'top',
                                          ifelse(masterfile$Country == "Russia", 'top',
                                                 ifelse(masterfile$Country == "Canada", 'top',
                                                        ifelse(masterfile$Country == "China", 'top',
                                                               ifelse(masterfile$Country == "Australia", 'top',
                                                                      ifelse(masterfile$Country == "Norway", 'top',
                                                                             ifelse(masterfile$Country == "Brazil", 'top',
                                                                                    ifelse(masterfile$Country == "Saudi Arabia", 'top',
                                                                                           ifelse(masterfile$Country == "United Kingdom", 'top',
                                                                                                  ifelse(masterfile$Country == "Nigeria", 'top', 'others')))))))))))




masterfile2 <- masterfile %>% filter(masterfile$Country == "Chile" |
                                       masterfile$Country == "Colombia" |
                                       masterfile$Country == "Cyprus" |
                                       masterfile$Country == "Finland" |
                                       masterfile$Country == "Latvia" |
                                       masterfile$Country == "Luxembourg" |
                                       masterfile$Country == "Philippines" |
                                       masterfile$Country == "Portugal" |
                                       masterfile$Country == "Singapore" |
                                       masterfile$Country == "Sweden" |
                                       masterfile$Country == "Switzerland" |
                                       masterfile$Country == "Vietnam"
)

masterfile <- anti_join(masterfile, masterfile2)

masterfile$oilprod[is.na(masterfile$oilprod)] <- 0
masterfile$gasprod[is.na(masterfile$gasprod)] <- 0
masterfile$opex[is.na(masterfile$opex)] <- 0
masterfile$oilres[is.na(masterfile$oilres)] <- 0
masterfile$gasres[is.na(masterfile$gasres)] <- 0
masterfile$explow_percent[is.na(masterfile$explow_percent)] <- 0
masterfile$invest[is.na(masterfile$invest)] <- 0
masterfile$govex[is.na(masterfile$govex)] <- 0

# Correlation matrix
## Correlation matrix
mycor <- masterfile  %>%
  dplyr::select(invest,oilprod, gasprod, 
                opex, govex, GDPpc, explow_percent, National, International)

#Write corstars function
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

corstars(mycor, method = "pearson", result = "none")

masterfile <- masterfile[, -(15:16)]
pdim(masterfile)$balanced #The dataset is balanced

#Transform into log
masterdata <- masterfile
masterdata$loginvest <- log(masterdata$invest + max(masterdata$invest) + 1)
masterdata$logoprod <- log(masterdata$oilprod + max(masterdata$oilprod) +1)
masterdata$loggasprod <- log(masterdata$gasprod + max(masterdata$gasprod) +1)
masterdata$logopex <- log(masterdata$opex + max(masterdata$opex) +1)
masterdata$logovex <- log(masterdata$govex + max(masterdata$govex) +1)
masterdata$logores <- log(masterdata$oilres + max(masterdata$oilres) +1)
masterdata$logasres <- log(masterdata$gasres + max(masterdata$gasres) +1)
masterdata$logigdp <- log(masterdata$GDPpc + max(masterdata$GDPpc) +1)

wbclass <- read.csv("CLASS.csv")
wbclass$Country <- gsub("Slovak Republic", "Slovakia", wbclass$Country)

masterdynamic <- masterdata[, c(1:7, 16, 18:25)]
masterdynamic <- left_join(masterdynamic, wbclass, by = "Country")
masterdynamic$IncomeLevel[is.na(masterdynamic$IncomeLevel)] <- "High income"
masterdynamic$GEO[is.na(masterdynamic$GEO)] <- "East Asia & Pacific"

library(pdynmc)
#Structure of panel dataset
data.info(masterdynamic, i.name = "Country", t.name = "Year")

#Define optimal number of lags
library(dynlm)
BIC <- function(model) {
  
  ssr <- sum(model$residuals^2)
  t <- length(model$residuals)
  npar <- length(model$coef)
  
  return(
    round(c("p" = npar - 1,
            "BIC" = log(ssr/t) + npar * log(t)/t,
            "R2" = summary(model)$r.squared), 4)
  )
}

BIC(dynlm(ts(masterdynamic$loginvest) ~ 1))
order <- 1:6
BICs <- sapply(order, function(x) 
  "AR" = BIC(dynlm(ts(masterdynamic$loginvest) ~ L(ts(masterdynamic$loginvest), 1:x))))
BICs

# select the AR model with the smallest BIC
BICs[, which.min(BICs[2, ])]

#I check with multiple variables
order <- 1:12
BICs <- sapply(order, function(x) 
  BIC(dynlm(ts(masterdynamic$loginvest) ~ L(ts(masterdynamic$logoprod), 1:x) + L(ts(masterdynamic$loggasprod), 1:x) +
              L(ts(masterdynamic$logopex), 1:x) + L(ts(masterdynamic$logovex), 1:x) + L(ts(masterdynamic$logigdp), 1:x) +
              L(ts(masterdynamic$explow_percent), 1:x))))
BICs
BICs[, which.min(BICs[2, ])] #According to the theory for ADL models with p = q
#it follows that p reports the number of estimated coefficients excluding the intercept. 
#Thus the lag order is obtained by dividing p by 2.


## Granger causality test for panel data
library(lmtest)
grangertest(ts(loginvest) ~ ts(National), order = 3, data = masterdynamic)
#Significant: this tests for reverse causation --> we check if X predicts Y controlling for the lags of Y
grangertest(ts(loginvest) ~ ts(International), order = 3, data = masterdynamic)

plot(ts(masterdynamic$loginvest))
plot(diff(ts(masterdynamic$loginvest))) # Better when differentiated
adf.test(ts(masterdynamic$loginvest)) # p-value is significant, we cannot use it.

# Test for NO ESPLORATION CAPEX (OPERATIONALCAPEX)
set.seed(1234)

#Corrected errors
m4 <- pdynmc(
  dat = masterdynamic, varname.i = "Country", varname.t = "Year", 
  use.mc.diff = TRUE, use.mc.lev = T, use.mc.nonlin = FALSE, 
  include.y = TRUE, varname.y = "loginvest", lagTerms.y = 3,
  fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = T,
  varname.reg.fur = c("International", "National", "logopex", "logovex", "logoprod", "loggasprod", "logigdp", "explow_percent"),
  lagTerms.reg.fur = c(3,3,3,3,3,3,3,3),
  include.dum = TRUE, dum.diff = T, dum.lev = T, varname.dum = "Year",
  w.mat = "iid.err", std.err = "corrected",
  estimation = "iterative", opt.meth = "none"
)
summary(m4) 
jtest.fct(m4) #Hansen test (J test)
mtest.fct(m4)
nobs(m4)

#UNADJUSTED
m5 <- pdynmc(
  dat = masterdynamic, varname.i = "Country", varname.t = "Year", 
  use.mc.diff = TRUE, use.mc.lev = T, use.mc.nonlin = FALSE, 
  include.y = TRUE, varname.y = "loginvest", lagTerms.y = 3,
  fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = T,
  varname.reg.fur = c("International", "National", "logopex", "logovex", "logoprod", "loggasprod", "logigdp", "explow_percent"),
  lagTerms.reg.fur = c(3,3,3,3,3,3,3,3),
  include.dum = TRUE, dum.diff = T, dum.lev = T, varname.dum = "Year",
  w.mat = "iid.err", std.err = "unadjusted",
  estimation = "iterative", opt.meth = "none"
)
summary(m5) 
jtest.fct(m5) #Hansen test (J test)
mtest.fct(m5)
nobs(m5)

#Run the code again but only for exploration capex

m6 <- pdynmc(
  dat = masterdynamic, varname.i = "Country", varname.t = "Year", 
  use.mc.diff = TRUE, use.mc.lev = T, use.mc.nonlin = FALSE, 
  include.y = TRUE, varname.y = "loginvest", lagTerms.y = 3,
  fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = T,
  varname.reg.fur = c("International", "National", "logopex", "logovex", "logoprod", "loggasprod", "logigdp", "explow_percent"),
  lagTerms.reg.fur = c(3,3,3,3,3,3,3,3),
  include.dum = TRUE, dum.diff = T, dum.lev = T, varname.dum = "Year",
  w.mat = "iid.err", std.err = "unadjusted",
  estimation = "iterative", opt.meth = "none"
)
summary(m6) 
jtest.fct(m6) #Hansen test (J test)
mtest.fct(m6)
nobs(m6)

#Corrected errors
m7 <- pdynmc(
  dat = masterdynamic, varname.i = "Country", varname.t = "Year", 
  use.mc.diff = TRUE, use.mc.lev = T, use.mc.nonlin = FALSE, 
  include.y = TRUE, varname.y = "loginvest", lagTerms.y = 3,
  fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = T,
  varname.reg.fur = c("International", "National", "logopex", "logovex", "logoprod", "loggasprod", "logigdp", "explow_percent"),
  lagTerms.reg.fur = c(3,3,3,3,3,3,3,3),
  include.dum = TRUE, dum.diff = T, dum.lev = T, varname.dum = "Year",
  w.mat = "iid.err", std.err = "corrected",
  estimation = "iterative", opt.meth = "none"
)
summary(m7) 
jtest.fct(m7) #Hansen test (J test)
mtest.fct(m7)
nobs(m7)

#Low income countries
lowincome <- masterdynamic %>% filter(masterdynamic$IncomeLevel== "Lower middle income")
m8 <- pdynmc(
  dat = lowincome, varname.i = "Country", varname.t = "Year", 
  use.mc.diff = TRUE, use.mc.lev = T, use.mc.nonlin = FALSE, 
  include.y = TRUE, varname.y = "loginvest", lagTerms.y = 3,
  fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = T,
  varname.reg.fur = c("International", "National", "logopex", "logovex", "logoprod", "loggasprod", "logigdp", "explow_percent"),
  lagTerms.reg.fur = c(3,3,3,3,3,3,3,3),
  include.dum = TRUE, dum.diff = T, dum.lev = T, varname.dum = "Year",
  w.mat = "iid.err", std.err = "corrected",
  estimation = "iterative", opt.meth = "none"
)
summary(m8) 
jtest.fct(m8) #Hansen test (J test)
mtest.fct(m8)
nobs(m8)

#Upper middle income countries
upperincome <- masterdynamic %>% filter(masterdynamic$IncomeLevel== "Upper middle income")
m9 <- pdynmc(
  dat = upperincome, varname.i = "Country", varname.t = "Year", 
  use.mc.diff = TRUE, use.mc.lev = T, use.mc.nonlin = FALSE, 
  include.y = TRUE, varname.y = "loginvest", lagTerms.y = 3,
  fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = T,
  varname.reg.fur = c("International", "National", "logopex", "logovex", "logoprod", "loggasprod", "logigdp", "explow_percent"),
  lagTerms.reg.fur = c(3,3,3,3,3,3,3,3),
  include.dum = TRUE, dum.diff = T, dum.lev = T, varname.dum = "Year",
  w.mat = "iid.err", std.err = "corrected",
  estimation = "iterative", opt.meth = "none"
)
summary(m9) 
jtest.fct(m9) #Hansen test (J test)
mtest.fct(m9)
nobs(m9)

#High income countries
highincome <- masterdynamic %>% filter(masterdynamic$IncomeLevel== "High income")
m10 <- pdynmc(
  dat = highincome, varname.i = "Country", varname.t = "Year", 
  use.mc.diff = TRUE, use.mc.lev = T, use.mc.nonlin = FALSE, 
  include.y = TRUE, varname.y = "loginvest", lagTerms.y = 3,
  fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = T,
  varname.reg.fur = c("International", "National", "logopex", "logovex", "logoprod", "loggasprod", "logigdp", "explow_percent"),
  lagTerms.reg.fur = c(3,3,3,3,3,3,3,3),
  include.dum = TRUE, dum.diff = T, dum.lev = T, varname.dum = "Year",
  w.mat = "iid.err", std.err = "unadjusted",
  estimation = "iterative", opt.meth = "none"
)
summary(m10) 
jtest.fct(m10) #Hansen test (J test)
mtest.fct(m10)
nobs(m10)

#Re run everything for the exploration capex
m11 <- pdynmc(
  dat = lowincome, varname.i = "Country", varname.t = "Year", 
  use.mc.diff = TRUE, use.mc.lev = T, use.mc.nonlin = FALSE, 
  include.y = TRUE, varname.y = "loginvest", lagTerms.y = 3,
  fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = T,
  varname.reg.fur = c("International", "National", "logopex", "logovex", "logoprod", "loggasprod", "logigdp", "explow_percent"),
  lagTerms.reg.fur = c(3,3,3,3,3,3,3,3),
  include.dum = TRUE, dum.diff = T, dum.lev = T, varname.dum = "Year",
  w.mat = "iid.err", std.err = "corrected",
  estimation = "iterative", opt.meth = "none"
)
summary(m11) 
jtest.fct(m11) #Hansen test (J test)
mtest.fct(m11)
nobs(m11)

m12 <- pdynmc(
  dat = upperincome, varname.i = "Country", varname.t = "Year", 
  use.mc.diff = TRUE, use.mc.lev = T, use.mc.nonlin = FALSE, 
  include.y = TRUE, varname.y = "loginvest", lagTerms.y = 3,
  fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = T,
  varname.reg.fur = c("International", "National", "logopex", "logovex", "logoprod", "loggasprod", "logigdp", "explow_percent"),
  lagTerms.reg.fur = c(3,3,3,3,3,3,3,3),
  include.dum = TRUE, dum.diff = T, dum.lev = T, varname.dum = "Year",
  w.mat = "iid.err", std.err = "corrected",
  estimation = "iterative", opt.meth = "none"
)
summary(m12) 
jtest.fct(m12) #Hansen test (J test)
mtest.fct(m12)
nobs(m12)

m13 <- pdynmc(
  dat = highincome, varname.i = "Country", varname.t = "Year", 
  use.mc.diff = TRUE, use.mc.lev = T, use.mc.nonlin = FALSE, 
  include.y = TRUE, varname.y = "loginvest", lagTerms.y = 3,
  fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = T,
  varname.reg.fur = c("International", "National", "logopex", "logovex", "logoprod", "loggasprod", "logigdp", "explow_percent"),
  lagTerms.reg.fur = c(3,3,3,3,3,3,3,3),
  include.dum = TRUE, dum.diff = T, dum.lev = T, varname.dum = "Year",
  w.mat = "iid.err", std.err = "dbl.corrected",
  estimation = "iterative", opt.meth = "none"
)
summary(m13) 
jtest.fct(m13) #Hansen test (J test)
mtest.fct(m13)
nobs(m13)


# FIGURES
# Representing coefficients
library(dplyr)
library(ggplot2)
library(stringr)
coef <- read.xlsx("ForFigure_GeneralAB.xlsx", sheet = "General")
data = coef %>% gather(key = "Time", value="value", -c(1,2)) 
# Set a number of 'empty bar' to add at the end of each group
empty_bar=2
nObsType=nlevels(as.factor(data$Time))
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$Type)*nObsType, ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group=rep(levels(data$Type), each=empty_bar*nObsType )
data=rbind(data, to_add)
data=data %>% arrange(Type, Variable)
data$id=rep( seq(1, nrow(data)/nObsType) , each=nObsType)
# Get the name and the y position of each label
label_data= data  %>% dplyr::group_by(id, Variable) %>% summarise(tot=sum(value))
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse(angle < -90, 1, 0)
#Remember that sometimes it does not work because you need to run detach(package:plyr)
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# prepare a data frame for base lines
base_data=data %>% 
  group_by(Type) %>% 
  summarise(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

angle <- 90 - 360 * (base_data$title-0.5)/number_of_bar  
base_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

# Make the plot
p = ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=Time), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-0.2,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(1,4), "cm") 
  ) +
  coord_polar() +
  # Add labels on top of each bar
  geom_text(data = label_data, aes(x=id, y=tot + 0.05, label = Variable, hjust=hjust, angle= angle,
                                ),  color="black", alpha=2.5, size=3, inherit.aes = FALSE ) +
  geom_segment(data=base_data, aes(x = start, y = -0.02, xend = end, yend = -0.02, colour = Type), 
               alpha=2, size=0.6 , inherit.aes = FALSE ) 

p
#If I remove the labels I can add them on power point

## Now I take care of income groups

coefgroup <- read.xlsx("ForFigure_GeneralAB.xlsx", sheet = "Sheet2")
#General plot
library(hrbrthemes)
ggplot(coefgroup,                         # Draw barplot with grouping & stacking
       aes(x = Variable,
           y = Coefficient,
           fill = Time)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  scale_fill_viridis(discrete=TRUE, name="") +
  theme_ipsum() +
  facet_grid(~ Type) +
  coord_flip()

## Low income groups (substitute for the others)
coeflow <- read.xlsx("ForFigure_GeneralAB.xlsx", sheet = "LowIncome")
coeflow <- pivot_longer(coeflow, -c(Variable, Type), 
                      values_to = "Coefficient", names_to = "Time")

ggplot(coeflow,                         # Draw barplot with grouping & stacking
       aes(x = Variable,
           y = Coefficient,
           fill = Time)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  scale_fill_viridis(discrete=TRUE, name="") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 13)) +
  facet_grid(~ Type) +
  coord_flip()

#If I remove the labels I can add them on power point

# To run the BGVAR part, I will need to identify who the top climate leaders are to understand how their shocks affect others.
# Let's run the climate policy figure

library(ggsci)
climatefigure <- masterdynamic
#ggplot(climatefigure[climatefigure$Country %in% c("United States", "Russia", "Canada", "China", "Australia",
#                                                  "Norway", "Brazil", "Saudi Arabia", "United Kingdom"),], 
#       aes(x = Year, y = International, 
#           group = Country, 
#           colour = Country)) +
#  geom_line() +
#  geom_point() +
#  scale_y_continuous(labels = comma) +
#  ylab("International CCPI") +
#  scale_color_npg() +
#  theme_bw() + theme(panel.border = element_blank())

library(tidyverse)
library(ggrepel)
library(ggtext)

library(showtext)
font_add_google("Lato")
showtext_auto()

highlights <- c("United States", "Russia", "Canada", "China", "Australia", "Norway", "Brazil", "Saudi Arabia", "United Kingdom")
n <- length(highlights)


# Theme definition
theme_set(theme_minimal(base_family = "Lato"))
theme_update(
  # Remove title for both x and y axes
  axis.title = element_blank(),
  # Axes labels are grey
  axis.text = element_text(color = "grey40"),
  # The size of the axes labels are different for x and y.
  axis.text.x = element_text(size = 10, margin = margin(t = 5)),
  axis.text.y = element_text(size = 9, margin = margin(r = 5)),
  # Also, the ticks have a very light grey color
  axis.ticks = element_line(color = "grey91", size = .5),
  # The length of the axis ticks is increased.
  axis.ticks.length.x = unit(1.3, "lines"),
  axis.ticks.length.y = unit(.7, "lines"),
  # Remove the grid lines that come with ggplot2 plots by default
  panel.grid = element_blank(),
  # Customize margin values (top, right, bottom, left)
  plot.margin = margin(20, 40, 20, 40),
  # Use a light grey color for the background of both the plot and the panel
  plot.background = element_rect(fill = "grey98", color = "grey98"),
  panel.background = element_rect(fill = "grey98", color = "grey98"),
  # Customize title appearence
  plot.title = element_text(
    color = "grey10", 
    size = 18, 
    face = "bold",
    margin = margin(t = 15)
  ),
  # Customize subtitle appearence
  plot.subtitle = element_markdown(
    color = "grey30", 
    size = 10,
    lineheight = 1,
    margin = margin(t = 15, b = 20)
  ),
  # Title and caption are going to be aligned
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "grey30", 
    size = 6,
    lineheight = 1.2, 
    hjust = 0,
    margin = margin(t = 40) # Large margin on the top of the caption.
  ),
  # Remove legend
  legend.position = "none"
)
#Create a group for color
climatefigure <- climatefigure %>% mutate(group=if_else(Country %in% highlights, Country, "other"),
                                                group = as.factor(group)
                                                ) %>% 
                                            mutate(
                                              group = fct_relevel(group, "other", after = Inf),
                                              name_lab = if_else(Year == 2020, Country, NA_character_)
                                            ) %>% 
                                            ungroup()

#I run this for both international and national climate policy
plt <- ggplot(
  # The ggplot object has associated the data for the highlighted countries
  climatefigure %>% filter(group != "other"), 
  aes(Year, National, group = Country)
) + 
  # Geometric annotations that play the role of grid lines
  geom_vline(
    xintercept = seq(2007, 2020, by = 1),
    color = "grey91", 
    size = .2
  ) +
  geom_segment(
    data = tibble(y = seq(1, 5, by = 1), x1 = 2007, x2 = 2020),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey91",
    size = .2
  ) +
  geom_segment(
    data = tibble(y = 2.5, x1 = 2007, x2 = 2020),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey60",
    size = .4
  ) +
  geom_vline(
    aes(xintercept = 2015), 
    color = "grey40",
    linetype = "dotted",
    size = .8
  ) +
  ## Lines for the non-highlighted countries
  geom_line(
    data = climatefigure %>% filter(group == "other"),
    color = "grey75",
    size = .4,
    alpha = .5
  ) +
  ## Lines for the highlighted countries.
  # It's important to put them after the grey lines
  # so the colored ones are on top
  geom_line(
    aes(color = group),
    size = .7
  )

# Create labels
plt <- plt + 
  geom_text_repel(
    aes(color = group, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 4,
    direction = "y",
    xlim = c(2021.8, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
  ## coordinate system + scales
  coord_cartesian(
    clip = "off",
    ylim = c(1, 5)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(2007, 2023.5), 
    breaks = seq(2007, 2020, by = 2)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(1, 5, by = 1),
    labels = glue::glue("{format(seq(1, 5, by = 1))}")
  )
plt 
# Add title and explanations
library(rcartocolor)
plt <- plt + 
  scale_color_manual(
    values = c(rcartocolor::carto_pal(n = n, name = "Bold")[1:n-1], "grey50")
  ) +
  labs(
    title = "National Climate Policy Index",
    #subtitle = " ",
    caption = "Visualization by Francesca Larosa  • Data by Germanwatch"
  )
plt


## Figure descriptive statistics for investments
investments <- read.xlsx("investments.xlsx")
names(investments)[2] <- "invest"
investments <- investments %>% filter(Oil.And.Gas.Category == "Gas" | Oil.And.Gas.Category == "Oil")
investments <- investments %>% filter(Economics.Category != "Exploration Capex")
#Repeat also for exploration capex
perYear <- aggregate(invest ~ Year + Continent, data = investments, sum)
ggplot(data = perYear, 
       aes(x = Year, y = invest, 
           group = Continent, 
           colour = Continent)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = comma) +
  ylab("Investments MUSD per continent") +
  scale_color_npg() +
  theme(
    legend.position = c(.05, .95),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6))
  

#Per continent
inv_Asia <- investments %>% filter(Continent == "Asia")
perY_Asia <- aggregate(invest ~ Year + Country, data = inv_Asia, sum)
perY_Asia10 <- perY_Asia %>% group_by(Year) %>% 
  slice_max(order_by = invest, n = 10)
ggplot(data = perY_Asia10, 
       aes(x = Year, y = invest, 
           group = Country, 
           colour = Country)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = comma) +
  ylab("Asia: investments mUSD") +
  scale_color_d3("category20") +
  theme(
    legend.position = c(.02, .99),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(3, 3, 3, 3),
    legend.title = element_blank()
  )

inv_Africa <- investments %>% filter(Continent == "Africa")
perY_Africa <- aggregate(invest ~ Year + Country, data = inv_Africa, sum)
perY_Africa10 <- perY_Africa %>% group_by(Year) %>% 
  slice_max(order_by = invest, n = 10)

ggplot(data = perY_Africa10, 
       aes(x = Year, y = invest, 
           group = Country, 
           colour = Country)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = comma) +
  ylab("Africa: investments mUSD") +
  scale_color_d3("category20") +
  theme(
    legend.position = c(.02, .99),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(3, 3, 3, 3),
    legend.title = element_blank()
  )

inv_SAM <- investments %>% filter(Continent == "America S")
perY_SAM <- aggregate(invest ~ Year + Country, data = inv_SAM, sum)
perY_SAM10 <- perY_SAM %>% group_by(Year) %>% 
  slice_max(order_by = invest, n = 5)
ggplot(data = perY_SAM10, 
       aes(x = Year, y = invest, 
           group = Country, 
           colour = Country)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = comma) +
  ylab("South America: investments mUSD") +
  scale_color_d3("category20") +
  theme(
    legend.position = c(.02, .99),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(3, 3, 3, 3),
    legend.title = element_blank()
  )

inv_EU <- investments %>% filter(Continent == "Europe")
perY_EU <- aggregate(invest ~ Year + Country, data = inv_EU, sum)
perY_EU10 <- perY_EU %>% group_by(Year) %>% 
  slice_max(order_by = invest, n = 7)
ggplot(data = perY_EU10, 
       aes(x = Year, y = invest, 
           group = Country, 
           colour = Country)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = comma) +
  ylab("Europe: investments mUSD") +
  scale_color_d3("category20") +
  theme(
    legend.position = c(.02, .99),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(3, 3, 3, 3),
    legend.title = element_blank()
  )

#Per country with top investors
topinvestors <- aggregate(invest ~ Year + Country, data = investments, sum)
topinvestors <- topinvestors %>% group_by(Year) %>% 
  slice_max(order_by = invest, n = 10)

ggplot(data = topinvestors, 
       aes(x = Year, y = invest, 
           group = Country, 
           colour = Country)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = comma) +
  ylab("Europe: investments mUSD") +
  scale_color_d3("category20") +
  theme(
    legend.position = c(.02, .99),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(3, 3, 3, 3),
    legend.title = element_blank()
  )


#### Figure green weighted cumulative diameter
#Start from line 455 of "Paper_Code_FINAL.R" to compute measure
greenfigure <- cumsumdiam
highlights_green <- c("Argentina", "United States", "Russia", "Canada", "China", 
                      "Australia", "UAE", "Saudi Arabia", "United States")
n <- length(highlights_green)

theme_set(theme_minimal(base_family = "Lato"))
theme_update(
  # Remove title for both x and y axes
  #axis.title = element_blank(),
  # Axes labels are grey
  axis.text = element_text(color = "grey40"),
  # The size of the axes labels are different for x and y.
  axis.text.x = element_text(size = 10, margin = margin(t = 5)),
  axis.text.y = element_text(size = 9, margin = margin(r = 5)),
  # Also, the ticks have a very light grey color
  axis.ticks = element_line(color = "grey91", size = .5),
  # The length of the axis ticks is increased.
  axis.ticks.length.x = unit(1.3, "lines"),
  axis.ticks.length.y = unit(.7, "lines"),
  # Remove the grid lines that come with ggplot2 plots by default
  panel.grid = element_blank(),
  # Customize margin values (top, right, bottom, left)
  plot.margin = margin(20, 40, 20, 40),
  # Use a light grey color for the background of both the plot and the panel
  plot.background = element_rect(fill = "grey98", color = "grey98"),
  panel.background = element_rect(fill = "grey98", color = "grey98"),
  # Customize title appearence
  plot.title = element_text(
    color = "grey10", 
    size = 18, 
    face = "bold",
    margin = margin(t = 5)
  ),
  # Customize subtitle appearence
  plot.subtitle = element_markdown(
    color = "grey30", 
    size = 10,
    lineheight = 1,
    margin = margin(t = 15, b = 20)
  ),
  # Title and caption are going to be aligned
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "grey30", 
    size = 6,
    lineheight = 1.2, 
    hjust = 0,
    margin = margin(t = 40) # Large margin on the top of the caption.
  ),
  # Remove legend
  legend.position = "none"
)
#Create a group for color
greenfigure <- greenfigure %>% mutate(group=if_else(Country %in% highlights_green, Country, "other"),
                                          group = as.factor(group)
) %>% 
  mutate(
    group = fct_relevel(group, "other", after = Inf),
    name_lab = if_else(Year == 2019, Country, NA_character_)
  ) %>% 
  ungroup()

plt <- ggplot(
  # The ggplot object has associated the data for the highlighted countries
  greenfigure %>% filter(group != "other"), 
  aes(Year, cumdiam, group = Country)
) + 
  # Geometric annotations that play the role of grid lines
  geom_vline(
    xintercept = seq(2000, 2020, by = 2),
    color = "grey91", 
    size = .2
  ) +
  geom_segment(
    data = tibble(y = seq(0, 150, by = 1), x1 = 2000, x2 = 2020),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey91",
    size = .2
  ) +
  geom_segment(
    data = tibble(y = 50, x1 = 2000, x2 = 2020),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey60",
    size = .4
  ) +
  geom_vline(
    aes(xintercept = 2015), 
    color = "grey40",
    linetype = "dotted",
    size = .8
  ) +
  ## Lines for the non-highlighted countries
  geom_line(
    data = greenfigure %>% filter(group == "other"),
    color = "grey75",
    size = .4,
    alpha = .5
  ) +
  ## Lines for the highlighted countries.
  # It's important to put them after the grey lines
  # so the colored ones are on top
  geom_line(
    aes(color = group),
    size = .7
  )

# Create labels
plt <- plt + 
  geom_text_repel(
    aes(color = group, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 4,
    direction = "y",
    xlim = c(2020.8, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
  ## coordinate system + scales
  coord_cartesian(
    clip = "off",
    ylim = c(0, 150)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(2000, 2023.5), 
    breaks = seq(2000, 2020, by = 5)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 150, by = 50),
    labels = glue::glue("{format(seq(0, 150, by = 50))}")
  )
plt 
# Add title and explanations
library(rcartocolor)
plt <- plt + 
  scale_color_manual(
    values = c(rcartocolor::carto_pal(n = n, name = "Bold")[1:n-1], "grey50")
  ) +
  labs(
    #title = "Weighted cumulative diameter",
    #subtitle = " ",
    caption = "Visualization by Francesca Larosa  • Data by BNEF"
  )
plt


