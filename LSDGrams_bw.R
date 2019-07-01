#### Libraries ####

library(lmtest)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(scales)
library(data.table)
library(stringr)

#### Data compilation ####
## Download BOTH Grams folders into one folder. Do not include the May 29th 2014 folder (not in format).

setwd("C:/Users/jacobmiller21/Desktop/Darknet Data")
DIR <- "C:/Users/jacobmiller21/Desktop/Darknet Data/GramsAll"
gramsFiles <- list.files(path = DIR, all.files=TRUE, full.names = TRUE, recursive = TRUE)
grams <- data.frame()

## Warning. Very long run-time

for (i in 1:length(gramsFiles)) {
  log <- read.csv(gramsFiles[i], header=TRUE)
  filespath <- unlist(strsplit(gramsFiles[i], split = "/"))
  log$Date <- as.Date(filespath[length(filespath)-1])
  log$LowerName <- tolower(log$name)
  log$LowerDesc <- tolower(log$description)
  log$LSDName <- grepl("lsd", log$LowerName)
  log <- log[log$LSDName == TRUE,]
  grams <- rbind(grams,log)
}

# Generated X observations for 15 variables in first attempt
## Intermediate storage: write.csv(grams, "CompiledGramsLSD.csv")

#### Data Cleaning ####
## Load intermediate file if above step has been performed and written to csv
setwd("C:/Users/jacobmiller21/Desktop/Darknet Data")
grams <- read.csv(file = "CompiledGramsLSD.csv", stringsAsFactors = FALSE)
## Clean out listings where price was not pulled. Mostly Agora from early on.
grams <- grams[grams$price != 0,]


## Add in the historical table of bitcoin prices.
## I manually pulled historical btc prices from 
## https://www.investing.com/currencies/btc-usd-historical-data
## I could not find a convenient api for btc prices.
## A comparison between other btc price charts showed
## minimal differences in prices. Accessed July 28, 2017.
## Convert bitcoin prices to USD prices.

btc <- read.csv("BTC Historical.csv")
btc$Date <- as.Date(btc$Date, format = "%m/%d/%Y")
grams$Date <- as.Date(grams$Date)
grams <- merge(grams, btc, by = "Date")
rm(btc)

## Clean out "X" columns
grams <- grams[, !names(grams) %in% c("X", "X.1")]

## Convert BTC price of good to USD price
grams$price <- gsub(",","",grams$price)
grams$price <- as.numeric(grams$price)
grams$dollarPrice <- grams$price * grams$btcPrice

## Clean out listings with text as prices (not numbers)
grams <- grams[complete.cases(grams[,"dollarPrice"]),]
length(unique(grams$vendor_name))



colnames(grams)[names(grams) == "LowerName"] <- "lower"
grams$nospace <- gsub(" ","", grams$lower, fixed = TRUE)

## Identify the different lsd doses and quantities
dosepatterns <- paste(c("[[:digit:]]+mic","[[:digit:]]+ug",
                        "[[:digit:]]+g", "[[:digit:]]+mg",
                        "[[:digit:]]+mcg"),collapse="|")

grams$Dose <- NA
grams$Dose <- grepl(pattern = dosepatterns, x = grams$nospace)
grams[grams$Dose,"Dose"] <- regmatches(grams$nospace,regexpr(dosepatterns,grams$nospace))
grams[grams$Dose != FALSE, "Dose"] <- gsub("[[:alpha:]]+", "",
                                           grams[grams$Dose != FALSE, "Dose"])
grams[grams$Dose != FALSE, "Dose"] <- str_sub(grams[grams$Dose != FALSE, "Dose"],start = -3)

quantpatterns1 <- paste(c("x[[:digit:]]+",
                          "x[[:space:]][[:digit:]]+"),collapse="|")
quantpatterns2 <- paste(c("[[:digit:]]+tabs",
                         "[[:digit:]]+x",
                         "[[:digit:]]+strip",
                         "[[:digit:]]+pack",
                         "[[:digit:]]+drop",
                         "[[:digit:]]+lsd",
                         "[[:digit:]]+blotter",
                         "[[:digit:]]+hit"),collapse="|")
grams$Quant <- NA
grams$Quant <- grepl(pattern = quantpatterns2, x = grams$nospace)
grams[grams$Quant,"Quant"] <- regmatches(grams$nospace,regexpr(quantpatterns2,grams$nospace))
sub1 <- grams$Quant == FALSE

grams[sub1,"Quant"] <- grepl(pattern = quantpatterns1, x = grams[sub1,"lower"])
grams[grams$Quant == TRUE,"Quant"] <- regmatches(grams[sub1,"lower"],regexpr(quantpatterns1,grams[sub1,"lower"]))
grams[grams$Quant != FALSE, "Quant"] <- gsub("[[:alpha:]]+", "",
                                           grams[grams$Quant != FALSE, "Quant"])
grams[,"Quant"] <- str_replace_all(grams[,"Quant"], fixed(" "), "")

sub1 <- grams$Quant == FALSE
quantpatterns3 <- paste(c("[[:digit:]]+[[:space:]]+[[:digit:]]+"))
grams[sub1,"Quant"] <- grepl(pattern = quantpatterns3, x = grams[sub1,"lower"])
grams[grams$Quant == TRUE,"Quant"] <- regmatches(grams[sub1,"lower"],regexpr(quantpatterns3,grams[sub1,"lower"]))
grams[,"Quant"] <-  sub(" .*", "", grams[,"Quant"])


## Include method to adjust for start with number to indicate quantity
sub1 <- grams$Quant == FALSE
quantpatterns4 <- "^[[:digit:]]+"
quantpatterns5 <- paste(c("^[[:digit:]]+mcg",
                          "^[[:digit:]]+g",
                          "^[[:digit:]]+ug",
                          "^[[:digit:]]+mic",
                          "^[[:digit:]]+mg"), collapse = "|")
grams[sub1,"Quant"] <- grepl(pattern = quantpatterns4, x = grams[sub1,"nospace"]) & 
  !grepl(pattern = quantpatterns5, x = grams[sub1,"nospace"])

grams[grams$Quant == TRUE,"Quant"] <- regmatches(grams[grams$Quant == TRUE,"nospace"],
                                                 regexpr(quantpatterns4,grams[grams$Quant == TRUE,"nospace"]))


rm(dosepatterns, quantpatterns1, quantpatterns2, 
   quantpatterns3, quantpatterns4, quantpatterns5, sub1)

## Clean up the ship from locations
grams[grepl("usa", tolower(grams$ship_from)),"ship_from"] <- "United States"
grams[grepl("united states", tolower(grams$ship_from)),"ship_from"] <- "United States"
grams[grepl("world", tolower(grams$ship_from)),"ship_from"] <- "Undeclared"
grams[grepl("united kingdom", tolower(grams$ship_from)),"ship_from"] <- "UK"
grams[grepl("china", tolower(grams$ship_from)),"ship_from"] <- "China"
grams[grepl("canada", tolower(grams$ship_from)),"ship_from"] <- "Canada"
grams[grepl("europe", tolower(grams$ship_from)),"ship_from"] <- "EU"
grams[grepl("cze", tolower(grams$ship_from)),"ship_from"] <- "Czech Republic"
grams[grepl("spain", tolower(grams$ship_from)),"ship_from"] <- "Spain"
grams[grepl("specified", tolower(grams$ship_from)),"ship_from"] <- "Undeclared"
grams[grepl("germany", tolower(grams$ship_from)),"ship_from"] <- "Germany"
grams[grepl("deu", tolower(grams$ship_from)),"ship_from"] <- "Germany"
grams[grepl("australia", tolower(grams$ship_from)),"ship_from"] <- "Australia"
grams[grepl("denmark", tolower(grams$ship_from)),"ship_from"] <- "Denmark"
grams[grepl("france", tolower(grams$ship_from)),"ship_from"] <- "France"
grams[grepl("japan", tolower(grams$ship_from)),"ship_from"] <- "Japan"
grams[grams$ship_from == "us","ship_from"] <- "United States"
grams[grepl("interwebs", tolower(grams$ship_from)),"ship_from"] <- "Undeclared"
grams[grepl("xxx", tolower(grams$ship_from)),"ship_from"] <- "Undeclared"
grams[grepl("unknown", tolower(grams$ship_from)),"ship_from"] <- "Undeclared"
grams[grepl("hungary", tolower(grams$ship_from)),"ship_from"] <- "Hungary"
grams[grepl("viet", tolower(grams$ship_from)),"ship_from"] <- "Vietnam"


## Depending on the frequency of the crawler, if a market produces
## identical results in terms of bitcoin, the crawler pulling the results
## did not successfully produce the new results and merely reported the
## old bitcoin price. Because bitcoin prices vary every day, we can
## assume that the crawler didn't update if bitcoin prices for a certain
## product from a certain vendor on a certain market are identical.

cleangrams <- grams
cleangrams <- cleangrams[!duplicated(cleangrams[,c("vendor_name", "market_name", "price", "name")]),]
rm(grams)
## Include only most popular amounts

## Screen out listings with issues or where exact quantity is unknown

df <- cleangrams[cleangrams$Quant != FALSE,]
df[,"Quant"] <- as.numeric(df[,"Quant"])
df <- df[df$Quant != 0,]
df$Cost_per_qty <- df$dollarPrice / df$Quant
df$Cost_per_qty <- round(df$Cost_per_qty, digits = 3)

# Cost per tab below 75 cents or above $25 per tab were almost
# exclusively listings that were either unclear, fat finger errors,
# or structured in such a way that the listing was difficult to understand.
df <- df[df$Cost_per_qty > 0.75,]
df <- df[df$Cost_per_qty < 25,]


## Add two events of interest

## Break up each listing into 10 day increments
df$TrendOnym <- as.numeric(df$Date) - as.numeric(as.Date("2014-11-05"))
df$TrendOnym <- round_any(df$TrendOnym, 10, f = ceiling)
df$TrendChin <- as.numeric(df$Date) - as.numeric(as.Date("2015-10-01"))
df$TrendChin <- round_any(df$TrendChin, 10, f = ceiling)
## Add two events of interest
df$Onymous <- df$Date >= as.Date("2014-11-06")
df$ChinaBan <- df$Date >= as.Date("2015-10-01")

## Only include most popular  amounts 
# and remove the "joint sales with ecstasy"
df1 <- df[df$Quant == 5 | df$Quant == 10 | df$Quant == 25 | df$Quant == 50 | 
            df$Quant == 100 | df$Quant == 250 | df$Quant == 500,]
df1 <- df1[!grepl(pattern = "mdma",x = df1$lower),]
# Treat quantities as factors
df1$QuantF <- as.factor(df1$Quant)
df1$DateNum <- as.numeric(df1$Date - as.Date("2014-06-09"))
#write.csv(df1, "LSD_Cleaned.csv")



#### Onymous Analysis ####
# DateNum variable is days since 2014-06-09
# Onymous took place on 2014-11-06 day 150
# China ban took place on 2015-01-10 day 479

## Remove listings that appear more than once during the same 10 day period
DataOnym <- df1[!duplicated(df1[,c("vendor_name", "market_name", "name", "TrendOnym")]),]
DataOnym <- DataOnym[,c("dollarPrice", "DateNum","QuantF","Onymous", "TrendOnym")]
colnames(DataOnym)[3] <- "Amt"
# Set Trend Variables and capture interested period (+/- 120 days)
# (add one to window for bracketing within Stata to complete full 120 days)
# Allows easy adjustment of window and randomization for EventDate
# for robustness checks
window <- 121
EventDate <- 150
# Shift to day before because of bracketing in Stata for binscatter
DataOnym$Trend <- DataOnym$TrendOnym - 1
DataOnym$OnymousTrend <- DataOnym$Trend * DataOnym$Onymous
DataOnym <- DataOnym[c("dollarPrice", "Trend", "Onymous", 
                       "OnymousTrend", "Amt")]
DataOnym <- DataOnym[(DataOnym$Trend >= (0 - window)) &
                       (DataOnym$Trend <= (0 + window)),]
# Convert to log form for dollar price
DataOnym$dollarPrice <- log(DataOnym$dollarPrice)
colnames(DataOnym)[1] <- "LogDollarPrice"
# Convert for stata
DataOnym$Onymous <- ifelse(DataOnym$Onymous, 1, 0)

#DataOnym <- read.csv("DataOnymousLSD.csv")
#DataOnym$Amt <- as.factor(DataOnym$Amt)
Onym1 <- lm(LogDollarPrice ~ Trend + Onymous + OnymousTrend + Amt,
            data = DataOnym)
summary(Onym1)
#write.csv(DataOnym, "DataOnymousLSD.csv", row.names = F)



#### China Ban Analysis ####
# DateNum variable is days since 2014-06-09
# Onymous took place on 2014-11-06 day 150
# China ban took place on 2015-01-10 day 479


## Remove listings that appear more than once during the same 10 day period
DataChina <- df1[!duplicated(df1[,c("vendor_name", "market_name", "name", "TrendChin")]),]
DataChina <- DataChina[,c("dollarPrice", "DateNum","QuantF","ChinaBan", "TrendChin")]
colnames(DataChina)[3] <- "Amt"
# Set Trend Variables and capture interested period (+/- 120 days)
# (add one to window for bracketing within Stata to complete full 120 days)
# Allows easy adjustment of window and randomization for EventDate
# for robustness checks
window <- 121
EventDate <- 479

DataChina$Trend <- DataChina$TrendChin - 1
DataChina$ChinaTrend <- DataChina$Trend * DataChina$ChinaBan
DataChina <- DataChina[c("dollarPrice","Trend","ChinaBan",
                         "ChinaTrend","Amt")]

DataChina <- DataChina[(DataChina$Trend >= (0 - window)) &
                         (DataChina$Trend <= (0 + window)),]
# Convert to log form for dollar price
DataChina$dollarPrice <- log(DataChina$dollarPrice)
colnames(DataChina)[1] <- "LogDollarPrice"
# Convert boolean to numeric for stata
DataChina$ChinaBan <- ifelse(DataChina$ChinaBan, 1, 0)

#DataChina <- read.csv("DataChinaLSD.csv")
China1 <- lm(LogDollarPrice ~ Trend + ChinaBan + ChinaTrend + Amt,
             data = DataChina)
summary(China1)

#write.csv(DataChina, "DataChinaLSD.csv", row.names = F)









#### Dataframes for graphs ####
mean(df1$Cost_per_qty)

ts <- df1 %>% group_by_(.dots=c("YrMonthBi", "QuantF")) %>% 
  summarise(mean=mean(Cost_per_qty), median=median(Cost_per_qty),
            q25th=quantile(Cost_per_qty, 0.25),
            q75th=quantile(Cost_per_qty, 0.75),
            max=max(Cost_per_qty),min=min(Cost_per_qty),
            vendors=length(unique(vendor_name)))


ts$Onymous <- ts$YrMonthBi >= as.Date("2014-11-06")
ts$AcetylButyrChinaBan <- ts$YrMonthBi >= as.Date("2015-10-01")



#### Exploratory Graphs ####

## Total Vendors for Popular Amounts

tvendors <- df1 %>% group_by_(.dots=c("YrMonthBi", "QuantF")) %>% 
  summarise(vendors=length(unique(vendor_name)))

## Arrange order of analogues for legend

ggplot(tvendors[tvendors$QuantF == 50 | tvendors$QuantF == 100 |
                  tvendors$QuantF == 25 | tvendors$QuantF == 250 |
                  tvendors$QuantF == 500,], 
       aes(x=YrMonthBi, y=vendors, linetype = QuantF)) +
  geom_line(size = 1) +
  ggtitle("Unique Vendors for LSD
          Event1 = Onymous, Event2 = Fent China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)
ggsave("LSD-Unique Vendors1.jpeg", device = "jpeg")


## 100mg
ggplot(ts[ts$QuantF=="25" | ts$QuantF == 50 | ts$QuantF == 100 |
            ts$QuantF == 250 | ts$QuantF == 500,], 
       aes(x=YrMonthBi, y=median, linetype = QuantF)) +
  geom_line(size = 1) +
  ggtitle("Median Price per LSD Tab
  Event1 = Operation Onymous, Event2 = China Fentanyl Ban") +
  xlab("Date") + ylab("Median Price") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)

ggsave("LSD-Cost Per Tab Plot.jpeg", device = "jpeg")


ts <- ts[ts$QuantF == 50 | ts$QuantF == 100 |
     ts$QuantF == 25 | ts$QuantF == 250 |
     ts$QuantF == 500,]
ts <- data.frame(ts)
## Composite Prices
for (n in 1:nrow(ts)) {
  tempAmt <- ts[[n,"QuantF"]]
  ts[n,"Composite"] <- (ts[n,"median"] - mean(ts[ts$QuantF == tempAmt,"median"])) /
    sd(ts[ts$QuantF == tempAmt,"median"])
}

ggplot(ts, aes(x=YrMonthBi, y=Composite, linetype = QuantF)) +
  geom_line(size = 1) +
  ggtitle("Composite Price of LSD by Amt Purchased
          Event1 = Onymous, Event2 = China Ban") +
  xlab("Date") + ylab("Standard Deviations") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)
ggsave("LSD-Normalized Price Plot.jpeg", device = "jpeg")



#### Statistical Analysis ####


df1$TrendOnym <- as.numeric(df1$Date) - as.numeric(as.Date("2014-11-05"))
df1$TrendOnym <- round_any(df1$TrendOnym, 10, f = ceiling)
df1$TrendChin <- as.numeric(df1$Date) - as.numeric(as.Date("2015-09-30"))
df1$TrendChin <- round_any(df1$TrendChin, 10, f = ceiling)
df1$Onymous <- df1$Date >= as.Date("2014-11-06")
df1$PostChinaBan <- df1$Date >= as.Date("2015-10-01")


#### Onymous Model ####
dfOnym <- df1[!duplicated(df1[,c("vendor_name", "market_name", "name", "TrendOnym")]),]


dfOnym <- dfOnym %>% group_by_(.dots=c("TrendOnym", "QuantF")) %>% 
  summarise(mean=mean(Cost_per_qty), Cost_per_qty=median(Cost_per_qty),
            q25th=quantile(Cost_per_qty, 0.25),
            q75th=quantile(Cost_per_qty, 0.75),
            max=max(Cost_per_qty),min=min(Cost_per_qty),
            Vendors=length(unique(vendor_name)))
dfOnym$Onymous <- dfOnym$TrendOnym >= 10
dfOnym$DateNum <- as.numeric(dfOnym$TrendOnym + 150)

dfOnym <- dfOnym[,c("Cost_per_qty", "DateNum","QuantF","Onymous","Vendors")]
dfOnym <- dfOnym[dfOnym$DateNum > 80 &  dfOnym$DateNum < 220,]
dfOnym <- as.data.frame(dfOnym)

for (n in 1:nrow(dfOnym)) {
  tempAmt <- dfOnym[[n,"QuantF"]]
  dfOnym[n,"Normalized"] <- (dfOnym[n,"Cost_per_qty"] - 
                               mean(dfOnym[dfOnym$QuantF == tempAmt,"Cost_per_qty"])) /
    sd(dfOnym[dfOnym$QuantF == tempAmt,"Cost_per_qty"])
}
rm(n,tempAmt)




dfOnym$OnymTrend <- (dfOnym$DateNum - 150) * dfOnym$Onymous
dfOnym <- dfOnym %>%
  select(Normalized, Onymous, DateNum, OnymTrend, Vendors) %>%
  rename(Trend = DateNum)

modelOnym <- lm(Normalized ~ Onymous + Trend + OnymTrend, data = dfOnym, weights = Vendors)
summary(modelOnym)
predOnym <- dfOnym[,c("Onymous","Trend","OnymTrend","Vendors")]
predOnym$pred <- predict(modelOnym, dfOnym[,c("Onymous","Trend","OnymTrend","Vendors")])

ggplot(dfOnym, aes(x=(Trend-155), y=Normalized)) +
  geom_point() +
  ggtitle("LSD: Normalized and Weighted Price on Darknet Markets
Event = Operating Onymous Market Shutdown") +
  xlab("Days") + ylab("Standard Deviations") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=0,color = "black", lwd = 2, linetype = "longdash") +
  geom_line(data=predOnym[predOnym$OnymTrend == 0,], aes(x=Trend-155, y=pred), lwd = 2) +
  geom_line(data=predOnym[predOnym$OnymTrend > 0,], aes(x=Trend-155, y=pred), lwd = 2)

ggsave("BW-LSD-Onymous.jpeg", device = "jpeg")


#### China Ban ####


dfChina <- df1[!duplicated(df1[,c("vendor_name", "market_name", "name", "TrendChin")]),]

dfChina <- dfChina %>% group_by_(.dots=c("TrendChin", "QuantF")) %>% 
  summarise(mean=mean(Cost_per_qty), Cost_per_qty=median(Cost_per_qty),
            q25th=quantile(Cost_per_qty, 0.25),
            q75th=quantile(Cost_per_qty, 0.75),
            max=max(Cost_per_qty),min=min(Cost_per_qty),
            Vendors=length(unique(vendor_name)))
dfChina$ChinaBan <- dfChina$TrendChin >= 10
dfChina$DateNum <- as.numeric(dfChina$TrendChin + 479)

dfChina <- dfChina[,c("Cost_per_qty", "DateNum","QuantF","ChinaBan","Vendors")]
dfChina <- dfChina[dfChina$DateNum > (479-180) &  dfChina$DateNum < (479+180),]
dfChina <- as.data.frame(dfChina)


for (n in 1:nrow(dfChina)) {
  tempAmt <- dfChina[[n,"QuantF"]]
  dfChina[n,"Normalized"] <- (dfChina[n,"Cost_per_qty"] - 
                               mean(dfChina[dfChina$QuantF == tempAmt,"Cost_per_qty"])) /
    sd(dfChina[dfChina$QuantF == tempAmt,"Cost_per_qty"])
}
rm(n,tempAmt)


dfChina$ChinTrend <- (dfChina$DateNum - 479) * dfChina$ChinaBan
dfChina <- dfChina %>%
  select(Normalized, ChinaBan, DateNum, ChinTrend, Vendors) %>%
  rename(Trend = DateNum)

modelChin <- lm(Normalized ~ ChinaBan + Trend + ChinTrend, data = dfChina, weights = Vendors)
summary(modelChin)
pred <- dfChina[,c("ChinaBan","Trend","ChinTrend","Vendors")]
pred$pred <- predict(modelChin, dfChina[,c("ChinaBan","Trend","ChinTrend","Vendors")])

ggplot(dfChina, aes(x = Trend-484, y = Normalized)) +
  geom_point() +
  ggtitle("LSD: Normalized and Weighted Price on Darknet Markets
Event = China Fentanyl Ban") +
  xlab("Days") + ylab("Standard Deviations") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=0,color = "black", lwd = 2, linetype = "longdash") +
  geom_line(data=pred[pred$ChinTrend == 0,], aes(x=Trend-484, y=pred), lwd = 2) +
  geom_line(data=pred[pred$ChinTrend > 0,], aes(x=Trend-484, y=pred), lwd = 2)
ggsave("BW-LSD-ChinaBan.jpeg", device = "jpeg")

#### Save results for Outreg for Fentanyl Comparison ####
## Must run InterruptedTwoEvents.R first
save(modelChin, file = "ChinaLSDModel.RData")
save(dfChina, file = "ChinaLSDData.RData")
save(modelOnym, file = "OnymLSDModel.RData")
save(dfOnym, file = "OnymLSDData.RData")

load("ChinaLSDModel.RData")
LSDmodelChin <- modelChin
rm(modelChin)
load("OnymLSDModel.RData")
LSDmodelOnym <- modelOnym
rm(modelOnym)
load("ChinaModel.RData")
FentModelChina <- modelChina
rm(modelChina)
load("OnymModel.RData")
FentModelOnymous <- modelOnym
rm(modelOnym)


oregOnym <- outreg(list("LSD" = LSDmodelOnym,
                        "Fentanyl" = FentModelOnymous), 
                   print.results = FALSE, float = TRUE, type = "csv")
cat(oregOnym, file = "oregOnym.csv")

oregChina <- outreg(list("LSD" = LSDmodelChin,
                        "Fentanyl" = FentModelChina), 
                   print.results = FALSE, float = TRUE, type = "csv")
cat(oregChina, file = "oregChina.csv")

