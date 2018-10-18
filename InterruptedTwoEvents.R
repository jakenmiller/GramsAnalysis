#### Libraries ####

library(ggplot2)
library(lmtest)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(scales)
library(data.table)
library(stringr)
library(rockchalk)

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
  log$FentanylName <- grepl("fentanyl", log$LowerName)
  log <- log[log$FentanylName == TRUE,]
  grams <- rbind(grams,log)
}


## Intermediate storage: write.csv(grams, "CompiledGramsTotalFentanyl.csv")

#### Data Cleaning ####
## Load intermediate file if above step has been performed and written to csv
## setwd("C:/Users/jacobmiller21/Desktop/Darknet Data")
## grams <- read.csv(file = "CompiledGramsTotalFentanyl.csv", stringsAsFactors = FALSE)
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
grams <- join(grams, btc, by = "Date")
grams$Date <- as.Date(grams$Date)
rm(btc)

## Clean out "X" columns
grams <- grams[, !names(grams) %in% c("X", "X.1")]

## Convert BTC price of good to USD price
grams$price <- gsub(",","",grams$price)
grams$price <- as.numeric(grams$price)
grams$dollarPrice <- grams$price * grams$btcPrice

## Clean out listings with text as prices (not numbers)
grams <- grams[complete.cases(grams[,"dollarPrice"]),]

## Uniquely nail down using regex each of the doseage amounts. Confusion matrix demonstrates high accuracy.

grams$Amt100mg <- 
  grepl("100mg", grams$LowerName) | 
  grepl("100[[:space:]]mg", grams$LowerName) | 
  grepl("\\.1g", grams$LowerName) | 
  grepl("\\.1[[:space:]]g", grams$LowerName)

grams$Amt200mg <- 
  !grepl("\\.00[[:space:]]2[[:space:]]g", grams$LowerName) &
  (grepl("200mg", grams$LowerName) | 
     grepl("200[[:space:]]mg", grams$LowerName) | 
     grepl("0\\.2g", grams$LowerName) | 
     grepl("0\\.2[[:space:]]g", grams$LowerName) |
     grepl("0[[:space:]]2g", grams$LowerName) | 
     grepl("0[[:space:]]2[[:space:]]g", grams$LowerName))

grams$Amt250mg <- 
  !grepl("00[[:space:]]25[[:space:]]g", grams$LowerName) &
  (grepl("250mg", grams$LowerName) | 
     grepl("250[[:space:]]mg", grams$LowerName) | 
     grepl("0\\.25g", grams$LowerName) | 
     grepl("0\\.25[[:space:]]g", grams$LowerName) |
     grepl("0[[:space:]]25g", grams$LowerName) | 
     grepl("0[[:space:]]25[[:space:]]g", grams$LowerName))

grams$Amt500mg <- 
  !(
    grepl("\\.00[[:space:]]5g", grams$LowerName) |
      grepl("[[:digit:]]500mg", grams$LowerName) |
      grepl("\\.00[[:space:]]5[[:space:]]g", grams$LowerName) |
      grepl("[[:digit:]]00[[:space:]]5g", grams$LowerName) |
      grepl("[[:space:]][[:digit:]][[:space:]]500mg", grams$LowerName)) &
  (grepl("500mg", grams$LowerName) | 
     grepl("500[[:space:]]mg", grams$LowerName) | 
     grepl("0\\.5g", grams$LowerName) | 
     grepl("0\\.5[[:space:]]g", grams$LowerName) |
     grepl("0[[:space:]]5g", grams$LowerName) | 
     grepl("0[[:space:]]5[[:space:]]g", grams$LowerName))

grams$Amt1g <- 
  (!
     (grepl("0[[:space:]]1g", grams$LowerName)| 
        grepl("0[[:space:]]1[[:space:]]g", grams$LowerName))) & 
  (grepl("[[:space:]]1[[:space:]]g", grams$LowerName) | 
     grepl("[[:space:]]1g", grams$LowerName) | 
     grepl("1[[:space:]]000mg", grams$LowerName) | 
     grepl("1000mg", grams$LowerName) |
     grepl("1[[:space:]]000[[:space:]]mg", grams$LowerName) | 
     grepl("^1[[:space:]]g", grams$LowerName) | 
     grepl("^1g", grams$LowerName))

grams$Amt5g <- 
  (!
     (grepl("[[:space:]]0[[:space:]]5g", grams$LowerName)| 
        grepl("[[:space:]]0[[:space:]]5[[:space:]]g", grams$LowerName) |
        grepl("^0[[:space:]]5g", grams$LowerName) |
        grepl("^0[[:space:]]5[[:space:]]g", grams$LowerName) |
        grepl("3[[:space:]]5g", grams$LowerName) |
        grepl("2[[:space:]]5[[:space:]]gram", grams$LowerName))) & 
  (grepl("[[:space:]]5[[:space:]]g", grams$LowerName) | 
     grepl("[[:space:]]5g", grams$LowerName) | 
     grepl("[[:space:]]5[[:space:]]g", grams$LowerName) |
     grepl("5[[:space:]]000mg", grams$LowerName) | 
     grepl("5[[:space:]]000[[:space:]]mg", grams$LowerName) | 
     grepl("^5[[:space:]]g", grams$LowerName) | 
     grepl("^5g", grams$LowerName))

## Remove awkward 2500mg and similar products for future "most common amounts" analysis
grams <- grams[!grepl("2[[:space:]]500[[:space:]]mg",grams$LowerName),]
grams <- grams[!grepl("2[[:space:]]500mg",grams$LowerName),]
grams <- grams[!grepl("1[[:space:]]5[[:space:]]gram",grams$LowerName),]
grams <- grams[!grepl("3[[:space:]]5[[:space:]]grams",grams$LowerName),]

## Converts dollar prices into natural logged form
grams$LogDollar <- log(grams$dollarPrice)

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


## Add in each fenatnyl analogue and method
grams$NameButyr <- grepl("butyr", grams$LowerName)
grams$NameAcetyl <- grepl("acetyl", grams$LowerName)
grams$NameFuranyl <- grepl("furanyl", grams$LowerName)
grams$NameCitrate <- grepl("citrate", grams$LowerName)
grams$MethodPatch <- grepl("patch", grams$LowerName) | grepl("transdermal", grams$LowerName)
grams$MethodHCl <- grepl("hcl", grams$LowerName)
grams$MethodBlotter <- grepl("blot", grams$LowerName)
grams$MethodPill <- grepl("cand", grams$LowerName) | grepl("pill", grams$LowerName)
grams$MethodGel <- grepl("gel", grams$LowerName)
grams$MethodPowder <- grepl("powder", grams$LowerName)
grams$DateNum <- as.numeric(grams$Date - as.Date("2014-06-09"))

## Depending on the frequency of the crawler, if a market produces
## identical results in terms of bitcoin, the crawler pulling the results
## did not successfully produce the new results and merely reported the
## old bitcoin price. Because bitcoin prices vary every day, we can
## assume that the crawler didn't update if bitcoin prices for a certain
## product from a certain vendor on a certain market are identical.

cleangrams <- grams
cleangrams <- cleangrams[!duplicated(cleangrams[,c("vendor_name", "market_name", "price", "name")]),]

## Include only most popular amounts
cleangrams <- cleangrams[(cleangrams$Amt100mg | cleangrams$Amt200mg | cleangrams$Amt250mg | cleangrams$Amt500mg | cleangrams$Amt1g | cleangrams$Amt5g),]

## Screen out fat finger listings (looks like confused BTC with dollar prices)
cleangrams <- cleangrams[cleangrams$dollarPrice < 10000,]

## There is a fentanyl lottery without a clear product
cleangrams <- cleangrams[(cleangrams$Amt100mg + cleangrams$Amt200mg + cleangrams$Amt250mg + 
                            cleangrams$Amt500mg + cleangrams$Amt1g + cleangrams$Amt5g <2),]

## Add three events of interest
cleangrams$Onymous <- cleangrams$Date >= as.Date("2014-11-06")
cleangrams$PostAcetylChinaBan <- cleangrams$Date >= as.Date("2015-10-01")

## Add names for amounts and analogues to treat as factors as needed
for (x in 1:nrow(cleangrams)) {
  if(cleangrams[x,"Amt100mg"]) {
    cleangrams[x,"Amt"] <- "Amt100mg"
  } 
  if (cleangrams[x,"Amt200mg"]) {
    cleangrams[x,"Amt"] <- "Amt200mg"
  } 
  if (cleangrams[x,"Amt250mg"]) {
    cleangrams[x,"Amt"] <- "Amt250mg"
  } 
  if (cleangrams[x,"Amt500mg"]) {
    cleangrams[x,"Amt"] <- "Amt500mg"
  } 
  if (cleangrams[x,"Amt1g"]) {
    cleangrams[x,"Amt"] <- "Amt1g"
  } 
  if (cleangrams[x,"Amt5g"]) {
    cleangrams[x,"Amt"] <- "Amt5g"
  }
}


## Add names for amounts and analogues to treat as factors as needed
for (x in 1:nrow(cleangrams)) {
  if(cleangrams[x,"MethodPatch"]) {
    cleangrams[x,"Method"] <- "Patch"
  } 
  else if (cleangrams[x,"MethodHCl"]) {
    cleangrams[x,"Method"] <- "HCl"
  } 
  else if (cleangrams[x,"MethodPill"]) {
    cleangrams[x,"Method"] <- "Pill"
  } 
  else if (cleangrams[x,"MethodGel"]) {
    cleangrams[x,"Method"] <- "Gel"
  } 
  else if (cleangrams[x,"MethodPowder"]) {
    cleangrams[x,"Method"] <- "Powder"
  }
  else
    cleangrams[x,"Method"] <- "Other"
}

for (x in 1:nrow(cleangrams)) {
  if(cleangrams[x,"NameFuranyl"]) {
    cleangrams[x,"Analogue"] <- "Furanyl"
  } 
  else if (cleangrams[x,"NameAcetyl"]) {
    cleangrams[x,"Analogue"] <- "Acetyl"
  } 
  else if (cleangrams[x,"NameButyr"]) {
    cleangrams[x,"Analogue"] <- "Butyr"
  }
  else if (cleangrams[x,"NameCitrate"]) {
    cleangrams[x,"Analogue"] <- "Citrate"
  }
  else
    cleangrams[x,"Analogue"] <- "Fentanyl"
}

for (x in 1:nrow(cleangrams)) {
  cleangrams[x,"YrMonthBi"] <- paste(year(cleangrams[x,"Date"]),
                                     month(cleangrams[x,"Date"]),
                                     if (day(cleangrams[x,"Date"]) >=16) {
                                       "15"
                                     } else {
                                       "01"
                                     }, sep = "-")
}
rm(x)
cleangrams$YrMonthBi <- as.Date(cleangrams$YrMonthBi)
cleangrams <- cleangrams[cleangrams$Amt != "Amt200mg",]

## Add in 10 day increments
cleangrams$TrendOnym <- as.numeric(cleangrams$Date) - as.numeric(as.Date("2014-11-05"))
cleangrams$TrendOnym <- round_any(cleangrams$TrendOnym, 10, f = ceiling)
cleangrams$TrendChin <- as.numeric(cleangrams$Date) - as.numeric(as.Date("2015-09-30"))
cleangrams$TrendChin <- round_any(cleangrams$TrendChin, 10, f = ceiling)


#### Graphs ####

cg15 <- cleangrams[!duplicated(cleangrams[,c("vendor_name", "market_name", "name", "YrMonthBi")]),]

ts <- cg15 %>% group_by_(.dots=c("YrMonthBi", "Analogue", "Amt")) %>% 
  summarize(mean=mean(dollarPrice), median=median(dollarPrice),
            q25th=quantile(dollarPrice, 0.25),
            q75th=quantile(dollarPrice, 0.75),
            max=max(dollarPrice),min=min(dollarPrice),
            vendors=length(unique(vendor_name)))

ts$Onymous <- ts$YrMonthBi >= as.Date("2014-11-06")
ts$AcetylButyrChinaBan <- ts$YrMonthBi >= as.Date("2015-10-01")
ts$Analogue <- factor(ts$Analogue, 
                      levels=c("Fentanyl", "Acetyl", "Furanyl", "Citrate", "Butyr"))


tsMethod <- cg15 %>% group_by_(.dots=c("YrMonthBi", "Method")) %>% 
  summarize(mean=mean(dollarPrice), median=median(dollarPrice),
            q25th=quantile(dollarPrice, 0.25),
            q75th=quantile(dollarPrice, 0.75),
            max=max(dollarPrice),min=min(dollarPrice),
            vendors=length(unique(vendor_name)))
tsAnalogue <- cg15 %>% group_by_(.dots=c("YrMonthBi", "Analogue")) %>% 
  summarize(mean=mean(dollarPrice), median=median(dollarPrice),
            q25th=quantile(dollarPrice, 0.25),
            q75th=quantile(dollarPrice, 0.75),
            max=max(dollarPrice),min=min(dollarPrice),
            vendors=length(unique(vendor_name)))

tsAmt <- cg15 %>% group_by_(.dots=c("YrMonthBi", "Amt")) %>% 
  summarize(mean=mean(dollarPrice), median=median(dollarPrice),
            q25th=quantile(dollarPrice, 0.25),
            q75th=quantile(dollarPrice, 0.75),
            max=max(dollarPrice),min=min(dollarPrice),
            vendors=length(unique(vendor_name)))


## Total Vendors for Popular Amounts

tvendors <- cg15 %>% group_by_(.dots=c("YrMonthBi", "Analogue")) %>% 
  summarize(vendors=length(unique(vendor_name)))

## Arrange order of analogues for legend
tvendors$Analogue <- factor(tvendors$Analogue, 
                            levels=c("Fentanyl", "Acetyl", "Furanyl", "Butyr", "Citrate"))


ggplot(tvendors[tvendors$Analogue != "Citrate" & 
                  tvendors$Analogue != "Butyr",], aes(x=YrMonthBi, y=vendors, linetype = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Unique Vendors for Fentanyl and Popular Analogues
Event1 = Onymous, Event2 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)
ggsave("BW-Unique Vendors1.jpeg", device = "jpeg")

ggplot(tvendors[tvendors$Analogue != "Fentanyl" & 
                  tvendors$Analogue != "Acetyl" &
                  tvendors$Analogue != "Furanyl",], aes(x=YrMonthBi, y=vendors, linetype = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Unique Vendors for Less Popular Analogues
Event1 = Onymous, Event2 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)
ggsave("BW-Unique Vendors2.jpeg", device = "jpeg")


## 100mg
ggplot(ts[ts$Amt=="Amt100mg" & ts$Analogue != "Butyr" & 
            ts$Analogue != "Citrate",], aes(x=YrMonthBi, y=median, linetype = Analogue)) +
  geom_line(size = 1) +
  ggtitle("100mg Price by Fentanyl Type
          Event1 = Onymous, Event2 = China Ban") +
  xlab("Date") + ylab("Median Price") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  scale_linetype_manual(values=c("dashed", "solid", "dotted")) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)
ggsave("BW-100mg Plot.jpeg", device = "jpeg")

ggplot(ts[ts$Amt=="Amt100mg",], aes(x=YrMonthBi, y=vendors, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Number of 100mg Vendors by Fentanyl Type
          Event1 = Onymous, Event2 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)

## 250mg
ggplot(ts[ts$Amt=="Amt250mg" & ts$Analogue != "Butyr",], aes(x=YrMonthBi, y=median, linetype = Analogue)) +
  geom_line(size = 1) +
  ggtitle("250mg Price by Fentanyl Type
          Event1 = Onymous, Event2 = China Ban") +
  xlab("Date") + ylab("Median Price") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)
ggsave("BW-250mg Plot.jpeg", device = "jpeg")

ggplot(ts[ts$Amt=="Amt250mg",], aes(x=YrMonthBi, y=vendors, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Number of 250mg Vendors by Fentanyl Type
          Event1 = Onymous, Event2 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)

## 500mg
ggplot(ts[ts$Amt=="Amt500mg" & ts$Analogue != "Butyr" & 
            ts$Analogue != "Citrate",], aes(x=YrMonthBi, y=median, linetype = Analogue)) +
  geom_line(size = 1) +
  ggtitle("500mg Price by Fentanyl Type
Event1 = Onymous, Event2 = China Ban") +
  xlab("Date") + ylab("Median Price") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  scale_linetype_manual(values=c("dashed", "solid", "dotted")) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)
ggsave("BW-500mg Plot.jpeg", device = "jpeg")

ggplot(ts[ts$Amt=="Amt500mg",], aes(x=YrMonthBi, y=vendors, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Number of 500mg Vendors by Fentanyl Type
          Event1 = Onymous, Event2 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)

## 1g
ggplot(ts[ts$Amt=="Amt1g" & ts$Analogue != "Butyr" & 
            ts$Analogue != "Citrate",], aes(x=YrMonthBi, y=median, linetype = Analogue)) +
  geom_line(size = 1) +
  ggtitle("1g Price by Fentanyl Type
          Event1 = Onymous, Event2 = China Ban") +
  xlab("Date") + ylab("Median Price") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)
ggsave("BW-1g Plot.jpeg", device = "jpeg")

ggplot(ts[ts$Amt=="Amt1g",], aes(x=YrMonthBi, y=vendors, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Number of 1g Vendors by Fentanyl Type
Event1 = Onymous, Event2 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)

## 5g
ggplot(ts[ts$Amt=="Amt5g" & ts$Analogue != "Butyr" &
            ts$Analogue != "Citrate",], aes(x=YrMonthBi, y=median, linetype = Analogue)) +
  geom_line(size = 1) +
  ggtitle("5g Price by Fentanyl Type
          Event1 = Onymous, Event2 = China Ban") +
  xlab("Date") + ylab("Median Price") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)
ggsave("BW-5g Plot.jpeg", device = "jpeg")

ggplot(ts[ts$Amt=="Amt5g",], aes(x=YrMonthBi, y=vendors, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Number of 5g Vendors by Fentanyl Type
          Event1 = Onymous, Event2 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)
ggsave("5g Plot.jpeg", device = "jpeg")

## Lethal doses
ld <- ts

for (x in 1:nrow(ld)) {
  if(ld[x,"Amt"] == "Amt100mg") {
    ld[x,"median"] <- ld[x,"median"] / 40
  } 
  if(ld[x,"Amt"] == "Amt250mg") {
    ld[x,"median"] <- ld[x,"median"] / 100
  } 
  if(ld[x,"Amt"] == "Amt500mg") {
    ld[x,"median"] <- ld[x,"median"] / 200
  }
  if(ld[x,"Amt"] == "Amt1g") {
    ld[x,"median"] <- ld[x,"median"] / 400
  }
  if(ld[x,"Amt"] == "Amt5g") {
    ld[x,"median"] <- ld[x,"median"] / 2000
  }
}

ld <- ld[ld$Amt == "Amt100mg" | 
           ld$Amt == "Amt500mg" |
           ld$Amt == "Amt5g",]

ggplot(ld[ld$Analogue=="Fentanyl",], aes(x=YrMonthBi, y=median, color = Amt)) +
  geom_line(size = 1.3) +
  ggtitle("Cost of Lethal Dose of Illicit Fentanyl (USD)
Event1 = Onymous, Event2 = China Ban") +
  xlab("Date") + ylab("Median Price") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  scale_color_grey(start = 0, end = 0.6)
ggsave("BW-Lethal Doses.jpeg", device = "jpeg")

## Method Popularity
ggplot(tsMethod, aes(x=YrMonthBi, y=vendors, color = Method)) +
  geom_line(size = 1) +
  ggtitle("Number of Vendors by Product and Method
          Event1 = Onymous, Event2 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)

## Analogue Popularity
ggplot(tsAnalogue, aes(x=YrMonthBi, y=vendors, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Number of Vendors by Analogue") +
  xlab("Date") + ylab("Unique Listings per 15 Day Period") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))
ggsave("Analogue Popularity.jpeg", device = "jpeg")


#### Onymous Analysis ####
# DateNum is days since 2014-06-09
# Onymous took place on 2014-11-06 day 150
# China ban took place on 2015-01-10 day 479



## Remove listings that appear more than once during the same 10 day period
cleangrams10Onym <- cleangrams[!duplicated(cleangrams[,c("vendor_name", "market_name", "name", "TrendOnym")]),]

## Set up trends and events for model by median price
dfOnym <- cleangrams10Onym %>% group_by_(.dots=c("TrendOnym", "Analogue", "Amt")) %>% 
  summarise(mean=mean(dollarPrice), dollarPrice=median(dollarPrice),
            q25th=quantile(dollarPrice, 0.25),
            q75th=quantile(dollarPrice, 0.75),
            max=max(dollarPrice),min=min(dollarPrice),
            Vendors=length(unique(vendor_name)))
dfOnym$Onymous <- dfOnym$TrendOnym >= 10
dfOnym$DateNum <- as.numeric(dfOnym$TrendOnym + 150)

dfOnym <- dfOnym[,c("dollarPrice", "DateNum","Analogue","Amt","Onymous","Vendors")]
dfOnym <- dfOnym[dfOnym$DateNum > 80 &  dfOnym$DateNum < 220,]
dfOnym <- as.data.frame(dfOnym)

for (n in 1:nrow(dfOnym)) {
  tempAmt <- dfOnym[[n,"Amt"]]
  tempAnlg <- dfOnym[[n,"Analogue"]]
  dfOnym[n,"Normalized"] <- (dfOnym[n,"dollarPrice"] - 
                               mean(dfOnym[dfOnym$Amt == tempAmt & dfOnym$Analogue == tempAnlg,"dollarPrice"])) /
    sd(dfOnym[dfOnym$Amt == tempAmt & dfOnym$Analogue == tempAnlg,"dollarPrice"])
}
rm(n,tempAmt,tempAnlg)



dfOnym$OnymTrend <- (dfOnym$DateNum - 150) * dfOnym$Onymous
dfOnym <- dfOnym %>%
  select(Normalized, Onymous, DateNum, OnymTrend, Vendors) %>%
  rename(Trend = DateNum)

modelOnym <- lm(Normalized ~ Onymous + Trend + OnymTrend, data = dfOnym, weights = Vendors)
summary(modelOnym)
predOnym <- modelOnym[,c("Onymous","Trend","OnymTrend","Vendors")]
predOnym$pred <- predict(model1, modelOnym[,c("Onymous","Trend","OnymTrend","Vendors")])

ggplot(dfOnym, aes(x=(Trend-155), y=Normalized)) +
  geom_point() +
  ggtitle("Fentanyl: Normalized and Weighted Price on Darknet Markets
Event = Operating Onymous Market Shutdown") +
  xlab("Days") + ylab("Standard Deviations") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=0,color = "black", lwd = 2, linetype = "longdash") +
  geom_line(data=predOnym[predOnym$OnymTrend == 0,], aes(x=(Trend-155), y=pred), lwd = 2) +
  geom_line(data=predOnym[predOnym$OnymTrend > 0,], aes(x=Trend-155, y=pred), lwd = 2)


ggsave("BW-Onymous.jpeg", device = "jpeg")

#### ChinaBan Analysis ####
# DateNum is days since 2014-06-01
# China ban took place on 2015-10-01

cleangrams10China <- cleangrams[!duplicated(cleangrams[,c("vendor_name", "market_name", "name", "TrendChin")]),]

dfChina <- cleangrams10China %>% 
  group_by_(.dots=c("TrendChin", "Analogue", "Amt")) %>% 
  summarise(mean=mean(dollarPrice), dollarPrice=median(dollarPrice),
            q25th=quantile(dollarPrice, 0.25),
            q75th=quantile(dollarPrice, 0.75),
            max=max(dollarPrice),min=min(dollarPrice),
            Vendors=length(unique(vendor_name)))
dfChina$ChinaBan <- dfChina$TrendChin >= 10
dfChina$DateNum <- as.numeric(dfChina$TrendChin + 479)

dfChina <- dfChina[,c("dollarPrice", "DateNum","Analogue","Amt","ChinaBan","Vendors")]
## Save data for other time windows
dfChin1 <- dfChina

## Data for 90 day time window
dfChina <- dfChina[dfChina$DateNum > (479-90) &  dfChina$DateNum < (479+90),]
dfChina <- as.data.frame(dfChina)


for (n in 1:nrow(dfChina)) {
  tempAmt <- dfChina[[n,"Amt"]]
  tempAnlg <- dfChina[[n,"Analogue"]]
  dfChina[n,"Normalized"] <- (dfChina[n,"dollarPrice"] - 
                               mean(dfChina[dfChina$Amt == tempAmt & dfChina$Analogue == tempAnlg,"dollarPrice"])) /
    sd(dfChina[dfChina$Amt == tempAmt & dfChina$Analogue == tempAnlg,"dollarPrice"])
}
rm(n,tempAmt,tempAnlg)

#### China 180 Day Time Window ####
dfChin1 <- dfChin1[dfChin1$DateNum > (479-180) &  dfChin1$DateNum < (479+180),]
dfChin1 <- as.data.frame(dfChin1)
for (n in 1:nrow(dfChin1)) {
  tempAmt <- dfChin1[[n,"Amt"]]
  tempAnlg <- dfChin1[[n,"Analogue"]]
  dfChin1[n,"Normalized"] <- (dfChin1[n,"dollarPrice"] - 
                               mean(dfChin1[dfChin1$Amt == tempAmt & dfChin1$Analogue == tempAnlg,"dollarPrice"])) /
    sd(dfChin1[dfChin1$Amt == tempAmt & dfChin1$Analogue == tempAnlg,"dollarPrice"])
}
rm(n,tempAmt,tempAnlg)



dfChin1$ChinTrend <- (dfChin1$DateNum - 479) * dfChin1$ChinaBan
dfChin1 <- dfChin1 %>%
  select(Normalized, ChinaBan, DateNum, ChinTrend, Vendors) %>%
  rename(Trend = DateNum)

modelChina <- lm(Normalized ~ ChinaBan + Trend + ChinTrend, data = dfChin1, weights = Vendors)
summary(modelChina)
predChina <- dfChin1[,c("ChinaBan","Trend","ChinTrend","Vendors")]
predChina$pred <- predict(modelChina, dfChin1[,c("ChinaBan","Trend","ChinTrend","Vendors")])


ggplot(dfChin1, aes(x = Trend-484, y = Normalized)) +
  geom_point() +
  ggtitle("Fentanyl: Normalized and Weighted Price on Darknet Markets
Event = China Ban") +
  xlab("Days") + ylab("Standard Deviations") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=0,color = "black", lwd = 2, linetype = "longdash") +
  geom_line(data=predChina[predChina$ChinTrend == 0,], aes(x=Trend-484, y=pred), lwd = 2) +
  geom_line(data=predChina[predChina$ChinTrend > 0,], aes(x=Trend-484, y=pred), lwd = 2)
ggsave("BW-ChinaBan.jpeg", device = "jpeg")


#### Save results for later outreg ####
save(modelChina, file = "ChinaModel.RData")
save(dfChin1, file = "ChinaData.RData")
save(modelOnym, file = "OnymModel.RData")
save(dfOnym, file = "OnymData.RData")






#### China Ban 90 day Window Robustness Check ####
modelChin2 <- dfChina
modelChin2$ChinTrend <- (modelChin2$DateNum - 479) * modelChin2$ChinaBan
modelChin2 <- modelChin2 %>%
  select(Normalized, ChinaBan, DateNum, ChinTrend, Vendors) %>%
  rename(Trend = DateNum)

model3 <- lm(Normalized ~ ChinaBan + Trend + ChinTrend, data = modelChin2, weights = Vendors)
summary(model3)
predChin2 <- modelChin2[,c("ChinaBan","Trend","ChinTrend","Vendors")]
predChin2$pred <- predict(model3, modelChin2[,c("ChinaBan","Trend","ChinTrend","Vendors")])

ggplot(modelChin2, aes(x = Trend-484, y = Normalized)) +
  geom_point() +
  ggtitle("Normalized and Weighted Price of Fentanyl Analogues
Event = China Ban") +
  xlab("Days") + ylab("Standard Deviations") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=0,color = "black", lwd = 2, linetype = "longdash") +
  geom_line(data=predChin2, aes(x=Trend-484, y=pred), lwd = 2)
ggsave("BW-ChinaBanRobust.jpeg", device = "jpeg")