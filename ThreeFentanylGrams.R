

library(lmtest)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(scales)
library(data.table)

## Download both Grams folders into one folder. Do not include the May 29th 2014 folder (not in format).

setwd("C:/Users/jacobmiller21/Desktop/Darknet Data")
DIR <- "C:/Users/jacobmiller21/Desktop/Darknet Data/GramsAll"
gramsFiles <- list.files(path = DIR, all.files=TRUE, full.names = TRUE, recursive = TRUE)
grams <- data.frame()

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

## Load intermediate file if above step has been performed and written to csv
## setwd("C:/Users/jacobmiller21/Desktop/Darknet Data")
## grams <- read.csv(file = "CompiledGramsTotalFentanyl.csv", stringsAsFactors = FALSE)
## setwd("C:/Users/jacobmiller21/Desktop/Darknet Data/ThreeAnalogue")
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

## Identify the different fentanyl analogs and consumption methods
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


## There is a fentanyl lottery without a clear product
cleangrams <- grams[(grams$Amt100mg + grams$Amt200mg + grams$Amt250mg + 
                       grams$Amt500mg + grams$Amt1g + grams$Amt5g <2),]

## Depending on the frequency of the crawler, if a market produces
## identical results in terms of bitcoin, the crawler pulling the results
## did not successfully produce the new results and merely reported the
## old bitcoin price. Because bitcoin prices vary every day, we can
## assume that the crawler didn't update if bitcoin prices for a certain
## product from a certain vendor on a certain market are identical.

cleangrams <- cleangrams[!duplicated(cleangrams[,c("vendor_name", "market_name", "price", "name")]),]

## Include only most popular amounts
cleangrams <- cleangrams[(cleangrams$Amt100mg | cleangrams$Amt200mg | cleangrams$Amt250mg | cleangrams$Amt500mg | cleangrams$Amt1g | cleangrams$Amt5g),]

## Screen out fat finger listings (looks like confused BTC with dollar prices)
cleangrams <- cleangrams[cleangrams$dollarPrice < 10000,]

## Add three events of interest
cleangrams$Onymous <- cleangrams$Date >= as.Date("2014-11-06")
cleangrams$PostAcetylChinaBan <- cleangrams$Date >= as.Date("2015-10-01")
cleangrams$PostAcetylSched1 <- cleangrams$Date >= as.Date("2015-05-21")

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

## Break up each listing into bi-monthly data
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

## Remove listings that appear more than once during the same 15 day period
cleangrams <- cleangrams[!duplicated(cleangrams[,c("vendor_name", "market_name", "name", "YrMonthBi")]),]
cleangrams$fent <- cleangrams$NameAcetyl == FALSE & cleangrams$NameButyr == FALSE &
  cleangrams$NameCitrate == FALSE & cleangrams$NameFuranyl == FALSE


## Calculate beginning and ending median prices
## 100mg
median(cleangrams[cleangrams$NameAcetyl & cleangrams$Amt100mg &
                    month(cleangrams$Date) == 7 & year(cleangrams$Date) == 2014,
                  "dollarPrice"])
median(cleangrams[cleangrams$NameAcetyl & cleangrams$Amt100mg &
                    month(cleangrams$Date) == 4 & year(cleangrams$Date) == 2016,
                  "dollarPrice"])
median(cleangrams[cleangrams$NameButyr & cleangrams$Amt100mg &
                    month(cleangrams$Date) == 7 & year(cleangrams$Date) == 2014,
                  "dollarPrice"])
median(cleangrams[cleangrams$NameButyr & cleangrams$Amt100mg &
                    month(cleangrams$Date) == 4 & year(cleangrams$Date) == 2016,
                  "dollarPrice"])
median(cleangrams[cleangrams$fent & cleangrams$Amt100mg &
                    month(cleangrams$Date) == 7 & year(cleangrams$Date) == 2014,
                  "dollarPrice"])
median(cleangrams[cleangrams$fent & cleangrams$Amt100mg &
                    month(cleangrams$Date) == 4 & year(cleangrams$Date) == 2016,
                  "dollarPrice"])
## 250mg median prices
median(cleangrams[cleangrams$NameAcetyl & cleangrams$Amt250mg &
                    month(cleangrams$Date) == 7 & year(cleangrams$Date) == 2014,
                  "dollarPrice"])
median(cleangrams[cleangrams$NameAcetyl & cleangrams$Amt250mg &
                    month(cleangrams$Date) == 4 & year(cleangrams$Date) == 2016,
                  "dollarPrice"])
median(cleangrams[cleangrams$NameButyr & cleangrams$Amt250mg &
                    month(cleangrams$Date) == 7 & year(cleangrams$Date) == 2014,
                  "dollarPrice"])
median(cleangrams[cleangrams$NameButyr & cleangrams$Amt250mg &
                    month(cleangrams$Date) == 4 & year(cleangrams$Date) == 2016,
                  "dollarPrice"])
median(cleangrams[cleangrams$fent & cleangrams$Amt250mg &
                    month(cleangrams$Date) == 7 & year(cleangrams$Date) == 2014,
                  "dollarPrice"])
median(cleangrams[cleangrams$fent & cleangrams$Amt250mg &
                    month(cleangrams$Date) == 4 & year(cleangrams$Date) == 2016,
                  "dollarPrice"])
## 500mg median prices
median(cleangrams[cleangrams$NameAcetyl & cleangrams$Amt500mg &
                    month(cleangrams$Date) == 7 & year(cleangrams$Date) == 2014,
                  "dollarPrice"])
median(cleangrams[cleangrams$NameAcetyl & cleangrams$Amt500mg &
                    month(cleangrams$Date) == 4 & year(cleangrams$Date) == 2016,
                  "dollarPrice"])
median(cleangrams[cleangrams$NameButyr & cleangrams$Amt500mg &
                    month(cleangrams$Date) == 7 & year(cleangrams$Date) == 2014,
                  "dollarPrice"])
median(cleangrams[cleangrams$NameButyr & cleangrams$Amt500mg &
                    month(cleangrams$Date) == 4 & year(cleangrams$Date) == 2016,
                  "dollarPrice"])
median(cleangrams[cleangrams$fent & cleangrams$Amt500mg &
                    month(cleangrams$Date) == 7 & year(cleangrams$Date) == 2014,
                  "dollarPrice"])
median(cleangrams[cleangrams$fent & cleangrams$Amt500mg &
                    month(cleangrams$Date) == 4 & year(cleangrams$Date) == 2016,
                  "dollarPrice"])




ts <- cleangrams %>% group_by_(.dots=c("YrMonthBi", "Analogue", "Amt")) %>% 
  summarize(mean=mean(dollarPrice), median=median(dollarPrice),
            q25th=quantile(dollarPrice, 0.25),
            q75th=quantile(dollarPrice, 0.75),
            max=max(dollarPrice),min=min(dollarPrice),
            vendors=length(unique(vendor_name)))

ts <- ts[ts$Analogue != "Citrate",]

ts$Onymous <- ts$YrMonthBi >= as.Date("2014-11-06")
ts$AcetylSched1 <- ts$YrMonthBi >= as.Date("2015-05-21")
ts$AcetylButyrChinaBan <- ts$YrMonthBi >= as.Date("2015-10-01")

tsMethod <- cleangrams %>% group_by_(.dots=c("YrMonthBi", "Method")) %>% 
  summarize(mean=mean(dollarPrice), median=median(dollarPrice),
            q25th=quantile(dollarPrice, 0.25),
            q75th=quantile(dollarPrice, 0.75),
            max=max(dollarPrice),min=min(dollarPrice),
            vendors=length(unique(vendor_name)))
tsAnalogue <- cleangrams %>% group_by_(.dots=c("YrMonthBi", "Analogue")) %>% 
  summarize(mean=mean(dollarPrice), median=median(dollarPrice),
            q25th=quantile(dollarPrice, 0.25),
            q75th=quantile(dollarPrice, 0.75),
            max=max(dollarPrice),min=min(dollarPrice),
            vendors=length(unique(vendor_name)))
tsAnalogue <- tsAnalogue[tsAnalogue$Analogue != "Citrate",]
tsAmt <- cleangrams %>% group_by_(.dots=c("YrMonthBi", "Amt")) %>% 
  summarize(mean=mean(dollarPrice), median=median(dollarPrice),
            q25th=quantile(dollarPrice, 0.25),
            q75th=quantile(dollarPrice, 0.75),
            max=max(dollarPrice),min=min(dollarPrice),
            vendors=length(unique(vendor_name)))



## 100mg
ggplot(ts[ts$Amt=="Amt100mg",], aes(x=YrMonthBi, y=median, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("100mg Price by Fentanyl Type
  Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Median Price") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)
ggsave("100mg Plot.jpeg", device = "jpeg")

ggplot(ts[ts$Amt=="Amt100mg",], aes(x=YrMonthBi, y=vendors, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Number of 100mg Vendors by Fentanyl Type
          Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)

## 250mg
ggplot(ts[ts$Amt=="Amt250mg",], aes(x=YrMonthBi, y=median, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("250mg Price by Fentanyl Type
  Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Median Price") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)
ggsave("250mg Plot.jpeg", device = "jpeg")

ggplot(ts[ts$Amt=="Amt250mg",], aes(x=YrMonthBi, y=vendors, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Number of 250mg Vendors by Fentanyl Type
          Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)

## 500mg
ggplot(ts[ts$Amt=="Amt500mg",], aes(x=YrMonthBi, y=median, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("500mg Price by Fentanyl Type
  Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Median Price") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)
ggsave("500mg Plot.jpeg", device = "jpeg")

ggplot(ts[ts$Amt=="Amt500mg",], aes(x=YrMonthBi, y=vendors, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Number of 500mg Vendors by Fentanyl Type
          Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)

## 1g
ggplot(ts[ts$Amt=="Amt1g",], aes(x=YrMonthBi, y=median, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("1g Price by Fentanyl Type
Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Median Price") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)
ggsave("1g Plot.jpeg", device = "jpeg")

ggplot(ts[ts$Amt=="Amt1g",], aes(x=YrMonthBi, y=vendors, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Number of 1g Vendors by Fentanyl Type
          Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)

## 5g
ggplot(ts[ts$Amt=="Amt5g",], aes(x=YrMonthBi, y=median, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("5g Price by Fentanyl Type
Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Median Price") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)
ggsave("5g Plot.jpeg", device = "jpeg")

ggplot(ts[ts$Amt=="Amt5g",], aes(x=YrMonthBi, y=vendors, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Number of 5g Vendors by Fentanyl Type
          Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)

## Method Popularity
ggplot(tsMethod, aes(x=YrMonthBi, y=vendors, color = Method)) +
  geom_line(size = 1) +
  ggtitle("Number of Vendors by Product and Method
          Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)

## Analogue Popularity
ggplot(tsAnalogue, aes(x=YrMonthBi, y=vendors, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Number of Vendors by Analogue") +
  xlab("Date") + ylab("Unique Listings per 15 Day Period") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))
ggsave("Analogue Popularity.jpeg", device = "jpeg")

## Composite Price Graphs
ts <- data.frame(ts)
## Composite Prices
for (n in 1:nrow(ts)) {
  tempAmt <- ts[[n,"Amt"]]
  tempAnlg <- ts[[n,"Analogue"]]
  ts[n,"Composite"] <- (ts[n,"median"] - mean(ts[ts$Amt == tempAmt & ts$Analogue == tempAnlg,"median"])) /
    sd(ts[ts$Amt == tempAmt & ts$Analogue == tempAnlg,"median"])
}

ggplot(ts[ts$Amt=="Amt100mg",], aes(x=YrMonthBi, y=Composite, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Composite Price of 100mg by Analogue
          Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Standard Deviations") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)

ggplot(ts[ts$Amt=="Amt250mg",], aes(x=YrMonthBi, y=Composite, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Composite Price of 250mg by Analogue
          Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Standard Deviations") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)

ggplot(ts[ts$Amt=="Amt500mg",], aes(x=YrMonthBi, y=Composite, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Composite Price of 500mg by Analogue
          Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Standard Deviations") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)

ggplot(ts[ts$Amt=="Amt1g",], aes(x=YrMonthBi, y=Composite, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Composite Price of 1g by Analogue
          Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Standard Deviations") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)

ggplot(ts[ts$Amt=="Amt5g",], aes(x=YrMonthBi, y=Composite, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Composite Price of 5g by Analogue
          Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Standard Deviations") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)

## Composite by Analogue
tsComp <- ts %>% group_by_(.dots=c("YrMonthBi", "Analogue")) %>%
  summarize(Composite = mean(Composite))
ggplot(tsComp[tsComp$Composite <2 & tsComp$Analogue != "Citrate" & tsComp$Analogue != "Furanyl",], aes(x=YrMonthBi, y=Composite, color = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Composite Median Price by Analogue") +
  xlab("Date") + ylab("Standard Deviations") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))
ggsave("Composite Analogues.jpeg", device = "jpeg")

# Removed lines
#+
#  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
#             color = "black", lwd = 1.5) +
#  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
#             color = "black", lwd = 1.5) +
#  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
#             color = "black", lwd = 1.5)



## Statistical Analysis -- Linear Model
ts$lnMedian <- log(ts$median)
ts$Onymous <- ts$YrMonthBi >= as.Date("2014-11-06")
ts$PostAcetylSched1 <- ts$YrMonthBi >= as.Date("2015-05-21")
ts$PostChinaBan <- ts$YrMonthBi >= as.Date("2015-10-01")

ts$TrendOnym <- (ts$YrMonthBi-as.Date("2014-11-06")) * ts$Onymous
ts$TrendSched1 <- (ts$YrMonthBi-as.Date("2015-05-21")) * ts$PostAcetylSched1
ts$TrendChina <- (ts$YrMonthBi-as.Date("2015-10-01")) * ts$PostChinaBan

## Exclude 200mg. Missing data for most of the dataset in this category.
ts <- ts[ts$Amt != "Amt200mg",]

## Run iterative model, beginning with most simple
modelts <- ts
modelts$YrMonthBi <- as.numeric(modelts$YrMonthBi - as.Date("2014-06-01"))
ModelLog <- modelts[,c("lnMedian", "YrMonthBi",
                  "Amt", "Analogue",
                  "Onymous", "PostAcetylSched1", "PostChinaBan",
                  "TrendOnym", "TrendSched1", "TrendChina")]

modelSimplest <- lm(lnMedian ~ Amt, data = ModelLog)
summary(modelSimplest)
modelSimpler <- lm(lnMedian ~ Amt + Analogue, data = ModelLog)
summary(modelSimpler)
modelSimple <- lm(lnMedian~ Amt + Analogue + YrMonthBi +
                    YrMonthBi*Analogue, data = ModelLog[,1:4])
summary(modelSimple)
modelLog <- lm(lnMedian ~. + YrMonthBi*Analogue, data = ModelLog)
summary(modelLog)

x <- data.frame(ModelLog, resid = resid(modelLog), predict = predict(modelLog))










## Graphs derived from CDC Wonder database
OpioidDeaths <- read.csv("Deaths from All Opiates.csv")
SynthDeaths <- read.csv("Deaths Synthetics Excl Methadone.csv")
SynthStateDeaths <- read.csv("Synth Deaths by State.csv")

OpioidandSynth <- OpioidDeaths
OpioidandSynth$Synthetics <- SynthDeaths$Deaths
OpioidandSynth <- OpioidandSynth[,c(1,3,5)]
names(OpioidandSynth) <- c("Year", "All Opioids", "Synthetics")

OSMelt <- melt(OpioidandSynth, id = c("Year"))
names(OSMelt) <- c("Year", "Cause", "Deaths")


ggplot(OSMelt) + geom_line(aes(x = Year, y=Deaths, color = Cause), size = 2) +
  ggtitle("US Accidental Deaths from All Opioids vs Synthetics") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 40000), labels = comma,
                     breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000))

ggsave("US Accidental Deaths All Opioids vs Synthetics.jpeg", device = "jpeg")



ggplot(OpioidDeaths, aes(x = Year, y = Opiate.Deaths.LHS.)) +
  geom_line(size = 2, color = "red") +
  xlab("Year") + ylab("US Deaths") + 
  ggtitle("US Deaths from Opioids, Including Synthetics") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 35000), labels = comma, 
                     breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000))

ggsave("UsDeathsAllOpioids.jpeg", device = "jpeg")


StateMelt <- melt(SynthStateDeaths, id = c("Year"))
names(StateMelt) <- c("Year", "State", "Deaths.per.100k")

ggplot(StateMelt) + geom_line(aes(x = Year, y = Deaths.per.100k, 
                                  color = State), size = 2) +
  xlab("Year") + ylab("Deaths per 100k") +
  ggtitle("Deaths Involving Synthetics excl Methadone")

ggsave("StateSynthDeaths.jpeg", device = "jpeg")

## Confusion Matrix - 100% accuracy in manual classification test
ConfusionData <- read.csv("ConfusionMatrix.csv")
table(ConfusionData$Manual.Classification, ConfusionData$Algorithm.Classification)
