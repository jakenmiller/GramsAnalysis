#### Libraries ####

library(lmtest)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(scales)
library(data.table)

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
  log$MDMAName <- grepl("mdma", log$LowerName) | grepl("ecstasy", log$LowerName)
  log <- log[log$MDMAName == TRUE,]
  grams <- rbind(grams,log)
}

# Generated X observations for 15 variables in first attempt
## Intermediate storage: write.csv(grams, "CompiledGramsHeroin.csv")

#### Data Cleaning ####
## Load intermediate file if above step has been performed and written to csv
setwd("C:/Users/jacobmiller21/Desktop/Darknet Data")
grams <- read.csv(file = "CompiledGramsMDMA.csv", stringsAsFactors = FALSE)
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

# Count number of vendors
length(unique(grams$vendor_name))

## Identify the different MDMA analogs and consumption methods
grams$Crystal <- grepl("crystal", grams$LowerName) | 
  grepl("dutch", grams$LowerName) | 
  grepl("crushed", grams$LowerName) | 
  grepl("powder", grams$LowerName) | 
  grepl("pure", grams$LowerName) | 
  grepl("champagne", grams$LowerName) | 
  grepl("rolls royce", grams$LowerName)


grams$DateNum <- as.numeric(grams$Date - as.Date("2014-06-09"))

## Uniquely nail down using regex each of the doseage amounts. Confusion matrix demonstrates high accuracy.

grams$Amt1g <- 
  !grepl("0[[:space:]]1[[:space:]]g", grams$LowerName) &
  !grepl("0[[:space:]]1g", grams$LowerName) &
  !grepl("[[:digit:]]1[[:space:]]g", grams$LowerName) & 
  !grepl("[[:digit:]]1g", grams$LowerName) & 
  !grepl("1g[[:space:]]free", grams$LowerName) &
  !grepl("1[[:space:]]g[[:space:]]free", grams$LowerName) & 
  !grepl("1gr[[:space:]]tested", grams$LowerName) & 
  !grepl("\\.1[[:space:]]g", grams$LowerName) & 
  !grepl("\\.1g", grams$LowerName) & 
  (grepl("1[[:space:]]g", grams$LowerName) | 
     grepl("1g", grams$LowerName))

grams$Amt2g <- 
  !grepl("0[[:space:]]2[[:space:]]g", grams$LowerName) &
  !grepl("0[[:space:]]2g", grams$LowerName) &
  !grepl("[[:digit:]]2[[:space:]]g", grams$LowerName) & 
  !grepl("[[:digit:]]2g", grams$LowerName) & 
    (grepl("2[[:space:]]g", grams$LowerName) | 
     grepl("2g", grams$LowerName))

grams$Amt2.5g <- 
  !grepl("0[[:space:]]25g", grams$LowerName) &
  !grepl("[[:digit:]]2[[:space:]]5g", grams$LowerName) & 
  !grepl("[[:digit:]]2[[:space:]]5[[:space:]]g", grams$LowerName) & 
  !grepl("2[[:space:]]5g[[:space:]]free", grams$LowerName) &
  !grepl("2[[:space:]]5[[:space:]]g[[:space:]]free", grams$LowerName) &
  (grepl("2[[:space:]]5g", grams$LowerName) | 
     grepl("2[[:space:]]5[[:space:]]g", grams$LowerName) |
     grepl("2\\.5 g", grams$LowerName) |
     grepl("2\\.5g", grams$LowerName))

grams$Amt5g <- 
  !grepl("0[[:space:]]5[[:space:]]g", grams$LowerName) &
  !grepl("0[[:space:]]5g", grams$LowerName) & 
  !grepl("[[:digit:]]5[[:space:]]g", grams$LowerName) & 
  !grepl("[[:digit:]]5g", grams$LowerName) & 
  !grepl("[[:digit:]][[:space:]]5g", grams$LowerName) & 
  !grepl("[[:digit:]][[:space:]]5[[:space:]]g", grams$LowerName) & 
  !grepl("5[[:space:]]g[[:space:]]free", grams$LowerName) & 
  !grepl("\\.5g", grams$LowerName) & 
  (grepl("5[[:space:]]g", grams$LowerName) | 
  grepl("5[[:space:]]g", grams$LowerName) | 
     grepl("5g", grams$LowerName))

grams$Amt10g <- 
  !grepl("0[[:space:]]10[[:space:]]g", grams$LowerName) &
  !grepl("0[[:space:]]10g", grams$LowerName) & 
  !grepl("[[:digit:]]10[[:space:]]g", grams$LowerName) & 
  !grepl("[[:digit:]]10g", grams$LowerName) &
  !grepl("10g[[:space:]]free", grams$LowerName) &
  !grepl("10[[:space:]]g[[:space:]]free", grams$LowerName) & 
  !grepl("10[[:space:]]green", grams$LowerName) & 
  (grepl("10[[:space:]]g", grams$LowerName) | 
     grepl("10g", grams$LowerName))

grams$Amt25g <- 
  !grepl("0[[:space:]]25[[:space:]]g", grams$LowerName) &
  !grepl("0[[:space:]]25g", grams$LowerName) & 
  !grepl("[[:digit:]]25[[:space:]]g", grams$LowerName) & 
  !grepl("[[:digit:]]25g", grams$LowerName) &
  (grepl("25[[:space:]]g", grams$LowerName) | 
     grepl("25g", grams$LowerName))

grams$Amt50g <- 
  !grepl("0[[:space:]]50[[:space:]]g", grams$LowerName) &
  !grepl("0[[:space:]]50g", grams$LowerName) & 
  !grepl("[[:digit:]]50[[:space:]]g", grams$LowerName) & 
  !grepl("[[:digit:]]50g", grams$LowerName) &
  (grepl("50[[:space:]]g", grams$LowerName) | 
     grepl("50g", grams$LowerName))

grams$Amt100g <- 
  !grepl("0[[:space:]]100[[:space:]]g", grams$LowerName) &
  !grepl("0[[:space:]]100g", grams$LowerName) & 
  !grepl("[[:digit:]]100[[:space:]]g", grams$LowerName) & 
  !grepl("[[:digit:]]100g", grams$LowerName) &
  (grepl("100[[:space:]]g", grams$LowerName) | 
     grepl("100g", grams$LowerName))

grams$Amt250g <- 
  !grepl("0[[:space:]]250[[:space:]]g", grams$LowerName) &
  !grepl("0[[:space:]]250g", grams$LowerName) & 
  !grepl("[[:digit:]]250[[:space:]]g", grams$LowerName) & 
  !grepl("[[:digit:]]250g", grams$LowerName) &
  (grepl("250[[:space:]]g", grams$LowerName) | 
     grepl("250g", grams$LowerName))

grams$Amt500g <- 
  !grepl("0[[:space:]]500[[:space:]]g", grams$LowerName) &
  !grepl("0[[:space:]]500g", grams$LowerName) & 
  !grepl("[[:digit:]]500[[:space:]]g", grams$LowerName) & 
  !grepl("[[:digit:]]500g", grams$LowerName) & 
  !grepl("5g[[:space:]]free", grams$LowerName) &
  !grepl("5[[:space:]]g[[:space:]]free", grams$LowerName) &
  (grepl("500[[:space:]]g", grams$LowerName) | 
     grepl("500g", grams$LowerName))

grams$Amt1000g <- 
  !grepl("0[[:space:]]1000[[:space:]]g", grams$LowerName) &
  !grepl("0[[:space:]]1000g", grams$LowerName) & 
  !grepl("[[:digit:]]1000[[:space:]]g", grams$LowerName) & 
  !grepl("[[:digit:]]1000g", grams$LowerName) &
  !grepl("0[[:space:]]1[[:space:]]kg", grams$LowerName) &
  !grepl("0[[:space:]]1kg", grams$LowerName) & 
  !grepl("[[:digit:]]1[[:space:]]kg", grams$LowerName) & 
  !grepl("[[:digit:]]1kg", grams$LowerName) &
    (grepl("1000[[:space:]]g", grams$LowerName) | 
     grepl("1000g", grams$LowerName) | 
       grepl("1[[:space:]]kg", grams$LowerName) |
       grepl("1kg", grams$LowerName))

## Only keep crystal
grams <- grams[grams$Crystal == TRUE,]
# Approximately half the listings remain

## Dual-listings, some are selling cocaine and MDMA
grams <- grams[!grepl("cocaine", grams$LowerName),]
grams <- grams[!grepl("coke", grams$LowerName),]



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
grams[grepl("not specified", tolower(grams$ship_from)),"ship_from"] <- "Undeclared"
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


## Quantity Check - Quantities should match
grams$QtyCheck <- rowSums(grams[,c(19:29)])
grams <- subset(grams, select = -c(hash, item_link, image_link))
grams <- grams[grams$QtyCheck == 1,]

## Depending on the frequency of the crawler, if a market produces
## identical results in terms of bitcoin, the crawler pulling the results
## did not successfully produce the new results and merely reported the
## old bitcoin price. Because bitcoin prices vary every day, we can
## assume that the crawler didn't update if bitcoin prices for a certain
## product from a certain vendor on a certain market are identical.
cleangrams <- grams
cleangrams <- cleangrams[!duplicated(cleangrams[,c("vendor_name", "market_name", "price", "name")]),]

## Screen out fat finger listings (looks like confused BTC with dollar prices)
cleangrams <- cleangrams[cleangrams$dollarPrice < 30000,]


## Add names for amounts and analogues to treat as factors as needed

colAmts <- grepl(pattern = "Amt[[:digit:]]", x = colnames(cleangrams))
cleangrams$Amt <- colnames(cleangrams[,colAmts])[max.col(cleangrams[,colAmts])]


## Break up each listing into 10 day increments
cleangrams$TrendOnym <- as.numeric(cleangrams$Date) - as.numeric(as.Date("2014-11-05"))
cleangrams$TrendOnym <- round_any(cleangrams$TrendOnym, 10, f = ceiling)
cleangrams$TrendChin <- as.numeric(cleangrams$Date) - as.numeric(as.Date("2015-10-01"))
cleangrams$TrendChin <- round_any(cleangrams$TrendChin, 10, f = ceiling)
## Add two events of interest
cleangrams$Onymous <- cleangrams$Date >= as.Date("2014-11-06")
cleangrams$ChinaBan <- cleangrams$Date >= as.Date("2015-10-01")


#### Onymous Analysis ####
# DateNum variable is days since 2014-06-09
# Onymous took place on 2014-11-06 day 150
# China ban took place on 2015-01-10 day 479

## Remove listings that appear more than once during the same 10 day period
DataOnym <- cleangrams[!duplicated(cleangrams[,c("vendor_name", "market_name", "name", "TrendOnym")]),]
DataOnym <- DataOnym[,c("dollarPrice", "DateNum","Amt","Onymous", "TrendOnym")]
# Set Trend Variables and capture interested period (+/- 120 days)
# (add one to window for bracketing within Stata to complete full 90 days)
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

#DataOnym <- read.csv("DataOnymousMDMA.csv")
Onym1 <- lm(LogDollarPrice ~ Trend + Onymous + OnymousTrend + Amt,
            data = DataOnym)
summary(Onym1)
#write.csv(DataOnym, "DataOnymousMDMA.csv", row.names = F)



#### China Ban Analysis ####
# DateNum variable is days since 2014-06-09
# Onymous took place on 2014-11-06 day 150
# China ban took place on 2015-01-10 day 479


## Remove listings that appear more than once during the same 10 day period
DataChina <- cleangrams[!duplicated(cleangrams[,c("vendor_name", "market_name", "name", "TrendChin")]),]
DataChina <- DataChina[,c("dollarPrice", "DateNum","Amt","ChinaBan", "TrendChin")]

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

#DataChina <- read.csv("DataChinaMDMA.csv")
China1 <- lm(LogDollarPrice ~ Trend + ChinaBan + ChinaTrend + Amt,
             data = DataChina)
summary(China1)
#write.csv(DataChina, "DataChinaMDMA.csv", row.names = F)






#### Dataframes for graphs ####

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


#### Exploratory Graphs ####

## Total Vendors for Popular Amounts

tvendors <- cleangrams %>% group_by_(.dots=c("YrMonthBi", "Analogue")) %>% 
  summarize(vendors=length(unique(vendor_name)))

## Arrange order of analogues for legend
tvendors$Analogue <- factor(tvendors$Analogue, 
                            levels=c("Fentanyl", "Acetyl", "Furanyl", "Butyr", "Citrate"))
ts$Analogue <- factor(ts$Analogue, 
                      levels=c("Fentanyl", "Acetyl", "Furanyl", "Butyr"))


ggplot(tvendors[tvendors$Analogue != "Citrate" & 
                  tvendors$Analogue != "Butyr",], aes(x=YrMonthBi, y=vendors, linetype = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Unique Vendors for Fentanyl and Popular Analogues
          Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)
ggsave("BW-Unique Vendors1.jpeg", device = "jpeg")

ggplot(tvendors[tvendors$Analogue != "Fentanyl" & 
                  tvendors$Analogue != "Acetyl" &
                  tvendors$Analogue != "Furanyl",], aes(x=YrMonthBi, y=vendors, linetype = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Unique Vendors for Less Popular Analogues
          Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)
ggsave("BW-Unique Vendors2.jpeg", device = "jpeg")


## 100mg
ggplot(ts[ts$Amt=="Amt100mg" & ts$Analogue != "Butyr",], aes(x=YrMonthBi, y=median, linetype = Analogue)) +
  geom_line(size = 1) +
  ggtitle("100mg Price by Fentanyl Type
  Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Median Price") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  scale_linetype_manual(values=c("dashed", "solid", "dotted")) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)
ggsave("BW-100mg Plot.jpeg", device = "jpeg")

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
ggplot(ts[ts$Amt=="Amt250mg" & ts$Analogue != "Butyr",], aes(x=YrMonthBi, y=median, linetype = Analogue)) +
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
ggsave("BW-250mg Plot.jpeg", device = "jpeg")

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
ggplot(ts[ts$Amt=="Amt500mg" & ts$Analogue != "Butyr",], aes(x=YrMonthBi, y=median, linetype = Analogue)) +
  geom_line(size = 1) +
  ggtitle("500mg Price by Fentanyl Type
  Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Median Price") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  scale_linetype_manual(values=c("dashed", "solid", "dotted")) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)
ggsave("BW-500mg Plot.jpeg", device = "jpeg")

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
ggplot(ts[ts$Amt=="Amt1g" & ts$Analogue != "Butyr",], aes(x=YrMonthBi, y=median, linetype = Analogue)) +
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
ggsave("BW-1g Plot.jpeg", device = "jpeg")

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
ggplot(ts[ts$Amt=="Amt5g" & ts$Analogue != "Butyr",], aes(x=YrMonthBi, y=median, linetype = Analogue)) +
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
ggsave("BW-5g Plot.jpeg", device = "jpeg")

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
Event1 = Onymous, Event2 = Furanyl Release, Event3 = China Ban") +
  xlab("Date") + ylab("Median Price") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5) +
  scale_color_grey(start = 0, end = 0.6)
ggsave("BW-Lethal Doses.jpeg", device = "jpeg")

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

ggplot(ts[ts$Amt=="Amt100mg" & ts$Analogue != "Butyr",], aes(x=YrMonthBi, y=Composite, linetype = Analogue)) +
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
ggplot(tsComp[tsComp$Composite <2 & 
                tsComp$Analogue != "Citrate" & 
                tsComp$Analogue != "Furanyl" &
                tsComp$Analogue != "Butyr",], aes(x=YrMonthBi, y=Composite, linetype = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Composite Median Price -- Fentanyl and Acetyl
          Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Standard Deviations") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)
ggsave("BW-Composite Analogues1.jpeg", device = "jpeg")

## Add back in Citrate, reuse code from above.
## Inefficient code, but serving as spotcheck.
#ts <- cleangrams %>% group_by_(.dots=c("YrMonthBi", "Analogue", "Amt")) %>% 
#  summarize(mean=mean(dollarPrice), median=median(dollarPrice),
#            q25th=quantile(dollarPrice, 0.25),
#            q75th=quantile(dollarPrice, 0.75),
#            max=max(dollarPrice),min=min(dollarPrice),
#            vendors=length(unique(vendor_name)))
#ts$Onymous <- ts$YrMonthBi >= as.Date("2014-11-06")
#ts$AcetylSched1 <- ts$YrMonthBi >= as.Date("2015-05-21")
#ts$AcetylButyrChinaBan <- ts$YrMonthBi >= as.Date("2015-10-01")
#ts <- data.frame(ts)
#for (n in 1:nrow(ts)) {
#  tempAmt <- ts[[n,"Amt"]]
#  tempAnlg <- ts[[n,"Analogue"]]
#  ts[n,"Composite"] <- (ts[n,"median"] - mean(ts[ts$Amt == tempAmt & ts$Analogue == tempAnlg,"median"])) /
#    sd(ts[ts$Amt == tempAmt & ts$Analogue == tempAnlg,"median"])
#}
#tsComp <- ts %>% group_by_(.dots=c("YrMonthBi", "Analogue")) %>%
#  summarize(Composite = mean(Composite))
##
##


ggplot(tsComp[tsComp$Composite <2 & 
                tsComp$Analogue != "Acetyl" & 
                tsComp$Analogue != "Fentanyl",], aes(x=YrMonthBi, y=Composite, linetype = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Composite Median Price -- Furanyl, Butyr, and Citrate
          Event1 = Onymous, Event2 = US Acetyl Ban, Event3 = China Ban") +
  xlab("Date") + ylab("Standard Deviations") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2015-05-21")),
             color = "black", lwd = 1.5)
ggsave("BW-Composite Analogues2.jpeg", device = "jpeg")


#### Statistical Analysis ####
# Linear Model
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

library(memisc)
z <- mtable("Model1"=modelSimplest,"Model2"=modelSimpler,"Model3"=modelSimple,"Model4"=modelLog)
write.mtable(z, file = "ModelComparison.txt")
file.show("ModelComparison.txt")
## Run model again without Butyr fentanyl, 
## thinnest market at end of data set

modelts <- ts[ts$Analogue != "Butyr",]
modelts$YrMonthBi <- as.numeric(modelts$YrMonthBi - as.Date("2014-06-01"))
ModelLog <- modelts[,c("lnMedian", "YrMonthBi",
                       "Amt", "Analogue",
                       "Onymous", "PostAcetylSched1", "PostChinaBan",
                       "TrendOnym", "TrendSched1", "TrendChina")]

modelLog <- lm(lnMedian ~. + YrMonthBi*Analogue, data = ModelLog)
summary(modelLog)





#### CDC Graphs (Opioid and Synth) and Confusion Matrix ####

## Graphs derived from data from CDC Wonder database
OpioidDeaths <- read.csv("Deaths from All Opiates.csv")
SynthDeaths <- read.csv("Deaths Synthetics Excl Methadone.csv")
SynthStateDeaths <- read.csv("Synth Deaths by State.csv")

OpioidandSynth <- OpioidDeaths
OpioidandSynth$Synthetics <- SynthDeaths$Deaths
OpioidandSynth <- OpioidandSynth[,c(1,3,5)]
names(OpioidandSynth) <- c("Year", "All Opioids", "Synthetics")

OSMelt <- melt(OpioidandSynth, id = c("Year"))
names(OSMelt) <- c("Year", "Cause", "Deaths")


ggplot(OSMelt) + geom_line(aes(x = Year, y=Deaths, linetype = Cause), size = 2) +
  ggtitle("US Accidental Deaths from All Opioids vs Synthetics") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 40000), labels = comma,
                     breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000))

ggsave("BW-US Accidental Deaths All Opioids vs Synthetics.jpeg", device = "jpeg")



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
