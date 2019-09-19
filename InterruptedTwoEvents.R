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
setwd("C:/Users/jacobmiller21/Desktop/Darknet Data")
grams <- read.csv(file = "CompiledGramsTotalFentanyl.csv", stringsAsFactors = FALSE)
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

## Clean out few listings with text as prices (not numbers)
grams <- grams[complete.cases(grams[,"dollarPrice"]),]
# Count number of vendors
length(unique(grams$vendor_name))
## Uniquely nail down using regex each of the doseage amounts. 
#Confusion matrix demonstrates high accuracy.

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

## Add in time variable with zero at the beginning of the dataset
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
cleangrams <- cleangrams[(cleangrams$Amt100mg | cleangrams$Amt250mg | cleangrams$Amt250mg | cleangrams$Amt500mg | cleangrams$Amt1g | cleangrams$Amt5g),]

## Screen out fat finger listings (looks like confused BTC with dollar prices)
cleangrams <- cleangrams[cleangrams$dollarPrice < 10000,]

## There is a fentanyl lottery without a clear product
cleangrams <- cleangrams[(cleangrams$Amt100mg + cleangrams$Amt200mg + cleangrams$Amt250mg + 
                            cleangrams$Amt500mg + cleangrams$Amt1g + cleangrams$Amt5g <2),]

## Add two events of interest
cleangrams$Onymous <- cleangrams$Date >= as.Date("2014-11-06")
cleangrams$ChinaBan <- cleangrams$Date >= as.Date("2015-10-01")

## Add names for amounts and analogues to treat as factors as needed

colAmts <- grepl(pattern = "Amt[[:digit:]]", x = colnames(cleangrams))
cleangrams$Amt <- colnames(cleangrams[,colAmts])[max.col(cleangrams[,colAmts])]

colMethod <- grepl(pattern = "Method", x = colnames(cleangrams))
cleangrams$Method <- colnames(cleangrams[,colMethod])[max.col(cleangrams[,colMethod])]

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


## Add in 10 day increments around events of interest
cleangrams$TrendOnym <- as.numeric(cleangrams$Date) - as.numeric(as.Date("2014-11-05"))
cleangrams$TrendOnym <- round_any(cleangrams$TrendOnym, 10, f = ceiling)
cleangrams$TrendChin <- as.numeric(cleangrams$Date) - as.numeric(as.Date("2015-10-01"))
cleangrams$TrendChin <- round_any(cleangrams$TrendChin, 10, f = ceiling)


## Add in 15 day increments for graphing
for (x in 1:nrow(cleangrams)) {
  cleangrams[x,"YrMonthBi"] <- paste(year(cleangrams[x,"Date"]),
                                          month(cleangrams[x,"Date"]),
                                          if (day(cleangrams[x,"Date"]) >=16) {
                                            "15"
                                          } else {
                                            "01"
                                          }, sep = "-")
}
cleangrams$YrMonthBi <- as.Date(cleangrams$YrMonthBi)

length(unique(cleangrams[cleangrams$NameFuranyl,"vendor_name"]))
length(unique(cleangrams[cleangrams$NameButyr,"vendor_name"]))
length(unique(cleangrams[cleangrams$NameAcetyl,"vendor_name"]))
cleangrams %>% group_by(Analogue) %>%
  summarise(number = length(unique(vendor_name)))

#### Onymous Analysis ####
# DateNum variable is days since 2014-06-09
# Onymous took place on 2014-11-06 day 150
# China ban took place on 2015-01-10 day 479

## Remove listings that appear more than once during the same 10 day period
DataOnym <- cleangrams[!duplicated(cleangrams[,c("vendor_name", "market_name", "name", "TrendOnym")]),]
DataOnym <- DataOnym[,c("dollarPrice", "DateNum","Analogue","Amt","Onymous", "TrendOnym")]
# Remove lollipops (very few listings and categorically different than pure
# fentanyl being sold)
DataOnym <- DataOnym[DataOnym$Analogue != "Citrate",]
# Set Trend Variables and capture interested period (+/- 90 days)
# (add one to window for bracketing within Stata to complete full 90 days)
# Allows easy adjustment of window and randomization for EventDate
# for robustness checks
window <- 121
EventDate <- 150
# Shift to day before because of bracketing in Stata for binscatter
DataOnym$Trend <- DataOnym$TrendOnym - 1
DataOnym$OnymousTrend <- DataOnym$Trend * DataOnym$Onymous
DataOnym <- DataOnym[c("dollarPrice", "Trend", "Onymous", 
                       "OnymousTrend", "Analogue", "Amt")]
DataOnym <- DataOnym[(DataOnym$Trend >= (0 - window)) &
                       (DataOnym$Trend <= (0 + window)),]
# Convert to log form for dollar price
DataOnym$dollarPrice <- log(DataOnym$dollarPrice)
colnames(DataOnym)[1] <- "LogDollarPrice"
# Convert for stata
DataOnym$Onymous <- ifelse(DataOnym$Onymous, 1, 0)

# DataOnym <- read.csv("DataOnymousFentanyl.csv")
Onym1 <- lm(LogDollarPrice ~ Trend + Onymous + OnymousTrend + Analogue + Amt,
   data = DataOnym)
summary(Onym1)
#write.csv(DataOnym, "DataOnymousFentanyl.csv", row.names = F)



#### China Regulation Analysis ####
# DateNum variable is days since 2014-06-09
# Onymous took place on 2014-11-06 day 150
# China regulation took place on 2015-01-10 day 479


## Remove listings that appear more than once during the same 10 day period
DataChina <- cleangrams[!duplicated(cleangrams[,c("vendor_name", "market_name", "name", "TrendChin")]),]
DataChina <- DataChina[,c("dollarPrice", "DateNum","Analogue","Amt","ChinaBan", "TrendChin")]
# Remove lollipops (very few listings and categorically different than pure
# fentanyl being sold)
DataChina <- DataChina[DataChina$Analogue != "Citrate",]
# Set Trend Variables and capture interested period (+/- 120 days)
# (add one to window for bracketing within Stata to complete full 120 days)
# Allows easy adjustment of window and randomization for EventDate
# for robustness checks
window <- 121
EventDate <- 479

DataChina$Trend <- DataChina$TrendChin - 1
DataChina$ChinaTrend <- DataChina$Trend * DataChina$ChinaBan
DataChina <- DataChina[c("dollarPrice","Trend","ChinaBan",
                         "ChinaTrend","Analogue","Amt")]

DataChina <- DataChina[(DataChina$Trend >= (0 - window)) &
                       (DataChina$Trend <= (0 + window)),]
# Convert to log form for dollar price
DataChina$dollarPrice <- log(DataChina$dollarPrice)
colnames(DataChina)[1] <- "LogDollarPrice"
# Convert boolean to numeric for stata
DataChina$ChinaBan <- ifelse(DataChina$ChinaBan, 1, 0)

#DataChina <- read.csv("DataChinaFentanyl.csv")
China1 <- lm(LogDollarPrice ~ Trend + ChinaBan + ChinaTrend + Analogue + Amt,
            data = DataChina)
summary(China1)
China2 <- lm(LogDollarPrice ~ Trend + ChinaBan + ChinaTrend + 
               Analogue + Amt + ChinaBan * Analogue + ChinaTrend * Analogue,
             data = DataChina)
summary(China2)
China3 <- lm(LogDollarPrice ~ Trend + ChinaBan + ChinaTrend + 
               Analogue + Amt + ChinaBan * Amt + ChinaTrend * Amt,
             data = DataChina)
summary(China3)


#write.csv(DataChina, "DataChinaFentanyl.csv", row.names = F)


#### Robustness check ####
# DateNum variable is days since 2014-06-09
# Onymous took place on 2014-11-06 day 150
# China ban took place on 2015-01-10 day 479

# Events set between 280 and 360 would not contain either of 
# the two events of interest

## Remove listings that appear more than once during the same 10 day period
DataRobust <- cleangrams[!duplicated(cleangrams[,c("vendor_name", "market_name", "name", "TrendChin")]),]
DataRobust <- DataRobust[,c("dollarPrice", "DateNum","Analogue","Amt","ChinaBan", "TrendChin")]
# Remove lollipops (very few listings and categorically different than pure
# fentanyl being sold)
DataRobust <- DataRobust[DataRobust$Analogue != "Citrate",]
# Set Trend Variables and capture interested period (+/- 120 days)
# (add one to window for bracketing within Stata to complete full 120 days)
# Allows easy adjustment of window and randomization for EventDate
# for robustness checks

df <- data.frame(Event = double(), TrendEvent = double())
for (i in seq(from = 280, to = 360, by = 10)) {
  window <- 121
  EventDate <- i
  DataRobust1 <- DataRobust
  # Reset to looped date
  DataRobust1$Trend <- DataRobust1$TrendChin - 1 + 479 - i
  colnames(DataRobust1)[5] <- "Event"
  DataRobust1$Event <- DataRobust1$Trend > 0
  DataRobust1$EventTrend <- DataRobust1$Trend * DataRobust1$Event
  DataRobust1 <- DataRobust1[c("dollarPrice","Trend","Event",
                           "EventTrend","Analogue","Amt")]

  DataRobust1 <- DataRobust1[(DataRobust1$Trend >= (0 - window)) &
                           (DataRobust1$Trend <= (0 + window)),]
  # Convert to log form for dollar price
  DataRobust1$dollarPrice <- log(DataRobust1$dollarPrice)
  colnames(DataRobust1)[1] <- "LogDollarPrice"
  # Convert boolean to numeric for stata
  DataRobust1$Event <- ifelse(DataRobust1$Event, 1, 0)

  #DataRobust1 <- read.csv("DataRobustFentanyl1.csv")
  Robust1 <- lm(LogDollarPrice ~ Trend + Event + EventTrend + Analogue + Amt,
               data = DataRobust1)
  df <- rbind(df, data.frame(Event = summary(Robust1)$coefficients[3,4], 
                             TrendEvent = summary(Robust1)$coefficients[4,4]))
}

#write.csv(DataRobust1, "DataRobustFentanyl1.csv", row.names = F)







#### Exploratory Graphs ####

cg15 <- cleangrams[!duplicated(cleangrams[,c("vendor_name", "market_name", "name", "YrMonthBi")]),]

cg15 <- cg15[!(grepl(pattern = "mannitol inositol", x = cg15$LowerDesc, fixed = TRUE) |
      grepl(pattern = "mannitol mix", x = cg15$LowerDesc, fixed = TRUE)),]


ts <- cg15 %>% group_by(YrMonthBi, Analogue, Amt) %>% 
  summarise(mean=mean(dollarPrice), median=median(dollarPrice),
            q25th=quantile(dollarPrice, 0.25),
            q75th=quantile(dollarPrice, 0.75),
            max=max(dollarPrice),min=min(dollarPrice),
            vendors=length(unique(vendor_name)))

ts$Onymous <- ts$YrMonthBi >= as.Date("2014-11-06")
ts$AcetylButyrChinaBan <- ts$YrMonthBi >= as.Date("2015-10-01")
ts$Analogue <- factor(ts$Analogue, 
                      levels=c("Fentanyl", "Acetyl", "Furanyl", "Citrate", "Butyr"))


tsMethod <- cg15 %>% group_by(YrMonthBi, Method) %>% 
  summarise(mean=mean(dollarPrice), median=median(dollarPrice),
            q25th=quantile(dollarPrice, 0.25),
            q75th=quantile(dollarPrice, 0.75),
            max=max(dollarPrice),min=min(dollarPrice),
            vendors=length(unique(vendor_name)))
tsAnalogue <- cg15 %>% group_by(YrMonthBi, Analogue) %>% 
  summarise(mean=mean(dollarPrice), median=median(dollarPrice),
            q25th=quantile(dollarPrice, 0.25),
            q75th=quantile(dollarPrice, 0.75),
            max=max(dollarPrice),min=min(dollarPrice),
            vendors=length(unique(vendor_name)))

tsAmt <- cg15 %>% group_by(YrMonthBi, Amt) %>% 
  summarise(mean=mean(dollarPrice), median=median(dollarPrice),
            q25th=quantile(dollarPrice, 0.25),
            q75th=quantile(dollarPrice, 0.75),
            max=max(dollarPrice),min=min(dollarPrice),
            vendors=length(unique(vendor_name)))


## Total Vendors for Popular Amounts

tvendors <- cg15 %>% group_by(YrMonthBi, Analogue) %>% 
  summarise(vendors=length(unique(vendor_name)))

## Arrange order of analogues for potential legends
tvendors$Analogue <- factor(tvendors$Analogue, 
                            levels=c("Fentanyl", "Acetyl", "Furanyl", "Butyr", "Citrate"))


ggplot(tvendors[tvendors$Analogue!="Citrate",], aes(x=YrMonthBi, y=vendors, linetype = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Unique Vendors for Fentanyl and Popular Analogues
          Event1 = Onymous, Event2 = China Ban") +
  xlab("Date") + ylab("Number of Vendors") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5) +
  geom_vline(xintercept=as.numeric(as.Date("2014-11-06")),
             color = "black", lwd = 1.5)
#ggsave("BW-Unique Vendors1.jpeg", device = "jpeg")


ggplot(tvendors[tvendors$Analogue != "Citrate" & 
                  tvendors$Analogue != "Butyr" &
                  tvendors$Analogue != "Fentanyl" &
                  tvendors$YrMonthBi >= as.Date("2015-07-31") &
                  tvendors$YrMonthBi <= as.Date("2015-12-10"),], 
       aes(x=YrMonthBi, y=vendors, linetype = Analogue)) +
  geom_line(size = 1) +
  ggtitle("Unique Vendors for Acetyl and Furanyl Fentanyl
          Event = China Acetyl Fentanyl Ban") +
  xlab("Date") + ylab("Number of Unique Vendors Across Markets") +
  ylim(0,15) +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  geom_vline(xintercept=as.numeric(as.Date("2015-10-01")),
             color = "black", lwd = 1.5)
#ggsave("BW-China Acetyl Furanyl.jpeg", device = "jpeg", height = 3, width = 5)



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
#ggsave("BW-Unique Vendors2.jpeg", device = "jpeg")


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
#ggsave("BW-100mg Plot.jpeg", device = "jpeg")

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
#ggsave("BW-250mg Plot.jpeg", device = "jpeg")

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
#ggsave("BW-500mg Plot.jpeg", device = "jpeg")

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

## Lethal dose tables
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

ggplot(ld[ld$Analogue=="Fentanyl",], aes(x=YrMonthBi, y=median, linetype = Amt)) +
  geom_line(size = 1.3) +
  ggtitle("Cost of Lethal Dose of Illicit Fentanyl") +
  labs(subtitle = "(Measured in USD, Non-Analogue Fentanyl, Select Amounts)") +
  xlab("Date") + ylab("Median Price Per Lethal Dose") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +
  theme(plot.subtitle=element_text(size=10, hjust=0.5, face="italic", color="black")) +
  scale_color_grey(start = 0, end = 0.6)
ggsave("BW-Lethal Doses.jpeg", device = "jpeg", width = 5, height = 3)

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
