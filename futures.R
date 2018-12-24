library(RQuantLib)
library(xtsExtra)
library(Quandl)
library(dplyr)
library(tibble)
library(lubridate)
library(TTR)

df <- read.csv("DCE_metadata.csv")
df <- read.csv("BITAL_metadata.csv")
row.names(df)[grep("Non-Finance", df[,"name"], ignore.case = TRUE)]
actives <- df[grepl("MIB Index Future", df[, "name"]) & df$from_date >= start_date & df$to_date <= end_date, c("code")]

monthlies <- function (fqcodes=fqcodes(2017, "DCE/JD", "FGJKNQVX"), cols="Open") {
    contract_data(fqcodes, cols)
}

quarterlies <- function (fqcodes=fqcodes(2017, "DCE/JD", "FGJKNQVX"), cols="Open") {
    contract_data(fqcodes, cols)
}

fqcode <- function(yyyy, prefix, band) {
    paste0(prefix, band, yyyy)
}

fqcodes <- function(yyyy, prefix, bands) {
    sapply(bands, function(x) { fqcode(yyyy, prefix, x) })
}

m2b <- function(month) {
    strsplit("FGHJKMNQUVXZ", "")[[1]][month]
}

codes_of_colnames <- function(names) {
    sapply(strsplit(names, "[/.]"), "[[", 2)
}

i2expiration <- function(code) {
    days_to_expiry(df, code, date)
    df <- read.csv("DCE_metadata.csv")
    as_date(df[grepl(instr, df[, "code"]),"to_date"])
}

# days to expiration versus volume quantile

days_to_expiry <- function(df, fqcode, dates) {
    ltd <- i2expiration(df, instr)
    businessDaysBetween("China", dates, ltd)
}

contract_data <- function (fqcodes, cols) {
    car <- substr(bands,1,1)
    cdr <- substr(bands,2,nchar(bands))
    result <- Quandl(fqcode(yyyy, prefix, car), type="xts")[,cols]
    colnames(result) <- paste(fqcode(yyyy, prefix, car), colnames(result), sep=".")
    for (c in strsplit(cdr, "")[[1]]) {
        subresult <- Quandl(fqcode(yyyy, prefix, c), type="xts")[,cols]
        colnames(subresult) <- paste(fqcode(yyyy, prefix, c), colnames(subresult),
                                     sep=".")
        result <- cbind(result, subresult)
    }
    result
}

plot(contract_data(2017, "DCE/JD", "Volume", "FGHJKMUVXZ"), screens=1, auto.legend=TRUE)
plot(contract_data(2017, "DCE/JD", c("Close", "Volume"), "FKU"), type=c("l","h"))
plot(cbind(ROC(contract_data(2017, "DCE/JD", c("Close"), "FKU")), contract_data(2017, "DCE/JD", c("Volume"), "FKU"))[,rep(1:3, each=2) + (0:1)*3], type=c("l","h"))

df[grepl("^TAIEX Futures,", df[, "name"]) & df$from_date >= start_date & df$to_date <= end_date, c("code", "name")]
# F G H J K M N Q U V X Z
# Eggs January May Aug Sep
start_date <- "2016-03-02"
end_date <- "2018-11-15"
front <- Quandl("TAIFEX/XIFV2018", type="xts", start_date=start_date, end_date=end_date)
interestColname <- colnames(front)[grep(pattern="Interest", colnames(front))]
front <- front[,c("Open","High","Low","Settle","Volume",interestColname)]
colnames(front) <- c("O","H","L","C","V","OI")
back <- Quandl("TAIFEX/XIFX2018", type="xts", start_date=start_date, end_date=end_date)
back <- back[,c("Open","High","Low","Settle","Volume",interestColname)]
colnames(back) <- c("BO","BH","BL","BS","BV","BI") #B for Back

start_date <- "2018-09-01"
end_date <- "2018-11-15"
front <- Quandl("TAIFEX/TXV2018", type="xts", start_date=start_date, end_date=end_date)
interestColname <- colnames(front)[grep(pattern="Interest", colnames(front))]
front <- front[,c("Open","High","Low","Settle","Volume",interestColname)]
colnames(front) <- c("O","H","L","C","V","OI")
back <- Quandl("TAIFEX/TXX2018", type="xts", start_date=start_date, end_date=end_date)
back <- back[,c("Open","High","Low","Settle","Volume",interestColname)]
colnames(back) <- c("BO","BH","BL","BC","BV","BI") #B for Back
both <- cbind(front,back)
both

start_date <- "2018-07-01"
end_date <- "2018-11-15"
front <- Quandl("TAIFEX/XIFV2018", type="xts", start_date=start_date, end_date=end_date)
front

start_date <- "2018-08-01"
end_date <- "2018-12-05"
back <- Quandl("TAIFEX/XIFX2018", type="xts", start_date=start_date, end_date=end_date)
back <- back[,c("Open","High","Low","Settle","Volume",interestColname)]
back
                                        #combine front and back for comparison
both <- cbind(front,back)
class(front)
inner_join(rownames_to_column(front), rownames_to_column(back), by="rowname")
merge(front, back, join='inner')[,make.names(c("Settle", paste("Settle", 1)))]

start_date <- "2018-09-01"
end_date <- "2018-11-15"
front <- Quandl("TAIFEX/TXV2018", type="xts", start_date=start_date, end_date=end_date)
interestColname <- colnames(front)[grep(pattern="Interest", colnames(front))]
back <- Quandl("TAIFEX/TXX2018", type="xts", start_date=start_date, end_date=end_date)
both <- cbind(front,back)
merge(front, back, join='inner')[,make.names(c("Settle", paste("Settle", 1)))]

Quandl("TAIFEX/GBFZ2018", type="xts", start_date=start_date, end_date=end_date)

# Generate all interesting dates
dates = as.Date(as.numeric(as.Date("2000-01-01")):as.numeric(as.Date("2050-01-01")))
 
# Get the end of month for each date
eom = getEndOfMonth("UnitedStates", dates)
 
# Get the last trading day, removing the last NA in the process
expi = head(eom[eom != lag.xts(eom, -1)], -1)
