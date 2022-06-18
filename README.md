ST558 - Project 1
================
Bryan Bittner
2022-06-17

# Financial Data Vignette

You might be asking yourself, what exactly is a vignette. Well let me
save you the hastle of a google search and answer that question here. A
vignette is defined as “a brief evocative description, account, or
episode”. Now you might ask yourself what evocative means. Rather then
get caught in an endless loop, let use an easier to understand
definition of a vignette. In this context a vignette is just a short,
informative guide for how to use and deploy a lookup on financial data.

This vignette will be centered around a call to an API. The API I chose
was created by Polygon.io and it is centered around financial data,
specifically stock prices.

Below is a list of packages that were used in the creation of this
vignette. The tidyverse and knitr packages are commonly used r packages
with a wide variety of uses. The httr and jsonlite packages will be used
to call an API an display the results. The remaining packages are lesser
known r packages to be used specifically for stock related functions.

The advantage of using an API in code is that we can customize
function(s) and have them automatically run and retrieve updated
results. This beats manually going out to a website to check the stock
ticker.

This program will allow for two separate API calls to retrieve financial
information. The first option called “Daily” will give the stock
readings for a specific stock ticker on a specific date. The second
option is called “Aggregate” and will allow you to pull back the
readings for a specific stock over a date range with different timespan
and multipliers. We can get into more detail for this below.

Daily Option The two required field to use this option are “Stock
Ticker” and “Date”. When the function is called, it will first check to
ensure that the call was valid and worked. If a problem occurred, the
status and error message will be presented. If the call was successful
the following values will be returned: open, high, low, close, volume

``` r
isValidSymbol <- function(symbolName){
  #Checks to make sure the stock ticker name is value
  print("Running isValidSymbol function")
  
  #Check to see if the function has already been called, if so, skip over the stockSymbols call to improve performance
  #if (exists('symbolsDF')==FALSE && is.data.frame(get('symbolsDF'))){
    symbolsDF <- stockSymbols("NASDAQ")  
  #}
  
  symbolsDFFiltered <- filter(symbolsDF, Symbol == toupper(symbolName))
  
  if (count(symbolsDFFiltered) > 0){
    return(TRUE)
  }
  
  return(FALSE)
}
```

``` r
isValidDate <- function(DateToCheck){
  #Dates in these API lookup must be a valid date and in the YYYY-MM-DD format
  
  print("Running isValidDate function")
  d <- try(as.Date(DateToCheck, format="%Y-%m-%d"))
  if("try-error" %in% class(d) || is.na(d)) {
    return(FALSE)
  }
  return(TRUE)
}
```

``` r
stockDailyLookup <- function(symbolName, lookupDate, printSummary=TRUE){
  
  #First lets make sure the symbol is valid
  if (isValidSymbol(symbolName)==FALSE){
    return(list("success"=FALSE,"resultsMessage"="Symbol not found"))
  }
  
  #Make sure date is valid and in the YYYY-MM-DD format
  if(isValidDate(lookupDate)==FALSE) {
    return(list("success"=FALSE,"resultsMessage"="Invalid Date"))
  }

  #Past the initial validation checks  - Call the API
  apiLookup<-paste("https://api.polygon.io/v1/open-close/",symbolName,"/",lookupDate,"?adjusted=true&apiKey=",myAPIKey,sep="")
  
  apiDailyData <-GET(apiLookup)
  apiDailyDataParsed <- fromJSON(rawToChar(apiDailyData$content))
  #print(apiDailyDataParsed)

  if (apiDailyDataParsed$status=="NOT_FOUND"){
    return(list("success"=FALSE,"resultsMessage"="Unknown error occurred. Date might be on a weekend or holiday"))
  }
  
  if (apiDailyDataParsed$status=="ERROR"){
    return(list("success"=FALSE,"resultsMessage"=apiDailyDataParsed$error))
  }
  
  if (printSummary==TRUE) {
    #print(paste("Summary for '",symbolName," - ",symbolsDFFiltered$Name, "'", sep=""))
    print(paste("Lookup Date: ",lookupDate,sep=""))
    print(paste("Opening Price: $",apiDailyDataParsed$open,sep=""))
    print(paste("Closing Price: $",apiDailyDataParsed$close,sep=""))
    print(paste("Daily High: $",apiDailyDataParsed$high,sep=""))
    print(paste("Daily Low: $",apiDailyDataParsed$low,sep=""))    
  }

  return(list("success"=TRUE,"resultsMessage"="succes!",apiDailyDataParsed))
}
```

``` r
dailyResults<-stockDailyLookup("AAPL","2022-06-01",TRUE)
```

    ## [1] "Running isValidSymbol function"

    ## Fetching NASDAQ symbols...

    ## [1] "Running isValidDate function"
    ## [1] "Lookup Date: 2022-06-01"
    ## [1] "Opening Price: $149.9"
    ## [1] "Closing Price: $148.71"
    ## [1] "Daily High: $151.74"
    ## [1] "Daily Low: $147.68"

Pulling data for a specific company on a single day is great and
everything, but it doesn’t tell us any trends. What if we wanted to pull
some history to see if the stock has been going up or down. For that we
will need another API. Enter the ‘Aggregate’ API lookup.

This has some additional parameters that will need to be defined and
validated StockTicker - The ticker symbol of the stock/equity.
DateFrom - The start of the aggregate time window with the format
YYYY-MM-DD DateTo - The end of the aggregate time window with the format
YYYY-MM-DD Multiplier - The size of the timespan multiplier. timespan -
The size of the time window - valid options
(‘minute’,‘hour’,‘day’,‘week’,‘month’,‘quarter’,‘year’)

``` r
isValidTimespan <- function(timespanToCheck){
  #Make sure the timespan is valid
  
  timespanToCheck<-tolower(timespanToCheck)
  
  if (timespanToCheck %in% "minute" || timespanToCheck %in% "hour" || timespanToCheck %in% "day"
      || timespanToCheck %in% "week" || timespanToCheck %in% "month" || timespanToCheck %in% "quarter"
      || timespanToCheck %in% "year") {
    return(TRUE)
  }

  return(FALSE)
}
```

``` r
stockAggregateLookup <- function(symbolName, lookupDateFrom, lookupDateTo, multiplier, timespan, printSummary=TRUE){
  
  #First lets make sure the symbol is valid
  if (isValidSymbol(symbolName)==FALSE){
    return(list("success"=FALSE,"resultsMessage"="Symbol not found"))
  }
  
  #Make sure date from is valid and in the YYYY-MM-DD format
  if(isValidDate(lookupDateFrom)==FALSE) {
    return(list("success"=FALSE,"resultsMessage"="Invalid Date From"))
  }
  
  #Make sure date to is valid and in the YYYY-MM-DD format
  if(isValidDate(lookupDateFrom)==FALSE) {
    return(list("success"=FALSE,"resultsMessage"="Invalid Date To"))
  }
  
  #Make sure timespan is valid
  if(isValidTimespan(timespan)==FALSE) {
    return(list("success"=FALSE,"resultsMessage"="Invalid Timespan"))
  }

  #Past the initial validation checks  - Call the API
  apiLookup<-paste("https://api.polygon.io/v2/aggs/ticker/",symbolName,"/range/",multiplier,"/",timespan,"/",lookupDateFrom,"/",lookupDateTo,"?adjusted=true&sort=asc&limit=1000&apiKey=",myAPIKey,sep="")
  
  apiAggData <-GET(apiLookup)
  apiAggDataParsed <- fromJSON(rawToChar(apiAggData$content))
  print(max(apiAggDataParsed$results$h))

  if (apiAggDataParsed$status=="NOT_FOUND"){
    return(list("success"=FALSE,"resultsMessage"="Unknown error occurred. Date might be on a weekend or holiday"))
  }
  
  if (apiAggDataParsed$status=="ERROR"){
    return(list("success"=FALSE,"resultsMessage"=apiAggDataParsed$error))
  }
  
  if (apiAggDataParsed$status!="OK"){
    return(list("success"=FALSE,"resultsMessage"="Unknown Error"))
  }
  
  #Convert results timestamp field to readable Date
  apiAggDataParsed$results<-apiAggDataParsed$results %>% mutate(tDate = as.POSIXct(apiAggDataParsed$results$t/1000, origin="1970-01-01"))
  
  if (printSummary==TRUE) {
    #print(paste("Summary for '",symbolName," - ",symbolsDFFiltered$Name, "'", sep=""))
    print(paste("Lookup Date From: ",lookupDateFrom,sep=""))
    print(paste("Lookup Date To: ",lookupDateTo,sep=""))
    print(paste("Number of records returned: ",apiAggDataParsed$resultsCount,sep=""))
    print(paste("Opening Price: $",apiAggDataParsed$results$o[1],sep=""))
    print(paste("Closing Price: $",apiAggDataParsed$results$c[apiAggDataParsed$resultsCount],sep=""))
    print(paste("Max Daily High: $",max(apiAggDataParsed$results$h),sep=""))
    print(paste("Min Daily Low: $",min(apiAggDataParsed$results$l),sep=""))
    print(paste("Date of Max: ",apiAggDataParsed$results$tDate[which.max(apiAggDataParsed$results$h)],sep=""))
    print(paste("Date of Low: ",apiAggDataParsed$results$tDate[which.min(apiAggDataParsed$results$l)],sep=""))
  }

  #return(list("success"=TRUE,"resultsMessage"="succes!",apiAggDataParsed))
}
```

``` r
stockAggregateLookup(symbolName="AAPL",lookupDateFrom="2022-06-01",lookupDateTo="2022-06-16",multiplier = 1,timespan="day",printSummary=TRUE)
```

    ## [1] "Running isValidSymbol function"

    ## Fetching NASDAQ symbols...

    ## [1] "Running isValidDate function"
    ## [1] "Running isValidDate function"
    ## [1] 151.74
    ## [1] "Lookup Date From: 2022-06-01"
    ## [1] "Lookup Date To: 2022-06-16"
    ## [1] "Number of records returned: 12"
    ## [1] "Opening Price: $149.9"
    ## [1] "Closing Price: $130.06"
    ## [1] "Max Daily High: $151.74"
    ## [1] "Min Daily Low: $129.04"
    ## [1] "Date of Max: 2022-05-31 23:00:00"
    ## [1] "Date of Low: 2022-06-15 23:00:00"

``` r
#getSymbols("AAPL", src="yahoo")
##TA=NULL to remove the trade volumn
#chartSeries(AAPL, subset="last 1 months", theme=chartTheme("white"),TA=NULL)
```

``` r
#multiple_stocks <- tq_get(c("AMZN", "AAPL"),
#                          get = "stock.prices",
#                          from = "2022-06-01",
#                          to = "2022-06-17")
#multiple_stocks
```

``` r
#ggplot(data = filter(multiple_stocks, symbol == "AAPL" || symbol == "AMZN"),
#       aes(x=date, color=symbol)) + geom_line(aes(x=date, y=open, color=symbol))
```
