############################################################## 
#' Get Trend data from Google
#'
#' This function scrapes Google Trends and returns the results nicely formatted in a R dataframe
#' 
#' @param query The keyword to be searched. It will be auto escaped.
#' 
#' @param country Country name, i.e: United Kingdom, Italy, Germany, etc. Case INSENSITIVE. Deafult option is the worldwide research.
#' 
#' @param region Region name, i.e: England, Scotland, etc. Case INSENSITIVE. Deafult option is none.
#' 
#' @param date The time span. Default is all. Other options are: 'last 7 days', 'last 30 days', 'last 90 days', 'last year' or the year number, i.e '2014'
#' 
#' @return data frame
#' 
#' @examples getTrends(query = "test me", country = "United Kingdom", region = "England", date = "last 30 days")
#' 
#' @export

getTrends = function(query, country = "all", region = "none", date = "all"){
  #Libraries
  library(httr, quietly = TRUE)
  library(RCurl, quietly = TRUE)
  library(XML, quietly = TRUE)
  library(stringr, quietly = TRUE)
  library(dplyr, quietly = TRUE)
  library(rjson, quietly = TRUE)
  data(Google_Trends_Data)
  
  #Make all lower case to reduce error rate
  country = tolower(country)
  region = tolower(region)
  date = tolower(date)
  
  #Check that country and region are correct and encode
  if(country != "all"){
    right_country = filter(GT_Nations, tolower(name) == country)
    if(nrow(right_country) == 0)
      stop("Error: The selected country is not available")
    if(region != "none"){
      right_country = filter(right_country, tolower(sub_name) == region)
      if(nrow(right_country) == 0)
        stop("Error: The selected region is not available")
      geo = paste(unique(right_country$id), unique(right_country$sub_id), sep = "-")
    }else{
      geo = unique(right_country$id)
    }
  }else{
    geo = "all"
  }
  
  #Check that date is correct and encode
  right_date = filter(GT_Dates, name == date)
  if(nrow(right_date) == 0)
    stop("Error: The selected timeframe is not available")
  else
    date = right_date$id
  
  #Get raw data
  html_results = get_googletrend_raw_data(query, geo, date)
  
  #Chart data
  chartData = Filter(function(x) grepl("chartData", x), html_results)
  chartData = suppressWarnings(raw_to_df_chart(chartData))
  
  #Map data
  mapData = Filter(function(x) grepl("gvizGeoMap", x), html_results)
  mapData = suppressWarnings(raw_to_df_map(mapData))
  
  #Return output
  return(list(trend = chartData, country = mapData$country, city = mapData$city))
}

