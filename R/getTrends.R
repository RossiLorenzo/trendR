############################################################## 
#' Get Trend data from Google
#'
#' This function scrapes Google Trends and returns the results nicely formatted in a R object
#' 
#' @param query The keyword to be searched. It will be auto escaped.
#' 
#' @param country Country name, i.e: United Kingdom, Italy, Germany, etc. Deafult option is the worldwide search.
#' 
#' @param region Region name, i.e: England, Scotland, etc. Deafult option is none. It is possible to use only the region name without specifying the related country.
#' 
#' @param date The time span. Default is all. Other options are: 'last 7 days', 'last 30 days', 'last 90 days', 'last year' or the year number, i.e '2014'
#' 
#' @param category The parent category of the keyword. Default is all. The options are listed in the data GT_Options$Category dataframe 
#' 
#' @param sub_category The child category of the keywrod. Default is none.
#' 
#' @note Please note that (except for the query argument) all the other arguments are case INSENSITIVE
#' 
#' @return The output is a list. This list contains the trend, the geographical segmentation (countries and cities), the suggested categories for the keyword and the related searches
#' 
#' @examples 
#' #Search the keyword 'test me' in England for last 30 days
#' results = getTrends(query = "test me", region = "England", date = "last 30 days")
#' 
#' #Access all available arguments
#' data(Google_Trends_Data)
#' #i.e: Check available categories
#' unique(GT_Options$Category$name)
#' 
#' @export

getTrends = function(query, country = "all", region = "none", date = "all", 
                     category = "all", sub_category = "none"){
  #Libraries
  library(httr, quietly = TRUE)
  library(RCurl, quietly = TRUE)
  library(XML, quietly = TRUE)
  library(stringr, quietly = TRUE)
  library(dplyr, quietly = TRUE)
  library(rjson, quietly = TRUE)
  data(Google_Trends_Data)
  
  #Check that country and region are correct and encode
  geo = get_geo_code(tolower(country), tolower(region))
  
  #Check that date is correct and encode
  date = get_date_code(tolower(date))  
  
  #Check that category is correct and encode
  cat = get_category_code(tolower(category), tolower(sub_category))
  
  #Scrape raw data
  html_results = get_googletrend_raw_data(query, geo, date, cat)
  parsed_results = content(html_results)
    #Get all scripts (where data live)
  all_scripts = getNodeSet(parsed_results, "//script")
  all_scripts = vapply(all_scripts, xmlValue, FUN.VALUE = character(1))
  
  #Chart data
  chartData = Filter(function(x) grepl("chartData", x), all_scripts)
  chartData = suppressWarnings(raw_to_df_chart(chartData))
  
  #Map data
  mapData = Filter(function(x) grepl("gvizGeoMap", x), all_scripts)
  mapData = suppressWarnings(raw_to_df_map(mapData))
  
  #Suggested categories
  suggested = raw_to_sug_cat(parsed_results)
  
  #Other search terms
  related = raw_to_rel_search(parsed_results)

  #Return output
  return(
    list(trend = chartData, 
         country = mapData$country, 
         city = mapData$city, 
         sug_cat = suggested,
         rel_searches = related))
}

