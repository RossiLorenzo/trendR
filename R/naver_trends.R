############################################################## 
#' Get data from Naver Trends
#'
#' This function scrapes Naver Trends and returns the results nicely formatted into a R object
#' 
#' @param query The keyword to be searched. It will be auto escaped. This can be a vector with up to 5 queries
#' 
#' @return The output is a dataframe containing Date and Value variables for all the queries
#' 
#' @examples 
#' #Search the keyword 'tokyo and japan'
#' results = naver_trends(c("tokyo", "japan"))
#' 
#' #Visualize results
#' ggplot(results, aes(x = Date, y = Value, col = Query)) + geom_line()
#' 
#' @export

naver_trends = function(query){
  library(httr)
  library(stringi)
  library(dplyr)
  library(tidyr)
  
  # Check that length of query is between 1 and 5
  if(length(query) == 0)
    stop("Please specify at least one query")
  if(length(query) > 5)
    stop("Maximum number of querys allowed is 5")
  names(query) = paste0("query", seq_along(query))
  
  # Rest of list
  rest_list = list(
    startDate = "201301",
    endDate = "201510")
  
  # Start and end date
  url = modify_url("http://trend.naver.com/trend.naver?", 
             query = append(rest_list, query))
  
  # Get the page
  get_url = GET(url)
  if(get_url$status_code != 200)
    stop(paste0("get request failed with code ",get_url$status_code))
  content_url = as(content(get_url), "character")
  
  # Extract the time series
  time_series = stri_extract_all(content_url, regex = paste0("\\{ name:[^\\}]*"))[[1]]
  
  # For all the time series create the dataframe
  all_res = lapply(time_series, function(i){
    # Get all dates and values
    dates_values = stri_extract_all(i, regex = "Date\\.UTC\\([0-9\\,]*\\)\\,[0-9]*\\]")[[1]]
    res = data_frame(Date = gsub("\\).*", "", gsub(".*\\(", "", dates_values)), Value = as.numeric(gsub("\\]", "", gsub(".*\\,", "", dates_values)))) %>% 
      separate(Date, c("Year", "Month", "Day"), sep = ",") %>% 
      mutate(
        Year = as.numeric(Year),
        Month = as.numeric(Month) + 1,
        Day = as.numeric(Day),
        Date = as.Date(paste(Year, Month, Day, sep = "-")),
        Query = gsub("\\'.*", "", gsub(".*name\\:\\'", "", i)))
  })
  all_res = rbind_all(all_res)

  # Return the results
  return(all_res)
}