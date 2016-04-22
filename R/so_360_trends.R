############################################################## 
#' Get data from SO 360 (Chinese Search Engine)
#'
#' This function scrapes 360 Trends and returns the results nicely formatted into a R object
#' 
#' @param query The keyword to be searched.
#' 
#' @return The output is a dataframe containing Date and Value variables for all the queries
#' 
#' @examples 
#' #Search the keyword 'tokyo and japan'
#' results = so_360_trends("洛杉矶酒店")
#' 
#' #Visualize results
#' ggplot(results, aes(x = Date, y = Value, col = Query)) + geom_line()
#' 
#' @export

so_360_trends = function(query){
  library(httr)
  library(dplyr)
  
  # Check that length of query is between 1 and 5
  if(length(query) == 0)
    stop("Please specify one query")
  if(length(query) > 1)
    stop("Maximum number of querys allowed is 1")
  
  # For both of them download the data
  all_data = lapply(c("Index", "Media"), function(type){
    # Create URL
    url = paste0("http://index.so.com/index.php?a=so", type, "Json&q=", query)
    # Get the data
    get_url = GET(url)
    if(get_url$status_code != 200)
      stop(paste0("get request failed with code ",get_url$status_code))
    content_index = content(get_url, type = "application/json")
    # Return empty if status is NULL
    if(content_index$status == -1)
      return(NULL)
    # Parse it 
    dates = seq(as.Date(content_index$data$period[[1]]), as.Date(content_index$data$period[[2]]), "day")
    values = as.numeric(strsplit(content_index$data[[tolower(type)]][[1]], "|", fixed = TRUE)[[1]])
    # Return data_frame
    tmp = data_frame(Date = as.Date(dates), Query = query, value = values)
    names(tmp)[3] = type
    return(tmp)
  })
  
  # Return results
  suppressWarnings(Reduce(left_join, Filter(function(x){ !is.null(x)}, all_data)))
}