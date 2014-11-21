raw_to_df = function(html_results){
  #Get info contanied in the plot itself (better formatted)
  all_info = str_extract_all(html_results, '"f":"[^"]*|null')[[1]]
  all_info = gsub('"f":"', '', all_info)
  all_info = lapply(all_info, function(x){
    value = NULL
    date = NULL
    #If is only numeric than it's the value
    if(nchar(gsub("[0-9]*|null", "", x)) == 0){
      if(x == "null")
        value = NA
      else
        value = as.numeric(x)
    }else{
        #case complete info (short periods)
        date = as.Date(x, format = "%A, %B %d, %Y")
        #case month and year (longer periods)
        if(is.na(date)){
          #case weeks
          y = gsub("\\\\u.*, ", "", x)
          y = gsub(",", "", y)
          date = as.Date(y, format = "%b %d %Y")
          #very long period (multiple years)
          if(is.na(date) | as.POSIXlt(date, tz = tz(date))$year + 1900 < 2000){
            y = paste("01", x)
            date = as.Date(y, format = "%d %B %Y")
          }
        }
#       }
    }
    return(list(value, date))
  })
  all_values = unlist(sapply(all_info, "[[", 1))
  all_dates = as.Date(unlist(sapply(all_info, "[[", 2)), origin = "1970-01-01")
  
  #Dataframe
  results = data.frame(Date = all_dates, Value = all_values, stringsAsFactors = FALSE)
  
  return(results)
}