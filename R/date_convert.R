date_convert = function(x){
  #case complete info (short periods)
  date = as.Date(x, format = "%A, %B %d, %Y")
  #case month and year (longer periods)
  if(is.na(date)){
    #case weeks
    y = gsub("\\\\u.*, ", "", x)
    y = gsub(",", "", y)
    date = as.Date(y, format = "%b %d %Y")
    #very long period (multiple years)
    if(is.na(date) | as.POSIXlt(date, tz = "UTC")$year + 1900 < 2000){
      y = paste("01", x)
      date = as.Date(y, format = "%d %B %Y")
    }
  }
  return(date)
}