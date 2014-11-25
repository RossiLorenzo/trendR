get_date_code = function(date){
  #Get right date
  right_date = filter(GT_Dates, name == date)
  if(nrow(right_date) == 0)
    stop(paste0("The selected timeframe is not available. Available options are:\n",paste(unique(GT_Dates$name),collapse=", ")))
  else
    return(right_date$id)
}