get_googletrend_raw_data = function(query, geo, date){
  #Session
  base_url = "http://www.google.com/trends/trendsReport?hl=en-US&tz=&content=1"
  
  #Create get request
    #Query  
    my_url = paste0(base_url, "&q=", curlEscape(query))
    #Date
    if(date != "all")
      my_url = paste0(my_url, "&date=", curlEscape(date))
    #Geo
    if(geo != "all")
      my_url = paste0(my_url, "&geo=", curlEscape(geo))
  
  #Go to results page
  main_page = GET(my_url, add_headers("Cookie" = "PREF=Fake_Cookie"))
  
  #Get all scripts (where data live)
  all_scripts = getNodeSet(content(main_page), "//script")
  all_scripts = vapply(all_scripts, xmlValue, FUN.VALUE = character(1))
  
  #Return raw results
  return(all_scripts)
}