get_googletrend_raw_data = function(query, geo, date){
  #Session
  base_url = "http://www.google.co.uk/trends/fetchComponent?hl=en-US&tz&cmpt=q&content=1&cid=TIMESERIES_GRAPH_0&export=3"
  
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
#   message(paste0("Downloading data from: ",my_url))
  results = GET(my_url, add_headers("Cookie" = "PREF=Fake_Cookie"))
  html_results = content(results, "text")
  
  #Return raw results
  return(html_results)
}