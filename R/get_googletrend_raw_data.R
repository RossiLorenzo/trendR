get_googletrend_raw_data = function(query, geo, date, cat){
  #Session
  base_url = "http://www.google.com/trends/trendsReport?hl=en-US&tz=&content=1"
  
#   modify_url(base_url, query = list(
#     date = date,
#     geo = geo,
#     cat = cat
#   ))
  
  #Create get request
    #Query  
    my_url = paste0(base_url, "&q=", curlEscape(query))
    #Date
    if(date != "all")
      my_url = paste0(my_url, "&date=", curlEscape(date))
    #Geo
    if(geo != "all")
      my_url = paste0(my_url, "&geo=", curlEscape(geo))
    #Category
    if(cat != "all")
      my_url = paste0(my_url, "&cat=", curlEscape(cat))
  
  #Go to results page
  main_page = GET(my_url)
  
  x = content(main_page)
  htmlTreeParse(x)
  str(x, max.level = 1)
  
  #Return raw results
  return(main_page)
}