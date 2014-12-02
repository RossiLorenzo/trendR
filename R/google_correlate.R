google_correlate = function(query, url, type = "monthly", shift = 0, country = "uk"){
  #Libraries
  library(RCurl)
  library(stringr)
  library(rjson)
  library(dplyr)
  library(XML)
  
  if(missing(url) & missing(query))
    stop("Please select eiher a query or a google correlate URL")
  
  if(missing(url)){
    #Base URL
    base_url = "http://www.google.com/trends/correlate/search?"
    
    #Creat URL from paramaters
    my_url = paste0(base_url, "e=", curlEscape(query))
    my_url = paste0(my_url, "&t=", curlEscape(type))
    my_url = paste0(my_url, "&shift=", curlEscape(shift))
    my_url = paste0(my_url, "&p=", curlEscape(country))
  }else{
    my_url = url
  }
  
  #Go to results page
  main_page = GET(my_url, add_headers("Cookie" = "PREF=Fake_Cookie"))
  
  #Extract data
  results = raw_to_df_correlate(main_page)
}

