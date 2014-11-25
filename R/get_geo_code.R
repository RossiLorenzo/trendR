get_geo_code = function(country, region){
  
  #If not option specified return all
  if(region == "none" & country == "all")
    return("all")
  
  #If region different from none filter by that
  if(region != "none"){
    my_code = filter(GT_Nations, sub_name == region)
    #Error not available region
    if(nrow(my_code) == 0)
      stop("Error: The selected region is not available")
    #Error not available country-region combination
    if(country != "all"){
      associated_country = unique(my_code$name)
      if(associated_country != country){
        stop(paste0("\nThe selected combination of country-region is wrong.\n",region," is in ",associated_country," not in ",country))
      }
    }
    #Not errors, return code
    return(paste(unique(my_code$id), unique(my_code$sub_id), sep = "-"))
  }
  
  my_code = GT_Nations %>% 
    filter(name == country) %>%
    select(id) %>%
    unique()
  
  return(as.character(my_code))
}