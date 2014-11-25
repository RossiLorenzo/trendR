get_category_code = function(category, sub_category){
  GT_Categories = GT_Options$Category
  
  #If not options specified return all
  if(sub_category == "none" & category == "all")
    return("all")
  
  #If sub_category different from none filter by that
  if(sub_category != "none"){
    my_code = filter(GT_Categories, sub_name == sub_category)
    #Error not available sub_category
    if(nrow(my_code) == 0)
      stop("Error: The selected sub_category is not available")
    #Error not available category-sub_category combination
    if(category != "all"){
      associated_category = unique(my_code$name)
      if(associated_category != category){
        stop(paste0("\nThe selected combination of category-sub_category is wrong.\n",sub_category," is in ",associated_category," not in ",category))
      }
    }
    #Not errors, return code
    return(paste("0",unique(my_code$id), unique(my_code$sub_id), sep = "-"))
  }
  
  my_code = GT_Categories %>% 
    filter(name == category) %>%
    select(id) %>%
    unique()
  
  return(as.character(my_code))
}