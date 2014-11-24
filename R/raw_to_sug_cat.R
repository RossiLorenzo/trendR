raw_to_sug_cat = function(parsed_results){
  suggested = getNodeSet(parsed_results, "//div[@class = 'category-suggest-item']")
  suggested = vapply(suggested, xmlValue, FUN.VALUE = character(1))
  suggested = str_trim(gsub("\n", "", suggested))
  return(suggested)
}