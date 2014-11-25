raw_to_rel_search = function(parsed_results){
  related = getNodeSet(parsed_results, "//div[@class = 'with-related-topics']//tr[contains(@class, 'trends-table-row ')]")
  related = vapply(related, xmlValue, FUN.VALUE = character(1))
  related = str_trim(gsub("\n", "", related))
  related = gsub(" {2,}", " ", related)
  if(length(related) == 0)
    return(NULL)
  related = lapply(1:length(related), function(i){
    my_row = related[i]
    last_string = gsub(".* ", "", my_row)
    #set type
    type = "Top"
    if(grepl("%", last_string) | last_string == "Breakout")
      type = "Rising"
    #create dataframe
    kwd = str_trim(gsub("\\+", "", gsub(last_string, "", my_row)))
    return(data.frame(keyword = kwd, value = last_string, type = type, stringsAsFactors = FALSE))
  })
  related = rbind_all(related)
  related = data.frame(related, stringsAsFactors = FALSE)
  return(related)
}