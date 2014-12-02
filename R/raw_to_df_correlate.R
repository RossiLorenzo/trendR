raw_to_df_correlate = function(html_page){
  #All correlated (except first one)
  parsed_page = content(html_page)
  all_values = getNodeSet(parsed_page, "//li[@class = 'result']//small")
  all_values = unlist(xmlApply(all_values, xmlValue))
  all_names = getNodeSet(parsed_page, "//li[@class = 'result']//a")
  all_urls = paste0("http://www.google.com", unlist(xmlApply(all_names, xmlGetAttr, "href")))
  all_names = unlist(xmlApply(all_names, xmlValue))
  all_correlations = data.frame(name = all_names, correlation = all_values, url = all_urls, stringsAsFactors = FALSE)
  
  #Get data for active correlated time series (first one)
  text_page = content(html_page, "text")
  data = gsub('.*series": ', "", text_page)
  data = gsub(";.*", "", data)
  data = fromJSON(data)
  
  #First series: Query
  series1_name = data[[1]][1]
  series1 = lapply(data[[1]][-1][[1]], function(x){
    return(data.frame(date = x$date, value = x$value, stringsAsFactors = FALSE))
  })
  series1 = rbind_all(series1)
  colnames(series1)[2] = series1_name
  
  #Second series: Correlated
  series2_name = data[[2]][1]
  series2 = lapply(data[[2]][-1][[1]], function(x){
    return(data.frame(date = x$date, value = x$value, stringsAsFactors = FALSE))
  })
  series2 = rbind_all(series2)
  colnames(series2)[2] = series2_name
  
  #Merge
  data = left_join(series1, series2)
  
  #Update all values
  tmp = data.frame(series2_name,round(cor(data[,2], data[,3]), 4),"#", stringsAsFactors = FALSE, row.names = 1)
  colnames(tmp) = colnames(all_correlations)
  all_correlations = rbind(tmp, all_correlations)
  
  #Return
  return(list(all_correlations = all_correlations, selected_correlated = data))
}