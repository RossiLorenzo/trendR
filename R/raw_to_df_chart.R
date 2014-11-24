raw_to_df_chart = function(chartData){
  chartData2 = str_split(chartData, "\\[\\[")
  if(length(chartData2) == 0)
    stop("Google Trends returns 'Not enough search volume to show graphs'")  
  chartData2 =chartData2[[1]]
  #chartData
  chartData2 = chartData2[2]
  chartData2 = gsub("\\]\\].*", "", chartData2)
  chartData2 = paste0("[[", chartData2, "]]")
  chartData2 = gsub(',\"v\":[^)]*)', "", chartData2)
  chartData2 = fromJSON(chartData2)
  #Get data
  extr_data = lapply(chartData2, function(x){
    date = date_convert(as.character(x[[1]]))
    tmp = sapply(x[-1], function(y){
      if(is.null(y))
        return("NA")
      else
        return(as.character(y))
    })
    tmp = c(date, tmp)
    tmp = data.frame(matrix(tmp, byrow = TRUE, nrow = 1), stringsAsFactors = FALSE)
    return(tmp)
  })
  extr_data = rbind_all(extr_data)
  news_title = gsub('.*<div class="news-tooltip-source">', '', extr_data$X6)
  news_title = gsub('<.*', '', news_title)
  news_text = gsub('.*<div class="news-tooltip-title">', '', extr_data$X6)
  news_text = gsub('<.*', '', news_text)
  
  results = data.frame(
    "date" = as.Date(as.numeric(extr_data$X1), origin = "1970-01-01"),
    "value" = as.numeric(extr_data$X4),
    "news_title" = news_title,
    "news_text" = news_text,
    "forecast" = !as.logical(extr_data$X7),
    stringsAsFactors = FALSE)
}