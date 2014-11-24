raw_to_df_map = function(mapData){
  mapData = str_split(mapData, "\\[\\[")[[1]]
  #Sub country specified so only cities
  if(length(mapData) == 2){
    #Cities
    cities = mapData[2]
    cities = gsub("\\]\\].*", "", cities)
    cities = paste0("[[", cities, "]]")
    cities = fromJSON(cities)
    cities = data.frame(matrix(unlist(cities[-1]), byrow = TRUE, ncol = 4), stringsAsFactors = FALSE)
    colnames(cities) = c("lat", "lon", "name", "value")
    return(list(country = NULL, city = cities))
  }else{
    #Countries
    countries = mapData[2]
    countries = gsub("\\]\\].*", "", countries)
    countries = paste0("[[", countries, "]]")
    countries = fromJSON(countries)
    countries = data.frame(matrix(unlist(countries[-1]), byrow = TRUE, ncol = 3), stringsAsFactors = FALSE)
    colnames(countries) = c("id", "name", "value")
    #Cities
    cities = mapData[3]
    cities = gsub("\\]\\].*", "", cities)
    cities = paste0("[[", cities, "]]")
    cities = fromJSON(cities)
    cities = data.frame(matrix(unlist(cities[-1]), byrow = TRUE, ncol = 4), stringsAsFactors = FALSE)
    colnames(cities) = c("lat", "lon", "name", "value")
    #Return results
    return(list(country = countries, city = cities))
  }
}