query_function <- function(subject, set.name = "", pokemon.name = "", sort.by = "number", select = NULL){
  
  #this ensures that if a name includes a space it can still work in the function
  pokemon.name <- URLencode(pokemon.name)
  set.name <- URLencode(set.name)
  
  #need an if statement for select piece because there are many values it can take that have different formats
  selected <- if (!is.null(select)) {
    paste0("&select=", paste(select, collapse = ","))
  } else { "" }
  
  #set up main conditional statement for querying
  if (set.name == "" && pokemon.name == "") {
    data_url <- paste0("https://api.pokemontcg.io/v2/", subject, "?orderBy=", sort.by, selected)
  } else if (set.name == "") {
    data_url <- paste0("https://api.pokemontcg.io/v2/", subject, "?q=name:", pokemon.name, "&orderBy=", sort.by, selected)
  } else if (pokemon.name == "") {
    data_url <- paste0("https://api.pokemontcg.io/v2/", subject, "?q=set.name:", set.name, "&orderBy=", sort.by, selected)
  } else {
    data_url <- paste0("https://api.pokemontcg.io/v2/", subject, "?q=set.name:", set.name, "%20name:", pokemon.name, "&orderBy=", sort.by, selected)
  }
  
  info <- httr::GET(url=data_url)
  parsed = jsonlite::fromJSON(rawToChar(info$content))
  queried_data = as.tibble(parsed$data)
  return(queried_data)
}