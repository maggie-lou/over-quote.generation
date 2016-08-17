#' @param image_url A string representing the URL of the image to be interpreted. Must begin with 'https://'.
#' @parm char_count An optional integer input parameter representing the max character count. If not included, the default is set at 300. 
#' @return A JSON of every quote related to that image, with quote, author, category, character count.
#* @get /imagerec
quote_imagerec <- function(image_url, char_count = 300) {
  #generate image recognition tags for input
  image_tags <- tag_image(image_url)
  
  #connect to database
  library(RSQLite)
  db <- dbConnect(SQLite(), "Auto_Generation.sqlite")
  
  #verify connection
  if (dbIsValid(db) & dbExistsTable(db, "Content")) {
    
    #search content for top 5 tags 
    content <- data.frame("row_names" = character(0), "ID" = integer(0), "Text" = character(0), "Author" = character(0), "Category"= character(0), "Char_Count" = integer(0))
    
    for (i in image_tags$Tag[1:5]) {
      content <- rbind(content, dbGetQuery(db, paste("SELECT * FROM Content WHERE Text LIKE \'%", i, "%\'AND Char_Count <",char_count, ";", sep ="")))
      content <- rbind(content, dbGetQuery(db, paste("SELECT * FROM Content WHERE Category LIKE \'%", i, "%\'AND Char_Count <",char_count, ";", sep ="")))
    }
    if (nrow(content) == 0) {
      warning("There is no content matching this image's top 5 tags.")
    }
    
    #convert to JSON 
    library(jsonlite)
    output <- toJSON(content)
    
  } else {
    
    stop("Error connecting to the database.")
    
  }
  
  return(output)
}




#' @param url A string representing the URL of the image to be interpreted. Must begin with 'http(s)://'.
#' @return A dataframe of tags and confidence based on image recognition performed on the image passed in with the input url.
tag_image <- function(url) {
  
  #load package
  library(httr)
  
  #store auth keys
  api_key <- "acc_d4e484d32c6754e"
  api_secret <- "39331edd5fafb67536beacda2e8f81a2"
  
  #build url with api endpoint
  comb_url <- paste("https://api.imagga.com/v1/tagging?url=", url, sep = "")
  
  #get request to immaga api 
  imagga_resp = GET(comb_url, authenticate(api_key, api_secret))
  
  #if get request is successful:
  if(http_status(imagga_resp)$category == "Success") {
    
    #get/parse response content 
    resp_content <- content(imagga_resp, as = "parsed")
    
    #error checking
    if (!is.null(resp_content$unsuccessful)) {
      stop("The input URL does not correspond to a downloadable image.")
    }
    
    #get only results from content
    results <- resp_content$results
    
    #get tags as list from results
    tags <- results[[1]]$tags
    
    #unlist tags to vector
    unlistTags <- unlist(tags)
    
    #index vector properly to build dataframe
    tagIndex <- seq(from = 2, to = length(unlistTags), by = 2)
    
    #seperate tags and confidence scores 
    tags <- unlistTags[tagIndex] %>% as.character()
    confidence <- unlistTags[-tagIndex] %>% as.character() %>% as.numeric()
    
    #build dataframe fom results
    df <- data.frame("Tag" = tags, "Confidence" = confidence)
    df$Tag <- as.character(df$Tag)
    df$Confidence <- as.numeric(as.character(df$Confidence))
    
    #return data frame with tags + confidences
    return(df)
  }
  
  #if get request is unsuccessful
  else {
    stop("Error processing image.")
  }
  
}