#' @return A JSON of every quote in the database, with quote, author, category, character count
#* @get /all
quote_all <- function() {
  #connect to database
  library(RSQLite)
  db <- dbConnect(SQLite(), "Auto_Generation.db")
  
  #verify connection
  if (dbIsValid(db) & dbExistsTable(db, "Content")) {
    
    #grab entire table 
    content <- dbReadTable(db, "Content")
    
    if (nrow(content) == 0) {
      warning("There was not any content in the table.")
    }
    
    #convert to JSON 
    library(jsonlite)
    output <- toJSON(content)
  } else {
    stop("Error connecting to the database.")
  }
  
  return(output)
}




#' @param search_term A string representing the term to be searched for.
#' @parm char_count An optional integer input parameter representing the max character count. If not included, the default is set at 300. 
#' @return A JSON of every quote related to that search term, with quote, author, category, character count. The keyword can be matched in the content's text, quthor, or category.
#* @get /search
quote_search <- function(search_term, char_count = 300) {
  #clean input 
  library(magrittr)
  library(stringr)
  library(SnowballC)
  library(RSQLite)
  search_term <- search_term %>% str_to_lower() %>% str_trim() %>% wordStem()
  
  #connect to database
  db <- dbConnect(SQLite(), "Auto_Generation.db")
  
  #verify connection
  if (dbIsValid(db) & dbExistsTable(db, "Content")) {
    
    #query table for input category
    content <- dbGetQuery(db, paste("SELECT * FROM Content WHERE Text LIKE \'%", search_term, "%\'AND Char_Count <",char_count, 
                                    " OR Author LIKE \'%", search_term, "%\'AND Char_Count <",char_count, 
                                    " OR Category LIKE \'%", search_term, "%\'AND Char_Count <",char_count,";", sep =""))
    
    if (nrow(content) == 0) {
      warning("There was not any content containing that search term.")
    }
    
    #convert to JSON 
    library(jsonlite)
    output <- toJSON(content)
  } else {
    stop("Error connecting to the database.")
  }
  
  print('hi')
  dbGetQuery(db, paste("SELECT * FROM Content WHERE Text LIKE \'%", "mountain", "%\'AND Char_Count <",300))
  
  return(output)
  output
}


#' @param category A string representing the category. 
#' @parm char_count An optional integer input parameter representing the max character count. If not included, the default is set at 300. 
#' @return A JSON of every quote in that category, with quote, author, category, character count. 
#* @get /category
quote_category <- function(category, char_count = 300) {
  #clean input 
  library(magrittr)
  library(stringr)
  category <- category %>% str_to_lower() %>% str_trim()
  
  #connect to database
  library(RSQLite)
  db <- dbConnect(SQLite(), "Auto_Generation.db")
  
  #verify connection
  if (dbIsValid(db) & dbExistsTable(db, "Content")) {
    
    #query table for input category
    content <- dbGetQuery(db, paste("SELECT * FROM Content WHERE Category == \'",category,"\' AND Char_Count <",char_count, ";", sep = ""))
    
    if (nrow(content) == 0) {
      warning("There was not any content in that category.")
    }
    
    #convert to JSON 
    library(jsonlite)
    output <- toJSON(content)
  } else {
    stop("Error connecting to the database.")
  }
  
  return(output)
}



#' @param image_url A string representing the URL of the image to be interpreted. Must begin with 'https://'.
#' @parm char_count An optional integer input parameter representing the max character count. If not included, the default is set at 300. 
#' @return A JSON of every quote related to that image, with quote, author, category, character count.
#* @get /imagerec
quote_imagerec <- function(image_url, char_count = 300) {
  #generate image recognition tags for input
  image_tags <- tag_image(image_url)
  
  #connect to database
  library(RSQLite)
  db <- dbConnect(SQLite(), "Auto_Generation.db")
  
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


#' Adds input content to the filtered and categorized database.
#* @post /addmanual 
add_quote_manual <- function(text, category = "", author = "") {
  #connect to database
  library(RSQLite)
  db <- dbConnect(SQLite(), "Auto_Generation.db")
  
  #verify connection
  if (dbIsValid(db) & dbExistsTable(db, "Content")) {
    
    continue <- TRUE
    
    while(continue == TRUE) {
      
      #calculate character count of text 
      char_count <- nchar(text)
      
      #calculate index - adds 1 to the index of the last element in the table
      previous <- (dbGetQuery(db, "SELECT ID FROM Content WHERE ID = (SELECT MAX(ID) FROM Content)"))[1,1]
      index <- previous + 1
      
      #clean inputs
      library(magrittr)
      library(stringr)
      text <- str_trim(text)
      author <- str_trim(author)
      category <- category %>% str_to_lower() %>% str_trim()
      
      #add escape characters for single quotes
      text <- gsub("\'", "\\'", text)
      
      #add to database
      dbGetQuery(db, paste("INSERT INTO Content VALUES(",index,",\"",
                           text, "\", \"",
                           author, "\", \'",
                           category, "\', ", 
                           char_count,");", sep = ""))
      
      #ask if user wants to continue inputting quotes
      print("Would you like to continue inputting quotes? (y or n) : ")
      repeat {
        continue <- readline() %>% str_to_lower() %>% str_trim()
        
        if (continue == "y") {
          continue <- TRUE
        } else if (continue == 'n'| continue =="") {
          continue <- FALSE
        } else {
          print("Invalid input. Input y to continue adding quotes. To end the function, input n or press ENTER.")
        }
        
        if (continue == TRUE | continue == FALSE) 
          break
      }
      
      #get next piece of content 
      if (continue == TRUE) {
        print("Enter the text of the content: ")
        text <- readline()
        
        print("Enter the author of the content (If none, press ENTER)")
        author <- readline()
        
        print("Enter the category (If none, press ENTER)")
        category <- readline()
      }
    }
  } else {
    stop("Error connecting to the database.")
  }
}


#' @param ID An int vector representing the ID numbers of the rows you want to remove.
#* @post /removerow
remove_row <- function(ID) {
  #connect to database
  library(RSQLite)
  db <- dbConnect(SQLite(), "Auto_Generation.db")
  
  #verify connection
  if (dbIsValid(db) & dbExistsTable(db, "Content")) {
    
    #remove rows
    for (i in ID) {
      dbGetQuery(db, paste("DELETE FROM Content WHERE id = ", i, sep = ""))
    }
    
  }
}



#' @param ID An int representing the ID number of the row you want to edit.
#' @param field A string representing the field to be edited 
#' @param edit A string representing the new text to be input into the table 
#' @details Valid fields that can be edited are : Text, Author, and Category. ID and char_count can not be edited. If text is edited, char_count is automatically updated. 
#* @post /editrow
edit_row <- function(ID, field, edit) {
  
  #clean inputs 
  library(magrittr)
  library(stringr)
  field <- field %>% str_to_title() %>% str_trim()
  
  #don't let user edit ID or char_count 
  if (field == "Id" | field == "Char_count") {
    stop("You cannot edit the ID or char_count fields.")
  }
  
  #connect to database
  library(RSQLite)
  db <- dbConnect(SQLite(), "Auto_Generation.db")
  
  #verify connection
  if (dbIsValid(db) & dbExistsTable(db, "Content")) {
    
    #save field to be updated 
    old <- (dbGetQuery(db, paste("SELECT ", field, " FROM Content WHERE ID = ", ID, sep = "")))[1,1]
    
    #edit field 
    dbGetQuery(db, paste("UPDATE Content SET ", field, " = '", edit, "' WHERE ID = ", ID, sep = ""))
    
    #recalculate char_count if text was edited 
    if (field == "Text") {
      char_count <- nchar(edit)
      dbGetQuery(db, paste("UPDATE Content SET Char_count = ", char_count, " WHERE ID = ", ID, sep = ""))
    }
  }
  
  print(paste("You replaced the ", field, " ", old, " with ",edit, ".", sep = ""))
}