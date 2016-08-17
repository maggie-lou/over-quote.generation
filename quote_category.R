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
  db <- dbConnect(SQLite(), "Auto_Generation.sqlite")
  
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