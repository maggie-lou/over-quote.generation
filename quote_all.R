#' @return A JSON of every quote in the database, with quote, author, category, character count
#* @get /all
quote_all <- function() {
  #connect to database
  library(RSQLite)
  db <- dbConnect(SQLite(), "Auto_Generation.sqlite")
  
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