# r <- plumb("quote_search.R")
# r$run(port = 8000)



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
  db <- dbConnect(SQLite(), "Auto_Generation.sqlite")
  
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