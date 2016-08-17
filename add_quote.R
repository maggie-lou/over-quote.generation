View(dbReadTable(db, "Content"))

#' Adds input content to the filtered and categorized database.
#* @post /addmanual 
add_quote_manual <- function(text, category = "", author = "") {
  #connect to database
  library(RSQLite)
  db <- dbConnect(SQLite(), "Auto_Generation.sqlite")
  
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


# If the input parameters all = 'table', until you stop running the function, randomly goes through the unfiltered quotes: 
#' 1. Displays a quote and it's author.
#' 2. Asks if you want to add it to your quote database. 
#'    a. If yes: 
#'      i. Asks if you want to edit the quote. 
#'      ii. Asks for the category
#' 3. Goes to next quote
#' 4. When the user tells the function to stop, it adds all of the categorized quotes (which are stored in a temporary database) to the filtered quotes csv 
#' 
#' Else, if you actually have a quote you want to input manually, just input them as strings (or vectors of strings that are all the same length). At the end, they will be added to the filtered database.
#'    - For manual input, adds to filtered database regardless of characteer count But still adds that count to the filtered table.  

#' 
#' 
#' 
#' #' @param table A boolean for whether or not you want to add content from the scraped table. If FALSE, you can input your own content. 
#' #'    
#' #' @description Allows you to add content to the filter and categorized database. You can either manually add content or use content from the scraped table.
#' #' If using content from the scraped table, it will randomly select  
#' add_quote <- function(table, content = "" , author = "" , category = "") {
#'   if ()
#'   
#' }
#' 
#' # removes the last quote you added to the database 
#' undo_add
#' 
#' 
#' 
#' update_rownumber_column() 