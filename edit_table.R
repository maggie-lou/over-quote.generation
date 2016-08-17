#' @param ID An int vector representing the ID numbers of the rows you want to remove.
remove_row <- function(ID) {
  #connect to database
  library(RSQLite)
  db <- dbConnect(SQLite(), "Auto_Generation.sqlite")
  
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
  db <- dbConnect(SQLite(), "Auto_Generation.sqlite")
  
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