#' Quote Generation 

#' @return A JSON of every quote in that category, with quote, author, category, character count. 
quote_category(category)

#' @return A JSON of every quote related to that image, with quote, author, category, character count.
quote_imagerec(image_url)

#' @return A JSON of every quote related to that search term, with quote, author, category, character count
quote_search(search_term)

#' @return A JSON of every quote in the database, with quote, author, category, character count
quote_all()

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
add_quote(content = table , author = table , category = table)


#Scrapes twitter 
scrape_quote(handle)

# Removes a quote from the filtered table
delete_quote(content)





#' Databases: 
#' - filtered and categorized quotes, all of which will appear in the app
#'    - this is a CSV (or this might have to be a SQL database so you don't have to store a CSV locally on your computer)
#'    - this is what is accessed by all of my functions
#'    - each quote can be in multiple categories: the category element can be a list of categories, separated by a comma and a space - 
#'    my program will then parse that to separate the categories 
#' - unfiltered quotes:s must be filtered and categorized - after which they will be added to the former category 





#' JSON Generation: 
#' JSON <- toJSON(dataframe_name)'
#' 



#; API Generation 
#' - deployr: https://deployr.revolutionanalytics.com/documents/getting-started/about/
#' - look into plumbr, deployr 
#' - detailed way to write an api : https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
#' - hosting my file on a server: https://plumber.trestletech.com/docs/hosting/
#' - have script constantly running: nohup script.R &
#'  - the & tells nohup to run your script in the background, even when you have disconnected