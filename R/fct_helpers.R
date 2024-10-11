#' helpers 
#'
#' @description A connection function
#' 
#' 
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect dbWriteTable
#'
#' @return connection
#'
#' @noRd


create_conn <- function(){
  

  if(file.exists("test_db_file")) {
    # unlink("test_db_file")
    db <- dbConnect(SQLite(), 'test_db_file')
    print("DB already exists")
    
  } else {
    db <- dbConnect(SQLite(), 'test_db_file')
    print("DB doesn't exist")

  }


  # dbWriteTable(db, "iris", iris, overwrite =T)
  # dbWriteTable(db, "mtcars", mtcars, overwrite =T)
  # dbWriteTable(db, "starwars2", starwars2, overwrite =T)
  
  return(db)
}


# TODO: Remove this mess before merging to main

