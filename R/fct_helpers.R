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
    db <- dbConnect(RSQLite::SQLite(), ":memory:")

    dbWriteTable(db, "iris", iris, overwrite =T)
    dbWriteTable(db, "mtcars", mtcars, overwrite =T)
    dbWriteTable(db, "starwars2", starwars2, overwrite =T)

    print("DB doesn't exist writing in memory")

  }

  
  return(db)
}


