#' helpers 
#'
#' @description A connection function
#' 
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect dbWriteTable
#' @importFrom RPostgres Postgres
#'
#' @return connection
#'
#' @noRd

create_conn <- function() {
  if (1==1) {
    tryCatch({
      conn <- DBI::dbConnect(
        RPostgres::Postgres(),
        host = Sys.getenv("DB_HOST"),
        port = Sys.getenv("DB_PORT"),
        dbname = Sys.getenv("DB_NAME"),
        user = Sys.getenv("DB_USER"),
        password = Sys.getenv("DB_PASS")
      )
      
      message("Successfully connected to PostgreSQL database")
      return(conn)
      
    }, error = function(e) {
      message(paste("PostgreSQL connection failed:", e$message, 
                   "\nFalling back to SQLite in-memory database."))
      # Fall back to SQLite
      create_sqlite_db()
    })
  } else {
    return(create_sqlite_db())
  }
}

# Helper function to create SQLite database
create_sqlite_db <- function() {
  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  
  DBI::dbWriteTable(db, "iris", iris, overwrite = TRUE)
  DBI::dbWriteTable(db, "mtcars", mtcars, overwrite = TRUE)
  DBI::dbWriteTable(db, "starwars2", starwars2, overwrite = TRUE)
  
  message("Using SQLite in-memory database")
  return(db)
}
