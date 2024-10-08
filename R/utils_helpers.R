#' add_sessionid_to_db 
#'
#' @description This function must accept two parameters: user and sessionid. It will be called whenever the user
# successfully logs in with a password.  
#'
#' @return This function saves to your database.
#'
#' 
#' @importFrom sodium password_store

add_sessionid_to_db <- function(user, sessionid, conn = db) {
  tibble(user = user, sessionid = sessionid, login_time = as.character(now())) |>
    dbWriteTable(conn, "sessionids", ., append = TRUE)
}


#' add_sessionid_to_db 
#'
#' @description Check user who has not visited the app for this many days. Check globals for number of days expiry
#'
#' @return users who have not visited the app for this many days
#'
#' @noRd
#' 
#' 
# 
get_sessionids_from_db <- function(conn = db, expiry = cookie_expiry) {
  dbReadTable(conn, "sessionids") |>
    mutate(login_time = ymd_hms(login_time)) |>
    as_tibble() |>
    filter(login_time > now() - days(expiry))
}

# TODO: Remove unused functions














