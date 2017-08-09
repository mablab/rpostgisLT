if(can_con(conn)) {
    try(RPostgreSQL::dbDisconnect(conn), silent = TRUE)
}