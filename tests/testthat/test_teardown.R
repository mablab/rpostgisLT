if(can_con(conn_empty)) {
    # try(RPostgreSQL::dbSendQuery(conn_empty,
    #                              paste(
    #                                  "SET log_error_verbosity TO",
    #                                  DBI::dbQuoteString(pg_verbosity)
    #                              )),
    #     silent = TRUE)
    # Clean up
    try(suppressMessages(rpostgis::dbDrop(
        conn_empty,
        "traj_min",
        type = "schema",
        cascade = TRUE,
        display = FALSE
    )),
    silent = TRUE)
    try(suppressMessages(rpostgis::dbDrop(
        conn_empty,
        "traj",
        type = "schema",
        cascade = TRUE,
        display = FALSE
    )),
    silent = TRUE)
    try(rpostgis::dbDrop(
        conn_empty,
        "type_I",
        type = "schema",
        cascade = TRUE,
        display = FALSE
    ),
    silent = TRUE)
    try(RPostgreSQL::dbDisconnect(conn_empty))
}

if(can_con(conn_data)) {
    try(RPostgreSQL::dbDisconnect(conn_data))
}