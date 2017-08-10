if(can_con(conn)) {
    # try(RPostgreSQL::dbSendQuery(conn,
    #                              paste(
    #                                  "SET log_error_verbosity TO",
    #                                  DBI::dbQuoteString(pg_verbosity)
    #                              )),
    #     silent = TRUE)
    # Clean up
    try(suppressMessages(rpostgis::dbDrop(
        conn,
        "traj_min",
        type = "schema",
        cascade = TRUE,
        display = FALSE
    )),
    silent = TRUE)
    try(suppressMessages(rpostgis::dbDrop(
        conn,
        "traj",
        type = "schema",
        cascade = TRUE,
        display = FALSE
    )),
    silent = TRUE)
    try(rpostgis::dbDrop(
        conn,
        "type_I",
        type = "schema",
        cascade = TRUE,
        display = FALSE
    ),
    silent = TRUE)
    try(RPostgreSQL::dbDisconnect(conn))
}