#' Function converts relocation data into the pgtraj data model.
#'
#' @author Balázs Dukai
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores the pgtraj data model.
#' @param relocation_data
#' @param pgtrajs
#' @param animals
#' @param bursts
#' @param steps
###############################################################################

as_pgtraj <- function(conn, schema = "pgtraj", relocation_data, 
        pgtrajs = "", animals = "", bursts = "", steps) {
    # TODO check if conn still valid
    # TODO check if relocation_data exists
    # TODO check if steps has SRID
    # TODO check if schema exists
    # TODO set up schema
}
