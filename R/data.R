#' Example data from a GPS tracking project
#'
#' Example datasets related to a GPS tracking project for roe deer in
#' Trentino Region, Italy. Four datasets include raw data from GPS
#' sensors (\code{roe_gps_data}), information on animals, sensors, and
#' sensor deployments on animals (\code{roe_sensors_animals_tables}),
#' and ancillary vector (\code{roe_vector_geom}) and raster
#' (\code{roe_raster}) spatial datasets.
#'
#' @format \code{roe_gps_data}: A list containing five
#'     \code{data.frame}s corresponding to five GPS sensors
#'
#' \describe{
#'     \item{GSM01438}{data frame for sensor 01438}
#'     \item{GSM01508}{data frame for sensor 01508}
#'     \item{GSM01511}{data frame for sensor 01511}
#'     \item{GSM01512}{data frame for sensor 01512}
#'     \item{GSM02927}{data frame for sensor 02927} }
#'
#' \code{roe_sensors_animals_tables}: A list containing three
#' \code{data.frame}s
#' \describe{
#'     \item{animals}{data frame containing basic information on
#'         animals}
#'     \item{gps_sensors}{data frame containing basic information
#'         on GPS sensors}
#'     \item{gps_sensors_animals}{data frame containing information
#'         on deployment of GPS sensors on animals}
#'  }
#'
#' \code{roe_vector_geom}: A list containing four
#' \code{Spatial*DataFrame}s
#' \describe{
#'     \item{study_area}{SpatialPolygonsDataFrame containing
#'         boundary of study area}
#'     \item{adm_boundaries}{SpatialPolygonsDataFrame containing
#'         administrative boundaries in study area}
#'     \item{meteo_stations}{SpatialPointsDataFrame containing
#'         locations of weather stations in study area}
#'     \item{roads}{SpatialLinesDataFrame containing representation
#'         of roads for study area}
#'  }
#'
#' \code{roe_raster}: A list containing two \code{RasterLayer} datasets
#' \describe{
#'     \item{corine06}{RasterLayer depicting land cover
#'         classification in the study area}
#'     \item{srtm_dem}{RasterLayer digital elevation model in the
#'         study area}
#'  }
#' @source Urbano, F. & Cagnacci, F., eds. (2014) Spatial Database for
#'     GPS Wildlife Tracking Data: A Practical Guide to Creating a
#'     Data Management System with PostgreSQL/PostGIS and R. Springer,
#'     257 pp. DOI: 10.1007/978-3-319-03743-1
#' @examples
#' data("roe_gps_data")
#' head(roe_gps_data$GSM01438)
"roe_gps_data"

#' @rdname roe_gps_data
#' @examples
#' data("roe_sensors_animals_tables")
#' roe_sensors_animals_tables$animals
"roe_sensors_animals_tables"

#' @rdname roe_gps_data
#' @examples
#' data("roe_vector_geom")
#' if (require(sp, quietly = TRUE)) {
#'     plot(roe_vector_geom$adm_boundaries)
#'     plot(roe_vector_geom$roads, col = 'red', add = TRUE)
#' }
"roe_vector_geom"

#' @rdname roe_gps_data
#' @examples
#' if (require(raster, quietly = TRUE)) {
#'  data("roe_raster")
#'  plot(roe_raster$srtm_dem)
#'  }
"roe_raster"
