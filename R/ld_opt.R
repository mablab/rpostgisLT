#' Faster versions of \code{\link[adehabitatLT]{ld}} and
#' \code{\link[adehabitatLT]{dl}}.
#'
#' In \code{\link[adehabitatLT]{ld}}, \code{strict = FALSE} can be up
#' to 10 times faster, but assumes that the \code{ltraj} is well
#' structured (i.e. not modified by the user). In
#' \code{\link[adehabitatLT]{dl}}, \code{strict = FALSE} can be up to
#' 20 times faster, but assumes that the trajectory parameters in the
#' data frame (x/y increments, angles, etc.) are still valid (e.g. no
#' locations have been removed).
#' @title Quick Conversion of Objects of Class ltraj from and to
#' Dataframes
#' @seealso See \code{\link[adehabitatLT]{ld}} for further details on
#' the function and all available arguments.
#' @author Modified by Mathieu Basille, Balázs Dukai,
#' \email{basille@@ase-research.org}, \email{balazs.dukai@@gmail.com}
#' @examples
#' data(puechcirc)
#' puechcirc ## class ltraj
#'
#' ## ld
#' df1 <- adehabitatLT::ld(puechcirc)
#' df2 <- ld(puechcirc, strict = FALSE)
#' all.equal(df1, df2)
#' ## Note a difference in row names:
#' attr(df1, "row.names")
#' attr(df2, "row.names")
#'
#' ## dl
#' all.equal(dl(df2), adehabitatLT::dl(df2))
#' dl(df2, strict = FALSE)
#' ## Comparison regarding 'strict'
#' all.equal(dl(df2), dl(df2, strict = FALSE))
#' ## Differences in row.names (numeric in regular 'dl', characters using
#' ## 'strict = FALSE') + NAs in R2n (for a reason, 'puechcirc[[2]]'
#' ## starts by a sequence of missing values, but has several 'R2n'
#' ## values. As a result, 'strict = FALSE' keeps the 'R2n' values)

# dl
#
#' @rdname ld

#debug(rpostgisLT:::dl_opt(DF2))
#
#x <- DF2
#
#for (i in (1:length(traj))) {
##    attr(traj[[i]], "id") <- as.character(idd[i])
##    attr(traj[[i]], "burst") <- names(idd[i])
##    rownames(traj[[i]]) <- as.character(traj_rname[[i]])
#    rownames(traj[[i]]) <- as.character(traj_rname[[i]])
#}
#
#fi <- DF2[DF2$burst == 'A153', ][6:10, ]
#se <- DF2[DF2$burst == 'A160', ][6:10, ]
#th <- DF2[DF2$burst == 'A286', ][6:10, ]
#fo <- DF2[DF2$burst == 'A289', ][6:10, ]
#
#
#for (i in (1:length(a))) {
#    rownames(a[[i]]) <- c(1,2,3,4,5)
#}

dl_opt <- function(x) {
    if (!inherits(x, "data.frame"))
        stop("x should be of class data.frame")
    #warning("Parameters of the trajectory are not recomputed.")
    trajnam <- c("x", "y", "date", "dx", "dy", "dist", "dt",
            "R2n", "abs.angle", "rel.angle")
    ## Extract unique IDs
    idd <- tapply(as.character(x$id), x$burst, unique)
    ## Split the data frame by burst
    traj <- split(x[, names(x) %in% trajnam], x$burst)
    traj_rname <- split(x[, "r.row.names"], x$burst)
    ## 'ltraj' names, class and attributes
    names(traj) <- NULL
    class(traj) <- c("ltraj", "list")
    attr(traj, "typeII") <- TRUE
    attr(traj, "regular") <- adehabitatLT:::is.regular(traj)
    ## In case of 'infolocs' data
    if (any(!(names(x) %in% c(trajnam, "id", "burst", "r.row.names")))) {
        ## Split the infolocs by burst
        inf <- split(x[, !(names(x) %in% c(trajnam, "id", "burst", "r.row.names")), 
                        drop = FALSE], x$burst)
        ## Loop in the ltraj to add 'id', 'burst' and 'infolocs'
        for (i in (1:length(traj))) {
            attr(traj[[i]], "id") <- as.character(idd[i])
            attr(traj[[i]], "burst") <- names(idd[i])
            attr(traj[[i]], "infolocs") <- inf[[i]]
            rownames(traj[[i]]) <- traj_rname[[i]]
        }
    }
    ## If no infolocs, loop in the ltraj to add 'id' and 'burst'
    else for (i in (1:length(traj))) {
            attr(traj[[i]], "id") <- as.character(idd[i])
            attr(traj[[i]], "burst") <- names(idd[i])
            rownames(traj[[i]]) <- traj_rname[[i]]
        }
    return(traj)
}


# ld
ld_opt <- function(ltraj) {
    if (!inherits(ltraj, "ltraj"))
        stop("ltraj should be of class ltraj")
    ## Builds the data frame without calls to 'rbind'
    inf <- infolocs(ltraj)
    df <- data.frame(
            x = unlist(lapply(ltraj, function(x) x$x)),
            y = unlist(lapply(ltraj, function(x) x$y)),
            date = unlist(lapply(ltraj, function(x) x$date)),
            dx = unlist(lapply(ltraj, function(x) x$dx)),
            dy = unlist(lapply(ltraj, function(x) x$dy)),
            dist = unlist(lapply(ltraj, function(x) x$dist)),
            dt = unlist(lapply(ltraj, function(x) x$dt)),
            R2n = unlist(lapply(ltraj, function(x) x$R2n)),
            abs.angle = unlist(lapply(ltraj, function(x) x$abs.angle)),
            rel.angle = unlist(lapply(ltraj, function(x) x$rel.angle)),
            id = rep(id(ltraj), sapply(ltraj, nrow)),
            burst = rep(burst(ltraj), sapply(ltraj, nrow)),
            r.row.names = unlist(lapply(ltraj, function(x) rownames(x))))
    #rownames(df) <- unlist(lapply(ltraj, function(x) rownames(x)))
    class(df$date) <-  c("POSIXct", "POSIXt")
    attr(df$date, "tzone") <- attr(ltraj[[1]]$date, "tzone")
    if (!is.null(inf)) {
        nc <- ncol(inf[[1]])
        infdf <- as.data.frame(matrix(nrow = nrow(df), ncol = nc))
        names(infdf) <- names(inf[[1]])
        for(i in 1:nc)
            infdf[[i]] <- unlist(lapply(inf, function(x) x[[i]]))
        df <- cbind(df, infdf)
    }
    return(df)
}
