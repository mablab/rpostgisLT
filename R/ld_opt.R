## ld_opt

##' Quick Conversion of Objects of Class ltraj from and to Dataframes
##'
##' Faster versions of \code{\link[adehabitatLT]{ld}} and
##' \code{\link[adehabitatLT]{dl}}.
##'
##' @param ltraj An object of class \code{ltraj}.
##' @return \code{ld_opt} returns a data frame with all trajectory
##'     parameters (and infolocs fields) as columns; \code{dl_opt}
##'     returns an object of class \code{ltraj}.
##' @keywords internal
##' @seealso See \code{\link[adehabitatLT]{ld}} for further details on
##'     the function and all available arguments.
##' @author Modified by Mathieu Basille \email{basille@@ufl.edu},
##'     Balázs Dukai \email{balazs.dukai@@gmail.com}

ld_opt <- function(ltraj) {
    if (!inherits(ltraj, "ltraj"))
        stop("ltraj should be of class ltraj")
    ## Equivalent of hab::ld(strict = FALSE)
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
        ## + Add row names as a column
        r.row.names = unlist(lapply(ltraj, function(x) rownames(x))))
    class(df$date) <- c("POSIXct", "POSIXt")
    attr(df$date, "tzone") <- attr(ltraj[[1]]$date, "tzone")
    if (!is.null(inf)) {
        nc <- ncol(inf[[1]])
        infdf <- as.data.frame(matrix(nrow = nrow(df), ncol = nc))
        names(infdf) <- names(inf[[1]])
        for (i in 1:nc)
            infdf[[i]] <- unlist(lapply(inf, function(x) x[[i]]))
        df <- cbind(df, infdf)
    }
    return(df)
}


## dl

##' @rdname ld_opt

dl_opt <- function(x, rnames = TRUE) {
    if (!inherits(x, "data.frame"))
        stop("x should be of class data.frame")
    ## Equivalent of hab::dl(strict = FALSE)
    trajnam <- c("x", "y", "date", "dx", "dy", "dist", "dt",
        "R2n", "abs.angle", "rel.angle")
    idd <- tapply(as.character(x$id), x$burst, unique)
    traj <- split(x[, names(x) %in% trajnam], x$burst)
    ## + Split row names by burst
    if (rnames) {
        traj_rname <- split(x[, "r.row.names"], x$burst)
            names(traj) <- NULL
        class(traj) <- c("ltraj", "list")
        attr(traj, "typeII") <- TRUE
        attr(traj, "regular") <- is.regular(traj)
        ## + Add r.row.names
        if (any(!(names(x) %in% c(trajnam, "id", "burst", "r.row.names")))) {
            inf <- split(x[, !(names(x) %in% c(trajnam, "id", "burst",
                "r.row.names")), drop = FALSE], x$burst)
            for (i in (1:length(traj))) {
                attr(traj[[i]], "id") <- as.character(idd[i])
                attr(traj[[i]], "burst") <- names(idd[i])
                attr(traj[[i]], "infolocs") <- inf[[i]]
                ## + Add r.row.names
                rownames(traj[[i]]) <- traj_rname[[i]]
            }
        } else for (i in (1:length(traj))) {
            attr(traj[[i]], "id") <- as.character(idd[i])
            attr(traj[[i]], "burst") <- names(idd[i])
            ## + Add r.row.names
            rownames(traj[[i]]) <- traj_rname[[i]]
        }
        return(traj)
    } else {
        names(traj) <- NULL
        class(traj) <- c("ltraj", "list")
        attr(traj, "typeII") <- TRUE
        attr(traj, "regular") <- is.regular(traj)
        if (any(!(names(x) %in% c(trajnam, "id", "burst")))) {
            inf <- split(x[, !(names(x) %in% c(trajnam, "id", "burst")),
                            drop = FALSE], x$burst)
            for (i in (1:length(traj))) {
                attr(traj[[i]], "id") <- as.character(idd[i])
                attr(traj[[i]], "burst") <- names(idd[i])
                attr(traj[[i]], "infolocs") <- inf[[i]]
                attr(traj[[i]], "row.names") <- rownames(traj[[i]])
            }
        } else for (i in (1:length(traj))) {
            attr(traj[[i]], "id") <- as.character(idd[i])
            attr(traj[[i]], "burst") <- names(idd[i])
            attr(traj[[i]], "row.names") <- rownames(traj[[i]])
        }
        return(traj)
    }

}
