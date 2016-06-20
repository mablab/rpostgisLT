# Plot disk usage difference in PostgreSQL
# Steps vs. relocations
# Author: bdukai
###############################################################################
library(ggplot2)
library(dplyr)
disk_usage <- read.csv("disk_usage.csv", sep = ",", stringsAsFactors = FALSE)

colnames(disk_usage) <- c("Geometry_type", "Nr_of_geometries", "Disk_usage_MB")
disk_usage <- group_by(disk_usage, Geometry_type)

# Average the nr. of geometries
av <- function(x) {
    for(i in seq(1, nrow(x), 2)) {
        a <- round((x[i, 2] + x[i+1, 2])/2)
        x[i, 2] <- a
        x[i+1, 2] <- a
    }
    return(x)
}
disk_usage_av <- av(disk_usage)

norm_diff <- function(x) {
    for(i in seq(1, nrow(x), 2)) {
        a <- x[i+1, 3]/x[i, 3]
        x[i+1, 3] <- a*100
        x[i, 3] <- 100
    }
    return(x)
    
}
disk_usage_n <- norm_diff(disk_usage_av)

gg <- ggplot(
                disk_usage_av,
                aes(
                        x = Nr_of_geometries,
                        y = Disk_usage_MB,
                        colour = factor(Geometry_type)
                )
        ) + 
        geom_line(
                aes(
                        group = Geometry_type
                )
        ) + 
        labs(
                x = "Nr. of geometries",
                y = "Disk usage (MB)",
                colour = "Geometry type"
        ) +
        annotate(
                "text",
                x = 1003536,
                y = 112,
                label = "112 MB"
        ) + 
        annotate(
                "text",
                x = 1003536,
                y = 97,
                label = "97 MB"
        )
gg

nd <- ggplot(
                disk_usage_n,
                aes(
                        x = Nr_of_geometries,
                        y = Disk_usage_MB,
                        colour = factor(Geometry_type)
                )
        ) + 
        geom_line(
                aes(
                        group = Geometry_type
                )
        ) + 
        labs(
                x = "Nr. of geometries",
                y = "Disk usage %",
                colour = "Geometry type"
        ) +
        scale_y_continuous(
                limits = c(70,130),
                breaks = seq(10, 130, 10)
        )
nd

ggsave("diff_n.png", nd, "png")

