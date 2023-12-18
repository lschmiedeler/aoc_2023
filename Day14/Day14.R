library(stringr)
setwd("Day14")

puzzle <- 1

connection <- file(description = "Day14Input.txt", open = "r")
rocks <- readLines(connection)
close(connection)

rocks_mat <- matrix(unlist(lapply(rocks, function(rock) {
    str_split(rock, "")[[1]]
})), nrow = length(rocks), byrow = T)

tilt_lever <- function(rocks) {
    while (T) {
        n <- 0
        for (i in which(rocks == "O")) {
            if (i > 1) {
                if (rocks[i-1] == ".") {
                    rocks[i-1] <- "O"
                    rocks[i] <- "."
                    n <- n + 1
                }   
            }
        }
        if (n == 0) { break }
    }
    return(rocks)
}

sum(unlist(lapply(1:ncol(rocks_mat), function(c) {
    rocks <- tilt_lever(rocks_mat[,c])
    which(rev(rocks) == "O")
})))
