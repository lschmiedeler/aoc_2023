library(stringr)
setwd("Day11")

puzzle <- 2
if (puzzle == 1) { n <- 1 } else { n <- 999999 }

connection <- file(description = "Day11Input.txt", open = "r")
data <- lapply(readLines(connection), function(line) { str_split(line, "")[[1]] })
data <- matrix(unlist(data), nrow = length(data), byrow = T)
close(connection)

find_expand_indices <- function(data) { 
    expand <- sapply(1:nrow(data), function(r) {
        row <- data[r,]
        if (sum(row == "#") == 0) { return(r) } else { return(0) }
    })
    return(expand[expand != 0])
}
expand_rows <- find_expand_indices(data)
expand_cols <- find_expand_indices(t(data))

find_expand_galaxy <- function(galaxy, n) { 
    galaxy <- unname(galaxy)
    return(c(n * sum(galaxy[1] >= expand_rows) + galaxy[1], n * sum(galaxy[2] >= expand_cols) + galaxy[2]))
}
galaxies <- which(data == "#", arr.ind = T)
sum(sapply(1:nrow(galaxies), function(x) {
    sum(sapply(x:nrow(galaxies), function(y) {
        expanded_galaxy_1 <- find_expand_galaxy(galaxies[x,], n)
        expanded_galaxy_2 <-  find_expand_galaxy(galaxies[y,], n)
        return(abs(expanded_galaxy_1[1] - expanded_galaxy_2[1]) + abs(expanded_galaxy_1[2] - expanded_galaxy_2[2]))
    }))
}))
