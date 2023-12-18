library(stringr)
setwd("Day12")

puzzle <- 1

connection <- file(description = "Day12Input.txt", open = "r")
records <- lapply(readLines(connection), function(record) { 
    record_split <- str_split(record, " ")[[1]]
    springs <- str_split(record_split[1], "")[[1]]
    groups <- as.integer(str_split(record_split[2], ",")[[1]])
    return(list(springs, groups))
})
close(connection)

count_valid_arrange <- function(springs, groups) {
    if (length(groups) > 0 & length(springs) == 0) { return(0) }
    else if (length(groups) == 0) { if ((sum(springs == "#") == 0)) { return(1) } else { return(0) } }
    else if (springs[1] == ".") { return(count_valid_arrange(springs[-1], groups)) }
    else if (springs[1] == "?") { return(count_valid_arrange(c(".", springs[-1]), groups) + count_valid_arrange(c("#", springs[-1]), groups)) }
    else {
        if (sum(springs == ".") == 0) {
            first_operational <- length(springs)
            current_group <- springs
        }
        else { 
            first_operational <- which(springs == ".")[1]
            current_group <- springs[1:first_operational - 1] 
        }
        if (length(current_group) < groups[1]) { return(0) }
        else if (length(current_group) == groups[1]) { return(count_valid_arrange(springs[-1:-first_operational], groups[-1])) }
        else if (current_group[groups[1] + 1] == "?") { return(count_valid_arrange(springs[-1:-(groups[1] + 1)], groups[-1])) }
        else { return (0) }
    }
}

sum(sapply(records, function(record) { count_valid_arrange(record[[1]], record[[2]]) }))
