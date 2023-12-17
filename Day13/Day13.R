library(stringr)
setwd("Day13")

puzzle <- 1

connection <- file(description = "Day13Input.txt", open = "r")
patterns <- str_split(str_split(paste(readLines(connection), collapse = "\n"), "\\n\\n")[[1]], "\\n")
close(connection)

summarize_reflection <- function(reflection_col, diff_indices, n, vertical) {
    if (sum(diff_indices %in% reflection_col[diff_indices]) == length(diff_indices)) {
        if (vertical) { return(n) } else { return(100 * n) }
    } else { return(0) }
}

check_reflection <- function(reflection_col, vertical) {
    reflection_col_diff <- diff(reflection_col)
    no_reflection <- which(reflection_col_diff != -1)
    if (length(start) == 0) { 
        n <- length(reflection_col) / 2
        if (vertical) { return(n) } else { return(100 * n) }
    }
    if (reflection_col_diff[1] == -1) { 
        stop <- no_reflection[1]
        summary <- summarize_reflection(reflection_col, 1:stop, stop / 2, vertical)
        if (summary > 0) { return(summary) }
    }
    if (reflection_col_diff[(length(reflection_col_diff))] == -1) { 
        last_no_reflecion <- no_reflection[length(no_reflection)]
        stop <- length(reflection_col)
        summary <- summarize_reflection(reflection_col, (last_no_reflecion + 1):stop, 
                                        last_no_reflecion + (stop - last_no_reflecion) / 2, vertical)
        if (summary > 0) { return(summary) }
    }
    return(0)
}

summarize_patterns <- function(patterns, vertical) {
    sapply(patterns, function(pattern) {
        all_matches <- lapply(1:length(pattern), function(i) {
            matches <- which(pattern == pattern[i])
            matches <- matches[matches != i]
            if (length(matches) == 0) { return(c(-1)) } else { return(matches) }
        })
        all_matches_mat <- unname(t(expand.grid(all_matches)))        
        c <- 1
        summary <- 0
        while (c <= ncol(all_matches_mat)) {
            col <- all_matches_mat[,c]
            current_summary <- check_reflection(col, vertical)
            if (current_summary > 0) { 
                summary <- current_summary 
                break
            }
            c <- c + 1
        }
        return(summary)
    })
}

patterns_t_mat <- lapply(patterns, function(pattern) {
    new_pattern <- (lapply(pattern, function(p) { str_split(p, "")[[1]] }))
    t(matrix(unlist(new_pattern), nrow = length(new_pattern), byrow = T))
})
patterns_t <- lapply(patterns_t_mat, function(pattern_t) {
    sapply(1:nrow(pattern_t), function(r) { paste(pattern_t[r,], collapse = "") }) 
})

sum(summarize_patterns(patterns, F), summarize_patterns(patterns_t, T))
