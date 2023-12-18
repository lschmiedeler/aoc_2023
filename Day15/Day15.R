library(stringr)
setwd("Day15")

puzzle <- 2

connection <- file(description = "Day15Input.txt", open = "r")
steps <- str_split(readLines(connection)[1], ",")[[1]]
close(connection)

if (puzzle == 1) {
    sum(sapply(steps, function(step) {
        current_value <- 0
        for (x in utf8ToInt(step)) { current_value <- (17 * (current_value + x)) %% 256 }
        return(current_value)
    }))
} else {
    boxes <- vector("list", 256)
    for (step in steps ) {
        box <- 0
        step_info <- str_split(step, "[=|-]")[[1]]
        label <- step_info[1]
        for (x in utf8ToInt(label)) { box <- (17 * (box + x)) %% 256 }
        box <- box + 1
        current_box <- boxes[[box]]
        current_labels <- sapply(current_box, function(cb) { str_split(cb, " ")[[1]][1] })
        if (step_info[2] == "") { if (length(boxes[[box]]) > 0) { boxes[[box]] <- current_box[which(current_labels != label)] } }
        else {
            focal_length <- as.integer(step_info[2])
            lens <- paste(label, focal_length)
            if (length(current_box) > 0) {
                if (label %in% current_labels) { boxes[[box]][which(current_labels == label)] <- lens }
                else { boxes[[box]] <- c(current_box, lens) }
            } else { boxes[[box]] <- lens }
        }
    }
    sum(sapply(1:length(boxes), function(b) {
        box <- boxes[[b]]
        if (length(box) > 0) {
            return(sum(sapply(1:length(box), function(l) { return(b * l * as.integer(str_split(box[l], " ")[[1]][2])) })))
        } else { return(0) }
    }))
}
