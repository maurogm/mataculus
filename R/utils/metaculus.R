
#' Takes a data.frame and exports it as table that can be parsed as a table
#' by metaculus' markdown format:
export_markdown_table <- function(df, file_namepath) {
    colnames_line <- colnames(df) %>% paste(collapse = "|")
    sep_line <- rep("|", ncol(df) - 1) %>% paste(collapse = "")
    rows_lines <- rep("PLACEHOLDER", nrow(df))
    for (i in 1:nrow(df)) {
        rows_lines[i] <- paste(df[i, ], collapse = "|")
    }
    # write vector of lines to text file:
    writeLines(c(colnames_line, sep_line, rows_lines), file_namepath)
}

