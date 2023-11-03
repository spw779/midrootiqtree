#' Mid-root IQ-TREE Function
#'
#' This function extracts a specified outgroup from an IQ-TREE formatted tree string and performs required operations.
#'
#' @param tree_string The input IQ-TREE formatted tree string.
#' @param outgroup_label The label of the outgroup to extract.
#'
#' @return A modified IQ-TREE formatted tree string.
#'
#' @export
mid_root_iqtree <- function(tree_string, outgroup_label) {
    pattern <- paste("(", outgroup_label, ":\\d+\\.\\d+)", sep = "")
    matches <- regmatches(tree_string, gregexpr(pattern, tree_string))

    # Extracted string
    extracted_string <- matches[[1]]

    # Remove the extracted portion from the input string
    modified_string <- gsub(paste0(',', extracted_string), "", tree_string)

    # Remove trailing semicolon if it exists
    modified_string <- sub(";$", "", modified_string)

    # Extract the numeric value (c) and divide it by 2
    outgroup_branch_len <- as.numeric(strsplit(extracted_string, ':')[[1]][2])
    outgroup_branch_len_0.5 <- outgroup_branch_len / 2

    # Create the final formatted string
    final_string <- paste("(", modified_string, ":", outgroup_branch_len_0.5, ",", outgroup_label, ":", outgroup_branch_len_0.5, ");", sep = "")

    return(final_string)
}

