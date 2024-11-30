#' Reroot a treedata object with branch length adjustment
#'
#' This function reroots a phylogenetic tree stored as a `treedata` object.
#' It supports specifying the root as either a node number or a tip label and
#' adjusts branch lengths accordingly.
#'
#' @param treedata_obj A `treedata` object containing a phylogenetic tree and associated data.
#' @param root_input Either a node number (integer) or a tip label (character).
#' @return A new `treedata` object with the rerooted tree.
#' @importFrom ape root
#' @importFrom treeio treedata
#' @importFrom ggtree as.phylo
#' @examples
#' library(ggtree)
#' tree <- ggtree::random_tree(10)
#' new_tree <- reroot_treedata(tree, root_input = "t3")
#' ggtree(new_tree)
reroot_treedata <- function(treedata_obj, root_input) {
  if (!inherits(treedata_obj, "treedata")) {
    stop("Input must be a 'treedata' object")
  }
  
  tree <- as.phylo(treedata_obj)
  metadata <- as.data.frame(treedata_obj)
  
  if (is.numeric(root_input)) {
    if (root_input <= 0 || root_input > max(tree$edge[, 1])) {
      stop("As a number, root_input must be a valid internal node number")
    }
    root_node <- root_input
  } else if (is.character(root_input)) {
    if (!root_input %in% tree$tip.label) {
      stop("As a label, root_input must be a valid tip label in the tree")
    }
    root_node <- which(tree$tip.label == root_input)
  } else {
    stop("root_input must be either a node number or a tip label")
  }
  
  parent_edge <- tree$edge[tree$edge[, 2] == root_node, , drop = FALSE]
  if (nrow(parent_edge) == 0) {
    stop("Could not find a valid edge for the specified root node or label")
  }
  original_length <- tree$edge.length[which(tree$edge[, 2] == root_node)]
  
  new_tree <- root(tree, node = root_node, resolve.root = TRUE)
  
  if (!is.null(original_length)) {
    new_tree$edge.length[1] <- original_length / 2
    new_tree$edge.length[2] <- original_length / 2
  }
  
  new_treedata <- treedata(phylo = new_tree, data = metadata)
  
  return(new_treedata)
}
