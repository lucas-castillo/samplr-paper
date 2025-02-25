library(ranger)
library(abcrf)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(foreach)
library(doParallel)

# Def functions -----------------------------------------------------------
find_leaf <- function(tree, datum){
  currentNodeID <- 0
  terminal <- F
  while (!terminal){
    currentNode <- tree[tree$nodeID == currentNodeID, ]
    terminal <- currentNode$terminal
    if (!terminal){
      if (datum[[currentNode$splitvarName]] > currentNode$splitval){
        currentNodeID <- currentNode$rightChild
      } else{
        currentNodeID <- currentNode$leftChild
      }
    }
  }
  return(c(currentNodeID, as.numeric(currentNode$prediction)))
}
get_tree_memory <- function(tree, training, dv_name){
  options <- levels(training[[dv_name]])
  M <- matrix(0, nrow=nrow(tree), ncol=length(options))
  
  for (row in 1:nrow(training)){
    datum <- training[row,]
    RES <- find_leaf(tree, datum)
    true_label <- as.numeric(datum[[dv_name]])
    M[RES[1], true_label] <- M[RES[1], true_label] + 1
  }
  M
}

get_forest_memory <- function(abcrf_model, training, n_trees=NULL){
  forest <- abcrf_model$model.rf
  # add lda -----
  training.p <- cbind(training, predict(abcrf_model$model.lda, training)$x)
  if (is.null(n_trees)){
    n_trees <- forest$num.trees
  }
  n_labels <- length(levels(forest$predictions))  
  dv_name <- abcrf_model$model.rf$dependent.variable.name
  M <- foreach (
    treeIndex=1:n_trees, 
    .packages = c("abcrf", "ranger"),
    .export = c("get_tree_memory", "find_leaf")
    ) %dopar% {
    tree <- treeInfo(forest, treeIndex)
    get_tree_memory(tree, training.p, dv_name)
  }
  M
}

get_posterior <- function(dat, mem, abcrf_model){
  dat.p <- cbind(dat, predict(abcrf_model$model.lda, dat)$x)
  compute_posterior <- function(row){
    leaves <- sapply(1:abcrf_model$model.rf$num.trees, FUN = \(k){find_leaf(treeInfo(abcrf_model$model.rf, k), datum = dat.p[row,])[1]})
    RES <- t(sapply(1:abcrf_model$model.rf$num.trees, FUN = \(x){mem[[x]][leaves[x],]}))
    RES.n <- t(apply(RES, 1, \(x){x/sum(x)}))
    apply(RES.n, 2, sum) / nrow(RES.n)
  }
  
  temp <- foreach(
    r=1:nrow(dat.p), 
    .combine = "rbind", 
    .packages = c("ranger", "abcrf"),
    .export = c("find_leaf")
  ) %dopar% {
    compute_posterior(r)
  }
  temp
}
