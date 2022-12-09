# setup 
rm(list = ls())
# set to repo wd
setwd('/Users/andrewdavis/Documents/git_repos/advent_of_code/2022')

# load packages
require(Hmisc)
library(data.table)
require(tidyverse)

std <- function(x) sd(x)/sqrt(length(x))

mytheme <- theme_bw() + theme(axis.line = element_line(colour = "black"),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              #panel.border = element_blank(),
                              #panel.background = element_blank(),
                              axis.text=element_text(size=11, color = 'black'),
                              axis.title=element_text(size=12),
                              legend.text=element_text(size=12))

# --- Day 8: Treetop Tree House ---
#   The expedition comes across a peculiar patch of tall trees all planted carefully in a grid. The Elves explain that a previous expedition planted these trees as a reforestation effort. Now, they're curious if this would be a good location for a tree house.
# 
# First, determine whether there is enough tree cover here to keep a tree house hidden. To do this, you need to count the number of trees that are visible from outside the grid when looking directly along a row or column.
# 
# The Elves have already launched a quadcopter to generate a map with the height of each tree (your puzzle input). For example:
# 
# 30373
# 25512
# 65332
# 33549
# 35390
# Each tree is represented as a single digit whose value is its height, where 0 is the shortest and 9 is the tallest.
# 
# A tree is visible if all of the other trees between it and an edge of the grid are shorter than it. Only consider trees in the same row or column; that is, only look up, down, left, or right from any given tree.
# 
# All of the trees around the edge of the grid are visible - since they are already on the edge, there are no trees to block the view. In this example, that only leaves the interior nine trees to consider:
# 
# The top-left 5 is visible from the left and top. (It isn't visible from the right or bottom since other trees of height 5 are in the way.)
# The top-middle 5 is visible from the top and right.
# The top-right 1 is not visible from any direction; for it to be visible, there would need to only be trees of height 0 between it and an edge.
# The left-middle 5 is visible, but only from the right.
# The center 3 is not visible from any direction; for it to be visible, there would need to be only trees of at most height 2 between it and an edge.
# The right-middle 3 is visible from the right.
# In the bottom row, the middle 5 is visible, but the 3 and 4 are not.
# With 16 trees visible on the edge and another 5 visible in the interior, a total of 21 trees are visible in this arrangement.
# 
# Consider your map; how many trees are visible from outside the grid?
  
input <- readLines('08/data/input.txt')

treeCheckOutside <- 
  function(x){
    # x <- input_mat[7,]
    xcheck <- rep(0, length(x))
    
    topH <- max(x)
    topTrees <- which(x == topH)[1]
    x[topTrees[1]] <- 0
    x <- x[1:topTrees]
    xcheck[topTrees[1]] <- topH
    if(topH == 0) xcheck[topTrees[1]] <- 1
    
    while (topTrees != 1) {
      
      topH <- max(x)
      topTrees <- which(x == topH)[1]
      x[topTrees[1]] <- 0
      x <- x[1:topTrees]
      xcheck[topTrees[1]] <- topH
      if(topH == 0) xcheck[topTrees[1]] <- 1
    }
    
    return(xcheck)
  }



# try on full data
input_mat <- 
  lapply(input, 
         function(x){
           str_split(x, '') %>% unlist() %>% as.numeric()
           }
         ) %>% 
  unlist() 
input_mat <- matrix(input_mat, nrow = length(input), byrow = T)

# run on matrix
leftRow <- apply(input_mat[,], 1, treeCheckOutside) %>% t()
rightRow <- apply(input_mat[, ncol(input_mat):1], 1, treeCheckOutside) %>% t()
rightRow <- rightRow[, ncol(input_mat):1]
 
topCol <- apply(input_mat[,], 2, treeCheckOutside) 
botCol <- apply(input_mat[ncol(input_mat):1, ], 2, treeCheckOutside) 
botCol <- botCol[nrow(input_mat):1, ] 

ans <- (leftRow | rightRow | topCol | botCol)
sum(ans) 

# --- Part Two ---
#   Content with the amount of tree cover available, the Elves just need to know the best spot to build their tree house: they would like to be able to see a lot of trees.
# 
# To measure the viewing distance from a given tree, look up, down, left, and right from that tree; stop if you reach an edge or at the first tree that is the same height or taller than the tree under consideration. (If a tree is right on the edge, at least one of its viewing distances will be zero.)
# 
# The Elves don't care about distant trees taller than those found by the rules above; the proposed tree house has large eaves to keep it dry, so they wouldn't be able to see higher than the tree house anyway.
# 
# In the example above, consider the middle 5 in the second row:
#   
#   30373
# 25512
# 65332
# 33549
# 35390
# Looking up, its view is not blocked; it can see 1 tree (of height 3).
# Looking left, its view is blocked immediately; it can see only 1 tree (of height 5, right next to it).
# Looking right, its view is not blocked; it can see 2 trees.
# Looking down, its view is blocked eventually; it can see 2 trees (one of height 3, then the tree of height 5 that blocks its view).
# A tree's scenic score is found by multiplying together its viewing distance in each of the four directions. For this tree, this is 4 (found by multiplying 1 * 1 * 2 * 2).
# 
# However, you can do even better: consider the tree of height 5 in the middle of the fourth row:
# 
# 30373
# 25512
# 65332
# 33549
# 35390
# Looking up, its view is blocked at 2 trees (by another tree with a height of 5).
# Looking left, its view is not blocked; it can see 2 trees.
# Looking down, its view is also not blocked; it can see 1 tree.
# Looking right, its view is blocked at 2 trees (by a massive tree of height 9).
# This tree's scenic score is 8 (2 * 2 * 1 * 2); this is the ideal spot for the tree house.
# 
# Consider each tree on your map. What is the highest scenic score possible for any tree?

## full matrix of all 4
full_mat <-  input_mat
full_mat_score <- full_mat
for(i in 1:nrow(full_mat)){
  for(j in 1:nrow(full_mat)){
    
    # i = 3
    # j = 2
    print(i)
    print(j)
    a <- full_mat[i,j]
    
    
    if(a == 0) next
    
    # left score 
    if (j == 1) {
      sight <- which(full_mat[i,1:(j)] >= a)
    } else{
      sight <- which(full_mat[i,1:(j-1)] >= a)
    }
    if(j == 1) {
      Lscore <- 0
    } else if(length(sight) == 0){
      Lscore <- length(1:j) - 1
    } else{ 
      Lscore <- j - max(sight)
    }
    
    # right score 
    if (j == ncol(full_mat)) {
      sight <- which(full_mat[i,(j):nrow(full_mat)] >= a) + ncol(full_mat) -1
    } else{
      sight <- which(full_mat[i,(j+1):nrow(full_mat)] >= a) + j
    }
    if(j == ncol(full_mat)) {
      Rscore <- 0
    } else if(length(sight) == 0){
      Rscore <- length(j:nrow(full_mat)) - 1
    } else{ 
      Rscore <- min(sight) - j
    }
    
    # top score 
    if (i == 1) {
      sight <- which(full_mat[1:i,j] >= a)
    } else{
      sight <- which(full_mat[1:(i-1),j] >= a)
    }
    if(i == 1) {
      Tscore <- 0
    } else if(length(sight) == 0){
      Tscore <- length(1:i) - 1
    } else{ 
      Tscore <- i - max(sight) 
    }
    
    # bootom score 
    if (i == nrow(full_mat)) {
      sight <- which(full_mat[(i):ncol(full_mat),j] >= a) + nrow(full_mat) -1
    } else{
      sight <- which(full_mat[(i+1):ncol(full_mat),j] >= a) + i
    }
    if(i == nrow(full_mat)) {
      Bscore <- 0
    } else if(length(sight) == 0){
      Bscore <- length(i:ncol(full_mat)) - 1
    } else{ 
      Bscore <- min(sight) - i
    }
    
    score <- 
      Lscore * Rscore * Tscore * Bscore
    
    full_mat_score[i, j] <- score
  }
}

max(full_mat_score)

# test on sample
samp <- readLines('08/data/sample.txt')

samp_mat <- 
  lapply(samp, 
         function(x){
           str_split(x, '') %>% unlist() %>% as.numeric()
         }
  ) %>% 
  unlist() 
samp_mat <- matrix(samp_mat, nrow = length(samp), byrow = T)


# run on matrix
leftRow <- apply(samp_mat[,], 1, treeCheckOutside) %>% t()
rightRow <- apply(samp_mat[, ncol(samp_mat):1], 1, treeCheckOutside) %>% t()
rightRow <- rightRow[, ncol(samp_mat):1]

topCol <- apply(samp_mat[,], 2, treeCheckOutside) 
botCol <- apply(samp_mat[ncol(samp_mat):1, ], 2, treeCheckOutside) 
botCol <- botCol[nrow(samp_mat):1, ] 

ans <- (leftRow | rightRow | topCol | botCol)
sum(ans != 0) 

## full matrix of all 4
full_mat <-  samp_mat
full_mat_score <- full_mat
for(i in 1:nrow(full_mat)){
  for(j in 1:nrow(full_mat)){
    
    # i = 3
    # j = 2
    print(i)
    print(j)
    a <- full_mat[i,j]
    
    
    if(a == 0) next
    
    # left score 
    if (j == 1) {
      sight <- which(full_mat[i,1:(j)] >= a)
    } else{
      sight <- which(full_mat[i,1:(j-1)] >= a)
    }
    if(j == 1) {
      Lscore <- 0
    } else if(length(sight) == 0){
      Lscore <- length(1:j) - 1
    } else{ 
      Lscore <- j - max(sight)
    }
    
    # right score 
    if (j == ncol(full_mat)) {
      sight <- which(full_mat[i,(j):nrow(full_mat)] >= a) + ncol(full_mat) -1
    } else{
      sight <- which(full_mat[i,(j+1):nrow(full_mat)] >= a) + j
    }
    if(j == ncol(full_mat)) {
      Rscore <- 0
    } else if(length(sight) == 0){
      Rscore <- length(j:nrow(full_mat)) - 1
    } else{ 
      Rscore <- min(sight) - j
    }
    
    # top score 
    if (i == 1) {
      sight <- which(full_mat[1:i,j] >= a)
    } else{
      sight <- which(full_mat[1:(i-1),j] >= a)
    }
    if(i == 1) {
      Tscore <- 0
    } else if(length(sight) == 0){
      Tscore <- length(1:i) - 1
    } else{ 
      Tscore <- i - max(sight) 
    }
    
    # bootom score 
    if (i == nrow(full_mat)) {
      sight <- which(full_mat[(i):ncol(full_mat),j] >= a) + nrow(full_mat) -1
    } else{
      sight <- which(full_mat[(i+1):ncol(full_mat),j] >= a) + i
    }
    if(i == nrow(full_mat)) {
      Bscore <- 0
    } else if(length(sight) == 0){
      Bscore <- length(i:ncol(full_mat)) - 1
    } else{ 
      Bscore <- min(sight) - i
    }
    
    score <- 
      Lscore * Rscore * Tscore * Bscore
    
    full_mat_score[i, j] <- score
  }
}

max(full_mat_score)
