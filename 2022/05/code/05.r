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

# --- Day 5: Supply Stacks ---
#   The expedition can depart as soon as the final supplies have been unloaded from the ships. Supplies are stored in stacks of marked crates, but because the needed supplies are buried under many other crates, the crates need to be rearranged.
# 
# The ship has a giant cargo crane capable of moving crates between stacks. To ensure none of the crates get crushed or fall over, the crane operator will rearrange them in a series of carefully-planned steps. After the crates are rearranged, the desired crates will be at the top of each stack.
# 
# The Elves don't want to interrupt the crane operator during this delicate procedure, but they forgot to ask her which crate will end up where, and they want to be ready to unload them as soon as possible so they can embark.
# 
# They do, however, have a drawing of the starting stacks of crates and the rearrangement procedure (your puzzle input). For example:
# 
#     [D]    
# [N] [C]    
# [Z] [M] [P]
#  1   2   3 
# 
# move 1 from 2 to 1
# move 3 from 1 to 3
# move 2 from 2 to 1
# move 1 from 1 to 2
# In this example, there are three stacks of crates. Stack 1 contains two crates: crate Z is on the bottom, and crate N is on top. Stack 2 contains three crates; from bottom to top, they are crates M, C, and D. Finally, stack 3 contains a single crate, P.
# 
# Then, the rearrangement procedure is given. In each step of the procedure, a quantity of crates is moved from one stack to a different stack. In the first step of the above rearrangement procedure, one crate is moved from stack 2 to stack 1, resulting in this configuration:
# 
# [D]        
# [N] [C]    
# [Z] [M] [P]
#  1   2   3 
# In the second step, three crates are moved from stack 1 to stack 3. Crates are moved one at a time, so the first crate to be moved (D) ends up below the second and third crates:
# 
#         [Z]
#         [N]
#     [C] [D]
#     [M] [P]
#  1   2   3
# Then, both crates are moved from stack 2 to stack 1. Again, because crates are moved one at a time, crate C ends up below crate M:
# 
#         [Z]
#         [N]
# [M]     [D]
# [C]     [P]
#  1   2   3
# Finally, one crate is moved from stack 1 to stack 2:
# 
#         [Z]
#         [N]
#         [D]
# [C] [M] [P]
#  1   2   3
# The Elves just need to know which crate will end up on top of each stack; in this example, the top crates are C in stack 1, M in stack 2, and Z in stack 3, so you should combine these together and give the Elves the message CMZ.
# 
# After the rearrangement procedure completes, what crate ends up on top of each stack?

input_boxes <- readLines('05/data/input.txt')[1:8]
input_boxes <- 
  input_boxes %>% 
  lapply(., 
         function(x){
           # add in break symbol 
           gsub("(.{4})", "\\1_", x) %>% 
             strsplit(., '_') %>% 
             unlist %>% 
             trimws()
         }) 

input_boxes <- matrix(unlist(input_boxes), nrow=length(input_boxes), byrow=TRUE) 

# flip upside down for easier editing 
input_boxes <- input_boxes[nrow(input_boxes):1,]

input_boxes_2 <- input_boxes

# read in moves
input_moves <- readLines('05/data/input.txt')[-(1:10)]

input_moves <- 
  lapply(input_moves, 
         function(x){
           x %>% str_match_all("[0-9]+") %>% unlist() %>% as.numeric()
         })
input_moves <- matrix(unlist(input_moves), nrow=length(input_moves), byrow=TRUE) 
input_moves <- as.data.frame(input_moves)
names(input_moves) <- c('nbox', 'from', 'to')

# move across moves
for (i in 1:nrow(input_moves)) {
# for (i in 1:3) {
    
  x <- input_moves[i,]
  move <- x[1,1]
  from <- x[1,2]
  to <- x[1,3]
  
  # grab chunk that is moving 
  from_chunk <- input_boxes[,from]
  from_chunk <- from_chunk[from_chunk != '']
  from_chunk_app <- from_chunk[-c(length(from_chunk):(length(from_chunk)-move+1))]
  if (length(from_chunk_app) == 0) {
    from_chunk_app <- ''
  }
  # rboxes move one at a time
  from_chunk <- from_chunk[(length(from_chunk)):(length(from_chunk)-move+1)]
  
  # update new col
  to_chunk <- input_boxes[,to]
  to_chunk <- to_chunk[to_chunk != '']
  to_chunk <- append(to_chunk, from_chunk)
  
  if (nrow(input_boxes) < length(to_chunk)) {
    while (nrow(input_boxes) < length(to_chunk)) {
      input_boxes <- rbind(input_boxes, '')
    }
  }
  
  # update lengths of plug in vectors 
  while (length(from_chunk_app) < nrow(input_boxes)) {
    from_chunk_app <- append(from_chunk_app, '')
  }
  
  while (length(to_chunk) < nrow(input_boxes)) {
    to_chunk <- append(to_chunk, '')
  }
  
  
  input_boxes[,to] <- to_chunk
  input_boxes[,from] <- from_chunk_app
  
  print(i)
}

ans <- 
  apply(input_boxes, 2, 
      function(x){
        x <- x[x!= '']
        x[length(x)]
      })

ans <- substr(ans, 2, 2)
paste0(ans, collapse = '')

# --- Part Two ---
#   As you watch the crane operator expertly rearrange the crates, you notice the process isn't following your prediction.
# 
# Some mud was covering the writing on the side of the crane, and you quickly wipe it away. The crane isn't a CrateMover 9000 - it's a CrateMover 9001.
# 
# The CrateMover 9001 is notable for many new and exciting features: air conditioning, leather seats, an extra cup holder, and the ability to pick up and move multiple crates at once.
# 
# Again considering the example above, the crates begin in the same configuration:
# 
#     [D]    
# [N] [C]    
# [Z] [M] [P]
#  1   2   3 
# Moving a single crate from stack 2 to stack 1 behaves the same as before:
# 
# [D]        
# [N] [C]    
# [Z] [M] [P]
#  1   2   3 
# However, the action of moving three crates from stack 1 to stack 3 means that those three moved crates stay in the same order, resulting in this new configuration:
# 
#         [D]
#         [N]
#     [C] [Z]
#     [M] [P]
#  1   2   3
# Next, as both crates are moved from stack 2 to stack 1, they retain their order as well:
# 
#         [D]
#         [N]
# [C]     [Z]
# [M]     [P]
#  1   2   3
# Finally, a single crate is still moved from stack 1 to stack 2, but now it's crate C that gets moved:
#   
#   [D]
# [N]
# [Z]
# [M] [C] [P]
# 1   2   3
# In this example, the CrateMover 9001 has put the crates in a totally different order: MCD.
# 
# Before the rearrangement process finishes, update your simulation so that the Elves know where they should stand to be ready to unload the final supplies. After the rearrangement procedure completes, what crate ends up on top of each stack?

for (i in 1:nrow(input_moves)) {
  # for (i in 1:3) {
  
  x <- input_moves[i,]
  move <- x[1,1]
  from <- x[1,2]
  to <- x[1,3]
  
  # grab chunk that is moving 
  from_chunk <- input_boxes_2[,from]
  from_chunk <- from_chunk[from_chunk != '']
  from_chunk_app <- from_chunk[-c(length(from_chunk):(length(from_chunk)-move+1))]
  if (length(from_chunk_app) == 0) {
    from_chunk_app <- ''
  }
  # reverse it since boxes move all at the same time
  from_chunk <- from_chunk[(length(from_chunk)-move+1):(length(from_chunk))]
  
  # update new col
  to_chunk <- input_boxes_2[,to]
  to_chunk <- to_chunk[to_chunk != '']
  to_chunk <- append(to_chunk, from_chunk)
  
  if (nrow(input_boxes_2) < length(to_chunk)) {
    while (nrow(input_boxes_2) < length(to_chunk)) {
      input_boxes_2 <- rbind(input_boxes_2, '')
    }
  }
  
  # update lengths of plug in vectors 
  while (length(from_chunk_app) < nrow(input_boxes_2)) {
    from_chunk_app <- append(from_chunk_app, '')
  }
  
  while (length(to_chunk) < nrow(input_boxes_2)) {
    to_chunk <- append(to_chunk, '')
  }
  
  
  input_boxes_2[,to] <- to_chunk
  input_boxes_2[,from] <- from_chunk_app
  
  print(i)
}

ans_2 <- 
  apply(input_boxes_2, 2, 
        function(x){
          x <- x[x!= '']
          x[length(x)]
        })

ans_2 <- substr(ans_2, 2, 2)
paste0(ans_2, collapse = '')


