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

# --- Day 7: No Space Left On Device ---
#   You can hear birds chirping and raindrops hitting leaves as the expedition proceeds. Occasionally, you can even hear much louder sounds in the distance; how big do the animals get out here, anyway?
#   
#   The device the Elves gave you has problems with more than just its communication system. You try to run a system update:
#   
#   $ system-update --please --pretty-please-with-sugar-on-top
# Error: No space left on device
# Perhaps you can delete some files to make space for the update?
#   
#   You browse around the filesystem to assess the situation and save the resulting terminal output (your puzzle input). For example:
#   
#   $ cd /
#   $ ls
# dir a
# 14848514 b.txt
# 8504156 c.dat
# dir d
# $ cd a
# $ ls
# dir e
# 29116 f
# 2557 g
# 62596 h.lst
# $ cd e
# $ ls
# 584 i
# $ cd ..
# $ cd ..
# $ cd d
# $ ls
# 4060174 j
# 8033020 d.log
# 5626152 d.ext
# 7214296 k
# The filesystem consists of a tree of files (plain data) and directories (which can contain other directories or files). The outermost directory is called /. You can navigate around the filesystem, moving into or out of directories and listing the contents of the directory you're currently in.
# 
# Within the terminal output, lines that begin with $ are commands you executed, very much like some modern computers:
# 
# cd means change directory. This changes which directory is the current directory, but the specific result depends on the argument:
# cd x moves in one level: it looks in the current directory for the directory named x and makes it the current directory.
# cd .. moves out one level: it finds the directory that contains the current directory, then makes that directory the current directory.
# cd / switches the current directory to the outermost directory, /.
# ls means list. It prints out all of the files and directories immediately contained by the current directory:
# 123 abc means that the current directory contains a file named abc with size 123.
# dir xyz means that the current directory contains a directory named xyz.
# Given the commands and output in the example above, you can determine that the filesystem looks visually like this:
# 
# - / (dir)
#   - a (dir)
#     - e (dir)
#       - i (file, size=584)
#     - f (file, size=29116)
#     - g (file, size=2557)
#     - h.lst (file, size=62596)
#   - b.txt (file, size=14848514)
#   - c.dat (file, size=8504156)
#   - d (dir)
#     - j (file, size=4060174)
#     - d.log (file, size=8033020)
#     - d.ext (file, size=5626152)
#     - k (file, size=7214296)
# Here, there are four directories: / (the outermost directory), a and d (which are in /), and e (which is in a). These directories also contain files of various sizes.
# 
# Since the disk is full, your first step should probably be to find directories that are good candidates for deletion. To do this, you need to determine the total size of each directory. The total size of a directory is the sum of the sizes of the files it contains, directly or indirectly. (Directories themselves do not count as having any intrinsic size.)
# 
# The total sizes of the directories above can be found as follows:
# 
# The total size of directory e is 584 because it contains a single file i of size 584 and no other directories.
# The directory a has total size 94853 because it contains files f (size 29116), g (size 2557), and h.lst (size 62596), plus file i indirectly (a contains e which contains i).
# Directory d has total size 24933642.
# As the outermost directory, / contains every file. Its total size is 48381165, the sum of the size of every file.
# To begin, find all of the directories with a total size of at most 100000, then calculate the sum of their total sizes. In the example above, these directories are a and e; the sum of their total sizes is 95437 (94853 + 584). (As in this example, this process can count files more than once!)
# 
# Find all of the directories with a total size of at most 100000. What is the sum of the total sizes of those directories?


####### SO to reddit u/enelen
####### https://github.com/AdroMine/AdventOfCode/blob/main/2022/Day07/d07_solution.R
## recursion hurts my head too hard
## got lost i, the logical statementys I was trying to make above 
# input <- readLines('sample.txt')
input <- readLines('07/data/input.txt')

# define a node (with items child / parent / size)
node <- function(child = list(), parent = NA, size = 0){
  
  list(
    child  = child, 
    parent = parent,
    size   = size
  )
  
}

i <- 1
cur <- ""
file_system <- list()
while(i <= length(input)){
  
  line <- input[i]
  
  # command
  if(startsWith(line, '$ cd')){
    
    # Go UP
    if(line == '$ cd ..'){
      cur <- file_system[[cur]]$parent
      stopifnot(!is.na(cur))
      i <- i + 1
      
    } else if(line == '$ cd /'){
      # start
      file_system[['/']] <- node()
      i <- i + 1
      cur <- "/"
    } else {
      # go down, create new node
      dir_name <- gsub("\\$ cd ", "", line)
      cur <- file.path(cur, dir_name)
      i <- i + 1
      
      # # does folder exist
      # if(dir_name %in% names(file_system[[cur]]$child)){
      #   # yes, modify existing folder
      #   
      # } else {
      #   # no, create new folder
      #   file_system[[dir_name]] <- node(parent = cur)
      # }
      # cur <- dir_name
      # move to next line
      
    }
    
  } else if(line == '$ ls'){
    i <- i + 1 # move to next line
    line <- input[i]
    
    # keep reading until we reach next command
    while(i <= length(input) && !startsWith(line, '$')){
      # read file structure
      if(startsWith(line, 'dir ')){
        # directories, create new node
        
        dir_name <- file.path(cur, gsub("dir ", "", line))
        file_system[[dir_name]] <- node(parent = cur)
        
      } else {
        
        # files, 
        
        temp <- strsplit(line, " ")[[1]] # get size name
        size <- as.integer(temp[1])
        file_name <- temp[2]
        file_system[[cur]]$child[[file_name]] <- node(parent = cur,
                                                      size   = size)
      }
      
      i <- i + 1
      line <- input[i]
    }
  }
}

size_node <- function(node){
  size <- node$size
  for(child in node$child){
    size <- size + size_node(child)
  }
  size
}

# find sizes of each directory
for(dir in names(file_system)){
  
  node_size <- size_node(file_system[[dir]])
  file_system[[dir]]$size <- node_size
  
  parent <- file_system[[dir]]$parent
  while(!is.na(parent)){
    file_system[[parent]]$size <- file_system[[parent]]$size + node_size
    parent <- file_system[[parent]]$parent
  }
}


# Part 1
# directories with sum <= 100,000
all_dir_size <- sapply(file_system, function(x) x$size)
idx <- which(all_dir_size <= 100000)
sum(sapply(file_system[idx], function(x) x$size))


# Part 2
# disk space 7e7
# need space 3e7
available_space <- 7e7 - file_system[['/']]$size
delete_required <- 3e7 - available_space
unname(sort(all_dir_size[all_dir_size >= delete_required])[1])





# Solution 2 --------------------------------------------------------------

directories <- numeric()
path <- c()

for(line in input){
  
  temp <- strsplit(line, " ")[[1]]
  
  if(temp[2] == 'cd'){
    if(temp[3] == '..'){
      path <- head(path, -1)
    } else {
      path <- c(path, temp[3])
    }
  } else if(temp[2] == 'ls'){
    next 
  } else if(temp[1] == 'dir'){
    next 
  } else {
    # files
    size <- as.integer(temp[1])
    for(i in seq_along(path)){
      dir_name <- paste(path[1:i], collapse = '/')
      if(!dir_name %in% names(directories)){
        directories[dir_name] <- 0
      }
      directories[dir_name] <- directories[dir_name] + size
    }
  }
}

# Part 1
sum(directories[directories <= 1e5])

# Part 2
used <- 7e7 - directories['/']
to_free <- 3e7 - as.integer(used)
min(directories[directories >= to_free])

# 
# input <- readLines('07/data/input.txt')
# 
# dirTree <- data.frame(matrix(ncol = 3, nrow = 0))
# names(dirTree) <- c('file', 'size', 'dir1')
# dirLev <- 'dir1'
# dirTree <- 
#   dirTree %>%
#   mutate_if(is.logical, as.character)
# lev <- 1
# dirTree_name <- 'base'
# dirTree[1, ] <-  c('dir', 0, dirTree_name)
# 
# line <- input[986]
# 
# # for (line in input[1:985]) {
# for (line in input) {
#   
#   print(paste('LINEEEEEEEEE', line))
#   
#   if (grepl('^\\$', line) & !grepl('\\$ ls', line)) {
#     line_dir <- str_split(line, ' ')[[1]][3]
#     
#     if (line_dir == '/') {
#       next
#     } 
#     if (line_dir == '..') {
#       lev <- lev - 1
#       dirLev <- paste0('dir', lev)
#       print(paste('LEV IS ', lev))
#       if (lev == 1) {
#         dirTree_name <- 'base'
#       }
#       next
#     } 
#     
#     dirTree_name <- line_dir
#     lev <- lev + 1
#     dirLev <- paste0('dir', lev)
#     print(paste(dirLev, dirTree_name))
#     
#     dirTree_name_cd <- dirTree_name
#     
#   } else{
#     if (grepl('\\$ ls', line)) {
#       next 
#     }
#     if (grepl('^dir', line)) {
#       lev <- lev + 1
#       dirLev <- paste0('dir', lev)
#       
#       ls_df <- data.frame(matrix(ncol = 3, nrow = 1))
#       names(ls_df) <- c('file', 'size', dirLev)
#       
#       dirname <- str_split(line, ' ')[[1]][2]
#       
#       ls_df[1,] <- c('dir', 0, dirname)
#       
#       tmp <- dirTree[nrow(dirTree), !is.na(dirTree[nrow(dirTree),])]
#       
#       if (!(paste0('dir', lev-1) %in% names(tmp))) {
#         tmp <- dirTree[which(dirTree[,which(names(dirTree)==paste0('dir', lev-1))] == dirTree_name), ]
#         tmp <- tmp[nrow(tmp),]
#         tmp <- tmp[,!is.na(tmp)]
#       } else if(tmp[, which(names(tmp)==paste0('dir', lev-1))] != dirTree_name) {
#         tmp <- dirTree[which(dirTree[,which(names(dirTree)==paste0('dir', lev-1))] == dirTree_name), ]
#         # grab most recent
#         tmp <- tmp[, 1:which(names(tmp) == paste0('dir', lev-1))]
#         tmp <- tmp[nrow(tmp),]
#         tmp <- tmp[,!is.na(tmp)]
#       }
#       
#       ls_df[,names(tmp)[!(names(tmp) %in% names(ls_df))]] <- 
#         tmp[nrow(tmp),!(names(tmp) %in% names(ls_df))]
#       
#       dirTree <- bind_rows(dirTree, ls_df)
#       # reset dirlev since no cd
#       lev <- lev - 1
#       dirLev <- paste0('dir', lev)
#       next 
#     }
#     
#     print('file')
#     
#     ls_df <- data.frame(matrix(ncol = 3, nrow = 1))
#     names(ls_df) <- c('file', 'size', dirLev)
#     
#     size <- str_split(line, ' ')[[1]][1]
#     filename <- str_split(line, ' ')[[1]][2]
#     
#     ls_df[1,] <- c(filename, size, dirTree_name)
#     
#     tmp <- dirTree[which(dirTree[,names(dirTree)[which(names(dirTree) == dirLev)]] == 
#                            dirTree_name), ]
#     if ((lev-1) > 0) {
#       tmp <- tmp[, 1:which(names(tmp) == paste0('dir', lev-1))]
#       tmp <- tmp[nrow(tmp),]
#       tmp <- tmp[,!is.na(tmp)]
#     } else if (nrow(tmp)>1) {
#       tmp <- dirTree[dirTree$file != 'dir', ]
#     }
#     tmp <- tmp[nrow(tmp), !is.na(tmp[nrow(tmp),])]
#     
#     print(tmp)
#     
#     ls_df[,names(tmp)[!(names(tmp) %in% names(ls_df))]] <- 
#       tmp[nrow(tmp),!(names(tmp) %in% names(ls_df))]
#     
#     print(names(ls_df))
#     print(ls_df)
#     dirTree <- bind_rows(dirTree, ls_df)
#   }
#   
# }
# dirTree$size <- as.numeric(dirTree$size)
# 
# ans_mat <- matrix(nrow = 0, ncol = 3)
# for(x in names(dirTree)[grepl('dir', names(dirTree))]){
#   print(x)
#   
#   a <- 
#     dirTree %>%
#     filter(!is.na(!!as.symbol(x))) %>%
#     group_by(!!as.symbol(x)) %>% 
#     summarise(thres = sum(size)) %>% 
#     filter(thres <= 100000)
#   
#   a$lev <- x
#   
#   ans_mat <- rbind(ans_mat, as.matrix(a))
# }
# 
# ans_mat[,2] %>% as.numeric() %>% sum()
