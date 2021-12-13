# --- Day 8: Seven Segment Search ---
#   You barely reach the safety of the cave when the whale smashes into the cave mouth, collapsing it. Sensors indicate another exit to this cave at a much greater depth, so you have no choice but to press on.
# 
# As your submarine slowly makes its way through the cave system, you notice that the four-digit seven-segment displays in your submarine are malfunctioning; they must have been damaged during the escape. You'll be in a lot of trouble without them, so you'd better figure out what's wrong.
# 
# Each digit of a seven-segment display is rendered by turning on or off any of seven segments named a through g:
# 
#   0:      1:      2:      3:      4:
#  aaaa    ....    aaaa    aaaa    ....
# b    c  .    c  .    c  .    c  b    c
# b    c  .    c  .    c  .    c  b    c
#  ....    ....    dddd    dddd    dddd
# e    f  .    f  e    .  .    f  .    f
# e    f  .    f  e    .  .    f  .    f
#  gggg    ....    gggg    gggg    ....
# 
#   5:      6:      7:      8:      9:
#  aaaa    aaaa    aaaa    aaaa    aaaa
# b    .  b    .  .    c  b    c  b    c
# b    .  b    .  .    c  b    c  b    c
#  dddd    dddd    ....    dddd    dddd
# .    f  e    f  .    f  e    f  .    f
# .    f  e    f  .    f  e    f  .    f
#  gggg    gggg    ....    gggg    gggg
# So, to render a 1, only segments c and f would be turned on; the rest would be off. To render a 7, only segments a, c, and f would be turned on.
# 
# The problem is that the signals which control the segments have been mixed up on each display. The submarine is still trying to display numbers by producing output on signal wires a through g, but those wires are connected to segments randomly. Worse, the wire/segment connections are mixed up separately for each four-digit display! (All of the digits within a display use the same connections, though.)
# 
# So, you might know that only signal wires b and g are turned on, but that doesn't mean segments b and g are turned on: the only digit that uses two segments is 1, so it must mean segments c and f are meant to be on. With just that information, you still can't tell which wire (b/g) goes to which segment (c/f). For that, you'll need to collect more information.
# 
# For each display, you watch the changing signals for a while, make a note of all ten unique signal patterns you see, and then write down a single four digit output value (your puzzle input). Using the signal patterns, you should be able to work out which pattern corresponds to which digit.
# 
# For example, here is what you might see in a single entry in your notes:
#   
#   acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
#   cdfeb fcadb cdfeb cdbaf
# (The entry is wrapped here to two lines so it fits; in your notes, it will all be on a single line.)
# 
# Each entry consists of ten unique signal patterns, a | delimiter, and finally the four digit output value. Within an entry, the same wire/segment connections are used (but you don't know what the connections actually are). The unique signal patterns correspond to the ten different ways the submarine tries to render a digit using the current wire/segment connections. Because 7 is the only digit that uses three segments, dab in the above example means that to render a 7, signal lines d, a, and b are on. Because 4 is the only digit that uses four segments, eafb means that to render a 4, signal lines e, a, f, and b are on.
# 
# Using this information, you should be able to work out which combination of signal wires corresponds to each of the ten digits. Then, you can decode the four digit output value. Unfortunately, in the above example, all of the digits in the output value (cdfeb fcadb cdfeb cdbaf) use five segments and are more difficult to deduce.
# 
# For now, focus on the easy digits. Consider this larger example:
# 
# be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
# fdgacbe cefdb cefbgd gcbe
# edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |
# fcgedb cgb dgebacf gc
# fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |
# cg cg fdcagb cbg
# fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |
# efabcd cedba gadfec cb
# aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |
# gecf egdcabf bgf bfgea
# fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |
# gebdcfa ecba ca fadegcb
# dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |
# cefg dcbef fcge gbcadfe
# bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |
# ed bcgafe cdgba cbgef
# egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |
# gbdfcae bgc cg cgb
# gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |
# fgae cfgab fg bagce
# Because the digits 1, 4, 7, and 8 each use a unique number of segments, you should be able to tell which combinations of signals correspond to those digits. Counting only digits in the output values (the part after | on each line), in the above example, there are 26 instances of digits that use a unique number of segments (highlighted above).
# 
# In the output values, how many times do digits 1, 4, 7, or 8 appear?
# 
# Your puzzle answer was 352.
# 
# --- Part Two ---
# Through a little deduction, you should now be able to determine the remaining digits. Consider again the first example above:
# 
# acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
# cdfeb fcadb cdfeb cdbaf
# After some careful analysis, the mapping between signal wires and segments only make sense in the following configuration:
# 
#  dddd
# e    a
# e    a
#  ffff
# g    b
# g    b
#  cccc
# So, the unique signal patterns would correspond to the following digits:
# 
# acedgfb: 8
# cdfbe: 5
# gcdfa: 2
# fbcad: 3
# dab: 7
# cefabd: 9
# cdfgeb: 6
# eafb: 4
# cagedb: 0
# ab: 1
# Then, the four digits of the output value can be decoded:
# 
# cdfeb: 5
# fcadb: 3
# cdfeb: 5
# cdbaf: 3
# Therefore, the output value for this entry is 5353.
# 
# Following this same process for each entry in the second, larger example above, the output value of each entry can be determined:
# 
# fdgacbe cefdb cefbgd gcbe: 8394
# fcgedb cgb dgebacf gc: 9781
# cg cg fdcagb cbg: 1197
# efabcd cedba gadfec cb: 9361
# gecf egdcabf bgf bfgea: 4873
# gebdcfa ecba ca fadegcb: 8418
# cefg dcbef fcge gbcadfe: 4548
# ed bcgafe cdgba cbgef: 1625
# gbdfcae bgc cg cgb: 8717
# fgae cfgab fg bagce: 4315
# Adding all of the output values in this larger example produces 61229.
# 
# For each entry, determine all of the wire/segment connections and decode the four-digit output values. What do you get if you add up all of the output values?
# 
# Your puzzle answer was 936117.



library(tidyverse)
library(readr)
library (plyr)

# d8 - 

d8_base <-  
  readLines("Documents/personal_repos/advent_of_code_2021/data/day_8/day8_AoC.txt")

d8_input <- 
  sapply(d8_base,
         function(x){
           str_split(x, '\\|')[[1]][1]
         }) %>% 
  unlist() %>% 
  trimws() %>% 
  sapply(
    function(x){
      str_split(x, ' ')
    }) %>% 
  unlist() 

d8_input <- 
  as.data.frame(
    matrix(d8_input,
           nrow=length(d8_base),
           byrow=TRUE)
  ) %>%
  as.data.frame()  

d8_output <- 
  sapply(d8_base,
         function(x){
           str_split(x, '\\|')[[1]][2]
         }) %>% 
  unlist() %>% 
  trimws() %>% 
  sapply(
    function(x){
      str_split(x, ' ')
    }) %>% 
  unlist() 

d8_output <- 
  as.data.frame(
    matrix(d8_output,
           nrow=length(d8_base),
           byrow=TRUE)
  ) %>%
  as.data.frame()  

num_check_basic <- 
  function(x){
    
    x_base <- vector(length = 4)
    names(x_base) <- c(1, 4, 7, 8)
    
    len_check <- 
      sapply(x, nchar) %>% unlist
    
    x_base['1'] <- length(len_check[len_check == 2])
    x_base['4'] <- length(len_check[len_check == 4])
    x_base['7'] <- length(len_check[len_check == 3])
    x_base['8'] <- length(len_check[len_check == 7])
    
    return(x_base)
  }

total_basic <- 
  apply(d8_output, 
        1, 
        num_check_basic) %>% 
  rowSums() %>% 
  sum()

# determine mapping and output values



d8_full <- 
  cbind(d8_input, d8_output)

signal_mapping <- 
  list()

for(row in 1:nrow(d8_full)){
  
  print(row)
  
  x <- d8_full[row,] %>% as.character()
  
  one_lets <- 
    unique(x[nchar(x) == 2]) %>% 
    strsplit(., '')
  one_lets <- one_lets[[1]]
  
  four_lets <- 
    unique(x[nchar(x) == 4]) %>% 
    strsplit(., '')
  four_lets <- four_lets[[1]]
  
  seven_lets <- 
    unique(x[nchar(x) == 3]) %>% 
    strsplit(., '')
  seven_lets <- seven_lets[[1]]
  
  eight_lets <- 
    unique(x[nchar(x) == 7]) %>% 
    strsplit(., '')
  eight_lets <- eight_lets[[1]]
  
  signal_mapping_tmp <- 
    vector(length = 7)
  
  # fill top position 
  signal_mapping_tmp[1] <- 
    seven_lets[!(seven_lets %in% one_lets)]
  
  # find possible position 2 and 4
  two_four_poss <- 
    four_lets[!(four_lets %in% one_lets)]
  
  # pos_345
  pos_345_gr <- 
    unique(x[nchar(x) == 6]) %>% 
    strsplit(., '') %>% 
    lapply(sort) %>% 
    unique()
  
  pos_345 <- 
    c(
      pos_345_gr[[1]][!(pos_345_gr[[1]] %in% pos_345_gr[[2]])],
      pos_345_gr[[2]][!(pos_345_gr[[2]] %in% pos_345_gr[[1]])],
      pos_345_gr[[1]][!(pos_345_gr[[1]] %in% pos_345_gr[[3]])],
      pos_345_gr[[3]][!(pos_345_gr[[3]] %in% pos_345_gr[[1]])],
      pos_345_gr[[2]][!(pos_345_gr[[2]] %in% pos_345_gr[[3]])],
      pos_345_gr[[3]][!(pos_345_gr[[3]] %in% pos_345_gr[[2]])]
    ) %>% 
    unique()
  
  # fill position 4 with two_four_poss overlap with pos_345
  signal_mapping_tmp[4] <- 
    two_four_poss[(two_four_poss %in% pos_345)]
  # then 2
  signal_mapping_tmp[2] <- 
    two_four_poss[!(two_four_poss %in% signal_mapping_tmp)]
  
  # possible 5 and 6
  pos_56_gr <- 
    unique(x[nchar(x) == 5]) %>% 
    strsplit(., '') %>% 
    lapply(sort) %>% 
    unique()
  pos_56_gr <- 
    pos_56_gr[
      lapply(pos_56_gr, function(x){!(any(x %in% signal_mapping_tmp[2]))}) %>% unlist()
    ]
  
  pos_56 <- 
    c(
      pos_56_gr[[1]][!(pos_56_gr[[1]] %in% pos_56_gr[[2]])],
      pos_56_gr[[2]][!(pos_56_gr[[2]] %in% pos_56_gr[[1]])]
    ) %>% 
    unique()
  
  # fill position 5 with pos_345 and pos_56
  signal_mapping_tmp[5] <- 
    pos_56[(pos_56 %in% pos_345)]
  signal_mapping_tmp[6] <- 
    pos_56[!(pos_56 %in% pos_345)]
  
  # fill remaining pos_345
  signal_mapping_tmp[3] <- 
    pos_345[!(pos_345 %in% signal_mapping_tmp)]
  
  signal_mapping_tmp[7] <- 
    letters[1:7][!( letters[1:7] %in% signal_mapping_tmp )]
  
  
  # fill in grid
  signal_mapping[[row]] <- 
    signal_mapping_tmp
}

signal_mapping

zero <- lapply(signal_mapping, function(x){x[-4]})
one <- lapply(signal_mapping, function(x){x[c(3, 6)]})
two <- lapply(signal_mapping, function(x){x[c(1, 3:5, 7)]}) 
three <- lapply(signal_mapping, function(x){x[c(1, 3:4, 6:7)]}) 
four <- lapply(signal_mapping, function(x){x[c(2:4, 6)]}) 
five <- lapply(signal_mapping, function(x){x[c(1:2, 4, 6:7)]}) 
six <- lapply(signal_mapping, function(x){x[c(1:2, 4:7)]}) 
seven <- lapply(signal_mapping, function(x){x[c(1, 3, 6)]}) 
eight <- lapply(signal_mapping, function(x){x[c(1:7)]})
nine <- lapply(signal_mapping, function(x){x[c(1:4, 6:7)]}) 

outputs_let_full <- vector()
for(row in 1:nrow(d8_output)){
  
  print(row)
  
  x <- d8_output[row,] %>% as.character()
  
  outputs_let <- 
    sapply(x, 
         function(x){
           strsplit(x, '') %>% 
             lapply(sort) 
         }) %>% 
    lapply(function(x){paste(x, collapse = "")}) %>%
    unlist()
  
  signal_mapping[[row]]
  
  outputs_let[outputs_let == paste(sort(zero[[row]]), collapse = "")] <- 0
  outputs_let[outputs_let == paste(sort(one[[row]]), collapse = "")] <- 1
  outputs_let[outputs_let == paste(sort(two[[row]]), collapse = "")] <- 2
  outputs_let[outputs_let == paste(sort(three[[row]]), collapse = "")] <- 3
  outputs_let[outputs_let == paste(sort(four[[row]]), collapse = "")] <- 4
  outputs_let[outputs_let == paste(sort(five[[row]]), collapse = "")] <- 5
  outputs_let[outputs_let == paste(sort(six[[row]]), collapse = "")] <- 6
  outputs_let[outputs_let == paste(sort(seven[[row]]), collapse = "")] <- 7
  outputs_let[outputs_let == paste(sort(eight[[row]]), collapse = "")] <- 8
  outputs_let[outputs_let == paste(sort(nine[[row]]), collapse = "")] <- 9
  
  outputs_let <- paste(outputs_let, collapse = "") %>% as.numeric()
  
  outputs_let_full <- c(outputs_let_full, outputs_let)
}

sum(outputs_let_full)






