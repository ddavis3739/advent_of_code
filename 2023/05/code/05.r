# setup 
rm(list = ls())
# set to repo wd
setwd('/Users/andrewdavis/Documents/git_repos/advent_of_code/2023')

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

# --- Day 5: If You Give A Seed A Fertilizer ---
#   You take the boat and find the gardener right where you were told he would be: managing a giant "garden" that looks more to you like a farm.
# 
# "A water source? Island Island is the water source!" You point out that Snow Island isn't receiving any water.
# 
# "Oh, we had to stop the water because we ran out of sand to filter it with! Can't make snow with dirty water. Don't worry, I'm sure we'll get more sand soon; we only turned off the water a few days... weeks... oh no." His face sinks into a look of horrified realization.
# 
# "I've been so busy making sure everyone here has food that I completely forgot to check why we stopped getting more sand! There's a ferry leaving soon that is headed over in that direction - it's much faster than your boat. Could you please go check it out?"
# 
# You barely have time to agree to this request when he brings up another. "While you wait for the ferry, maybe you can help us with our food production problem. The latest Island Island Almanac just arrived and we're having trouble making sense of it."
# 
# The almanac (your puzzle input) lists all of the seeds that need to be planted. It also lists what type of soil to use with each kind of seed, what type of fertilizer to use with each kind of soil, what type of water to use with each kind of fertilizer, and so on. Every type of seed, soil, fertilizer and so on is identified with a number, but numbers are reused by each category - that is, soil 123 and fertilizer 123 aren't necessarily related to each other.
# 
# For example:
#   
#   seeds: 79 14 55 13
# 
# seed-to-soil map:
#   50 98 2
# 52 50 48
# 
# soil-to-fertilizer map:
#   0 15 37
# 37 52 2
# 39 0 15
# 
# fertilizer-to-water map:
#   49 53 8
# 0 11 42
# 42 0 7
# 57 7 4
# 
# water-to-light map:
#   88 18 7
# 18 25 70
# 
# light-to-temperature map:
#   45 77 23
# 81 45 19
# 68 64 13
# 
# temperature-to-humidity map:
#   0 69 1
# 1 0 69
# 
# humidity-to-location map:
#   60 56 37
# 56 93 4
# The almanac starts by listing which seeds need to be planted: seeds 79, 14, 55, and 13.
# 
# The rest of the almanac contains a list of maps which describe how to convert numbers from a source category into numbers in a destination category. That is, the section that starts with seed-to-soil map: describes how to convert a seed number (the source) to a soil number (the destination). This lets the gardener and his team know which soil to use with which seeds, which water to use with which fertilizer, and so on.
# 
# Rather than list every source number and its corresponding destination number one by one, the maps describe entire ranges of numbers that can be converted. Each line within a map contains three numbers: the destination range start, the source range start, and the range length.
# 
# Consider again the example seed-to-soil map:
#   
#   50 98 2
# 52 50 48
# The first line has a destination range start of 50, a source range start of 98, and a range length of 2. This line means that the source range starts at 98 and contains two values: 98 and 99. The destination range is the same length, but it starts at 50, so its two values are 50 and 51. With this information, you know that seed number 98 corresponds to soil number 50 and that seed number 99 corresponds to soil number 51.
# 
# The second line means that the source range starts at 50 and contains 48 values: 50, 51, ..., 96, 97. This corresponds to a destination range starting at 52 and also containing 48 values: 52, 53, ..., 98, 99. So, seed number 53 corresponds to soil number 55.
# 
# Any source numbers that aren't mapped correspond to the same destination number. So, seed number 10 corresponds to soil number 10.
# 
# So, the entire list of seed numbers and their corresponding soil numbers looks like this:
# 
# seed  soil
# 0     0
# 1     1
# ...   ...
# 48    48
# 49    49
# 50    52
# 51    53
# ...   ...
# 96    98
# 97    99
# 98    50
# 99    51
# With this map, you can look up the soil number required for each initial seed number:
# 
# Seed number 79 corresponds to soil number 81.
# Seed number 14 corresponds to soil number 14.
# Seed number 55 corresponds to soil number 57.
# Seed number 13 corresponds to soil number 13.
# The gardener and his team want to get started as soon as possible, so they'd like to know the closest location that needs a seed. Using these maps, find the lowest location number that corresponds to any of the initial seeds. To do this, you'll need to convert each seed number through other categories until you can find its corresponding location number. In this example, the corresponding types are:
# 
# Seed 79, soil 81, fertilizer 81, water 81, light 74, temperature 78, humidity 78, location 82.
# Seed 14, soil 14, fertilizer 53, water 49, light 42, temperature 42, humidity 43, location 43.
# Seed 55, soil 57, fertilizer 57, water 53, light 46, temperature 82, humidity 82, location 86.
# Seed 13, soil 13, fertilizer 52, water 41, light 34, temperature 34, humidity 35, location 35.
# So, the lowest location number in this example is 35.
# 
# What is the lowest location number that corresponds to any of the initial seed numbers?

input <- readLines('05/data/input.in') 
# input <- readLines('05/data/ex1.txt')

input_cat <- 
  input %>%
  as_tibble() %>%
  mutate(value = na_if(value, '')) %>% 
  group_by(id_Group =cumsum(is.na(value))+1) %>% 
  na.omit() %>% 
  summarise(value1 =list(value)) %>% 
  pull(value1)

input_catNames <- 
  lapply(input_cat, function(x){
    x <- x[[1]][1] %>% 
      str_split(., ':')
    unlist(x)[1]
  }) %>% unlist()

# get seed information
seeds <- 
  input_cat[[which(input_catNames=='seeds')]]

seeds <- 
  str_split(seeds, ':') %>% unlist() %>% trimws()

seeds <- 
  str_split(seeds, ' ') %>% unlist() %>% trimws()

seeds_df <- 
  data_frame(
    source = 'seed',
    source_range = seeds[seeds != 'seeds'] %>% as.numeric(), 
    seed_inital = seeds[seeds != 'seeds'] %>% as.numeric()
  )

# get mapping information
mapping <- 
  input_cat[which(input_catNames!='seeds')]

mapping_names <- 
  input_catNames[which(input_catNames!='seeds')]

mapping_vals <- lapply(mapping, '[', -1)

mapping_vals_df <- 
  lapply(mapping_vals, function(x){
  sapply(x, function(y){
    str_split(y, ' ') %>% unlist()
  })
}) %>% 
  unlist() %>% 
  as.numeric()

mapping_vals_df <- 
  matrix(mapping_vals_df, ncol = 3, byrow = T) %>% 
  as.data.frame()

names(mapping_vals_df) <- 
  c(
    'destination_range_start',
    'source_range_start',
    'range_length'
  )

mapping_lengths <- 
  lapply(mapping_vals, function(x){
    length(x)
  }) %>% 
  unlist()

mapping_vals_df$map <- 
  rep(mapping_names, mapping_lengths)
mapping_vals_df$map <- gsub(' map', '', mapping_vals_df$map)

map_spread <- 
  lapply(mapping_vals_df$map, function(x){
  str_split(x, '-')
}) %>% unlist()
map_spread <- map_spread[map_spread!='to'] 

map_spread <- 
  matrix(map_spread, ncol = 2, byrow = T) %>% 
  as.data.frame()

names(map_spread) <- 
  c(
    'source',
    'destination'
  )

mapping_df <- 
  mapping_vals_df %>% 
  cbind(map_spread)

mapping_df_long <- mapping_df
names(mapping_df_long) <- gsub('_start', '', names(mapping_df_long))
 

####### 
# seed-soil extension
for(i in 1:mapping_lengths[1]){
  print(i)
  a <- mapping_df[i,]
  
  rl <- a$range_length - 1
  
  dr <- a$destination_range_start + rl
  sr <- a$source_range_start + rl
  
  sr1 <-  seeds_df$source_range[
    seeds_df$source_range >= a$source_range_start & 
      seeds_df$source_range <= sr]
  r1 <- sr1 - a$source_range_start
  
  dr1 <- a$destination_range_start + r1
  
  a_df <- 
    data_frame(
      destination_range = dr1,
      source_range = sr1,
      range_length = NA,
      map = a$map,
      source = a$source,
      destination = a$destination, 
      seed_inital = sr1
      
    )
  
  mapping_df_long <- 
    mapping_df_long %>% 
    bind_rows(a_df) %>%
    distinct() %>% 
    arrange(source, destination, source_range)
}

mapping_df_long <- 
  mapping_df_long %>% 
  full_join(seeds_df)

# seed-fertilizer extension
soil_df <- 
  mapping_df_long %>% 
  filter(!is.na(seed_inital)) %>% 
  mutate(
    destination_range = ifelse(is.na(destination_range), source_range, destination_range),
    map = 'seed-to-soil',
    destination = 'soil'
  ) %>%
  rename(soil_id = destination_range) %>%
  mutate(source = destination,
         source_range = soil_id) %>% 
  select(-destination, -range_length) %>% 
  arrange(seed_inital)

mapping_df_long <- mapping_df
names(mapping_df_long) <- gsub('_start', '', names(mapping_df_long))

for(i in (mapping_lengths[1]+1):(mapping_lengths[2]+mapping_lengths[1])){
  print(i)
  a <- mapping_df[i,]
  print(a$map)
  rl <- a$range_length - 1
  
  dr <- a$destination_range_start + rl
  sr <- a$source_range_start + rl
  
  sr1 <-  soil_df$source_range[
     soil_df$source_range >= a$source_range_start & 
      soil_df$source_range <= sr]
  sr2 <-  soil_df$seed_inital[
    soil_df$source_range >= a$source_range_start & 
      soil_df$source_range <= sr]
  if(length(sr1) != 0){
  
  r1 <- sr1 - a$source_range_start
  
  dr1 <- a$destination_range_start + r1
  
  a_df <- 
    data_frame(
      destination_range = dr1,
      source_range = sr1,
      range_length = NA,
      map = a$map,
      source = a$source,
      destination = a$destination, 
      seed_inital = sr2, 
      soil_id = sr1
    )
  
  mapping_df_long <- 
    mapping_df_long %>% 
    bind_rows(a_df) %>%
    distinct() %>% 
    arrange(source, destination, source_range)
  } else{
    
    print('no match')
  }

}

# seed-water extension
fertilizer_df <- 
  mapping_df_long %>% 
  filter(!is.na(soil_id)) %>% 
  full_join(soil_df %>% 
              select(seed_inital, soil_id)) %>% 
  mutate(
    destination_range = ifelse(is.na(destination_range), soil_id, destination_range),
    map = 'seed-to-fertilizer',
    destination = 'fertilizer'
  ) %>%
  rename(fertilizer_id = destination_range) %>%
  mutate(source = destination,
         source_range = fertilizer_id) %>% 
  select(-destination, -range_length, -map) %>% 
  arrange(seed_inital)

mapping_df_long <- mapping_df
names(mapping_df_long) <- gsub('_start', '', names(mapping_df_long))

for(i in (sum(mapping_lengths[1:2])+1):
    (sum(mapping_lengths[1:2])+sum(mapping_lengths[3])
         )
     ){
  print(i)
  a <- mapping_df[i,]
  print(a$map)
  rl <- a$range_length - 1
  
  dr <- a$destination_range_start + rl
  sr <- a$source_range_start + rl
  
  sr1 <-  fertilizer_df$source_range[
    fertilizer_df$source_range >= a$source_range_start & 
      fertilizer_df$source_range <= sr]
  sr2 <-  fertilizer_df$seed_inital[
    fertilizer_df$source_range >= a$source_range_start & 
      fertilizer_df$source_range <= sr]
  sr3 <-  fertilizer_df$soil_id[
    fertilizer_df$source_range >= a$source_range_start & 
      fertilizer_df$source_range <= sr]
  if(length(sr1) != 0){
    
    r1 <- sr1 - a$source_range_start
    
    dr1 <- a$destination_range_start + r1
    
    a_df <- 
      data_frame(
        destination_range = dr1,
        source_range = sr1,
        range_length = NA,
        map = a$map,
        source = a$source,
        destination = a$destination, 
        soil_id = sr3,
        seed_inital = sr2, 
        fertilizer_id = sr1
      )
    
    print(nrow(a_df))
    
    mapping_df_long <- 
      mapping_df_long %>% 
      bind_rows(a_df) %>%
      distinct() %>% 
      arrange(source, destination, source_range)
  } else{
    
    print('no match')
  }
  
}

# seed-water extension
water_df <- 
  mapping_df_long %>% 
  filter(!is.na(fertilizer_id)) %>% 
  full_join(fertilizer_df %>% 
              select(seed_inital, soil_id, fertilizer_id)) %>% 
  mutate(
    destination_range = ifelse(is.na(destination_range), fertilizer_id, destination_range),
    map = 'seed-to-water',
    destination = 'water'
  ) %>%
  rename(water_id = destination_range) %>%
  mutate(source = destination,
         source_range = water_id) %>% 
  select(-destination, -range_length) %>% 
  arrange(seed_inital)

mapping_df_long <- mapping_df
names(mapping_df_long) <- gsub('_start', '', names(mapping_df_long))

for(i in (sum(mapping_lengths[1:3])+1):
    (sum(mapping_lengths[1:3])+sum(mapping_lengths[4])
    )
){
  print(i)
  a <- mapping_df[i,]
  print(a$map)
  rl <- a$range_length - 1
  
  dr <- a$destination_range_start + rl
  sr <- a$source_range_start + rl
  
  sr1 <-  water_df$source_range[
    water_df$source_range >= a$source_range_start & 
      water_df$source_range <= sr]
  sr2 <-  water_df$seed_inital[
    water_df$source_range >= a$source_range_start & 
      water_df$source_range <= sr]
  sr3 <-  water_df$soil_id[
    water_df$source_range >= a$source_range_start & 
      water_df$source_range <= sr]
  sr4 <-  water_df$fertilizer_id[
    water_df$source_range >= a$source_range_start & 
      water_df$source_range <= sr]
  if(length(sr1) != 0){
    
    r1 <- sr1 - a$source_range_start
    
    dr1 <- a$destination_range_start + r1
    
    a_df <- 
      data_frame(
        destination_range = dr1,
        source_range = sr1,
        range_length = NA,
        map = a$map,
        source = a$source,
        destination = a$destination, 
        soil_id = sr3,
        seed_inital = sr2, 
        fertilizer_id = sr4,
        water_id = sr1
      )
    
    mapping_df_long <- 
      mapping_df_long %>% 
      bind_rows(a_df) %>%
      distinct() %>% 
      arrange(source, destination, source_range)
  } else{
    
    print('no match')
  }
  
}

# seed-light extension
light_df <- 
  mapping_df_long %>% 
  select(-map) %>%
  filter(!is.na(water_id)) %>% 
  full_join(water_df %>% 
              select(seed_inital, soil_id, fertilizer_id, water_id)) %>% 
  mutate(
    destination_range = ifelse(is.na(destination_range), water_id, destination_range),
    map = 'seed-to-light',
    destination = 'light'
  ) %>%
  rename(light_id = destination_range) %>%
  mutate(source = destination,
         source_range = light_id) %>% 
  select(-destination, -range_length) %>% 
  arrange(seed_inital)

mapping_df_long <- mapping_df
names(mapping_df_long) <- gsub('_start', '', names(mapping_df_long))

for(i in (sum(mapping_lengths[1:4])+1):
    (sum(mapping_lengths[1:4])+sum(mapping_lengths[5])
    )
){
  print(i)
  a <- mapping_df[i,]
  print(a$map)
  rl <- a$range_length - 1
  
  dr <- a$destination_range_start + rl
  sr <- a$source_range_start + rl
  
  sr1 <-  light_df$source_range[
    light_df$source_range >= a$source_range_start & 
      light_df$source_range <= sr]
  sr2 <-  light_df$seed_inital[
    light_df$source_range >= a$source_range_start & 
      light_df$source_range <= sr]
  sr3 <-  light_df$soil_id[
    light_df$source_range >= a$source_range_start & 
      light_df$source_range <= sr]
  sr4 <-  light_df$fertilizer_id[
    light_df$source_range >= a$source_range_start & 
      light_df$source_range <= sr]
  sr5 <-  light_df$water_id[
    light_df$source_range >= a$source_range_start & 
      light_df$source_range <= sr]
  if(length(sr1) != 0){
    
    r1 <- sr1 - a$source_range_start
    
    dr1 <- a$destination_range_start + r1
    
    a_df <- 
      data_frame(
        destination_range = dr1,
        source_range = sr1,
        range_length = NA,
        map = a$map,
        source = a$source,
        destination = a$destination, 
        soil_id = sr3,
        seed_inital = sr2, 
        fertilizer_id = sr4,
        water_id = sr5,
        light_id = sr1
        
      )
    
    mapping_df_long <- 
      mapping_df_long %>% 
      bind_rows(a_df) %>%
      distinct() %>% 
      arrange(source, destination, source_range)
  } else{
    
    print('no match')
  }
  
}

# seed-temperature extension
temperature_df <- 
  mapping_df_long %>% 
  select(-map) %>%
  filter(!is.na(light_id)) %>% 
  full_join(light_df %>% 
              select(seed_inital, soil_id, fertilizer_id, water_id, light_id)) %>% 
  mutate(
    destination_range = ifelse(is.na(destination_range), light_id, destination_range),
    map = 'seed-to-temperature',
    destination = 'temperature'
  ) %>%
  rename(temperature_id = destination_range) %>%
  mutate(source = destination,
         source_range = temperature_id) %>% 
  select(-destination, -range_length) %>% 
  arrange(seed_inital)

mapping_df_long <- mapping_df
names(mapping_df_long) <- gsub('_start', '', names(mapping_df_long))

for(i in (sum(mapping_lengths[1:5])+1):
    (sum(mapping_lengths[1:5])+sum(mapping_lengths[6])
    )
){
  print(i)
  a <- mapping_df[i,]
  print(a$map)
  rl <- a$range_length - 1
  
  dr <- a$destination_range_start + rl
  sr <- a$source_range_start + rl
  
  sr1 <-  temperature_df$source_range[
    temperature_df$source_range >= a$source_range_start & 
      temperature_df$source_range <= sr]
  sr2 <-  temperature_df$seed_inital[
    temperature_df$source_range >= a$source_range_start & 
      temperature_df$source_range <= sr]
  sr3 <-  temperature_df$soil_id[
    temperature_df$source_range >= a$source_range_start & 
      temperature_df$source_range <= sr]
  sr4 <-  temperature_df$fertilizer_id[
    temperature_df$source_range >= a$source_range_start & 
      temperature_df$source_range <= sr]
  sr5 <-  temperature_df$water_id[
    temperature_df$source_range >= a$source_range_start & 
      temperature_df$source_range <= sr]
  sr6 <-  temperature_df$light_id[
    temperature_df$source_range >= a$source_range_start & 
      temperature_df$source_range <= sr]
  if(length(sr1) != 0){
    
    r1 <- sr1 - a$source_range_start
    
    dr1 <- a$destination_range_start + r1
    
    a_df <- 
      data_frame(
        destination_range = dr1,
        source_range = sr1,
        range_length = NA,
        map = a$map,
        source = a$source,
        destination = a$destination, 
        soil_id = sr3,
        seed_inital = sr2, 
        fertilizer_id = sr4,
        water_id = sr5,
        light_id = sr6,
        temperature_id = sr1
      )
    
    mapping_df_long <- 
      mapping_df_long %>% 
      bind_rows(a_df) %>%
      distinct() %>% 
      arrange(source, destination, source_range)
  } else{
    
    print('no match')
  }
  
}

# seed-humidity extension
humidity_df <- 
  mapping_df_long %>% 
  select(-map) %>%
  filter(!is.na(temperature_id)) %>% 
  full_join(temperature_df %>% 
              select(seed_inital, soil_id, fertilizer_id, water_id, light_id, temperature_id)) %>% 
  mutate(
    destination_range = ifelse(is.na(destination_range), temperature_id, destination_range),
    map = 'seed-to-humidity',
    destination = 'humidity'
  ) %>%
  rename(humidity_id = destination_range) %>%
  mutate(source = destination,
         source_range = humidity_id) %>% 
  select(-destination, -range_length) %>% 
  arrange(seed_inital)

mapping_df_long <- mapping_df
names(mapping_df_long) <- gsub('_start', '', names(mapping_df_long))

for(i in (sum(mapping_lengths[1:6])+1):
    (sum(mapping_lengths[1:6])+sum(mapping_lengths[7])
    )
){
  print(i)
  a <- mapping_df[i,]
  print(a$map)
  rl <- a$range_length - 1
  
  dr <- a$destination_range_start + rl
  sr <- a$source_range_start + rl
  
  sr1 <-  humidity_df$source_range[
    humidity_df$source_range >= a$source_range_start & 
      humidity_df$source_range <= sr]
  sr2 <-  humidity_df$seed_inital[
    humidity_df$source_range >= a$source_range_start & 
      humidity_df$source_range <= sr]
  sr3 <-  humidity_df$soil_id[
    humidity_df$source_range >= a$source_range_start & 
      humidity_df$source_range <= sr]
  sr4 <-  humidity_df$fertilizer_id[
    humidity_df$source_range >= a$source_range_start & 
      humidity_df$source_range <= sr]
  sr5 <-  humidity_df$water_id[
    humidity_df$source_range >= a$source_range_start & 
      humidity_df$source_range <= sr]
  sr6 <-  humidity_df$temperature_id[
    humidity_df$source_range >= a$source_range_start & 
      humidity_df$source_range <= sr]
  sr7 <-  humidity_df$light_id[
    humidity_df$source_range >= a$source_range_start & 
      humidity_df$source_range <= sr]
  if(length(sr1) != 0){
    
    r1 <- sr1 - a$source_range_start
    
    dr1 <- a$destination_range_start + r1
    
    a_df <- 
      data_frame(
        destination_range = dr1,
        source_range = sr1,
        range_length = NA,
        map = a$map,
        source = a$source,
        destination = a$destination, 
        seed_inital = sr2, 
        soil_id = sr3,
        fertilizer_id = sr4,
        water_id = sr5,
        light_id = sr7,
        temperature_id = sr6,
        humidity_id = sr1
      )
    
    mapping_df_long <- 
      mapping_df_long %>% 
      bind_rows(a_df) %>%
      distinct() %>% 
      arrange(source, destination, source_range)
  } else{
    
    print('no match')
  }
  
}

# seed-location extension

location_df <- 
  mapping_df_long %>% 
  select(-map) %>%
  filter(!is.na(humidity_id)) %>% 
  full_join(humidity_df %>% 
              select(seed_inital, soil_id, fertilizer_id, water_id, light_id, temperature_id,humidity_id)) %>% 
  mutate(
    destination_range = ifelse(is.na(destination_range), humidity_id, destination_range),
    map = 'seed-to-humidity',
    destination = 'humidity'
  ) %>%
  rename(location_id = destination_range) %>%
  mutate(source = destination,
         source_range = location_id) %>% 
  select(-destination, -range_length) %>% 
  arrange(seed_inital)
#######

p1 <- min(location_df$location_id %>% as.numeric())

p1



# --- Part Two ---
#   Everyone will starve if you only plant such a small number of seeds. Re-reading the almanac, it looks like the seeds: line actually describes ranges of seed numbers.
# 
# The values on the initial seeds: line come in pairs. Within each pair, the first value is the start of the range and the second value is the length of the range. So, in the first line of the example above:
#   
#   seeds: 79 14 55 13
# This line describes two ranges of seed numbers to be planted in the garden. The first range starts with seed number 79 and contains 14 values: 79, 80, ..., 91, 92. The second range starts with seed number 55 and contains 13 values: 55, 56, ..., 66, 67.
# 
# Now, rather than considering four seed numbers, you need to consider a total of 27 seed numbers.
# 
# In the above example, the lowest location number can be obtained from seed number 82, which corresponds to soil 84, fertilizer 84, water 84, light 77, temperature 45, humidity 46, and location 46. So, the lowest location number is 46.
# 
# Consider all of the initial seed numbers listed in the ranges on the first line of the almanac. What is the lowest location number that corresponds to any of the initial seed numbers?



#### shamelessly copied from https://github.com/AdroMine/AdventOfCode/blob/main/2023/Day05/solution.R
### I was aiming to do something similar in my head but real life required my participation
file_name <- '05/data/input.in'
input <- strsplit(readr::read_file(file_name), '\\n\\n')[[1]]

seeds <- strsplit(input[1], ": ")[[1]][2] |> 
  strsplit(" ") |> 
  unlist() |> 
  as.numeric()

seed_maps <- 
  strsplit(input[-1], "\\n") |> 
  lapply(\(x) x[-1]) |> # remove first element (map name)
  lapply(strsplit, ' ') |> # multiple lines of numbers
  purrr::map_depth(2, as.numeric) # convert each to number

s_idx <- seq(1, length(seeds), by = 2)
r_idx <- seq(2, length(seeds), by = 2)

sds <- seeds[s_idx]
rng <- seeds[r_idx]


# Part 2 Non brute force

p2 <- Inf
for(i in seq_along(sds)){
  
  # create a range of seed locations
  cur_ranges <- list(c(sds[i], sds[i] + rng[i]))
  
  # for each mapping 
  for(j in seq_along(seed_maps)) {
    
    sm <- seed_maps[[j]]
    
    # ranges that have been mapped to the next seed map
    mapped_ranges <- list()
    
    # go with each row of current seed map
    for(k in seq_along(sm)) {
      
      sm_row <- sm[[k]]
      start <- sm_row[2]
      end <- sm_row[2] + sm_row[3]
      # new ranges that we will have to create (that are not mapped)
      # due to splits
      new_ranges <- list()
      
      # while we still have ranges to go through
      while(length(cur_ranges) > 0) {
        
        # take one object and remove from list
        cr <- cur_ranges[[length(cur_ranges)]]
        cur_ranges[[length(cur_ranges)]] <- NULL
        
        
        # create ranges (before mapping interval, overlapping interval, after mapping interval)
        part1 <- c(cr[1], min(cr[2], start))
        part2 <- c(max(cr[1], start), min(cr[2], end))
        part3 <- c(max(end, cr[1]), cr[2])
        
        # only part 2 will be part of mapping
        
        # if part1 and part3 are valid ranges, add them to next list of ranges to check/map
        if(part1[1] < part1[2]) new_ranges <- c(new_ranges, list(part1))
        if(part3[1] < part3[2]) new_ranges <- c(new_ranges, list(part3))
        
        # this is the only part that will have some mapping, add this to mapped ranges
        if(part2[1] < part2[2]) {
          mapped_part2 <- c(part2[1] - sm_row[2] + sm_row[1], 
                            part2[2] - sm_row[2] + sm_row[1])
          mapped_ranges <- c(mapped_ranges, list(mapped_part2))
        }
        
      }
      # iterate over any new ranges created
      cur_ranges <- new_ranges
    }
    # if any range still left, that goes unmapped, add the mapped ranges to it
    cur_ranges <- c(cur_ranges, mapped_ranges)
  }
  
  # All ranges are now mapped to location and present in cur_ranges
  
  # the first will always be lower, so take the first from each range and min over all
  p2 <- min(p2, min(sapply(cur_ranges, `[[`, 1)))
  
}

p2

ans <-  matrix(c(p1, p2)) 
write_lines(ans, '05/data/input.ans')

