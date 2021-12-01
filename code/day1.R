library(tidyverse)

# d1 - check depth changes increase/decrease
d1 <- read.csv("Desktop/d1_AoC.csv", header = F)
names(d1) <- 'poition'

for(i in 1:nrow(d1)){
  if (i == 1) {
     next
  } else{
    d1$depth[i] <- (d1$poition[i] - d1$poition[i-1]) > 0
  }
}
d1$depth %>% sum(na.rm = T)

# part 2 - compare in 3 depth intervals
d2 <- vector()
d2_depth <- vector()
for(i in 1:nrow(d1)){
  if (i %in% c(1, 2)) {
    next
  } else{
    i
    d2[i] <- 
      sum(
        d1$poition[i],
        d1$poition[i-1],
        d1$poition[i-2]
      )
    d2_depth[i] <- (d2[i] - d2[i-1]) > 0
  }
}
d2_depth %>% sum(na.rm = T)

