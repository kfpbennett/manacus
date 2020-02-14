# --------------------------- Step-up -------------------------------

path<-"C:/Users/kbenn/Documents/grad/phd/dissertation/field/planning"
setwd(path)

library(tidyverse)
library(gtools)
# library(rabi)


# Read in Kira's band combos

k.combos <- read.csv('kira_combos_proc.csv', 
                     stringsAsFactors = FALSE)

# -------------------------- Make combos ----------------------------

# Color key:
# Aluminum:   A
# Black:      K
# White:      W
# Red:        R
# Yellow:     Y
# Dark blue:  B
# Dark green: G
# Brown:      N
# Purple:     U
# Orange:     O
# Light blue: L
# Grey:       E
# Hot pink:   I
# Violet:     V
# Spearmint:  M
# No Band:    X

# Permute all combinations of 2020 band colors not used before

colors <- c('K', 'W', 'R', 'Y', 'B', 'G', 'N', 'V', 'I', 'M', 'X')

all.combos <- permutations(n = 11, r = 3, v = colors, 
                       repeats.allowed = TRUE) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  rename(left_top = V1, left_bottom = V2, right_top = V3) %>%
  mutate(right_bottom = 'A') %>%
  select(left_top, left_bottom, right_top, right_bottom) %>%
  # Remove duplicates and unbanded left leg combos
  filter(left_top != 'X') %>%
  mutate(combo = paste(left_top, '/', left_bottom, '_', 
                       right_top, '/', right_bottom, sep = ''),
         # Remove Xs as stand-ins for no band
         combo = gsub('X/', '', combo),
         combo = gsub('/X', '', combo)) %>%
  # Remove combos already used by Kira
  filter(!combo %in% k.combos$combo) %>%
  select(-right_bottom)


# Filter for the presence of at least one new color

new.combos <- all.combos %>%
  filter(grepl('I|V|M', combo))


# Filter to omit combos with new violet color (similar to purple)

non.V.combos <- all.combos %>%
  filter(!grepl('V', combo))


# Combos without violet and with at least one new color. Kira should
# use combinations from this set for 2020

non.V.new.combos <- non.V.combos %>%
  inner_join(select(new.combos, combo), by = 'combo')


# Combinations not previously used but not available to Kira in 2020.
# This includes combos without new colors and those with violet.
# Kevin should use combinations from this set for 2020

leftover.combos <- all.combos %>%
  filter(!combo %in% non.V.new.combos$combo)


# Refer to 'optimize.populations.R' for how to get these seeds
# improving the combinations of colors in each population.

set.seed(162)
leftover.combos <- leftover.combos[-c(653:655),]
pops <- rep(1:4, 163)
leftover.combos.pop <- 
  leftover.combos[sample(nrow(leftover.combos)),] %>%
  cbind(pops)

set.seed(71)
non.V.new.combos <- non.V.new.combos[-c(451:452),]
pops <- rep(c(2, 3, 4, 9, 9.5, 10), 75)
non.V.new.combos.pop <- 
  non.V.new.combos[sample(nrow(non.V.new.combos)),] %>%
  cbind(pops)


# HERE I WILL FIGURE OUT HOW TO ORDER THE BANDS




# Write output to file

write.csv(combos, 'color_combos.csv', row.names = FALSE)




# ------------------------ Works in progress ------------------------

band_order <- function(dat) {
  diffs <- list(length = nrow(dat))
  for(i in 1:nrow(dat)){
    int.result <- c()
    for(j in 1:nrow(dat)){
      int.result[j] <- string.diff(
        dat[i,4], 
        dat[j,4], 
        exclude = c('/', '_', 'A'))
    }
    diffs[[i]] <- int.result
  }
  
  sums <- c()
  for(i in 1:56){
    sums[i] <- sum(diffs[[i]])
  }
  order <- 1:56
  dat_ordered <- cbind(dat, sums) %>%
    arrange(desc(sums)) %>%
    cbind(order)
  return(dat_ordered)
}

get_bigrams <- function(str) {
  lstr = tolower(str)
  bigramlst = list()
  for(i in 1:(nchar(str)-1)) {
    bigramlst[[i]] = substr(str, i, i+1)
  }
  return(bigramlst)
}

str_similarity <- function(str1, str2) {
  pairs1 = get_bigrams(str1)
  pairs2 = get_bigrams(str2)
  unionlen  = length(pairs1) + length(pairs2)
  hit_count = 0
  for(x in 1:length(pairs1)) {
    for(y in 1:length(pairs2)) {
      if (pairs1[[x]] == pairs2[[y]])
        hit_count = hit_count + 1
    }
  }
  return ((2.0 * hit_count) / unionlen)
}

band_order2 <- function(dat) {
  sim <- list(length = nrow(dat))
  for(i in 1:nrow(dat)){
    int.result <- c()
    for(j in 1:nrow(dat)){
      int.result[j] <- str_similarity(dat[i,4], dat[j,4])
    }
    sim[[i]] <- int.result
  }
  
  sums <- c()
  for(i in 1:length(sim)){
    sums[i] <- sum(sim[[i]])
  }
  order <- 1:length(sums)
  dat_ordered <- cbind(dat, sums) %>%
    arrange(sums) %>%
    cbind(order)
  return(dat_ordered)
}

one <- mix_combos[mix_combos$pop == 1,]
two <- mix_combos[mix_combos$pop == 2,]

result1 <- band_order(one)
result2 <- band_order(two)

results <- rbind(result1, result2)

# Order combos for easier viewing

# combos <- combos[order(
#   factor(combos$left_top, levels = colors),
#   factor(combos$right_top, levels = colors),
#   factor(combos$right_bottom,levels = colors)),]


