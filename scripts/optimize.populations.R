# --------------------------- Step-up -------------------------------

path<-"C:/Users/kbenn/Documents/grad/phd/dissertation/field/planning"
setwd(path)

library(tidyverse)
library(gtools)

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

k.combos <- read.csv('kira_combos_proc.csv', 
                     stringsAsFactors = FALSE)

k.combos.long <- read.csv(
  'kira_color_combos.csv', stringsAsFactors = FALSE) %>%
  separate(combo, into = c('left', 'right'), sep = ' ') %>%
  mutate(left = str_sub(left, start = 3),
         right = str_sub(right, start = 3)) %>%
  separate(left, into = c('left_top', 'left_bottom'), sep = '/',
           remove = FALSE) %>%
  separate(right, into = c('right_top', 'right_bottom'), sep = '/',
           remove = FALSE) %>%
  mutate(combo = paste(left, '_', right, sep = ''),
         # User the key above to edit color codes
         combo = gsub('0', 'X', combo),
         combo = gsub('a', 'A', combo),
         combo = gsub('p', 'U', combo),
         combo = gsub('n', 'B', combo),
         combo = gsub('bk', 'K', combo),
         combo = gsub('or', 'O', combo),
         combo = gsub('bw', 'N', combo),
         combo = gsub('ltb', 'L', combo),
         combo = gsub('gy', 'E', combo),
         combo = gsub('g', 'G', combo),
         combo = gsub('w', 'W', combo),
         combo = gsub('r', 'R', combo),
         combo = gsub('y', 'Y', combo),
         combo = gsub('/','', combo),
         combo = gsub('_', '', combo))

colors <- c('K', 'W', 'R', 'Y', 'B', 'G', 'N', 'V', 'I', 'M', 'X')

test.combos1 <- permutations(n = 11, r = 3, v = colors, 
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
         combo = gsub('/X', '', combo))


inds1 <- which(test.combos1$combo %in% k.combos$combo)


test.combos2 <- permutations(n = 11, r = 3, v = colors, 
                             repeats.allowed = TRUE) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  rename(left_top = V1, left_bottom = V2, right_top = V3) %>%
  mutate(right_bottom = 'A') %>%
  select(left_top, left_bottom, right_top, right_bottom) %>%
  # Remove duplicates and unbanded left leg combos
  filter(left_top != 'X') %>%
  mutate(combo = paste(left_top, '/', left_bottom, '_', 
                       right_top, '/', right_bottom, sep = ''),
         # Remove all but colors in combo
         combo = gsub('/', '', combo),
         combo = gsub('_', '', combo))

test.combos2 <- test.combos2[-inds1,]

test.combos.new <- test.combos2 %>%
  filter(grepl('I|V|M', combo))

test.combos.noV <- test.combos2 %>%
  filter(!grepl('V', combo))

test.combos.kira <- test.combos.noV %>%
  inner_join(select(test.combos.new, combo), by = 'combo')

test.combos.kevin <- test.combos2 %>%
  filter(!combo %in% test.combos.kira$combo)


test.combos.kevin <- test.combos.kevin[-c(653:655),] %>%
  select(combo)

test.combos.kira <- test.combos.kira[-c(451:452),] %>%
  select(combo)

test.pops <- as.data.frame(rep(1:4, 163))
sims <- c()

for(i in 1:1000) {
  set.seed(i)
  
  ord <- test.combos.kevin[sample(nrow(test.combos.kevin)),] %>%
    cbind(test.pops)
  
  one <- ord[ord$`rep(1:4, 163)` == 1,]$. %>% as.character()
  two <- ord[ord$`rep(1:4, 163)` == 2,]$. %>% as.character()
  three <- ord[ord$`rep(1:4, 163)` == 3,]$. %>% as.character()
  four <- ord[ord$`rep(1:4, 163)` == 4,]$. %>% as.character()
  
  int.sims <- c()
  int.sims1 <- c()
  int.sims2 <- c()
  int.sims3 <- c()
  int.sims4 <- c()
  for(j in 1:length(one)) {
    int.sims1[[j]] <- lapply(one, str_similarity, one[j])
    int.sims2[[j]] <- lapply(two, str_similarity, two[j])
    int.sims3[[j]] <- lapply(three, str_similarity, three[j])
    int.sims4[[j]] <- lapply(four, str_similarity, four[j])
    
    int.sims[j] <- sum(unlist(int.sims1[[j]]), 
                       unlist(int.sims2[[j]]), 
                       unlist(int.sims3[[j]]), 
                       unlist(int.sims4[[j]]))
  }
  sims[i] <- sum(int.sims)
  print(i)
}

# Out of 1:1000, 254 was the best, so this is the way to generate
# the best combination of populations:

set.seed(254)
ord <- test.combos.kevin[sample(nrow(test.combos.kevin)),] %>%
  cbind(test.pops)


# Same for Kira's populations

k.test.pops <- as.data.frame(rep(1:6, 75), col.names = 'pop')
k.sims <- c()

for(i in 1:500) {
  set.seed(i)
  
  ord <- test.combos.kira[sample(nrow(test.combos.kira)),] %>%
    cbind(k.test.pops)
  colnames(ord) <- c('combo', 'pop')
  
  two <- c(as.character(ord[ord$pop == 1,]$combo),
           k.combos.long[k.combos.long$pop == 2,]$combo)
  three <- c(as.character(ord[ord$pop == 2,]$combo),
             k.combos.long[k.combos.long$pop == 3,]$combo)
  four <- c(as.character(ord[ord$pop == 3,]$combo),
            k.combos.long[k.combos.long$pop == 4,]$combo)
  nine <- c(as.character(ord[ord$pop == 4,]$combo),
            k.combos.long[k.combos.long$pop == 9,]$combo)
  nineb <- c(as.character(ord[ord$pop == 5,]$combo),
             k.combos.long[k.combos.long$pop == 9.5,]$combo)
  ten <- c(as.character(ord[ord$pop == 6,]$combo),
           k.combos.long[k.combos.long$pop == 10,]$combo)
  
  int.sims <- c()
  int.sims1 <- c()
  int.sims2 <- c()
  int.sims3 <- c()
  int.sims4 <- c()
  int.sims5 <- c()
  int.sims6 <- c()
  for(j in 1:length(two)) {
    int.sims1[[j]] <- lapply(two, str_similarity, two[j])}
  for(j in 1:length(three)) {
    int.sims2[[j]] <- lapply(three, str_similarity, three[j])}
  for(j in 1:length(four)) {
    int.sims3[[j]] <- lapply(four, str_similarity, four[j])}
  for(j in 1:length(nine)) {
    int.sims4[[j]] <- lapply(nine, str_similarity, nine[j])}
  for(j in 1:length(nineb)) {
    int.sims5[[j]] <- lapply(nineb, str_similarity, nineb[j])}
  for(j in 1:length(ten)) {
    int.sims6[[j]] <- lapply(ten, str_similarity, ten[j])}
  
  k.sims[i] <- sum(unlist(int.sims1), unlist(int.sims2),
                   unlist(int.sims3), unlist(int.sims4),
                   unlist(int.sims5), unlist(int.sims6))
  print(i)
}

set.seed(433)
ord <- test.combos.kira[sample(nrow(test.combos.kira)),] %>%
  cbind(k.test.pops)



# ORDER THE COMBOS

set.seed(254)
test.combos.kevin.pop <- 
  test.combos.kevin[sample(nrow(test.combos.kevin)),] %>%
  cbind(test.pops)

colnames(test.combos.kevin.pop) <- c('combo', 'pops')

test.combos.kevin.pop$combo <- 
  as.character(test.combos.kevin.pop$combo)

order_bands <- function(dat, pop, reps) {
  pcoms <- dat[dat$pops == pop,]$combo
  results1 <- c()
  
  for(i in 1:reps) {
    set.seed(i)
    pcoms20 <- sample(pcoms)[1:20]
    
    int.results1 <- c()
    
    for(j in 1:20) {
      int.results1[[j]] <- 
        lapply(pcoms20, str_similarity, pcoms20[j])
    }
    results1[i] <- sum(unlist(int.results1))
  }
  best.i <- which(results1 == min(results1))
  
  set.seed(best.i)
  pcoms_best20 <- sample(pcoms)[1:20]
  print(which(results1 == min(results1)))
  
  inds <- which(pcoms_best20 %in% pcoms)
  pcoms_no_best20 <- pcoms[-inds]
  
  results2 <- c()
  
  for(i in 1:reps) {
    set.seed(i)
    pcoms_samp55 <- sample(pcoms_no_best20)[1:35]
    pcoms55 <- c(pcoms_best20, pcoms_samp55)
    
    int.results2 <- c()
    
    for(j in 1:55) {
      int.results2[[j]] <-
        lapply(pcoms55, str_similarity, pcoms55[j])
    }
    results2[i] <- sum(unlist(int.results2))
  }
  best.i <- which(results2 == min(results2))
  
  set.seed(best.i)
  pcoms_bestrest <- sample(pcoms_no_best20)
  
  final.outcome <- c(pcoms_best20, pcoms_bestrest)
  print(min(results2))
  return(final.outcome)
}

order.pop1 <- order_bands(test.combos.kevin.pop, 1, 1000)
order.pop2 <- order_bands(test.combos.kevin.pop, 2, 1000)
order.pop3 <- order_bands(test.combos.kevin.pop, 3, 1000)
order.pop4 <- order_bands(test.combos.kevin.pop, 4, 1000)


kev.order <- 
  data.frame(combo = c(order.pop1, order.pop2, 
                       order.pop3, order.pop4),
             pop = c(rep(1, 163), rep(2, 163), 
                     rep(3, 163), rep(4, 163)),
             order = rep(1:163, 4)) %>%
  mutate(left_top = str_sub(combo, end = 1),
         left_bottom = str_sub(combo, start = 2, end = 2),
         right_top = str_sub(combo, start = 3, end = 3),
         right_bottom = str_sub(combo, start = 4, end = 4),
         combo = paste(left_top, '/', left_bottom, '_',
                       right_top, '/', right_bottom, sep = ''),
         combo = gsub('X/', '', combo),
         combo = gsub('/X', '', combo)) %>%
  select(combo, pop, order)



write.csv(kev.order, 'band_order_kev.csv', row.names = FALSE)


# ============ Kira =============

set.seed(433)
test.combos.kira.pop <- 
  test.combos.kira[sample(nrow(test.combos.kira)),] %>%
  cbind(k.test.pops)

colnames(test.combos.kira.pop) <- c('combo', 'pops')
test.combos.kira.pop[test.combos.kira.pop$pops == 6,]$pops <- 10
test.combos.kira.pop[test.combos.kira.pop$pops == 5,]$pops <- 9.5
test.combos.kira.pop[test.combos.kira.pop$pops == 4,]$pops <- 9
test.combos.kira.pop[test.combos.kira.pop$pops == 3,]$pops <- 4
test.combos.kira.pop[test.combos.kira.pop$pops == 2,]$pops <- 3
test.combos.kira.pop[test.combos.kira.pop$pops == 1,]$pops <- 2
test.combos.kira.pop$combo <- 
  as.character(test.combos.kira.pop$combo)

order_bands_kira <- function(dat, pop, reps) {
  coms <- dat[dat$pops == pop,]$combo
  old_coms <- k.combos.long[k.combos.long$pop == pop,]$combo
  results1 <- c()
  
  for(i in 1:reps) {
    set.seed(i)
    coms20 <- sample(coms)[1:20]
    
    int.results1 <- c()
    
    for(j in 1:20) {
      int.results1[[j]] <- 
        lapply(c(old_coms,coms20), str_similarity, coms20[j])
    }
    results1[i] <- sum(unlist(int.results1))
  }
  best.i <- which(results1 == min(results1))
  
  set.seed(best.i)
  coms_best20 <- sample(coms)[1:20]
  print(which(results1 == min(results1)))
  
  inds <- which(coms_best20 %in% coms)
  coms_no_best20 <- coms[-inds]
  
  results2 <- c()
  
  for(i in 1:reps) {
    set.seed(i)
    coms_samp55 <- sample(coms_no_best20)[1:35]
    coms55 <- c(coms_best20, coms_samp55)
    
    int.results2 <- c()
    
    for(j in 1:55) {
      int.results2[[j]] <-
        lapply(c(old_coms, coms55), str_similarity, coms55[j])
    }
    results2[i] <- sum(unlist(int.results2))
  }
  best.i <- which(results2 == min(results2))
  
  set.seed(best.i)
  coms_bestrest <- sample(coms_no_best20)
  
  final.outcome <- c(coms_best20, coms_bestrest)
  print(min(results2))
  return(final.outcome)
}

order.kpop2 <- order_bands_kira(test.combos.kira.pop, 2, 500)
order.kpop3 <- order_bands_kira(test.combos.kira.pop, 3, 500)
order.kpop4 <- order_bands_kira(test.combos.kira.pop, 4, 1000)
order.kpop9 <- order_bands_kira(test.combos.kira.pop, 9, 500)
order.kpop9b <- order_bands_kira(test.combos.kira.pop, 9.5, 500)
order.kpop10 <- order_bands_kira(test.combos.kira.pop, 10, 500)




kira.order <- 
  data.frame(combo = c(order.kpop2, order.kpop3, 
                       order.kpop4, order.kpop9,
                       order.kpop9b, order.kpop10),
             pop = c(rep(2, 75), rep(3, 75), 
                     rep(4, 75), rep(9, 75),
                     rep(9.5, 75), rep(10, 75)),
             order = rep(1:75, 6)) %>%
  mutate(left_top = str_sub(combo, end = 1),
         left_bottom = str_sub(combo, start = 2, end = 2),
         right_top = str_sub(combo, start = 3, end = 3),
         right_bottom = str_sub(combo, start = 4, end = 4),
         combo = paste(left_top, '/', left_bottom, '_',
                       right_top, '/', right_bottom, sep = ''),
         combo = gsub('X/', '', combo),
         combo = gsub('/X', '', combo)) %>%
  select(combo, pop, order)

write.csv(kira.order, 'band_order_kira.csv', row.names = FALSE)


