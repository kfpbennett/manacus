# --------------------------- Step-up -------------------------------

path<-"C:/Users/kbenn/Documents/grad/phd/dissertation/field/planning"
setwd(path)

library(tidyverse)


# ------------------------ Process combos ----------------------------

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

k.combos <- read.csv(
  'kira_color_combos.csv', stringsAsFactors = FALSE
  ) %>%
  separate(combo, into = c('left', 'right'), sep = ' ') %>%
  mutate(left = str_sub(left, start = 3),
         right = str_sub(right, start = 3),
         combo = paste(left, '_', right, sep = ''),
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
         # Remove Xs as placeholders for 'no band'
         combo = gsub('X/', '', combo),
         combo = gsub('/X', '', combo),
         combo = gsub('X_', '_', combo),
         combo = gsub('_X', '_', combo)) %>%
  select(combo, pop)


write.csv(k.combos, 'kira_combos_proc.csv', row.names = FALSE)

