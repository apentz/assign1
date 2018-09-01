## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=TRUE----------------------------------------------------------

library(cf)
library(dplyr)
library(tidyr)

glimpse(book_ratings)
glimpse(book_info)
glimpse(user_info)


## ---- echo=TRUE----------------------------------------------------------

step1 <- cf::remove_zeros_blanks(book_ratings)
glimpse(step1)


## ---- echo=TRUE----------------------------------------------------------

step2 <- cf::long_to_wide(step1, fill = 0)
glimpse(step2)


