## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = TRUE,
  comment = "#>"
)

# load packages
library(cf)
library(dplyr)
library(tidyr)



## ----remove--------------------------------------------------------------

data <- cf::remove_zeros_blanks(book_ratings)
head(data, 10)


## ----dedupe--------------------------------------------------------------

library(dplyr)

# identify the duplicates
book_ratings_with_names <- data %>% 
  left_join(book_info) %>%
  select(User.ID, ISBN, Book.Title, Book.Rating) %>% 
  filter(Book.Rating %in% c(1:10))
book_ratings_with_names[c("1008","1009","16884","16885"),]

# let's get the average rating for book 'the nanny diaries: a novel'
book_ratings %>% 
  left_join(book_info) %>%
  select(Book.Title, Book.Rating) %>% 
  filter(Book.Title == 'the nanny diaries: a novel') %>% 
  filter(Book.Rating %in% c(1:10)) %>% 
  summarise(avg_bookrating = mean(Book.Rating))

# let's get the average rating for reader 11676
book_ratings %>% 
  filter(User.ID == '11676') %>% 
  summarise(avg_readerrating = mean(Book.Rating))
# the mean rating for this book is 7.35 which is closer to 6 than to 9
# the mean rating for this user is 5.05 which is closer to 6 than to 9
# so we will discard the second rating for reader 11676
# reader 251140 gave the same rating so it doesn't matter which is removed

duplicates <- book_ratings_with_names[c("1009","16885"),]
duplicates
book_ratings_with_names_deduped <- book_ratings_with_names %>% 
  anti_join(duplicates) %>% 
  select(User.ID, Book.Title, Book.Rating)



## ----wide----------------------------------------------------------------

data <- cf::long_to_wide(book_ratings_with_names_deduped, fill = 0)
data[c(8,30,35,39,40),1:10]


## ----matrix--------------------------------------------------------------

data <- cf::convert_to_matrix(data)

# let's take a look at ratings for a for a selection of readers and books
demo <- data[c("23872","63360","98783","16795", "11676"),
     c("1st to die: a novel","angela's ashes", "black and blue",
       "fahrenheit 451", "hannibal", "silence of the lambs",
       "interview with the vampire", "stones from the river", 
       "the fellowship of the ring (the lord of the rings, part 1)",
       "the horse whisperer","the vampire lestat (vampire chronicles, book ii)")]
# let's transpose it to make it easier to display
t(demo)


## ----similarities--------------------------------------------------------

# calculate cosine similarity scores for all users
user_sims <- cf::user_similarities(data)

# let's look at the scores for a few users we now know
user_sims[c("23872","63360","98783","16795", "11676"),
          c("23872","63360","98783","16795", "11676")]



## ----similar-------------------------------------------------------------

# who are the most similar reader to a given reader?
head(sort(user_sims["23872",], decreasing = TRUE),8)
head(sort(user_sims["63360",], decreasing = TRUE),8)
head(sort(user_sims["98783",], decreasing = TRUE),8)
head(sort(user_sims["16795",], decreasing = TRUE),8)
head(sort(user_sims["11676",], decreasing = TRUE),8)


## ----predict-------------------------------------------------------------

user_predictions <- user_predict(user = "23872",
                                  user_similarities = user_sims,
                                  ratings_wide = data, 
                                  predict_n = 10)
user_predictions


## ----predict_demo--------------------------------------------------------

lapply(c("23872","63360","98783","16795", "11676"), user_predict, user_sims, data, 10)


## ----predict_new---------------------------------------------------------

# create data for new users
users_new <- data.frame(User.ID = (rep(max(book_ratings_with_names_deduped$User.ID)+1,3)), 
                        ISBN = c("0440234743", "0971880107", "0345417623"), 
                        Book.Rating = c(2, 5, 3))
# get the User.ID of the new user #"278844"
max(book_ratings_with_names_deduped$User.ID)+1

# remove ISBN and add Book.Title 
users_new <- users_new %>% 
  left_join(book_info, by = "ISBN") %>% 
  select(User.ID,Book.Title,Book.Rating)

# attach the new user to the data
book_ratings_new <- rbind(book_ratings_with_names_deduped, users_new)

# reshape the data from long to wide
book_ratings_new_wide <- cf::long_to_wide(book_ratings_new, fill = 0)

# convert to matrix
book_ratings_new_wide <- cf::convert_to_matrix(book_ratings_new_wide)

# recalculate similarities
user_sims_new <- cf::user_similarities(book_ratings_new_wide)

# generate predictions
user_predictions <- user_predict(user = "278844",
                                  user_similarities = user_sims_new,
                                  ratings_wide = book_ratings_new_wide, 
                                  predict_n = 10)
user_predictions



