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

# calculate cosine similarity scores for all items
item_sims <- cf::item_similarities(data)

# let's look at the scores for a few items
ix <- c("1st to die: a novel","angela's ashes", "black and blue",
       "fahrenheit 451", "hannibal", "silence of the lambs",
       "interview with the vampire", "stones from the river", 
       "the fellowship of the ring (the lord of the rings, part 1)",
       "the horse whisperer","the vampire lestat (vampire chronicles, book ii)")
item_sims[ix,ix]

dim(item_sims)


## ----similar-------------------------------------------------------------

# which books are the most similar to a given book?
head(sort(item_sims["1st to die: a novel",], decreasing = TRUE),3)
head(sort(item_sims["angela's ashes",], decreasing = TRUE),3)
head(sort(item_sims["black and blue",], decreasing = TRUE),3)
head(sort(item_sims["fahrenheit 451",], decreasing = TRUE),3)
head(sort(item_sims["hannibal",], decreasing = TRUE),3)
head(sort(item_sims["silence of the lambs",], decreasing = TRUE),3)


## ------------------------------------------------------------------------

# data including the implicit ratings which have been read but not rated
book_ratings_read <- book_ratings %>% 
  left_join(book_info) %>%
  select(User.ID, Book.Title, Book.Rating)
book_ratings_read$Book.Rating <- 1                         # make all ratings 1 to indicate read
book_ratings_read <- unique(book_ratings_read)             # remove dupilcates
# wide format
data_read <- cf::long_to_wide(book_ratings_read, fill = 0)
# matrix format
data_read <- cf::convert_to_matrix(data_read)


# which books has a given reader read?
user <- c("23872")
user_read <- row.names(item_sims)[data_read[user,] == 1]
user_read[1:10]
length(user_read)
# which books has a given reader rated?
user_rated <- row.names(item_sims)[data[user,] %in% c(1:10)]
user_rated[1:10]
length(user_rated)


# which books has a given reader read?
user <- c("63360")
user_read <- row.names(item_sims)[data_read[user,] == 1]
#user_read[1:10]
length(user_read)
# which books has a given reader rated?
user_rated <- row.names(item_sims)[data[user,] %in% c(1:10)]
# user_rated[1:10]
length(user_rated)

# which books has a given reader read?
user <- c("98783")
user_read <- row.names(item_sims)[data_read[user,] == 1]
# user_read[1:10]
length(user_read)
# which books has a given reader rated?
user_rated <- row.names(item_sims)[data[user,] %in% c(1:10)]
# user_rated[1:10]
length(user_rated)


# which books has a given reader read?
user <- c("16795")
user_read <- row.names(item_sims)[data_read[user,] == 1]
# user_read[1:10]
length(user_read)
# which books has a given reader rated?
user_rated <- row.names(item_sims)[data[user,] %in% c(1:10)]
# user_rated[1:10]
length(user_rated)


# which books has a given reader read?
user <- c("11676")
user_read <- row.names(item_sims)[data_read[user,] == 1]
# user_read[1:10]
length(user_read)
# which books has a given reader rated?
user_rated <- row.names(item_sims)[data[user,] %in% c(1:10)]
# user_rated[1:10]
length(user_rated)


## ----predict-------------------------------------------------------------

item_predictions <- item_predict(user = "23872",
                                 item_similarities = item_sims,
                                 ratings_wide = data_read, 
                                 predict_n = 10)
item_predictions


## ----predict_demo--------------------------------------------------------

lapply(c("23872","63360","98783","16795", "11676"), item_predict, item_sims, data_read, 10)


## ----predict_new---------------------------------------------------------

# create data for new users
users_new <- data.frame(User.ID = (rep(max(book_ratings_with_names_deduped$User.ID)+1,3)), 
                        ISBN = c("0440234743", "0971880107", "0345417623"), 
                        Book.Rating = c(2, 5, 3))
# get the User.ID of the new user 
max(book_ratings_with_names_deduped$User.ID)+1
#"278844"

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
item_sims_new <- cf::item_similarities(book_ratings_new_wide)

# data including the implicit ratings which have been read but not rated
books_read_new <- book_ratings %>% 
  left_join(book_info) %>%
  select(User.ID, Book.Title, Book.Rating)
books_read_new$Book.Rating <- 1                         # make all ratings 1 to indicate read
books_read_new <- unique(books_read_new)                # remove dupilcates
# attach the new user to the data
books_read_new <- rbind(books_read_new, users_new)
books_read_new$Book.Rating <- 1                         # make all ratings 1 to indicate read
# wide format
books_read_new <- cf::long_to_wide(books_read_new, fill = 0)
# matrix format
books_read_new <- cf::convert_to_matrix(books_read_new)

# generate predictions
item_predictions_new <- item_predict(user = "278844",
                                 item_similarities = item_sims_new,
                                 ratings_wide = books_read_new, 
                                 predict_n = 10)
item_predictions_new



