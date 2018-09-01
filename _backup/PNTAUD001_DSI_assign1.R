#------------------------------------------------------------------
# Load Required Packages
#------------------------------------------------------------------
library(tidyverse)
library(coop)
library(NNLM)

#------------------------------------------------------------------
# Load Data
#------------------------------------------------------------------
load("./book_ratings.Rdata")
load("./movie_ratings.Rdata")

# Store Data Dimensions
items_n <- function(data){length(unique(ratings_long[,2]))}    #150
users_n <- function(data){length(unique(ratings_long[,1]))}  #10000

#------------------------------------------------------------------
# Exploratory Data Analysis (EDA)
#------------------------------------------------------------------

library(cf)
library(ggplot2)

# how many ratings are there?
book_ratings %>% 
  count()

# let's see how many of each rating there are
table(book_ratings$Book.Rating)

# how many non-zero ratings are there?
book_ratings %>% 
  filter(Book.Rating != 0) %>% 
  count()

# are there any NA values?
sum(is.na(book_ratings$Book.Rating))    #0
sum(is.na(user_info$Age))               #3311

# how many unique users are there that have rated at least 1 book?
book_ratings %>% 
  filter(Book.Rating != 0) %>% 
  select(User.ID) %>% 
  unique() %>% 
  count()

# how many unique books are there that have been rated at least once?
book_ratings %>% 
  filter(Book.Rating != 0) %>% 
  select(ISBN) %>% 
  unique() %>% 
  count()

# let's remove the zero ratings and see what the distribution looks like
ggplot(data = ratings_long_test, aes(Book.Rating)) + 
  geom_histogram(binwidth = 0.5) +
  labs(title = "Distribution of Book Ratings excluding zeros", x = "Book Rating")

# let's see which users have rated the most books
book_ratings %>% 
  filter(Book.Rating != 0) %>%
  group_by(User.ID) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(50)

# let's see which books have the most ratings
book_ratings %>% 
  filter(Book.Rating != 0) %>%
  select(User.ID, ISBN, Book.Rating) %>% 
  group_by(ISBN) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(50)%>% 
  left_join(book_info)

# let's check that the data joins up nicely
book_ratings %>% 
  left_join(book_info) %>%
  select(User.ID, Book.Title, Book.Rating) %>% 
  filter(Book.Rating %in% c(1:10)) %>%                    # remove not rated (value=0)
  spread(key = Book.Title, value = Book.Rating, fill = 0) # pivot the data, ISBN are columns

# ...oh dear, we have some duplicates, let's take a look
book_ratings_with_names <- book_ratings %>% 
  left_join(book_info) %>%
  select(User.ID, ISBN, Book.Title, Book.Rating) %>% 
  filter(Book.Rating %in% c(1:10))
book_ratings_with_names[c("1008","1009","16884","16885"),]
# we have found that 2 readers have rated a book with different ISBNs but the same title, twice
# reader 11676 has given 2 differet ratings for what we will assume is the same book
# reader 251140 has given the same rating for what we will assume is the same book twice

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
  anti_join(duplicates)
book_ratings_with_names_deduped

# let's try again
book_ratings_wide <- book_ratings_with_names_deduped %>% 
spread(key = Book.Title, value = Book.Rating, fill = 0) # pivot the data, ISBN are columns
dim(book_ratings_wide)  # yay!!

# let's see the distribution of Ages
ggplot(data = user_info, aes(Age)) + 
  geom_histogram(binwidth = 0.5) +
  labs(title = "Distribution of Reader Ages", x = "Age of Reader")
# these need to be cleaned up if we are going to use them


#------------------------------------------------------------------
# Create Data for New Users
#------------------------------------------------------------------
users_new <- data.frame(User.ID = rep(max(book_ratings$User.ID)+1,3), 
                        ISBN = c(0440234743, 0971880107, 0345417623), 
                        Book.Rating = c(2, 5, 3))


#------------------------------------------------------------------
# Create Data to Sense Check Results
#------------------------------------------------------------------

# let's see who is similar to a few users that have rated 10 books
book_ratings %>% 
  filter(Book.Rating != 0) %>%
  group_by(User.ID) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  filter(n %in% 5:10)

head(sort(user_similarities_books["23872",], decreasing = TRUE),1)  
# 63360 is most similar to 23872
head(sort(user_similarities_books["63360",], decreasing = TRUE),1)  
# 98783 is most similar to 63360
head(sort(user_similarities_books["98783",], decreasing = TRUE),1)  
head(sort(user_similarities_books["63360",], decreasing = TRUE),435) 
# 16795 is least similar to 63360 (other than zeros)
head(sort(user_similarities_books["16795",], decreasing = TRUE),1) 
# 11676 is most similar to 63360

# let's create a demo dataset
book_ratings_wide <- convert_to_matrix(book_ratings_wide)
readers_demo <- c("23872","63360","98783","16795", "11676")
books_demo <- c("1st to die: a novel","along came a spider (alex cross novels)", 
                "angela's ashes", "black and blue", "fahrenheit 451", "hannibal", 
                "house of sand and fog","interview with the vampire", "silence of the lambs",
                "the fellowship of the ring (the lord of the rings, part 1)",
                "the horse whisperer","the vampire lestat (vampire chronicles, book ii)",
                "stones from the river","the prince of tides", "a map of the world")
users_test <- book_ratings_wide[readers_demo, books_demo]
t(users_test)


#------------------------------------------------------------------
# Task 1: function to remove zeros and blanks
#------------------------------------------------------------------
remove_zeros_blanks <- function(ratings_long){
    
    ratings_long <- ratings_long %>%
      filter(ratings_long[,3] %in% c(1:10))     # remove implicit ratings ie. not rated (value=0) and NA values
    
}

# books example existing
ratings_long_test <- remove_zeros_blanks(book_ratings)

# books example new
ratings_long_new <- remove_zeros_blanks(users_new)

#------------------------------------------------------------------
# Task 2: function to reshape the data from long to wide format
#------------------------------------------------------------------
long_to_wide <- function(ratings_long, fill){
  
  # reshape the data from long to wide format
  ratings_wide <- ratings_long %>%
    spread(key = colnames(ratings_long[2]), value = colnames(ratings_long[3]), fill = fill) # pivot the data, items become columns
  
  return(ratings_wide)
  
}

# books example existing
ratings_wide_test <- long_to_wide(ratings_long_test, fill = 0)

# books example new
ratings_wide_new <- long_to_wide(ratings_long_new, fill = 0)


#------------------------------------------------------------------
# Task 3: function to remove first column (User ID) column and convert to matrix format
#------------------------------------------------------------------
convert_to_matrix <- function(ratings_wide){
  # convert to matrix; convert first column to rownames
  sorted_users <- as.character(unlist(ratings_wide[,1]))  # save the Users
  ratings_wide <- as.matrix(ratings_wide[,-1])            # convert to matrix and remove the UserID column
  row.names(ratings_wide) <- sorted_users                 # replace the rownames with User IDs
  return(ratings_wide)
}
# movies example
movie_ratings <- convert_to_matrix(movie_ratings)

# books example existing
ratings_wide_test <- convert_to_matrix(ratings_wide_test)

# books example new
ratings_wide_new <- convert_to_matrix(ratings_long_new)


#------------------------------------------------------------------
# Task 4: function to calculate cosine similarity for users and set diagonal to zero
#------------------------------------------------------------------
similarities_users <- function(ratings_wide){
  similarities_users <- coop::tcosine(ratings_wide)  # the tcosine function calculates cosine similarity between rows (users)
  diag(similarities_users) <- 0                      # set diagonal to zero to avoid returning similarity to self
  return(similarities_users)
}
# movies example
user_similarities_movies <- similarities_users(movie_ratings)
# who are the 10 most similar users to user 114?
head(sort(user_similarities_movies[1,], decreasing = TRUE),10)

# books example existing
user_similarities_books <- similarities_users(book_ratings_wide)
# who are the 10 most similar users to user 37712?
head(sort(user_similarities_books["23872",], decreasing = TRUE),10)


# let's look at their ratings
t(book_ratings_wide[c("23872","63360","98783","16795", "11676"),
                  c("1st to die: a novel","along came a spider (alex cross novels)", 
                    "angela's ashes", "black and blue", "fahrenheit 451", "hannibal", 
                    "house of sand and fog","interview with the vampire", "silence of the lambs",
                    "the fellowship of the ring (the lord of the rings, part 1)",
                    "the horse whisperer","the vampire lestat (vampire chronicles, book ii)",
                    "stones from the river","the prince of tides", "a map of the world")])

# books example new
user_similarities_new <- similarities_users(ratings_wide_new)


#------------------------------------------------------------------
# Task 5: function to calculate item scores for any 1 user
#------------------------------------------------------------------
cf_user_predict <- function (users, user_similarities, ratings_wide, predict_n) {
  
  # turn into character if not already
  user <- ifelse(is.character(users), users, as.character(users))
  
  # get scores
  user_scores <- data.frame(title = colnames(ratings_wide), 
                            score = as.vector(user_similarities[users,] %*% ratings_wide), 
                            used = ratings_wide[users,])
  
  # sort 'unused' items by score and remove the 'used' column
  user_scores <- user_scores %>% 
    filter(used == 0) %>% 
    arrange(desc(score)) %>% 
    select(-used)
  
  return(user_scores[1:predict_n,])
  
}

# example existing - 1 user
cf_user_predict_test <- cf_user_predict(users = "23872", 
                                        user_similarities = user_similarities_books, 
                                        ratings_wide = book_ratings_wide, 
                                        predict_n = 10)

# example new
cf_user_predict_new <- cf_user_predict(users = users_new, 
                                       user_similarities = user_similarities_new, 
                                       ratings_wide = ratings_wide_new)

