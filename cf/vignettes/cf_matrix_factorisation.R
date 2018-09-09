## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
# load packages
library(cf),
library(dplyr),
library(tidyr)
)

## ------------------------------------------------------------------------

# load packages
library(cf)
library(dplyr)
library(tidyr)

# 1. remove zeros
book_ratings_with_names <- book_ratings %>% 
  left_join(book_info) %>% 
  filter(Book.Rating != 0) %>%
  select(User.ID, Book.Title, Book.Rating)
head(book_ratings_with_names)

# 2. remove duplicates
duplicates <- book_ratings_with_names[c("1009","16885"),]
duplicates
book_ratings_with_names_deduped <- book_ratings_with_names %>% 
  anti_join(duplicates)
head(book_ratings_with_names_deduped)


## ------------------------------------------------------------------------

set.seed(123)
# create an index
index_long <- sample(seq_len(nrow(book_ratings_with_names_deduped)), 
                size = round(nrow(book_ratings_with_names_deduped)*0.8,0))

# split the data
train_long <- book_ratings_with_names_deduped[index_long,]    # train the model on 80% of the data
test_long <- book_ratings_with_names_deduped[-index_long,]    # hold out 20%
blanks <- test_long                   # create a copy of the test ratings
blanks[,3] <- NA                      # remove the test ratings and replace with NAs (blanks)
train <- rbind(train_long,blanks)     # combine NAs with the train set to preserve matrix size

# convert to wide matrix format
# convert training set from long to wide matrix and fill with NAs
train_wide <- cf::long_to_wide(train, NA)
train_wide <- cf::convert_to_matrix(train_wide) 
# convert testing set from long to wide matrix and fill with NAs
test_wide <- cf::long_to_wide(test_long, NA)      
test_wide <- cf::convert_to_matrix(test_wide)

# let's take a look at ratings for a for a selection of readers and books
demo <- train_wide[c("23872","63360","98783","16795", "11676"),
     c("1st to die: a novel","angela's ashes", "black and blue",
       "fahrenheit 451", "hannibal", "silence of the lambs",
       "interview with the vampire", "stones from the river", 
       "the fellowship of the ring (the lord of the rings, part 1)",
       "the horse whisperer","the vampire lestat (vampire chronicles, book ii)")]
# let's transpose it to make it easier to display
t(demo)

dim(train_wide)  # 7503
dim(test_wide)   # 2687
  

## ------------------------------------------------------------------------

set.seed(123)

rmse_model <- data.frame(0, nrow = 4, ncol = 3)

for (i in 1:4){ 
  
model <- cf::mf_model_build(train_wide, k_latent = i, 
                         alpha = 0, beta = 0, 
                         bias = "FALSE")

rmse_model[i,] <- cf::mf_model_rmse(model, train_wide, test_wide)
colnames(rmse_model) <- c("rmse_train", "rmse_test", "rmse_all")
  
}

rmse_model


## ------------------------------------------------------------------------

set.seed(123)

rmse_model <- data.frame(0, nrow = 5, ncol = 3)

for (i in 1:5){ 
  
model <- cf::mf_model_build(train_wide, k_latent = 1, 
                         alpha = i, beta = i, 
                         bias = "FALSE")

rmse_model[i,] <- cf::mf_model_rmse(model, train_wide, test_wide)
colnames(rmse_model) <- c("rmse_train", "rmse_test", "rmse_all")
  
}

rmse_model


## ------------------------------------------------------------------------

set.seed(123)

rmse_model <- data.frame(0, nrow = 8, ncol = 3)

for (i in 1:8){ 
  
model <- cf::mf_model_build(train_wide, k_latent = i, 
                         alpha = 1, beta = 1, 
                         bias = "FALSE")

rmse_model[i,] <- cf::mf_model_rmse(model, train_wide, test_wide)
colnames(rmse_model) <- c("rmse_train", "rmse_test", "rmse_all")
  
}

rmse_model


## ------------------------------------------------------------------------

set.seed(123)

rmse_model <- data.frame(0, nrow = 6, ncol = 3)

for (i in 1:3){ 
  
model <- cf::mf_model_build(train_wide, k_latent = i, 
                         alpha = 0, beta = 0, 
                         bias = "TRUE")           
# the function is designed to make it easy to add Bias

rmse_model[i,] <- cf::mf_model_rmse(model, train_wide, test_wide)
colnames(rmse_model) <- c("rmse_train", "rmse_test", "rmse_all")
  
}

rmse_model



## ------------------------------------------------------------------------

set.seed(123)

rmse_model <- data.frame(0, nrow = 8, ncol = 3)

for (i in 1:8){ 
  
model <- cf::mf_model_build(train_wide, k_latent = 1, 
                         alpha = i, beta = i, 
                         bias = "TRUE")           
# the function is designed to make it easy to add Bias

rmse_model[i,] <- cf::mf_model_rmse(model, train_wide, test_wide)
colnames(rmse_model) <- c("rmse_train", "rmse_test", "rmse_all")
  
}

rmse_model


## ------------------------------------------------------------------------

set.seed(123)

rmse_model <- data.frame(0, nrow = 5, ncol = 3)

for (i in 1:5){ 
  
model <- cf::mf_model_build(train_wide, k_latent = i, 
                         alpha = 6, beta = 6, 
                         bias = "TRUE")           
# the function is designed to make it easy to add Bias

rmse_model[i,] <- cf::mf_model_rmse(model, train_wide, test_wide)
colnames(rmse_model) <- c("rmse_train", "rmse_test", "rmse_all")
  
}

rmse_model


## ------------------------------------------------------------------------

# save the final model so that we can use it to make predictions
model_final <- cf::mf_model_build(train_wide, k_latent = 1, 
                         alpha = 0, beta = 0, 
                         bias = "FALSE")


## ------------------------------------------------------------------------

# make predictions
predictions <- cf::mf_predict(model_final)

# here's a recap of the ratings of a selection of users we looked at earlier
t(demo)

# let's compare that to the model predicitons
t(predictions[c("23872","63360","98783","16795", "11676"),
     c("1st to die: a novel","angela's ashes", "black and blue",
       "fahrenheit 451", "hannibal", "silence of the lambs",
       "interview with the vampire", "stones from the river", 
       "the fellowship of the ring (the lord of the rings, part 1)",
       "the horse whisperer","the vampire lestat (vampire chronicles, book ii)")])


## ------------------------------------------------------------------------

user <- c("23872","63360","98783","16795", "11676")
used <- book_ratings_with_names_deduped[user,]

ratings_wide <- cf::long_to_wide(book_ratings_with_names_deduped, NA)
ratings_wide <- cf::convert_to_matrix(ratings_wide)

# for 1 user
mf_recommend(user = "23872", predictions, ratings_wide, predict_n = 10)

# for all the other demo users:
lapply(c("63360","98783","16795", "11676"), mf_recommend, 
       predictions, ratings_wide, predict_n = 10)


## ------------------------------------------------------------------------

# create data for new users
users_new <- data.frame(User.ID = (rep(max(book_ratings$User.ID)+1,3)), 
                        ISBN = c("0440234743", "0971880107", "0345417623"), 
                        Book.Rating = c(2, 5, 3))

# remove ISBN and add Book.Title
users_new <- users_new %>% 
  left_join(book_info, by = "ISBN") %>% 
  select(User.ID,Book.Title,Book.Rating)






