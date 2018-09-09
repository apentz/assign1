## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# load packages
library(cf)
library(dplyr)
library(tidyr)
library(ggplot2)


## ----transform-----------------------------------------------------------

dplyr::glimpse(book_ratings)
dplyr::glimpse(book_info)
dplyr::glimpse(user_info)

head(book_ratings, 10)
head(book_info, 10)
head(user_info, 10)


## ---- echo=TRUE----------------------------------------------------------

dim(book_ratings)


## ---- echo=TRUE----------------------------------------------------------

table(book_ratings$Book.Rating)


## ---- echo=TRUE----------------------------------------------------------

book_ratings %>% 
  filter(Book.Rating != 0) %>% 
  count()


## ---- echo=TRUE----------------------------------------------------------

book_ratings %>% 
  filter(Book.Rating != 0) %>%     # remove the zero ratings
  group_by(User.ID) %>% 
  count() %>%                      # number of books rated per reader
  group_by(n) %>%                  
  count() %>%                      # number of readers that rated x books
  rename("number of books rated" = n, "number of readers" = nn)


## ---- echo=TRUE----------------------------------------------------------

sum(is.na(book_ratings$Book.Rating))    #0
sum(is.na(user_info$Age))               #3311


## ---- echo=TRUE----------------------------------------------------------

book_ratings %>% 
  filter(Book.Rating != 0) %>% 
  select(User.ID) %>% 
  unique() %>% 
  count()


## ---- echo=TRUE----------------------------------------------------------

book_ratings %>% 
  filter(Book.Rating != 0) %>% 
  select(ISBN) %>% 
  unique() %>% 
  count()


## ---- echo=TRUE----------------------------------------------------------

ratings_long <- book_ratings %>% 
  filter(Book.Rating != 0)         # remove the zero ratings

ggplot(data = ratings_long, aes(Book.Rating)) + 
  geom_histogram(binwidth = 0.5) +
  labs(title = "Distribution of Book Ratings excluding zeros", x = "Book Rating")


## ---- echo=TRUE----------------------------------------------------------

book_ratings %>% 
  filter(Book.Rating != 0) %>%
  group_by(User.ID) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(50)


## ---- echo=TRUE----------------------------------------------------------

book_ratings %>% 
  filter(Book.Rating != 0) %>%
  select(User.ID, ISBN, Book.Rating) %>% 
  group_by(ISBN) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(50)%>% 
  left_join(book_info)


## ---- echo=TRUE----------------------------------------------------------

# book_ratings %>% 
#  left_join(book_info) %>%
#  select(User.ID, Book.Title, Book.Rating) %>% 
#  filter(Book.Rating %in% c(1:10)) %>%                    # remove not rated (value=0)
#  spread(key = Book.Title, value = Book.Rating, fill = 0) # pivot the data, ISBN are columns


## ---- echo=TRUE----------------------------------------------------------

book_ratings_with_names <- book_ratings %>% 
  left_join(book_info) %>%
  select(User.ID, ISBN, Book.Title, Book.Rating) %>% 
  filter(Book.Rating %in% c(1:10))
book_ratings_with_names[c("1008","1009","16884","16885"),]


## ---- echo=TRUE----------------------------------------------------------

book_ratings %>% 
  left_join(book_info) %>%
  select(Book.Title, Book.Rating) %>% 
  filter(Book.Title == 'the nanny diaries: a novel') %>% 
  filter(Book.Rating %in% c(1:10)) %>% 
  summarise(avg_bookrating = mean(Book.Rating))


## ---- echo=TRUE----------------------------------------------------------

book_ratings %>% 
  filter(User.ID == '11676') %>% 
  summarise(avg_readerrating = mean(Book.Rating))


## ---- echo=TRUE----------------------------------------------------------

duplicates <- book_ratings_with_names[c("1009","16885"),]
duplicates
book_ratings_with_names_deduped <- book_ratings_with_names %>% 
  anti_join(duplicates)
head(book_ratings_with_names_deduped)


## ------------------------------------------------------------------------

book_ratings_wide <- book_ratings_with_names_deduped %>% 
spread(key = Book.Title, value = Book.Rating, fill = 0) # pivot the data, Book.Title are columns
dim(book_ratings_wide)  

# yay!!


## ------------------------------------------------------------------------

ggplot(data = user_info, aes(Age)) + 
  geom_histogram(binwidth = 0.5) +
  labs(title = "Distribution of Reader Ages", x = "Age of Reader")


