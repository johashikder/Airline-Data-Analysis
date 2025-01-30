# Install & Load Necessary Packages
install.packages(c("tidyverse", "ggplot2", "wordcloud", "tm", "syuzhet", "corrplot"))
library(tidyverse)
library(ggplot2)
library(wordcloud)
library(tm)
library(syuzhet)
library(corrplot)

# Load the Dataset
df <- read.csv(choose.files(), stringsAsFactors = FALSE)

# Data Overview & Cleaning -----
str(df)  # Check structure
summary(df)  # Summary statistics
df <- df %>% mutate(Datetime = as.Date(Datetime, format="%Y-%m-%d"))  # Convert date

# Check for Missing Values
missing_values <- colSums(is.na(df))
print(missing_values)

# Drop Unnecessary Columns
df <- df %>% select(-c(Name, DateFlown, Aircraft))  

# Exploratory Data Analysis (EDA) -----
# Distribution of Overall Ratings
ggplot(df, aes(x = OverallRating)) + 
  geom_bar(fill = "skyblue") + 
  theme_minimal() + 
  labs(title = "Distribution of Overall Ratings", x = "Rating", y = "Count")

# Most Common Traveler Types
ggplot(df, aes(x = TypeOfTraveller)) + 
  geom_bar(fill = "lightgreen") + 
  theme_minimal() + 
  labs(title = "Most Common Type of Traveller", x = "Type", y = "Count") + 
  coord_flip()

# Correlation Between Ratings
rating_cols <- df %>% select(SeatComfort, CabinStaffService, GroundService, ValueForMoney, OverallRating)
cor_matrix <- cor(rating_cols, use = "complete.obs")
corrplot(cor_matrix, method = "color", tl.col = "black", tl.srt = 45)

# Sentiment Analysis on ReviewBody -----
# Clean Text Data
df$clean_review <- tolower(df$ReviewBody)
df$clean_review <- removePunctuation(df$clean_review)
df$clean_review <- removeNumbers(df$clean_review)
df$clean_review <- removeWords(df$clean_review, stopwords("en"))

# Sentiment Scores
df$sentiment <- get_nrc_sentiment(df$clean_review)
df$sentiment_score <- df$sentiment$positive - df$sentiment$negative

# Visualization of Sentiment Distribution
ggplot(df, aes(x = sentiment_score)) +
  geom_histogram(fill = "coral", bins = 30) +
  theme_minimal() +
  labs(title = "Sentiment Score Distribution", x = "Sentiment Score", y = "Frequency")

# Word Cloud from Reviews -----
wordcloud(df$clean_review, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

# Save Cleaned Data
write.csv(df, "cleaned_airline_reviews.csv", row.names = FALSE)

