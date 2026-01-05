install.packages("readr","dplyr")
library(readr)
library(dplyr)

#clean the dataset
data <- read_csv("movies.csv")
clean_movies <- movies %>%
  filter(gross > 0,                  # Only movies that earned money
         budget > 0,                 # Only movies with known budget
         !is.na(score),              # Must have rating
         !is.na(runtime),            # Must have runtime
         !is.na(genre),              # Must have genre
         votes >= 100) %>%           # At least 100 votes for reliable rating

  filter(gross < quantile(gross, 0.99),      # Remove top 1% highest gross
         budget < quantile(budget, 0.99)) %>%
  
# Clean genre column (remove extra spaces)
  mutate(genre = trimws(genre))

# Check how many movies left after cleaning
print(paste("Original movies:", nrow(movies)))
print(paste("Cleaned movies:", nrow(clean_movies)))

# See first few rows
head(clean_movies)  

install.packages("ggplot2")
library(ggplot2)

ggplot(clean_movies, aes(x = score, y = gross / 1000000)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(color = "darkblue") +
  labs(title = "Higher Ratings → More Box Office Revenue?",
       x = "IMDb Rating (out of 10)",
       y = "Worldwide Gross (Millions $)") +
  theme_minimal()

ggplot(clean_movies, aes(x = budget / 1000000, y = gross / 1000000)) +
  geom_point(color = "red", alpha = 0.6) +
  geom_smooth(color = "darkred") +
  labs(title = "Bigger Budget → Much More Revenue",
       x = "Budget (Millions $)",
       y = "Worldwide Gross (Millions $)") +
  theme_minimal()

ggplot(clean_movies, aes(x = runtime, y = gross / 1000000)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_smooth(color = "darkgreen") +
  labs(title = "Optimal Movie Length for Revenue",
       x = "Runtime (minutes)",
       y = "Worldwide Gross (Millions $)") +
  theme_minimal()


install.packages("tidyr")
library(tidyr) 

genre_split <- clean_movies %>%
  separate_rows(genre, sep = ",") %>%
  mutate(genre = trimws(genre))  # Remove extra spaces

ggplot(genre_split, aes(x = reorder(genre, gross, median), y = gross / 1000000)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  coord_flip() +  # Flip for better reading
  labs(title = "Which Genre Makes the Most Money?",
       x = "Genre",
       y = "Worldwide Gross (Millions $)") +
  theme_minimal()

write.csv(clean_movies, "cleaned_movies_for_research.csv", row.names = FALSE)

