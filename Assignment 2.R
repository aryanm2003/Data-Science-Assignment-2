#QUESTION 1
data(iris)

boxplot(iris$Sepal.Length ~ iris$Species, 
        main = "Boxplots of Sepal.Length by Species",
        xlab = "Species",
        ylab = "Sepal.Length")
boxplot(iris$Petal.Length ~ iris$Species,
        main = "Boxplots of Petal.Length by Species",
        xlab = "Species",
        ylab = "Petal.Length")
# Create scatterplot
plot(iris$Sepal.Length, iris$Petal.Length,
     col = iris$Species,
     xlab = "Sepal.Length",
     ylab = "Petal.Length",
     main = "Scatterplot of Sepal.Length vs. Petal.Length")



#QUESTION 2
library(imager)

flip <- function(image) {
  width <- dim(image)[2]
  flipped_image <- as.cimg(array(0, dim = dim(image)))
  
  for (i in 1:width) {
    flipped_image[, i] <- rev(image[, i])
  }
  
  return(flipped_image)
}

# Example usage:
input_image <- load.image("C://Users//Dell//Pictures//Camera Roll//WhatsApp Image 2023-05-31 at 12.09.08 AM.jpeg")  # Replace with the path to your input image
flipped_image <- flip(input_image)

# Display the input and flipped images
par(mfrow = c(1, 2))
plot(input_image, main = "Input Image")
plot(flipped_image, main = "Flipped Image")




#QUESTION 3
library(MASS)

# Load the ships dataset
data(ships)

# Create a bar plot showing the number of accidents for each ship type
accidents_by_ship_type <- table(ships$type)
barplot(accidents_by_ship_type, xlab = "Ship Type", ylab = "Number of Accidents", main = "Accidents by Ship Type")

# Create a box plot comparing the number of accidents for each ship type
boxplot(n.accidents ~ type, data = ships, xlab = "Ship Type", ylab = "Number of Accidents", main = "Number of Accidents by Ship Type")

# Create a stacked bar plot showing the percentage of accidents for each ship type
total_accidents <- sum(accidents_by_ship_type)
accident_percentages <- round(accidents_by_ship_type / total_accidents * 100, 2)
barplot(accident_percentages, xlab = "Ship Type", ylab = "Percentage of Accidents", main = "Percentage of Accidents by Ship Type")

# Test hypothesis by comparing mean number of accidents for each ship type
mean_accidents_by_type <- tapply(ships$n.accidents, ships$type, mean)
print(mean_accidents_by_type)

# Perform a one-way ANOVA to test for significant differences in accidents among ship types
anova_result <- aov(n.accidents ~ type, data = ships)
summary(anova_result)



#QUESTION 4
library(rvest)

# Specify the URL of the website
url <- "https://stats.stackexchange.com/questions?tab=Votes"

# Read the HTML content of the webpage
page <- read_html(url)

# Scrape the required information
titles <- page %>% html_nodes(".question-hyperlink") %>% html_text()
views <- page %>% html_nodes(".views") %>% html_text() %>% gsub("\\D+", "", .)
answers <- page %>% html_nodes(".status") %>% html_text() %>% gsub("\\D+", "", .)
votes <- page %>% html_nodes(".vote-count-post") %>% html_text()

# Ensure all vectors have the same length
max_length <- max(length(titles), length(views), length(answers), length(votes))
titles <- rep(titles, length.out = max_length)
views <- rep(views, length.out = max_length)
answers <- rep(answers, length.out = max_length)
votes <- rep(votes, length.out = max_length)

# Create a dataframe
data <- data.frame(
  Title = titles,
  Views = as.integer(views),
  Answers = as.integer(answers),
  Votes = as.integer(votes)
)

# Display the dataframe
print(data)




#QUESTION 5
set.seed(123)  # Set a seed for reproducibility

num_trials <- 10000  # Number of trials to run
days <- vector(length = num_trials)  # Vector to store the number of days for each trial

for (i in 1:num_trials) {
  bottle <- rep(2, 100)  # Create a fresh bottle with 100 whole tablets (value 2)
  count <- 0  # Counter for the number of days
  
  while (TRUE) {
    pull <- sample(length(bottle), 1)  # Randomly select an index from the bottle
    
    if (bottle[pull] == 1) {
      count <- count + 1  # Increment the day count when a half-tablet is pulled
      break
    } else {
      bottle[pull] <- 1  # Change a whole tablet to a half-tablet
    }
  }
  
  days[i] <- count  # Store the number of days for this trial
}

average_days <- mean(days)  # Calculate the average number of days

# Print the result
cat("On average, it will take", average_days, "days to pull a half-tablet out of the bottle.")









