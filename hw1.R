# Ubaidillah bin Mohammad Razali
# ubrzl@uw.edu
#I worked on this assignment alone using this quarter's course materials and websites below:
# https://www.digitalocean.com/community/tutorials/get-number-of-rows-and-columns-in-r
# https://www.statology.org/round-in-r/
# https://www.geeksforgeeks.org/binding-rows-and-columns-of-a-data-frame-in-r-bind_rows-and-bind_cols-function/

library("stringr") #loads in the stringr library, all library statements must be at the top of the page! 
library(testthat)
library(dplyr)
df <- read.csv("2013_Video_Games_Dataset.csv") #loads in your dataset DO NOT CHANGE!!

# Overview ---------------------------------------------------------------------

# Homework 1: Video Game Market Analysis
# Before you begin, make sure you read the assignment description on canvas first!
# For each question/prompt, write the necessary code to calculate the answer.
# For grading, it's important that you store your answers in the variable names
# listed with each question in `backtics`. Make sure you DO NOT hardcode values
# unless specified to do so in the instructions! Do not load in any additional 
# libraries and do not change the address for the dataset file. 

# Part 1: Background Math ------------------------------------------------------
#
# In this section, you will be calculating some general numbers about this dataset
# that you'll need later as reference values!
#
# ------------------------------------------------------------------------------

# How many games in total are in this dataset? Store this value into a variable called `total_games`
`total_games` <- nrow(df)

# How many games in this dataset are sequels? Store this value into a variable called `total_seq`
`total_seq` <- nrow(df[df$Sequel == 1, ])

# What percentage of games in the dataset are sequels? Round this number up to the nearest whole number/integer 
# and store into a variable called `perc_seq`
`perc_seq` <- round((total_seq / total_games) * 100)

# How many games in this dataset were published by an AAA studio? Store this value into a variable called `total_AAA_games`
# AAA in this case means the publisher was either Activision, Nintendo, Rockstar, Sony, Disney, Electronic Arts, or Microsoft.
# Make sure you don't double count games who are produced by multiple AAA publishers
# i.e if a game is produced by both Microsoft and Nintendo you should only count that game once!
# If you are having trouble, check out the stringr library documentation for functions that might be able to help you do this. 
`total_AAA_games` <- nrow(df[str_detect(df$Publisher,"Activision") | str_detect(df$Publisher,"Nintendo") | str_detect(df$Publisher,"Rockstar") | str_detect(df$Publisher,"Sony") | str_detect(df$Publisher,"Disney")| str_detect(df$Publisher,"Electronic Arts")| str_detect(df$Publisher,"Microsoft"), ])

# What percentage of games where published by an AAA studio? Round this number up to the nearest whole number
# and store into a variable called `perc_AAA`. You should use the round function for this task. 
`perc_AAA` <- round((total_AAA_games / total_games) * 100) 

# How many games in this dataset were published by a company that also made the same console hardware 
# many of the video games are played on? i.e how many of these games are published by Nintendo, Microsoft, or Sony? 
# Store this value into a variable called `total_hardware_games`

others <- filter(df, str_detect(df$Publisher, substr(df$Console, 1, 5))) 
xbox <- filter(df, df$Console == "X360" & str_detect(df$Publisher, "Micro" ))
ps <- filter(df, df$Console == "PlayStation 3" & str_detect(df$Publisher, "Sony" ))
xbox_ps3 <- bind_rows(xbox, ps)
`total_hardware_games` <- bind_rows(others,xbox_ps3)


# What percentage of games where published by a company that also makes the console hardware? Round this number up to the nearest whole number
# and store into a variable called `perc_hardware`
`perc_hardware` <- round((nrow(total_hardware_games) / total_games) * 100) 




# Part 2: Examining Trends Over Time -------------------------------------------
#
# In this section, you will be examining trends over time in the dataset! You'll
# write code to see how the video game release cycle differed between years to help
# tell the story about how video game release schedules are impacted by the 
# release of certain gaming consoles. 
#
# ------------------------------------------------------------------------------

# Fill in the function below called `num_games_per_yr` that takes in a numeric parameter `year`
# and returns the number of video games produced that year. 

`num_games_per_yr` <- function(year){
  number <- nrow(df[df$YearReleased == year, ])
  return(number)
}

# Using your `num_games_per_yr` function, find out how many games were produced in 2004.
# Store this value in a variable called `games_2004`
`games_2004` <- num_games_per_yr(2004)

# Using your `num_games_per_yr` function, find out how many games were produced in 2005.
# Store this value in a variable called `games_2005`
`games_2005` <- num_games_per_yr(2005)

# Using your `num_games_per_yr` function, find out how many games were produced in 2010.
# Store this value in a variable called `games_2005`
`games_2010` <- num_games_per_yr(2010)

# Using your `num_games_per_yr` function, create a vector called `games_per_yr` 
# that stores the number of games released each year in the dataset in order. 
# This means that your vector at index 1 should store the number of games released in 2004
# and at index 2 should store the number of games released in 2005, and so on. 
`games_per_yr` <- c()
for (n in 1:length(unique(df$YearReleased))){
    `games_per_yr`[n] <- num_games_per_yr(unique(df$YearReleased)[n])
  }



# Using the variables `games_2004`, `games_2005`, and `games_2010` check if elements
# in your vector `games_per_yr` are in the correct order. Meaning the first index 
# in your vector should store the same value as games_2004, the second index should
# store the same value as `games_2005`, and the final index should store the same 
# value as `games_2010`.For this check, you should create a variable named `correct_order` 
# that stores TRUE if the elements are in the correct order, and FALSE if they are not. 
g2004 <- games_2004 == games_per_yr[1]
g2005 <- games_2005 == games_per_yr[2]
g2010 <- games_2010 == games_per_yr[7]
`correct_order` <- c(g2004, g2005, g2010) 


# Now create a vector called `diff_per_yr` that stores the the differences between 
# the number of games released each year. For example, this vector at index 1 should 
# store the difference between games released in 2005 versus 2004; at index 2 should
# store the difference between games released in 2006 versus 2005. 
# Note: This vector should have a different length than your earlier `games_per_yr` vector. 
`diff_per_yr` <- c()
r <- 1 
while (r != length(unique(df$YearReleased))){
  `diff_per_yr`[r] <- games_per_yr[r+1] - games_per_yr[r]
  r <- r + 1
}


# Now create a vector called `perc_per_yr` that stores the the PERCENTAGE difference/change
# between the number of games released each year. For example, this vector at index 1 should 
# store the percentage difference between games released in 2005 versus 2004.
# You do not need to round these values. 
# Note: This vector should have the SAME length as the `diff_per_yr` vector. 
# HINT -- use the two vectors you created earlier, just keep in mind that your 
# two vectors are different lengths so you may want to "slice" one of your vectors. 
`perc_per_yr` <- c()
for (t in 1:length(`diff_per_yr`)){
  if (t != 7){
    change <- diff_per_yr[t] 
    early <- games_per_yr[t]
    `perc_per_yr`[t] <- (change / early) * 100
  }
}


#To run the test file, uncomment the line below before running source. 
test_file("hw1_test.r")

