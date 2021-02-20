### ADVENT OF CODE 2015 ###
###      DAY 1          ###


library(tidyverse)
library(readtext)

# Santa is trying to deliver presents in a large apartment building, 
# but he can't find the right floor - the directions he got are a little confusing. 
# He starts on the ground floor (floor 0) and then follows the instructions one character at a time.
# 
# An opening parenthesis, (, means he should go up one floor, 
# and a closing parenthesis, ), means he should go down one floor.
# 
# The apartment building is very tall, and the basement is very deep; 
# he will never find the top or bottom floors.
# 
# For example:
# 
# (()) and ()() both result in floor 0.
# ((( and (()(()( both result in floor 3.
# ))((((( also results in floor 3.
# ()) and ))( both result in floor -1 (the first basement level).
# ))) and )())()) both result in floor -3.

# To what floor do the instructions take Santa?

myinput <- readtext("advent-of-code/2015/01-day/day1-input.txt") %>%
  pull(text)

ups <- str_count(myinput, "\\(")
downs <- str_count(myinput, "\\)")

ups - downs


# Now, given the same instructions, find the position of the first character 
# that causes him to enter the basement (floor -1). The first character in the instructions 
# has position 1, the second character has position 2, and so on.
# 
# For example:
#   
#   ) causes him to enter the basement at character position 1.
# ()()) causes him to enter the basement at character position 5.
# What is the position of the character that causes Santa to first enter the basement?

# make each () a separate element
split <- unlist(strsplit(myinput, ""))

# Replace with 1 or -1
num.split <- str_replace_all(split, "\\(", "1") %>%
  str_replace_all("\\)", "-1") %>%
  as.numeric()

# Create a blank column to populate
add <- matrix(data = rep(0, length(num.split)), ncol = 1)

# Sum each element in the vector
for (i in 1:length(num.split)){
  
  add[i, 1] <- sum(num.split[1:i])
  
}

# Find position where Santa first enters the basement!
which(add == -1)
