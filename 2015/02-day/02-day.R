# The elves are running low on wrapping paper, and so they need to submit an order for more. 
# They have a list of the dimensions (length l, width w, and height h) of each present, 
# and only want to order exactly as much as they need.
# 
# Fortunately, every present is a box (a perfect right rectangular prism), 
# which makes calculating the required wrapping paper for each gift a little easier: 
#   find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. 
# The elves also need a little extra paper for each present: the area of the smallest side.
# 
# For example:
#   LxWxH
# A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet of wrapping paper 
# plus 6 square feet of slack, for a total of 58 square feet.
# A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet of wrapping paper 
# plus 1 square foot of slack, for a total of 43 square feet.
# All numbers in the elves' list are in feet. 
# How many total square feet of wrapping paper should they order?

library(tidyverse)

myinput <- read_csv("day2-input.csv", col_names = FALSE, 
                    na = c("NA", " ", ""), trim_ws = TRUE, skip_empty_rows = TRUE) %>%
  setNames(., "dims") %>%
  separate("dims", into = c("l", "w", "h"), sep = "x", remove = TRUE) %>%
  mutate(l = as.numeric(l),
         w = as.numeric(w),
         h = as.numeric(h))

sides <- myinput %>%
  mutate(s1 = 2 * l * w,
         s2 = 2 * w * h,
         s3 = 2 * h * l) %>%
  mutate(lxw = l * w,
         lxh = l* h,
         wxh = w * h) %>%
  select(-l, -w, -h)

sums <- sides %>%
  rowwise() %>%
  mutate(min_side = min(lxw, lxh, wxh)) %>%
  mutate(sum = sum(s1, s2, s3, min_side))
  

sum(sums$sum)



# --- Part Two ---
# The elves are also running low on ribbon. Ribbon is all the same width, 
# so they only have to worry about the length they need to order, 
# which they would again like to be exact.
# 
# The ribbon required to wrap a present is the shortest distance around 
# its sides, or the smallest perimeter of any one face. Each present also 
# requires a bow made out of ribbon as well; the feet of ribbon required for 
# the perfect bow is equal to the cubic feet of volume of the present. 
# Don't ask how they tie the bow, though; they'll never tell.
# 
# For example:
#   
# A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon to wrap the present 
# plus 2*3*4 = 24 feet of ribbon for the bow, for a total of 34 feet.
# A present with dimensions 1x1x10 requires 1+1+1+1 = 4 feet of ribbon to wrap the present 
# plus 1*1*10 = 10 feet of ribbon for the bow, for a total of 14 feet.
# How many total feet of ribbon should they order?

# smallest perimeter --> so shortest two sides ?


bows <- myinput %>%
  mutate(s1 = 2 * l * w,
         s2 = 2 * w * h,
         s3 = 2 * h * l) %>%
  mutate(bow = l * w * h) %>%
  mutate(oneside = l * 2 + w * 2,
         twoside = l * 2 + h * 2,
         threeside = h * 2 + w * 2) %>%
  select(bow:threeside) %>%
  rowwise() %>%
  mutate(min_side = min(oneside, twoside, threeside)) %>%
  mutate(sum = sum(bow, min_side))

sum(bows$sum)
