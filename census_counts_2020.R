# November 15, 2021
# YSE603
# Process census data to get percent African American by county
# ~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o #

# First things first. Read the datasets in and look at them.

df <- read.csv("/Users/jrm236/Dropbox (Yale_FES)/FES_EDVC/lab_materials/ccam/ycom/county_race_2020_table.csv")
fips <- read.csv("/Users/jrm236/Dropbox (Yale_FES)/FES_EDVC/lab_materials/ccam/ycom/fipsindex.csv")

View(df)
View(fips)

# Think about what we're trying to do.
# Our goal is to get the percentage of African Americans in each county.
# Here is one approach (there are always multiple ways to accomplish your goal).

# First add the counties (which is in the fips dataset) to the dataset that has the race information (df)
# To join the datasets, they must share a common field, which we must create.
# We can try to break apart "01001.Black" into two parts, to separate out the fips code.
# Then we can paste a zero before the fips code in the GEOID field so both are exactly five digits.
# Then the two fields will be able to be joined because there will be identical values that can be matched
# on a one-to-one basis. If that works we can calculate the percentages of African Americans in each county.

# ~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o #
# Step 1. Break apart "01001.Black" in the df dataset
# Google: "stringr extract everything before" and it takes you here as the top search result:
# https://stackoverflow.com/questions/40113963/how-to-extract-everything-until-first-occurrence-of-pattern
# Adapt this: str_extract("L0_123_abc", "[^_]+") to fit our case

library(tidyverse)
str_extract("01001.Black", "[^.]+") # replace the string with our string, and replace the underscore with a period

# Use the tidyverse and mutate to create a new variable with the new string containing the extracted fips code
# We'll just call the new variable "fips".

df2 <- df %>%  
  mutate(fips=str_extract(Group.1, "[^.]+"))

# ~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o #
# Step 2. Paste a zero before the fips code in the GEOID column
# Google "stringr padding" and it takes you here with the top search result
# https://stringr.tidyverse.org/reference/str_pad.html
# Copy and paste the instructions at the top:

# str_pad(
#   string,
#   width,
#   side = c("left", "right", "both"),
#   pad = " ",
#   use_length = FALSE
# )

# fill in the parameters based on what we want and test it out
str_pad("1001",5,side = "left",pad = "0")

# Apply to the whole dataset by using mutate again to store the new variable in its own column.
# We'll call the new variable here "fips" again so that when we join the two datasets R will see that these contain 
# the same kinds of information and so it can seamlessly merge them without any confusion. 
# This merge process is the major challenge when creating data for mapping. You have to create a consistent variable, shared across
# the datasets that you want to link together. This ensures that all your data values are assigned to a unique geographic unit,
# which will in turn be associated with a specific shape or area (also known as a polygon) on your map.

fips2 <- fips %>% 
  mutate(fips = str_pad(GEOID, 5, side = "left", pad = "0"))

# Check now to see if both df2 and fips2 have a field with identical values that are strings of five numbers
# Note that for every row in fips2, there are many rows in df2, because each County has a count for each race
# Our task now is to summarise the multiple race counts within each County to get the percentage of black people per county
# But first we must join the County fips code (and all associated data, like county and state names, to the df2 file)

# test it out
merge(df2, fips2, by = "fips")

# save it 
df3 <- merge(df2, fips2, by = "fips")

# get rid of everything else to avoid confusion
rm(df, df2, fips, fips2)

# ~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o~~~o #
# Step 3. Calculate percentages of each race by County
# Google: tidyverse 

# The top search result is this:
# https://quantpalaeo.wordpress.com/2018/04/29/how-to-calculate-percent-from-counts-in-r/
# and skimming down the page I find this line, which seems promising: 
# mutate(percent = count / sum(count) * 100)
# Since our dataset is already in long format, we can skip the "gather" command.
# However, we do need to use "group_by" beforehand so that the calculations operate on each County as its own unit.

# Try it, being sure to replace the word "count" with "n" which is the variable that holds the actual counts in our dataset.

df4 <- df3 %>% group_by(County) %>% 
  mutate(percent = n / sum(n) * 100)

# Check the math for the first county by typing the values into the console.
# > 7893+1033+1198+32050
# [1] 42174
# > 7893/42174
# [1] 0.1871532
# This matches the values that were added to the new field we just created called "percent."
# Hooray. We have the data we actually want to map now... we actually have too much data, so we can subset easily.

# Subset rows to just include percentages of African American people.
# Filter is the tidyverse command to select certain rows based on a condition.
# I googled "R tidyverse string contains" 
# https://stringr.tidyverse.org/reference/str_detect.html

df4 %>% str_detect(Group.1, "Black") # fails because str_detect is not reading df4
str_detect(df4$Group.1, "Black") # this works.. but how to implement?
# the key here is to insert "filter" beforehand 
# Googling "r tidyverse select rows based on criteria" will point you to the "filter" command.

df5 <- df4 %>% filter(str_detect(Group.1, "Black")) 
# can round the percentages

df5 <- df4 %>% filter(str_detect(Group.1, "Black")) %>% 
  mutate(percent = round(percent, 2))

df6 <- df5 %>% select(fips, County, percent)
write.csv(df6, "/Users/jrm236/Dropbox (Yale_FES)/FES_EDVC/lab_materials/ccam/ycom/pct_afam_county_2020.csv", row.names=F)

# BONUS CHALLENGE:
# How could you get percentages by state?


