library(tidyverse)
library(rvest)
library(shiny)
library(fmsb) # for radar chart
library(rvest)

################################################################################
# write R code here (such as, load dataset, any code prior to the app, etc.)


# data wrangling for main dataset
df <- read.csv("menu.csv")


colnames(df) <- gsub("\\.", "_", colnames(df)) # replace . with _
colnames(df) <- gsub("_+", "_", colnames(df)) # replace lots of _ with a single _
colnames(df) <- gsub("_+$", "", colnames(df)) # strips appearance of _ at the end of a name
df$Serving_Size_grams <- as.numeric(gsub(".*\\((\\d+) g\\).*", "\\1", df$Serving_Size)) # extracting grams as numeric
df <- df[, -which(names(df) == "Serving_Size")] # getting rid of the original col after extracting
df <- df[!grepl("(Large Biscuit)|(Child)|Egg Whites", df$Item), ] # getting rid of observations that are too obscure

df <- df %>% 
  mutate(Sodium = Sodium / 1000)




categories <- unique(df$Category) # 9 categories



# helper function

# extract_numeric <- function(string) {
#   as.numeric(gsub("[^0-9.]", "", string))
# }

# function that extracts the very first occurence of a number
extract_numeric <- function(string) {
  numeric_start <- regexpr("[0-9.]+", string) # returns the pos of first num occurence
  numeric_end <- numeric_start + attr(numeric_start, "match.length") - 1
  as.numeric(substr(string, numeric_start, numeric_end))
}

# helper dataset

page <- read_html("https://www.nutrition.org.uk/life-stages/men/nutrition-recommendations-for-men/")

tables <- page %>% 
  html_elements("table") %>% 
  html_table()

male_table <- tables[[1]]
male_table <- pivot_wider(male_table, names_from = "X1", values_from = "X2")

male_table <- mutate_all(male_table, funs(extract_numeric))

male_table <- male_table %>%
  rename(Calories = Energy,
         Carbohydrates = Carbohydrate,
         Sugars = `Free sugars`,
         Sodium = Salt,
         Dietary_Fiber = Fibre,
         Saturated_Fat = `Saturated fat`)


page2 <- read_html("https://www.nutrition.org.uk/life-stages/women/nutrition-recommendations-for-women/")

tables2 <- page2 %>% 
  html_elements("table") %>% 
  html_table()

female_table <- tables2[[1]]
female_table <- pivot_wider(female_table, names_from = "X1", values_from = "X2")

female_table <- female_table %>%
  rename(Calories = Energy,
         Carbohydrates = Carbohydrate,
         Sugars = `Free sugars`,
         Sodium = Salt,
         Dietary_Fiber = Fibre,
         Saturated_Fat = `Saturated fat`)

female_table <- mutate_all(female_table, funs(extract_numeric))




daily_val_male_df <- df %>%
  mutate(Calories = (Calories / 2500) * 100,
         Total_Fat = (Total_Fat / 97) * 100,
         Saturated_Fat = (Saturated_Fat / 31) * 100,
         Carbohydrates = (Carbohydrates / 333) * 100,
         Sugars = (Sugars / 33) * 100,
         Protein = (Protein / 55) * 100,
         Dietary_Fiber = (Dietary_Fiber / 30) * 100,
         Sodium = (Sodium / 6) * 100) %>%
  select(Item, Calories, Total_Fat, Saturated_Fat, Saturated_Fat, Carbohydrates, Sugars, Protein, Dietary_Fiber, Sodium)

daily_val_female_df <- df %>%
  mutate(Calories = (Calories / 2000) * 100,
         Total_Fat = (Total_Fat / 78) * 100,
         Saturated_Fat = (Saturated_Fat / 24) * 100,
         Carbohydrates = (Carbohydrates / 267) * 100,
         Sugars = (Sugars / 27) * 100,
         Protein = (Protein / 45) * 100,
         Dietary_Fiber = (Dietary_Fiber / 30) * 100,
         Sodium = (Sodium / 6) * 100) %>% 
  select(Item, Calories, Total_Fat, Saturated_Fat, Saturated_Fat, Carbohydrates, Sugars, Protein, Dietary_Fiber, Sodium)


dummy_rows <- data.frame(
  Item = c("a", "b"),
  Calories = c(100, 0),
  Total_Fat = c(100, 0),
  Saturated_Fat = c(100, 0),
  Carbohydrates = c(100, 0),
  Sugars = c(100, 0),
  Protein = c(100, 0),
  Dietary_Fiber = c(100, 0),
  Sodium = c(100, 0)
)

# Combine the dummy rows with the existing dataframe to set min,max
daily_val_male_df <- rbind(dummy_rows, daily_val_male_df)
daily_val_female_df <- rbind(dummy_rows, daily_val_female_df)

for_radar_male <- daily_val_male_df %>% 
  filter(Item %in% c("Big Mac", "a", "b")) %>% 
  select(-Item)

radarchart(for_radar_male)





df %>%
  add_row(
    Item = "Sum",
    across(where(is.numeric), ~sum(.))
  )
