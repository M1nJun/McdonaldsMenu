###################################################################################
# This R script should contain all relevant codes you have used to generate the Shiny app.
# Codes should be well-commented.
# Do NOT include codes that you have experimented with, or is not connected to the app.
###################################################################################
# install and load required packages here

# install.packages("package_name")   # uncomment if required
# library("package_name")   # uncomment if required

library(tidyverse)
library(rvest)
library(shiny)
library(fmsb) # for radar chart
library(rvest)
library(kableExtra) # for table

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
  select(Item, Calories, Total_Fat, Saturated_Fat, Carbohydrates, Sugars, Protein, Dietary_Fiber, Sodium)

daily_val_female_df <- df %>%
  mutate(Calories = (Calories / 2000) * 100,
         Total_Fat = (Total_Fat / 78) * 100,
         Saturated_Fat = (Saturated_Fat / 24) * 100,
         Carbohydrates = (Carbohydrates / 267) * 100,
         Sugars = (Sugars / 27) * 100,
         Protein = (Protein / 45) * 100,
         Dietary_Fiber = (Dietary_Fiber / 30) * 100,
         Sodium = (Sodium / 6) * 100) %>% 
  select(Item, Calories, Total_Fat, Saturated_Fat, Carbohydrates, Sugars, Protein, Dietary_Fiber, Sodium)


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



# colors for the radar plot
colors <- c("#FFB6C1", "#87CEEB", "#98FB98", "#FFD700", "#FFA07A", "#FFC0CB", "#B0E0E6", "#FFDAB9", "#ADD8E6", "#F0E68C")


################################################################################
# shiny app code

# define UI 
ui <- fluidPage(
  titlePanel("Mcdonalds Menu Nutrition"),
  
  fluidRow(
    column(2,
           radioButtons(inputId = "gender",
                        label = "Please select your biological gender:",
                        choices = c("Male", "Female"))),
    column(10,
           plotOutput("radar"))
  ),
  fluidRow(
    column(6,
           tableOutput("table")),
    column(6,
           tableOutput("table_sum"))
  ),
  fluidRow(
    column(4,
           checkboxGroupInput(inputId = "category",
                              label = "Pick your categories:",
                              choices = categories,
                              selected = "Breakfast")
    ),
    column(4,
           uiOutput("item_by_category")
    ),
    column(4,
           checkboxGroupInput(inputId = "info",
                              label = "Pick the information you want to display for your item:",
                              choices = c("Calories", "Total_Fat", "Saturated_Fat", "Carbohydrates", "Sugars", "Protein", "Dietary_Fiber", "Sodium"),
                              selected = "Protein"))
  ),
  fluidRow(
    column(6,
           varSelectInput(inputId = "yvar",
                          label = "Select Variable to Display on y-axis",
                          data = df %>% select(Serving_Size_grams, Calories, Calories_from_Fat, Total_Fat, Saturated_Fat, Trans_Fat, Cholesterol, Sodium, Carbohydrates, Dietary_Fiber, Sugars, Protein),
                          selected = "Protein"))
  ),
  fluidRow(
    column(6, plotOutput("bar"))
  )
  
)



# define server logic 
server <- function(input, output) {
  
  get_items <- reactive({
    items <- df %>% 
      filter(Category %in% input$category)
    return(items$Item)
  })
  
  get_avg <- reactive({
    avg <- df %>% 
      group_by(Category) %>% 
      dplyr::summarize(avg = mean(!!input$yvar, na.rm = TRUE))
    return(avg)
  })
  
  table_setup <- reactive({
    temp_df <- df %>%
      filter(Item %in% input$item) %>%
      select(Item, input$info)
  
    return(temp_df)
  })
  
  output$item_by_category <- renderUI({
    checkboxGroupInput(inputId = "item",
                       label = 'Choose your item of interest:',
                       choices = get_items(),
                       selected = "Egg McMuffin")
  })
  
  output$bar <- renderPlot({
    ggplot(data = get_avg(), aes(x = Category, y = avg)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = "Category")
  })
  
  output$table <- renderTable({
    table_setup()
  })
  
  output$table_sum <- renderTable({
    
    sum <- colSums(table_setup()[-1])
    
    nutrition <- colnames(table_setup()[-1])
    
    sum_table <- data.frame(Nutrition = nutrition, Sum = sum)
    
    sum_table <- pivot_wider(sum_table, names_from = "Nutrition", values_from = "Sum")
  })
  
  output$radar <- renderPlot({
    if (input$gender == "Male") {
      for_radar <- daily_val_male_df %>%
        filter(Item %in% c("a", "b", input$item)) %>%
        select(-Item)
    } else {
      for_radar <- daily_val_female_df %>%
        filter(Item %in% c("a", "b", input$item)) %>%
        select(-Item)
    }
    
    radarchart(for_radar,
               pcol = colors,
               cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, 20, 5), cglwd = 0.8,
               title = "How much does your menu fulfill daily Nutrition Intake?"
    )
    
    legend(x = 1.5, y = 1, legend = input$item, bty = "n", pch = 20, col = colors, cex = 1.2, pt.cex = 3, text.font = 0.7)
  })
  
}



# run the app
shinyApp(ui = ui, server = server)


################################################################################
