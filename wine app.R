#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

library(shiny)
library(dplyr)

wine_facts<- read.csv(file.choose())
wine_reviews<- read.csv(file.choose())

# Filter out rows with missing or blank brand values
wine_reviews <- wine_reviews %>%
  filter(!is.na(brand) & brand != "") 


# Calculate mean ratings for each brand-category combination
mean_ratings <- wine_reviews %>%
  group_by(brand, categories) %>%
  summarise(mean_rating = mean(reviews.rating, na.rm = TRUE), .groups = 'drop')

# Replace NA in reviews.rating with the mean_rating
wine_reviews <- wine_reviews %>%
  left_join(mean_ratings, by = c("brand", "categories")) %>%
  mutate(reviews.rating = ifelse(is.na(reviews.rating), mean_rating, reviews.rating)) %>%
  select(-mean_rating)


# For wine_reviews dataset
wine_reviews <- wine_reviews[!is.na(wine_reviews$reviews.username) & wine_reviews$reviews.username != "", ]
# For wine_facts dataset
wine_facts <- wine_facts[!is.na(wine_facts$reviews.username) & wine_facts$reviews.username != "", ]


# Joining the datasets on 'id', 'name', and 'reviews.username' after filtering
combined_data <- left_join(wine_reviews, wine_facts,  by = c("id", "name", "reviews.username"), suffix = c("_facts", "_reviews"))





ui <- fluidPage(
  titlePanel("Wine Recommendation System"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectedBrand", "Select Brand:", choices = NULL), # Placeholder for dynamic choices
      sliderInput("minRating", "Minimum rating:", min = 0, max = 5, value = 3)
    ),
    mainPanel(
      dataTableOutput("results")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Populate the brand choices dynamically from the dataset
  observe({
    updateSelectInput(session, "selectedBrand",
                      choices = c("All", unique(wine_reviews$brand[!is.na(wine_reviews$brand)])))
  })
  
  # Filter and display the results based on selected brand and minimum rating
  output$results <- renderDataTable({
    filtered_data <- wine_reviews
    if (input$selectedBrand != "All") {
      filtered_data <- filtered_data %>%
        filter(brand == input$selectedBrand, reviews.rating >= input$minRating)
    } else {
      filtered_data <- filtered_data %>%
        filter(reviews.rating >= input$minRating)
    }
    # Select specific columns to display
    filtered_data <- filtered_data %>%
      select(brand, categories, dimension, flavors, reviews.rating, reviews.title, reviews.text, reviews.username)
    filtered_data
  })
}

shinyApp(ui = ui, server = server)