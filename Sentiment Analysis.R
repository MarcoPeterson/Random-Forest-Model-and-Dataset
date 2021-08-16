##  This is how to run the libraries first

library("NLP")
library("glue")
library("readr")
library("dplyr")
library("tidytext")
library("textdata")

## Connect to Snowflake

myconn <- DBI::dbConnect(odbc::odbc(), "cradlepoint", uid="MPETERSON", pwd='Welcome1@') 
warehouse <- DBI::dbGetQuery(myconn,"USE WAREHOUSE PROD_MGMT_XS")


## This is to connect to the dataset

fileName <- DBI::dbGetQuery(myconn,
"select REASON_RESPONSE
from PROD_MGMT_DB.ML.POLL_DATASET")

attach(fileName)
str(fileName)


fileName <- as.String(fileName)


##  This is how to create tokens with a string variable

fileName <- trimws(fileName)
fileText <- glue(read_file(fileName))
fileText <- gsub("\\$", "", fileText) 
tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
tokens

## This is how to filter down in the dataset and produce what you want

df4 <- tokens %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup() %>% top_n(50,n)

df4
## This is how to create the graph that was produced for Mike Hagman


df5 <- df4 %>% group_by(sentiment) %>% ungroup() %>% mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Total Count",
       y = "Most Prevelant Negative and Positive Words in Google Reviews for LA Fitness")


df5 

## Export file to Desktop to upload to Tableau

write.csv(df4, file = "Poll.csv", row.names = TRUE)




# # Define UI for app that draws a histogram ----
# ui <- fluidPage(
#   # App title ----
#   titlePanel("Histogram of Poll Responses"),
#   
#   # Sidebar layout with input and output definitions ----
#   sidebarLayout(
#     
#     # Sidebar panel for inputs ----
#     sidebarPanel(
#       
#       # Input: Slider for the number of bins ----
#       sliderInput(inputId = "bins",
#                   label = "Number of bins:",
#                   min = 1,
#                   max = 10,
#                   value = 30)
#       
#     ),
#     
#     # Main panel for displaying outputs ----
#     mainPanel(
#       
#       # Output: Histogram ----
#       plotOutput(outputId = "distPlot")
#       
#     )
#   )
# )
# 
# # Define server logic required to draw a histogram ----
# server <- function(input, output) {
#   
#   
#   output$distPlot <- renderPlot({
#     
#     x    <- fileName$HOW_LIKELY_TO_RECOMMEND
#     x    <- na.omit(x)
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
#     
#     hist(x, breaks = bins, col = "#75AADB", border = "black",
#          xlab = "Ozone level",
#          main = "Histogram of Ozone level")
#     
#   output$plot <- renderPlot({
#     
#     
#   })  
#     
#   })
#   
# }
# # Create Shiny app ----
# shinyApp(ui = ui, server = server)














