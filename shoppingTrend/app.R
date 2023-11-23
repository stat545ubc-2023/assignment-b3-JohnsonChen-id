#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# In this app, features that are implemented are: 
# 1. Use the DT package to turn a static table into an interactive table. 
#    This greatly enhances the usefulness of the table in this app, allowing user to sort and search entries with great efficiency.
# 2. Show the number of results found whenever the filters change. 
#    This shows the user how many data entries that we aggregate with in the main plot. We cannot see the entries but it would be great to know how much summarization is done.
# 3. Allow the user to download your table as a .csv file.
#    This allows user to download the age-filtered data from their side to further interact with the dataset
# 4. Add an image to the UI.
#    This little shopping cart adds to the aesthetic of the page while making the app more visually interesting
# 5. If you have both a plot and a table, place them in separate tabs.
#    This separation divides output into independently viewable sections, making both the plot and table individually clearer

library(shiny)
library(tidyverse)
library(dplyr)
library(DT)
# Source: https://www.kaggle.com/datasets/iamsouravbanerjee/customer-shopping-trends-dataset/data
shopping <- read.csv("shopping_trends.csv")
shopping <- dplyr::select(shopping,-c(Customer.ID, Size,Color, Season, Subscription.Status,
                                      Discount.Applied,Promo.Code.Used, Shipping.Type,
                                      Preferred.Payment.Method,Payment.Method))

ui <- fluidPage(
    # Application title
    titlePanel("Shopping Trend Study"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("age",
                        "Select an age range:",
                        min = 7,
                        max = 70,
                        value = c(20,40)),
            img(src='shopping_cart.png', height="25%", width="25%",align = "right"),
            downloadButton("downloadData", "Download"),
        ),
        mainPanel(
          
          textOutput("rows"),
          tabsetPanel(
            tabPanel("plot",plotOutput("shopping_plot")),
            tabPanel("Table", dataTableOutput("table"))
          )
        )
    )
)

server <- function(input, output) {
    observe(print(input$age))
  input_filtered <- reactive({shopping %>% filter(between(Age,input$age[1],input$age[2])) })
    output$shopping_plot <- renderPlot({
       input_filtered() %>% group_by(Previous.Purchases,Gender) %>% 
        summarise(Purchase = mean(Purchase.Amount..USD.)) %>% 
        ggplot(aes(x = Previous.Purchases,y = Purchase, color = Gender)) + geom_point(alpha = 0.5)
    })
    output$table <-  renderDataTable({
      input_filtered()
    })
    output$rows <- renderText({
      paste("There is ", length(input_filtered()$Age), 
            " people's shopping records summarized in this age range")
    })
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("shopping_trend.csv")
      },
      content = function(file) {
        write.csv(input_filtered(), file)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
