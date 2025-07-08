library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(purrr)



# Define UI ----

ui <- dashboardPage(
  dashboardHeader(title = "Pokemon TCG Dash"),
  
  dashboardSidebar(sidebarMenu(
    #Adding in sidebar menu items for the different tabs
    menuItem("About", tabName = "About", icon = icon("dashboard")),
    menuItem("Data Download", tabName = "Data_Download", icon = icon("th")))),
  
  dashboardBody(tabItems(
    tabItem(tabName = "About",
            fluidRow(
              box(title = "About the App", 
                  p("I created this app for a project for ST558 (Data Science for Statisticians) at NC State. 
          It is an app that pulls from a database about Pokemon TCG Cards and can return loads of information about different things
          such as what sets there are, types of Pokemon, card prices, card rarity, and so much more. Additionally, you can turn this information 
          into graphs to visually see the breakdowns. I grew up playing Pokemon on my Nintendo DS with my brother. He collected cards 
          long before I did, but now I'm the one playing once a week at a local Pokemon League while he's chosen to stick to the video game!
          I've loved this topic for quite some time and this app is on a topic I find very interesting and fun. I hope you are able to use it 
          to be curious about the Pokemon TCG like I am.")),
              
              box(title = "Source Data", 
                  p("The data that is able to be pulled from this API is broken down very well. I found that searching by set or by pokemon
          is the most useful, but you can also search by other things like type or rarity. The person that created this API is named 
          Andrew Beckes, and the card list is extensive, updated for every new set that releases."), 
                  tags$a("Here is a link to the website I used", href = "https://docs.pokemontcg.io/"))),
            
            fluidRow(
              tabBox(
                title = "Tab Info",
                # From class source: The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "250px",
                tabPanel("About",
                         p("You're already here! This tab is to give a brief overview of what this app is about, where the data came from,
                 a bit about why I chose the topic, and what the app can do.")),
                tabPanel("Data Download", 
                         p("The Data Download can allow the user (you!) to specify changes to your API querying functions and return data, then 
                 display that returned data. Additionally, if you want to you can subset the data, and save it as a seperate file.")),
                tabPanel("Data Exploration", "The Data Exploration tab can allow the user (you!) to choose variables/combinations of variables 
                 that are summarized via numerical and graphical summaries. There are options for how you may want to do this, so go check
                 it out!")
              ),
              box(tags$img(src = "TCG.png", height = "200px"))
            )
    ),
    tabItem(tabName = "Data_Download",
            fluidRow(
              box(title = "Query Options",
                  selectInput(inputId = "subject", label = "Search for:", choices = c("cards", "sets", "types"), multiple = FALSE),
                  textInput(inputId = "set_name", label = "Set Name", placeholder = "Celebrations"),
                  textInput(inputId = "pokemon_name", label = "Pokemon Name", placeholder = "Pikachu"),
                  uiOutput(outputId = "selection"),
                  actionButton(inputId = "search", "Search")
              ),
              
              box(title = "Plot Output Options for Card Searches Only:",
                  selectInput("plot_type", "Choose a plot:",
                              choices = c("Rarity Count", "Type Count", "Price by Rarity"))
              ),
              box(plotOutput("bargraph")), 
            ),
            
            fluidRow(
              box(title = "Search Results", DT::DTOutput("datatable"), width = NULL, collapsible = TRUE),
              box(downloadButton("download_data", "Download data"))
            )
    )
    
  )
  )
)


# Define server logic ----
server <- function(input, output, session) {
  
  queried_data <- reactiveVal() #stores reactive values
  selected_vars <- reactiveVal() #for the subset
  
  observeEvent(input$search, {
    df <- query_function(subject = input$subject, set.name = input$set_name, 
                         pokemon.name = input$pokemon_name, select = NULL)
    queried_data(df)
    selected_vars(NULL) 
  }) #reading the reactive values from the input
  
  output$selection <- renderUI({
    req(queried_data())
    selectizeInput( inputId = "select", label = "Subset the Data here (Optional)",
                    choices = names(queried_data()), selected = selected_vars(), multiple = TRUE)
  })
  
  # Reactive subset of queried data
  subsetted_data <- reactive({
    req(queried_data(), input$select)
    queried_data()[, input$select, drop = FALSE]
  })
  
  observeEvent(input$select, {
    selected_vars(input$select)
  }, ignoreNULL = FALSE) # still will calculate when there are NULL values
  #updates with each new selection
  
  output$datatable <- DT::renderDataTable({
    req(queried_data(), selected_vars())
    subsetted_data()
  }) #subsetting
  
  output$bargraph <- renderPlot({
    data <- queried_data()
    
    if (input$plot_type == "Rarity Count") {
      ggplot(data, aes(x = rarity)) +
        geom_bar(fill = "blue") +
        labs(title = "Card Rarity")
      
    } else if (input$plot_type == "Type Count") {
      if ("types" %in% names(data)) {
        types_df <- data |> tidyr::unnest(types) |> count(types)
        ggplot(types_df, aes(x = types, y = n)) +
          geom_bar(stat = "identity", fill = "blue") +
          labs(title = "Card Types")
      } else {
        ggplot() + 
          annotate("text", x = 1, y = 1, label = "Types was not a selected variable") + 
          theme_void()
      }
      
    } else if (input$plot_type == "Price by Rarity") {
      data2 <- queried_data() |>
        select(name, rarity, tcgplayer) |>
        unnest_wider(tcgplayer) |> unnest_wider(prices) |> unnest_wider(holofoil)  |>
        filter(!is.na(low) | !is.na(mid) | !is.na(high))
      
      ggplot(data2, aes(x = rarity, y = mid)) +
        geom_boxplot(fill = "blue") +
        labs(title = "Card Price by Card Rarity", y = "Price", x = "Rarity")
    }
  })
  
  
  # Download file
  output$download_data <- downloadHandler(
    filename = "pokemontcg_data.csv",
    content = function(file) {
      write.csv(subsetted_data(), file, row.names = FALSE)
      #so that the csv is the subsetted data, not the original
    }
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)