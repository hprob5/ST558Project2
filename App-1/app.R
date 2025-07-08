library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)


# Define UI ----

ui <- dashboardPage(
  dashboardHeader(title = "Pokemon TCG Dash"),
  
  dashboardSidebar(sidebarMenu(
    #Adding in sidebar menu items for the different tabs
    menuItem("About", tabName = "About", icon = icon("dashboard")),
    menuItem("Data Download", tabName = "Data_Download", icon = icon("th")),
    menuItem("Data Exploration", tabName = "Data_Exploration", icon = icon("chart-bar")))),
  
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
              )), # all of the query options
            fluidRow(
              box(title = "Search Results", DT::DTOutput("datatable"), width = NULL, collapsible = TRUE),
              box(downloadButton("download_data", "Download data"))
            ) # this created a collapsible output of the searched data
    ),
    tabItem(tabName = "Data_Exploration", 
        fluidRow(
            box(title = "Variable Selection", width = 4,
                selectInput("x", "Select X Variable:", choices = NULL),
                uiOutput("y_ui"),  # dynamic element comes from x
                uiOutput("faceting_ui"), # dynamic, also changes based on prev. selections
                selectInput("summary_type", "Summary Type:",
                            choices = c("Mean","Count")),
                selectInput("plot_type", "Plot Type:",
                            choices = c("Histogram","Boxplot", "Barplot", "Scatterplot"))
            ),
            box(title = "Summary Output", width = 8, verbatimTextOutput("summary_out")),
            box(title = "Plot Output", width = 8, plotOutput("summary_plot"))
            )
      )
  )
 )
)

# Define server logic ----
server <- function(input, output, session) {
  #Data Download Tab
  queried_data <- reactiveVal() #stores reactive values
  selected_vars <- reactiveVal() #for the subset
  
  #Query search
  observeEvent(input$search, {
    df <- query_function(subject = input$subject, set.name = input$set_name, 
                         pokemon.name = input$pokemon_name, select = NULL) |> 
      unnest_wider(tcgplayer) |> unnest_wider(prices) |> unnest_wider(holofoil) |>
      rename("lowendprice" = "low","aboutrightprice" = "mid", "highendprice"= "high")
    queried_data(df)
    selected_vars(NULL) 
  }) #reading the reactive values from the input
  #unnests price from the tcgplayer nested variable, bc price is very interesting to plot and it cant be nested
   
  
  output$selection <- renderUI({
    req(queried_data())
    selectizeInput( inputId = "select", label = "Subset the Data here (Optional)",
                    choices = names(queried_data()), selected = selected_vars(), multiple = TRUE)
  }) #these is the subset section
  
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
  
  
  # Download file
  output$download_data <- downloadHandler(
    filename = "pokemontcg_data.csv",
    content = function(file) {
      write.csv(subsetted_data(), file, row.names = FALSE)
      #so that the csv is the subsetted data, not the original
    }
  )
 
  #Data Exploration Tab
  observe({
    req(queried_data())
    updateSelectInput(session, "x", choices = names(queried_data()))
  }) # X variable for graphs
  
  output$y_ui <- renderUI({
    req(input$plot_type == "Scatterplot")
    # For scatterplot only pick the y value
    if (input$plot_type == "Scatterplot") {
      selectInput("y", "Select Y Variable:", choices = names(queried_data()), selected = NULL)
    } 
  })
  
  #optional faceting 
  output$faceting_ui <- renderUI({
    req(queried_data())
    selectInput("faceting_var", "Facet by (optional):", choices = c(None = "", names(queried_data())))
  })

  observeEvent(input$x, {
    req(queried_data())
    x_data <- queried_data()[[input$x]]
  
    if (is.numeric(x_data)) {
      updateSelectInput(session, "summary_type", choices = c("Mean", "Count"))
    } else {
      updateSelectInput(session, "summary_type", choices = c("Count"))
    }
  }) # cant take mean of a categorical var, so this changes based on that 
  
output$summary_out <- renderPrint({
  req(queried_data(), input$x, input$summary_type)
  x <- queried_data()[[input$x]]
  
  if (is.numeric(x)) {
    if (input$summary_type == "Mean") {
      mean(x, na.rm = TRUE) #removes NAs
    } else if (input$summary_type == "Count") {
      length(na.omit(x)) #omits NA values from the length returned
    }
  } else {
    table(x, useNA = "ifany") #will include NA values in non numerics
  }
})

output$summary_plot <- renderPlot({
  data <- queried_data()
  req(data, input$x, input$plot_type)
  
  facet <- input$faceting_var #easier name 
  facet_formula <- if(facet != ""){ as.formula(paste("~", facet)) } else { NULL }
  #checks if a facet was chosen, and puts together the formula for the graph if one was
  plot <- ggplot(data, aes_string(x = input$x))
  
  if (input$plot_type == "Histogram" && is.numeric(data[[input$x]])) {
    plot <- plot + geom_histogram(binwidth = 10, fill = "purple", color = "white")
  } else if (input$plot_type == "Boxplot" && is.numeric(data[[input$x]])) {
    plot <- plot + geom_boxplot(fill = "pink")
  } else if (input$plot_type == "Barplot" && !is.numeric(data[[input$x]])) {
    plot <- plot + geom_bar(fill = "red")
  } else if (input$plot_type == "Scatterplot" && !is.null(input$y)) {
    plot <- ggplot(data, aes_string(x = input$x, y = input$y)) +
      geom_point()
  }
  
  if (!is.null(facet_formula)) {
    plot <- plot + facet_wrap(facet_formula)
  }
  
  plot + theme_minimal() #minimalistic theme seemed best here
})
}
# Run the app ----
shinyApp(ui = ui, server = server)