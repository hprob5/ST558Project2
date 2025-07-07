library(shiny)
library(shinydashboard)

# Define UI ----

ui <- dashboardPage(
  dashboardHeader(title = "Pokemon TCG Dash"),
  
  dashboardSidebar(sidebarMenu(
    #Adding in sidebar menu items for the different tabs
    menuItem("About", tabName = "About", icon = icon("dashboard")),
    menuItem("Data Download", tabName = "Data Download", icon = icon("th")))),
  
  dashboardBody(
    fluidRow(
      box(title = "About the App", 
          p("I created this app for a project for ST558 at NC State. 
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
  )
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
