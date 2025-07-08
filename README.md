# ST558Project2
ST 558 Project 2

- Brief description of the app and its purpose:
From About Tab: I created this app for a project for ST558 (Data Science for Statisticians) at NC State. It is an app that pulls from a database about Pokemon TCG Cards and can return loads of information about different things such as what sets there are, types of Pokemon, card prices, card rarity, and so much more. Additionally, you can turn this information into graphs to visually see the breakdowns. I grew up playing Pokemon on my Nintendo DS with my brother. He collected cards long before I did, but now I'm the one playing once a week at a local Pokemon League while he's chosen to stick to the video game! I've loved this topic for quite some time and this app is on a topic I find very interesting and fun. I hope you are able to use it to be curious about the Pokemon TCG like I am.

- A list of packages needed to run the app.
	- (shiny)
	- (shinydashboard)
	- (DT)
	- (dplyr)
	
- A line of code that would install all the packages used
lapply(c("shiny", "shinydashboard", "DT", "dplyr"), library)

- The shiny::runGitHub() code that we can copy and paste into RStudio to run your app.
shiny::runGitHub(repo = "ST555Proj2", username = "hprob5")
