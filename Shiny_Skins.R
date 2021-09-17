library(shiny)
library(tidyverse)
library(scales)
library(expss)
library(plotly)
library(shinydashboard)


ui <- dashboardPage(
    dashboardHeader(title = "OTB Skins Matches Breakdown"),
    dashboardSidebar(
        sidebarMenu(
            menuItem('About', tabName = 'About', icon  = icon('dashboard')),
            menuItem('Players', tabName = 'Players', icon =icon('th')),
            menuItem('Tournaments', tabName = 'Tournaments', icon =icon('trophy'))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'About'
            ),
            #Players Tab
            tabItem(tabName = 'Players',
                    fluidRow(
                        box(title = 'Player Selection',
                            selectInput("PlayerName",
                                        "Player Name",
                                        sort(unique(Player_Stats$Winner)),
                                        selected = "Texas"),
                        ),
                        valueBox(textOutput("Appearances"),'Appearances',icon = icon('trophy')),
                        valueBox(textOutput("Money"),'Total Earnings',icon = icon('money-bill-wave'))
                    ),
                    fluidRow(
                        box(plotOutput('PlayerPlot')
                        )
                    )
                
            ),
            tabItem(tabName = 'Tournaments',
                    
                
            )
        )
    )
)



server <- function(input, output) {
    
##### Players Tab ############
    output$PlayerPlot <- renderPlot({
        Player_Stats %>%
            filter(Winner == input$PlayerName) %>%
            ggplot(aes(x = Course, y = Total_Winnings)) +
            geom_col()+
            coord_flip()
    })
    #Appearances Value Box
    output$Appearances <- renderText({ 
        as.character(table(Match_Info$Player)[input$PlayerName])
    })
    #Total Earnings Value Box
    output$Money <- renderText({ 
        as.character(paste("$",filter(Earnings , Winner == input$PlayerName)['Money']))
    })

##### Tournament Tab ########## 
}

# Run the application 
shinyApp(ui = ui, server = server)
