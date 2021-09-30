library(shiny)
library(tidyverse)
library(scales)
library(expss)
library(plotly)
library(shinydashboard)
library(dashboardthemes)
library(ggthemes)

ui <- dashboardPage(
    dashboardHeader(
        title = 'OTB Skins Breakdown'
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem('About', tabName = 'About', icon  = icon('dashboard')),
            menuItem('Players', tabName = 'Players', icon =icon('th')),
            menuItem('Tournaments', tabName = 'Tournaments', icon =icon('trophy'))
    )),
    
    dashboardBody(
        # Adding Shiny Dashboard Theme
        shinyDashboardThemes(
            theme = 'blue_gradient'
        ),
        tabItems(
#================ About tab ===============================================  

            tabItem(tabName = 'About',
                    fluidRow(
                       box( 
                        width = 8,
                        status = 'primary',
                        p("\t This is a simple dashboard that uses data from the OTB Skins Matches and breaks it down by player and
                          tournaments. As the OTB Skins matches are on going, I will continue to update the App when new matches come out. Currently there are only 
                          the first 16 Skins matches.")
                             )
                       ),
                    fluidRow(
                        box(
                            title = 'Code',
                            solidHeader = TRUE,
                            width = 8,
                            status = 'primary',
                            p('The code used is available at',
                                a("Github",
                                  href = 'https://github.com/LucioBrandon/OTB_Skins_Matches/blob/main/Shiny_Skins.R'
                                ))
                        )
                    )
                    ),
#============== Players Tab ================================================

            tabItem(tabName = 'Players',
                    fluidRow(
                        box(
                            title = "Player Selection", status = 'primary',
                            solidHeader = TRUE,
                            selectInput("PlayerName",
                                    "",
                                    sort(unique(Player_Stats$Winner))
                                    ),
                            checkboxInput("Type",
                                          "Breakdown Winnings by Type",
                                          value = FALSE), 
                            collapsible = TRUE)                     
                    ),
                    fluidRow(
                        valueBox(textOutput("Appearances"),
                                 'Appearances',
                                 icon = icon('trophy')),
                        
                        valueBox(textOutput("Money"),
                                 'Total Earnings',
                                 icon = icon('money-bill-wave')),
                        
                        valueBox(textOutput('Skunk'),
                                 'Skunks')
                    ),
                    fluidRow(
                        box(
                            title = '',
                            width = 8,
                            plotlyOutput('PlayerPlot'),
                            collapsible = TRUE
                        )
                    ),
                    fluidRow(
                        box(
                            title ='',
                            width = 10,
                            tableOutput('PlayerTable'),
                            collapsible = TRUE
                                )
                    )
                
            ),
#================== Tournaments Tab ===========================================            
            tabItem(tabName = 'Tournaments',
                    fluidRow(
                        box(
                            title = "Skin Match Selection", status = 'primary',
                            solidHeader = TRUE,
                            selectInput("Course Name",
                                        "",
                                        sort(unique(Match_Info$Tournament))
                            ),
                            collapsible = TRUE)                     
                    ) 
                
            )
        )
    )
)



server <- function(input, output) {
#======================== About Tab =========================================    
    
    output$About <- renderText({
        "\tHello"
    })
    
#======================== Players Tab =========================================
    output$PlayerPlot <- renderPlotly({
        
    if(input$Type){
        A2 <- Player_Stats_Type %>%
            filter(Winner == input$PlayerName) %>%
            ggplot(aes(y = Course, x = Total_Winnings, fill = `Hole Type`)) +
            geom_col() +
            scale_x_continuous(labels = scales::dollar )+
            labs(title = ,
                x ='Winnings') +
            theme_clean() +
            scale_fill_brewer(palette="Paired")
        
        ggplotly(A2) %>%
            layout(yaxis = list(fixedrange = TRUE),
                   xaxis = list(fixedrange = TRUE)) %>%
            config(displayModeBar = FALSE)
    }
    else{
        A1 <- Player_Stats %>%
            filter(Winner == input$PlayerName) %>%
            ggplot(aes(y = Course, x = Total_Winnings)) +
            geom_col() +
            scale_x_continuous(labels = scales::dollar )+
            labs(x ='Winnings') +
            theme_clean() +
            scale_fill_brewer(palette="Paired")      
        
        ggplotly(A1)%>%
            layout(yaxis = list(fixedrange = TRUE),
                   xaxis = list(fixedrange = TRUE)) %>%
            config(displayModeBar = FALSE)
    }
    })
    #Appearances Value Box
    output$Appearances <- renderText({ 
        as.character(table(Match_Info$Player)[input$PlayerName])
    })
    #Total Earnings Value Box
    output$Money <- renderText({ 
        as.character(paste("$",filter(Earnings , Winner == input$PlayerName)['Money']))
    })
    
    output$PlayerTable <- renderTable({
        Player_Stats_Type %>%
            filter(Winner == input$PlayerName)
    })

#================= Tournament Tab =======================================
}

# Run the application 
shinyApp(ui = ui, server = server)
