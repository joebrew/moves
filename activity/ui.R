
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Xu's Measurement Class Project"),

  sidebarLayout(
    sidebarPanel(

      checkboxGroupInput('person', 'People included in group analysis:',
                         c('Jake', 'Joe', 'Sheldon', 'Xu', 'Yun'),
                         selected = c('Jake', 'Joe', 'Sheldon', 'Xu', 'Yun')),
      
      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
      
      selectInput('person2', 'Person for individual analysis:',
                  c('Jake', 'Joe', 'Sheldon', 'Xu', 'Yun'),
                  selected = 'Jake')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      h2('Group analysis'),
      plotOutput("plot1"),
      plotOutput("plot0"),
      h2(textOutput('who')),
      plotOutput("plot_ts"),
      plotOutput("plot3"),
      plotOutput('plot2')
    )
  )
))
