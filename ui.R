library(shinydashboard)
library(DT)
library(shinythemes)




ui <- fluidPage(
  navbarPage(theme=shinytheme("flatly"),collapsible = TRUE,
             "Visual CanReg", id="nav",
           tabPanel("Reg mapper"),
           tabPanel("Reg calculator",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          sidebarLayout(
                            sidebarPanel(width=3,
                                         span(tags$i(h6("Upload your own data, and calculate the cancer incidence and mortality and their 95% confidence interval.")), style="color:#045a8d"),
                                         tags$hr(),
                                         fileInput('file1', 'Choose xlsx file',accept = c(".xlsx")),
                                         tags$hr(),
                                         radioButtons("sexx","Gender:", c("Both" = 3, "Male" = 1,"Female"=2), inline=T),
                                         radioButtons("type","", c("Incidence" = 1, "Mortality" = 2), inline=T),
                                         
                            ),
                            mainPanel(
                              tags$hr(),
                              dataTableOutput('table'),
                              downloadButton("downloadData", "Download as CSV"),
                              tags$hr()
                              
                            )
                          )
                          

                      )
               
             ),
           tabPanel("Visualization",
                    h2(paste0("The date distribution of Cancer cases when diagnosed and dead.")),
                    fluidRow( column( width = 6,h4("Incidence", align = 'center'), plotOutput('plot2') ),
                              column( width = 6,h4("Mortality", align = 'center'), plotOutput('plot3') )
                    )),
           tabPanel("About this site",includeMarkdown("README.md"))
  )
)


