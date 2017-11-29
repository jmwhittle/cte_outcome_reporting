library(shiny)
library(tidyverse); theme_set(theme_minimal())

data <- read_csv('survey_pidms.csv') %>% filter(DEGREE_CODE != "APE")

library(lubridate)
data$GRADUATION_DATE <- dmy(data$GRADUATION_DATE)
data <- data %>% filter(GRADUATION_DATE > "2012-01-01" & GRADUATION_DATE < "2016-12-30")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("SLCC CTE program and workforce outcomes"),
  
  sidebarLayout(
    sidebarPanel(
  selectInput(inputId = "college_ind", label = "College", multiple = FALSE, choices = data$COLLEGE),
  uiOutput("program"),
  uiOutput("award")
  ),
  
  mainPanel(
    plotOutput(outputId = "plot"),
    plotOutput(outputId = "plot_ts"),
    tableOutput(outputId = "table")
            )
)) 

# Define server logic required to draw a histogram
server <- function(input, output) {
  df0 <- eventReactive(input$college_ind, {
    data %>% dplyr::filter(COLLEGE %in% input$college_ind)
  })

  output$program <- renderUI({
    selectInput(inputId = "program", label = "Program", multiple = TRUE, choices = df0()$MAJOR_CODE_1)
  })
  
  df1 <- eventReactive(input$program, {
    df0() %>% dplyr::filter(MAJOR_CODE_1 %in% input$program)
  })
  
  output$award <- renderUI({
    selectInput(inputId = "award", label = "Award level", choices = df1()$DEGREE_CODE)
  })

  df2 <- eventReactive(input$award, {
    df1() %>% dplyr::filter(DEGREE_CODE %in% input$award)
  })
  
  output$plot <- renderPlot(df2() %>% 
      ggplot() + 
      geom_density(aes(CUM_INSTITUTION_GPA, color = MAJOR_CODE_1)) + 
      geom_vline(xintercept = median(as.numeric(df2()$CUM_INSTITUTION_GPA))) +
      geom_vline(xintercept = quantile(as.numeric(df2()$CUM_INSTITUTION_GPA), 0.25), linetype=3) + 
      geom_vline(xintercept = quantile(as.numeric(df2()$CUM_INSTITUTION_GPA), 0.75), linetype=3) + 
      ggtitle("Cummulative institutional GPA") + 
      xlab("Cummulative GPA") + 
      ylab("Density") + 
      scale_color_discrete(name="Program")
  )
  
  output$plot_ts <- renderPlot(df2() %>% 
                                 group_by(TERM_CODE, MAJOR_CODE_1) %>% 
                                 tally() %>%
                                 ggplot() + 
                                 geom_line(aes(x=TERM_CODE, y=n, color = MAJOR_CODE_1)) +
                                 ggtitle("Number of Awards by program") +
                                 xlab("Year (calendar)") + 
                                 ylab("Number of Awards") + 
                                 scale_color_discrete(name="Program")
                               )
  

  output$table <- renderTable({df2() %>% group_by(MAJOR_CODE_1) %>% 
    summarise('Median GPA' = median(CUM_INSTITUTION_GPA),
              '25%' = quantile(as.numeric(CUM_INSTITUTION_GPA), 0.25),
              '75%' = quantile(as.numeric(CUM_INSTITUTION_GPA), 0.75),
              students = n_distinct(PIDM),
              awards = n())
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

