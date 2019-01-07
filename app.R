

library(shiny)
source("plot_models.R")


# Define UI wide ----
ui <- fluidPage(
  
  titlePanel("Dual Task Process Models"),
  
  sidebarLayout(
    
    # Configure the side panel (input)
    sidebarPanel(width = 2,
      
      h4("Configurations"),
      br(),
      
      sliderInput(inputId = "r.ratio", 
                  label = "Resource sharing in central stage", 
                  min = 0, 
                  max = 1, 
                  value = .6, 
                  step = .01),
      
      sliderInput(inputId = "c.ratio", 
                  label = "Proportion of response activation in central stage", 
                  min = 0, 
                  max = 1, 
                  value = .75, 
                  step = .01),
      
      sliderInput(inputId = "soa", 
                  label = "Stimulus Onset Asynchrony (SOA)", 
                  min = 0, 
                  max = 500, 
                  value = 10, 
                  step = 5),
      
      br(),
      
      helpText("The following settings usually don't have to be adjusted. They indicate the length of each stage."),
      
      numericInput(inputId = "p", 
                   label = "Perception stage", 
                   value = 200),
      
      numericInput(inputId = "rs", 
                   label = "Central stage", 
                   value = 200),
      
      numericInput(inputId = "mr", 
                   label = "Motor response stage", 
                   value = 200)
      
    ), # end of sidebarPanel
    
    
    
    
    # Configure the main panel (output)
    mainPanel(width = 10,
      
      fluidRow(
        
        column(width = 6,
               h4("Sequential Model"), 
               plotOutput("sequential", 
                          height = "250px"),
               br(),
               h4("Crosstalk Model"), 
               plotOutput("crosstalk", 
                          height = "250px")
               ),
        
        column(width = 6,
               h4("Parallel Model"), 
               plotOutput("parallel", 
                          height = "250px"),
               br(),
               h4("Integrated/Hybrid Model"), 
               plotOutput("integrated", 
                          height = "250px")
               )
        
        ), # end of fluidRow
      
      br(),
      
      helpText("Note: In the figures, white boxes represent processes that can overlap without resource sharing. 
               Light gray boxes represent processes that can overlap but need to share resources. The resource allocation
               can be adjusted in the configurations. Dark gray boxes represent processes that can't overlap with any other
               process. During this type of processes, the system is blocked.")
      
      
      ) # end of mainPanel
    
    
  ) # end of sidebarLayout
  
  
  
)





# Define server logic ----
server <- function(input, output) {
  
  output$sequential <- renderPlot({
    
    plot(plot.models(soa = input$soa, rsp = input$r.ratio, rarsp = input$c.ratio,
                     p = input$p, rs = input$rs, mr = input$mr, models = "sequential")$sequential)
    
  })
  
  
  output$parallel <- renderPlot({
    
    plot(plot.models(soa = input$soa, rsp = input$r.ratio, rarsp = input$c.ratio,
                     p = input$p, rs = input$rs, mr = input$mr, models = "parallel")$parallel)
    
  })
  
  
  output$crosstalk <- renderPlot({
    
    plot(plot.models(soa = input$soa, rsp = input$r.ratio, rarsp = input$c.ratio,
                     p = input$p, rs = input$rs, mr = input$mr, models = "crosstalk")$crosstalk)
    
  })
  
  
  output$integrated <- renderPlot({
    
    plot(plot.models(soa = input$soa, rsp = input$r.ratio, rarsp = input$c.ratio,
                     p = input$p, rs = input$rs, mr = input$mr, models = "integrated")$integrated)
    
  })
  
  
}


# Run the app
shinyApp(ui = ui, server = server)

# Remove the custom function after the app is closed
#rm(plot.models)
