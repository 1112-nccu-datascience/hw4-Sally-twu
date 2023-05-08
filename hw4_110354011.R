library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("HW4-PRINCIPLE COMPONENT ANALYSIS"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      h3("choose how many input to do PCA:"),
      sliderInput(inputId = "subdata",
                  label = "Number of points",
                  min = 6,
                  max = 150,
                  value = 100),
      h3("choose what you wnt to see on 'PCA Result : Plot"),
      selectInput(inputId = "X",
                  label = "X Variable",
                  choices = list("PC1" = 1, "PC2" = 2,
                                 "PC3" = 3, "PC4" = 4), selected = 1),
      selectInput(inputId = "Y",
                  label = "Y Variable",
                  choices = list("PC1" = 1, "PC2" = 2,
                                 "PC3" = 3, "PC4" = 4), selected = 2)
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "pca")
      
    )
  )
)

server <- function(input, output) {
  output$pca <- renderPlot({
    
    data(iris)
    # log transform 
    log.ir <- log(iris[1:input$subdata, 1:4])
    ir.species <- iris[1:input$subdata, 5]
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    library(ggbiplot)
    g <- ggbiplot(ir.pca,choices = input$X:input$Y, obs.scale = 1, var.scale = 1, groups = ir.species, circle = TRUE)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    plot(g)
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)