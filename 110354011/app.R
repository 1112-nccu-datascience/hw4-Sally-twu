library(shiny)
library(ggplot2)
library(ca)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("凃于珊-統計碩二110354011"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      h3("choose how many input to do PCA:"),
      sliderInput(inputId = "number",
                  label = "Number of points",
                  min = 6,
                  max = 150,
                  value = 100),
      h3("choose what you want to see on PCA Result : Plot"),
      uiOutput("the_pcs_to_plot_x"),
      uiOutput("the_pcs_to_plot_y")
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("PCA Result : Plot", plotOutput(outputId = "pca")),
        tabPanel("Pareto chart",plotOutput(outputId = "pareto")),
        tabPanel("CA", plotOutput(outputId = "ca"))
      )
    )
  )
)

server <- function(input, output) {
  pca_objects <- reactive({
    data(iris)
    iris <- iris[1:input$number,]
    # log transform 
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
    pca_output <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    pcs_df <- cbind(iris,pca_output$x)
    return(list(pca_output = pca_output,
      pcs_df = pcs_df,
      Species = ir.species
    ))
  })
  output$the_pcs_to_plot_x <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    # drop down selection
    selectInput(inputId = "the_pcs_to_plot_x", 
                label = "X axis:",
                choices= colnames(pca_output), 
                selected = 'PC1')
  })
  output$the_pcs_to_plot_y <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    
    # drop down selection
    selectInput(inputId = "the_pcs_to_plot_y", 
                label = "Y axis:",
                choices= colnames(pca_output), 
                selected = 'PC2')
  })
  output$pareto <- renderPlot({
    pca_output <- pca_objects()$pca_output
    eig = (pca_output$sdev)^2
    variance <- eig*100/sum(eig)
    cumvar <- paste(round(cumsum(variance),1), "%")
    eig_df <- data.frame(eig = eig,
                         PCs = colnames(pca_output$x),
                         cumvar =  cumvar)
    ggplot(eig_df, aes(reorder(PCs, -eig), eig)) +
      geom_bar(stat = "identity", fill = "white", colour = "black") +
      geom_text(label = cumvar, size = 4,
                vjust=-0.4) +
      theme_bw(base_size = 14) +
      xlab("PC") +
      ylab("Variances") +
      ylim(0,(max(eig_df$eig) * 1.1))
  })
  output$pca <- renderPlot({
    pcs_df <- pca_objects()$pcs_df
    pca_output <- pca_objects()$pca_output
    var_expl_x <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_x))]^2/sum(pca_output$sdev^2), 1)
    var_expl_y <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_y))]^2/sum(pca_output$sdev^2), 1)
    labels <- rownames(pca_output$x)
    # plot without grouping variable
    pc_plot <- ggplot(pcs_df, 
                      aes_string(input$the_pcs_to_plot_x, 
                                  input$the_pcs_to_plot_y)) +
      geom_text(aes(label = labels),  size = 5) +
      theme_bw(base_size = 14) +
      coord_equal() +
      xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
      ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
    # the plot
    pc_plot
  })
  output$ca <- renderPlot({
    fit <- ca(iris[1:input$number, 1:4])
    plot(fit)
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

