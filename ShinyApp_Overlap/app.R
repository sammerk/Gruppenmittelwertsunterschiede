library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(hrbrthemes)
library(Hmisc)
library(thematic)
library(bslib)
library(shinyWidgets)
library(bayestestR)
library(viridis)


ui <- fluidPage(
  theme = bs_theme(
    bg = "#202123",
    fg = "#b8bcc2",
    primary = "#62DC3A",
    base_font = font_google("Roboto Condensed")
  ),
  titlePanel("Überlappung vs. Mittelwertsdifferenz"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      sliderInput("sample_n", "Größe der Stichprobe", 1, 800, 164),
      sliderTextInput(
        inputId = "form",
        label = "Verteilungsform", 
        choices = c("1 = umgekehrt u-förmig", 
                    2:9, 
                    "10 = u-förmig"),
        selected = "4"
      ),
      sliderTextInput(
        inputId = "overlap",
        label = "Überlappung", 
        choices = c("1 = (fast) keine", 
                    2:9, 
                    "10 = (fast) völlständige"),
        selected = "6"
        
      ),
      numericInput("mean1", "Mittelwert Gruppe 1", 505),
      numericInput("mean2", "Mittelwert Gruppe 2", 512), 
      selectInput(
        "plottype",
        "Art der Visualisierung",
        c("Histogramm" = "Histogramm",
          "Dotplot" = "Dotplot",
          "Densityplot" = "Densityplot",
          "Violinplot" = "Violinplot",
          "Sinaplot" = "Sinaplot",
          "Jitterplot" = "Jitterplot",
          "Boxplot" = "Boxplot",
          "Errorbarplot" = "Errorbarplot"))
      ),
 
  mainPanel(
      uiOutput("plotUI")
  )
  )
)


server <- function(input, output, session) {
  
  shape1_shape2 <- reactive({
    case_when(input$form == "1 = umgekehrt u-förmig" ~ 20,
              input$form == "2" ~ 16,
              input$form == "3" ~ 12,
              input$form == "4" ~ 8,
              input$form == "5" ~ 4,
              input$form == "6" ~ 2,
              input$form == "7" ~ 1,
              input$form == "8" ~ .7,
              input$form == "9" ~ .4,
              input$form == "10 = u-förmig" ~ .2)
  })
  
  range_diff <- reactive({
    case_when(input$overlap == "1 = (fast) keine" ~ 2,
              input$overlap == "2" ~ 1.5,
              input$overlap == "3" ~ 1,
              input$overlap == "4" ~ .8,
              input$overlap == "5" ~ .6,
              input$overlap == "6" ~ .5,
              input$overlap == "7" ~ .4,
              input$overlap == "8" ~ .3,
              input$overlap == "9" ~ .2,
              input$overlap == "10 = (fast) völlständige" ~ .1)
  })

  mean_diff <- reactive({
    abs(input$mean1 - input$mean2)
  })
  
  # adapt data to input
  data1 <- reactive({
    tibble(G1 = distribution_beta(input$sample_n,
                                   shape1_shape2(), 
                                   shape1_shape2()),
           G2 = distribution_beta(input$sample_n,
                                   shape1_shape2(), 
                                   shape1_shape2()))
  })
  
  range_emp <- reactive({max(data1()$G1) - min(data1()$G1)})
  
  data <- reactive({
    tibble(
      G1 = (data1()$G1 - .5) / (max(data1()$G1) - .5) *  # centering and range =1
            mean_diff() * 1/range_diff() + 0.5 + input$mean1,
      G2 = (data1()$G2 - .5) / (max(data1()$G1) - .5) *  # centering and range =1
            mean_diff() * 1/range_diff() + 0.5 + input$mean2
    ) %>% 
      gather(Gruppe, Ausprägung)
  })
  
  
  output$plot <- renderPlot({
    
    
    if (input$plottype == "Dotplot")
      plot <-
        ggplot(data(), aes(x = Ausprägung, 
                           fill = Gruppe)) +
        labs(title = "Dotplot") +
        geom_dotplot(
          method = "histodot",
          color = "#ffffff00") +
        theme_modern_rc() +
        scale_fill_manual(values = c("#8cd00090", "#d77d0090")) 
    
    if (input$plottype == "Histogramm")
      plot <-
        ggplot(data(), aes(x = Ausprägung, 
                           color = Gruppe, 
                           fill = Gruppe)) + 
        geom_histogram(color = "#8cd00000",
                       position="identity") +
        theme_ipsum_rc() +
        labs(title = "Histogramm") +
        theme_modern_rc() +
        scale_fill_manual(values = c("#8cd00090", "#d77d0090"))
    
    if (input$plottype == "Boxplot")
      plot <-
        ggplot(data(), 
               aes(x = Ausprägung, y = Gruppe)) + 
        geom_boxplot(color = "#8cd000",
                     fill = "#8cd00020") + 
        theme_modern_rc() +
        labs(title = "Boxplot")
  
    if (input$plottype == "Jitterplot")
      plot <-
        ggplot(data(), 
               aes(x = Ausprägung, y = Gruppe)) + 
        geom_jitter(color = "#8cd000") + 
        theme_modern_rc() +
        labs(title = "Jitterplot") 
    
    if (input$plottype == "Densityplot")
      plot <-
        ggplot(data(), aes(x = Ausprägung, 
                           color = Gruppe, 
                           fill = Gruppe)) + 
        labs(title = "Densityplot") +
        theme_modern_rc() +
        scale_fill_manual(values = c("#8cd00050", "#d77d0050")) +
        scale_color_manual(values = c("#8cd000", "#d77d00")) +
        geom_density() 
      
    
    if (input$plottype == "Violinplot")
      plot <-
        ggplot(data(), 
               aes(x = Ausprägung, y = Gruppe)) + 
        geom_violin(color = "#8cd000",
                     fill = "#8cd00020") + 
        theme_modern_rc() +
        labs(title = "Violinplot")
    
    if (input$plottype == "Sinaplot")
      plot <-
        ggplot(data(), 
               aes(x = Ausprägung, y = Gruppe)) + 
        ggforce::geom_sina(color = "#8cd000",
                    fill = "#8cd00020") + 
        theme_modern_rc() +
        labs(title = "Violinplot")
    
    if (input$plottype == "Errorbarplot")
      plot <-
        ggplot(data(), aes(x = Ausprägung, y = Gruppe)) + 
        stat_summary(fun.data = mean_sdl, 
                     geom = "linerange", 
                     fun.args = list(mult = 1), 
                     width = 1.3,
                     color = "#8cd000") + 
        stat_summary(fun.y = mean, 
                     geom = "point",
                     color = "#8cd000",
                     size = 3) + 
        theme_modern_rc() +
        labs(title = "Errorbarplot")
    
    return(plot)
  })
  
  output$plotUI <- renderUI({
    plotOutput("plot", 
               height = case_when(input$plottype == "Dotplot" ~ "400px",
                                  input$plottype == "Histogramm" ~ "400px",
                                  input$plottype == "Boxplot" ~ "200px",
                                  input$plottype == "Jitterplot" ~ "300px",
                                  input$plottype == "Densityplot" ~ "400px",
                                  input$plottype == "Violinplot" ~ "330px",
                                  input$plottype == "Sinaplot" ~ "330px",
                                  input$plottype == "Errorbarplot" ~ "200px")
    )
  })
}

shinyApp(ui = ui, server = server)