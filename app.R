library(shiny)
library(ggplot2)
library(bslib)
library(magrittr)

line_colors <- c("Linje 1" = "#F8766D", "Linje 2" = "#00BFC4")

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "litera",
    "font-size-base" = "0.8rem",
    "spacer" = "0.5rem",
    "input-btn-padding-y" = "0.1rem",
    "input-padding-y" = "0.1rem"
  ),
  
  tags$head(
    tags$style(HTML("
      .line1-header { color: #F8766D; font-weight: 600; margin: 0; }
      .line2-header { color: #00BFC4; font-weight: 600; margin: 0; }
      .form-group { margin-bottom: 0 !important; }
      hr { margin: 0.2rem 0; }
      .card-body { padding: 0.4rem; }
      .card-header { padding: 0.3rem; }
      .form-control { padding: 0.1rem 0.3rem; height: 1.5rem; }
      .shiny-input-container { margin-bottom: 0 !important; }
      h5 { font-size: 0.85rem; margin: 0; padding: 0; }
      label { margin-bottom: 0 !important; }
      .btn { padding: 0.15rem 0.5rem; }
    "))
  ),
  
  layout_columns(
    col_widths = c(4, 8),
    gap = "0.3rem",
    
    card(
      card_header(
        actionButton("redraw", "Dan nye linjer", class = "btn-primary btn-sm")
      ),
      card_body(
        h5("Linje 1", class = "line1-header"),
        numericInput("m1", "Hældning:", value = 0),
        numericInput("b1", "Y-skæring:", value = 0),
        hr(),
        h5("Linje 2", class = "line2-header"),
        numericInput("m2", "Hældning:", value = 0),
        numericInput("b2", "Y-skæring:", value = 0),
        hr(),
        h5("Skæringspunkt", style = "font-weight: 600;"),
        numericInput("x_int", "X-koordinat:", value = 0),
        numericInput("y_int", "Y-koordinat:", value = 0),
        hr(),
        actionButton("check", "Kontroller svar", class = "btn-success btn-sm w-100"),
        textOutput("feedback") %>% 
          tagAppendAttributes(style = "margin-top: 0.2rem; font-weight: 500; font-size: 0.8rem;")
      )
    ),
    
    card(
      card_body(
        plotOutput("linePlot", height = "400px")
      )
    )
  )
)

server <- function(input, output, session) {
  
  get_random_lines <- function() {
    repeat {
      x_int <- sample(-5:5, 1)
      y_int <- sample(-5:5, 1)
      slopes <- sample(c(-3:-1, 1:3), 2)
      m1 <- slopes[1]
      m2 <- slopes[2]
      b1 <- y_int - m1 * x_int
      b2 <- y_int - m2 * x_int
      if (abs(b1) <= 8 && abs(b2) <= 8) break
    }
    return(list(x_int = x_int, y_int = y_int,
                m1 = m1, m2 = m2,
                b1 = b1, b2 = b2))
  }
  
  lines <- reactiveVal(get_random_lines())
  feedback_text <- reactiveVal("")
  
  observeEvent(input$redraw, {
    lines(get_random_lines())
    feedback_text("")
    updateNumericInput(session, "m1", value = 0)
    updateNumericInput(session, "b1", value = 0)
    updateNumericInput(session, "m2", value = 0)
    updateNumericInput(session, "b2", value = 0)
    updateNumericInput(session, "x_int", value = 0)
    updateNumericInput(session, "y_int", value = 0)
  })
  
  output$linePlot <- renderPlot({
    current <- lines()
    
    x <- seq(-8, 8, length.out = 100)
    df1 <- data.frame(x = x, y = current$m1 * x + current$b1, line = "Linje 1")
    df2 <- data.frame(x = x, y = current$m2 * x + current$b2, line = "Linje 2")
    plot_data <- rbind(df1, df2)
    
    ggplot(plot_data, aes(x = x, y = y, color = line)) +
      geom_line(size = 1) +
      scale_x_continuous(breaks = -8:8, limits = c(-8, 8)) +
      scale_y_continuous(breaks = -8:8, limits = c(-8, 8)) +
      scale_color_manual(values = line_colors) +
      geom_hline(yintercept = 0, color = "black", size = 0.5) +
      geom_vline(xintercept = 0, color = "black", size = 0.5) +
      geom_point(aes(x = current$x_int, y = current$y_int), 
                 color = "red", size = 3, inherit.aes = FALSE) +
      labs(x = "X-akse", y = "Y-akse", color = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "top",
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
      )
  })
  
  observeEvent(input$check, {
    current <- isolate(lines())
    
    slope1_correct <- abs(input$m1 - current$m1) < 0.1
    slope2_correct <- abs(input$m2 - current$m2) < 0.1
    b1_correct <- abs(input$b1 - current$b1) < 0.1
    b2_correct <- abs(input$b2 - current$b2) < 0.1
    x_int_correct <- abs(input$x_int - current$x_int) < 0.1
    y_int_correct <- abs(input$y_int - current$y_int) < 0.1
    
    feedback <- character(0)
    
    if (!slope1_correct) feedback <- c(feedback, sprintf("Linje 1 hældning skal være %d", current$m1))
    if (!b1_correct) feedback <- c(feedback, sprintf("Linje 1 y-skæring skal være %d", current$b1))
    if (!slope2_correct) feedback <- c(feedback, sprintf("Linje 2 hældning skal være %d", current$m2))
    if (!b2_correct) feedback <- c(feedback, sprintf("Linje 2 y-skæring skal være %d", current$b2))
    if (!x_int_correct || !y_int_correct) {
      feedback <- c(feedback, sprintf("Skæringspunktet skal være (%d, %d)", current$x_int, current$y_int))
    }
    
    if (length(feedback) == 0) {
      feedback_text("Korrekt! Alle svar er rigtige!")
    } else {
      feedback_text(paste("Forkert:", paste(feedback, collapse = "; ")))
    }
  })
  
  output$feedback <- renderText({
    feedback_text()
  })
}

shinyApp(ui, server)