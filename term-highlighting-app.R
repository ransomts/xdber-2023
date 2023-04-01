library(shiny)
library(tidyverse)
library(DT)
library(magrittr)
library(styler)
library(lintr)
library(ggwordcloud)

data_fn <- "/home/tsranso/Code/reddit-analysis/data/lda-Python-2017-6.rds"
sample_posts_filename <- "/home/tsranso/Code/reddit-analysis/data/sample_python_posts.rds"
wordcloud_title <- "r/python 2017 topic wordclouds"

make_wordcloud <- function(data_filename, title, number_cols = 2) {
  plot <- read_rds(data_filename) %>%
    group_by(topic) %>%
    top_n(30, beta) %>%
    ungroup() %>%
    ggplot(aes(label = term, size = beta)) +
    geom_text_wordcloud_area(rm_outside = TRUE) +
    scale_size_area(max_size = 35) +
    theme_minimal() +
    ggtitle(title) +
    facet_wrap(~topic, ncol = number_cols)

  read_rds(data_filename) %>%
    group_by(topic) %>%
    top_n(7, beta) %>%
    transmute(dt = paste0(term, collapse = " OR "), rh = paste0(term, collapse = ".|.")) %>%
    unique() %>%
    pivot_longer(cols = c(rh, dt)) %>%
    print()

  return(plot)
}

wordHighlight <- function(SuspWord, colH = "yellow") {
  paste0('<span style="background-color:', colH, '">', SuspWord, "</span>")
}

wordHighlight2 <- function(SuspWord, colH = "blue") {
  paste0('<span style="background-color:', colH, '">', SuspWord, "</span>")
}

wordHighlight3 <- function(SuspWord, colH = "green") {
  paste0('<span style="background-color:', colH, '">', SuspWord, "</span>")
}

wordHighlight4 <- function(SuspWord, colH = "red") {
  paste0('<span style="background-color:', colH, '">', SuspWord, "</span>")
}

wordHighlight5 <- function(SuspWord, colH = "orange") {
  paste0('<span style="background-color:', colH, '">', SuspWord, "</span>")
}

wordHighlight6 <- function(SuspWord, colH = "purple") {
  paste0('<span style="background-color:', colH, '">', SuspWord, "</span>")
}

wordHighlight7 <- function(SuspWord, colH = "violet") {
  paste0('<span style="background-color:', colH, '">', SuspWord, "</span>")
}

ui <- fluidPage(
  titlePanel("Text Highlighting"),
  sidebarLayout(
    sidebarPanel(
      textInput("wordSearch", "Yellow", value = "want |import |need |list |input |error |learn"),
      textInput("wordSearch2", "Blue", value = "python |like |help |find |trying |github |much"),
      textInput("wordSearch3", "Green", value = "file |print |line |also |script |class |used"),
      textInput("wordSearch4", "Red", value = "https |self |time |anyone |first |name |found"),
      textInput("wordSearch5", "Orange", value = "thanks |http |working |program |really |files |install"),
      textInput("wordSearch6", "Purple", value = "code |using |data |know |work |make |project"),
      textInput("wordSearch7", "Violet", )
    ),
    mainPanel(
      fillRow(
        DT::dataTableOutput("table"),
        plotOutput("plotBottomRight")
      )
    )
  )
)

server <- function(input, output) {
  YourData <- read_rds(sample_posts_filename) %>%
    transmute(str_to_lower(selftext)) %>%
    sample_n(250) %>%
    data.frame()

  highlightData <- reactive({
    YourData2 <- YourData

    if (input$wordSearch != "") {
      patterns <- input$wordSearch
      YourData2[, 1] %<>% str_replace_all(regex(patterns, ignore_case = TRUE), wordHighlight)
    }

    if (input$wordSearch2 != "") {
      patterns <- input$wordSearch2
      YourData2[, 1] %<>% str_replace_all(regex(patterns, ignore_case = TRUE), wordHighlight2)
    }

    if (input$wordSearch3 != "") {
      patterns <- input$wordSearch3
      YourData2[, 1] %<>% str_replace_all(regex(patterns, ignore_case = TRUE), wordHighlight3)
    }

    if (input$wordSearch4 != "") {
      patterns <- input$wordSearch4
      YourData2[, 1] %<>% str_replace_all(regex(patterns, ignore_case = TRUE), wordHighlight4)
    }

    if (input$wordSearch5 != "") {
      patterns <- input$wordSearch5
      YourData2[, 1] %<>% str_replace_all(regex(patterns, ignore_case = TRUE), wordHighlight5)
    }

    if (input$wordSearch6 != "") {
      patterns <- input$wordSearch6
      YourData2[, 1] %<>% str_replace_all(regex(patterns, ignore_case = TRUE), wordHighlight6)
    }

    if (input$wordSearch7 != "") {
      patterns <- input$wordSearch7
      YourData2[, 1] %<>% str_replace_all(regex(patterns, ignore_case = TRUE), wordHighlight7)
    }

    return(YourData2)
  })

  output$table <- DT::renderDataTable(
    {
      data <- highlightData()
    },
    options = list(pageLength = 5),
    escape = FALSE
  )

  output$plotBottomRight <- renderPlot(
    make_wordcloud(
      data_filename = data_fn,
      title = wordcloud_title,
      number_cols = 1
    )
  )
}

shinyApp(ui = ui, server = server)
