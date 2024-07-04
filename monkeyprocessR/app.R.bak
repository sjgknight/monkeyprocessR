#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(janitor)
library(HH)

source('../tidysheets.R')

# Define UI
ui <- fluidPage(
  titlePanel("Likert Data Processing"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File", accept = c(".xlsx")),
      uiOutput("table_selector"),
      actionButton("process", "Process Selected Tables"),
      hr(),
      downloadButton("download_combined", "Download Combined Data"),
      downloadButton("download_long", "Download Long Data"),
      hr(),
      textInput("main_title", "Main Title", value = "Your title here"),
      numericInput("cex", "Text Size (cex)", value = 0.4, min = 0.1, max = 2, step = 0.1),
      textInput("col", "Color of footnote", value = "purple"),
      actionButton("plot", "Generate Plot"),
      hr(),
      radioButtons("plot_format", "Select plot format", choices = c("png")),
      downloadButton("download_plot", "Download Plot")
    ),
    mainPanel(
      tableOutput("extracted_tables"),
      plotOutput("likert_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  table_raw <- reactive({
    req(input$file)
    read_excel(input$file$datapath,
               skip = 1,
               col_names = FALSE,
               col_types = "text")
  })

  #q_tables in my code
  extracted_tables <- reactive({
    split_table <- table_raw() %>%
      split_df(complexity = 2)

    extract_tables(split_table)
  })

  output$table_selector <- renderUI({
    req(extracted_tables())
    checkboxGroupInput("selected_tables",
                       "Select Tables to Process",
                       choices = names(extracted_tables()))
  })

  output$extracted_tables <- renderTable({
    req(extracted_tables())
    extracted_tables()
  })

  processed_data <- eventReactive(input$process, {
    req(input$selected_tables)
    q_tables <- extracted_tables()
    likert_wide <- map2(
      .x = q_tables[input$selected_tables],
      .y = names(q_tables[input$selected_tables]),
      .f = likert_that
    )
    likert_combined <- do.call(rbind, likert_wide)
    likert_long <- likert_combined %>%
      dplyr::select(c(question:responses_strongly_agree)) %>%
      gather(data, group, responses_strongly_disagree:responses_strongly_agree) %>%
      arrange(factor(question, levels = c("responses_strongly_disagree", "responses_disagree", "responses_neither_agree_nor_disagree", "responses_agree", "responses_strongly_agree"))) %>%
      mutate(question = factor(question, levels = unique(question)))
    list(likert_combined = likert_combined, likert_long = likert_long)
  })

  output$download_combined <- downloadHandler(
    filename = function() { "likert_combined.csv" },
    content = function(file) {
      write.csv(processed_data()$likert_combined, file, row.names = FALSE)
    }
  )

  output$download_long <- downloadHandler(
    filename = function() { "likert_long.csv" },
    content = function(file) {
      write.csv(processed_data()$likert_long, file, row.names = FALSE)
    }
  )


  data_plot <- eventReactive(input$plot, {
    req(processed_data())
    data_sum <- processed_data()$likert_combined
    n_sizes <- get_n_sizes(data_sum)

    data_sum <- data_sum %>%
      rename(Item = question) %>%
      dplyr::select(Item:responses_strongly_agree) %>%
      mutate(across(c(-Item), ~as.numeric(.x) * 100)) %>%
      rename("Strongly Disagree" = responses_strongly_disagree,
             "Disagree" = responses_disagree,
             "Neither agree nor disagree" = responses_neither_agree_nor_disagree,
             "Agree" = responses_agree,
             "Strongly Agree" = responses_strongly_agree)

    data_sum <- data_sum %>%
      rowwise() %>%
      mutate(Item_orig = Item,
             Item = stringr::str_wrap(Item_orig, width = 50))

    data_sum <- data_sum %>%
      mutate(Item = factor(Item, levels = c(Item)))

    data_plot <- HH::likertplot(data_sum[2:6],
                                horizontal = TRUE,
                                main = input$main_title,
                                sub = list(paste0(n_sizes),
                                           cex = input$cex,
                                           col = input$col),
                                xlab = list("%", cex = input$cex),
                                par.strip.text = list(cex = input$cex, lines = 5, rotate = 30),
                                auto.key = list(space = "right", columns = 1, cex = input$cex),
                                strip = FALSE,
                                add.text = TRUE,
                                xlim = c(-100, 100),
                                scales = list(x = list(rot = 45, cex = input$cex, at = seq(-90, 90, 10)),
                                              y = list(cex = input$cex, labels = rev(data_sum$Item))))

    data_plot
  }
)

  output$likert_plot <- renderPlot({
    data_plot()
  })


  output$plotformat <- renderUI({
    radioboxGroupInput("plot_format",
                       "Select plot format",
                       choices = c("png"))
  })


  output$download_plot <- downloadHandler(
    filename = function() { paste0("likert_plot.", input$plot_format) },
    content = function(file) {
      #HH::hhpng(data_plot(), target = file)
      png(file)
      print(data_plot())
      dev.off()
      #png::writePNG(data_plot(), target = file)
      #ggplot2::ggsave(file, plot = data_plot(), dpi = 300,device="png")
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)

