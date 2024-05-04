b35_custom_explorer_module_sidebar_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(NS(id, 'uploaded_data'), 'Upload Data'),
    uiOutput(NS(id, 'dynamic_controls')),
    uiOutput(NS(id, 'timepoint_order_ui')),
    uiOutput(NS(id, 'response_order_ui')),
    actionButton(
      NS(id, 'btn_ae_visualize'),
      'Visualize',
      icon = icon(name = 'chart-bar', lib = 'font-awesome')
    ),
    downloadButton(
      NS(id, 'report'),
      'Download Report',
      icon = icon(name = 'chart-bar', lib = 'font-awesome')
    )
  )
}

b35_custom_explorer_module_main_UI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header('Custom AE/QOL Explorer'),
      card_body(
        'The custom AE/QOL explorer allows the investigator to upload their own data in the application for generating Sankey diagrams. Please utilize the following file as a reference on the structure the application requires for data upload.',
        tags$a(href = "https://raw.githubusercontent.com/cshsbiostats/breast-cancer-symptom-explorer/master/res/custom_sankey_data_template.csv", "Download Data Template")
      ),
      full_screen = TRUE,
      fill = TRUE,
      max_height = '250px'
    ),
    card(
      card_header('Sankey Diagram'),
      card_body(class = "p-0", plotOutput(NS(id, 'sankey_plot'))),
      full_screen = TRUE,
      fill = TRUE,
      height = '750px'
    )
  )
}

b35_custom_explorer_module_Server <- function(id) {
  
  generate_custom_sankey <- \(data, timepoint_order, response_order, selected_group, timepoint_col, patientid_col) {
    
    timepoint_col <- sym(timepoint_col)
    patientid_col <- sym(patientid_col)
    
    data <- data |>
      rename(
        'timepoint' = !!timepoint_col,
        'patientid' = !!patientid_col
      )
    
    data <- data |>
      mutate(timepoint = factor(timepoint, levels = c(timepoint_order))) |> 
      arrange(timepoint) |>
      filter(!is.na(timepoint)) |>
      pivot_longer(-c(timepoint, patientid))

    plot_data <- data |>
      filter(name == selected_group) |>
      mutate(value = as.character(value)) |>
      pivot_wider(names_from = timepoint, values_from = value, values_fill = 'Missing')

    plot_data <- plot_data |> make_long(3:ncol(plot_data))

    plot_data <- plot_data |>
      mutate(node = factor(node, levels = c(response_order, 'Missing')))

    grade_colors <- c(
      "#999999",
      "#E69F00",
      "#56B4E9",
      "#009E73",
      "#D55E00",
      "#000000"
    )

    plot <- ggplot(plot_data,
                   aes(
                     x = x,
                     next_x = next_x,
                     node = node,
                     next_node = next_node,
                     fill = node
                   )) +
      geom_sankey(flow.alpha = .3) +
      theme_sankey(base_size = 20) +
      theme(
        legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x = element_text(face = 'bold'),
        axis.title = element_blank()
      ) +
      scale_color_manual(
        values = grade_colors
      ) +
      scale_fill_manual(
        values = grade_colors
      )

    plot
    
  }
  
  moduleServer(id, function(input, output, session) {
    
    data <- reactive({
      file <- input$uploaded_data
      
      file_type <- fs::path_ext(file$datapath)
      
      switch (
        file_type,
        'csv' = read_csv(file$datapath),
        'xlsx' = readxl::read_excel(file$datapath)
      )
      
    })
    
    observeEvent(input$uploaded_data, {
      output$dynamic_controls <- renderUI({
        req(data())
        
        col_names <- names(data())
        
        tagList(
          selectInput(
            inputId = NS(id, 'select_patientid'),
            label = '1. Select Patient ID Column',
            choices = col_names,
            selected = col_names[[1]]
          ),
          selectInput(
            inputId = NS(id, 'select_timepoint'),
            label = '2. Select Timepoint Column',
            choices = col_names,
            selected = col_names[[2]]
          ),
          selectInput(
            inputId = NS(id, 'select_group'),
            label = '3. Select AE / QOL Column',
            choices = col_names,
            selected = col_names[[3]]
          )
        )
      })
    })
    
    observeEvent(input$select_timepoint, {
      output$timepoint_order_ui <- renderUI({
        timepoint_choices <- data() |> pull(.data[[input$select_timepoint]])
        
        selectInput(
          inputId = NS(id, 'timepoint_order'),
          label = '4. Select Timepoint Ordering',
          choices = timepoint_choices,
          selectize = TRUE,
          multiple = TRUE
        )
      })
      
    })
    
    observeEvent(input$select_group, {
      output$response_order_ui <- renderUI({
        req(data())
        
        data <- data() |>
          pivot_longer(-one_of(input$select_patientid, input$select_timepoint))
        
        response_choices <- data |>
          filter(name == input$select_group) |>
          pull(value) |>
          unique()
        
        selectInput(
          inputId = NS(id, 'response_order'),
          label = '5. Select Response Ordering',
          choices = response_choices,
          selectize = TRUE,
          multiple = TRUE
        )
      })
      
    })
    
    results <- eventReactive(input$btn_ae_visualize, {
      req(data())
      
      out <- generate_custom_sankey(
        data = data(),
        timepoint_order = input$timepoint_order,
        response_order = input$response_order,
        selected_group = input$select_group,
        timepoint_col = input$select_timepoint,
        patientid_col = input$select_patientid
      )
      
      out
      
    })
    
    observeEvent(input$btn_ae_visualize, {
      req(results())
      
      
      output$sankey_plot <- renderPlot({
        results()
        
      })
      
      
    })
    
    output$report <- downloadHandler(
      filename = \(x) {
        paste0('pro_ctcae_ae_sankey_', Sys.Date(), '.pdf')
      },
      content = function(file) {
        rmarkdown::render(
          'explore_qol_template.Rmd',
          output_file = file,
          params = list('results' = results()),
          envir = new.env(parent = globalenv())
        )
        
      }
    )
  })
}

# library(shiny)
# 
# ui <- fluidPage(
#   b35_custom_explorer_module_sidebar_UI('a'),
#   b35_custom_explorer_module_main_UI('a')
# )
# 
# server <- function(input, output, session) {
#   b35_custom_explorer_module_Server('a')
# }
# 
# shinyApp(ui, server)
