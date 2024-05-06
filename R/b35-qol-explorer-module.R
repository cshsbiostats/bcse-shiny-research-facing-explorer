b35_qol_explorer_sidebar_UI <- function(id, data) {
  ns <- NS(id)
  
  data <- data |>
    select(patientid, trt, time_point_numeric, value, description)
  
  trt_options <- data |> pull(trt) |> unique() |> sort()
  
  desc_options <- data |>
    pull(description) |>
    unique()
  
  resp_options <- data |>
    pull(value) |>
    unique() |>
    sort()
  
  tagList(
    selectInput(
      inputId = NS(id, 'select_trt'),
      label = tooltip(
        span("1. Select Treatment", bs_icon("info-circle")),
        "This dropdown selects the treatment of interest at the starting timeframe.",
        placement = "right"
      ),
      choices = trt_options
    ),
    selectInput(
      inputId = NS(id, 'select_desc'),
      label = tooltip(
        span("2. Select Description", bs_icon("info-circle")),
        "This dropdown selects the QOL description of the cohort of interest at the initial starting timeframe",
        placement = "right"
      ),
      choices = desc_options
    ),
    selectInput(
      inputId = NS(id, 'select_resp'),
      label = tooltip(
        span("3. Response", bs_icon("info-circle")),
        "This dropdown selects the response for the QOL description for the cohort of interest at the initial starting timeframe",
        placement = "right"
      ),
      choices = resp_options
    ),
    sliderInput(
      inputId = NS(id, 'select_timeframe'),
      label = tooltip(
        span("4. Timeframe", bs_icon("info-circle")),
        "This slider selects the initial and end timeframe in months for the cohort of interest to visualize.",
        placement = "right"
      ),
      min = 0,
      max = 60,
      value = c(0, 12),
      step = 6
    ),
    actionButton(
      NS(id, 'btn_ae_visualize'),
      tooltip(
        span("Visualize", bs_icon("info-circle")),
        "Clicking on the following button will generate a Sankey diagram and summary statement based upon the selected patient cohort.",
        placement = "right"
      ),
      icon = icon(name = 'chart-bar', lib = 'font-awesome')
    ),
    generate_report_UI('report')
  )
}

b35_qol_explorer_main_UI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("B35 QOL Cohort Explorer"),
      card_body(
        "The following application allows you to visualize the PRO-CTCAE QOL data in a Sankey diagram format. The Sankey diagram is a useful tool for visualizing the flow of patients between different timepoint of interest. The following application allows you to select the treatment, QOL description, and response of the cohort of interest at a specified timepoint. We can then observe the flow and responses of the patients across the various timepoints."
      ),
      max_height = '250px'
    ),
    card(
      full_screen = TRUE,
      card_header("Results"),
      card_body(class = "p-0", plotOutput(NS(id, 'sankey_plot'))),
      card_body(htmlOutput(NS(id, 'summary_descr')), max_height = '300px'),
      height = '750px'
    )
  )
}

b35_qol_explorer_Server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    data <- data |>
      select(patientid, trt, time_point_numeric, value, description)
    
    make_qol_sankey <- \(data, selected_treatment, selected_description, selected_timeframe, selected_response) {
      
      response_order <- c(
        'No response',
        'Not at all',
        'Slightly',
        'Moderately',
        'Quite a bit',
        'Extremely'
      )
      
      data <- local({
        data <- data |>
          select(patientid, trt, time_point_numeric, value, description) |>
          mutate(time_point_numeric = time_point_numeric * 6)
        
        data <- data |>
          filter(time_point_numeric <= 60)
        
        data <- data |>
          filter(trt == selected_treatment) |>
          group_by(patientid) |>
          filter(
            any(
              time_point_numeric == selected_timeframe[1] &
                description == selected_description &
                value == selected_response
            )
          ) |>
          ungroup() |>
          filter(
            time_point_numeric >= selected_timeframe[1] &
              time_point_numeric <= selected_timeframe[2]
          ) |>
          filter(description == selected_description) |>
          mutate(time_point_label = glue::glue('{time_point_numeric} M'))
        
      })
      
      total_patients <- data |>
        pull(patientid) |>
        unique() |>
        length()
      
      plot_data <- local({
        plot_data <- data |>
          select(-time_point_numeric) |>
          pivot_wider(names_from = time_point_label, values_from = value)
        
        plot_data <- plot_data |>
          make_long(4:ncol(plot_data))
      })
      
      plot <- local({
        
        cbf_1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        grade_colors <- c(
          'No response' = "#999999",
          'Not at all' = "#E69F00",
          'Slightly' = "#56B4E9",
          'Moderately' = "#009E73",
          'Quite a bit' = "#D55E00",
          'Extremely' = "#F0E442"
        )
        
        plot_data <- plot_data |>
          mutate(node = factor(
            node,
            levels = response_order
          ))
        
        plot <- ggplot(
          plot_data,
          aes(
            x = x,
            next_x = next_x,
            node = node,
            next_node = next_node,
            fill = factor(node)
          )
        ) +
          geom_sankey(flow.alpha = .3) +
          theme_sankey(base_size = 20) +
          theme(
            legend.position = 'bottom',
            legend.title = element_blank(),
            axis.text.x = element_text(face = 'bold'),
            axis.title = element_blank()
          ) +
          scale_color_manual(values = grade_colors) +
          scale_fill_manual(values = grade_colors) +
          labs(
            title = selected_description,
            subtitle = glue::glue('{selected_treatment}, n = {total_patients}')
          )
        
        plot
      })
      
      summary_description <- local({
        res <- data |> 
          filter(time_point_numeric == selected_timeframe[2]) |> 
          count(value) |> 
          mutate(value = factor(value, levels = response_order)) |> 
          arrange(value) |> 
          mutate(
            prop = n / sum(n),
            prop = scales::percent_format()(prop)
          )
        
        summary_res <- glue::glue_data(res, 'A total of {n} ({prop}) of the patients responded "{value}". ') |> 
          glue::glue_collapse(sep = '<br>')
        
        
        summary_description <- glue::glue(
          'Among a total of {total_patients} patients who responded "{selected_response}" for "{selected_description}" at {selected_timeframe[1]} M.',
          'By the {selected_timeframe[2]} M timepoint',
          
          '{summary_res}',
          .sep = '<br><br>'
        )
      })
      
      tibble::lst(
        plot,
        summary_description
      )
      
    }
    
    
    results <- eventReactive(input$btn_ae_visualize, {
      make_qol_sankey(
        data = data,
        selected_treatment = input$select_trt,
        selected_description = input$select_desc,
        selected_response = input$select_resp,
        selected_timeframe = input$select_timeframe
      )
      
    })
    
    observeEvent(input$btn_ae_visualize, {
      req(results())
      
      output$sankey_plot <- renderPlot({
        results()$plot
        
      })
      
      output$summary_descr <- renderPrint({
        results()$summary_description
      })
      
    })
    
    generate_report_Server('report')
    
  })
}



# library(shiny)
# 
# ui <- fluidPage(
#   
#   b35_qol_explorer_sidebar_UI('a'),
#   b35_qol_explorer_main_UI('a')
#   
# )
# 
# server <- function(input, output, session) {
#   
#   b35_qol_explorer_Server('a')
# }
# 
# shinyApp(ui, server)