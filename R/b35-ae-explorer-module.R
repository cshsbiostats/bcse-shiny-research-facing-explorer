
b35_ae_explorer_sidebar_UI <- function(id) {
  ns <- NS(id)
  
  data <- read_csv(here::here('data/u01_b35_data.csv'),
                   show_col_types = FALSE)
  
  trt_options <- data |> pull(trt) |> unique() |> sort()
  
  ae_options <- data |>
    count(ae, trt) |>
    count(ae) |>
    filter(n == 2) |> pull(ae)
  
  tagList(
  
    selectizeInput(
      NS(id, 'select_ae'),
      tooltip(
        span(
          "1. Select Adverse Events",
          bs_icon("info-circle")
        ),
        "The following contains a list of adverse events that are present in both treatment arms. Select one or more adverse events to visualize the flow of patients between grades for each treatment arm.",
        placement = "right"
      ),
      choices = ae_options,
      multiple = T,
      options = list(placeholder = 'Select an Adverse Event')
    ),
    actionButton(
      NS(id, 'btn_ae_visualize'),
      tooltip(
        span(
          "Visualize",
          bs_icon("info-circle")
        ),
        "Clicking on the following button will generate a Sankey diagram for each selected adverse event.",
        placement = "right"
      ),
      icon = icon(name = 'chart-bar', lib = 'font-awesome')
    ),
    generate_report_UI(NS(id, 'generate-report'))
    
  )
}

b35_ae_explorer_main_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    card(
      card_header("B35 AE Explorer"),
      card_body(
        "The following application allows you to visualize the flow of patients between grades for each treatment arm. Select one or more adverse events from the dropdown menu and click on the 'Visualize' button to generate a Sankey diagram for each selected adverse event. The 'Download Report' button will generate a PDF report containing the Sankey diagrams for each selected adverse event."
      ),
      min_height = 250
    ),
    uiOutput(NS(id, 'sankey_plots'))
    
  )
}

b35_ae_explorer_Server <- function(id) {
  
  make_sankey_diagram <- \(data, trt, ae, cycle_limit = 10) {
    
    data <- data |> 
      filter(ncycle <= !!cycle_limit)
    
    data <- data |> 
      filter(trt == !!trt & ae == !!ae)
    
    total_patients <- data |> 
      filter(ncycle == 1) |> 
      pull(patientid) |> 
      unique() |> 
      length()
    
    data <- data |> 
      mutate(
        ncycle = ncycle * 6,
        ncycle = glue::glue('{ncycle} M')
      )
    
    data <- data |> 
      pivot_wider(names_from = ncycle, values_from = ae_grade,
                  values_fill = 'Off Treatment')
    
    plot_data <- data |> make_long(4:ncol(data))
    
    cbf_1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    
    
    grade_colors <- c(
      "0-1" = "#999999",
      "2" = "#E69F00",
      "3" = "#56B4E9",
      "4" = "#009E73",
      "5" = "#D55E00",
      "Off Treatment" = "#000000"
    )
    
    plot <- ggplot(plot_data,
                   aes(
                     x = x,
                     next_x = next_x,
                     node = node,
                     next_node = next_node,
                     fill = factor(node)
                   )) +
      geom_sankey(flow.alpha = .3) +
      theme_sankey(base_size = 20) +
      theme(legend.position = 'bottom',
            legend.title = element_blank(),
            axis.text.x = element_text(face = 'bold'),
            axis.title = element_blank()) +
      scale_color_manual(
        values = grade_colors
      ) + 
      scale_fill_manual(
        values = grade_colors
      ) + 
      labs(title = ae, subtitle = glue::glue('{trt}, n = {total_patients}'))
    
    plot
    
  }
  
  data <- read_csv(here::here('data/u01_b35_data.csv'),
                   show_col_types = FALSE)
  
  trt_options <- data |> pull(trt) |> unique() |> sort()
  
  ae_options <- data |>
    count(ae, trt) |>
    count(ae) |>
    filter(n == 2) |> pull(ae)
  
  
  moduleServer(
    id,
    function(input, output, session) {
     
      plots <- eventReactive(input$btn_ae_visualize, {
        
        map(input$select_ae, \(x) {
          
          plot1 <- make_sankey_diagram(data, trt = trt_options[1], ae = x)
          plot2 <- make_sankey_diagram(data, trt = trt_options[2], ae = x)
          
          plot1 / plot2
          
        }) |> set_names(nm = input$select_ae)
        
      })
      
      
      observeEvent(input$btn_ae_visualize, {
        
        req(plots())
        
        output$sankey_plots <- renderUI({
          
          res <- pmap(list(plots(), 1:length(plots()), names(plots())), \(plot, i, title) {
            
            output[[paste0('plot', i)]] <- renderPlot({plot})
      
            card(
              card_header(title),
              card_body(
                plotOutput(NS(id, paste0('plot', i)), height = 500),
                min_height = 500
              ),
              full_screen = TRUE,
              min_height = 750
              
            )
            
          })
          
          tagList(!!!res)
        })
        
      })
      
      generate_report_Server('generate-report', 
                             template_file = 'b35-ae-explorer-report-template.qmd',
                             output_file_name = 'thisisatest.pdf',
                             results = plots())
      
    }
  )
}

library(shiny)

run_app <- \() {

  data <- read_csv(here::here('data/u01_b35_data.csv'),
                   show_col_types = FALSE)

  trt_options <- data |> pull(trt) |> unique() |> sort()

  ae_options <- data |>
    count(ae, trt) |>
    count(ae) |>
    filter(n == 2) |> pull(ae)

  ui <- fluidPage(
    b35_ae_explorer_sidebar_UI('a'),
    b35_ae_explorer_main_UI('a')
  )

  server <- function(input, output, session) {
    b35_ae_explorer_Server('a')
  }

  shinyApp(ui, server)

}

run_app()
