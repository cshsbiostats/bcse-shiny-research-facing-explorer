generate_report_UI <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(
      NS(id, 'report'),
      'Download Report',
      icon = icon(name = 'chart-bar', lib = 'font-awesome')
    )
  )
}

generate_report_Server <- function(id, template_file, output_file_name, results) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$report <- downloadHandler(
        filename = \(x) output_file_name,
        content = function(file) {
          
          file_type <- fs::path_ext(output_file_name)
          
          temp_results <- tempfile(fileext = '.rds')
          
          write_rds(results, temp_results)
          
          template <- file.path(tempdir(), template_file)
          
          file.copy(template_file, template, overwrite = TRUE)
          
          temp_report <- tempfile(fileext = paste0('.', file_type))
          
          quarto::quarto_render(
            input = template,
            output_file = fs::path_file(temp_report),
            execute_params = list('results' = temp_results),
            output_format = file_type
          )
          
          file.copy(fs::path_file(temp_report), file)

        }
      )
      
    }
  )
}

# library(shiny)
# 
# ui <- fluidPage(
#   generate_report_UI('a')
# )
# 
# server <- function(input, output, session) {
#   
#   data <- read_csv('data/u01_b35_data.csv')
#   
#   tbl <- data |> 
#     slice(1:10)
#   
#   plot <- ggplot(tbl, aes(x = ncycle, y = ae_grade)) + 
#     geom_point()
#   
#   generate_report_Server(
#     id = 'a',
#     template_file = 'template.qmd',
#     output_file_name = 'thisisatest.pdf',
#     results = plot
#   )
#   
# }
# 
# shinyApp(ui, server)
# 
# 
# 

