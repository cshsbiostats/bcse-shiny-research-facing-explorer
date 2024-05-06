library(tidyverse)
library(shiny)
library(bslib)
library(ggsankey)
library(patchwork)
library(bsicons)
library(plotly)

ae_data <- read_csv(here::here('data/u01_b35_data.csv'),
                 show_col_types = FALSE)

qol_data <- read_csv(here::here('data/processed_full_qol_pro_data.csv'),
                 show_col_types = FALSE)

ui <- bslib::page_navbar(
  title = 'Breast Cancer Symptom Explorer',
  nav_spacer(),
  nav_panel(
    'B35 AE Explorer',
    page_sidebar(sidebar = b35_ae_explorer_sidebar_UI('a', data = ae_data), b35_ae_explorer_main_UI('a'))
  ),
  nav_panel(
    'B35 QOL Explorer',
    page_sidebar(sidebar = b35_qol_explorer_sidebar_UI('b', data = qol_data), b35_qol_explorer_main_UI('b'))
  ),
  nav_panel(
    'B35 AE Cohort Explorer',
    page_sidebar(sidebar = b35_patient_explorer_sidebar_UI('c', data = ae_data), b35_patient_explorer_main_UI('c'))
  ),
  nav_panel(
    'Custom AE/QOL Explorer',
    page_sidebar(sidebar = b35_custom_explorer_module_sidebar_UI('d'), b35_custom_explorer_module_main_UI('d'))
  )
)

server <- function(input, output, session) {
  b35_ae_explorer_Server('a', data = ae_data)
  
  b35_qol_explorer_Server('b', data = qol_data)
  
  b35_patient_explorer_Server('c', data = ae_data)
  
  b35_custom_explorer_module_Server('d')
  
}

shinyApp(ui, server)