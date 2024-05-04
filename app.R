library(tidyverse)
library(shiny)
library(bslib)
library(ggsankey)
library(patchwork)
library(bsicons)
library(plotly)


ui <- bslib::page_navbar(
  title = 'Breast Cancer Symptom Explorer',
  nav_spacer(),
  nav_panel(
    'B35 AE Explorer',
    page_sidebar(sidebar = b35_ae_explorer_sidebar_UI('a'), b35_ae_explorer_main_UI('a'))
  ),
  nav_panel(
    'B35 QOL Explorer',
    page_sidebar(sidebar = b35_qol_explorer_sidebar_UI('b'), b35_qol_explorer_main_UI('b'))
  ),
  nav_panel(
    'B35 AE Cohort Explorer',
    page_sidebar(sidebar = b35_patient_explorer_sidebar_UI('c'), b35_patient_explorer_main_UI('c'))
  ),
  nav_panel(
    'Custom AE/QOL Explorer',
    page_sidebar(sidebar = b35_custom_explorer_module_sidebar_UI('d'), b35_custom_explorer_module_main_UI('d'))
  )
)

server <- function(input, output, session) {
  b35_ae_explorer_Server('a')
  
  b35_qol_explorer_Server('b')
  
  b35_patient_explorer_Server('c')
  
  b35_custom_explorer_module_Server('d')
  
}

shinyApp(ui, server)