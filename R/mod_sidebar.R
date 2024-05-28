#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  refreshed <- purrr::map(list.files(path = "data", full.names = TRUE), ~file.info(.x)$mtime) |> {\(x) {do.call(c, x)}}() |> max()
  bs4Dash::bs4DashSidebar(
    id = "sidebar",
    status = "white",
    skin = "light",
    elevation = 4,
    collapsed = TRUE,
    fixed = TRUE,
      bs4Dash::bs4SidebarMenu(
        id = "active_tab",
        compact = TRUE,
        bs4Dash::bs4SidebarMenuItem(
          text = "Welcome",
          tabName = "welcome", #homeTab
          icon = shiny::icon("home")
        ),
        bs4Dash::bs4SidebarMenuItem(
          text = "Prioritization",
          tabName = "prioritization", #prioritizationListTab
          icon = shiny::icon("sitemap")
        ),
        bs4Dash::bs4SidebarMenuItem(
          text = "Client Counts",
          tabName = "client_counts", #currentProviderLevel
          icon = shiny::icon("users") 
        ),
        bs4Dash::bs4SidebarMenuItem(
          text = "COVID-19",
          tabName = "covid19",
          icon = shiny::icon("virus"),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Vaccine Status",
            tabName = "c19_vaccine_status"
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Second Dose Logistics", #vaccineStatus
            tabName = "c19_second_dose" #vaccineSecondDose
          )
        ),
        bs4Dash::bs4SidebarMenuItem(
          text = "Bed & Unit Use",
          tabName = "utilization", #utilizationTab
          icon = shiny::icon("bed")
        ),
        bs4Dash::bs4SidebarMenuItem(
          text = "Data Quality",
          tabName = "dq",
          icon = shiny::icon("database"),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Program-level",
            tabName = "dq_program_level" #dqTab
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Data Entry Timeliness",
            tabName = "dq_timeliness" #deskTime
          ),
          # Deprecated in Clarity
          # bs4Dash::bs4SidebarMenuSubItem(
          #   text = "Unsheltered",
          #   tabName = "dq_unsheltered" #dqUnsh
          # ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Region-level",
            tabName = "dq_region_level" #dqRegion
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "System Summary",
            tabName = "dq_system_summary" # dqCoC
          )
        ),
        bs4Dash::bs4SidebarMenuItem(
          text = "Veterans",
          tabName = "vet_active_list", 
          icon = shiny::icon("medal")
        ),
        bs4Dash::bs4SidebarMenuItem(
          text = "BoS CoC Competition",
          tabName = "coc_competition", # cocCompetitionTab
          icon = shiny::icon("flag-checkered")
        ),
        # bs4Dash::bs4SidebarMenuItem(
        #   text = "Mahoning CoC Competition",
        #   tabName = "coc_competition_mahoning",
        #   icon = shiny::icon("flag-checkered")
        # ),
        bs4Dash::bs4SidebarMenuItem(
          text = "Ohio BoSCoC Performance",
          icon = shiny::icon("file-medical-alt"),
          tabName = "qpr",
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Permanent Housing Need",
            tabName = "qpr_community_need_ph" #spdat1Tab
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Literally Homeless Need",
            tabName = "qpr_community_need_lh" #spdat2Tab
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Length of Stay",
            tabName = "qpr_length_of_stay" # LoS-Tab
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Exits to Permanent Housing",
            tabName = "qpr_permanent_housing" # PHTab
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Exits to Temp or Permanent Housing",
            tabName = "qpr_temp_permanent_housing" # PHTab
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Non-Cash Benefits at Exit",
            tabName = "qpr_noncash_benefits" # NCB-Tab
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Health Insurance at Exit",
            tabName = "qpr_health_insurance" # HI-Tab
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Income Growth",
            tabName = "qpr_income_growth" # income-Tab
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Rapid Placement for RRH",
            tabName = "qpr_rrh_placement" # rapid-Tab
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "RRH Spending",
            tabName = "qpr_rrh_spending" # spending-Tab
          )
        ),
        bs4Dash::bs4SidebarMenuItem(
          text = "Mahoning Performance",
          tabName = "mpo",
          icon = shiny::icon("file-medical-alt")
        ),
        bs4Dash::bs4SidebarMenuItem(
          text = "Agency & Program Lookup",
          tabName = "program_lookup",
          icon = shiny::icon("search")
        ),
        actionButton(
          inputId = "logOut",
          label = "Log Out",
          onclick =
            "window.open(window.location.href + '__logout__/', '_self')"
        )
        , bs4Alert(
          tags$strong("Data refreshed: ",tags$br(), refreshed),
          id = "data_refresh",
          status = purrr::when(
            refreshed,
            . > Sys.Date() ~ "success",
            . > Sys.Date() - 7 ~ "warning",
            ~ "danger"
          ),
          width = 12
        )
      )
      
    )
}
    

#' sidebar Server Functions
#'
#' @noRd 
mod_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}


## To be copied in the UI
# mod_sidebar_ui("sidebar_1")
    
## To be copied in the server
# mod_sidebar_server("sidebar_1")
