#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

shinyUI(
  dashboardPage(skin = "blue",
                dashboardHeader(title = "BLOOMY"),
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("General Overview", icon = icon("globe"), tabName = "tb_dashboard"),
                    menuItem("Inconsistencies", icon = icon("exclamation-triangle"), tabName = "tb_queries"),
                    menuItem("Calendar", icon = icon("calendar"), tabName = "tb_calendar")
                  )
                ),
                dashboardBody(
                  shinyauthr::loginUI(
                    "login", 
                    cookie_expiry = cookie_expiry
                  ),
                  div(id = 'main_page',
                      #div(textOutput(outputId="hh_detail_txt2"), style="color:black; font-size:16px; position:relative;"),
                      tabItems(
                        tabItem("tb_dashboard",
                                fluidRow(
                                  withSpinner(valueBoxOutput("total",  width = 2), type = 2),
                                  withSpinner(valueBoxOutput("total_12_v",  width = 2), type = 2),
                                  withSpinner(valueBoxOutput("lt_15",  width = 2), type = 2),
                                  withSpinner(valueBoxOutput("gt_15_lt_30",  width = 2), type = 2),
                                  withSpinner(valueBoxOutput("mt_30",  width =2), type = 2),
                                  withSpinner(valueBoxOutput("not_done",  width = 2), type = 2)
                                  ),
                                fluidRow(
                                  box(
                                    width = 6, status = "info", solidHeader = TRUE,
                                    title = "Days since the last visit",
                                    withSpinner(plotlyOutput("plt_distr"),type = 2)),
                                  box(
                                    width = 6, status = "info",solidHeader = TRUE,
                                    title = "Visits Not Done",
                                    withSpinner(plotlyOutput("plt_pie"),type = 2)),
                                ),
                                  DT::dataTableOutput("dt_all"),
                                
                                
                        ),
                        tabItem("tb_calendar",
                                fluidRow(
                                  withSpinner(valueBoxOutput("cal_date"),type = 2),
                                  withSpinner(valueBoxOutput("tot_tovisit"),type = 2),
                                  withSpinner(valueBoxOutput("tot_visited"),type = 2),
                                  dateInput("sel_date","Select Week to Display the Visits",value = NULL,min = NULL,max = NULL,format = "yyyy-mm-dd",startview = "month",weekstart = 0,language = "en",
                                    width = NULL,
                                    autoclose = TRUE,
                                    datesdisabled = NULL,
                                    daysofweekdisabled = NULL
                                  ),
                                  
                                  DT::dataTableOutput("calendar"),
                                  actionButton("all_data", "All Data", icon = icon("list")),
                                  actionButton("to_visit", "Selected Date Records"), icon = icon("list"))
                                ),
                        tabItem("tb_queries",
                                fluidRow(
                                  withSpinner(valueBoxOutput("tot_rm"),type = 2),
                                  DT::dataTableOutput("to_rm")
                                  #numericInput("maxrows", "Rows to show", 25),
                                  #verbatimTextOutput("rawtable"),
                                  #downloadButton("downloadCsv", "Download as CSV"))
                                ))
                      )
                  ) %>% shinyjs::hidden() 
                )
  )
)