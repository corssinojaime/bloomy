#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)

function(input, output, session) {
  
  
  withProgress(message = 'Loading Data', value = 4, {
    data_raw <- loadData(session)
    data_load <- process(data_raw)
    
  })
  

  #data_load <- shiny::eventReactive(c(loadData(session), auth()),{
  #  shinybusy::
  #})
  
  #data = loadData(session)
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  shiny::observe({
    if (credentials()$user_auth) {
      shinyjs::show(id = 'main_page')
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  user_info <- reactive({
    credentials()$info
  })
  
  
  dat_long = data_raw %>% dplyr::select(record_id, redcap_event_name, visits_date)
  
  get_calendar = function(select_date){
    week_begin = as.Date(format(as.Date(select_date, "%m/%d/%Y"),"%Y-%W-1"),"%Y-%W-%u")
    #week_begin = as.Date(format(as.Date(Sys.Date(), "%m/%d/%Y"),"%Y-%W-1"),"%Y-%W-%u")
    week_end = week_begin + 6
  
  cal = data_raw %>% dplyr::filter(grepl("recrut", redcap_event_name)) %>% 
    dplyr::select(record_id, inq_date, visits_date) %>% 
    dplyr::mutate(visit_1 = visits_date + 15,
                  visit_2 = visit_1 + 15,
                  visit_3 = visit_2 + 15,
                  visit_4 = visit_3 + 15,
                  visit_5 = visit_4 + 15,
                  visit_6 = visit_5 + 15,
                  visit_7 = visit_6 + 15,
                  visit_8 = visit_7 + 15,
                  visit_9 = visit_8 + 15,
                  visit_10 = visit_9 + 15,
                  visit_11 = visit_10 + 15,
                  visit_12 = visit_11 + 15)
  
  long =  reshape2::melt(cal,
               id.vars=c("record_id"),
               measure.vars=c("visit_1","visit_2","visit_3","visit_4","visit_5","visit_6","visit_7","visit_8","visit_9","visit_10","visit_11","visit_12"),
               variable.name="visits",
               value.name="visits_date") %>%
    dplyr::mutate(to_visit_or_done = (visits_date >= week_begin & visits_date <= week_end)*1) %>% 
    dplyr::filter(to_visit_or_done == 1) %>% 
    dplyr::distinct(record_id, .keep_all = TRUE) %>% 
    dplyr::select(record_id, to_visit_or_done)
  
  c = merge(cal, long, by = "record_id", all.x = TRUE)
  
  rd = data_raw %>% 
    dplyr::filter(!grepl(c("recru"), redcap_event_name)) %>%  
    dplyr::select(record_id, redcap_event_name, visits_date) %>% 
    dplyr::mutate(to_visit_or_done = (visits_date >= week_begin & visits_date <= week_end)*1) %>% 
    dplyr::filter(to_visit_or_done == 1) %>% 
    dplyr::distinct(record_id, .keep_all = TRUE) %>% 
    dplyr::select(record_id, to_visit_or_done)
  
  c2 = data_load %>%  dplyr::select(record_id, recrutamento, contains("visita"), -c(visita_nao_program))
  c2 = merge(c2, rd, by = "record_id", all.x = TRUE)
  
#  %>% dplyr::rename(visit_1 = `1a_visita_arm_1`)
  
  #c2 = data_load %>%  dplyr::select(record_id, recrutamento, contains("visita")) 
  names(c2) = names(c)
  c2$id = "redcap"
  c$id = "calendario"
  calendar = rbind(c,c2) %>% dplyr::arrange(record_id) %>% dplyr::mutate(id = as.factor(id),
                                                                         to_visit_or_done = as.factor(to_visit_or_done))

  
  
  
  return(calendar)
  
  }
  
  
  not_done = data_raw %>% dplyr::filter(!is.na(visit_notdone_reason)) %>% 
    dplyr::select(record_id, redcap_event_name, visits_date, visit_notdone_reason) %>% 
    dplyr::mutate(reason = case_when(visit_notdone_reason == 1 ~ "Lost Followup",
                                     visit_notdone_reason == 2 ~ "Absent",
                                     visit_notdone_reason == 4 ~ "Withdrawal",
                                     visit_notdone_reason == 5 ~ "Other"))
  
  
  
  
  user_data <- reactive({
    req(credentials()$user_auth)
    if (user_info()$permissions == "admin") {
      #dplyr::starwars[, 1:10]
    } else if (user_info()$permissions == "standard") {
      #dplyr::storms[, 1:11]
    }
  })
  

  select_week <- reactive({
    output$cal_date <- renderValueBox({
      val =   input$sel_date
      calendar = get_calendar(val)
      
      week_begin = as.Date(format(as.Date(val, "%m/%d/%Y"),"%Y-%W-1"),"%Y-%W-%u")
      week_end = week_begin + 6
      
      output$calendar <- DT::renderDT({
        DT::datatable(
          calendar,
          extensions = "Buttons",
          filter = 'top',
          options = list(
            lengthChange = TRUE,
            dom = "Bfrtip",
            buttons = list(
              list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download',
                   exportOptions = list(modifier = list(search = "applied")))
            )
          )
        )
      }, server = FALSE)
      
      
      output$tot_tovisit <- renderValueBox({
        valueBox(
          value = calendar %>% dplyr::filter(id == "calendario" & to_visit_or_done == 1) %>%  nrow(),
          subtitle = paste0("To visit on Week: ", lubridate::isoweek(input$sel_date), ": From ", week_begin, " To ", week_end),
          icon = icon("list"),
          color = "blue"
        )
      })
      
      output$tot_visited <- renderValueBox({
        valueBox(
          value = calendar %>% dplyr::filter(id == "redcap" & to_visit_or_done == 1) %>%  nrow(),
          subtitle = paste0("Visited on Week: ", lubridate::isoweek(input$sel_date), ": From ", week_begin, " To ", week_end),
          icon = icon("list"),
          color = "red"
        )
      })
      
      valueBox(
            value = input$sel_date,
            subtitle = paste0("Selected Week: ", lubridate::isoweek(input$sel_date), ": From ", week_begin, " To ", week_end),
            icon = icon("person"),
            color = "green"
          )
        })
      })

      
  observe(select_week())
  
  output$total <- renderValueBox({
    val =  data_load %>% nrow()
    valueBox(
      value = val,
      subtitle = "Total Participants",
      icon = icon("person"),
      color = "green"
    )
  })
  
  output$total_12_v <- renderValueBox({
    val =  data_raw %>% dplyr::filter(grepl("12a_visita_", redcap_event_name)) %>% nrow()
    valueBox(
      value = val,
      subtitle = "Participants with 12th visit ",
      icon = icon("person"),
      color = "black"
    )
  })
  
  output$lt_15 <- renderValueBox({
    val = data_load %>% dplyr::filter(duration < 15) %>% nrow()
    valueBox(
      value = val,
      subtitle = "Visited from the last 15 days",
      icon = icon("users"),
      color = "navy"
    )
  })
  
  output$gt_15_lt_30 <- renderValueBox({
    val = data_load %>% dplyr::filter(duration > 15 & duration <= 30) %>% nrow()
    valueBox(
      value = val,
      subtitle = "Visited from the last 15 to 30 days",
      icon = icon("users"),
      color = "yellow"
    )
  })
  
  output$mt_30 <- renderValueBox({
    val = data_load %>% dplyr::filter(duration > 30) %>% nrow()
    valueBox(
      value = val,
      subtitle = "Last visit date more than 30 days",
      icon = icon("users"),
      color = "red"
    )
  })
  
  output$not_done <- renderValueBox({
    val = not_done %>% nrow()
    valueBox(
      value = val,
      subtitle = "Not Done Visits",
      icon = icon("exclamation-triangle"),
      color = "blue"
    )
  })
 
  
  output$plt_distr <- renderPlotly({
    df <-data_load
    df = df %>% 
      dplyr::group_by(duration) %>% 
      dplyr::summarise(total = n()) %>% 
      plotly::plot_ly(x=~duration, y=~total, type = 'bar', colors = 'Accent') %>%
      plotly::layout(xaxis = list(title= 'Days Since Last Visit'), yaxis = list(title='total'))
    df
  })
 
  
  output$plt_pie <- renderPlotly({
    df = not_done %>% dplyr::group_by(reason) %>% 
      dplyr::summarise(total = n())
    
    fig <- plot_ly(df, labels = ~reason, values = ~total, type = 'pie')
    fig <- fig %>% layout(title = 'Visits not done vs Reasons',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  })
  

  
  output$dt_all <- DT::renderDT({
    DT::datatable(
      data_load %>% dplyr::select(record_id, recrutamento, contains("visita"), duration, pending_vis) %>% 
        dplyr::mutate(duration = as.factor(duration),
                      pending_vis = as.factor(pending_vis)),
      extensions = "Buttons",
      filter = 'top',
      options = list(
        lengthChange = TRUE,
        dom = "Bfrtip",
        buttons = list(
          list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download',
               exportOptions = list(modifier = list(search = "applied")))
        )
      )
    )
  }, server = FALSE)
  
  
  
  to_rm = data_load %>% 
    dplyr::filter(good_seq == 0) %>% 
    dplyr::select(record_id, recrutamento, contains("visita"), redcap_event_name, duration, pending_vis)
 
   ##### Tab Queries
  output$tot_rm <- renderValueBox({
    val =  paste0(if_else(to_rm %>% nrow() == 1, " Record to Verify", " Records to Verify"))
    val =  paste0(to_rm %>% nrow(), val)
    
    valueBox(
      value = val,
      subtitle = "Queries to be solved",
      icon = icon("list"),
      color = "red"
    )
  })
  
  output$to_rm <- DT::renderDT(
    to_rm,
    #caption = "Records to be deleted",  
    #colnames=c("#", "record id", "message"),
    extensions = 'Buttons',
    selection = 'single', ## enable selection of a single row
    #filter = 'bottom',              ## include column filters at the bottom
    #rownames = FALSE,                ## don't show row numbers/names
    
    options = list(lengthChange = TRUE,
                   dom = 'Bfrtip',
                   buttons = list('copy', 'print', list(
                     extend = 'collection',
                     buttons = c('csv', 'excel', 'pdf'),
                     text = 'Download')))
  )
  
  ##################### Calendar ########################
  
 
  
  #brks <- quantile(df, probs = seq(.05, .95, .05), na.rm = TRUE)
  #clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
  #  {paste0("rgb(255,", ., ",", ., ")")}
  #datatable(df) %>% formatStyle(names(df), backgroundColor = styleInterval(brks, clrs))
  
#  output$calendar <- DT::renderDT(
#    calendar,
#    caption = "Calendar and visit match",  
    #colnames=c("#", "record id", "message"),
#    extensions = 'Buttons',
#    selection = 'single', ## enable selection of a single row
#    filter = 'top',   
#    options = list(lengthChange = TRUE,dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')))
#  ) 
  
  
  #output$calendar <- DT::renderDT({
  #  DT::datatable(
 #     calendar,
 #     extensions = "Buttons",
 #     filter = 'top',
 #     options = list(
 #       lengthChange = TRUE,
 #       dom = "Bfrtip",
 #       buttons = list(
 #         list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download',
 #              exportOptions = list(modifier = list(search = "applied")))
#        )
#      )
#    )
#  }, server = FALSE)
  
 
  observeEvent(input$all_data, {
    calendar = get_calendar(input$sel_date)
    output$calendar <- DT::renderDT({
      DT::datatable(
        calendar,
        extensions = "Buttons",
        filter = 'top',
        options = list(
          lengthChange = TRUE,
          dom = "Bfrtip",
          buttons = list(
            list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download',
                 exportOptions = list(modifier = list(search = "applied")))
          )
        )
      )
    }, server = FALSE)
  })
  
  observeEvent(input$to_visit, {
    calendar = get_calendar(input$sel_date)
    x = calendar %>% dplyr::filter(id == "calendario" & to_visit_or_done == 1)
    calendar = subset(calendar, calendar$record_id%in%x$record_id) %>% dplyr::arrange(record_id)
    output$calendar <- DT::renderDT({
      DT::datatable(
        calendar,
        extensions = "Buttons",
        filter = 'top',
        options = list(
          lengthChange = TRUE,
          dom = "Bfrtip",
          buttons = list(
            list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download',
                 exportOptions = list(modifier = list(search = "applied")))
          )
        )
      )
    }, server = FALSE)
  })
  
}