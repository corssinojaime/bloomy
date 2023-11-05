library(shiny)
library(dplyr)
library(shinydashboard)
library(magrittr)
library(plotly)
library(shinyauthr)
library(accrualPlot)
library(ggplot2)
library(ggthemes)
library(REDCapR)
library(redcapAPI)
library(shinycssloaders)
#library(flexdashboard)


cookie_expiry <- 7

user_base <- tibble::tibble(
  user = c("user1g", "user2g", "bloomy"),
  password = c("pass1g", "pass2g", "bloomy"),
  password_hash = sapply(c("pass1g", "pass2g", "bloomy"), sodium::password_store),
  permissions = c("admin", "standard", "admin"),
  name = c("User One", "User Two", "User Three")
)


num_fails_to_lockout <- 3


loadData <-function(session){

  token <- "0DA45681F1F7750883C02D6BAF0503E2"
  redcap_api_url <- "https://redcap.manhica.net/api/"
  
  data<- redcap_read(redcap_uri = redcap_api_url, token = token)$data
  #write.csv(x = data, file  = "./bloomy_data.csv")
  
  #data = rio::import("./bloomy_data.csv")
  
 return(data) 
}


process <-function(data){
  
  #data = loadData()
  
  event_names <- c("recrutamento_arm_1", "1a_visita_arm_1", "2a_visita_arm_1","3a_visita_arm_1","4a_visita_arm_1" , "5a_visita_arm_1", "6a_visita_arm_1", "7a_visita_arm_1", "8a_visita_arm_1", "9a_visita_arm_1", "10a_visita_arm_1", "11a_visita_arm_1", "12a_visita_arm_1", "13a_visita_arm_1","visita_nao_program_arm_1")
  
  identification_fields = c("inq_date", "res_name", "res_perm_id", "childs_name", "childs_perm_id", "childs_dob", "childs_age", "age_ins", "gender", "magude_posts", "magude_loc", "mahele_loc", "mapulanguene_loc", "motaze_loc", "panjane_loc", "cell")
  
  recrutamento = data %>% dplyr::filter(grepl("recrut", redcap_event_name)) %>% dplyr::select(record_id, redcap_event_name, identification_fields)
  visit_1 = data %>% dplyr::filter(grepl("^1a_vis", redcap_event_name))%>% dplyr::select(!identification_fields)
  visit_2 = data %>% dplyr::filter(grepl("^2a_vis", redcap_event_name))%>% dplyr::select(!identification_fields)
  visit_3 = data %>% dplyr::filter(grepl("^3a_vis", redcap_event_name))%>% dplyr::select(!identification_fields)
  visit_4 = data %>% dplyr::filter(grepl("^4a_vis", redcap_event_name))%>% dplyr::select(!identification_fields)
  visit_5 = data %>% dplyr::filter(grepl("^5a_vis", redcap_event_name))%>% dplyr::select(!identification_fields)
  visit_6 = data %>% dplyr::filter(grepl("^6a_vis", redcap_event_name))%>% dplyr::select(!identification_fields)
  visit_7 = data %>% dplyr::filter(grepl("^7a_vis", redcap_event_name))%>% dplyr::select(!identification_fields)
  visit_8 = data %>% dplyr::filter(grepl("^8a_vis", redcap_event_name))%>% dplyr::select(!identification_fields)
  visit_9 = data %>% dplyr::filter(grepl("^9a_vis", redcap_event_name))%>% dplyr::select(!identification_fields)
  visit_10 = data %>% dplyr::filter(grepl("^10a_vis", redcap_event_name))%>% dplyr::select(!identification_fields)
  visit_11 = data %>% dplyr::filter(grepl("^11a_vis", redcap_event_name))%>% dplyr::select(!identification_fields)
  visit_12 = data %>% dplyr::filter(grepl("^12a_vis", redcap_event_name))%>% dplyr::select(!identification_fields)
  visit_13 = data %>% dplyr::filter(grepl("^13a_vis", redcap_event_name))%>% dplyr::select(!identification_fields)
  vnp = data %>% dplyr::filter(grepl("^visita_nao", redcap_event_name))%>% dplyr::select(!identification_fields)
  
  rec = recrutamento %>% dplyr::select(record_id, redcap_event_name, inq_date) %>% dplyr::rename(visits_date = "inq_date")
  v1 = visit_1 %>% dplyr::select(record_id, redcap_event_name, visits_date)
  v2 = visit_2 %>% dplyr::select(record_id, redcap_event_name, visits_date)
  v3 = visit_3 %>% dplyr::select(record_id, redcap_event_name, visits_date)
  v4 = visit_4 %>% dplyr::select(record_id, redcap_event_name, visits_date)
  v5 = visit_5 %>% dplyr::select(record_id, redcap_event_name, visits_date)
  v6 = visit_6 %>% dplyr::select(record_id, redcap_event_name, visits_date)
  v7 = visit_7 %>% dplyr::select(record_id, redcap_event_name, visits_date)
  v8 = visit_8 %>% dplyr::select(record_id, redcap_event_name, visits_date)
  v9 = visit_9 %>% dplyr::select(record_id, redcap_event_name, visits_date)
  v10 =visit_10 %>% dplyr::select(record_id, redcap_event_name, visits_date)
  v11 =visit_11 %>% dplyr::select(record_id, redcap_event_name, visits_date)
  v12 =visit_12 %>% dplyr::select(record_id, redcap_event_name, visits_date)
  v13 =visit_13 %>% dplyr::select(record_id, redcap_event_name, visits_date)
  vn = vnp %>% dplyr::select(record_id, redcap_event_name, visits_date)
  
  df = rbind(rec,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,vn)
  df$record_id = gsub("\\s", "", df$record_id)
  df = df[order(df$record_id),]
  
  
  y = df [!is.na(df$visits_date),]
  y = y %>% group_by(record_id) %>% 
    dplyr::arrange(visits_date, .by_group = TRUE) %>% 
    mutate(good_seq = (visits_date > lag(visits_date))*1)
            # Compare the dates
  y = y %>% dplyr::filter(good_seq == 0) %>% dplyr::distinct(record_id, .keep_all = TRUE) %>% dplyr::select(record_id,redcap_event_name, good_seq)
  
  
  #na_vis = df [is.na(df$visits_date),] 
  diff_d = df %>% 
    dplyr::filter(!is.na(visits_date))%>%
    dplyr::group_by(record_id) %>% 
    dplyr::arrange(visits_date, .by_group = TRUE) %>% 
    mutate(duration = (difftime(Sys.Date(), visits_date, units = "days") %>% as.numeric('days'))) %>% 
    dplyr::slice(n()) %>% 
    dplyr::select(record_id, duration)
  
  #Perda de seguimento
  #not_done_1 = data %>% dplyr::filter(visit_notdone_reason == 1) %>% dplyr::select(record_id, redcap_event_name, visits_date)
  #Ausencia do participante (Depois de pelomenos 3 tentativas consecutivas)
  #not_done_2 = data %>% dplyr::filter(visit_notdone_reason == 2) %>% dplyr::select(record_id, redcap_event_name, visits_date)
  #Desistencia
  #not_done_4 = data %>% dplyr::filter(visit_notdone_reason == 4) %>% dplyr::select(record_id, redcap_event_name, visits_date)
  #Outro
  #not_done_5 = data %>% dplyr::filter(visit_notdone_reason == 5) %>% dplyr::select(record_id, redcap_event_name, visits_date)

  
  x = reshape2::dcast(df, record_id~redcap_event_name, value.var = "visits_date") %>% dplyr::select(record_id, event_names)
  x$recrutamento_arm_1 = as.Date(x$recrutamento_arm_1)
  x$`1a_visita_arm_1` = as.Date(x$`1a_visita_arm_1`)
  x$`2a_visita_arm_1` = as.Date(x$`2a_visita_arm_1`)
  x$`3a_visita_arm_1` = as.Date(x$`3a_visita_arm_1`)
  x$`4a_visita_arm_1` = as.Date(x$`4a_visita_arm_1`)
  x$`5a_visita_arm_1` = as.Date(x$`5a_visita_arm_1`)
  x$`6a_visita_arm_1` = as.Date(x$`6a_visita_arm_1`)
  x$`7a_visita_arm_1` = as.Date(x$`7a_visita_arm_1`)
  x$`8a_visita_arm_1` = as.Date(x$`8a_visita_arm_1`)
  x$`9a_visita_arm_1` = as.Date(x$`9a_visita_arm_1`)
  x$`10a_visita_arm_1` = as.Date(x$`10a_visita_arm_1`)
  x$`11a_visita_arm_1` = as.Date(x$`11a_visita_arm_1`)
  x$`12a_visita_arm_1` = as.Date(x$`12a_visita_arm_1`)
  x$`13a_visita_arm_1` = as.Date(x$`13a_visita_arm_1`)
  x$visita_nao_program_arm_1 = as.Date(x$visita_nao_program_arm_1)
  
  
  data = merge(x, y, by = "record_id", all.x = TRUE)
  data = merge(data, diff_d, by = "record_id", all.x = TRUE)
  
  data %<>% dplyr::rename_with(~stringr::str_remove(., '_arm_1'))
  data$pending_vis <- rowSums(is.na(data[3:15]))
  
  return(data)
}

