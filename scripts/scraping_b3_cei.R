library(dplyr)
library(rvest)
library(stringr)
library(purrr)

source('scripts/utils.R')
#---- login ----

lnk <- 'https://cei.b3.com.br'

httr::set_config(
  httr::config(
    ssl_verifypeer = 0L
  )
)

session <- html_session(lnk) 

form_login <- html_form(session)

form_preenchido <- set_values(form_login[[1]], 
                              'ctl00$ContentPlaceHolder1$txtLogin' = rstudioapi::showPrompt('CPF', 'Insert your CPF: '),
                              'ctl00$ContentPlaceHolder1$txtSenha' = rstudioapi::askForPassword())

submit_form(session, form_preenchido)

#---- pag negociacao ativos ----

url_dados <- glue::glue('{lnk}/CEI_Responsivo/negociacao-de-ativos.aspx')

pagina_dados <- jump_to(session, url_dados)

form_dados <- html_form(pagina_dados)

#---- corretoras ----

instituicoes <- read_html(pagina_dados) %>% 
  html_nodes('option') %>% 
  html_text() %>% 
  str_subset(., "Selecione", negate = TRUE)

#---- * tabela de compra e venda + preco medio ----

# as datas devem ser nesse formato %d/%m/%Y

dt_ini <- '01/01/2019' 

dt_fim <- '31/12/2019'


db_cei <- map(instituicoes,
              possibly(~read_table_cei(pagina_dados, .x, dt_ini, dt_fim), otherwise = NULL)) %>% 
  set_names(instituicoes)

