read_table_cei <- function(session, instituicao, dt_ini, dt_fim){
  
  banco <- str_split(instituicao, ' - ', simplify = TRUE)
  
  numero_banco <- banco[1]
  
  nome_banco <- banco[2]
  
  # ---- check data inicial ----
  
  dt_minimo <- form_dados[[1]]$fields$`ctl00$ContentPlaceHolder1$txtDataDeBolsa`$value
  
  dt_minimo_format <- as.Date(dt_minimo, format = '%d/%m/%Y')
  
  dt_ini_format <- as.Date(dt_ini, format = '%d/%m/%Y')
  
  if(dt_minimo_format > dt_ini_format)
  {
    dt_ini <- dt_minimo
  }
  
  # ---- form da consulta ----
  
  form_dados_preenchidos <- set_values(
    form_dados[[1]], 
    'ctl00$ContentPlaceHolder1$ddlAgentes'      = numero_banco,
    'ctl00$ContentPlaceHolder1$txtDataDeBolsa'  = dt_ini,
    'ctl00$ContentPlaceHolder1$txtDataAteBolsa' = dt_fim
    )
  
  resultado <- submit_form(session, form_dados_preenchidos)
  
  # ---- excessao sem dados ----
  
  nao_teve_resultado <- resultado %>%
    read_html() %>% 
    html_text() %>% 
    str_detect('Não foram encontrados resultados para esta pesquisa')
  
  
  if(nao_teve_resultado) {
    
    message(paste(nome_banco, 'Não há dados para a instituição', sep = ' - '))
    
  } else{
    
    # ---- compra e venda ----
    
    dados_compra_venda <- resultado %>% 
      html_nodes('#ctl00_ContentPlaceHolder1_rptAgenteBolsa_ctl00_rptContaBolsa_ctl00_pnAtivosNegociados') %>%
      html_node('table') %>%
      html_table() %>% 
      .[[1]] %>% 
      .[-nrow(.),] %>%  #descartando a última linha da tabela por ser um totalizador
      janitor::clean_names() %>% 
      mutate(
        data_do_negocio = as.Date(data_do_negocio, format = '%d/%m/%Y'),
        preco_r         = as.numeric(str_replace_all(preco_r, ',', '.')),
        valor_total_r   = str_remove_all(valor_total_r, '\\.') %>% str_replace_all(., ',', '.') %>% as.numeric()
      ) 
    
    dados_compra_venda$nome_banco_banco <- nome_banco #adicionado nome_banco do banco no dataframe
    
    # ---- preco medio ----
    
    dados_preco_medio <- suppressWarnings(
      resultado  %>% 
        html_nodes('#ctl00_ContentPlaceHolder1_rptAgenteBolsa_ctl00_rptContaBolsa_ctl00_pnResumoNegocios') %>% 
        html_node('table') %>% 
        html_table() %>%
        .[[1]] %>% 
        janitor::clean_names() %>% 
        tidyr::separate(periodo, into = c("inicio", "fim"), sep = ' a ') %>% 
        mutate(
          inicio             = as.Date(inicio, format = '%d/%m/%Y'),
          fim                = as.Date(fim, format = '%d/%m/%Y'),
          preco_medio_compra = as.numeric(str_replace_all(preco_medio_compra, ',', '.')),
          preco_medio_venda  = as.numeric(str_replace_all(preco_medio_venda, ',', '.'))
        )
    )
    
    dados_preco_medio$nome_banco_banco <- nome_banco  #adicionado nome_banco do banco no dataframe
    
    message(paste(nome_banco, "OK", sep = ' - '))
    
    return(list('dados_compra_venda' = dados_compra_venda, 'dados_preco_medio' = dados_preco_medio))
  }
  
}
