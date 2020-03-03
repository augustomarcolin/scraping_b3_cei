read_table_cei <- function(session, instituicao){
  
  banco <- str_split(instituicao, ' - ', simplify = TRUE)
  
  numero <- banco[1]
  
  nome <- banco[2]
  
  #o formulário espera receber a seleção do código númerico do banco
  
  form_dados_preenchidos <- set_values(form_dados[[1]], 
                                       'ctl00$ContentPlaceHolder1$ddlAgentes' = numero)
  
  resultado <- submit_form(session, form_dados_preenchidos)
  
  #caso não tenha ações, a mensagem será exibida
  nao_teve_resultado <- resultado %>%
    read_html() %>% 
    html_text() %>% 
    str_detect('Não foram encontrados resultados para esta pesquisa')
  
  #caso a mensagem não apareça
  if(nao_teve_resultado) {
    
    message('Não foram encontrados resultados para esta pesquisa')
    
  } else{
    
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
    
    dados_compra_venda$nome_banco <- nome #adicionado nome do banco no dataframe
    
    
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
          preco_medio_venda  = as.numeric(str_replace_all(preco_medio_compra, ',', '.'))
        )
    )
    
    dados_preco_medio$nome_banco <- nome  #adicionado nome do banco no dataframe
    
    return(list('dados_compra_venda' = dados_compra_venda, 'dados_preco_medio' = dados_preco_medio))
  }
  
}
