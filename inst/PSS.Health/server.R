


server <- function(input, output, session) {



  shinyhelper::observe_helpers(withMathJax = TRUE, help_dir = "www")

  #----------------------------.
  #      FUNCOES INTERNAS
  #----------------------------.

  print_r_code <- function(code){
    paste0("</br></br>",
           "<i>Comando R utilizado:</i><br>",
           "<p style=\"font-family:'Courier New';font-size:100% \">", code(code), "</p>",
           "<br><br><b><i>* Sempre procure um profissional de estatística para orientações no planejamento do estudo.</b></i>"
           )
  }


  validate_n <- function(n){
    paste0("validate(
      need(!is.na(", n, "), 'Não foi possível calcular sua solicitação. Verifique as entradas no painel lateral. Se o erro persistir, por favor, envie um e-mail para l-bioestatistica@hcpa.edu.br.'))")
  }

  validate_n_inf <- function(n){
    paste0("validate(
      need(", n, " != Inf, 'Não foi possível calcular sua solicitação. Verifique as entradas no painel lateral. Se o erro persistir, por favor, envie um e-mail para l-bioestatistica@hcpa.edu.br.'))")
  }


  try_n <- function(code){
    tryCatch({
      eval(parse(text = code))
    }, warning = function(warning_condition) {
      NA
    }, error = function(error_condition) {
      NA
    }
    )
  }

  warning_prop <- function(id, entre0e1 = FALSE){

    if(!entre0e1){
      paste0(
        'observeEvent(input$', id,', {
      shinyFeedback::hideFeedback("', id, '")
      if(is.na(input$', id,')){
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = "Deve ser fornecido um valor para o %.",
          color = "red"
        )
      } else if (input$', id,' >= 100) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = "Deve ser menor do que 100%.",
          color = "red"
        )
      } else if (input$', id,' <= 0) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = "Deve ser maior do que 0%.",
          color = "red"
        )
      }
    })
    '
      )
    } else{
      paste0(
        'observeEvent(input$', id,', {
      shinyFeedback::hideFeedback("', id, '")
      if(is.na(input$', id,')){
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = "Deve ser fornecido um valor entre 0 e 1.",
          color = "red"
        )
      } else if (input$', id,' > 1) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = "Deve ser menor do que 1.",
          color = "red"
        )
      } else if (input$', id,' < 0) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = "Deve ser maior do que 0.",
          color = "red"
        )
      }
    })
    '
      )
    }
  }






  warning_numero_positivo <- function(id){
    paste0(
      'observeEvent(input$', id,', {
      shinyFeedback::hideFeedback("', id, '")
      if(is.na(input$', id,')){
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = "Deve ser fornecido um valor.",
          color = "red"
        )
      } else if (input$', id,' <= 0) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = "Deve ser maior do que 0.",
          color = "red"
        )
      }
    })
    '
    )
  }


  warning_inteiro <- function(id){

    paste0(
  'observeEvent(input$', id,', {
    shinyFeedback::hideFeedback("', id,'")

    if(is.na(input$', id,')){
      shinyFeedback::showFeedbackWarning(
        inputId = "', id,'",
        text = "Deve ser um número.",
        color = "red"
      )
    } else if(input$', id,'%%1 != 0 | input$', id,' < 1){
      shinyFeedback::showFeedbackWarning(
        inputId = "', id,'",
        text = "Deve ser um número inteiro positivo.",
        color = "red"
      )
    }
  })'
    )
  }



  warning_perdas <- function(id){
    paste0(
      'observeEvent(input$', id,', {
      shinyFeedback::hideFeedback("', id, '")
      if(is.na(input$', id,')){
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = "Tem certeza que não considerarás perdas?"
        )
      } else if (input$', id,' >= 100) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = "Deve ser menor do que 100%."
        )
      } else if (input$', id,' <= 0) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = "Tem certeza que não considerarás perdas?"
        )
      }
    })
    '
    )
  }

  testar_valor_perdas_valido <- function(x){
    if(!is.na(x)){
      x > 0 & x < 100
    } else{
      FALSE
    }
  }




  # n_cor <- function(r, rho0, sig.level, power, alternative){
  #   tryCatch({
  #     wp.correlation(n = NULL,
  #                    r = r,
  #                    power = power/100,
  #                    p = 0,  # Number of variables to partial out.
  #                    rho0 = rho0,
  #                    alpha = sig.level/100,
  #                    alternative = alternative)$n %>% ceiling() },
  #   warning = function(warning_condition) { NA },
  #   error = function(error_condition) { NA })
  # }
  # n_corV <- Vectorize(n_cor)




  auc_th_n <- function(auc, power, sig.level, kappaa){

    n_p <- pROC::power.roc.test(auc = auc, sig.level = sig.level/100, power = power/100, kappa = kappaa)
    n_casos <- ceiling(n_p$ncases)
    n_control <- ceiling(n_p$ncontrols)

    tibble(n_casos = n_casos, n_control = n_control)
  }



  n_cox_th <- function(power, k, pE, pC, RR, alpha){

    n <- tryCatch({
      powerSurvEpi::ssizeCT.default(k = k,
                                    pE = pE/100,
                                    pC = pC/100,
                                    RR = RR,
                                    alpha = alpha/100,
                                    power = power/100)
    }, warning = function(warning_condition) {
      NA
    }, error = function(error_condition) {
      NA
    }
    )

    if(is.numeric(n)){
      df_ <- data.frame(n_trat = n[1], n_control = n[2])
    } else{
      df_ <- data.frame(n_trat = NA, n_control = NA)
    }

    return(df_)
  }


  n_th2_prop <- function(prop_controle,
                         prop_tratamento,
                         significancia,
                         poder,
                         alternative,
                         ratio_controle_caso,
                         correct,
                         type_info = "both"){


    n <- tryCatch({
      suppressWarnings(
          EnvStats::propTestN(p.or.p1     = prop_tratamento,
                            p0.or.p2    = prop_controle,
                            alpha       = significancia,
                            power       = poder,
                            sample.type = "two.sample",
                            alternative = alternative,
                            ratio       = if(ratio_controle_caso >= 1) ratio_controle_caso else 1/ratio_controle_caso,
                            correct     = correct)
      )
    }, warning = function(warning_condition) {
      NA
      # 1: In EnvStats::propTestN(p.or.p1 = prop_controle, p0.or.p2 = prop_tratamento,  :
      #                             The computed sample sizes 'n1' and 'n2' are too small, relative to the given values of 'p1' and 'p2', for the normal approximation to work well for the following element indices:
      #                             1
    }, error = function(error_condition) {
      NA
    }
    )


    if(sum(is.na(n)) > 0 ) return(NA)

    if(ratio_controle_caso > 1){
      n1 <- n$n1
      n2 <- n$n2
    } else if(ratio_controle_caso == 1){
      n1 <- n
      n2 <- n
    } else{
      n1 <- n$n2
      n2 <- n$n1
    }


    if(type_info == "n1"){
      return(n1)
    } else if(type_info == "n2"){
      return(n2)
    } else if(type_info == "tibble"){
      tibble(n1 = n1, n2 = n2)
    } else{
      return(list(n1 = n1, n2 = n2))
    }

  }

  # n_th2_prop(35/100, 45/100, 5/100, 80/100, "two.sided", 1, TRUE)
  # n_th2_prop(35/100, 45/100, 5/100, 80/100, "two.sided", 2, TRUE)
  # n_th2_prop(35/100, 45/100, 5/100, 80/100, "two.sided", 0.5, TRUE)
  # n_th2_prop(35/100, 45/100, 5/100, 80/100, "two.sided", 2, TRUE, "n2")


  n_est2_prop <- function(prop_controle,
                          prop_tratamento,
                          confianca,
                          precisao,
                          ratio_controle_caso,
                          correct,
                          ci.method){


    n_calc <- tryCatch({
      suppressWarnings(
        EnvStats::ciBinomN(half.width      = precisao/100,
                           p.hat.or.p1.hat = prop_controle/100,
                           p2.hat          = prop_tratamento/100,
                           sample.type     = "two.sample",
                           conf.level      = confianca/100,
                           ratio           = ratio_controle_caso,
                           correct         = correct,
                           ci.method       = ci.method,
                           n.or.n1.max	   = 1E8,
                           warn            = FALSE)
      )
    }, warning = function(warning_condition) {
      NA
    }, error = function(error_condition) {
      NA
    }
    )


    df <- tibble::tibble(`Precisão (%)`    = precisao,
                         `% no Tratamento` = prop_tratamento,
                         prop_tratamento   = prop_tratamento/100,
                         `% Controle`      = prop_controle,
                         `Nível de confiança (%)` = confianca,
                         `Balanço da amostra (Controle/ Tratamento)` = ratio_controle_caso,
                         `Correção de continuidade` = correct,
                         `Método para construir ic` = ci.method)

    if(length(n_calc) == 1 ){
      df %<>%
        dplyr::bind_cols(tibble::tibble( `n Controle` = NA, `n Tratamento` = NA))
    } else{
      df %<>%
        dplyr::bind_cols(tibble::tibble( `n Controle` = n_calc$n1, `n Tratamento` = n_calc$n2))
    }

    return(mutate(df, `n total` = `n Tratamento` + `n Controle`))

  }



  n_icc_th <- function(p, p0, k, alpha, power, tails){

    n <- tryCatch({
      ICC.Sample.Size::calculateIccSampleSize(
        p = p,
        p0 = p0,
        k = k,
        alpha = alpha/100,
        power = power/100,
        tails = tails)

    }, warning = function(warning_condition) {
      NA
    }, error = function(error_condition) {
      NA
    }
    )

    if(is.list(n)){
      df_ <- data.frame(n = n[[1]]$N)
    } else{
      df_ <- data.frame(n = NA)
    }

    return(df_)
  }




  #---------------------------------------.
  #  Calcula n para perdas e recusas
  #---------------------------------------.

  n_perdas <- function(n, perdas){
    ceiling(n/(1 - perdas/100))
  }



  #---------------------------------------------------------.
  # Tamanho de efeito para comparar dus medias (d de Cohen)
  #---------------------------------------------------------.

  cohen_d <- function(mean_diff, n_1, n_2, sd_1, sd_2){
    pooled_sd_n <- ((n_1-1)*(sd_1**2)) + ((n_2-1)*(sd_2**2))
    pooled_sd_d <- ((n_1 + n_2) - 2)
    pooled_sd <- sqrt(pooled_sd_n/pooled_sd_d)
    mean_diff / pooled_sd
  }



  #---------------------------------------------------.
  # Sample size to estimate a  correlation
  #---------------------------------------------------.

  n_est_corr <- function(conf, cor, w, method) {

    n <- presize::prec_cor(r = cor,
                           conf.width = w,
                           conf.level = conf/100,
                           method = method)

    return(n$n)
  }








  #--------------------------------------------------------------.
  # Sample size to estimate a Cronbach alpha reliability
  #--------------------------------------------------------------.

  size.ci.cron1 <- function(alpha, k, rel, w) {
    # Computes sample size required to estimate a Cronbach
    # alpha reliability with desired precision
    # Arguments:
    #   alpha: alpha value for 1-alpha confidence
    #   k:     number of measurements
    #   rel:   reliability planning value
    #   w:     desired CI width
    # Returns:
    #   required sample size
    z <- qnorm(1 - alpha/2)
    n0 <- ceiling((8*k/(k - 1))*(1 - rel)^2*(z/w)^2 + 2)
    b <- log(n0/(n0 - 1))
    ll <- 1 - exp(log(1 - rel) - b + z*sqrt(2*k/((k - 1)*(n0 - 2))))
    ul <- 1 - exp(log(1 - rel) - b - z*sqrt(2*k/((k - 1)*(n0 - 2))))
    w0 <- ul - ll
    n <- ceiling((n0 - 2)*(w0/w)^2 + 2)
    return (n)
  }



  text_input_to_vector <- function(input_text){

    input_text %>%
      strsplit(",") %>%
      unlist() %>%
      strsplit("\\n") %>%
      unlist() %>%
      as.numeric() %>%
      na.omit()
  }


  check_text_input_to_vector <- function(id){

    paste0("observeEvent(input$", id, ", {

    vetor_teste <- text_input_to_vector(input$", id, ")

    if(length(vetor_teste) == 0){
      shinyFeedback::showFeedbackWarning(
        inputId = '", id, "',
        text = 'Entrada inválida.',
        color = 'red')
    } else {
      shinyFeedback::hideFeedback('", id, "')
    }
  })
  "
    )
  }








  #...........................-----
  #  1  media  ----

  # Estimar ----


  # Ui inputs

  output$mean_um <- renderUI({
    textInput(inputId = "mean_unidade_medida",
              label   = paste("Descreva a unidade de medida de", input$mean_nome_desfecho),
              value   = "u.m.") %>%
      .help_buttom(body = .txt_um,
                  title = paste("Unidade de medida de", input$mean_nome_desfecho))

  })



  # aas
  output$mean_sd <- renderUI({
    req(input$mean_delineamento == "aas")

    numericInput( "s_mean",
                  paste0("Desvio padrão esperado de ", input$mean_nome_desfecho, " (em ", input$mean_unidade_medida, ")"),
                  value = 24,
                  min   = 0,
                  max   = Inf,
                  step  = 1) %>%
      .help_buttom(body = .txt_dp,
                  title = "Desvio padrão esperado")
  })


  observeEvent(input$s_mean, {
    if(is.na(input$s_mean)){
      shinyFeedback::showFeedbackWarning(
        inputId = "s_mean",
        text = "Deve ser fornecido um valor de desvio padrão.",
        color = "red"
      )
    } else if (input$s_mean<= 0) {
      shinyFeedback::showFeedbackWarning(
        inputId = "s_mean",
        text = "O desvio padrão deve ser maior do que zero.",
        color = "red"
      )
    } else {
      shinyFeedback::hideFeedback("s_mean")
    }
  })





  # ac1

  output$mean_ac1Ui <- renderUI({
    req(input$mean_delineamento == "ac1")


    fluidPage(fluidRow(


      numericInput( "mean_ac1_ac",
                    "Número total de conglomerados",
                    value = 5,
                    min   = 1,
                    max   = Inf,
                    step  = 1) %>%
        .help_buttom(body = paste0("Número total de conglomerados", .txt_definido_pesquisador),
                    title = "Número total de conglomerados"),

      numericInput( "mean_ac1_xbar",
                    paste0("Média esperada de ", input$mean_nome_desfecho, " (em ", input$mean_unidade_medida, ")"),
                    value = 34,
                    min   = -Inf,
                    max   = Inf,
                    step  = 1),

      numericInput( "mean_ac1_sd",
                    paste0("Desvio padrão esperado de ", input$mean_nome_desfecho, " (em ", input$mean_unidade_medida, ")"),
                    value = 5.5,
                    min   = 0,
                    max   = Inf,
                    step  = 1) %>%
        .help_buttom(body = .txt_dp, title = "Desvio padrão esperado"),


      numericInput( "mean_ac1_b",
                    "Média do número de indivíduos em cada conglomerado",
                    value = 25,
                    min   = 1,
                    max   = Inf,
                    step  = 1) %>%
        .help_buttom(body = paste0("Média do número de indivíduos em cada conglomerado", .txt_definido_pesquisador_OU_literatura),
                    title = "Média do número de indivíduos em cada conglomerado"),

      numericInput( "mean_ac1_b_sd",
                    "Desvio padrão do número de indivíduos em cada conglomerado",
                    value = 0,
                    min   = 0,
                    max   = Inf,
                    step  = 1) %>%
        .help_buttom(body = paste0("Desvio padrão do tamanho de cada conglomerado", .txt_definido_literatura),
                    title = "Desvio padrão do tamanho de cada conglomerado"),

      numericInput( "mean_ac1_rho",
                    "Coeficiente de correlação intra conglomerados",
                    value = 0.1,
                    min   = 0,
                    max   = 1,
                    step  = .1) %>%
        .help_buttom(body = paste0("Coeficiente de correlação intra conglomerados", .txt_definido_literatura),
                    title = "Coeficiente de correlação intra conglomerados"),


    ))

  })


   # aae
  output$mean_aae_ui <- renderUI({
    req(input$mean_delineamento == "aae")

    fluidPage(fluidRow(

      numericInput(inputId = "mean_aee_E",
                   label   = "Número de estratos",
                   value   = 3,
                   min     = 2,
                   max     = Inf,
                   step    = 1),

      textInput( "mean_aae_N_grupo",
                 "Tamanho populacional de cada estrato",
                 value = "600, 500, 400"),

      textInput( "mean_aae_xbar_grupo",
                 paste0("Médias esperadas de ", input$mean_nome_desfecho, " (em ", input$mean_unidade_medida, ") em cada estrato"),
                 value = "0.164, 0.166, 0.236"),

      textInput( "mean_aae_sigma_grupo",
                 paste0("Desvios padrões esperados de ", input$mean_nome_desfecho, " (em ", input$mean_unidade_medida, ") em cada estrato"),
                 value = "0.245, 0.296, 0.436"
      )
    ))
  })

  mean_aae_inputs <- reactive({
    req(input$mean_delineamento == "aae")

    req(!is.null(input$mean_aae_N_grupo))
    req(!is.null(input$mean_aae_xbar_grupo))
    req(!is.null(input$mean_aae_sigma_grupo))

    N <- text_input_to_vector(input$mean_aae_N_grupo) %>%
      keep(~.x > 1)

    medias <- text_input_to_vector(input$mean_aae_xbar_grupo)

    desvio <- text_input_to_vector(input$mean_aae_sigma_grupo) %>%
      keep(~.x > 0)

    list(N = N, medias = medias, desvio = desvio)
  })


  observeEvent(c(input$mean_aee_E, input$mean_aae_N_grupo, input$mean_aae_xbar_grupo, input$mean_aae_sigma_grupo), {

    if(is.na(input$mean_aee_E)){
      shinyFeedback::showFeedbackWarning(
        inputId = "mean_aee_E",
        text = "Deve ser fornecido valores",
        color = "red")

    } else {
      shinyFeedback::hideFeedback("mean_aee_E")

      if(all(is.na(mean_aae_inputs()$N))){
        shinyFeedback::showFeedbackWarning(
          inputId = "mean_aae_N_grupo",
          text = "Deve ser fornecido valores",
          color = "red")

      } else if (length(mean_aae_inputs()$N)  != input$mean_aee_E) {
        shinyFeedback::showFeedbackWarning(
          inputId = "mean_aae_N_grupo",
          text = paste0("Deve ser menor fornecido ", input$mean_aee_E, " valores válidos."),
          color = "red")

      } else if (any(mean_aae_inputs()$N <= 0) | any(mean_aae_inputs()$N%%1 != 0) | any(mean_aae_inputs()$N < 2) ) {
        shinyFeedback::showFeedbackWarning(
          inputId = "mean_aae_N_grupo",
          text = "Deve ser menor fornecido apenas valores positivos inteiros maiores do que 1.",
          color = "red")

      }else { shinyFeedback::hideFeedback("mean_aae_N_grupo")}





      if(all(is.na(mean_aae_inputs()$medias))){
        shinyFeedback::showFeedbackWarning(
          inputId = "mean_aae_xbar_grupo",
          text = "Deve ser fornecido valores.",
          color = "red")

      } else if (length(mean_aae_inputs()$medias) != input$mean_aee_E) {
        shinyFeedback::showFeedbackWarning(
          inputId = "mean_aae_xbar_grupo",
          text = paste0("Deve ser menor fornecido ", input$mean_aee_E, " valores válidos."),
          color = "red")

      } else { shinyFeedback::hideFeedback("mean_aae_xbar_grupo")}





      if(all(is.na(mean_aae_inputs()$desvio))){
        shinyFeedback::showFeedbackWarning(
          inputId = "mean_aae_sigma_grupo",
          text = "Deve ser fornecido valores.",
          color = "red")

      } else if (length(mean_aae_inputs()$desvio) != input$mean_aee_E) {
        shinyFeedback::showFeedbackWarning(
          inputId = "mean_aae_sigma_grupo",
          text = paste0("Deve ser menor fornecido ", input$mean_aee_E, " valores válidos."),
          color = "red")

      } else if (any(mean_aae_inputs()$desvio <= 0)) {
        shinyFeedback::showFeedbackWarning(
          inputId = "mean_aae_sigma_grupo",
          text = "Deve ser menor fornecido apenas valores positivos.",
          color = "red")

      } else { shinyFeedback::hideFeedback("mean_aae_sigma_grupo")}
    }

 })









  output$e_meanUi <- renderUI({
    numericInput( "e_mean",
                  label = case_when(input$mean_delineamento == "aas" ~ paste0("Margem de erro/ semi-amplitude absoluta (em ", input$mean_unidade_medida, ")"),
                                    input$mean_delineamento == "aae" ~ "Diferença relativa (%) máxima entre a estimativa e o valor desconhecido da população",
                                    input$mean_delineamento == "ac1" ~ "Diferença relativa (%) máxima entre a estimativa e o valor desconhecido da população"
                  ),

                  value = case_when(input$mean_delineamento == "aas" ~ 5,
                                    input$mean_delineamento == "aae" ~ 20,
                                    input$mean_delineamento == "ac1" ~ 10
                                    ),
                  min   = 0,
                  max   = Inf,
                  step  = 1
    ) %>% .help_buttom(body =
                        if(input$mean_delineamento == "aas"){
                          .txt_precisao
                        } else{
                          paste0("Diferença relativa (%) máxima entre a estimativa e o valor desconhecido da população.", .txt_definido_literatura)
                        },
                      title = if(input$mean_delineamento == "aas"){
                        "Margem de erro ou semi-amplitude"
                      } else{
                        "Diferença relativa (%)"
                      })
  })


  observeEvent(input$e_mean, {
    if(is.na(input$e_mean)){
      shinyFeedback::showFeedbackWarning(
        inputId = "e_mean",
        text = "Deve ser fornecido um valor de margem de erro.",
        color = "red"
      )
    } else if (input$e_mean<= 0) {
      shinyFeedback::showFeedbackWarning(
        inputId = "e_mean",
        text = "A margem de erro deve ser maior do que zero.",
        color = "red"
      )
    } else {
      shinyFeedback::hideFeedback("e_mean")
    }
  })



  observeEvent(input$conf_mean, {
    if(is.na(input$conf_mean)){
      shinyFeedback::showFeedbackWarning(
        inputId = "conf_mean",
        text = "Deve ser fornecido um valor do nível de significância.",
        color = "red"
      )
    } else if (input$conf_mean >= 100) {
      shinyFeedback::showFeedbackWarning(
        inputId = "conf_mean",
        text = "Nível de significância deve ser menor do que 100%.",
        color = "red"
      )
    } else if (input$conf_mean <= 0) {
      shinyFeedback::showFeedbackWarning(
        inputId = "conf_mean",
        text = "Nível de significância deve ser maior do que 0%.",
        color = "red"
      )
    } else {
      shinyFeedback::hideFeedback("conf_mean")
    }
  })



  eval(parse(text = warning_perdas("mean_perdas_recusa")))



  # Output do tamanho amostral

  output$mean <- renderText({

    req(!(is.null(input$s_mean) | is.null(input$e_mean) | is.null(input$conf_mean)))

    validate(
      need(!is.na(input$s_mean),    "É obrigatório fornecer um valor do desvio padrão."),
      # need(!is.na(input$e_mean),    "É obrigatório fornecer um valor da precisão."),
      need(!is.na(input$conf_mean), "É obrigatório fornecer um valor do nível de confiança."),

      need(input$s_mean > 0,    "O desvio padrão deve ser maior do que zero."),
      # need(input$e_mean > 0,    "A precisão deve ser maior do que zero."),
      need(input$conf_mean > 0, "O nível de confiança deve ser maior do que 0%."),
      need(input$conf_mean < 100, "O nível de confiança deve ser menor do que 100%.")

    )


    mean_unidade_medida <- ifelse(is.null(input$mean_unidade_medida), "u.m", input$mean_unidade_medida)



    if(input$mean_delineamento == "aas"){

      code <- paste0(
        "presize::prec_mean(",
        "mu = 0, ",
        "sd = ", input$s_mean, ", ",
        "conf.width = ", input$e_mean, "*2, ",
        "conf.level = ", input$conf_mean, "/100)"
      )


      n <- try_n(code)
      eval(parse(text = validate_n("n")))
      n <- ceiling(n$n)
      eval(parse(text = validate_n_inf("n")))

      paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
             "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

             "O cálculo do tamanho de amostra foi realizado por meio da ", .txt_citacao_tap, ". ",
             "Para estimar a média do <b>", input$mean_nome_desfecho,
             "</b> com margem de erro absoluta de <b>", input$e_mean, " ", mean_unidade_medida,
             "</b> e nível de confiança de <b>", input$conf_mean, "%</b>. ",
             "Considerando desvio padrão esperado do <b>", input$mean_nome_desfecho, "</b> de <b>", input$s_mean, " ", mean_unidade_medida, "</b> como referido em Fulano (1900), ",
             "chegou-se ao tamanho de amostra de <b>", n, "</b> sujeitos. ",

             if(testar_valor_perdas_valido(input$mean_perdas_recusa)){
               paste0(
                 "Acrescentando <b>", input$mean_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>",
                 n_perdas(n, input$mean_perdas_recusa), "</b>.")
             },
             .txt_referencia_tap, print_r_code(code))




      # Amostragem estratificada
    } else if(input$mean_delineamento == "aae"){


      validate(need(all(sapply(mean_aae_inputs(),length) == input$mean_aee_E),
                    'Não foi possível calcular sua solicitação. Verifique as entradas no painel lateral. Se o erro persistir, por favor, envie um e-mail para l-bioestatistica@hcpa.edu.br.'))


      code <- paste0(
        "epiR::epi.ssstrataestc(",
        "strata.n = c(", paste(mean_aae_inputs()$N, collapse = ", "), "), ",
        "strata.xbar = c(", paste(mean_aae_inputs()$medias, collapse = ", "), "), ",
        "strata.sigma = c(", paste(mean_aae_inputs()$desvio, collapse = ", "), "), ",
        "epsilon.r = ", input$e_mean, "/100, ",
        "conf.level = ", input$conf_mean, "/100)"
      )


      n <- try_n(code)
      eval(parse(text = validate_n("n")))


      paste0("<b><font size = '5'>Tamanho amostral calculado: ", n$total.sample,
             "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

             "O cálculo do tamanho de amostra foi realizado por meio da ", .txt_citacao_tap, ". ",
             "Para estimar a média do <b>", input$mean_nome_desfecho,
             "</b> com margem de erro relativa entre a estimativa e o valor desconhecido da população de <b>", input$e_mean,
             "%</b> e nível de confiança de <b>", input$conf_mean, "%</b>. ",
             "Considerando um processo de amostragem estratificada simples nos estratos ",
             paste(LETTERS[1:input$mean_aee_E], collapse = ", ") %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish(),
             ", com tamanhos populacionais de <b>",
             paste(mean_aae_inputs()$N, collapse = ", ") %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish(),
             "</b> indivíduos, médias esperadas de <b>",
             paste(mean_aae_inputs()$medias, collapse = ", ") %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish(),
             " ",
             mean_unidade_medida,
             "</b> e desvios padrões esperados de <b>",
             paste(mean_aae_inputs()$desvio, collapse = ", ") %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish(),
             " ",
             mean_unidade_medida,
             "</b>, respectivamente, chegou-se ao tamanho de amostra total de <b>", n$total.sample, "</b> sujeitos, sendo <b>",

             paste0(n$strata.sample, " no estrato ", LETTERS[1:input$mean_aee_E], collapse = ", ") %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish(),

             "</b>. ",

             if(testar_valor_perdas_valido(input$mean_perdas_recusa)){
               paste0("Acrescentando <b>", input$mean_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser ",
                      paste0(n_perdas(n$strata.sample, input$mean_perdas_recusa), " no estrato ", LETTERS[1:input$mean_aee_E], collapse = ", ") %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish(), ".")
             },
             .txt_referencia_tap, print_r_code(code))



      # Amostragem por conglomerados em um estagio
    } else if(input$mean_delineamento == "ac1"){


      code <- paste0(
        "epiR::epi.ssclus1estc(",
        "b = c(", input$mean_ac1_b, ", ", input$mean_ac1_b_sd, "), ",
        "N = ", input$mean_ac1_ac, "*", input$mean_ac1_b,", ",
        "xbar = ", input$mean_ac1_xbar, ", ",
        "xsigma = ", input$mean_ac1_sd, ", ",
        "epsilon.r = ", input$e_mean, "/100, ",
        "rho = ", input$mean_ac1_rho, ", ",
        "conf.level = ", input$conf_mean, "/100)"
      )


      n <- try_n(code)
      eval(parse(text = validate_n("n")))
      nc <- n$n.psu

      eval(parse(text = validate_n_inf("nc")))

      # print_r_code(code)
      paste0("<b><font size = '5'>Tamanho amostral calculado: <i>", nc, " conglomerados</i>",
             "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

             "O cálculo do tamanho de amostra foi realizado por meio da ", .txt_citacao_tap, ". ",
             "Para estimar a média do <b>", input$mean_nome_desfecho,
             "</b> com margem de erro relativa entre a estimativa e o valor desconhecido da população de <b>", input$e_mean,
             "%</b> e nível de confiança de <b>", input$conf_mean, "%</b>. ",

             "Considerando um processo de amostragem aleatória por conglomerados em um único estágio, ",
             "de uma população dividida em <b>", input$mean_ac1_ac, "</b> conglomerados, ",
             "na qual o número de indivíduos em cada conglomerado é em média de <b>", input$mean_ac1_b, "</b> ",
             if(input$mean_ac1_b_sd != 0){
               paste0("com desvio padrão de <b>", input$mean_ac1_b_sd, "</b> ")
             },
             "indivíduos; ",

             "média de ", input$mean_nome_desfecho, " esperada de <b>", input$mean_ac1_xbar, " ", mean_unidade_medida, "</b> e ",
             "desvio padrão esperado de <b>", input$mean_ac1_sd, " ", mean_unidade_medida, "</b>; ",

             "coeficiente de correlação intra conglomerados de <b>", input$mean_ac1_rho, "</b>, como referido em Fulano (1900), ",

             "será necessário amostrar <b>", nc, "</b> conglomerados. ",
             .txt_referencia_tap, print_r_code(code))
    }


    })



  ## Cenarios ----

  output$cenarios_uma_media_estUi <- renderUI({

    conditionalPanel("input.mean_delineamento == 'aas'",

                     br(),
                     HTML('<hr style="color: black;">'),
                     br(),br(),

                     titlePanel("Construção de cenários"),
                     br(),

                     wellPanel("Utilize os argumentos abaixo para construir diferentes cenários.
                                        Você pode definir um intervalo de margem de erro e especificar diferentes valores para o desvio padrão.
                                        Demais informações serão recuperadas do painel lateral."),

                     HTML("<b>Defina a sequência de valores para a margem de erro absoluta:</b>"),
                     br(),
                     div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
                         numericInput("mean_from", "Mínimo", value = input$e_mean, step = 1)
                     ),
                     div(style="display: inline-block;vertical-align:top; width: 80px;",
                         numericInput("mean_to", "Máximo", value = input$e_mean + 2, step = 1)
                     ),
                     div(style="display: inline-block;vertical-align:top; width: 80px;",
                         numericInput("mean_by", "Intervalo", value = 0.5, min = 0, step = 0.5) %>%
                           .help_buttom(body = "Defina a sequência de margem de erro absoluta. Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                                       title = "Sequência da margem de erro absoluta")
                     ),

                     fluidRow(
                       column(6,
                              textInput(inputId = "mean_sd_plot",
                                        label   = "Digite valores de desvio padrão (DP) para fazer o gráfico:",
                                        value   = paste0(c(input$s_mean, input$s_mean + 0.2, input$s_mean + 0.5), collapse = ", "),
                                        width   = "100%") %>%
                                .help_buttom(body = "Defina os valores de desvio padrão.
                                                      Esses valores serão utilizados para criar diferentes linhas no gráfico.
                                                      Separe os valores por vírgula ',' e utilize ponto '.' como decimal.")
                       )
                     ),

                     shinycssloaders::withSpinner(plotly::plotlyOutput("mean_plot", width = "80%"), type = 5),
                     br(), br(),
                     downloadButton("download_mean_tab","Download tabela"),
                     shinycssloaders::withSpinner(DT::dataTableOutput("mean_tab", width = "100%"), type = 5)

    ) # Fecha conditionalPanel aas

  })


  eval(parse(text = check_text_input_to_vector("mean_sd_plot")))

  tab_mean_cenarios <- reactive({

    desvios_plot <- text_input_to_vector(input$mean_sd_plot)

    req(length(desvios_plot) > 0)

    expand.grid(`Margem de erro`      = seq(from = input$mean_from, to = input$mean_to, by = input$mean_by),
                `Desvio padrão` = desvios_plot,
                `Nível de confiança (%)` = input$conf_mean) %>%
      mutate(`Tamanho da amostra` = mapply(
        function(sd, conf.width, conf.level){ presize::prec_mean(mu = 0, sd = sd, conf.width = conf.width*2, conf.level = conf.level/100)$n },
        `Desvio padrão`, `Margem de erro`, `Nível de confiança (%)`),
        `Tamanho da amostra`   = ceiling(`Tamanho da amostra`),
        `% de perdas/ recusas` = input$mean_perdas_recusa,
        `n + perdas/ recusas`  = n_perdas(`Tamanho da amostra`, input$mean_perdas_recusa))
  })



  output$mean_plot <- plotly::renderPlotly({

    req(!(is.null(input$mean_from) | is.null(input$mean_to) | is.null(input$mean_by) | is.null(input$conf_mean) | is.null(input$s_mean)))
    req(!(is.na(input$mean_from) | is.na(input$mean_to) | is.na(input$mean_by) | is.na(input$conf_mean)))

    validate(need(input$conf_mean > 0, "O nível de confiança deve ser maior do que zero."))


    g1 <- tab_mean_cenarios() %>%
      mutate(DP = factor(`Desvio padrão`)) %>%
      ggplot(aes(x = `Margem de erro`, y = `Tamanho da amostra`, color = DP))+
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = seq(from = input$mean_from, to = input$mean_to, by = input$mean_by)) +
      xlab("Margem de erro absoluta") +
      ylab("Tamanho da amostra*") +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(palette = "Set1")

    plotly::ggplotly(g1, tooltip = c("x", "colour", "y")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                showarrow = F, xref='paper', yref='paper',
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=10)))
  })



  output$mean_tab <- DT::renderDataTable({

    req(!(is.null(input$mean_from) | is.null(input$mean_to) | is.null(input$mean_by) | is.null(input$conf_mean)))
    req(!(is.na(input$mean_from) | is.na(input$mean_to) | is.na(input$mean_by) | is.na(input$conf_mean)))
    req(input$conf_mean > 0)


    tab_mean_cenarios() %>%
      DT::datatable(extensions = c('FixedColumns'),
                rownames   = FALSE,
                filter     = "none",
                options    = list(pageLength = 10,
                                  scrollX = TRUE,
                                  scrollY = TRUE,
                                  searching = FALSE,
                                  fixedColumns = list(leftColumns = 1),
                                  dom = 'B<"dwnld">frtip'))
  })


  output$download_mean_tab <- downloadHandler(
    filename = function() { "Cenarios_tamanho_amostra_uma_media.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_mean_cenarios(), path = file)}
  )









  # Testar ------


  output$th_mean_formula1 <- renderUI({
    withMathJax(
      paste0("$$H_0: \\mu", "=", input$margin_TH_mean,
             "\\text{  vs  }",
             "H_1: \\mu", "\\neq", input$margin_TH_mean,
             "$$"))
  })


  eval(parse(text = warning_prop("alpha_TH_mean")))
  eval(parse(text = warning_prop("beta_TH_mean")))
  eval(parse(text = warning_numero_positivo("sigma_TH_mean")))
  eval(parse(text = warning_perdas("mean_TH_recusa")))

  observeEvent(input$media_TH_mean, {
    if(is.na(input$media_TH_mean)){
      shinyFeedback::showFeedbackWarning(
        inputId = "media_TH_mean",
        text = "Deve ser fornecido um valor.",
        color = "red")
    } else {
      shinyFeedback::hideFeedback("media_TH_mean")
    }
  })

  observeEvent(input$margin_TH_mean, {
    if(is.na(input$margin_TH_mean)){
      shinyFeedback::showFeedbackWarning(
        inputId = "margin_TH_mean",
        text = "Deve ser fornecido um valor.",
        color = "red")
    } else {
      shinyFeedback::hideFeedback("margin_TH_mean")
    }
  })




  output$THmean <- renderText({

    # code <- paste0(
    #   "TrialSize::OneSampleMean.Equality(",
    #   "alpha = ", input$alpha_TH_mean, "/100,",
    #   "beta = 1 - ", input$beta_TH_mean, "/100, ",
    #   "sigma = ", input$sigma_TH_mean, ", ",
    #   "margin = ", input$media_TH_mean - input$margin_TH_mean, ")"
    # )

    code <- paste0(
      "pwr::pwr.t.test(n = NULL, ",
      "d = ", input$media_TH_mean - input$margin_TH_mean, "/ ", input$sigma_TH_mean, ", ",
      "power = ", input$beta_TH_mean, "/100, ",
      "type = 'one.sample')"
    )

    n <- try_n(code)
    eval(parse(text = validate_n("n")))
    n <- ceiling(n$n)
    eval(parse(text = validate_n_inf("n")))



    paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
           "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

           "O cálculo do tamanho de amostra foi realizado por meio da ", .txt_citacao_tap, ", ",
           "para testar se a média de <b>", "Y", "</b> é diferente de <b>", input$margin_TH_mean, " u.m.</b>. ",
           "Considerando poder de <b>", input$beta_TH_mean,
           "%</b>, nível de significância de <b>", input$alpha_TH_mean, "%</b>, ",
           "média esperada de <b>", input$media_TH_mean, " u.m.</b> e ",
           "desvio padrão esperado de <b>", input$sigma_TH_mean, " u.m.</b> como referido em Fulano (1900), ",
           "chegou-se ao tamanho de amostra de <b>", n, "</b> sujeitos. ",
           if(testar_valor_perdas_valido(input$mean_TH_recusa)){
             paste0(
               "Acrescentando <b>", input$mean_TH_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>",
               n_perdas(n, input$mean_TH_recusa), "</b>.")
           },
           .txt_referencia_tap, print_r_code(code)
    )

  })






  # Poder -----

  eval(parse(text = warning_prop("mean1_power_sig")))
  eval(parse(text = warning_numero_positivo("mean1_power_sigma")))
  eval(parse(text = warning_numero_positivo("mean1_power_n")))


  observeEvent(input$mean1_power_diff, {
    if(is.na(input$mean1_power_diff)){
      shinyFeedback::showFeedbackWarning(
        inputId = "mean1_power_diff",
        text = "Deve ser fornecido um valor.",
        color = "red")
    } else {
      shinyFeedback::hideFeedback("mean1_power_diff")
    }
  })

  output$poder_TH1_mean <- renderText ({

    code <- paste0("stats::power.t.test(",
                   "n = ", input$mean1_power_n, ", ",
                   "delta = ", input$mean1_power_diff, ", ",
                   "sd = ", input$mean1_power_sigma, ", ",
                   "type = 'one.sample', ",
                   "sig.level = ", input$mean1_power_sig,  "/100, ",
                   "power = NULL, ",
                   "alternative = 'two.sided')")

    power_pwd <- eval(parse(text = code))

    paste0("<b><font size = '5'>Poder calculado: ",round(100*power_pwd$power, digits = 3),
           "%</font></b></br></br><i>Sugestão de texto:</i></br></br>",

           "Foi calculado o poder para testar uma média utilizando a ", .txt_citacao_tap, ". ",

           "Considerando um nível de significância de <b>", input$mean1_power_sig, "%</b>, ",
           "diferença a ser detectada de <b>", input$mean1_power_diff, " u.m.</b>, ",
           "desvio padrão das diferenças de <b>", input$mean1_power_sigma, " u.m.</b> ",
           "e um tamanho de amostra de <b>", input$mean1_power_n, "</b> sujeitos, ",
           "chegou-se à um poder de <b>", round(100*power_pwd$power, digits = 1), "%</b>.",

           .txt_referencia_tap, print_r_code(code))

  })





  #_____________----
  #  1 proporção  ----

  # Estimar ----

  eval(parse(text = warning_prop("e_prop")))
  eval(parse(text = warning_prop("p_prop")))
  eval(parse(text = warning_prop("conf_prop")))
  eval(parse(text = warning_numero_positivo("N_pop_prop")))
  eval(parse(text = warning_perdas("prop_perdas_recusa")))



  output$prop <- renderText({

    code <- paste0(
      "presize::prec_prop(",
      "p = ", input$p_prop, "/100, ",
      "conf.width = ", input$e_prop, "/100,",
      "conf.level = ", input$conf_prop, "/100, ",
      "method = '", input$p1_metodo, "')"
    )


    n <- try_n(code)
    eval(parse(text = validate_n("n")))
    n <- ceiling(n$n)
    eval(parse(text = validate_n_inf("n")))


    # if(input$n_size_prop == TRUE){
    #   paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
    #          "</font></b></br></br><i>Sugestão de texto:</i></br></br>",
    #
    #          "Para o cálculo do tamanho de amostra foi utilizado a ", .txt_citacao_tap, ". ",
    #          "Considerando o tamanho da população igual a <b>", input$N_pop_prop, "</b>, um nível de confiança de <b>", input$conf_prop, "%</b>, ",
    #          "amplitude desejada para o intervalo de confiança de <b>", input$e_prop, "%</b> ",
    #          "%</b> utilizando o método de ", str_to_title(input$p1_metodo),
    #          " e proporção esperada de <b>", input$prop_nome_desfecho, "</b> de <b>", input$p_prop, "%</b> como é referida em Fulano (1900), ",
    #          "chegou-se ao tamanho de amostra de <b>", n, "</b> sujeitos. ",
    #          "Acrescentando <b>", input$prop_perdas_recusa, "</b>% para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas(n, input$prop_perdas_recusa), "</b>.",
    #
    #          .txt_referencia_tap, print_r_code(code)
    #   )
    #
    # } else {
      paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
             "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

             "Foi calculado o tamanho de amostra para estimar o <b>", input$prop_nome_desfecho,
             "</b> com uma amplitude máxima para o intervalo de confiança de <b>", input$e_prop, "%</b>, ",
             "utilizando a ", .txt_citacao_tap, ". ",
             "Considerando nível de confiança de <b>", input$conf_prop, "%</b>, ",
             " método de ", str_to_title(input$p1_metodo), " para estimar o intervalo de confiança",
             " e proporção esperada de <b>", input$prop_nome_desfecho, "</b> de <b>", input$p_prop, "%</b> como é referida em Fulano (1900), ",
             "chegou-se ao tamanho de amostra de <b>", n, "</b> sujeitos. ",
             if(testar_valor_perdas_valido(input$prop_perdas_recusa)){
               paste0(
                 "Acrescentando <b>", input$prop_perdas_recusa, "</b>% para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas(n, input$prop_perdas_recusa), "</b>.")
             },
             .txt_referencia_tap, print_r_code(code)
      )
    # }
  })


  ## Cenarios ----

  output$prop_precisao_cenarioUi <- renderUI({
    fluidPage(fluidRow(
      br(),
      HTML('<hr style="color: black;">'),
      br(),br(),

      titlePanel("Construção de cenários"),
      br(),

      wellPanel("Utilize os argumentos abaixo para construir diferentes cenários.
      Você pode especificar uma sequência de valores para a proporção esperada e valores de precisão desejada. Demais informações serão recuperadas do painel lateral."),

      HTML("<b>Defina a sequência de valores para a proporção (%) esperada:</b>"),
      # bsplus::shiny_iconlink(name = "question-circle") %>%
      # bsplus::bs_embed_popover(title = "Defina a sequência de proporção Essa sequência será utilizada para compor o eixo x do gráfico",
      #                          placement = "left"),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
          numericInput("prop_from", "Mínimo", value = 0, step = 5, min = 0, max = 100)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("prop_to", "Máximo", value = 100, step = 5, min = 0, max = 100)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("prop_by", "Intervalo", value = 5, min = 0, step = 1, max = 99) %>%
            .help_buttom(body = "Defina a sequência de proporção esperada. Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                        title = "Sequência da proporção esperada")
      ),

      fluidRow(
        column(6,
               textInput(inputId = "prop_precisoes_plot",
                         label   = "Digite valores de amplitude (%) para fazer o gráfico",
                         value   = paste0(c(input$e_prop, input$e_prop + 1, input$e_prop + 2), collapse = ", "),
                         width   = "600px") %>%
                 .help_buttom(body = "Defina os valores de amplitude desejada.
                                                      Esses valores serão utilizados para criar diferentes linhas no gráfico.
                                                      Separe os valores por vírgula ',' e utilize ponto '.' como decimal.")
        )
      ),

      shinycssloaders::withSpinner(plotly::plotlyOutput("prop_plot", width = "80%"), type = 5),
      br(), br(),
      downloadButton("download_prop_tab","Download tabela"),
      DT::dataTableOutput("prop_tab", width = "100%")

    ))
  })


  eval(parse(text = check_text_input_to_vector("prop_precisoes_plot")))


  tab_prop_cenarios <- reactive({

    precisoes <- text_input_to_vector(input$prop_precisoes_plot)
    req(length(precisoes) > 0)


    expand.grid(`Amplitude (%)` = precisoes,
                `Proporção esperada (%)` = seq(from = input$prop_from, to = input$prop_to, by = input$prop_by),
                `Nível de confiança (%)` = input$conf_prop,
                `Método` = input$p1_metodo,
                stringsAsFactors = FALSE) %>%
      mutate(n = mapply(
        function(e, P, level, method){ presize::prec_prop(p = P, conf.width = e, conf.level = level, method = method)$n },
        `Amplitude (%)`/100, `Proporção esperada (%)`/100, `Nível de confiança (%)`/100, `Método`),
        `n + perdas/ recusas`  = n_perdas(n, input$prop_perdas_recusa),
        `Tamanho da amostra`   = ceiling(n),
        `% de perdas/ recusas` = input$prop_perdas_recusa)
  })



  output$prop_plot <- plotly::renderPlotly({

    g1 <- tab_prop_cenarios() %>%
      mutate(`Amplitude (%)` = factor(`Amplitude (%)`)) %>%
      ggplot(aes(x = `Proporção esperada (%)`, y = `Tamanho da amostra`, color = `Amplitude (%)`)) +
      geom_point() +
      geom_line() +
      xlab("Proporção esperada (%)") +
      ylab("Tamanho da amostra*") +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(palette = "Set1")

    plotly::ggplotly(g1, tooltip = c("x", "colour", "y")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                showarrow = F, xref='paper', yref='paper',
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=10)))
  })


  tab_prop_cenarios_print <- reactive({
    tab_prop_cenarios() %>%
      dplyr::select(`Proporção esperada (%)`,
                    `Nível de confiança (%)`,
                    `Amplitude (%)`,
                    `Método`,
                    `Tamanho da amostra`,
                    `% de perdas/ recusas`,
                    `n + perdas/ recusas`)
  })


  output$prop_tab <- DT::renderDataTable({
    tab_prop_cenarios_print() %>%
      DT::datatable(extensions = c('FixedColumns'),
                rownames   = FALSE,
                filter     = "none",
                options    = list(pageLength = 10,
                                  scrollX = TRUE,
                                  scrollY = TRUE,
                                  searching = FALSE,
                                  fixedColumns = list(leftColumns = 1),
                                  dom = 'B<"dwnld">frtip'))
  })


  output$download_prop_tab <- downloadHandler(
    filename = function() { "PSS_Health_Cenarios_tamanho_amostra_uma_proporcao.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_prop_cenarios_print(), path = file)}
  )










  # Testar  ------

  eval(parse(text = warning_prop("p_TH_h0")))
  eval(parse(text = warning_prop("p_TH_observado")))
  eval(parse(text = warning_prop("alpha_TH_prop")))
  eval(parse(text = warning_prop("beta_TH_prop")))
  eval(parse(text = warning_perdas("prop_1th_perdas_recusa")))


  output$th_prop_formula1 <- renderUI({
    withMathJax(paste0("$$H_0: \\pi", "=", input$p_TH_h0,
                       "\\text{  vs  }",
                       "H_1: \\pi", "\\neq", input$p_TH_h0,
                       "$$"))
  })



  output$TH1prop <- renderText({

    code <- paste0(
      "EnvStats::propTestN(p.or.p1     = ", input$p_TH_h0, "/100, ",
      "p0.or.p2 = ", input$p_TH_observado, "/100, ",
      "alpha       = ", input$alpha_TH_prop,  "/100, ",
      "power       = ", input$beta_TH_prop, "/100, ",
      "sample.type = 'one.sample', ",
      # "alternative = '", input$alternative_TH2_prop_pwr2, "', ",
      "approx     = ", input$prop_1th_approx, ", ",
      if(input$prop_1th_approx){
        paste0("correct = ",  input$prop_1th_correction, ", ")
      },
      "warn = FALSE)"
    )


    n <- try_n(code)
    eval(parse(text = validate_n("n")))

    if(input$prop_1th_approx){
      n <- ceiling(n)
    } else{
      n <- ceiling(n$n)
    }

    eval(parse(text = validate_n_inf("n")))


    paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
           "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

           "O cálculo do tamanho de amostra foi realizado por meio da ", .txt_citacao_tap, ", ",
           "para testar se a proporção de <b>", "Y", "</b> é diferente de ", input$p_TH_h0, ". ",
           "Considerando poder de <b>", input$beta_TH_prop,
           "%</b>, nível de significância de <b>", input$alpha_TH_prop, "%</b>",

           " proporção esperada de <b>", input$p_TH_observado, "</b> como referido em Fulano (1900), ",

           if(input$prop_1th_approx){
             if(input$prop_1th_correction){
               "  e utilizando o cálculo baseada na aproximação da normal com correção de continuidade, "
             } else{
               "  e utilizando o cálculo baseada na aproximação da normal sem correção de continuidade, "
             }
           } else{
               "  e utilizando o método exato, "
           },


           "chegou-se ao tamanho de amostra de <b>", n, "</b> sujeitos. ",
           if(testar_valor_perdas_valido(input$prop_1th_perdas_recusa)){
             paste0("Acrescentando <b>", input$prop_1th_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>",
                    n_perdas(n, input$prop_1th_perdas_recusa), "</b>.")
           },
           .txt_referencia_tap, print_r_code(code)
    )

  })





  # Poder ----

  eval(parse(text = warning_prop("p_power_h0")))
  eval(parse(text = warning_prop("p_power_observado")))
  eval(parse(text = warning_prop("p_power_alpha")))
  eval(parse(text = warning_inteiro("p_power_n")))


  output$p_power_th <- renderUI({
    withMathJax(paste0("$$H_0: \\pi", "=", input$p_power_h0,
                       "\\text{  vs  }",
                       "H_1: \\pi", "\\neq", input$p_power_h0,
                       "$$"))
  })



  output$p_power_output <- renderText({

    code <- paste0(
      "EnvStats::propTestPower (",
      "n.or.n1 = ", input$p_power_n, ", ",
      "p.or.p1 = ", input$p_power_h0, "/100, ",
      "p0.or.p2 = ", input$p_power_observado, "/100, ",
      "alpha = ", input$p_power_alpha,  "/100, ",
      "sample.type = 'one.sample', ",
      "approx = ", input$p_power_approx, ", ",
      if(input$p_power_approx){
        paste0("correct = ",  input$p_power_correction, ", ")
      },
      "warn = FALSE)"
    )


    n <- try_n(code)
    eval(parse(text = validate_n("n")))

    if(input$p_power_approx){
      n <- round(n*100, 1)
    } else{
      n <- round(n$power*100, 1)
    }

    eval(parse(text = validate_n_inf("n")))


    paste0("<b><font size = '5'>Poder calculado: ", n,
           "%</font></b></br></br><i>Sugestão de texto:</i></br></br>",

           "O cálculo do poder do teste foi realizado por meio da ", .txt_citacao_tap, ", ",
           "para testar se a proporção de <b>", "Y", "</b> é diferente de ", input$p_power_h0, ". ",
           "Considerando um tamanho amostral de <b>", input$input$p_power_n,
           "</b>, nível de significância de <b>", input$p_power_alpha, "%</b>",

           " proporção esperada de <b>", input$p_power_observado, "%</b> ",

           if(input$p_power_approx){
             if(input$p_power_correction){
               "  e utilizando o cálculo baseada na aproximação da normal com correção de continuidade, "
             } else{
               "  e utilizando o cálculo baseada na aproximação da normal sem correção de continuidade, "
             }
           } else{
             "  e utilizando o método exato, "
           },


           "chegou-se a um poder de <b>", n, "%</b>. ",
           .txt_referencia_tap, print_r_code(code)
    )

  })





  # output$mean_eq <- renderText({
  #
  #   n <- TrialSize::OneSampleMean.Equivalence(
  #     alpha = input$alpha_eq_mean,
  #     beta = 1 - input$beta_eq_mean,
  #     sigma = input$sigma_eq_mean,
  #     margin = input$margin_eq_mean,
  #     delta = input$delta_eq_mean
  #   )
  #
  #   paste0("<b><font size = '5'>Tamanho amostral calculado: ",ceiling(n),
  #          "</font></b></br></br><i>Sugestão de texto:</i></br></br>
  #            Com o objetivo de detectar uma diferença mínima entre proporções igual a ",input$margin_eq_mean,
  #          ", considerando o nível de significância igual a ",input$alpha_eq_mean,
  #          ", o poder igual a ", input$beta_eq_mean,
  #          ", o desvio padrão da variável de interesse igual a ", input$sigma_eq_mean,
  #          " e a margem de não inferioridade ou superioridade igual a ",input$delta_eq_mean,
  #          ", o tamanho de amostra calculado é igual a ",ceiling(n), ".")
  #
  # })
  #
  # output$mean_eq2 <- renderText({
  #
  #   n <- TrialSize::OneSampleMean.NIS(
  #     alpha = input$alpha_eq_mean2,
  #     beta = 1 - input$beta_eq_mean2,
  #     sigma = input$sigma_eq_mean2,
  #     margin = input$margin_eq_mean2,
  #     delta = input$delta_eq_mean2
  #   )
  #
  #   paste0("<b><font size = '5'>Tamanho amostral calculado: ",ceiling(n),
  #          "</font></b></br></br><i>Sugestão de texto:</i></br></br>
  #             Com o objetivo de detectar uma diferença mínima entre médias igual a ",input$delta_eq_mean,
  #          ", considerando o nível de significância igual a ",input$alpha_eq_mean,
  #          ", o poder igual a ", input$beta_eq_mean,
  #          ", o desvio padrão da variável de interesse igual a ", input$sigma_eq_mean,
  #          " e a margem de não inferioridade ou superioridade igual a ",input$margin_eq_mean,
  #          ", o tamanho de amostra calculado é igual a ",ceiling(n), ".")
  #
  # })
  #
  # output$prop_eq <- renderText({
  #
  #   n <- TrialSize::OneSampleProportion.Equivalence(
  #     alpha = input$alpha_eq_prop,
  #     beta = 1 - input$beta_eq_prop,
  #     p = input$p_eq_prop,
  #     margin = input$margin_eq_prop,
  #     delta = input$delta_eq_prop
  #   )
  #
  #   paste0(
  #     "<b><font size = '5'>Tamanho amostral calculado: ",ceiling(n),
  #     "</font></b></br></br><i>Sugestão de texto:</i></br></br>
  #      Considerando o nível de significância igual a ",input$alpha_eq_prop,
  #     ", o poder igual a ", input$beta_eq_prop,
  #     ", a verdadeira proporção da variável de interesse na população igual a ", input$sigma_eq_prop,
  #     ", a diferença mínima a ser detectada igual a ",input$margin_eq_prop,
  #     " e a margem de não inferioridade ou superioridade igual a ",input$delta_eq_prop,
  #     ", o tamanho de amostra calculado para o teste de equivalência entre proporções é igual a ",ceiling(n), ".")
  #
  # })
  #
  # output$prop_eq2 <- renderText({
  #
  #   n <- TrialSize::OneSampleProportion.NIS(
  #     alpha = input$alpha_eq_prop2,
  #     beta = 1 - input$beta_eq_prop2,
  #     p = input$p_eq_prop2,
  #     margin = input$margin_eq_prop2,
  #     delta = input$delta_eq_prop2
  #   )
  #
  #   paste0(
  #     "<b><font size = '5'>Tamanho amostral calculado: ",ceiling(n),
  #     "</font></b></br></br><i>Sugestão de texto:</i></br></br>
  #      Considerando o nível de significância igual a ",input$alpha_eq_prop2,
  #     ", o poder igual a ", input$beta_eq_prop2,
  #     ", a verdadeira proporção da variável de interesse na população igual a ", input$sigma_eq_prop2,
  #     ", a diferença mínima a ser detectada igual a ",input$margin_eq_prop2,
  #     " e a margem de não inferioridade ou superioridade igual a ",input$delta_eq_prop2,
  #     ", o tamanho de amostra calculado é igual a ",ceiling(n), ".")
  #
  # })




  #__________-----
  #  Cronbach  ----


  # Estimar ----


  eval(parse(text = warning_prop("conf_Cronbach")))
  eval(parse(text = warning_prop("Cronbach_espected", entre0e1 = TRUE)))
  eval(parse(text = warning_numero_positivo("Cronbach_precisao")))
  eval(parse(text = warning_inteiro("k_Cronbach")))


  output$Cronbach_est <- renderText({

    code <- paste0("size.ci.cron1(",
                "alpha = 1 - ", input$conf_Cronbach, "/100, ",
                "k = ", input$k_Cronbach, ", ",
                "rel = ", input$Cronbach_espected, ", ",
                "w = ", input$Cronbach_precisao,
                ") # Douglas G. Bonett. (2020). Sample Size Planning for Behavioral Science Research. Sample Size Planning for Behavioral Science Research. https://people.ucsc.edu/~dgbonett/sample.html"
    )

    n <- eval(parse(text = code))
    eval(parse(text = validate_n("n")))
    eval(parse(text = validate_n_inf("n")))


    paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
           "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

           "Para o cálculo do tamanho de amostra para estimar o coeficiente alfa de Cronbach foi utilizado a ", .txt_citacao_tap, ". ",
           "Considerando um instrumento com <b>", input$k_Cronbach, "</b> itens, ",
           "margem de erro de <b>", input$Cronbach_precisao, "</b>, ",
           "</b> nível de confiança de <b>", input$conf_Cronbach, "%</b>",
           " e alfa de Cronbach esperado de <b>", input$Cronbach_espected,"</b> como referido em Fulano (1900), ",
           "chegou-se ao tamanho de amostra de <b>", n, "</b> sujeitos. ",
           if(testar_valor_perdas_valido(input$Cronbach_perdas_recusa)){
             paste0(
               "Acrescentando <b>", input$Cronbach_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>",
               n_perdas(n, input$Cronbach_perdas_recusa), "</b>.")
           },
           .txt_referencia_tap#, print_r_code(code)

           # "<br><br>Foi utilizada a função ", code("size.ci.cron1()"), " criada por: <br>Bonett, D.G. (2016). Sample Size Planning for Behavioral Science Research. Retrieved from ",
           # '<a href="https://people.ucsc.edu/~dgbonett/sample.html" target="_blank">https://people.ucsc.edu/~dgbonett/sample.html.</a>.'

    )

  })


  output$cronbach_code <- renderText({

    paste0("</br></br>",
           "<i>Comando R utilizado:</i><br>",
           "<p style=\"font-family:'Courier New';font-size:100% \">",
           HTML(
             "# Douglas G. Bonett. (2020). Sample Size Planning for Behavioral Science Research. Sample Size Planning for Behavioral Science Research. https://people.ucsc.edu/~dgbonett/sample.html"
           ),
           br(),br(),

           code("size.ci.cron1 <- function(alpha, k, rel, w) {"), br(),
           code("# Computes sample size required to estimate a Cronbach"), br(),
           code("# alpha reliability with desired precision"), br(),
           code("# Arguments:"), br(),
           code("#   alpha: alpha value for 1-alpha confidence"), br(),
           code("#   k:     number of measurements"), br(),
           code("#   rel:   reliability planning value"), br(),
           code("#   w:     desired CI width"), br(),
           code("# Returns:"), br(),
           code("#   required sample size"), br(),
           code("z <- qnorm(1 - alpha/2)"), br(),
           code("n0 <- ceiling((8*k/(k - 1))*(1 - rel)^2*(z/w)^2 + 2)"), br(),
           code("b <- log(n0/(n0 - 1))"), br(),
           code("ll <- 1 - exp(log(1 - rel) - b + z*sqrt(2*k/((k - 1)*(n0 - 2))))"), br(),
           code("ul <- 1 - exp(log(1 - rel) - b - z*sqrt(2*k/((k - 1)*(n0 - 2))))"), br(),
           code("w0 <- ul - ll"), br(),
           code("n <- ceiling((n0 - 2)*(w0/w)^2 + 2)"), br(),
           code("return (n)"), br(),
           code("}"), br(),
           br(), br(),

           code("size.ci.cron1("),
           code(paste0("alpha = 1 - ", input$conf_Cronbach, "/100, ")),
           code(paste0("k = ", input$k_Cronbach, ", ")),
           code(paste0("rel = ", input$Cronbach_espected, ", ")),
           code(paste0("w = ", input$Cronbach_precisao, ")")),

           "</p>",
           "<br><br><b><i>* Sempre procure um profissional de estatística para orientações no planejamento do estudo.</b></i>"
    )

  })







  #________________----
  # 2 media ind----

  # Testar ----

  observeEvent(input$show_d_cohen, {
    showModal(
      modalDialog(
        title = "d de Cohen",
        fluidPage(
          withMathJax(includeMarkdown(file.path("www", "Effect_size_d_Cohen.Rmd")))
        ),
        easyClose = TRUE,
        footer    = NULL,
        size      = "l"
      )
    )
  })




  observeEvent(input$show_th_2mean, {
    showModal(
      modalDialog(
        title = "Ajustes",
        fluidPage(

          HTML("<b>Preencha os campos abaixo de acordo com seu estudo para que sirvam de guia no preenchimento dos demais campos</b>."),
          br(), br(),
          textInput(inputId = "mean2_nome_desfecho",
                    label   = "Descreva o nome do desfecho",
                    value   = ifelse(input$show_th_2mean == 0, "Y", mean2_nome_desfecho())),
          HTML(paste0("<i>", str_remove_all(.txt_desfecho, "<br><br>"), "</i>")),
          br(), br(),
          textInput(inputId = "mean2_unidade_medida",
                    label   = paste0("Descreva a unidade de medida do desfecho"),
                    value   = ifelse(input$show_th_2mean == 0, "u.m.", mean2_unidade_medida())),
          HTML(paste0("<i>", str_remove_all(.txt_um, "<br><br>"), "</i>")),
          br(), br(),

          textInput(inputId = "th2mean_grupoTratamento",
                    label   = "Descreva um nome para o grupo Tratamento",
                    value   = ifelse(input$show_th_2mean == 0, "Tratamento", th2mean_grupoTratamento())),

          HTML("<i>Em alguns estudos o grupo Tratamento também pode ser chamadado de grupo Intervenção ou grupo Exposto.</i><br><br>"),

          textInput(inputId = "th2mean_grupoControle",
                    label   = "Descreva um nome para o grupo Controle",
                    value   = ifelse(input$show_th_2mean == 0, "Controle", th2mean_grupoControle())),

          HTML("<i>Em alguns estudos o grupo Controle também pode ser chamadado de grupo Placebo/ Sham ou grupo Não exposto.</i>"),


        ),
        easyClose = TRUE,
        footer    = NULL
      )
    )
  })





  th2mean_grupoControle <- reactive({
    ifelse(is.null(input$th2mean_grupoControle), "Controle", input$th2mean_grupoControle)
  })

  th2mean_grupoTratamento <- reactive({
    ifelse(is.null(input$th2mean_grupoTratamento), "Tratamento", input$th2mean_grupoTratamento)
  })

  mean2_nome_desfecho <- reactive({
    ifelse(is.null(input$mean2_nome_desfecho), "Y", input$mean2_nome_desfecho)
  })

  mean2_unidade_medida <- reactive({
    ifelse(is.null(input$mean2_unidade_medida), "u.m.", input$mean2_unidade_medida)
  })




  observeEvent(c(input$show_desvio_pooled_2), {
    validate(need(input$show_desvio_pooled_2 > 0, ''))

    eval(parse(text = warning_numero_positivo("pooled_sigma1_modal_2")))
    eval(parse(text = warning_numero_positivo("pooled_sigma2_modal_2")))
    eval(parse(text = warning_numero_positivo("pooled_n1_modal_2")))
    eval(parse(text = warning_numero_positivo("pooled_n2_modal_2")))


    showModal(
      modalDialog(
        title = "Obter o desvio padrão combinado",
        fluidPage(

          HTML(paste0("<b><font size = '3'>", "Desvio padrão", " do</font></b><br>")),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
              numericInput( "pooled_sigma1_modal_2",
                            th2mean_grupoTratamento(),
                            value = 2.0,
                            min = 0,
                            max = Inf,
                            step = .5)),
          div(style="display: inline-block;vertical-align:top; width: 49%;",
              numericInput( "pooled_sigma2_modal_2",
                            th2mean_grupoControle(),
                            value = 1.5,
                            min = 0,
                            max = Inf,
                            step = .5)),


          HTML(paste0("<b><font size = '3'>", "Tamanho amostral", " do</font></b><br>")),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
              numericInput( "pooled_n1_modal_2",
                            th2mean_grupoTratamento(),
                            value = 20,
                            min = 3,
                            max = Inf,
                            step = 1)),
          div(style="display: inline-block;vertical-align:top; width: 49%;",
              numericInput( "pooled_n2_modal_2",
                            th2mean_grupoControle(),
                            value = 30,
                            min = 3,
                            max = Inf,
                            step = 1)),

          htmlOutput("ferramentas_pooled_modal_2"),

          p("Foi utilizado a fórmula"),
          withMathJax("$$s_{combinado} = \\sqrt{ \\dfrac{(n_A - 1)s_A^2 + (n_B - 1)s_B^2}{n_A+n_B-2} }$$")

        ),
        easyClose = TRUE,
        footer    = NULL,
        size      = "m"
      )
    )
  })

  output$ferramentas_pooled_modal_2 <- renderText({

    s2a <- input$pooled_sigma1_modal_2^2
    s2b <- input$pooled_sigma2_modal_2^2

    n1 <- input$pooled_n1_modal_2
    n2 <- input$pooled_n2_modal_2

    numerador   <- (n1 - 1)*s2a + (n2 - 1)*s2b
    denominador <- n1 + n2 - 2

    s_pooled <- sqrt(numerador/denominador)

    paste0("<br><br><b><font size = '5'>",
           "<i>Desvio padrão<sub>combinado</sub></i> = ", round(s_pooled, 4),
           "<br><br><br>")
  })







  output$mean2Ui <- renderUI({
    fluidPage(fluidRow(
      numericInput( "TH2_mean_margin",
                    paste0("Diferença do ", mean2_nome_desfecho(), " a ser detectada em ", mean2_unidade_medida(), " (Média do ", th2mean_grupoTratamento(),
                           ") - (Média do grupo ", th2mean_grupoControle(), ")"),
                    value = 1,
                    min = -Inf,
                    max = Inf,
                    step = .5
      ) %>% .help_buttom(body = .txt_diferenca_clinica, title = "Diferença a ser detectada"),

      numericInput( "sigma1_TH2_mean_pwr",
                    "Desvio padrão esperado",
                    value = 1.2,
                    min = 0,
                    max = Inf,
                    step = 1
      ) %>% .help_buttom(body = .txt_dp, title = "Desvio padrão esperado"),
      actionLink("show_desvio_pooled_2", "Calcular o desvio padrão combinado"),
      br(),
      br(),

      numericInput( "ratio_TH2_mean_pwr",
                    paste0("Balanceamento (", th2mean_grupoTratamento(), ":", th2mean_grupoControle(), ")"),
                    value = 1,
                    min = 0,
                    max = Inf,
                    step = 0.5
      ) %>% .help_buttom(body = .txt_balanceamento, title = "Balanceamento"),

      numericInput( "deff_TH2_mean_pwr",
                    "Efeito do plano amostral",
                    value = 1,
                    min = 0,
                    max = Inf,
                    step =.2
      ) %>% .help_buttom(body = paste0(
        "O efeito do plano amostral (em inglês, <i>design effect</i> ou, abreviadamente, <i>deff</i>) é utilizado para medir o efeito de um plano amostral sobre a variância de um estimador. <br><br>",
        "Ele representa o quanto o plano amostral proposto é mais ou menos eficiente, em termos de variabilidade da estimativa, do que a amostragem aleatória simples. <br><br>",
        "Se seu valor for igual a 1, então o plano amostral proposto é a amostragem aleatória simples ou é considerado tão eficiente quanto esta. Valores maiores do que 1 indicam que o plano amostral proposto é menos eficiente do que a amostragem aleatória simples (geralmente a amostragem por conglomerados tem essa característica) e valores menores do que 1 indicam que o plano amostral proposto é mais eficiente do que a amostragem aleatória simples (geralmente a amostragem estratificada tem essa característica).",
        .txt_definido_literatura),
        title = "Efeito do plano amostral")
    ))
  })



  output$th2_mean_formula1 <- renderUI({
    req(!is.null(sided_two_means_test()))
    req(length(sided_two_means_test()) != 0)
    req(!is.null(th2mean_grupoTratamento()))
    sinal_h0 <- case_when(input$alternative_TH2_mean_pwr == 'two.sided' ~ "=",
                          input$alternative_TH2_mean_pwr == 'one.sided' & sided_two_means_test()$alternative_d == "greater"  ~ "\\leq",
                          input$alternative_TH2_mean_pwr == 'one.sided' & sided_two_means_test()$alternative_d == "less" ~ "\\geq")

    withMathJax(
      paste0("$$H_0: \\mu_{", th2mean_grupoTratamento(), "} ", sinal_h0, " \\mu_{", th2mean_grupoControle(), "} $$"))
  })

  output$th2_mean_formula2 <- renderUI({
    req(!is.null(sided_two_means_test()))
    req(length(sided_two_means_test()) != 0)
    req(!is.null(th2mean_grupoTratamento()))
    sinal_h1 <- case_when(input$alternative_TH2_mean_pwr == 'two.sided' ~ "\\neq",
                          input$alternative_TH2_mean_pwr == 'one.sided' & sided_two_means_test()$alternative_d == "greater"  ~ ">",
                          input$alternative_TH2_mean_pwr == 'one.sided' & sided_two_means_test()$alternative_d == "less" ~ "<")

    withMathJax(
      paste0("$$H_1: \\mu_{", th2mean_grupoTratamento(), "}", sinal_h1, " \\mu_{", th2mean_grupoControle(), "} $$"))
  })





  observeEvent(input$cohen_TH2_mean_pwr, {
    if(is.na(input$cohen_TH2_mean_pwr)){
      shinyFeedback::showFeedbackWarning(
        inputId = "cohen_TH2_mean_pwr",
        text = "Deve ser fornecido um valor do tamanho de efeito.",
        color = "red"
      )
    } else if (input$cohen_TH2_mean_pwr == 0) {
      shinyFeedback::showFeedbackWarning(
        inputId = "cohen_TH2_mean_pwr",
        text = "O tamanho de efeito deve ser diferente de que zero.",
        color = "red"
      )
    } else {
      shinyFeedback::hideFeedback("cohen_TH2_mean_pwr")
    }
  })


  observeEvent(input$TH2_mean_margin, {
    if(is.na(input$TH2_mean_margin)){
      shinyFeedback::showFeedbackWarning(
        inputId = "TH2_mean_margin",
        text = "Deve ser fornecido um valor da diferença a ser detectada.",
        color = "red"
      )
    } else if(input$TH2_mean_margin == 0){
      shinyFeedback::showFeedbackWarning(
        inputId = "TH2_mean_margin",
        text = "A diferença a ser detectada deve ser diferente de 0.",
        color = "red"
      )
    } else {
      shinyFeedback::hideFeedback("TH2_mean_margin")
    }
  })


  observeEvent(input$sigma1_TH2_mean_pwr, {
    if(is.na(input$sigma1_TH2_mean_pwr)){
      shinyFeedback::showFeedbackWarning(
        inputId = "sigma1_TH2_mean_pwr",
        text = "Deve ser fornecido um valor de desvio padrão.",
        color = "red"
      )
    } else if (input$sigma1_TH2_mean_pwr<= 0) {
      shinyFeedback::showFeedbackWarning(
        inputId = "sigma1_TH2_mean_pwr",
        text = "O desvio padrão deve ser maior do que zero.",
        color = "red"
      )
    } else {
      shinyFeedback::hideFeedback("sigma1_TH2_mean_pwr")
    }
  })





  observeEvent(input$power_TH2_mean_pwr, {
    if(is.na(input$power_TH2_mean_pwr)){
      shinyFeedback::showFeedbackWarning(
        inputId = "power_TH2_mean_pwr",
        text = "Deve ser fornecido um valor do poder.",
        color = "red"
      )
    } else if (input$power_TH2_mean_pwr >= 100) {
      shinyFeedback::showFeedbackWarning(
        inputId = "power_TH2_mean_pwr",
        text = "Poder deve ser menor do que 100%.",
        color = "red"
      )
    } else if (input$power_TH2_mean_pwr <= 0) {
      shinyFeedback::showFeedbackWarning(
        inputId = "power_TH2_mean_pwr",
        text = "Poder deve ser maior do que 0%.",
        color = "red"
      )
    } else {
      shinyFeedback::hideFeedback("power_TH2_mean_pwr")
    }
  })




  observeEvent(input$sig_TH2_mean_pwr, {
    if(is.na(input$sig_TH2_mean_pwr)){
      shinyFeedback::showFeedbackWarning(
        inputId = "sig_TH2_mean_pwr",
        text = "Deve ser fornecido um valor do nível de significância.",
        color = "red"
      )
    } else if (input$sig_TH2_mean_pwr >= 100) {
      shinyFeedback::showFeedbackWarning(
        inputId = "sig_TH2_mean_pwr",
        text = "Nível de significância deve ser menor do que 100%.",
        color = "red"
      )
    } else if (input$sig_TH2_mean_pwr <= 0) {
      shinyFeedback::showFeedbackWarning(
        inputId = "sig_TH2_mean_pwr",
        text = "Nível de significância deve ser maior do que 0%.",
        color = "red"
      )
    } else {
      shinyFeedback::hideFeedback("sig_TH2_mean_pwr")
    }
  })


  eval(parse(text = warning_perdas("TH_mean_perdas_recusa")))
  eval(parse(text = warning_numero_positivo("deff_TH2_mean_pwr")))


  sided_two_means_test <- reactive({

    req(!is.null(input$alternative_TH2_mean_pwr))
    req(!is.null(input$TH2_mean_margin))
    # req(!is.null(input$cohen_TH2_mean_pwr) | !is.null(input$TH2_mean_margin))


    # Se entrar com o d de Cohen
    if(input$th2_mean_cohen){

      if(input$alternative_TH2_mean_pwr != "two.sided" & input$cohen_TH2_mean_pwr < 0){
        texto_comparacao <- paste0("Foi calculado o tamanho de amostra para detectar que a média de <b>Y</b> no grupo ", th2mean_grupoTratamento(), " é inferior a do ", th2mean_grupoControle(), ", ")
        alternative <- 1
        alternative_d <- "less"
      } else{
        texto_comparacao <- paste0("Foi calculado o tamanho de amostra para detectar que a média de <b>Y</b> no grupo ", th2mean_grupoTratamento(), " é superior a do ", th2mean_grupoControle(), ",")
        alternative <- 1
        alternative_d <- "greater"
      }
    } else {

      if(input$alternative_TH2_mean_pwr != "two.sided" & input$TH2_mean_margin < 0){
        texto_comparacao <- paste0("Foi calculado o tamanho de amostra para detectar que a média de <b>Y</b> no grupo ", th2mean_grupoTratamento(), " é inferior a do ", th2mean_grupoControle(), ", ")
        alternative <- 1
        alternative_d <- "less"
      } else{
        texto_comparacao <- paste0("Foi calculado o tamanho de amostra para detectar que a média de <b>Y</b> no grupo ", th2mean_grupoTratamento(), " é superior a do ", th2mean_grupoControle(), ",")
        alternative <- 1
        alternative_d <- "greater"
      }
    }


    # O de dois lados se sobrepoe
    if(input$alternative_TH2_mean_pwr == "two.sided"){
      texto_comparacao <- paste0("Foi calculado o tamanho de amostra para detectar diferenças na média de <b>", mean2_nome_desfecho(), "</b> entre os grupos ", th2mean_grupoTratamento(), " e ", th2mean_grupoControle(), ", ")
      alternative <- 2
      alternative_d <- "two.sided"
    }

    list(alternative = alternative,
         alternative_d = alternative_d,
         texto_comparacao = texto_comparacao)
  })



  output$THmean2 <- renderText({

    if(!input$th2_mean_cohen){
      validate(
        need(!is.na(input$TH2_mean_margin),     "É obrigatório fornecer um valor da diferença a ser detectada."),
        need(!is.na(input$sigma1_TH2_mean_pwr), paste0("É obrigatório fornecer um valor de desvio padrão do ", th2mean_grupoTratamento(), ".")),

        need(input$TH2_mean_margin != 0,       "A diferença a ser detectada deve ser diferente de zero."),
        need(input$sigma1_TH2_mean_pwr > 0,    "O desvio padrão deve ser maior do que zero."),

        need(input$deff_TH2_mean_pwr > 0,    "O efeito do delineamento deve ser maior do que zero.")

            )
    } else{

      validate(
        need(!is.na(input$cohen_TH2_mean_pwr), "É obrigatório fornecer um valor do tamanho do efeito."),
        need(input$cohen_TH2_mean_pwr != 0,    "O tamanho do efeito deve ser diferente de zero.")
      )
    }

    validate(
      need(!is.na(input$power_TH2_mean_pwr), "É obrigatório fornecer um valor do poder."),
      need(!is.na(input$sig_TH2_mean_pwr),   "É obrigatório fornecer um valor do nível de significância."),

      need(input$power_TH2_mean_pwr > 0, "O poder deve ser maior do que zero."),
      need(input$sig_TH2_mean_pwr > 0,   "O nível de significância deve ser maior do que zero.")
    )




    #----------------------------------------------------.
    # Se o usuario entrar com o tamanho do efeito !!!!
    #----------------------------------------------------.


    if(input$th2_mean_cohen){

      code <- paste0(
        "pwr::pwr.t.test(d = ", input$cohen_TH2_mean_pwr, ", ",
        "sig.level = ", input$sig_TH2_mean_pwr/100, ", ",
        "power = ", input$power_TH2_mean_pwr/100, ", ",
        "alternative = '", sided_two_means_test()$alternative_d, "', ",
        "type = 'two.sample')")

      n <- try_n(code)
      eval(parse(text = validate_n("n")))

      n <- n$n* 2 # multiplica por 2 pois calcula o n por grupo!

      n <- 2 * ceiling(n/2)
      nperdas <- n_perdas(n, input$TH_mean_perdas_recusa)
      nperdas <- 2 * ceiling(nperdas/2)
      eval(parse(text = validate_n_inf("n")))



      cabecalho <- paste0("<b><font size = '5'>Tamanho amostral calculado: ", n, " (<i>", n/2, " em cada grupo</i>)",
                          "</font></b></br></br><i>Sugestão de texto:</i></br></br>")


      texto_tamanho_efeito <- paste0(sided_two_means_test()$texto_comparacao,
                                     " por meio da ", .txt_citacao_tap, ". ",
                                     "Considerando poder de <b>", input$power_TH2_mean_pwr, "%</b>, ",
                                     "nível de significância de <b>", input$sig_TH2_mean_pwr, "%</b> ",
                                     "e tamanho de efeito d de Cohen de <b>", input$cohen_TH2_mean_pwr, "</b> (dados de Fulano (1900) <b>OU</b> escolha do pesquisador), ",
                                     "chegou-se ao tamanho total da amostra de <b>", n, "</b> sujeitos, sendo <b>", n/2, "</b> sujeitos em cada grupo. ",
                                     if(testar_valor_perdas_valido(input$TH_mean_perdas_recusa)){
                                       paste0(
                                         "Acrescentando <b>", input$TH_mean_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", nperdas, "</b>.")
                                     }
      )

      return(paste0(cabecalho, texto_tamanho_efeito, .txt_referencia_tap, print_r_code(code)))


    } else{

      #----------------------------------------.
      # se nao usar o tamanho de efeito !!!!
      #----------------------------------------.




      ratio <- input$ratio_TH2_mean_pwr

      code <- paste0(
        "epiR::epi.sscompc(",
        "control = 0, ",
        "treat = ", abs(input$TH2_mean_margin), ", ",
        "sigma = ", input$sigma1_TH2_mean_pwr, ", ",
        "n = NA, ",
        "power = ", input$power_TH2_mean_pwr, "/100,  ",
        "conf.level = 1 - ", input$sig_TH2_mean_pwr, "/100, ",
        "r = ", ratio, ", ",
        "design = ", input$deff_TH2_mean_pwr, ", ",
        "sided.test = ", sided_two_means_test()$alternative, ")")

      npwr <- try_n(code)
      eval(parse(text = validate_n("npwr")))


      n1 <- ceiling(npwr$n.treat)
      n2 <- ceiling(npwr$n.control)

      n <- n1 + n2
      nperdas1 <- n_perdas(n1, input$TH_mean_perdas_recusa)
      nperdas2 <- n_perdas(n2, input$TH_mean_perdas_recusa)
      eval(parse(text = validate_n("n")))
      eval(parse(text = validate_n_inf("n")))


      cabecalho <- paste0("<b><font size = '5'>Tamanho amostral calculado: ", n, " (<i>", n1, " no ", th2mean_grupoTratamento(), " e ", n2, " no ", th2mean_grupoControle(), "</i>)",
                          "</font></b></br></br><i>Sugestão de texto:</i></br></br>")


      texto_desvio <- paste0(sided_two_means_test()$texto_comparacao,
                             " tendo uma diferença de <b>", input$TH2_mean_margin, " ", mean2_unidade_medida(), "</b> como relevante para o estudo. ",
                             "Para isso foi utilizado a ", .txt_citacao_tap, ". ",
                             "Considerando poder de <b>", input$power_TH2_mean_pwr, "%</b>, ",
                             "nível de significância de <b>", input$sig_TH2_mean_pwr, "%</b> ",

                             if(input$deff_TH2_mean_pwr != 1){
                               paste0("tamanho de efeito do delineamento de <b>", input$deff_TH2_mean_pwr, "</b> ")
                             },

                             "e desvio padrão de <b>", input$sigma1_TH2_mean_pwr, " ", mean2_unidade_medida(), "</b> (dados de Fulano (1900)), ",
                             # "</b> para o ", th2mean_grupoTratamento(), " e <b>", input$sigma2_TH2_mean_pwr, " ", mean2_unidade_medida(),
                             # "</b> para o ", th2mean_grupoControle(), " (dados de Fulano (1900)), ",

                             ifelse(ratio == 1,
                                    paste0(
                                      "chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n1, "</b> em cada grupo.",
                                      " Acrescentando <b>", input$TH_mean_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
                                      "deverá ser <b>", nperdas1 + nperdas2, "</b> (", nperdas1, " em cada grupo)."
                                    ),
                                    paste0(
                                      "chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n1, "</b> no ", th2mean_grupoTratamento(), " e <b>", n2, "</b> no ", th2mean_grupoControle(), ".",
                                      if(testar_valor_perdas_valido(input$TH_mean_perdas_recusa)){
                                        paste0(
                                          " Acrescentando <b>", input$TH_mean_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
                                          "deverá ser <b>", nperdas1 + nperdas2, "</b> (", nperdas1, " no ", th2mean_grupoTratamento(), " e ", nperdas2, " no ", th2mean_grupoControle(), ").")
                                      }
                                    )
                             )
      )

      paste0(cabecalho,
             texto_desvio,
             .txt_referencia_tap, print_r_code(code))

    }
  })




  ## Cenarios ----

  output$cenarios_duas_medias_thUi <- renderUI({

    if(input$th2_mean_cohen){
      req(!is.null(input$cohen_TH2_mean_pwr))
      val_min <- ifelse(input$cohen_TH2_mean_pwr < 0, input$cohen_TH2_mean_pwr - 1, input$cohen_TH2_mean_pwr)
      val_max <- ifelse(input$cohen_TH2_mean_pwr < 0, input$cohen_TH2_mean_pwr, input$cohen_TH2_mean_pwr + 1)

    } else{
      req(!is.null(input$TH2_mean_margin))
      val_min <- ifelse(input$TH2_mean_margin < 0, input$TH2_mean_margin - 1, input$TH2_mean_margin)
      val_max <- ifelse(input$TH2_mean_margin < 0, input$TH2_mean_margin, input$TH2_mean_margin + 1)
    }


    fluidPage(fluidRow(

      br(),
      HTML('<hr style="color: black;">'),
      br(),br(),

      titlePanel("Construção de cenários"),
      br(),

      conditionalPanel(condition = "input.th2_mean_cohen == true",
                       wellPanel("Utilize os argumentos abaixo para construir diferentes cenários.
                                        Você pode especificar valores de poder e uma sequência de magnitude de efeito desejado.
                                        Demais informações serão recuperadas do painel lateral.")
      ),

      conditionalPanel(condition = "input.th2_mean_cohen == false",
                       wellPanel("Utilize os argumentos abaixo para construir diferentes cenários.
                                        Você pode especificar valores de poder e uma sequência da diferença a ser detectada.
                                        Demais informações serão recuperadas do painel lateral.")
      ),

      fluidRow(
        column(6,
               textInput(inputId = "th2mean_power_plot",
                         label   = "Digite valores de poder para fazer o gráfico",
                         value   = "80, 90, 95",
                         width   = "400px") %>%
                 .help_buttom(body = "Defina os valores de poder desejado.
                                                      Esses valores serão utilizados para criar diferentes linhas no gráfico.
                                                      Separe os valores por vírgula ',' e utilize ponto '.' como decimal.")
        )
      ),

      conditionalPanel(condition = "input.th2_mean_cohen == true",
                       HTML("<b>Defina a sequência de valores para a magnitude do efeito:</b>"),
                       br(),
                       div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
                           numericInput("th2_mean_from", "Mínimo", value = val_min, step = 0.5)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 80px;",
                           numericInput("th2_mean_to", "Máximo", value = val_max, step = 0.5)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 80px;",
                           numericInput("th2_mean_by", "Intervalo", value = 0.2, min = 0, step = 0.1) %>%
                             .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                                         title = "Sequência"))
      ),

      conditionalPanel(condition = "input.th2_mean_cohen == false",
                       HTML("<b>Defina a sequência de valores para a diferença a ser detectada:</b>"),
                       br(),
                       div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
                           numericInput("th2_mean_from_diff", "Mínimo", value = val_min, step = 0.5)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 80px;",
                           numericInput("th2_mean_to_diff", "Máximo", value = val_max, step = 0.5)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 80px;",
                           numericInput("th2_mean_by_diff", "Intervalo", value = 0.5, min = 0, step = 0.2) %>%
                             .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                                         title = "Sequência"))
      ),

      br(),

      plotly::plotlyOutput("th2mean_plot", width = "80%"),
      br(), br(),
      downloadButton("download_th2mean_tab","Download tabela"),
      DT::dataTableOutput("th2mean_tab", width = "100%")


    ))
  })


  eval(parse(text = check_text_input_to_vector("th2mean_power_plot")))

  tab_mean2_cenarios <- reactive({

    power <- text_input_to_vector(input$th2mean_power_plot)
    req(length(power) > 0)


    if(input$th2_mean_cohen){
      expand.grid(`Magnitude do efeito` = seq(from = input$th2_mean_from,
                          to = input$th2_mean_to,
                          by = input$th2_mean_by),
                  `Poder (%)` = power,
                  `Nível de significância (%)` =  input$sig_TH2_mean_pwr,
                  # `Hipótese alternativa` = sided_two_means_test()$alternative_d,
                  alternativa_temp = sided_two_means_test()$alternative,
                  stringsAsFactors = FALSE) %>%

        mutate(`Hipótese alternativa` = case_when(alternativa_temp == 2     ~ "two.sided",
                                                  `Magnitude do efeito` > 0 ~ "greater",
                                                  `Magnitude do efeito` < 0 ~ "less")) %>%
        select(- alternativa_temp) %>%

        mutate(`Tamanho da amostra` = mapply(
          function(d, sig.level, power, alternative){
            tryCatch({
              pwr::pwr.t.test(d = d,
                              sig.level = sig.level/100,
                              power = power/100,
                              alternative = alternative,
                              type = 'two.sample')$n* 2},
              warning = function(warning_condition) { NA },
              error   = function(error_condition) { NA })
            }, `Magnitude do efeito`, `Nível de significância (%)`, `Poder (%)`, `Hipótese alternativa`),

          `Tamanho da amostra` = 2 * ceiling(`Tamanho da amostra`/2),
          `% de perdas/ recusas` = input$TH_mean_perdas_recusa)

    } else{

      # req(!is.null(input$sigma2_TH2_mean_pwr))
      req(!is.null(input$sigma1_TH2_mean_pwr))


      simul_n_inputs <- expand.grid(
        `Diferença entre as médias` = seq(from = input$th2_mean_from_diff,
                                          to = input$th2_mean_to_diff,
                                          by = input$th2_mean_by_diff),
        `Poder (%)` = power,
        `Nível de significância (%)` =  input$sig_TH2_mean_pwr,
        sd1 = input$sigma1_TH2_mean_pwr,
        # sd2 = input$sigma2_TH2_mean_pwr,
        Balanceamento = input$ratio_TH2_mean_pwr,
        `Hipótese alternativa` = sided_two_means_test()$alternative,
        `Efeito do delineamento` = input$deff_TH2_mean_pwr,
        stringsAsFactors = FALSE)

      simul_n <- simul_n_inputs %$%
        purrr::pmap_dfr(.l = list(`Diferença entre as médias`,
                                  sd1,
                                  Balanceamento,
                                  `Poder (%)`,
                                  `Nível de significância (%)`,
                                  `Hipótese alternativa`,
                                  `Efeito do delineamento`),
                        .f = function(a, b, d, e, f, g, h){
                          n <- epiR::epi.sscompc(
                            control = 0,
                            treat = a,
                            n = NA,
                            sigma = b,
                            power = e/100,
                            r = d,
                            conf.level = 1 - f/100,
                            sided.test = g,
                            design = h
                          )

                          return(data.frame(n1 = n$n.treat,
                                            n2 = n$n.control,
                                            n_total = n$n.total))

                        })

      bind_cols(simul_n_inputs, simul_n)
    }
  })





  output$th2mean_plot <- plotly::renderPlotly({

    if(input$th2_mean_cohen){
    g1 <- tab_mean2_cenarios() %>%
      mutate(`Poder (%)` = factor(`Poder (%)`)) %>%
      ggplot(aes(x = `Magnitude do efeito`, y = `Tamanho da amostra`, color = `Poder (%)`))+
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = seq(from = input$th2_mean_from, to = input$th2_mean_to, by = input$th2_mean_by)) +
      xlab("Magnitude do efeito (d)") +
      ylab("Tamanho total da amostra*") +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(palette = "Set1")

    plotly::ggplotly(g1, tooltip = c("x", "colour", "y")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                        showarrow = F, xref='paper', yref='paper',
                                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                        font=list(size=10)))

    } else{
      g1 <- tab_mean2_cenarios() %>%
        mutate(`Poder (%)` = factor(`Poder (%)`)) %>%
        ggplot(aes(x = `Diferença entre as médias`, y = n_total, color = `Poder (%)`, n1 = n1, n2 = n2))+
        geom_point() +
        geom_line() +
        scale_x_continuous(breaks = seq(from = input$th2_mean_from_diff, to = input$th2_mean_to_diff, by = input$th2_mean_by_diff)) +
        xlab("Diferença entre as médias") +
        ylab("Tamanho da amostra*") +
        theme_bw() +
        theme(axis.text = element_text(colour = "black")) +
        scale_color_brewer(palette = "Set1")


      plotly::ggplotly(g1, tooltip = c("x", "colour", "y", "n1", "n2")) %>%
        plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                          showarrow = F, xref='paper', yref='paper',
                                          xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                          font=list(size=10)))

    }
  })


  tab_2mean_print <- reactive({
    if(input$th2_mean_cohen){
      tab_mean2_cenarios()

    } else{
      df_ <- tab_mean2_cenarios() %>%
        dplyr::select(`Diferença entre as médias`,
                      n_total,
                      n1,
                      n2,
                      sd1,
                      # sd2,
                      Balanceamento,
                      `Poder (%)`,
                      `Nível de significância (%)`,
                      `Hipótese alternativa`,
                      `Efeito do delineamento`)

      colnames(df_) <- c("Diferença entre as médias",
                         "n total",
                         paste0("n ", th2mean_grupoTratamento()),
                         paste0("n ", th2mean_grupoControle()),
                         "Desvio padrão",
                         # paste0("Desvio padrão ", th2mean_grupoTratamento()),
                         # paste0("Desvio padrão ", th2mean_grupoControle()),
                         "Balanceamento",
                         "Poder (%)",
                         "Nível de significância (%)",
                         "Hipótese alternativa",
                         "Efeito do delineamento")

      df_

    }

  })

  output$th2mean_tab <- DT::renderDataTable({

    tab_2mean_print() %>%
      DT::datatable(extensions = c('FixedColumns'),
                rownames   = FALSE,
                filter     = "none",
                #callback   = DT::JS("$('div.dwnld').append($('#download_th2mean_tab'));"),
                options    = list(pageLength = 10,
                                  scrollX = TRUE,
                                  scrollY = TRUE,
                                  searching = FALSE,
                                  fixedColumns = list(leftColumns = 1),
                                  dom = 'B<"dwnld">frtip'
                                  # buttons = list(list(extend = 'none'))
                )
      )
  })


  output$download_th2mean_tab <- downloadHandler(
    filename = function() { "Cenarios_tamanho_amostra_duas_medias.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_2mean_print(), path = file)}
  )











  # Estimar  ----

  output$mean2TH_um <- renderUI({
    textInput(inputId = "meanTH2_unidade_medida",
              label   = paste("Descreva a unidade de medida de", input$mean2TH_nome_desfecho),
              value   = "u.m.") %>%
      .help_buttom(body = .txt_um)
  })


  eval(parse(text = warning_prop("conf_TH2_mean_pwr")))
  eval(parse(text = warning_numero_positivo("sigma_TH2_mean_est")))
  eval(parse(text = warning_numero_positivo("TH2_mean_precisao")))
  eval(parse(text = warning_perdas("TH_mean_perdas_recusa_est")))


  output$THmean2_est <- renderText({

    code <- paste0(
      "EnvStats::ciNormN(",
      "half.width = ", input$TH2_mean_precisao, ", ",
      "sigma.hat = ", input$sigma_TH2_mean_est,  ", ",
      "conf.level = ", input$conf_TH2_mean_pwr,  "/100, ",
      "sample.type = 'two.sample')"
    )

    n <- try_n(code)
    eval(parse(text = validate_n("n")))

    n <- n*2
    n_perdas <- n_perdas(n, input$TH_mean_perdas_recusa_est)
    n_perdas <- 2 * ceiling(n_perdas/2)
    eval(parse(text = validate_n_inf("n")))


    paste0(
      "<b><font size = '5'>Tamanho amostral calculado: ", n, " (<i>", n/2, " em cada grupo</i>)",
      "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

      "O cálculo do tamanho de amostra foi realizado por meio da ", .txt_citacao_tap, ", ",
      "para estimar a diferença entre as médias de <b>", input$mean2TH_nome_desfecho, "</b> nos grupos A e B, ",
      "com margem de erro de <b>", input$TH2_mean_precisao, " ", input$meanTH2_unidade_medida,
      "</b>, nível de confiança de <b>", input$conf_TH2_mean_pwr, "%</b>",
      " e desvio padrão esperado de <b>", input$sigma_TH2_mean_est, " ", input$meanTH2_unidade_medida, "</b> como referido em Fulano (1900), ",
      "chegou-se ao tamanho de amostra de <b>", n, " sujeitos </b> (sendo <b>", n/2, "</b> em cada grupo). ",
      if(testar_valor_perdas_valido(input$TH_mean_perdas_recusa_est)){
        paste0(
      "Acrescentando <b>", input$TH_mean_perdas_recusa_est, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas, "</b>.")
      },

      .txt_referencia_tap, print_r_code(code)
    )


  })






  # Poder ----

  eval(parse(text = warning_prop("poder_sig_TH2_mean_pwr")))
  eval(parse(text = warning_inteiro("poder_n1_TH2_mean_pwr")))
  eval(parse(text = warning_inteiro("poder_n2_TH2_mean_pwr")))
  eval(parse(text = warning_numero_positivo("poder_sigma1_TH2_mean_pwr")))
  eval(parse(text = warning_numero_positivo("poder_sigma2_TH2_mean_pwr")))


  observeEvent(input$cohen_TH2_mean_pwr_poder, {
    if(is.na(input$cohen_TH2_mean_pwr_poder)){
      shinyFeedback::showFeedbackWarning(
        inputId = "cohen_TH2_mean_pwr_poder",
        text = "Deve ser fornecido um valor do tamanho de efeito.",
        color = "red"
      )
    } else if (input$cohen_TH2_mean_pwr_poder == 0) {
      shinyFeedback::showFeedbackWarning(
        inputId = "cohen_TH2_mean_pwr_poder",
        text = "O tamanho de efeito deve ser diferente de que zero.",
        color = "red"
      )
    } else {
      shinyFeedback::hideFeedback("cohen_TH2_mean_pwr_poder")
    }
  })


  observeEvent(input$poder_TH2_mean_margin, {
    if(is.na(input$poder_TH2_mean_margin)){
      shinyFeedback::showFeedbackWarning(
        inputId = "poder_TH2_mean_margin",
        text = "Deve ser fornecido um valor da diferença a ser detectada.",
        color = "red"
      )
    } else if(input$poder_TH2_mean_margin == 0){
      shinyFeedback::showFeedbackWarning(
        inputId = "poder_TH2_mean_margin",
        text = "A diferença a ser detectada deve ser diferente de 0.",
        color = "red"
      )
    } else {
      shinyFeedback::hideFeedback("poder_TH2_mean_margin")
    }
  })


  output$poder_th2_mean_formula1 <- renderUI({
    sinal_h0 <- case_when(input$poder_alternative_TH2_mean_pwr == 'two.sided' ~ "=",
                          input$poder_alternative_TH2_mean_pwr == 'one.sided' & sided_two_means_test_power()$alternative_d == "greater"  ~ "\\leq",
                          input$poder_alternative_TH2_mean_pwr == 'one.sided' & sided_two_means_test_power()$alternative_d == "less" ~ "\\geq")

    withMathJax(
      paste0("$$H_0: \\mu_{", "Grupo A", "} ", sinal_h0, " \\mu_{", "Grupo B", "} $$"))
  })

  output$poder_th2_mean_formula2 <- renderUI({
    sinal_h1 <- case_when(input$poder_alternative_TH2_mean_pwr == 'two.sided' ~ "\\neq",
                          input$poder_alternative_TH2_mean_pwr == 'one.sided' & sided_two_means_test_power()$alternative_d == "greater"  ~ ">",
                          input$poder_alternative_TH2_mean_pwr == 'one.sided' & sided_two_means_test_power()$alternative_d == "less" ~ "<")

    withMathJax(
      paste0("$$H_1: \\mu_{", "Grupo A", "}", sinal_h1, " \\mu_{", "Grupo B", "} $$"))
  })




  sided_two_means_test_power <- reactive({

    req(!is.null(input$poder_alternative_TH2_mean_pwr))


    # Se entrar com o d de Cohen
    if(input$th2_pwr_mean_cohen){

      if(input$poder_alternative_TH2_mean_pwr != "two.sided" & input$cohen_TH2_mean_pwr < 0){
        texto_comparacao <- paste0("Foi calculado o poder do teste para detectar que a média de <b>Y</b> no grupo ", th2mean_grupoTratamento(), " é inferior a do ", th2mean_grupoControle(), ", ")
        alternative <- 1
        alternative_d <- "less"
      } else{
        texto_comparacao <- paste0("Foi calculado o poder do teste para detectar que a média de <b>Y</b> no grupo ", th2mean_grupoTratamento(), " é superior a do ", th2mean_grupoControle(), ",")
        alternative <- 1
        alternative_d <- "greater"
      }
    } else {

      if(input$poder_alternative_TH2_mean_pwr != "two.sided" & input$poder_TH2_mean_margin < 0){
        texto_comparacao <- paste0("Foi calculado o poder do teste para detectar que a média de <b>Y</b> no grupo ", th2mean_grupoTratamento(), " é inferior a do ", th2mean_grupoControle(), ", ")
        alternative <- 1
        alternative_d <- "less"
      } else{
        texto_comparacao <- paste0("Foi calculado o poder do teste para detectar que a média de <b>Y</b> no grupo ", th2mean_grupoTratamento(), " é superior a do ", th2mean_grupoControle(), ",")
        alternative <- 1
        alternative_d <- "greater"
      }
    }


    # O de dois lados se sobrepoe
    if(input$poder_alternative_TH2_mean_pwr == "two.sided"){
      texto_comparacao <- paste0("Foi calculado o poder do teste para detectar diferenças na média de <b>", mean2_nome_desfecho(), "</b> entre os grupos ", th2mean_grupoTratamento(), " e ", th2mean_grupoControle(), ", ")
      alternative <- 2
      alternative_d <- "two.sided"
    }

    list(alternative = alternative,
         alternative_d = alternative_d,
         texto_comparacao = texto_comparacao)
  })








  output$THmean2_power <- renderText({
    if(!input$th2_pwr_mean_cohen){

      cohens_d <- cohen_d(mean_diff = input$poder_TH2_mean_margin,
                          n_1 = input$poder_n1_TH2_mean_pwr,
                          n_2 = input$poder_n2_TH2_mean_pwr,
                          sd_1 = input$poder_sigma1_TH2_mean_pwr,
                          sd_2 = input$poder_sigma2_TH2_mean_pwr)

    } else {
      cohens_d <- if(sided_two_means_test_power()$texto_comparacao == "less") - input$cohen_TH2_mean_pwr_poder else input$cohen_TH2_mean_pwr_poder
    }

    code <- paste0(
      "pwr::pwr.t2n.test(",
      "n1 = ", input$poder_n1_TH2_mean_pwr, ", ",
      "n2 = ", input$poder_n2_TH2_mean_pwr, ", ",
      "d = ", cohens_d, ", ",
      "sig.level = ", input$poder_sig_TH2_mean_pwr, "/100, ",
      "power = NULL, ",
      "alternative = '", sided_two_means_test_power()$alternative_d, "')")

    npwr <- eval(parse(text = code))

    cabecalho <- paste0("<b><font size = '5'>Poder calculado: ",round(100*npwr$power, digits = 1),
                        "%</font></b></br></br><i>Sugestão de texto:</i></br></br>")

    if(!input$th2_pwr_mean_cohen){
      texto_desvio <- paste0(sided_two_means_test_power()$texto_comparacao,
                             "tendo uma diferença de <b>", input$poder_TH2_mean_margin, " u.m.</b> como relevante para o estudo. ",
                             "Para isso foi utilizado a ", .txt_citacao_tap, ". ",
                             "Considerando nível de significância de <b>", input$poder_sig_TH2_mean_pwr, "%</b>, ",
                             "tamanho amostral de ", input$poder_n1_TH2_mean_pwr, " para o grupo A e ", input$poder_n2_TH2_mean_pwr, " para o grupo B, ",
                             " desvio padrão de <b>", input$poder_sigma1_TH2_mean_pwr, " u.m.</b> para o grupo A e <b>", input$poder_sigma2_TH2_mean_pwr, " u.m.</b> para o grupo B, ",
                             "chegou-se à um poder de de <b>", round(100*npwr$power, digits = 1), "%</b>.")

      paste0(cabecalho,
             texto_desvio,
             .txt_referencia_tap, print_r_code(code))

    } else{
      texto_tamanho_efeito <- paste0(sided_two_means_test_power()$texto_comparacao,
                                     "por meio da ", .txt_citacao_tap, ". ",
                                     "Considerando nível de significância de <b>", input$poder_sig_TH2_mean_pwr, "%</b>, ",
                                     "tamanho amostral de ", input$poder_n1_TH2_mean_pwr, " para o grupo A e ", input$poder_n2_TH2_mean_pwr, " para o grupo B, ",
                                     "e tamanho de efeito de <b>", round( input$cohen_TH2_mean_pwr, 2), "</b>, ",
                                     "chegou-se à um poder de de <b>", round(100*npwr$power, digits = 1), "%</b>.")
      paste0(cabecalho,
             texto_tamanho_efeito,
             .txt_referencia_tap, print_r_code(code))
    }

  })








  #---------------.
  # Equivalência ----
  #---------------.


  output$margin_mean_inf_eq_sup <- renderUI({

    margem_2_ind <- case_when(input$mean_test_inf_eq_sup == "Não inferioridade" ~ -1,
                              input$mean_test_inf_eq_sup == "Superioridade"     ~ 0.5,
                              TRUE ~ 1)

    numericInput( "margem_2_ind",
                  paste0("Margem de ", input$mean_test_inf_eq_sup),
                  value = margem_2_ind,
                  min   = -Inf,
                  max   = Inf,
                  step  = 1) %>%
      .help_buttom(body = case_when(input$mean_test_inf_eq_sup == "Não inferioridade"  ~ .txt_margem_nao_inferior,
                                    input$mean_test_inf_eq_sup == "Superioridade"      ~ .txt_margem_superior,
                                    TRUE ~ .txt_margem_equivalencia),
                   title = paste0("Margem de ", input$mean_test_inf_eq_sup))
  })


  output$inf_sup_nomesUi <- renderUI({
    label_ <- case_when(input$mean_test_inf_eq_sup == "Não inferioridade" ~ "é não inferior ao grupo:",
                        input$mean_test_inf_eq_sup == "Superioridade"     ~ "é superior ao grupo:",
                        TRUE ~ "é equivalente ao grupo:")

    fluidPage(
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
          textInput(inputId = "inf_sup_groupA",
                    label   = "Comparar se o grupo:",
                    value   = "Experimental")),

      div(style="display: inline-block;vertical-align:top; width: 49%;",
          textInput(inputId = "inf_sup_groupB",
                    label   = label_,
                    value   = "Convencional"))
    )
  })


  output$th2_equi_mean_formula1 <- renderUI({

    if(input$mean_test_inf_eq_sup == "Equivalência"){
      HTML("<b>H<sub>0</sub></b>: A diferença entre as médias <b>não está</b> dentro das margens de equivalência")
    } else {
      HTML(paste0("<b>H<sub>0</sub></b>: A diferença entre as médias é <b>menor ou igual</b> do que a margem de ",
      if(input$mean_test_inf_eq_sup == "Não inferioridade") "não inferioridade" else "superioridade"))
    }
  })

  output$th2_equi_mean_formula2 <- renderUI({

    if(input$mean_test_inf_eq_sup == "Equivalência"){
      HTML("<b>H<sub>1</sub></b>: A diferença entre as médias <b>está</b> dentro das margens de equivalência")
    } else{
      HTML(paste0("<b>H<sub>1</sub></b>: A diferença entre as médias é <b>maior</b> do que a margem de ",
                  if(input$mean_test_inf_eq_sup == "Não inferioridade") "não inferioridade" else "superioridade"))
      }
  })



  output$inf_sup_complementoUi <- renderUI({

    delta_2_ind <- case_when(input$mean_test_inf_eq_sup == "Não inferioridade" ~ -0.5,
                             input$mean_test_inf_eq_sup == "Superioridade"     ~ 1,
                             TRUE ~ 0)

    fluidPage(fluidRow(
      numericInput( "delta_2_ind",
                    paste0("Diferença esperada entre as médias (", input$inf_sup_groupA, " - ", input$inf_sup_groupB, ")"),
                    value = delta_2_ind,
                    min = -Inf,
                    max = Inf,
                    step = .5
      )  %>% .help_buttom(body = paste0("É a diferença que se espera observar entre os dois grupos. ", .txt_definido_literatura),
                          title = "Diferença esperada entre as médias"),
      uiOutput("margin_mean_inf_eq_sup"),
      numericInput( "sigma_2_ind",
                    "Desvio padrão esperado",
                    value = 2,
                    min = 0,
                    max = Inf,
                    step = 1
      ) %>% .help_buttom(body = .txt_dp, title = "Desvio padrão esperado"),

      actionLink("show_desvio_pooled", "Calcular o desvio padrão combinado"),
      br(), br(),


      numericInput( "k_2_ind",
                    paste0("Balanceamento ", input$inf_sup_groupA, ":", input$inf_sup_groupB),
                    value = 1,
                    min = 0,
                    max = Inf,
                    step = .5
      ) %>% .help_buttom(body = .txt_balanceamento, title = "Balanceamento"),
      numericInput( "beta_2_ind",
                    "Poder (%)",
                    value = 80,
                    min = 0,
                    max = 100,
                    step = 1
      ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
      numericInput( "alpha_2_ind",
                    "Nível de significância (%)",
                    value = 5,
                    min = 0,
                    max = 100,
                    step = 1
      ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
      numericInput( "eq_mean_perdas_recusa",
                    "Perdas/ Recusa (%)",
                    value = 10,
                    min = 0,
                    max = 100,
                    step = 1
      ) %>% .help_buttom(body = "Percentual de perdas previstas ao longo da pesquisas. O cálculo do tamanho da amostra é ajustado para compensá-las.", title = "Perdas/ Recusas (%)")
    ))
  })





  observeEvent(c(input$show_desvio_pooled), {
    validate(need(input$show_desvio_pooled > 0, ''))

    eval(parse(text = warning_numero_positivo("pooled_sigma1_modal")))
    eval(parse(text = warning_numero_positivo("pooled_sigma2_modal")))
    eval(parse(text = warning_numero_positivo("pooled_n1_modal")))
    eval(parse(text = warning_numero_positivo("pooled_n2_modal")))


    showModal(
      modalDialog(
        title = "Obter o desvio padrão combinado",
        fluidPage(

          HTML(paste0("<b><font size = '3'>", "Desvio padrão", " do</font></b><br>")),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
              numericInput( "pooled_sigma1_modal",
                            input$inf_sup_groupA,
                            value = 2.0,
                            min = 0,
                            max = Inf,
                            step = .5)),
          div(style="display: inline-block;vertical-align:top; width: 49%;",
              numericInput( "pooled_sigma2_modal",
                            input$inf_sup_groupB,
                            value = 1.5,
                            min = 0,
                            max = Inf,
                            step = .5)),


          HTML(paste0("<b><font size = '3'>", "Tamanho amostral", " do</font></b><br>")),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
              numericInput( "pooled_n1_modal",
                            input$inf_sup_groupA,
                            value = 20,
                            min = 3,
                            max = Inf,
                            step = 1)),
          div(style="display: inline-block;vertical-align:top; width: 49%;",
              numericInput( "pooled_n2_modal",
                            input$inf_sup_groupB,
                            value = 30,
                            min = 3,
                            max = Inf,
                            step = 1)),

          htmlOutput("ferramentas_pooled_modal"),

          p("Foi utilizado a fórmula"),
          withMathJax("$$s_{combinado} = \\sqrt{ \\dfrac{(n_A - 1)s_A^2 + (n_B - 1)s_B^2}{n_A+n_B-2} }$$")

        ),
        easyClose = TRUE,
        footer    = NULL,
        size      = "m"
      )
    )
  })


  output$ferramentas_pooled_modal <- renderText({

    s2a <- input$pooled_sigma1_modal^2
    s2b <- input$pooled_sigma2_modal^2

    n1 <- input$pooled_n1_modal
    n2 <- input$pooled_n2_modal

    numerador   <- (n1 - 1)*s2a + (n2 - 1)*s2b
    denominador <- n1 + n2 - 2

    s_pooled <- sqrt(numerador/denominador)

    paste0("<br><br><b><font size = '5'>",
           "<i>Desvio padrão<sub>combinado</sub></i> = ", round(s_pooled, 4),
           "<br><br><br>")
  })





  problemaMedia <- reactive({

    mensagem_print <- NULL

    if(input$mean_test_inf_eq_sup == "Não inferioridade"){
      if(input$margem_2_ind > 0){
        mensagem_print <- "Para um estudo de não inferioridade, a margem de não inferioridade deve ser um valor negativo."
      } else if(input$margem_2_ind >= input$delta_2_ind){
        mensagem_print <- "Para um estudo de não inferioridade, a margem de não inferioridade deve ser menor do que a diferença esperada."
      }
    }



    if(input$mean_test_inf_eq_sup == "Superioridade"){
      if(input$margem_2_ind < 0){
        mensagem_print <- "Para um estudo de superioridade, a margem de superioridade deve ser um valor positivo."
      } else if(input$margem_2_ind >= input$delta_2_ind){
        mensagem_print <- "Para um estudo de superioridade, a margem de superioridade deve ser menor do que a diferença esperada."
      }
    }



    if(input$mean_test_inf_eq_sup == "Equivalência"){
      if(abs(input$margem_2_ind) <= abs(input$delta_2_ind)){
        mensagem_print <- "Para um estudo de equivalência, o módulo da margem de equivalência deve ser maior do que o módulo da diferença esperada."
      }
    }


    mensagem_print
  })






  eval(parse(text = warning_prop("alpha_2_ind")))
  eval(parse(text = warning_prop("beta_2_ind")))
  eval(parse(text = warning_numero_positivo("sigma_2_ind")))
  eval(parse(text = warning_numero_positivo("k_2_ind")))
  eval(parse(text = warning_perdas("eq_mean_perdas_recusa")))





  output$mean_equivalence_2_ind <- renderText({

    req(!is.null(input$alpha_2_ind))
    req(!is.null(input$beta_2_ind))
    req(!is.null(input$sigma_2_ind))
    req(!is.null(input$k_2_ind))
    req(!is.null(input$margem_2_ind))
    req(!is.null(input$delta_2_ind))


    alpha  = input$alpha_2_ind/100
    power  = input$beta_2_ind/100
    sigma  = input$sigma_2_ind
    k      = input$k_2_ind
    margin = input$margem_2_ind
    delta  = input$delta_2_ind


    if(!is.null(problemaMedia())){
      shinyFeedback::showFeedbackWarning(
        inputId = "margem_2_ind",
        text = " ",
        color = "red"
      )

      shinyFeedback::showFeedbackWarning(
        inputId = "delta_2_ind",
        text = " ",
        color = "red"
      )

      problemaMedia()
    } else {


      shinyFeedback::hideFeedback("margem_2_ind")
      shinyFeedback::hideFeedback("delta_2_ind")

      if(input$mean_test_inf_eq_sup == "Equivalência"){

        code <- paste0(
          "epiR::epi.ssequc(",
          "control = 0, ",
          "treat = ", margin, ", ",
          "n = NA, ",
          "sd = ",  sigma, ", ",
          "delta = ",  delta, ", ",
          "r = ",  k, ", ",
          "alpha = ", alpha, ", ",
          "power  = ",  power, ")"
        )

      } else if(input$mean_test_inf_eq_sup == "Não inferioridade"){


        code <- paste0(
          "epiR::epi.ssninfc(",
          "control = 0, ",
          "treat = ", margin, ", ",
          "n = NA, ",
          "sd = ",  sigma, ", ",
          "delta = ",  delta, ", ",
          "r = ",  k, ", ",
          "alpha = ", alpha, ", ",
          "power  = ",  power, ")"
        )

      } else {

        code <- paste0(
          "epiR::epi.sssupc(",
          "control = 0, ",
          "treat = ", margin, ", ",
          "n = NA, ",
          "sd = ",  sigma, ", ",
          "delta = ",  delta, ", ",
          "r = ",  k, ", ",
          "alpha = ", alpha, ", ",
          "power  = ",  power, ")"
        )

      }



      n <- try_n(code)
      eval(parse(text = validate_n("n")))

      n1 <- n$n.treat
      n2 <- n$n.control
      n <- n1 + n2

      nperdas1 <- n_perdas(n1, input$eq_mean_perdas_recusa)
      nperdas2 <- n_perdas(n2, input$eq_mean_perdas_recusa)
      eval(parse(text = validate_n_inf("n")))



      cabecalho <- paste0("<b><font size = '5'>Tamanho amostral calculado: ", n, " (", n1, " ", input$inf_sup_groupA, " e ", n2, " ", input$inf_sup_groupB, ")",
                          "</font></b></br></br><i>Sugestão de texto:</i></br></br>")



      #---- Texto do teste
      if(input$mean_test_inf_eq_sup == "Equivalência"){
        texto_comparacao <- paste0("Foi calculado o tamanho de amostra para testar a <b>equivalência</b>, em termos de médias de <b>Y</b>, entre os grupos ", input$inf_sup_groupA, " e ", input$inf_sup_groupB,
                                   " por meio da ", .txt_citacao_tap, ". ",
                                   "Considerando uma margem de equivalência de <b>", margin, " u.m.</b>"
        )
      } else if(input$mean_test_inf_eq_sup == "Não inferioridade"){
        texto_comparacao <- paste0("Foi calculado o tamanho de amostra para testar a <b>não inferioridade</b>, em termos de médias de <b>Y</b>, do grupo ", input$inf_sup_groupA, " em relação ao grupo ", input$inf_sup_groupB,
                                   " por meio da ", .txt_citacao_tap, ". ",
                                   # " utilizando as fómulas descritas por Julious, S.A. (2009). ",
                                   "Considerando uma margem de não inferioridade de <b>", margin, " u.m.</b>"
        )
      } else{
        texto_comparacao <- paste0("Foi calculado o tamanho de amostra para testar a <b>superioridade</b>, em termos de médias de <b>Y</b>, do grupo ", input$inf_sup_groupA, " em relação ao grupo ", input$inf_sup_groupB,
                                   " por meio da ", .txt_citacao_tap, ". ",
                                   "Considerando uma margem de superioridade de <b>", margin, " u.m.</b>"
        )
      }
      #----------------.


      texto_final <- paste0(", poder de <b>", input$beta_2_ind, "%</b>, ",
                            "nível de significância de <b>", input$alpha_2_ind, "%</b>, ",
                            "diferença entre as médias de <b>", delta, " u.m.</b> ",
                            "e desvio padrão esperado de <b>", input$sigma_2_ind, " u.m.</b> (dados de Fulano (1900)), ",
                            "chegou-se ao tamanho total da amostra de <b>", n, "</b> sujeitos, ",

                            if(input$k_2_ind == 1){
                              paste0(
                                "sendo <b>", n1, "</b> sujeitos em cada grupo. ",
                                if(testar_valor_perdas_valido(input$eq_mean_perdas_recusa)){
                                  paste0("Acrescentando <b>", input$eq_mean_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", nperdas1 + nperdas2, "</b>.")
                                }
                              )
                            } else{
                              paste0(
                                "sendo <b>", n1, "</b> sujeitos no grupo ", input$inf_sup_groupA, " e ", n2, " sujeitos no grupo ", input$inf_sup_groupB,
                                if(testar_valor_perdas_valido(input$eq_mean_perdas_recusa)){
                                  paste0(" Acrescentando <b>", input$eq_mean_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", nperdas1 + nperdas2, "</b>",
                                         " (<b>", nperdas1, "</b> no grupo ", input$inf_sup_groupA, " e <b>", nperdas2, "</b> no grupo ", input$inf_sup_groupB, ").")
                                }
                              )
                            }

                            #

      )


      paste0(cabecalho,
             texto_comparacao,
             texto_final,
             .txt_referencia_tap, print_r_code(code)
      )
    }

  }
  )





  output$plot_eq_medias <- renderPlot({

    req(!is.null(input$alpha_2_ind))
    req(!is.null(input$beta_2_ind))
    req(!is.null(input$sigma_2_ind))
    req(!is.null(input$k_2_ind))
    req(!is.null(input$margem_2_ind))
    req(!is.null(input$delta_2_ind))

    # req(problemaMedia() == "ok")
    delta   <- input$delta_2_ind
    margin  <- input$margem_2_ind
    desvio <- input$sigma_2_ind

    # delta = 0.5; margin = 1; desvio = 2
    #
    if(input$mean_test_inf_eq_sup == "Não inferioridade"){
      testee <- annotate(geom = "text",
                         x = margin,
                         y = 16,
                         label = "Margem não inferioridade",
                         color = "red",
                         hjust = 0)
    } else if(input$mean_test_inf_eq_sup == "Superioridade"){
      testee <- annotate(geom = "text",
                         x = margin,
                         y = 16,
                         label = "Margem superioridade",
                         color = "red",
                         hjust = 1)
    } else{
      testee <- annotate(geom = "text",
                         x = margin,
                         y = 16,
                         label = "Margem de equivalência",
                         color = "red",
                         hjust = 0)
    }


    p <- tibble::tibble(x = seq(min(delta, margin) - 0.5*desvio, max(2, desvio), 0.01)) %>%
      mutate(y = 0) %>%
      ggplot(aes(x = x, y = y)) +
      geom_blank() +

      annotate(geom = "curve",
               x = 0,
               y = 10,
               xend = 0,
               yend = 20,
               curvature = 0) +
      annotate(geom = "curve",
               x = margin,
               y = 10,
               xend = margin,
               yend = 15,
               curvature = 0,
               colour = "red",
               linetype="dashed") +
      annotate(geom = "curve",
               x = delta,
               y = 10,
               xend = delta,
               yend = 15,
               curvature = 0,
               colour = "blue",
               linetype="dashed") +


      annotate(geom = "text",
               x = 0.1,
               y = 20,
               label = paste0("Favorável ao ", input$inf_sup_groupA),
               color = "black",
               fontface = "bold",
               hjust = 0) +
      annotate(geom = "text",
               x = -0.1,
               y = 20,
               label = paste0("Favorável ao ", input$inf_sup_groupB),
               color = "black",
               fontface = "bold",
               hjust = 1) +

      testee +
      annotate(geom = "text",
               x = delta,
               y = 18,
               label = "Diferença esperada entre as médias",
               color = "blue",
               hjust = ifelse(input$mean_test_inf_eq_sup == "Não inferioridade", 0, 1)) +

      annotate(geom = "curve",
               x = margin ,
               y = 16-0.2,
               xend = margin,
               yend = 14,
               colour = "red",
               curvature = -.3,
               arrow = arrow(length = unit(2, "mm"))) +

      annotate(geom = "curve",
               x = delta ,
               y = 18 - 0.2,
               xend = delta,
               yend = 14,
               colour = "blue",
               curvature = .3,
               arrow = arrow(length = unit(2, "mm"))) +

      theme_classic() +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank()) +
      ylim(c(10,20)) +
      xlab(paste0("Diferença entre as médias (", input$inf_sup_groupA, " - ", input$inf_sup_groupB, ")"))


    if(input$mean_test_inf_eq_sup == "Equivalência"){
      p +
        annotate(geom = "text",
                 x = -margin,
                 y = 16,
                 label = "Margem de equivalência",
                 color = "red",
                 hjust = 0) +
        annotate(geom = "curve",
                 x = - margin,
                 y = 10,
                 xend = -margin,
                 yend = 15,
                 curvature = 0,
                 colour = "red",
                 linetype="dashed") +
        annotate(geom = "curve",
                 x = -margin ,
                 y = 16-0.2,
                 xend = -margin,
                 yend = 14,
                 colour = "red",
                 curvature = -.3,
                 arrow = arrow(length = unit(2, "mm")))
    } else{
      p
    }

  })





  #___________----
  # 2 proporcao ----
  #---------------.


  # Estimar ----

  # observeEvent(input$p2_TH_ratio_est, {
  #
  #   if(!is.null(input$p2_TH_ratio_est & !is.null(input$p2_TH_prop2_est))){
  #   if(!is.na(input$p2_TH_ratio_est & !is.na(input$p2_TH_prop2_est))){
  #
  #     if((100/input$p2_TH_prop2_est) < input$p2_TH_ratio_est){
  #
  #       shinyFeedback::showFeedbackWarning(
  #         inputId = "p2_TH_ratio_est",
  #         text = paste0("Dado o percentual inserido no grupo Controle acima, o risco relativo não pode ser maior do que ",
  #                       round(1/input$p2_TH_prop2_est/100, 2)),
  #         color = "red"
  #       )
  #     } else {
  #       shinyFeedback::hideFeedback("p2_TH_ratio_est")
  #     }
  #   }
  #   }
  # })

  output$perc_controle_estimar <- renderUI({
    numericInput( "p2_TH_prop2_est",
                  paste0("% de ", input$prop2_nome_desfecho_est, " esperado no grupo Controle"),
                  value = 15,
                  min = 0,
                  max = 100,
                  step = 1
    ) %>% .help_buttom(body = .txt_perc_esperado)
  })

  output$perc_tratamento_estimar <- renderUI({
    numericInput( "p1_TH_prop2_est",
                  paste0("% de ", input$prop2_nome_desfecho_est, " esperado no grupo Tratamento"),
                  value = 45,
                  min = 0,
                  max = 100,
                  step = 1
    ) %>% .help_buttom(body = .txt_perc_esperado)
  })


  eval(parse(text = warning_numero_positivo("p2_TH_ratio_est")))
  eval(parse(text = warning_numero_positivo("p2_TH_odds_est")))

  eval(parse(text = warning_prop("p1_TH_prop2_est")))
  eval(parse(text = warning_prop("p2_TH_prop2_est")))
  eval(parse(text = warning_prop("prop2_TH_precisao")))

  eval(parse(text = warning_numero_positivo("k_TH_prop2_est")))
  eval(parse(text = warning_prop("conf_TH_prop2")))
  eval(parse(text = warning_perdas("TH_prop_perdas_recusa_est")))





  output$THprop2_est <- renderText({

    req(!(is.null(input$p2_TH_prop2_est) | is.null(input$p1_TH_prop2_est)))
    req(!(is.na(input$p2_TH_prop2_est) | is.na(input$p1_TH_prop2_est)))


    if(input$prop2_estatistica_B_est == "percent"){
      p2 <- input$p1_TH_prop2_est/100

      text_just <- paste0("e uma proporção esperada de <b>", input$prop2_nome_desfecho_est, "</b> no grupo Tratamento de <b>", input$p1_TH_prop2_est, "% </b>",
                          "e no grupo Controle de <b>", input$p2_TH_prop2_est, "%</b> como é referida em Fulano (1900), ")

    } else if (input$prop2_estatistica_B_est == "ratio") {
      p2 <- (input$p2_TH_prop2_est/100)*input$p2_TH_ratio_est

      text_just <- paste0("e uma proporção esperada de <b>", input$prop2_nome_desfecho_est, "</b> no grupo Controle de <b>", input$p2_TH_prop2_est, "% </b>",
                          "e um risco relativo de <b>", input$p2_TH_ratio_est, "</b> como é referida em Fulano (1900) <b>OU</b> escolha do pesquisador, ")

    } else {
      # https://stats.stackexchange.com/questions/324410/converting-odds-ratio-to-percentage-increase-reduction
      prob_control <- input$p2_TH_prop2_est/100
      p2 <- (input$p2_TH_odds_est*prob_control)/ (1 + input$p2_TH_odds_est*prob_control - prob_control)

      text_just <- paste0("e uma proporção de <b>", input$prop2_nome_desfecho_est, "</b> no grupo Controle de <b>", input$p2_TH_prop2_est, "% </b>",
                          "e uma razão de chance de <b>", input$p2_TH_odds_est, "</b> como é referida em Fulano (1900) <b>OU</b> escolha do pesquisador, ")
    }




    ratio_controle_caso <- input$k_TH_prop2_est
    if(ratio_controle_caso >= 1){
      p1a <- input$p2_TH_prop2_est/100
      p2a <- p2
    } else{
      p2a <- input$p2_TH_prop2_est/100
      p1a <- p2
    }

    code <- paste0(
      "EnvStats::ciBinomN(half.width      = ", input$prop2_TH_precisao/100, ", ",
      "p.hat.or.p1.hat = ", p1a, ", ",
      "p2.hat          = ", p2a, ", ",
      "sample.type     = 'two.sample', ",
      "conf.level      = ", input$conf_TH_prop2/100, ", ",
      "ratio           = ", ifelse(ratio_controle_caso >= 1, ratio_controle_caso, 1/ratio_controle_caso), ", ",
      "correct         = ", input$prop_correction_est, ", ",
      "ci.method       = '", input$prop_TH_est_method, "', ",
      "n.or.n1.max	    = 1E8)")

    n <- eval(parse(text = code))
    eval(parse(text = validate_n("n")))


    # n <- n_est2_prop(prop_controle   = input$p2_TH_prop2_est,
    #                  prop_tratamento = p2*100,
    #                  confianca       = input$conf_TH_prop2,
    #                  precisao        = input$prop2_TH_precisao,
    #                  ratio_controle_caso = input$k_TH_prop2_est,
    #                  correct   = input$prop_correction_est,
    #                  ci.method = input$prop_TH_est_method,
    #                  type_info = "both")

    if(ratio_controle_caso >= 1){
      n1 <- n$n1
      n2 <- n$n2
    # } else if(ratio_controle_caso == 1){
    #   n1 <- n
    #   n2 <- n
    } else{
      n1 <- n$n2
      n2 <- n$n1
    }

    # n1 <- n$n1
    # n2 <- n$n2
    n <- n1 + n2

    nperdas1 <- n_perdas(n1, input$TH_prop_perdas_recusa_est)
    nperdas2 <- n_perdas(n2, input$TH_prop_perdas_recusa_est)
    eval(parse(text = validate_n_inf("n")))


    poder_teste <- EnvStats::propTestPower(n.or.n1 = n1,
                                           p.or.p1 = input$p2_TH_prop2_est/100,
                                           n2      = n2,
                                           p0.or.p2    = p2,
                                           alpha       = 1 - input$conf_TH_prop2/100,
                                           sample.type = "two.sample",
                                           correct     = input$prop_correction_est)



    cabecalho <- paste0("<b><font size = '5'>Tamanho amostral calculado: ", n, " (<i>", n1, " no Tratamento e ", n2, " no grupo Controle</i>)",
                        "</font></b></br></br><i>Sugestão de texto:</i></br></br>")

    texto_comparacao <- paste0("Foi calculado o tamanho de amostra para estimar a diferença entre as proporções de <b>",
                               input$prop2_nome_desfecho_est, "</b> entre os Tratamentos e Controles com uma margem de erro de <b>", input$prop2_TH_precisao, "%</b>",
                               " por meio da ", .txt_citacao_tap, ". ",
                               "Considerando nível de confiança de <b>", input$conf_TH_prop2, "% </b>",
                               text_just)


    if(input$k_TH_prop2_est == 1){

      texto_grupos <- paste0("chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n1, "</b> em cada grupo.",
                             if(testar_valor_perdas_valido(input$TH_prop_perdas_recusa_est)){
                               paste0(" Acrescentando <b>", input$TH_prop_perdas_recusa_est, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
                                      "deverá ser <b>", nperdas1 + nperdas2, "</b>.")
                             })
    } else{

      texto_grupos <- paste0("chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n1, "</b> no Tratamento e <b>", n2, "</b> no grupo Controle.",
                             if(testar_valor_perdas_valido(input$TH_prop_perdas_recusa_est)){
                               paste0(" Acrescentando <b>", input$TH_prop_perdas_recusa_est, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
                                      "deverá ser <b>", nperdas1 + nperdas2, "</b> (", nperdas1, " no Tratamento e ", nperdas2, " no grupo Controle).")
                             })
    }

    paste0(cabecalho,
           texto_comparacao,
           texto_grupos,

           "</br></br></br><i>Obs.:</i> Com esse tamanho de amostra o poder do teste, para comparar a igualdade entre as proporções, será aproximadamente <b>",
           round(poder_teste*100, 1), "</b>%.",

           "</br></br>",
           .txt_referencia_tap, print_r_code(code)
    )
  })




  # eval(parse(text = check_text_input_to_vector("precisao_p2_EST_plot")))
  #
  #
  # # COnstroi a tabela com os cenarios
  # tab_p2_EST_cenarios <- reactive({
  #
  #
  #   precisao <- text_input_to_vector(input$precisao_p2_EST_plot)
  #   req(length(precisao) > 0)
  #
  #
  #   if(input$prop2_estatistica_B_est == "percent"){
  #     rrrr <- NA
  #     odssss <- NA
  #     prop_tratamento <- seq(from = input$p2_EST_from, to = input$p2_EST_to, by = input$p2_EST_by)/100
  #
  #   } else if (input$prop2_estatistica_B_est == "ratio") {
  #
  #     rrrr <- seq(from = input$rr_p2_EST_from, to = input$rr_p2_EST_to, by = input$rr_p2_EST_by)
  #     odssss <- NA
  #     prop_tratamento <- (input$p2_TH_prop2_est/100)*rrrr
  #
  #   } else {
  #
  #     rrrr <- NA
  #     odssss <- seq(from = input$ods_p2_EST_from, to = input$ods_p2_EST_to, by = input$ods_p2_EST_by)
  #     prob_control <- input$p2_TH_prop2_est/100
  #     prop_tratamento <- (odssss*prob_control)/ (1 + odssss*prob_control - prob_control)
  #
  #   }
  #
  #
  #   df_inputs_prop <- tibble::tibble(rrrr, odssss, prop_tratamento)
  #
  #   # df_inputs_prop
  #
  #
  #   simul_n <- expand.grid(prop_controle   = input$p2_TH_prop2_est,
  #                          prop_tratamento = prop_tratamento*100,
  #                          confianca       = input$conf_TH_prop2,
  #                          precisao           = precisao,
  #                          ratio_controle_caso = input$k_TH_prop2_est,
  #                          correct        = input$prop_correction_est,
  #                          ci.method      = input$prop_TH_est_method,
  #                          stringsAsFactors = FALSE)
  #
  #   simul_n_grid_n <- dplyr::filter(simul_n, prop_tratamento != prop_controle) %$%
  #     purrr::pmap_dfr(.l = list(prop_controle, prop_tratamento, confianca, precisao,
  #                               ratio_controle_caso, correct, ci.method),
  #                     .f = n_est2_prop)
  #
  #   simul_n_grid_n %>%
  #     left_join(df_inputs_prop, by = "prop_tratamento") %>%
  #     rename(`Risco relativo` = rrrr,
  #            `Razão de chance` = odssss)
  #
  #
  # })
  #
  #
  # output$p2_EST_plot <- plotly::renderPlotly({
  #
  #   req(!(is.null(input$p2_TH_prop2_est) | is.null(input$precisao_p2_EST_plot)))
  #   # req(!(is.na(input$p2_TH_prop2_est) | is.na(input$p2_EST_from) | is.na(input$power_p2_EST_plot)))
  #   df <- tab_p2_EST_cenarios() %>%
  #     mutate(`Precisão (%)` = factor(`Precisão (%)`)) %>%
  #     filter(!is.na(`n total`))
  #
  #   if(input$prop2_estatistica_B_est == 'percent'){
  #
  #     g1 <- df %>%
  #       ggplot(aes(x = `% no Tratamento`,
  #                  y = `n total`,
  #                  color = `Precisão (%)`,
  #                  Tratamento = `n Tratamento`,
  #                  Controle   = `n Controle`))+
  #       geom_line() +
  #       geom_point() +
  #       geom_vline(xintercept=input$p2_TH_prop2_est, linetype="dashed", color = "red") +
  #       scale_x_continuous(breaks = seq(from = input$p2_EST_from, to = input$p2_EST_to, by = input$p2_EST_by)) +
  #       xlab("% no Tratamento") +
  #       ylab("Tamanho da amostra*")
  #   } else if(input$prop2_estatistica_B_est == 'ratio'){
  #
  #     g1 <- ggplot(df,
  #                  aes(x = `Risco relativo`,
  #                      y = `n total`,
  #                      color = `Precisão (%)`,
  #                      Tratamento = `n Tratamento`,
  #                      Controle   = `n Controle`))+
  #       geom_line() +
  #       geom_point() +
  #       # geom_vline(xintercept=input$p2_TH_prop2_est, linetype="dashed", color = "red") +
  #       scale_x_continuous(breaks = seq(from = input$rr_p2_EST_from, to = input$rr_p2_EST_to, by = input$rr_p2_EST_by)) +
  #       xlab("Risco relativo (Tratamento/Controle)") +
  #       ylab("Tamanho da amostra*")
  #   } else{
  #
  #     g1 <- ggplot(df,
  #                  aes(x = `Razão de chance`,
  #                      y = `n total`,
  #                      color = `Precisão (%)`,
  #                      Tratamento = `n Tratamento`,
  #                      Controle   = `n Controle`))+
  #       geom_line() +
  #       geom_point() +
  #       # geom_vline(xintercept=input$p2_TH_prop2_est, linetype="dashed", color = "red") +
  #       scale_x_continuous(breaks = seq(from = input$rr_p2_EST_from, to = input$rr_p2_EST_to, by = input$rr_p2_EST_by)) +
  #       xlab("Razão de chance (Tratamento/Controle)") +
  #       ylab("Tamanho da amostra*")
  #   }
  #
  #
  #
  #   plotly::ggplotly(g1,
  #                    tooltip = c("x", "colour", "y", "Tratamento", "Controle")) %>%
  #     plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
  #                               showarrow = F, xref='paper', yref='paper',
  #                               xanchor='right', yanchor='auto', xshift=0, yshift=0,
  #                               font=list(size=10)))
  # })
  #
  #
  # return_table_p2_EST_tab <- reactive({
  #   tab_p2_EST_cenarios()
  # })
  #
  #
  #
  # output$p2_EST_tab <- DT::renderDataTable({
  #
  #   req(!(is.null(input$p2_TH_prop2_est) | is.null(input$precisao_p2_EST_plot)))
  #   # req(!(is.na(input$p2_TH_prop2_est) | is.na(input$p2_EST_from) | is.na(input$power_p2_EST_plot)))
  #
  #
  #   return_table_p2_EST_tab() %>%
  #     DT::datatable(extensions = c('FixedColumns'),
  #               rownames   = FALSE,
  #               filter     = "none",
  #               #callback   = DT::JS("$('div.dwnld').append($('#download_p2_EST_tab'));"),
  #               options    = list(pageLength = 10,
  #                                 scrollX = TRUE,
  #                                 scrollY = TRUE,
  #                                 searching = FALSE,
  #                                 fixedColumns = list(leftColumns = 1),
  #                                 dom = 'B<"dwnld">frtip'
  #                                 # buttons = list(list(extend = 'none'))
  #               )
  #     )
  # })
  #
  #
  # output$download_p2_EST_tab <- downloadHandler(
  #   filename = function() { "Cenarios_tamanho_amostra_duas_prop_estimar.xlsx"},
  #   content = function(file) {writexl::write_xlsx(return_table_p2_EST_tab(),
  #                                                 path = file)}
  # )









  #--------------------.
  #     Testar    ----
  #--------------------.


  observeEvent(input$show_th_2prop, {
    showModal(
      modalDialog(
        title = "Ajustes",
        fluidPage(

          HTML("<b>Preencha os campos abaixo de acordo com seu estudo para que sirvam de guia no preenchimento dos demais campos</b>."),
          br(), br(),
          textInput(inputId = "prop2_nome_desfecho",
                    label   = "Descreva o nome do desfecho",
                    value   = ifelse(input$show_th_2prop == 0, "Y", prop2_nome_desfecho())),
          HTML(paste0("<i>", str_remove_all(.txt_desfecho, "<br><br>"), "</i>")),
          br(), br(),
          textInput(inputId = "th2prop_grupoTratamento",
                    label   = "Descreva um nome para o grupo Tratamento",
                    value   = ifelse(input$show_th_2prop == 0, "Tratamento", th2prop_grupoTratamento())),

          HTML("<i>Em alguns estudos o grupo Tratamento também pode ser chamadado de grupo Intervenção ou grupo Exposto.</i><br><br>"),

          textInput(inputId = "th2prop_grupoControle",
                    label   = "Descreva um nome para o grupo Controle",
                    value   = ifelse(input$show_th_2prop == 0, "Controle", th2prop_grupoControle())),

          HTML("<i>Em alguns estudos o grupo Controle também pode ser chamadado de grupo Placebo/ Sham ou grupo Não exposto.</i>"),


        ),
        easyClose = TRUE,
        footer    = NULL
      )
    )
  })


  th2prop_grupoControle <- reactive({
    ifelse(is.null(input$th2prop_grupoControle), "Controle", input$th2prop_grupoControle)
  })

  th2prop_grupoTratamento <- reactive({
    ifelse(is.null(input$th2prop_grupoTratamento), "Tratamento", input$th2prop_grupoTratamento)
  })

  prop2_nome_desfecho <- reactive({
    ifelse(is.null(input$prop2_nome_desfecho), "Y", input$prop2_nome_desfecho)
  })


  # cenarios_perc_tratUi <- renderUI({
  #   fluidPage(
  #     HTML(paste0("<b>Defina a sequência de valores (%) para o grupo ", th2prop_grupoTratamento(), ":</b>"))
  #   )
  # })

  output$perc_controle_testar <- renderUI({
    numericInput( "p2_TH_prop2",
                  paste0("% de ", prop2_nome_desfecho(), " esperado no grupo ", th2prop_grupoControle()),
                  value = 15,
                  min = 0,
                  max = 100,
                  step = 1
    ) %>% .help_buttom(body = .txt_perc_esperado)
  })

  output$prop2_estatistica_BUi <- renderUI({

    nomes_opcoes_medida <- c(
      paste0("Razão de chance (", th2prop_grupoTratamento(), "/", th2prop_grupoControle(), ")"),
      paste0("Risco relativo (", th2prop_grupoTratamento(), "/", th2prop_grupoControle(), ")"),
      paste0("% esperado no grupo ", th2prop_grupoTratamento())
    )
    opcoes_medida <- c('odds', 'ratio', "percent")
    names(opcoes_medida) <- nomes_opcoes_medida


    selectInput('prop2_estatistica_B',
                paste0('Medida do grupo ', th2prop_grupoTratamento()),
                choices = opcoes_medida,
                selected = 'percent'
    )
  })

  output$k_TH_prop2Ui <- renderUI({
    numericInput( "k_TH_prop2",
                  paste0("Balanço da amostra (", th2prop_grupoControle(), ":", th2prop_grupoTratamento(), ")"),
                  value = 1,
                  min   = 0,
                  max   = Inf,
                  step  = .5
    ) %>% .help_buttom(
      paste0("Nº de ", th2prop_grupoControle(), " para cada ", th2prop_grupoTratamento(), ". Se colocar o valor 2, será calculado um tamanho de amostra ",
             "tal que será necessário 2 ", th2prop_grupoControle(), " para cada ", th2prop_grupoTratamento(), " Se colocar o valor 0.5, ",
             "será calculado um tamanho de amostra ",
             "tal que será necessário 2 ", th2prop_grupoTratamento(), " para cada ", th2prop_grupoControle(), ".")
    )
  })


  output$perc_tratamento_testar <- renderUI({
    numericInput( "p1_TH_prop2",
                  paste0("% de ", prop2_nome_desfecho(), " esperado no grupo ", th2prop_grupoTratamento()),
                  value = 45,
                  min = 0,
                  max = 100,
                  step = 1
    ) %>% .help_buttom(body = .txt_perc_esperado)
  })


  # output$alternative_TH2_prop_pwr2Ui <- renderUI({
  #   nomes_opcoes_alternativa <- c(
  #     paste0('A % no grupo ', th2prop_grupoTratamento(), ' é DIFERENTE da % do grupo ', th2prop_grupoControle()),
  #     paste0('A % no grupo ', th2prop_grupoTratamento(), ' é MAIOR do que a % do grupo ', th2prop_grupoControle()),
  #     paste0('A % no grupo ', th2prop_grupoTratamento(), ' é MENOR do que a % do grupo ', th2prop_grupoControle())
  #   )
  #
  #   opcoes_alternativa <- c('two.sided', 'greater', "less")
  #   names(opcoes_alternativa) <- nomes_opcoes_alternativa
  #
  #
  #   selectInput('alternative_TH2_prop_pwr2',
  #               'Tipo de teste de acordo com hipótese alternativa:',
  #               choices = opcoes_alternativa,
  #               selected = 'two.sided'
  #   ) %>% .help_buttom(body = .txt_h1)
  # })


  alternative_TH2_prop2 <- reactive({

    if(input$alternative_TH2_prop_pwr2 == "Bilateral"){
      "two.sided"
    } else if(input$prop2_estatistica_B == "percent"){
      ifelse(input$p1_TH_prop2 > input$p2_TH_prop2, "greater", "less")
    } else if(input$prop2_estatistica_B == "ratio"){
      ifelse(input$p2_TH_ratio > 1, "greater", "less")
    } else if(input$prop2_estatistica_B == "odds"){
      ifelse(input$p2_TH_odds > 1, "greater", "less")
    }

  })

  # output$lala <- renderText({alternative_TH2_prop2()})

  output$th2_prop_formula1 <- renderUI({
    req(!is.null(alternative_TH2_prop2()))

    sinal_h0 <- case_when(alternative_TH2_prop2() == 'two.sided' ~ "=",
                          alternative_TH2_prop2() == 'greater'   ~ "\\leq",
                          alternative_TH2_prop2() == 'less'      ~ "\\geq")

    withMathJax(
      paste0("$$H_0: \\pi_{", th2prop_grupoTratamento(), "} ", sinal_h0, " \\pi_{", th2prop_grupoControle(), "}$$"))
  })

  output$th2_prop_formula2 <- renderUI({
    req(!is.null(alternative_TH2_prop2()))

    sinal_h1 <- case_when(alternative_TH2_prop2() == 'two.sided' ~ "\\neq",
                          alternative_TH2_prop2() == 'greater'   ~ ">",
                          alternative_TH2_prop2() == 'less'      ~ "<")

    withMathJax(
      paste0("$$H_1: \\pi_{", th2prop_grupoTratamento(), "}", sinal_h1, " \\pi_{", th2prop_grupoControle(), "}$$"))
  })

  eval(parse(text = warning_numero_positivo("p2_TH_ratio")))
  eval(parse(text = warning_numero_positivo("p2_TH_odds")))

  eval(parse(text = warning_prop("p1_TH_prop2")))
  eval(parse(text = warning_prop("p2_TH_prop2")))

  eval(parse(text = warning_numero_positivo("k_TH_prop2")))
  eval(parse(text = warning_prop("beta_TH_prop2")))
  eval(parse(text = warning_prop("alpha_TH_prop2")))
  eval(parse(text = warning_perdas("TH_prop_perdas_recusa")))





  output$THprop2 <- renderText({

    req(!(is.null(input$p2_TH_prop2) | is.null(input$p2_TH_prop2) | is.null(input$p1_TH_prop2)))
    # req(!(is.na(input$p2_TH_prop2) | is.na(input$p2_TH_prop2) | is.na(input$p1_TH_prop2)))

    validate(
      need(!is.na(input$p2_TH_prop2), paste0("É obrigatório fornecer um valor do % do grupo ", th2prop_grupoControle(), ".")),
      need(!is.na(input$k_TH_prop2), "É obrigatório fornecer um valor do balanço da amostra."),
      need(!is.na(input$beta_TH_prop2), "É obrigatório fornecer um valor do poder."),
      need(!is.na(input$alpha_TH_prop2),   "É obrigatório fornecer um valor do nível de significância."),
      need(!is.na(input$k_TH_prop2), "É obrigatório fornecer um valor para o balanço da amostra."),

      need(input$p2_TH_prop2 > 0 & input$p2_TH_prop2 < 100, paste0("O % do grupo ", th2prop_grupoControle(), " deve ser maior do que 0% e menor do que 100%.")),
      need(input$k_TH_prop2 > 0, "O balanço da amostra deve ser maior do que zero."),
      need(input$beta_TH_prop2 > 0, "O poder deve ser maior do que zero."),
      need(input$alpha_TH_prop2 > 0,   "O nível de significância deve ser maior do que zero.")
      # need(input$k_TH_prop2 >= 1,  "O balanço deve ser maior ou igual a 1")
    )



    if(input$prop2_estatistica_B == "percent"){
      validate(
        need(!is.na(input$p1_TH_prop2), paste0("É obrigatório fornecer um valor do % do grupo ", th2prop_grupoTratamento())),
        need(input$p1_TH_prop2 > 0 & input$p1_TH_prop2 < 100, paste0("O % do grupo ", th2prop_grupoTratamento(), " deve ser maior do que 0% e menor do que 100%."))
      )

      p2 <- input$p1_TH_prop2/100
      text_just <- paste0("e proporção de <b>", prop2_nome_desfecho(), "</b> no ", th2prop_grupoTratamento(), " de <b>", input$p1_TH_prop2, "% </b>",
                          "e no ", th2prop_grupoControle(), " de <b>", input$p2_TH_prop2, "%</b> como é referida em Fulano (1900), ")

    } else if (input$prop2_estatistica_B == "ratio") {
      validate(
        need(!is.na(input$p2_TH_ratio), "É obrigatório fornecer um valor do risco relativo."),
        need(input$p2_TH_ratio > 0 & input$p2_TH_ratio != 1, "O risco relativo deve ser maior do que 0 e diferente de 1.")
      )

      p2 <- (input$p2_TH_prop2/100)*input$p2_TH_ratio
      text_just <- paste0("e proporção de <b>", prop2_nome_desfecho(), "</b> no ", th2prop_grupoControle(), " de <b>", input$p2_TH_prop2, "% </b>",
                          "e risco relativo de <b>", input$p2_TH_ratio, "</b> como é referida em Fulano (1900) <b>OU</b> escolha do pesquisador, ")

    } else {
      validate(
        need(!is.na(input$p2_TH_odds), "É obrigatório fornecer um valor da razão de chances."),
        need(input$p2_TH_odds > 0 & input$p2_TH_odds != 1, "A razão de chances deve ser maior do que 0 e diferente de 1.")
      )

      # https://stats.stackexchange.com/questions/324410/converting-odds-ratio-to-percentage-increase-reduction
      prob_control <- input$p2_TH_prop2/100
      p2 <- (input$p2_TH_odds*prob_control)/ (1 + input$p2_TH_odds*prob_control - prob_control)
      text_just <- paste0("e proporção de <b>", prop2_nome_desfecho(), "</b> no ", th2prop_grupoControle(), " de <b>", input$p2_TH_prop2, "% </b>",
                          "e razão de chance de <b>", input$p2_TH_odds, "</b> como é referida em Fulano (1900) <b>OU</b> escolha do pesquisador, ")
    }

    # validate(
    #   need((p2 > input$p2_TH_prop2/100 & alternative_TH2_prop2() != 'less') |
    #          (p2 < input$p2_TH_prop2/100 & alternative_TH2_prop2() != 'greater'),
    #        "Não é possível calcular um tamanho de amostra para a diferença esperada especificada e para o tipo de hipótese alternativa escolhido.")
    # )


    ratio_controle_caso <- input$k_TH_prop2
    if(ratio_controle_caso >= 1){
      p1a <- input$p2_TH_prop2/100
      p2a <- p2
    } else{
      p2a <- input$p2_TH_prop2/100
      p1a <- p2
    }

    code <- paste0(
      "EnvStats::propTestN(p.or.p1     = ", p2a, ", ",
      "p0.or.p2    = ", p1a, ", ",
      "alpha       = ", input$alpha_TH_prop2/100,  ", ",
      "power       = ", input$beta_TH_prop2/100, ", ",
      "sample.type = 'two.sample', ",
      "alternative = '", alternative_TH2_prop2(), "', ",
      "ratio       = ", ifelse(ratio_controle_caso >= 1, ratio_controle_caso, 1/ratio_controle_caso), ", ",
      "correct     = ", input$prop_correction, ", ",
      "warn = FALSE)"
      )

    n <- try_n(code)
    eval(parse(text = validate_n("n")))

    if(ratio_controle_caso > 1){
      n1 <- n$n1
      n2 <- n$n2
    } else if(ratio_controle_caso == 1){
      n1 <- n
      n2 <- n
    } else{
      n1 <- n$n2
      n2 <- n$n1
    }



    n <- n1 + n2
    nperdas1 <- n_perdas(n1, input$TH_prop_perdas_recusa)
    nperdas2 <- n_perdas(n2, input$TH_prop_perdas_recusa)
    eval(parse(text = validate_n_inf("n")))


    # IC aproximado
    testep <- prop.test(x = c(n1*p2, n2*input$p2_TH_prop2/100), n = c(n1, n2))
    ic_aprox <- round(testep$conf.int[1:2]*100, 1)
    txt_ic_p <- paste0("</b></br></br></br></br><i>Obs.:</i> Com esse tamanho de amostra o intervalo de confiança da diferença entre as ",
                       "proporções será aproximadamente <b>[", ic_aprox[1], "% ; ", ic_aprox[2], "%]</b>."
    )




    if(alternative_TH2_prop2() == "two.sided"){
      texto_comparacao <- paste0("Foi calculado o tamanho de amostra para detectar diferenças entre as proporções de <b>",
                                 prop2_nome_desfecho(), "</b> entre os ", th2prop_grupoTratamento(), " e ", th2prop_grupoControle(), ",")
    } else if(alternative_TH2_prop2() == "less"){
      texto_comparacao <- paste0("Foi calculado o tamanho de amostra para detectar que a proporções de <b>",
                                 prop2_nome_desfecho(), "</b> no ", th2prop_grupoTratamento(), " é menor do que no ", th2prop_grupoControle(), ",")
    } else{
      texto_comparacao <- paste0("Foi calculado o tamanho de amostra para detectar que a proporções de <b>",
                                 prop2_nome_desfecho(), "</b> no ", th2prop_grupoTratamento(), " é maior do que no ", th2prop_grupoControle(), ",")
    }


    cabecalho <- paste0("<b><font size = '5'>Tamanho amostral calculado: ", n, " (<i>", n1, " no ", th2prop_grupoTratamento(), " e ", n2, " no grupo ", th2prop_grupoControle(), "</i>)",
                        "</font></b></br></br><i>Sugestão de texto:</i></br></br>")


    if(input$k_TH_prop2 == 1){

      texto_grupos <- paste0(" por meio da ", .txt_citacao_tap, ". ",
                             "Considerando poder de <b>", input$beta_TH_prop2, "%</b>, nível de significância de <b>", input$alpha_TH_prop2, "% </b>",
                             text_just,
                             "chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n1, "</b> em cada grupo.",
                             if(testar_valor_perdas_valido(input$TH_prop_perdas_recusa)){
                               paste0(" Acrescentando <b>", input$TH_prop_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
                                      "deverá ser <b>", nperdas1 + nperdas2, "</b>.")
                             })
    } else{

      texto_grupos <- paste0(" por meio da ", .txt_citacao_tap, ". ",
                             "Considerando poder de <b>", input$beta_TH_prop2, "%</b>, nível de significância de <b>", input$alpha_TH_prop2, "% </b>",
                             text_just,
                             "chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n1, "</b> no ", th2prop_grupoTratamento(), " e <b>", n2, "</b> no ", th2prop_grupoControle(), ". ",
                             if(testar_valor_perdas_valido(input$TH_prop_perdas_recusa)){
                               paste0("Acrescentando <b>", input$TH_prop_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
                                      "deverá ser <b>", nperdas1 + nperdas2, "</b> (", nperdas1, " no ", th2prop_grupoTratamento(), " e ", nperdas2, " no ", th2prop_grupoControle(), ").")
                             })
    }

    paste0(cabecalho,
           texto_comparacao,
           texto_grupos,
           txt_ic_p,
           .txt_referencia_tap, print_r_code(code))

})





  ## Cenarios ----

  output$cenarios_duas_prop_thUi <- renderUI({


    razao_usada <- ifelse(input$prop2_estatistica_B == 'ratio', input$p2_TH_ratio, input$p2_TH_odds)

    if(razao_usada > 1){
      ratio_start <- razao_usada
      ratio_end  <- razao_usada + 1
      ratio_by   <- 0.1
    } else{
      ratio_start <- max(0, razao_usada - 0.3)
      ratio_end  <- razao_usada
      ratio_by   <- 0.05
    }




    fluidPage(fluidRow(
      br(),
      HTML('<hr style="color: black;">'),
      br(),br(),
      titlePanel("Construção de cenários"),
      br(),


      ###
      conditionalPanel(condition = "input.prop2_estatistica_B == 'percent'",

                       wellPanel(paste0(
                         "Utilize os argumentos abaixo para construir diferentes cenários. ",
                         "Você pode definir um intervalo de % para o grupo ", th2prop_grupoTratamento(),
                         "e especificar valores do poder (%). ",
                         "Demais informações serão recuperadas do painel lateral."
                       )),
                       HTML(paste0("<b>Defina a sequência de valores (%) para o grupo ", th2prop_grupoTratamento(), ":</b>")),
                       br(),
                       div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
                           numericInput("p2_TH_from", "Mínimo", value = 5, step = 1, min = 0, max = 99)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 80px;",
                           numericInput("p2_TH_to", "Máximo", value = 95, step = 1, min = 1, max = 100)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 80px;",
                           numericInput("p2_TH_by", "Intervalo", value = 5, min = 0, step = 1, max = 99) %>%
                             .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                                         title = "Sequência")
                       )
      ),


      # ratio
      conditionalPanel(condition = "input.prop2_estatistica_B == 'ratio'",


                       wellPanel(paste0(
                       "Utilize os argumentos abaixo para construir diferentes cenários. ",
                       "Você pode definir um intervalo para o Risco relativo (", th2prop_grupoTratamento(), "/", th2prop_grupoControle(),") e especificar valores do poder (%). ",
                       "Demais informações serão recuperadas do painel lateral."
                       )),
                       HTML("<b>Defina a sequência do risco relativo:</b>"),
                       br(),
                       div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
                           numericInput("rr_p2_TH_from", "Mínimo", value = ratio_start, step = .1, min = 0, max = Inf)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 80px;",
                           numericInput("rr_p2_TH_to", "Máximo", value = ratio_end, step = .1, min = 0, max = Inf)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 80px;",
                           numericInput("rr_p2_TH_by", "Intervalo", value = ratio_by, min = 0, step = .1) %>%
                             .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                                         title = "Sequência")
                       )

      ),

      # odds
      conditionalPanel(condition = "input.prop2_estatistica_B == 'odds'",


                       wellPanel(paste0(
                         "Utilize os argumentos abaixo para construir diferentes cenários. ",
                         "Você pode definir um intervalo para a Razão de chances (", th2prop_grupoTratamento(), "/", th2prop_grupoControle(),") e especificar valores do poder (%). ",
                         "Demais informações serão recuperadas do painel lateral."
                       )),
                       HTML("<b>Defina a sequência da razão de chance:</b>"),
                       br(),
                       div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
                           numericInput("rc_p2_TH_from", "Mínimo", value = ratio_start, step = .1, min = 0, max = Inf)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 80px;",
                           numericInput("rc_p2_TH_to", "Máximo", value = ratio_end, step = .1, min = 0, max = Inf)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 80px;",
                           numericInput("rc_p2_TH_by", "Intervalo", value = ratio_by, min = 0, step = .1) %>%
                             .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                                         title = "Sequência")
                       )

      ),

      fluidRow(
        column(6,
               textInput(inputId = "power_p2_th_plot",
                         label   = "Digite valores de poder (%) para fazer o gráfico:",
                         value   = "80, 90, 95",
                         width   = "400px") %>%
                 .help_buttom(body = "Defina os valores de poder (%).
                                                      Esses valores serão utilizados para criar diferentes linhas no gráfico.
                                                      Separe os valores por vírgula ',' e utilize ponto '.' como decimal.")
        )
      ),




      shinycssloaders::withSpinner(plotly::plotlyOutput("p2_TH_plot", width = "80%"), type = 5),
      conditionalPanel(condition = "input.prop2_estatistica_B == 'percent'",
                       p("Obs.: a linha tracejada representa a % no grupo controle definida no painel lateral.")
      ),
      conditionalPanel(condition = "input.prop2_estatistica_B == 'ratio'",
                       p("Obs.: a linha tracejada representa o risco relativo definido no painel lateral.")
      ),
      conditionalPanel(condition = "input.prop2_estatistica_B == 'odds'",
                       p("Obs.: a linha tracejada representa a razão de chances definida no painel lateral.")
      ),

      br(), br(),
      downloadButton("download_p2_TH_tab","Download tabela"),
      shinycssloaders::withSpinner(DT::dataTableOutput("p2_TH_tab", width = "100%"), type = 5)

    ))

  })



  eval(parse(text = check_text_input_to_vector("power_p2_th_plot")))

  tab_p2_TH_cenarios <- reactive({

    poder <- text_input_to_vector(input$power_p2_th_plot)
    req(length(poder) > 0)


    if(input$prop2_estatistica_B == "percent"){
      rrrr <- NA
      odssss <- NA
      prop_tratamento <- seq(from = input$p2_TH_from, to = input$p2_TH_to, by = input$p2_TH_by)/100

    } else if (input$prop2_estatistica_B == "ratio") {

      rrrr <- seq(from = input$rr_p2_TH_from, to = input$rr_p2_TH_to, by = input$rr_p2_TH_by)
      odssss <- NA
      prop_tratamento <- (input$p2_TH_prop2/100)*rrrr

    } else {

      rrrr <- NA
      odssss <- seq(from = input$rc_p2_TH_from, to = input$rc_p2_TH_to, by = input$rc_p2_TH_by)
      prob_control <- input$p2_TH_prop2/100
      prop_tratamento <- (odssss*prob_control)/ (1 + odssss*prob_control - prob_control)
    }


    df_inputs_prop <- tibble::tibble(rrrr, odssss, prop_tratamento)


    simul_n <- expand.grid(prop_controle   = input$p2_TH_prop2/100,
                           prop_tratamento = prop_tratamento,
                           significancia   = input$alpha_TH_prop2/100,
                           poder           = poder/100,
                           alternative     = alternative_TH2_prop2(),
                           ratio_controle_caso = input$k_TH_prop2,
                           correct        = input$prop_correction,
                           type_info1     = "n1",
                           type_info2     = "n2",
                           stringsAsFactors = FALSE) %>%
      dplyr::filter(prop_tratamento != prop_controle) %>%
      mutate(`n Tratamento` = mapply(n_th2_prop, prop_controle, prop_tratamento, significancia, poder,
                                     alternative, ratio_controle_caso, correct, type_info1),
             `n Controle` = mapply(n_th2_prop, prop_controle, prop_tratamento, significancia, poder,
                                   alternative, ratio_controle_caso, correct, type_info2)) %>%
      dplyr::filter(!is.na(`n Tratamento`) & !is.na(`n Controle`)) %>%
      mutate(`n total` = `n Tratamento` + `n Controle`,
             `Trat + perdas/ recusas` = n_perdas(`n Tratamento`, input$TH_prop_perdas_recusa),
             `Cont + perdas/ recusas` = n_perdas(`n Controle`,   input$TH_prop_perdas_recusa)) %>%
      mutate(`n total + perdas/ recusas`  = `Trat + perdas/ recusas` + `Cont + perdas/ recusas`,
             `Nível de significância (%)` = input$alpha_TH_prop2,
             `% de perdas/ recusas`   = input$TH_prop_perdas_recusa,
             `% no Tratamento` = prop_tratamento*100,
             `Poder (%)` = poder*100,
             `Poder (%) ` = factor(poder*100),
             `% Controle` = prop_controle*100,
             `% Tratamento` = prop_tratamento*100,
             `Hipótese alternativa` = alternative,
             `Balanço da amostra (Controle/ Tratamento)` = ratio_controle_caso,
             `Correção de continuidade` = correct
      )

    simul_n %>%
      left_join(df_inputs_prop, by = "prop_tratamento") %>%
      rename(`Risco relativo` = rrrr,
             `Razão de chance` = odssss)


  })


  output$p2_TH_plot <- plotly::renderPlotly({

    req(!(is.null(input$p2_TH_prop2) | is.null(input$p2_TH_from) | is.null(input$power_p2_th_plot)))
    req(!(is.na(input$p2_TH_prop2) | is.na(input$p2_TH_from) | is.na(input$power_p2_th_plot)))

    if(input$prop2_estatistica_B == 'percent'){

      g1 <- ggplot(tab_p2_TH_cenarios(),
                   aes(x = `% no Tratamento`,
                       y = `n total`,
                       color = `Poder (%) `,
                       Tratamento = `n Tratamento`,
                       Controle   = `n Controle`))+
        geom_line() +
        geom_point() +
        geom_vline(xintercept=input$p2_TH_prop2, linetype="dashed", color = "red") +
        scale_x_continuous(breaks = seq(from = input$p2_TH_from, to = input$p2_TH_to, by = input$p2_TH_by)) +
        xlab(paste0("% no ", th2prop_grupoTratamento())) +
        ylab("Tamanho da amostra*") +
        theme_bw() +
        theme(axis.text = element_text(colour = "black")) +
        scale_color_brewer(palette = "Set1")

    } else if(input$prop2_estatistica_B == 'ratio'){

      g1 <- ggplot(tab_p2_TH_cenarios(),
                   aes(x = `Risco relativo`,
                       y = `n total`,
                       color = `Poder (%) `,
                       Tratamento = `n Tratamento`,
                       Controle   = `n Controle`))+
        geom_line() +
        geom_point() +
        geom_vline(xintercept=input$p2_TH_ratio, linetype="dashed", color = "red") +
        scale_x_continuous(breaks = seq(from = input$rr_p2_TH_from, to = input$rr_p2_TH_to, by = input$rr_p2_TH_by)) +
        xlab(paste0("Risco relativo/ razão de prevalências (", th2prop_grupoTratamento(), "/", th2prop_grupoControle(), ")")) +
        ylab("Tamanho da amostra*") +
        theme_bw() +
        theme(axis.text = element_text(colour = "black")) +
        scale_color_brewer(palette = "Set1")
    } else{

      g1 <- ggplot(tab_p2_TH_cenarios(),
                   aes(x = `Razão de chance`,
                       y = `n total`,
                       color = `Poder (%) `,
                       Tratamento = `n Tratamento`,
                       Controle   = `n Controle`))+
        geom_line() +
        geom_point() +
        geom_vline(xintercept=input$p2_TH_odds, linetype="dashed", color = "red") +
        scale_x_continuous(breaks = seq(from = input$rc_p2_TH_from, to = input$rc_p2_TH_to, by = input$rc_p2_TH_by)) +
        xlab(paste0("Razão de chance (", th2prop_grupoTratamento(), "/", th2prop_grupoControle(), ")")) +
        ylab("Tamanho da amostra*") +
        theme_bw() +
        theme(axis.text = element_text(colour = "black")) +
        scale_color_brewer(palette = "Set1")
    }



    plotly::ggplotly(g1,
                     tooltip = c("x", "colour", "y", "Tratamento", "Controle")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                showarrow = F, xref='paper', yref='paper',
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=10)))
  })


  return_table_p2_TH_tab <- reactive({
    if(input$prop2_estatistica_B == 'percent'){

     df_ <- tab_p2_TH_cenarios() %>%
        dplyr::select(`% Tratamento`,
                      `% Controle`,
                      `n total`,
                      `n Tratamento`,
                      `n Controle`,
                      `Nível de significância (%)`,
                      `Poder (%)`,
                      `Hipótese alternativa`,
                      `Balanço da amostra (Controle/ Tratamento)`,
                      `Correção de continuidade`)

     colnames(df_) <- c(paste0("% ", th2prop_grupoTratamento()),
                        paste0("% ", th2prop_grupoControle()),
                        "n total",
                        paste0("n ", th2prop_grupoTratamento()),
                        paste0("n ", th2prop_grupoControle()),
                        "Nível de significância (%)",
                        "Poder (%)",
                        "Hipótese alternativa",
                        paste0("Balanço da amostra (", th2prop_grupoControle(), "/",  th2prop_grupoTratamento(), ")"),
                        "Correção de continuidade")

    } else if(input$prop2_estatistica_B == 'ratio'){

      df_ <- tab_p2_TH_cenarios() %>%
        dplyr::select(`% Controle`,
               `Risco relativo`,
               `n total`,
               `n Tratamento`,
               `n Controle`,
               `Nível de significância (%)`,
               `Poder (%)`,
               `Hipótese alternativa`,
               `Balanço da amostra (Controle/ Tratamento)`,
               `Correção de continuidade`)

      colnames(df_) <- c(paste0("% ", th2prop_grupoControle()),
                         paste0("Risco relativo/ razão de prevalências (", th2prop_grupoTratamento(), "/", th2prop_grupoControle(), ")"),
                         "n total",
                         paste0("n ", th2prop_grupoTratamento()),
                         paste0("n ", th2prop_grupoControle()),
                         "Nível de significância (%)",
                         "Poder (%)",
                         "Hipótese alternativa",
                         paste0("Balanço da amostra (", th2prop_grupoControle(), "/",  th2prop_grupoTratamento(), ")"),
                         "Correção de continuidade")
    } else{

      df_ <- tab_p2_TH_cenarios() %>%
        dplyr::select(`% Controle`,
             `Razão de chance`,
             `n total`,
             `n Tratamento`,
             `n Controle`,
             `Nível de significância (%)`,
             `Poder (%)`,
             `Hipótese alternativa`,
             `Balanço da amostra (Controle/ Tratamento)`,
             `Correção de continuidade`)

      colnames(df_) <- c(paste0("% ", th2prop_grupoControle()),
                         paste0("Razão de chance (", th2prop_grupoTratamento(), "/", th2prop_grupoControle(), ")"),
                         "n total",
                         paste0("n ", th2prop_grupoTratamento()),
                         paste0("n ", th2prop_grupoControle()),
                         "Nível de significância (%)",
                         "Poder (%)",
                         "Hipótese alternativa",
                         paste0("Balanço da amostra (", th2prop_grupoControle(), "/",  th2prop_grupoTratamento(), ")"),
                         "Correção de continuidade")
    }

    df_
  })



  output$p2_TH_tab <- DT::renderDataTable({

    req(!(is.null(input$p2_TH_prop2) | is.null(input$p2_TH_from) | is.null(input$power_p2_th_plot)))
    req(!(is.na(input$p2_TH_prop2) | is.na(input$p2_TH_from) | is.na(input$power_p2_th_plot)))


    return_table_p2_TH_tab() %>%
      DT::datatable(extensions = c('FixedColumns'),
                rownames   = FALSE,
                filter     = "none",
                # #callback   = DT::JS("$('div.dwnld').append($('#download_p2_TH_tab'));"),
                options    = list(pageLength = 10,
                                  scrollX = TRUE,
                                  scrollY = TRUE,
                                  searching = FALSE,
                                  fixedColumns = list(leftColumns = 1),
                                  dom = 'B<"dwnld">frtip'
                                  # buttons = list(list(extend = 'none'))
                )
      )
  })


  output$download_p2_TH_tab <- downloadHandler(
    filename = function() { "Cenarios_tamanho_amostra_duas_prop_testar.xlsx"},
    content = function(file) {writexl::write_xlsx(return_table_p2_TH_tab(),
                                                  path = file)}
  )







  #-------------------.
  #  Poder        ----
  #-------------------.


  eval(parse(text = warning_numero_positivo("prop2a_th_n")))
  eval(parse(text = warning_numero_positivo("prop2b_th_n")))

  eval(parse(text = warning_prop("prop2a_th_power")))
  eval(parse(text = warning_prop("prop2b_th_power")))
  eval(parse(text = warning_prop("prop2_th_power_sig")))


  output$THprop2_power <- renderText({


    code <- paste0(
      "EnvStats::propTestPower(",
      "n.or.n1     = ", input$prop2a_th_n, ", ",
      "p.or.p1 = ", input$prop2a_th_power, "/100, ",
      "n2 = ", input$prop2b_th_n, ", ",
      "p0.or.p2	= ", input$prop2b_th_power, "/100, ",
      "alpha       = ", input$prop2_th_power_sig, "/100, ",
      "sample.type = 'two.sample', ",
      "alternative = 'two.sided', ",
      "correct     = ", input$prop2_th_power_correction, ")"
    )

    codeR <- str_replace(code, fixed(")"), ", warn = FALSE)")

    # code <- paste0(
    #   "pwr::pwr.2p2n.test(",
    #   "h = pwr::ES.h(", input$prop2a_th_power, "/100, ",  input$prop2b_th_power, "/100), ",
    #   "n1 = 33000,
    #                 n2 = 28700,
    #                 sig.level = 0.05,
    #                 power = NULL,
    #                 alternative = c("two.sided", "less","greater"))
    #
    # )


    poder <- eval(parse(text = codeR))

    paste0("<b><font size = '5'>Poder calculado: ", round(poder*100, digits = 1),
           "%</font></b></br></br><i>Sugestão de texto:</i></br></br>",


           "Foi calculado o poder para testar duas proporções independentes utilizando a ", .txt_citacao_tap, ". ",

           "Considerando um nível de significância de <b>", input$prop2_th_power_sig, "%</b>, ",
           "proporções do desfecho de <b>", input$prop2a_th_power, "%</b> e <b>", input$prop2b_th_power, "%</b>, ",
           " tamanho amostral de <b>", input$prop2a_th_n, "</b> e <b>", input$prop2b_th_n, "</b> ",
           "nos grupos A e B, respectivamente, ",

           if(input$prop2_th_power_correction){
             "e aplicando correção de continuidade, "
           },

           "chegou-se à um poder de de <b>", round(100*poder, digits = 1), "%</b>.",

           .txt_referencia_tap, print_r_code(code))


  })

  # output$THprop22 <- renderText({
  #
  #   n <- TrialSize::TwoSampleProportion.Equality(
  #     alpha = input$alpha_TH_prop22,
  #     beta = 1 - input$beta_TH_prop22,
  #     p1 = input$p1_TH_prop22,
  #     p2 = input$p2_TH_prop22,
  #     k = input$k_TH_prop22,
  #     delta = input$margin_TH_prop22
  #   )
  #
  #   paste0("Considerando o Nível de significância igual a ", input$alpha_TH_prop22,
  #          ", o poder igual a ", input$beta_TH_prop22,
  #          ", a proporção para a amostra 1 igual a ", input$p1_TH_prop22,
  #          ", a proporção para a amostra 2 igual a ", input$p2_TH_prop22,
  #          ", a razão entre os tamanhos de amostra 1 e 2 igual a ", input$k_TH_prop22,
  #          " e a diferença mínima a ser detectada igual a ", input$margin_TH_prop22,
  #          ", o tamanho de amostra calculado é igual a ",round(n))
  #
  # })












  # Equivalencia ----


  observeEvent(input$margin_2_ind_prop, {
    if(is.na(input$margin_2_ind_prop)){
      shinyFeedback::showFeedbackWarning(
        inputId = "margin_2_ind_prop",
        text = "Deve ser fornecido um valor do tamanho de efeito.",
        color = "red"
      )
    } else if (input$prop_test_inf_eq_sup == "Não inferioridade" & input$margin_2_ind_prop > 0) {
      shinyFeedback::showFeedbackWarning(
        inputId = "margin_2_ind_prop",
        text = "Para um estudo de não inferioridade a margem deve ser negativa.",
        color = "red"
      )
    }  else if (input$prop_test_inf_eq_sup == "Superioridade" & input$margin_2_ind_prop < 0) {
      shinyFeedback::showFeedbackWarning(
        inputId = "margin_2_ind_prop",
        text = "Para um estudo de superioridade a margem deve ser positiva.",
        color = "red"
      )
    } else {
      shinyFeedback::hideFeedback("margin_2_ind_prop")
    }
  })




  output$inf_sup_nomesUi_prop <- renderUI({
    label_ <- case_when(input$prop_test_inf_eq_sup == "Não inferioridade" ~ "é não inferior ao grupo:",
                        input$prop_test_inf_eq_sup == "Superioridade"     ~ "é superior ao grupo:",
                        TRUE ~ "é equivalente ao grupo:")

    fluidPage(
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
          textInput(inputId = "inf_sup_groupB_prop",
                    label   = "Comparar se o grupo:",
                    value   = "Experimental")),

      div(style="display: inline-block;vertical-align:top; width: 49%;",
          textInput(inputId = "inf_sup_groupA_prop",
                    label   = label_,
                    value   = "Convencional"))
    )
  })




  output$prop2eq_estatistica_BUi <- renderUI({

    nomes_opcoes_medida <- c(
      paste0("Razão de chance (", input$inf_sup_groupA_prop, "/", input$inf_sup_groupB_prop, ")"),
      paste0("Risco relativo (", input$inf_sup_groupA_prop, "/", input$inf_sup_groupB_prop, ")"),
      paste0("% esperado no grupo ", input$inf_sup_groupA_prop)
    )
    opcoes_medida <- c('odds', 'ratio', "percent")
    names(opcoes_medida) <- nomes_opcoes_medida


    selectInput('prop2_estatistica_B_est_non_inf',
                paste0('Medida do grupo ', input$inf_sup_groupA_prop),
                choices = opcoes_medida,
                selected = 'percent'
    )
  })


  output$side_bar_prop_inf <- renderUI({
    fluidPage(fluidRow(
      numericInput( "margin_2_ind_prop",
                    paste0("Margem de ", input$prop_test_inf_eq_sup, " (%)"),
                    value = case_when(input$prop_test_inf_eq_sup == "Não inferioridade" ~ -10,
                                      input$prop_test_inf_eq_sup == "Superioridade"     ~ 10,
                                      TRUE ~ 15),
                    min   = -100,
                    max   = 100,
                    step  = 1
      ) %>% .help_buttom(body = case_when(input$prop_test_inf_eq_sup == "Não inferioridade"  ~ .txt_margem_nao_inferior,
                                          input$prop_test_inf_eq_sup == "Superioridade"      ~ .txt_margem_superior,
                                          TRUE ~ .txt_margem_equivalencia),
                         title = paste0("Margem de ", input$prop_test_inf_eq_sup, " (%)")),

      numericInput( "p2_2_ind_prop",
                    paste0("% esperado no grupo ", input$inf_sup_groupB_prop),
                    value = 50,
                    min = 0,
                    max = 100,
                    step = 5
      ),

      uiOutput("prop2eq_estatistica_BUi"),


      conditionalPanel(condition = 'input.prop2_estatistica_B_est_non_inf == "percent"',
                       numericInput( "p1_2_ind_prop_kkk",
                                     paste0("% esperado no grupo ", input$inf_sup_groupA_prop),
                                     value = case_when(input$prop_test_inf_eq_sup == "Não inferioridade" ~ 40,
                                                       input$prop_test_inf_eq_sup == "Superioridade"     ~ 70,
                                                       TRUE ~ 45),
                                     min = 0,
                                     max = 100,
                                     step = 2
                       ) %>% .help_buttom(body = "O percentual esperado de eventos no grupo Tratamento, geralmente é utilizado algum valor com base na literatura.")
      ),
      conditionalPanel(condition = 'input.prop2_estatistica_B_est_non_inf == "ratio"',
                       numericInput( "p2_TH_ratio_est_kkk",
                                     "Risco relativo",
                                     value = case_when(input$prop_test_inf_eq_sup == "Não inferioridade" ~ 0.8,
                                                       input$prop_test_inf_eq_sup == "Superioridade"     ~ 1.5,
                                                       TRUE ~ 1.2),
                                     min = 0,
                                     max = Inf,
                                     step = 0.1
                       ) %>% .help_buttom(body = .txt_risco_relativo, title = "Risco relativo/ Razão de prevalências")
      ),
      conditionalPanel(condition = 'input.prop2_estatistica_B_est_non_inf == "odds"',
                       numericInput( "p2_TH_odds_est_kkk",
                                     "Odds ratio",
                                     value = case_when(input$prop_test_inf_eq_sup == "Não inferioridade" ~ 0.8,
                                                       input$prop_test_inf_eq_sup == "Superioridade"     ~ 1.5,
                                                       TRUE ~ 1.2),
                                     min = 0,
                                     max = Inf,
                                     step = 0.1
                       ) %>% .help_buttom(body = .txt_razao_chance, title = "Razão de chance")
      ),

      numericInput( "k_2_ind_prop",
                    paste0("Balanceamento ", input$inf_sup_groupA_prop, ":", input$inf_sup_groupB_prop),
                    value = 1,
                    min = 0,
                    max = Inf,
                    step = 0.2
      )%>% .help_buttom(body = .txt_balanceamento, title = "Balanceamento"),
      numericInput( "beta_2_ind_prop",
                    "Poder (%)",
                    value = 80,
                    min = 0,
                    max = 100,
                    step = 1
      ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
      numericInput( "alpha_2_ind_prop",
                    "Nível de significância (%)",
                    value = 5,
                    min = 0,
                    max = 100,
                    step = 1
      ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
      numericInput( "eq_prop_perdas_recusa",
                    "Perdas/ Recusa (%)",
                    value = 10,
                    min = 0,
                    max = 100,
                    step = 1
      ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")

    ))
  })






  output$prop_equivalence_2_ind <- renderText({

    req(!is.null(input$margin_2_ind_prop))
    req(!is.null(input$prop2_estatistica_B_est_non_inf))
    req(!is.null(input$k_2_ind_prop))

    if(input$prop_test_inf_eq_sup == "Não inferioridade") req(input$margin_2_ind_prop <= 0)
    if(input$prop_test_inf_eq_sup == "Superioridade") req(input$margin_2_ind_prop >= 0)


    prob_control <- input$p2_2_ind_prop/100

    if(input$prop2_estatistica_B_est_non_inf == "percent"){
      p1 <- input$p1_2_ind_prop_kkk/100

    } else if (input$prop2_estatistica_B_est_non_inf == "ratio") {
      p1 <- prob_control*input$p2_TH_ratio_est_kkk

    } else {
      p1 <- (input$p2_TH_odds_est_kkk*prob_control)/ (1 + input$p2_TH_odds_est_kkk*prob_control - prob_control)
    }



    if(input$prop_test_inf_eq_sup == "Equivalência"){

      code <- paste0("epiR::epi.ssequb(",
                     "alpha = ", input$alpha_2_ind_prop, "/100, ",
                     "power = ", input$beta_2_ind_prop, "/100, ",
                     "treat = ", p1, ", ",
                     "control = ", prob_control, ", ",
                     "r = ", input$k_2_ind_prop, ", ",
                     "delta = ", input$margin_2_ind_prop, "/100, n = NA)")

    } else if(input$prop_test_inf_eq_sup == "Não inferioridade"){

      code <- paste0("epiR::epi.ssninfb(",
                     "alpha = ", input$alpha_2_ind_prop, "/100, ",
                     "power = ", input$beta_2_ind_prop, "/100, ",
                     "control = ", p1, ", ",
                     "treat = ", prob_control, ", ",
                     "r = ", input$k_2_ind_prop, ", ",
                     "delta = ", input$margin_2_ind_prop, "/100, n = NA)")

    } else{

      code <- paste0("epiR::epi.sssupb(",
                     "alpha = ", input$alpha_2_ind_prop, "/100, ",
                     "power = ", input$beta_2_ind_prop, "/100, ",
                     "treat = ", p1, ", ",
                     "control = ", prob_control, ", ",
                     "r = ", input$k_2_ind_prop, ", ",
                     "delta = ", input$margin_2_ind_prop, "/100, n = NA)")
    }

    n <- eval(parse(text = code))

    n1 <- n$n.treat
    n2 <- n$n.control
    n <- n1 + n2
    eval(parse(text = validate_n("n1")))
    eval(parse(text = validate_n_inf("n1")))


    nperdas1 <- n_perdas(n1, input$eq_prop_perdas_recusa)
    nperdas2 <- n_perdas(n2, input$eq_prop_perdas_recusa)

    cabecalho <- paste0("<b><font size = '5'>Tamanho amostral calculado: ", n, " (<i>", n1, " ", input$inf_sup_groupA_prop, " e ", n2, " ", input$inf_sup_groupB_prop, "</i>)",
                        "</font></b></br></br><i>Sugestão de texto:</i></br></br>")


    #---- Texto do teste
    if(input$prop_test_inf_eq_sup == "Equivalência"){
      texto_comparacao <- paste0("Foi calculado o tamanho de amostra para testar a <b>equivalência</b>, em termos de proporção, entre os grupos ", input$inf_sup_groupA_prop, " e ", input$inf_sup_groupB_prop, ", ",
                                 "por meio da ", .txt_citacao_tap, ". ",
                                 "Considerando uma margem de equivalência de <b>", input$margin_2_ind_prop, "%</b>"
      )
    } else if(input$prop_test_inf_eq_sup == "Não inferioridade"){
      texto_comparacao <- paste0("Foi calculado o tamanho de amostra para testar a <b>não inferioridade</b>, em termos de proporção, do grupo ", input$inf_sup_groupA_prop, " em relação ao grupo ", input$inf_sup_groupB_prop, ", ",
                                 "por meio da ", .txt_citacao_tap, ". ",
                                 "Considerando uma margem de não inferioridade de <b>", input$margin_2_ind_prop, "%</b>"
      )
    } else{
      texto_comparacao <- paste0("Foi calculado o tamanho de amostra para testar a <b>superioridade</b>, em termos de proporção, do grupo ", input$inf_sup_groupA_prop, " em relação ao grupo ", input$inf_sup_groupB_prop, ", ",
                                 "por meio da ", .txt_citacao_tap, ". ",
                                 "Considerando uma margem de não inferioridade de <b>", input$margin_2_ind_prop, "%</b>"
      )
    }
    #----------------.


    texto_final <- paste0(", poder de <b>", input$beta_2_ind_prop, "%</b>, ",
                          "nível de significância de <b>", input$alpha_2_ind_prop, "%</b>, ",
                          "proporções esperadas de <b>", round(p1*100, 1), "%</b> no grupo ", input$inf_sup_groupA_prop, " ",
                          "e <b>", input$p2_2_ind_prop, "%</b> no grupo ", input$inf_sup_groupB_prop, " (dados de Fulano (1900)), ",
                          "chegou-se ao tamanho total da amostra de <b>", n, "</b> sujeitos, ",

                          if(input$k_2_ind_prop == 1){
                            paste0(
                              "sendo <b>", n1, "</b> sujeitos em cada grupo. ",
                              "Acrescentando <b>", input$eq_prop_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", nperdas1 + nperdas2, "</b>."
                            )
                          } else{
                            paste0(
                              "sendo <b>", n1, "</b> sujeitos no grupo ", input$inf_sup_groupA_prop, " e <b>", n2, "</b> sujeitos no grupo ", input$inf_sup_groupB_prop, ". ",
                              "Acrescentando <b>", input$eq_prop_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", nperdas1 + nperdas2, "</b>",
                              " (<b>", nperdas1, "</b> no grupo ", input$inf_sup_groupA_prop, " e <b>", nperdas2, "</b> no grupo ", input$inf_sup_groupB_prop, ")."
                            )
                          }
 )


    paste0(cabecalho,
           texto_comparacao,
           texto_final,
           .txt_referencia_tap, print_r_code(code))

  })





  # 2 Dependentes ----



  observeEvent(input$show_prop2n_dep, {
    showModal(
      modalDialog(
        title = "Ajustes",
        fluidPage(

          HTML("<b>Preencha os campos abaixo de acordo com seu estudo para que sirvam de guia no preenchimento dos demais campos</b>."),
          br(), br(),
          textInput(inputId = "prop2n_dep_nome_desfecho",
                    label   = "Descreva o nome do desfecho",
                    value   = ifelse(input$show_prop2n_dep == 0, "Desfecho de interesse", prop2n_dep_nome_desfecho())),
          HTML(paste0("<i>", str_remove_all(.txt_desfecho, "<br><br>"), "</i>")),
          br(), br(),
          textInput(inputId = "prop2n_dep_cat1",
                    label   = "Descreva o nome da categoria 1",
                    value   = ifelse(input$show_prop2n_dep == 0, "A", prop2n_dep_cat1())),


          textInput(inputId = "prop2n_dep_cat2",
                    label   = "Descreva o nome da categoria 1",
                    value   = ifelse(input$show_prop2n_dep == 0, "B", prop2n_dep_cat2())),
        ),
        easyClose = TRUE,
        footer    = NULL
      )
    )
  })





  prop2n_dep_nome_desfecho <- reactive({
    ifelse(is.null(input$prop2n_dep_nome_desfecho), "Desfecho de interesse", input$prop2n_dep_nome_desfecho)
  })

  prop2n_dep_cat1 <- reactive({
    ifelse(is.null(input$prop2n_dep_cat1), "A", input$prop2n_dep_cat1)
  })

  prop2n_dep_cat2 <- reactive({
    ifelse(is.null(input$prop2n_dep_cat2), "B", input$prop2n_dep_cat2)
  })


  output$prop2n_dep_sideUi <- renderUI({
    req(!is.null(prop2n_dep_cat1()))
    req(!is.null(prop2n_dep_cat2()))

    fluidPage(fluidRow(
      HTML(paste0("<b><font size = '2.99'>% de discordância na categoria</font></b><br>")),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
          numericInput( "prop2n_dep_discordante1",
                        paste0(prop2n_dep_cat1(), " (célula pb)"),
                        value = 50,
                        min = 0,
                        max = 100,
                        step = 5
          )
      ),
      div(style="display: inline-block;vertical-align:top; width: 49%;",
          numericInput( "prop2n_dep_discordante2",
                        paste0(prop2n_dep_cat2(), " (célula pc)"),
                        value = 30,
                        min = 0,
                        max = 100,
                        step = 5
          ) %>% .help_buttom(body = paste0(
            "Percentual do total de pares discordantes (aqueles que obtiveram respostas diferentes) em cada categoria - ver explicação no cabeçalho da aba.",
            .txt_definido_pesquisador_OU_literatura
          )
          )
      ),

      numericInput( "prop2n_dep_pwr",
                    "Poder (%)",
                    value = 80,
                    min = 0,
                    max = 100,
                    step = 1
      ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
      numericInput( "prop2n_dep_sig",
                    "Nível de significância (%)",
                    value = 5,
                    min = 0,
                    max = 100,
                    step = 1
      ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
      selectInput('prop2n_dep_alternativa',
                  'Tipo de teste de acordo com hipótese alternativa:',
                  choices = c("Bilateral" = "two.sided", "Unilateral" = "one.sided"),
                  selected = 'two.sided'
      ) %>% .help_buttom(body = .txt_h1),
      numericInput( "prop2n_dep_perdas_recusa",
                    "Perdas/ Recusa (%)",
                    value = 10,
                    min = 0,
                    max = 100,
                    step = 1
      ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")

    ))
  })



  eval(parse(text = warning_prop("prop2n_dep_discordante1")))
  eval(parse(text = warning_prop("prop2n_dep_discordante2")))
  eval(parse(text = warning_prop("prop2n_dep_sig")))
  eval(parse(text = warning_prop("prop2n_dep_pwr")))
  eval(parse(text = warning_perdas("prop2n_dep_perdas_recusa")))



  observeEvent(c(input$prop2n_dep_discordante1, input$prop2n_dep_discordante2), {
    req(!is.na(input$prop2n_dep_discordante1))
    req(!is.na(input$prop2n_dep_discordante2))

    if(input$prop2n_dep_discordante1 + input$prop2n_dep_discordante2 > 100){
      shinyFeedback::showFeedbackWarning(
        inputId = "prop2n_dep_discordante1",
        text = "A soma dos %deve ser menor do que 100%.",
        color = "red"
      )
    }  else {
      shinyFeedback::hideFeedback("prop2n_dep_discordante1")
    }
  })

  observeEvent(c(input$prop2n_dep_discordante1, input$prop2n_dep_discordante2), {
    req(!is.na(input$prop2n_dep_discordante1))
    req(!is.na(input$prop2n_dep_discordante2))

    if(input$prop2n_dep_discordante1 + input$prop2n_dep_discordante2 > 100){
      shinyFeedback::showFeedbackWarning(
        inputId = "prop2n_dep_discordante2",
        text = "",
        color = "red"
      )
    }  else {
      shinyFeedback::hideFeedback("prop2n_dep_discordante2")
    }
  })



  output$prop2n_dep_th <- renderUI({
    req(!is.null(input$prop2n_dep_discordante1))
    req(!is.null(input$prop2n_dep_discordante2))

    req(!is.na(input$prop2n_dep_discordante1))
    req(!is.na(input$prop2n_dep_discordante2))

    if(input$prop2n_dep_alternativa == "two.sided"){
      withMathJax("$$H_0: p_b = p_c\\text{  vs  } H_1: p_b \\neq p_c$$")
    } else if(input$prop2n_dep_discordante1 > input$prop2n_dep_discordante2){
      withMathJax("$$H_0: p_b \\leq p_c\\text{  vs  } H_1: p_b > p_c$$")
    } else {
      withMathJax("$$H_0: p_b \\geq p_c\\text{  vs  } H_1: p_b < p_c$$")
    }
  })



  output$prop2n_dep_resultadoUi <- renderText({
    req(!is.null(input$prop2n_dep_discordante1))
    req(!is.null(input$prop2n_dep_discordante2))

    req(!is.na(input$prop2n_dep_discordante1))
    req(!is.na(input$prop2n_dep_discordante2))

    numerador   <- max(input$prop2n_dep_discordante1, input$prop2n_dep_discordante2)
    denominador <- min(input$prop2n_dep_discordante1, input$prop2n_dep_discordante2)

    code <- paste0(
      "MESS::power_mcnemar_test(n = NULL, ",
      "psi = ", numerador, "/", denominador, ", ",
      "paid = ", denominador, "/100, ",
      "sig.level = ", input$prop2n_dep_sig, "/100, ",
      "power = ", input$prop2n_dep_pwr, "/100, ",
      'alternative = "', input$prop2n_dep_alternativa, '", ',
      'method = "normal")'
    )


    n <- try_n(code)
    eval(parse(text = validate_n("n")))
    n <- ceiling(n$n)
    eval(parse(text = validate_n_inf("n")))


    if(input$prop2n_dep_alternativa == "two.sided"){
      texto_comparacao <- paste0("Foi calculado o tamanho de amostra para detectar que as discordâncias de <b>", prop2n_dep_nome_desfecho(), "</b> na categoria <b><i>", prop2n_dep_cat1(), "</i></b> é diferente das discordâncias na categoria <b><i>", prop2n_dep_cat2(), "</i></b> após a <b><i>intervenção</i></b>, ")
    } else if(input$prop2n_dep_discordante1 > input$prop2n_dep_discordante2){
      texto_comparacao <- paste0("Foi calculado o tamanho de amostra para detectar que as discordâncias de <b>", prop2n_dep_nome_desfecho(), "</b> na categoria <b><i>", prop2n_dep_cat1(), "</i></b> é maior do que as discordâncias na categoria <b><i>", prop2n_dep_cat2(), "</i></b> após a <b><i>intervenção</i></b>, ")
    } else {
      texto_comparacao <- paste0("Foi calculado o tamanho de amostra para detectar que as discordâncias de <b>", prop2n_dep_nome_desfecho(), "</b> na categoria <b><i>", prop2n_dep_cat1(), "</i></b> é menor do que as discordâncias na categoria <b><i>", prop2n_dep_cat2(), "</i></b> após a <b><i>intervenção</i></b>, ")
    }


    cabecalho <- paste0("<b><font size = '5'>Tamanho amostral calculado: ", n, "<i> pares</i>",
                        "</font></b></br></br><i>Sugestão de texto:</i></br></br>")


    texto_desvio <- paste0(texto_comparacao,
                           "utilizando a ", .txt_citacao_tap, ". ",
                           "Considerando poder de <b>", input$prop2n_dep_pwr, "%</b>, ",
                           "nível de significância de <b>", input$prop2n_dep_sig, "%</b>, ",
                           "percentual de pares discordantes de <b>", input$prop2n_dep_discordante1, "%</b> e <b>", input$prop2n_dep_discordante2,
                           "%</b> nas categorias <i>", prop2n_dep_cat1(), "</i> e <i>", prop2n_dep_cat2(), "</i>, respectivamente (dados de Fulano (1900)), ",
                           "chegou-se ao tamanho total de <b>", n, "</b> pares. ",
                           "Acrescentando <b>", input$prop2n_dep_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>",
                           n_perdas(n, input$prop2n_dep_perdas_recusa), "</b> pares.")

    paste0(cabecalho,
           texto_desvio,
           .txt_referencia_tap, print_r_code(code))

    # code
  })




  ## Cenarios ----

  output$cenarios_duas_prop_dep_thUi <- renderUI({
    req(!is.null(input$prop2n_dep_discordante1))
    req(!is.null(input$prop2n_dep_discordante2))

    req(!is.na(input$prop2n_dep_discordante1))
    req(!is.na(input$prop2n_dep_discordante2))


    fluidPage(fluidRow(

      br(),
      HTML('<hr style="color: black;">'),
      br(),br(),

      titlePanel("Construção de cenários"),
      br(),

      wellPanel(
        paste0("Utilize os argumentos abaixo para construir diferentes cenários.
                                        Você pode especificar valores de percentual de pares discordantes na categoria ", prop2n_dep_cat1(), " e uma sequência de percentual de pares discordantes na categoria ", prop2n_dep_cat2(), ".
                                        Demais informações serão recuperadas do painel lateral."
        )
      ),


      HTML(paste0(
        "<b>Defina a sequência de valores para o percentual (%) de pares discordantes na categoria <i>", prop2n_dep_cat1(), "</i>:</b>")),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
          numericInput("prop2n_dep_from", "Mínimo", value = input$prop2n_dep_discordante1, step = 1)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("prop2n_dep_to", "Máximo", value = input$prop2n_dep_discordante1 + 10, step = 1)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("prop2n_dep_by", "Intervalo", value = 2, min = 0, step = 1) %>%
            .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.")
      ),


      fluidRow(
        column(6,
               textInput(inputId = "prop2n_dep_seq_prop2",
                         label   = paste0("Digite valores de percentual de pares discordantes na categoria ", prop2n_dep_cat2(), " (Disc. 2) para fazer o gráfico"),
                         value   = paste0(c(input$prop2n_dep_discordante2, input$prop2n_dep_discordante2 + 1, input$prop2n_dep_discordante2 + 3), collapse = ", "),
                         width   = "400px") %>%
                 .help_buttom(body = paste0("Defina os valores de percentual (%) de pares discordantes na categoria <i>", prop2n_dep_cat2(), "</i>.
                                                      Esses valores serão utilizados para criar diferentes linhas no gráfico.
                                                      Separe os valores por vírgula ',' e utilize ponto '.' como decimal."))
        )
      ),



      br(),

      plotly::plotlyOutput("prop2n_dep_plot", width = "80%"),
      br(), br(),
      downloadButton("prop2n_dep_down","Download tabela"),
      DT::dataTableOutput("prop2n_dep_tab", width = "100%")


    ))
  })




  eval(parse(text = check_text_input_to_vector("prop2n_dep_seq_prop2")))

  prop2n_dep_cenarios <- reactive({

    prop2 <- text_input_to_vector(input$prop2n_dep_seq_prop2)
    req(length(prop2) > 0)


    expand.grid(`% de pares discordantes no grupo 1` = seq(from = input$prop2n_dep_from,
                                                           to = input$prop2n_dep_to,
                                                           by = input$prop2n_dep_by),
                `% de pares discordantes no grupo 2` = prop2,
                `Poder (%)` = input$prop2n_dep_pwr,
                `Nível de significância (%)` =  input$prop2n_dep_sig,
                `Hipótese alternativa` = input$prop2n_dep_alternativa,
                stringsAsFactors = FALSE) %>%

      mutate(`Tamanho da amostra (pares)` = mapply(
        function(perc1, perc2, sig, power, alternative){
          tryCatch({

            numerador   <- max(perc1, perc2)
            denominador <- min(perc1, perc2)

            ceiling(
              MESS::power_mcnemar_test(n = NULL,
                                       psi = numerador/denominador,
                                       paid = denominador/100,
                                       sig.level = sig/100,
                                       power = power/100,
                                       alternative = alternative,
                                       method = "normal")$n)

          },
          warning = function(warning_condition) { NA },
          error   = function(error_condition) { NA })
        },
        `% de pares discordantes no grupo 1`,
        `% de pares discordantes no grupo 2`,
        `Nível de significância (%)`, `Poder (%)`, `Hipótese alternativa`)
      )
  })





  output$prop2n_dep_plot <- plotly::renderPlotly({


    g1 <- prop2n_dep_cenarios() %>%
      mutate(`Disc. 2` = factor(`% de pares discordantes no grupo 2`)) %>%
      ggplot(aes(x = `% de pares discordantes no grupo 1`,
                 y = `Tamanho da amostra (pares)`,
                 color = `Disc. 2`))+
      geom_point() +
      geom_line() +
      xlab(paste0("% de pares discordantes na categoria ", prop2n_dep_cat1())) +
      ylab("Tamanho total da amostra (pares)*") +
      scale_x_continuous(breaks = seq(from = input$prop2n_dep_from, to = input$prop2n_dep_to, by = input$prop2n_dep_by)) +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(palette = "Set1")


    plotly::ggplotly(g1, tooltip = c("x", "colour", "y")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                        showarrow = F, xref='paper', yref='paper',
                                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                        font=list(size=10)))
  })


  prop2n_dep_cenarios_print <- reactive({
    dataset <- prop2n_dep_cenarios()
    colnames(dataset) <- c(paste0("% de pares discordantes na categoria ", prop2n_dep_cat1()),
                           paste0("% de pares discordantes na categoria ", prop2n_dep_cat2()),
                           "Poder (%)",
                           "Nível de significância (%)",
                           "Hipótese alternativa",
                           "Tamanho da amostra (pares)")

    dataset
  })


  output$prop2n_dep_tab <- DT::renderDataTable({

    prop2n_dep_cenarios_print() %>%
      DT::datatable(extensions = c('FixedColumns'),
                    rownames   = FALSE,
                    filter     = "none",
                    #callback   = DT::JS("$('div.dwnld').append($('#download_th2mean_tab'));"),
                    options    = list(pageLength = 10,
                                      scrollX = TRUE,
                                      scrollY = TRUE,
                                      searching = FALSE,
                                      fixedColumns = list(leftColumns = 1),
                                      dom = 'B<"dwnld">frtip'
                                      # buttons = list(list(extend = 'none'))
                    )
      )
  })


  output$prop2n_dep_down <- downloadHandler(
    filename = function() { "Cenarios_tamanho_amostra_duas_prop_dep.xlsx"},
    content = function(file) {writexl::write_xlsx(prop2n_dep_cenarios_print(), path = file)}
  )






















  #________________------
  # 2 medias dependentes ----
  # Comparação entre duas médias de grupos pareados

  output$print_desvio_tpareado <- renderText({
    s1 <- input$popup_sd_baseline
    s2 <- input$popup_sd_follow

    temp <- s1^2 + s2^2 - (2*s1*s2*input$popup_sd_correlation)

    paste0("<b><font size = '5'><br><br>",
           "<i>Desvio padrão da diferença</i> = ", round(sqrt(temp), 4))
  })




  # Testar ----

  observeEvent(c(input$show_desvio_tpareado, input$show_desvio_tpareado2), {
    validate(need(input$show_desvio_tpareado > 0 || input$show_desvio_tpareado2 > 0, ''))

    eval(parse(text = warning_numero_positivo("popup_sd_baseline")))
    eval(parse(text = warning_numero_positivo("popup_sd_follow")))

    observeEvent(input$popup_sd_correlation, {
      shinyFeedback::hideFeedback("popup_sd_correlation")
      if(is.na(input$popup_sd_correlation)){
        shinyFeedback::showFeedbackWarning(
          inputId = "popup_sd_correlation",
          text = "Deve ser fornecido um valor.",
          color = "red"
        )
      } else if (input$popup_sd_correlation >= 1) {
        shinyFeedback::showFeedbackWarning(
          inputId = "popup_sd_correlation",
          text = "Deve ser menor do que 1.",
          color = "red"
        )
      } else if (input$popup_sd_correlation <= -1) {
        shinyFeedback::showFeedbackWarning(
          inputId = "popup_sd_correlation",
          text = "Deve ser maior do que -1.",
          color = "red"
        )
      }
    })


    showModal(
      modalDialog(
        title = "Obter o desvio padrão da diferença entre grupos pareados",
        fluidPage(

          numericInput( "popup_sd_baseline",
                        "Desvio padrão do Grupo 1",
                        value = 4,
                        min = 0,
                        max = Inf,
                        step = 1),
          HTML("Em estudos longitudinais o Grupo 1 pode ser entendido como o Momento 1.<br><br>"),

          numericInput( "popup_sd_follow",
                        "Desvio padrão do Grupo 2",
                        value = 4.4,
                        min = 0,
                        max = Inf,
                        step = 1),
          HTML("Em estudos longitudinais o Grupo 2 pode ser entendido como o Momento 2.<br><br>"),

          numericInput( "popup_sd_correlation",
                        "Correlação entre as medidas do Grupo1 e Grupo2",
                        value = 0.5,
                        min = -1,
                        max = 1,
                        step = .1),

          htmlOutput("print_desvio_tpareado")

        ),
        easyClose = TRUE,
        footer    = NULL,
        size      = "l"
      )
    )
  })




  alternative_mean_paired_n <- reactive({
    case_when(input$alternative_mean_paired_n == 'two.sided' ~ "two.sided",
              input$alternative_mean_paired_n == 'one.sided' &  input$mean_paired_n_differenca > 0  ~ "greater",
              input$alternative_mean_paired_n == 'one.sided' &  input$mean_paired_n_differenca < 0  ~ "less")
  })


  output$mean_paired_formula <- renderUI({

    sinal_h0 <- case_when(alternative_mean_paired_n() == 'two.sided' ~ "=",
                          alternative_mean_paired_n() == 'greater'   ~ "\\leq",
                          alternative_mean_paired_n() == 'less'      ~ "\\geq")

    sinal_h1 <- case_when(alternative_mean_paired_n() == 'two.sided' ~ "\\neq",
                          alternative_mean_paired_n() == 'greater'   ~ ">",
                          alternative_mean_paired_n() == 'less'      ~ "<")

    withMathJax(paste0("$$H_0: \\mu_D ", sinal_h0, " 0",
                       " \\text{  vs  } H_1: \\mu_D", sinal_h1, " 0 $$"))
  })





  eval(parse(text = warning_numero_positivo("sigma_mean_paired_n")))
  eval(parse(text = warning_prop("power_mean_paired_n")))
  eval(parse(text = warning_prop("sig_mean_paired_n")))

  observeEvent(input$mean_paired_n_differenca, {
    if(is.na(input$mean_paired_n_differenca)){
      shinyFeedback::showFeedbackWarning(
        inputId = "mean_paired_n_differenca",
        text = "Deve ser fornecido um valor.",
        color = "red")
    } else if(input$mean_paired_n_differenca == 0){
      shinyFeedback::showFeedbackWarning(
        inputId = "mean_paired_n_differenca",
        text = "Deve ser diferente de 0.",
        color = "red")
    } else {
      shinyFeedback::hideFeedback("mean_paired_n_differenca")
    }
  })

  output$mean_paired_n <- renderText({

    validate(
      need(!is.na(input$mean_paired_n_differenca),     "É obrigatório fornecer um valor da diferença a ser detectada."),
      need(!is.na(input$sigma_mean_paired_n), "É obrigatório fornecer um valor de desvio padrão da diferença entre as médias."),

      need(input$mean_paired_n_differenca != 0,       "A diferença a ser detectada deve ser diferente de zero."),
      need(input$sigma_mean_paired_n > 0,    "O desvio padrão da diferença entre as médias deve ser maior do que zero."),

      # need(input$alternative_mean_paired_n == "two.sided" |
      #        (input$mean_paired_n_differenca > 0 & input$alternative_mean_paired_n != 'less') |
      #        (input$mean_paired_n_differenca < 0 & input$alternative_mean_paired_n != 'greater'),
      #      paste0("Não é possível calcular um tamanho de amostra para a diferença esperada especificada e para o tipo de hipótese alternativa escolhido. ",
      #             "Verifique o valor da diferença a ser detectada e a hipótese alternativa.")),

      need(!is.na(input$power_mean_paired_n), "É obrigatório fornecer um valor do poder."),
      need(!is.na(input$sig_mean_paired_n),   "É obrigatório fornecer um valor do nível de significância."),

      need(input$power_mean_paired_n > 0, "O poder deve ser maior do que zero."),
      need(input$sig_mean_paired_n > 0,   "O nível de significância deve ser maior do que zero.")
    )


    d <- input$mean_paired_n_differenca/input$sigma_mean_paired_n

    # code <- paste0(
    #   "pwr::pwr.t.test(",
    #   "n = NULL, ",
    #   "d =", d, ", ",
    #   "sig.level = ", input$sig_mean_paired_n/100, ", ",
    #   "power = ", input$power_mean_paired_n/100, ", ",
    #   "type = 'paired', ",
    #   "alternative = '", input$alternative_mean_paired_n, "')"
    # )

    # code <- paste0(
    #   "MESS::power_t_test(",
    #   "delta = ", input$mean_paired_n_differenca, ", ",
    #   "sd = ", input$sigma_mean_paired_n, ", ",
    #   "power = ", input$power_mean_paired_n/100, ", ",
    #   "sig.level = ", input$sig_mean_paired_n/100, ", ",
    #   "type = 'paired', df.method = 'classical')"
    # )

    code <- paste0("stats::power.t.test(",
                   "n = NULL, ",
                   "delta = abs(", input$mean_paired_n_differenca, "), ",
                   "sd = ", input$sigma_mean_paired_n, ", ",
                   "type = 'paired', ",
                   "sig.level = ", input$sig_mean_paired_n/100,  ", ",
                   "power = ", input$power_mean_paired_n/100, ", ",
                   "alternative = '",
                   ifelse(alternative_mean_paired_n() == "two.sided",
                          "two.sided", "one.sided"), "')")

    n <- try_n(code)
    eval(parse(text = validate_n("n")))

    n <- ceiling(n$n)
    nperdas <- n_perdas(n, input$mean_paired_n_perdas_recusa)
    eval(parse(text = validate_n_inf("n")))


    cabecalho <- paste0("<b><font size = '5'>Tamanho amostral calculado: ", n, "<i> pares</i>",
                        "</font></b></br></br><i>Sugestão de texto:</i></br></br>")




    texto_desvio <- paste0(
      "Foi calculado o tamanho de amostra para detectar que a média das diferenças de <b>Y</b> entre os grupos 1 e 2 é ",
      if(alternative_mean_paired_n() == "two.sided"){
        "diferente de zero, "
      } else if(alternative_mean_paired_n() == "less"){
        "menor do que zero, "
      } else{
        "maior do que zero, "
      },

      "tendo uma média das diferenças de <b>", abs(input$mean_paired_n_differenca), " u.m.</b> como relevante para o estudo. ",
      "Para isso foi utilizado a ", .txt_citacao_tap, ". ",
      "Considerando poder de <b>", input$power_mean_paired_n, "%</b>, ",
      "nível de significância de <b>", input$sig_mean_paired_n, "%</b> ",
      "e desvio padrão da diferença esperado de <b>", input$sigma_mean_paired_n, " u.m.</b> (dados de Fulano (1900)), ",
      "chegou-se ao tamanho total de <b>", n, "</b> pares. ",
      "Acrescentando <b>", input$mean_paired_n_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", nperdas, "</b> pares.")

    paste0(cabecalho,
           texto_desvio,
           .txt_referencia_tap, print_r_code(code))

  })


  ## Cenarios ----

  output$cenarios_duas_medias_dep_thUi <- renderUI({

    if(input$mean_paired_n_differenca > 0){
      dif_start <- input$mean_paired_n_differenca
      dif_end  <- input$mean_paired_n_differenca + 1
      dif_by   <- 0.1
    } else{
      dif_start <- input$mean_paired_n_differenca - 1
      dif_end  <- input$mean_paired_n_differenca
      dif_by   <- 0.1
    }


    fluidPage(fluidRow(

      br(),
      HTML('<hr style="color: black;">'),
      br(),br(),

      titlePanel("Construção de cenários"),
      br(),

      wellPanel("Utilize os argumentos abaixo para construir diferentes cenários.
                                        Você pode especificar valores de desvio padrão e uma sequência de diferença a ser detectada.
                                        Demais informações serão recuperadas do painel lateral."
      ),


      HTML("<b>Defina a sequência de valores para a diferença a ser detectada:</b>"),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
          numericInput("th2_mean_paired_from", "Mínimo", value = dif_start, step = 0.5)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("th2_mean_paired_to", "Máximo", value = dif_end, step = 0.5)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("th2_mean_paired_by", "Intervalo", value = dif_by, min = 0, step = 0.1) %>%
            .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                        title = "Sequência")),


      fluidRow(
        column(6,
               textInput(inputId = "th2mean_paired_dep_desvio",
                         label   = "Digite valores de desvio padrão (DP) da diferença para fazer o gráfico",
                         value   = paste0(c(input$sigma_mean_paired_n, input$sigma_mean_paired_n + 0.2, input$sigma_mean_paired_n + 0.5), collapse = ", "),
                         width   = "400px") %>%
                 .help_buttom(body = "Defina os valores de desvio padrão.
                                                      Esses valores serão utilizados para criar diferentes linhas no gráfico.
                                                      Separe os valores por vírgula ',' e utilize ponto '.' como decimal.")
        )
      ),



      br(),

      plotly::plotlyOutput("th2mean_paired_plot", width = "80%"),
      br(), br(),
      downloadButton("download_th2mean_paired_tab","Download tabela"),
      DT::dataTableOutput("th2mean_paired_tab", width = "100%")


    ))
  })




  eval(parse(text = check_text_input_to_vector("th2mean_paired_dep_desvio")))

  tab_mean2_paired_cenarios <- reactive({

    desvios <- text_input_to_vector(input$th2mean_paired_dep_desvio)
    req(length(desvios) > 0)

    expand.grid(`Diferença a ser detectada` = seq(from = input$th2_mean_paired_from,
                                                  to = input$th2_mean_paired_to,
                                                  by = input$th2_mean_paired_by),
                `Desvio padrão da diferença` = desvios,
                `Poder (%)` = input$power_mean_paired_n,
                `Nível de significância (%)` =  input$sig_mean_paired_n,
                `Hipótese alternativa` = ifelse(alternative_mean_paired_n() == "two.sided",
                                                "two.sided", "one.sided"),

                stringsAsFactors = FALSE) %>%



        mutate(`Tamanho da amostra (pares)` = mapply(
          function(delta, sd, sig.level, power, alternative){
            tryCatch({
              ceiling(
                stats::power.t.test(n = NULL,
                                    delta = abs(delta),
                                    sd = sd,
                                    type = 'paired',
                                    sig.level = sig.level/100,
                                    power = power/100,
                                    alternative = alternative)$n)

              },
              warning = function(warning_condition) { NA },
              error   = function(error_condition) { NA })
          }, `Diferença a ser detectada`, `Desvio padrão da diferença`, `Nível de significância (%)`, `Poder (%)`, `Hipótese alternativa`)
        )
  })





  output$th2mean_paired_plot <- plotly::renderPlotly({


    g1 <- tab_mean2_paired_cenarios() %>%
      mutate(DP = factor(`Desvio padrão da diferença`)) %>%
      ggplot(aes(x = `Diferença a ser detectada`, y = `Tamanho da amostra (pares)`, color = DP))+
      geom_point() +
      geom_line() +
      xlab("Diferença a ser detectada") +
      ylab("Tamanho total da amostra (pares)*") +
      scale_x_continuous(breaks = seq(from = input$th2_mean_paired_from, to = input$th2_mean_paired_to, by = input$th2_mean_paired_by)) +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(palette = "Set1")


    plotly::ggplotly(g1, tooltip = c("x", "colour", "y")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                        showarrow = F, xref='paper', yref='paper',
                                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                        font=list(size=10)))
  })



  output$th2mean_paired_tab <- DT::renderDataTable({

    tab_mean2_paired_cenarios() %>%
      DT::datatable(extensions = c('FixedColumns'),
                    rownames   = FALSE,
                    filter     = "none",
                    #callback   = DT::JS("$('div.dwnld').append($('#download_th2mean_tab'));"),
                    options    = list(pageLength = 10,
                                      scrollX = TRUE,
                                      scrollY = TRUE,
                                      searching = FALSE,
                                      fixedColumns = list(leftColumns = 1),
                                      dom = 'B<"dwnld">frtip'
                                      # buttons = list(list(extend = 'none'))
                    )
      )
  })


  output$download_th2mean_paired_tab <- downloadHandler(
    filename = function() { "Cenarios_tamanho_amostra_duas_medias.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_mean2_paired_cenarios(), path = file)}
  )







  # Poder ----


  eval(parse(text = warning_numero_positivo("n_mean_paired_power")))
  eval(parse(text = warning_numero_positivo("sigma_mean_paired_power")))
  eval(parse(text = warning_prop("sig_mean_paired_power")))

  observeEvent(input$mean_paired_poder_differenca, {
    if(is.na(input$mean_paired_poder_differenca)){
      shinyFeedback::showFeedbackWarning(
        inputId = "mean_paired_poder_differenca",
        text = "Deve ser fornecido um valor.",
        color = "red")
    } else if(input$mean_paired_poder_differenca == 0){
      shinyFeedback::showFeedbackWarning(
        inputId = "mean_paired_poder_differenca",
        text = "Deve ser diferente de 0.",
        color = "red")
    } else {
      shinyFeedback::hideFeedback("mean_paired_poder_differenca")
    }
  })



  output$mean_paired_power <- renderText({

    code <- paste0("stats::power.t.test(",
                   "n = ", input$n_mean_paired_power, ", ",
                   "delta = ", input$mean_paired_poder_differenca, ", ",
                   "sd = ", input$sigma_mean_paired_power, ", ",
                   "type = 'paired', ",
                   "sig.level = ", input$sig_mean_paired_power/100,  ", ",
                   "power = NULL, ",
                   "alternative = 'two.sided')")

    power_pwd <- eval(parse(text = code))

    paste0("<b><font size = '5'>Poder calculado: ", round(100*power_pwd$power, digits = 3),
           "%</font></b></br></br><i></i></br></br>",

           "Foi calculado o poder para detectar diferença entre as médias dos grupos dependentes  X1 e X2, ",
           "por meio da ", .txt_citacao_tap, ". ",

           "Considerando um nível de significância de <b>", input$sig_mean_paired_power, "</b>, ",
           "uma diferença de <b>", input$mean_paired_poder_differenca, "</b>, ",
           "desvio padrão das diferenças de <b>", input$sigma_mean_paired_power, "</b> ",
           "e um tamanho de amostra de <b>", input$n_mean_paired_power, "</b>, ",
           "chegou-se à um poder de de <b>", round(100*power_pwd$power, digits = 1), "%</b>.",

           .txt_referencia_tap, print_r_code(code))

  })





  # 2 Deltas----


  output$th2_mean_dep_out <- renderText({

    if(!input$th2_mean_dep_utilizar_medias){
      validate(
        need(!is.na(input$th2_mean_dep_diff), "É obrigatório fornecer um valor da diferença a ser detectada."),
        need(input$th2_mean_dep_diff != 0, "A diferença a ser detectada deve ser diferente de zero.")
      )
    } else{

      validate(
        need(!is.na(input$th2_mean_dep_delta_tratamento), "É obrigatório fornecer um valor da mudança média ao longo do tempo do grupo Tratamento."),
        need(!is.na(input$th2_mean_dep_delta_controle), "É obrigatório fornecer um valor da mudança média ao longo do tempo do grupo Controle.")
      )
    }


    validate(
      need(!is.na(input$th2_mean_dep_pwr), "É obrigatório fornecer um valor do poder."),
      need(!is.na(input$th2_mean_dep_sig),   "É obrigatório fornecer um valor do nível de significância."),
      need(!is.na(input$th2_mean_dep_sigma1), "É obrigatório fornecer um valor do desvio padrão do grupo Tratamento no início do estudo."),
      need(!is.na(input$th2_mean_dep_sigma2), "É obrigatório fornecer um valor do desvio padrão do grupo Tratamento no final do estudo."),
      need(!is.na(input$th2_mean_dep_rho), "É obrigatório fornecer um valor da correlação das medidas (início e fim) dentro do grupo Tratamento."),

      need(input$th2_mean_dep_pwr < 100, "O poder deve ser menor do que 100%."),
      need(input$th2_mean_dep_sig < 100,   "O nível de significância deve ser menor do que 100%."),
      need(input$th2_mean_dep_pwr > 0, "O poder deve ser maior do que zero."),
      need(input$th2_mean_dep_sig > 0,   "O nível de significância deve ser maior do que zero."),
      need(input$th2_mean_dep_sigma1 > 0, "O desvio padrão do grupo Tratamento no início do estudo deve ser maior do que zero."),
      need(input$th2_mean_dep_sigma2 > 0, "O desvio padrão do grupo Tratamento no final do estudo deve ser maior do que zero."),
      need(input$th2_mean_dep_rho >= -1 & input$th2_mean_dep_rho <= 1,
           "A correlação das medidas (início e fim) dentro do grupo Tratamento deve ser estar entre -1 e 1.")
    )




    if(!input$th2_mean_dep_utilizar_medias){
      delta <- input$th2_mean_dep_diff
      info_texto <- paste0("diferença esperada/ desejada de <b>", input$th2_mean_dep_diff, " u.m</b>, ")
    } else {
      delta <- abs(input$th2_mean_dep_delta_tratamento - input$th2_mean_dep_delta_controle)
      info_texto <- paste0("uma variação média no intervalo de tempo de <b>",
                           input$th2_mean_dep_delta_tratamento, " u.m.</b> e <b>",
                           input$th2_mean_dep_delta_controle, " u.m.</b>",
                           " para o grupo Tratamento e Controle, respectivamente, ")

    }


    code <- paste0(
      "powerMediation::ssLongFull(",
      "delta  = ", delta, ", ",
      "sigma1 = ", input$th2_mean_dep_sigma1, ", ",
      "sigma2 = ", input$th2_mean_dep_sigma2, ", ",
      "rho    = ", input$th2_mean_dep_rho, ", ",
      "alpha  = ", input$th2_mean_dep_sig/100, ", ",
      "power  = ", input$th2_mean_dep_pwr/100, ")"
    )


    n <- try_n(code)
    eval(parse(text = validate_n("n")))

    n <- 2 * ceiling(n/2)
    nperdas <- n_perdas(n, input$th2_mean_dep_perdas_recusa)
    nperdas <- 2 * ceiling(nperdas/2)

    eval(parse(text = validate_n("n")))


    paste0("<b><font size = '5'>Tamanho amostral calculado: ", n, " (<i>", n/2, " em cada grupo</i>)",
           "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

           "Foi calculado o tamanho de amostra para detectar diferenças, na mudança média de <b>Y</b>, em um intervalo de tempo <b>T</b> entre os grupos Tratamento e o Controle, ",
           "por meio da ", .txt_citacao_tap, ". ",
           "Considerando poder de <b>", input$th2_mean_dep_pwr, "%</b>, ",
           "nível de significância de <b>", input$th2_mean_dep_sig, "%</b>, ",
           info_texto,
           "correlação entre as medidas no tempo do grupo Tratamento de <b>", input$th2_mean_dep_rho, "</b>, ",
           "desvios padrão de <b>", input$th2_mean_dep_sigma1, "</b> e <b>", input$th2_mean_dep_sigma2, " u.m.</b> ",
           "do grupo Tratamento no ínicio e no final do estudo, respectivamente (dados de Fulano (1900)), ",
           "chegou-se ao tamanho total da amostra de <b>", n, "</b> sujeitos, sendo <b>", n/2, "</b> sujeitos em cada grupo. ",
           "Acrescentando <b>", input$th2_mean_dep_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", nperdas, "</b>.",

           .txt_referencia_tap, print_r_code(code)
    )


  })




  #____________-----
  # Medidas repetidas -----


  # Testar ----
  observeEvent(input$show_th_rep, {
    showModal(
      modalDialog(
        title = "Ajustes",
        fluidPage(

          HTML("<b>Preencha os campos abaixo de acordo com seu estudo para que sirvam de guia no preenchimento dos demais campos</b>."),
          br(), br(),
          textInput(inputId = "rep_nome_desfecho",
                    label   = "Descreva o nome do desfecho",
                    value   = ifelse(input$show_th_rep == 0, "Y", rep_nome_desfecho())),
          HTML(paste0("<i>", str_remove_all(.txt_desfecho, "<br><br>"), "</i>")),
          br(), br(),
          textInput(inputId = "rep_grupoTratamento",
                    label   = "Descreva um nome para o grupo Tratamento",
                    value   = ifelse(input$show_th_rep == 0, "Tratamento", rep_grupoTratamento())),

          HTML("<i>Em alguns estudos o grupo Tratamento também pode ser chamadado de grupo Intervenção ou grupo Exposto.</i><br><br>"),

          textInput(inputId = "rep_grupoControle",
                    label   = "Descreva um nome para o grupo Controle",
                    value   = ifelse(input$show_th_rep == 0, "Controle", rep_grupoControle())),

          HTML("<i>Em alguns estudos o grupo Controle também pode ser chamadado de grupo Placebo/ Sham ou grupo Não exposto.</i>"),


        ),
        easyClose = TRUE,
        footer    = NULL
      )
    )
  })



  rep_grupoControle <- reactive({
    ifelse(is.null(input$rep_grupoControle), "Controle", input$rep_grupoControle)
  })

  rep_grupoTratamento <- reactive({
    ifelse(is.null(input$rep_grupoTratamento), "Tratamento", input$rep_grupoTratamento)
  })

  rep_nome_desfecho <- reactive({
    ifelse(is.null(input$rep_nome_desfecho), "Y", input$rep_nome_desfecho)
  })



  output$rep_formula1 <- renderUI({
    withMathJax(
      paste0("$$H_0: \\mu_{", rep_grupoTratamento(), "} = \\mu_{", rep_grupoControle(), "} $$"))
  })

  output$rep_formula2 <- renderUI({
    withMathJax(
      paste0("$$H_1: \\mu_{", rep_grupoTratamento(), "} \\neq \\mu_{", rep_grupoControle(), "} $$"))
  })


  observeEvent(input$rep_n_tempos, {
      shinyFeedback::hideFeedback("rep_n_tempos")

      if(is.na(input$rep_n_tempos)){
        shinyFeedback::showFeedbackWarning(
          inputId = "rep_n_tempos",
          text = "Deve ser fornecido um valor.",
          color = "red"
        )
      } else if (input$rep_n_tempos < 2) {
        shinyFeedback::showFeedbackWarning(
          inputId = "rep_n_tempos",
          text = "Devem haver pelo menos 2 momentos.",
          color = "red"
        )
      } else if(input$rep_n_tempos%%1 != 0){
        shinyFeedback::showFeedbackWarning(
          inputId = "rep_n_tempos",
          text = "Deve ser um número inteiro positivo.",
          color = "red"
        )
      }
    })



  eval(parse(text = warning_numero_positivo("rep_sigma1")))
  eval(parse(text = warning_numero_positivo("rep_sigma2")))
  eval(parse(text = warning_numero_positivo("rep_ratio")))

  eval(parse(text = warning_prop("rep_ar1_rho1", entre0e1 = TRUE)))
  eval(parse(text = warning_prop("rep_ar1_rho2", entre0e1 = TRUE)))

  eval(parse(text = warning_prop("rep_power")))
  eval(parse(text = warning_prop("rep_sig")))
  eval(parse(text = warning_perdas("rep_perdas_recusa")))







  # Matriz de correlacao do grupo tratamento

  correlation_R1 <- reactiveValues(mcor = data.frame(a = 1))

  observeEvent(input$rep_n_tempos, {

    req(!is.na(input$rep_n_tempos))
    req(input$rep_n_tempos%%1 == 0 & input$rep_n_tempos > 1)

    # Cria uma matriz AR1 inicial
    tempos <- ifelse(is.null(input$rep_n_tempos), 4, input$rep_n_tempos)
    exponent <- abs(matrix(1:tempos - 1, nrow = tempos, ncol = tempos, byrow = TRUE) - (1:tempos - 1))
    M_correlation <- as.data.frame(0.9^exponent)
    colnames(M_correlation) <- paste0("Momento ", 1:tempos)
    rownames(M_correlation) <- paste0("Momento ", 1:tempos)
    correlation_R1$mcor <- M_correlation
  })







  output$rep_cor_grupo1 <- DT::renderDT({
    temp <- input$rep_cor_grupo1_cell_edit
    DT::datatable(correlation_R1$mcor,
                  editable = "cell",
                  extensions = c('FixedColumns'),
                  rownames   = TRUE,
                  filter     = "none",
                  options    = list(autoWidth = FALSE,
                                    searching = FALSE,
                                    ordering  = FALSE,
                                    pageLength = 15,
                                    dom = 't',
                                    columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  })





  observeEvent(input$rep_cor_grupo1_cell_edit, {
    cell <- input$rep_cor_grupo1_cell_edit
    newdf <- correlation_R1$mcor
    new_value <- cell$value %>%
      as.character() %>%
      str_replace(",", ".") %>%
      gsub("[^0-9.-]", "", .) %>%
      as.numeric()

    newdf[cell$row, cell$col] <- new_value
    newdf[cell$col, cell$row] <- new_value

    correlation_R1$mcor <- newdf
  })







  rep_problemas_matriz_correlacao1 <- reactive({
    problemas <- ""

    if(any(is.na(correlation_R1$mcor))){
      problemas <- paste0('<font color = "red"><b>Todas as células devem ser preenchidas.</b></font><br/>')
    } else if(any(abs(correlation_R1$mcor) > 1)){
      problemas <- paste0('<font color = "red"><b>Todas as correlações devem estar entre -1 e 1.</b></font><br/>')
    } else if(any(abs(correlation_R1$mcor) > 1)){
      problemas <- paste0('<font color = "red"><b>Os valores da diagonal devem ser igual a 1.</b></font><br/>')
    }
    problemas
  })








  # Matriz de correlacao do grupo Controle

  correlation_R2 <- reactiveValues(mcor = data.frame(a = 1))

  observeEvent(input$rep_n_tempos, {

    req(!is.na(input$rep_n_tempos))
    req(input$rep_n_tempos%%1 == 0 & input$rep_n_tempos > 1)
    # Cria uma matriz AR1 inicial
    tempos <- ifelse(is.null(input$rep_n_tempos), 4, input$rep_n_tempos)
    exponent <- abs(matrix(1:tempos - 1, nrow = tempos, ncol = tempos, byrow = TRUE) - (1:tempos - 1))
    M_correlation <- as.data.frame(0.9^exponent)
    colnames(M_correlation) <- paste0("Momento ", 1:tempos)
    rownames(M_correlation) <- paste0("Momento ", 1:tempos)
    correlation_R2$mcor <- M_correlation
  })







  output$rep_cor_grupo2 <- DT::renderDT({
    temp <- input$rep_cor_grupo2_cell_edit
    DT::datatable(correlation_R2$mcor,
                  editable = "cell",
                  extensions = c('FixedColumns'),
                  rownames   = TRUE,
                  filter     = "none",
                  options    = list(autoWidth = FALSE,
                                    searching = FALSE,
                                    ordering  = FALSE,
                                    pageLength = 15,
                                    dom = 't',
                                    columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  })





  observeEvent(input$rep_cor_grupo2_cell_edit, {
    cell <- input$rep_cor_grupo2_cell_edit
    newdf <- correlation_R2$mcor
    new_value <- cell$value %>%
      as.character() %>%
      str_replace(",", ".") %>%
      gsub("[^0-9.-]", "", .) %>%
      as.numeric()

    newdf[cell$row, cell$col] <- new_value
    newdf[cell$col, cell$row] <- new_value

    correlation_R2$mcor <- newdf
  })







  rep_problemas_matriz_correlacao2 <- reactive({
    problemas <- ""

    if(any(is.na(correlation_R2$mcor))){
      problemas <- paste0('<font color = "red"><b>Todas as células devem ser preenchidas.</b></font><br/>')
    } else if(any(abs(correlation_R2$mcor) > 1)){
      problemas <- paste0('<font color = "red"><b>Todas as correlações devem estar entre -1 e 1.</b></font><br/>')
    } else if(any(abs(correlation_R2$mcor) > 1)){
      problemas <- paste0('<font color = "red"><b>Os valores da diagonal devem ser igual a 1.</b></font><br/>')
    }
    problemas
  })




  output$rep_print_matriz_cor <- renderUI({
    fluidPage(fluidRow(wellPanel(

      # Tratamento
      if(rep_problemas_matriz_correlacao1() != ""){
        HTML(paste0('<font size = "+0.1"><font color = "red">', "<b>Matriz de correlação do grupo ", rep_grupoTratamento(), ":</b>", '</font></font><br/>'))
      } else{
        HTML(paste0("<b>Matriz de correlação do grupo ", rep_grupoTratamento()), ":</b>")
      },

      DT::dataTableOutput("rep_cor_grupo1"),

      if(rep_problemas_matriz_correlacao1() != ""){
        HTML(rep_problemas_matriz_correlacao1())
      },


      br(), br(),
      # Controle
      if(rep_problemas_matriz_correlacao2() != ""){
        HTML(paste0('<font size = "+0.1"><font color = "red">', "<b>Matriz de correlação do grupo ", rep_grupoTratamento(), ":</b>", '</font></font><br/>'))
      } else{
        HTML(paste0("<b>Matriz de correlação do grupo ", rep_grupoControle()), ":</b>")
      },

      DT::dataTableOutput("rep_cor_grupo2"),

      if(rep_problemas_matriz_correlacao2() != ""){
        HTML(rep_problemas_matriz_correlacao2())
      },




      br(), br()
    )))
  })



  output$rep_retencao1 <- renderUI({
    len <- as.integer(input$rep_n_tempos)
    lapply(1:len, function(i) {
      div(
        numericInput(
          inputId = paste0("retencao1_", i),
          label = paste0("Momento ", i),
          value = 100 + 1 - i
        )
      )
    })
  })


  output$rep_retencao2 <- renderUI({
    len <- as.integer(input$rep_n_tempos)
    lapply(1:len, function(i) {
      div(
        numericInput(
          inputId = paste0("retencao2_", i),
          label = paste0("Momento ", i),
          value = 100 + 1 - i
        )
      )
    })
  })

  observe({
    req(!is.null(input$rep_n_tempos))
    req(!is.na(input$rep_n_tempos))

    len <- as.integer(input$rep_n_tempos)

    req(!is.null(input[[paste0("retencao1_", len)]]))

    lapply(1:len, function(i) {
      shinyFeedback::hideFeedback(paste0("retencao1_", i))
      if(is.na(input[[paste0("retencao1_", i)]])){
        shinyFeedback::showFeedbackWarning(
          inputId = paste0("retencao1_", i),
          text = "Deve ser fornecido um valor.",
          color = "red"
        )
      } else if(input[[paste0("retencao1_", i)]] > 100){
        shinyFeedback::showFeedbackWarning(
          inputId = paste0("retencao1_", i),
          text = "Deve ser fornecido um valor menor do que 100%",
          color = "red"
        )
      } else if(input[[paste0("retencao1_", i)]] < 0){
        shinyFeedback::showFeedbackWarning(
          inputId = paste0("retencao1_", i),
          text = "Deve ser fornecido um valor maior do que 0%",
          color = "red"
        )
      }
    })
  })


  observe({
    req(!is.null(input$rep_n_tempos))
    req(!is.na(input$rep_n_tempos))

    len <- as.integer(input$rep_n_tempos)

    req(!is.null(input[[paste0("retencao2_", len)]]))

    lapply(1:len, function(i) {
      shinyFeedback::hideFeedback(paste0("retencao2_", i))
      if(is.na(input[[paste0("retencao2_", i)]])){
        shinyFeedback::showFeedbackWarning(
          inputId = paste0("retencao2_", i),
          text = "Deve ser fornecido um valor.",
          color = "red"
        )
      } else if(input[[paste0("retencao2_", i)]] > 100){
        shinyFeedback::showFeedbackWarning(
          inputId = paste0("retencao2_", i),
          text = "Deve ser fornecido um valor menor do que 100%",
          color = "red"
        )
      } else if(input[[paste0("retencao2_", i)]] <= 0){
        shinyFeedback::showFeedbackWarning(
          inputId = paste0("retencao2_", i),
          text = "Deve ser fornecido um valor maior do que 0%",
          color = "red"
        )
      }
    })
  })


  output$medidas_repetidas_ui_sided <- renderUI({

    fluidPage(fluidRow(
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            HTML("<b><font size = '2.8'>Hipóteses a serem testadas*</font></b>"),
            uiOutput("rep_formula1"),
            uiOutput("rep_formula2"),
            HTML("<i><font size = '2.8'>*Resposta média no último tempo</font></i>"),
          ),

          actionLink("show_th_rep", "Mudar nomes"), br(), br(),

          numericInput( "rep_n_tempos",
                        "Número de momentos a ser avaliado",
                        value = 4,
                        min = 2,
                        max = Inf,
                        step = 1
          ),
          # uiOutput("rep_n_temposUi"),

          # uiOutput("rep_cor_matriz"),
          #
          # column(6, uiOutput("selects2")),

          numericInput( "rep_dif_medias",
                        paste0("Diferença do ", rep_nome_desfecho(), " a ser detectada no último tempo de avaliação: (Média do ", rep_grupoTratamento(),
                               ") - (Média do grupo ", rep_grupoControle(), ")"),
                        value = 0.5,
                        min = -Inf,
                        max = Inf,
                        step = .5
          ) %>% .help_buttom(body = paste0("É a menor diferença considerada clinicamente relevante (que tenha algum valor clínico) no último tempo de internação.", .txt_definido_pesquisador)),

          # Desvio padrao
          HTML(paste0("<b><font size = '2.95'>Desvio padrão esperado de ", rep_nome_desfecho(), " no grupo</font></b><br>")),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
              numericInput( "rep_sigma1",
                            rep_grupoTratamento(),
                            value = 1,
                            min = 0,
                            max = Inf,
                            step = 1
              )
          ),
          div(style="display: inline-block;vertical-align:top; width: 49%;",
              numericInput( "rep_sigma2",
                            rep_grupoControle(),
                            value = 1,
                            min = 0,
                            max = Inf,
                            step = 1
              ) %>% .help_buttom(body = .txt_dp, title = "Desvio padrão esperado")
          ),

          wellPanel(

            selectInput('rep_tipo_matriz_cor',
                        "Selecione o tipo de matriz de correlação",
                        choices = c("AR(1)",
                                    "Componente permutável",
                                    "Não estruturada"),
                        selected = 'Não estruturada'
            ),

            conditionalPanel("input.rep_tipo_matriz_cor == 'Não estruturada'",
                             HTML("<i><b>ATENÇÃO!</b> Edite as matrizes de correlação no painel principal ao lado. As matrizes são editáveis, basta clicar duas vezes sobre a célula --> </i><br><br>")
            ),

            conditionalPanel("input.rep_tipo_matriz_cor == 'AR(1)'",
                             HTML(paste0("<b><font size = '2.95'>Parâmetro autorregressivo no grupo</font></b><br>")),
                             div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                                 numericInput( "rep_ar1_rho1",
                                               rep_grupoTratamento(),
                                               value = 0.4,
                                               min = 0,
                                               max = 1,
                                               step = .1
                                 )
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 49%;",
                                 numericInput( "rep_ar1_rho2",
                                               rep_grupoControle(),
                                               value = 0.2,
                                               min = 0,
                                               max = 1,
                                               step = .1
                                 )
                             )
            ),

            conditionalPanel("input.rep_tipo_matriz_cor == 'Componente permutável'",
                             HTML(paste0("<b><font size = '2.95'>Valor da correlação entre os diferentes momentos no grupo</font></b><br>")),
                             div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                                 numericInput( "rep_cs_rho1",
                                               rep_grupoTratamento(),
                                               value = 0.7,
                                               min = -1,
                                               max = 1,
                                               step = .1
                                 )
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 49%;",
                                 numericInput( "rep_cs_rho2",
                                               rep_grupoControle(),
                                               value = 0.7,
                                               min = -1,
                                               max = 1,
                                               step = .1
                                 )
                             )
            )
          ),




          # Retencao
          # HTML('<hr style="color: black;">'),
          # br(),
          wellPanel(
            HTML(paste0("<b>Retenção esperada (%) no grupo</b><br><br>")),
            fluidPage(fluidRow(
              div(style="display: inline-block;vertical-align:top; width: 49%;",
                  wellPanel(
                    HTML(paste0("<b>", rep_grupoTratamento(), ":</b>")),
                    br(), br(),
                    uiOutput("rep_retencao1")
                  )),
              div(style="display: inline-block;vertical-align:top; width: 49%;", wellPanel(
                HTML(paste0("<b>", rep_grupoControle(), ":</b>")),
                br(), br(),
                uiOutput("rep_retencao2")
              ))
            ))
          ),


          # HTML('<hr style="color: black;">'),
          # HTML(paste0("<b><font size = '2.95'>Retenção esperada (%) no grupo</font></b><br>")),
          # div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
          #     textInput( "rep_retencao1",
          #                rep_grupoTratamento(),
          #                value = "100, 90, 80, 70")
          # ),
          # div(style="display: inline-block;vertical-align:top; width: 49%;",
          #     textInput( "rep_retencao2",
          #                   rep_grupoControle(),
          #                   value = "100, 90, 80, 70") %>%
          #       .help_buttom(body = "Retenção esperada dos grupos (% de sujeitos que ainda estão no estudo em dado ponto no tempo",
          #                   title = "Retenção esperada")
          # ),

          fluidPage(fluidRow(

            numericInput( "rep_ratio",
                          paste0("Balanceamento (", rep_grupoTratamento(), ":", rep_grupoControle(), ")"),
                          value = 1,
                          min = 0,
                          max = Inf,
                          step = 0.5
            ) %>% .help_buttom(body = .txt_balanceamento, title = "Balanceamento")
          )),


          numericInput( "rep_power",
                        "Poder (%)",
                        value = 80,
                        min = 0,
                        max = 100,
                        step = 1
          ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
          numericInput( "rep_sig",
                        "Nível de significância (%)",
                        value = 5,
                        min = 0,
                        max = 100,
                        step = 1
          ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
          numericInput( "rep_perdas_recusa",
                        "Perdas/ Recusa (%)",
                        value = 10,
                        min = 0,
                        max = 100,
                        step = 1
          ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
        ),

        mainPanel(
          conditionalPanel("input.rep_tipo_matriz_cor == 'Não estruturada'",
                           uiOutput("rep_print_matriz_cor") %>%
                             shinycssloaders::withSpinner(type = 5),

          ),
          shinycssloaders::withSpinner(htmlOutput("rep_outout_text"), type = 5),
          shinycssloaders::withSpinner(uiOutput("rep_corr_matrix_out"), type = 5),
          shinycssloaders::withSpinner(htmlOutput("rep_out_codigo"), type = 5),
          uiOutput("cenarios_medidas_rep_thUi")

        )
      )
    ))
  })




  # Aqui eh definido a matriz que sera utilizada no calculo -----.

  correlation_Ra <- reactive({

    if(input$rep_tipo_matriz_cor == 'Não estruturada'){
      correlation_Ra_v <- unlist(c(correlation_R1$mcor))

    } else if(input$rep_tipo_matriz_cor == 'AR(1)'){
      tempos <- input$rep_n_tempos
      exponent <- abs(matrix(1:tempos - 1, nrow = tempos, ncol = tempos, byrow = TRUE) - (1:tempos - 1))
      correlation_Ra_v <- c(input$rep_ar1_rho1^exponent)
    } else if(input$rep_tipo_matriz_cor == 'Componente permutável'){
      tempos <- input$rep_n_tempos
      exponent <- matrix(1, nrow = tempos, ncol = tempos, byrow = TRUE)
      diag(exponent) <- 0
      correlation_Ra_v <- c(input$rep_cs_rho1^exponent)
    }
    correlation_Ra_v
  })

  correlation_Rb <- reactive({
    if(input$rep_tipo_matriz_cor == 'Não estruturada'){
      correlation_Rb_v <- unlist(c(correlation_R2$mcor))

    } else if(input$rep_tipo_matriz_cor == 'AR(1)'){
      tempos <- input$rep_n_tempos
      exponent <- abs(matrix(1:tempos - 1, nrow = tempos, ncol = tempos, byrow = TRUE) - (1:tempos - 1))
      correlation_Rb_v <- c(input$rep_ar1_rho2^exponent)
    } else if(input$rep_tipo_matriz_cor == 'Componente permutável'){
      tempos <- input$rep_n_tempos
      exponent <- matrix(1, nrow = tempos, ncol = tempos, byrow = TRUE)
      diag(exponent) <- 0
      correlation_Rb_v <- c(input$rep_cs_rho2^exponent)
    }

    correlation_Rb_v
  })





  rep_retencao_A <- reactive({
    req(!is.null(input$retencao1_1))
    lapply(1:input$rep_n_tempos, function(i) {
      input[[paste0("retencao1_", i)]]
    }) %>% unlist()
  })

  rep_retencao_B <- reactive({
    req(!is.null(input$retencao2_1))

    lapply(1:input$rep_n_tempos, function(i) {
      input[[paste0("retencao2_", i)]]
    }) %>% unlist()
  })





  rep_n_calc <- reactive({
    req(!is.null(input$rep_n_tempos))
    req(rep_problemas_matriz_correlacao1() == "")

    n <- longpower::power.mmrm(
      Ra = matrix(data = correlation_Ra(), nrow = input$rep_n_tempos, byrow = TRUE),
      ra = rep_retencao_A()/100,
      sigmaa = input$rep_sigma1,
      Rb = matrix(data = correlation_Rb(), nrow = input$rep_n_tempos, byrow = TRUE),
      rb = rep_retencao_B()/100,
      sigmab = input$rep_sigma2,
      delta = input$rep_dif_medias,
      lambda = input$rep_ratio,
      sig.level = input$rep_sig/100,
      power = input$rep_power/100
    )

    validate(need(!is.na(n$n1), 'Não foi possível calcular sua solicitação. Verifique as entradas no painel lateral. Se o erro persistir, por favor, envie um e-mail para l-bioestatistica@hcpa.edu.br.'))
    n
  })


  rep_erro_inputs <- reactive({
    req(!is.null(input$rep_n_tempos))

    input$rep_n_tempos != sqrt(length(correlation_Ra())) |
      input$rep_n_tempos != sqrt(length(correlation_Rb())) |
      !all(diff(rep_retencao_A()) <= 0) | !all(rep_retencao_A() > 0) | !all(rep_retencao_A() <= 100) |
      !all(diff(rep_retencao_B()) <= 0) | !all(rep_retencao_B() > 0) | !all(rep_retencao_B() <= 100) |
      input$rep_sigma1 <= 0 |
      input$rep_sigma2 <= 0 |
      input$rep_ratio <= 0 |
      input$rep_sig <= 0 | input$rep_sig >= 100 |
      input$rep_power <= 0 | input$rep_power >= 100 |
      input$rep_ar1_rho2 <= 0 | input$rep_ar1_rho2 >= 1 |
      rep_problemas_matriz_correlacao1() != "" |
      input$rep_n_tempos%%1 != 0 | input$rep_n_tempos < 1

  })


  eval(parse(text = warning_inteiro("rep_n_tempos")))


  # Render output ----


  output$rep_outout_text <- renderText({

    validate(need(!rep_erro_inputs(), "Não foi possível calcular sua solicitação. Verifique as entradas no painel lateral. Se o erro persistir, por favor, envie um e-mail para l-bioestatistica@hcpa.edu.br."))

    n  <- rep_n_calc()
    n1 <- ceiling(n$n1)
    n2 <- ceiling(n$n2)
    n  <- n1 + n2

    nperdas1 <- n_perdas(n1, input$rep_perdas_recusa)
    nperdas2 <- n_perdas(n2, input$rep_perdas_recusa)

    retencaoA <- paste0(rep_retencao_A(), "%", collapse = ", ") %>%
      stringi::stri_replace_last_fixed(",", " e ") %>%
      stringr::str_trim() %>% stringr::str_squish()

    retencaoB <- paste0(rep_retencao_B(), "%", collapse = ", ") %>%
      stringi::stri_replace_last_fixed(",", " e ") %>%
      stringr::str_trim() %>% stringr::str_squish()

    momentos <- paste0(1:input$rep_n_tempos, collapse = ", ") %>%
      stringi::stri_replace_last_fixed(",", " e ") %>%
      stringr::str_trim() %>% stringr::str_squish()

    paste0(
      "<b><font size = '5'>Tamanho amostral calculado: ", n, " (<i>", n1, " no grupo ", rep_grupoTratamento(),
      " e ", n2, " no grupo ", rep_grupoControle(),"</i>)",
      "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

      "Tendo em vista um estudo longitudinal que avaliará o (a) <b>", rep_nome_desfecho(), "</b> entre os grupos ", rep_grupoTratamento(), " e ", rep_grupoControle(),
      " em <b>", input$rep_n_tempos, "</b> momentos, ",
      "foi calculado o tamanho de amostra para detectar diferenças na resposta média de <b>", rep_nome_desfecho(), "</b> entre esses grupos no tempo final (momento ", input$rep_n_tempos,
      "), tendo uma diferença de <b>", input$rep_dif_medias, " u.m.</b> como relevante para o estudo. ",

      "Para isso foi utilizado a ", .txt_citacao_tap, ". ",

      "Considerando os seguintes valores esperados para o grupo ", rep_grupoControle(), ":",

      "<ul>", # inicio da lista
      "<li> desvio padrão de <b>", input$rep_sigma2, " u.m.</b>,</li>",
      "<li> retenção de ", retencaoB, " nos momentos ", momentos,", respectivamente,</li>",

      if(input$rep_tipo_matriz_cor == 'Não estruturada'){
      paste0("<li> matriz de correlação não estruturada (definida abaixo),</li>")
      } else if(input$rep_tipo_matriz_cor == 'AR(1)'){
        paste0("<li> matriz de correlação autorregressiva de parâmetro ",
               input$rep_ar1_rho2, " (definida abaixo),</li>")
      } else if(input$rep_tipo_matriz_cor == 'Componente permutável'){
        paste0("<li> matriz de correlação permutável com correlação ",
               input$rep_cs_rho2, " (definida abaixo),</li>")
      },


      "</ul>",

      "e para o grupo ", rep_grupoTratamento(), " (dados de Fulano (1900)):",
      "<ul>", # inicio da lista
      "<li> desvio padrão de <b>", input$rep_sigma1, " u.m.</b>,</li>",
      "<li> retenção de ", retencaoA, " nos momentos ", momentos,", respectivamente,</li>",

      if(input$rep_tipo_matriz_cor == 'Não estruturada'){
      "<li> matriz de correlação não estruturada (definida abaixo),</li>"
      } else if(input$rep_tipo_matriz_cor == 'AR(1)'){
        paste0("<li> matriz de correlação autorregressiva de parâmetro ",
               input$rep_ar1_rho1, " (definida abaixo),</li>")
      } else if(input$rep_tipo_matriz_cor == 'Componente permutável'){
        paste0("<li> matriz de correlação permutável com correlação ",
               input$rep_cs_rho1, " (definida abaixo),</li>")
      },


      "</ul>",

      "poder de <b>", input$rep_power, "%</b> ",
      " e nível de significância de <b>", input$rep_sig, "%</b>, ",


      ifelse(input$rep_ratio == 1,
             paste0(
               "chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n1, "</b> em cada grupo.",
               " Acrescentando <b>", input$rep_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
               "deverá ser <b>", nperdas1 + nperdas2, "</b> (", nperdas1, " em cada grupo)."
             ),
             paste0(
               "chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n1, "</b> no ", rep_grupoTratamento(), " e <b>", n2, "</b> no ", th2mean_grupoControle(), ".",
               " Acrescentando <b>", input$rep_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
               "deverá ser <b>", nperdas1 + nperdas2, "</b> (", nperdas1, " no ", rep_grupoTratamento(), " e ", nperdas2, " no ", rep_grupoControle(), ")."
             )
      ),
      "<br>", "<br>"
    )

  })




  output$rep_corr_matrix_out <- renderUI({

    req(!rep_erro_inputs())

    fluidPage(
      HTML(paste0("Matriz de correlação do grupo ", rep_grupoTratamento(), ":<br>")),
      renderTable({
        df_ <- matrix(data = correlation_Ra(), nrow = input$rep_n_tempos, byrow = TRUE) %>%
          as.data.frame()

        rownames(df_) <- paste0("Momento ", 1:input$rep_n_tempos)
        colnames(df_) <- paste0("Momento ", 1:input$rep_n_tempos)
        df_
      },
      rownames = TRUE,
      colnames = TRUE
      ),

      HTML(paste0("Matriz de correlação do grupo ", rep_grupoControle(), ":<br>")),      renderTable({
        df2_ <- matrix(data = correlation_Rb(), nrow = input$rep_n_tempos, byrow = TRUE) %>%
          as.data.frame()

        rownames(df2_) <- paste0("Momento ", 1:input$rep_n_tempos)
        colnames(df2_) <- paste0("Momento ", 1:input$rep_n_tempos)
        df2_
      },
      rownames = TRUE,
      colnames = TRUE
      )

    )
  })


  output$rep_out_codigo <- renderText({

    req(!rep_erro_inputs())

    code <- paste0("longpower::power.mmrm(",
                   "Ra = Ra, ",
                   "ra = c(", paste0(rep_retencao_A(), collapse = ", "), ")/100, ",
                   "sigmaa = ", input$rep_sigma1,  ", ",
                   "Rb = Rb, ",
                   "rb = c(", paste0(rep_retencao_B(), collapse = ", "), ")/100, ",
                   "sigmab = ", input$rep_sigma2, ", ",
                   "delta = ", input$rep_dif_medias, ", ",
                   "lambda = ", input$rep_ratio, ", ",
                   "sig.level = ", input$rep_sig, "/100, ",
                   "power = ", input$rep_power, "/100)"
    )


    paste0(
      .txt_referencia_tap,

      "</br></br>",
      "<i>Comando R utilizado:</i><br>",
      "<p style=\"font-family:'Courier New';font-size:100% \">",
      # Ra
      code(paste0("Ra <- matrix(data = c(", paste0(correlation_Ra(), collapse = ", "),
                  "), nrow = ", input$rep_n_tempos, ", byrow = TRUE)")),
      "<br>",
      # Rb
      code(paste0("Rb <- matrix(data = c(", paste0(correlation_Rb(), collapse = ", "),
                  "), nrow = ", input$rep_n_tempos, ", byrow = TRUE)")),
      "<br>",
      code(code),
      "</p>"
    )

  })


  ## Cenarios ----

  output$cenarios_medidas_rep_thUi <- renderUI({
    req(!rep_erro_inputs())


    if(input$rep_dif_medias > 0){
      dif_start <- input$rep_dif_medias
      dif_end  <- input$rep_dif_medias + 2
      dif_by   <- 0.4
    } else{
      dif_start <- input$rep_dif_medias - 2
      dif_end  <- input$rep_dif_medias
      dif_by   <- 0.4
    }

    fluidPage(fluidRow(

      br(),
      HTML('<hr style="color: black;">'),
      br(),br(),

      titlePanel("Construção de cenários"),
      br(),

      wellPanel(paste0(
        "Utilize os argumentos abaixo para construir diferentes cenários. ",
        "Você pode definir um intervalo de diferença a ser detectada no último tempo de avaliação e especificar diferentes valores para o desvio padrão do grupo ", rep_grupoTratamento(), ". ",
        "Demais informações serão recuperadas do painel lateral.")),

      HTML("<b>Defina a sequência de valores para a diferença a ser detectada no último tempo de avaliação:</b>"),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
          numericInput("rep_th_from", "Mínimo", value = dif_start, step = 1)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("rep_th_to", "Máximo", value = dif_end, step = 1)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("rep_th_by", "Intervalo", value = dif_by, min = 0, step = 0.5) %>%
            .help_buttom(body = "Defina a sequência de margem de erro absoluta. Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                         title = "Sequência da margem de erro absoluta")
      ),

      fluidRow(
        column(6,
               textInput(inputId = "rep_sd_plot",
                         label   = paste0("Digite valores de desvio padrão (DP) do grupo ", rep_grupoTratamento(), " para fazer o gráfico:"),
                         value   = paste0(c(input$rep_sigma1, input$rep_sigma1 + 0.2, input$rep_sigma1 + 0.5), collapse = ", "),
                         width   = "100%") %>%
                 .help_buttom(body = "Defina os valores de desvio padrão.
                                                      Esses valores serão utilizados para criar diferentes linhas no gráfico.
                                                      Separe os valores por vírgula ',' e utilize ponto '.' como decimal.")
        )
      ),

      shinycssloaders::withSpinner(plotly::plotlyOutput("rep_th_plot", width = "80%"), type = 5),
      br(), br(),
      downloadButton("download_rep_th_tab","Download tabela"),
      shinycssloaders::withSpinner(DT::dataTableOutput("rep_th_tab", width = "100%"), type = 5)

    ))

  })


  eval(parse(text = check_text_input_to_vector("rep_sd_plot")))

  tab_rep_th_cenarios <- reactive({

    desvios_plot <- text_input_to_vector(input$rep_sd_plot)

    req(length(desvios_plot) > 0)

    grid <- expand.grid(`Diferença a ser detectada` = seq(from = input$rep_th_from, to = input$rep_th_to, by = input$rep_th_by),
                        `Desvio padrão 1` = desvios_plot,
                        `Desvio padrão 2` = input$rep_sigma2,
                        `Nível de significância (%)` = input$rep_sig,
                        `Poder (%)` = input$rep_power,
                        `Balanceamento` = input$rep_ratio)

    grid_n <- grid %$%
      purrr::pmap_dfr(
        .l = list(`Diferença a ser detectada`,
                  `Desvio padrão 1`,
                  `Desvio padrão 2`,
                  `Nível de significância (%)`,
                  `Poder (%)`,
                  `Balanceamento`),
        .f = function(delta, sigmaa, sigmab, sig.level, power, lambda){
          tryCatch({
            n <- longpower::power.mmrm(
              Ra = matrix(data = correlation_Ra(), nrow = input$rep_n_tempos, byrow = TRUE),
              ra = rep_retencao_A()/100,
              sigmaa = sigmaa,
              Rb = matrix(data = correlation_Rb(), nrow = input$rep_n_tempos, byrow = TRUE),
              rb = rep_retencao_B()/100,
              sigmab = sigmab,
              delta = delta,
              lambda = lambda,
              sig.level = sig.level/100,
              power = power/100
            )

            tibble(`n Tratamento` = ceiling(n$n1), `n Controle` = ceiling(n$n2))
          },
          warning = function(warning_condition) { NA },
          error = function(error_condition) { NA })
        })

    bind_cols(grid, grid_n) %>%
      mutate(`Tamanho da amostra` = `n Tratamento` + `n Controle`,
             `Matriz correlação Tratamento` = paste0("matrix(data = c(", paste0(correlation_Ra(), collapse = ", "),"), nrow = ", input$rep_n_tempos, ", byrow = TRUE)"),
             `Matriz correlação Controle` = paste0("matrix(data = c(", paste0(correlation_Rb(), collapse = ", "),"), nrow = ", input$rep_n_tempos, ", byrow = TRUE)")
             )

  })



  output$rep_th_plot <- plotly::renderPlotly({

    req(!is.null(tab_rep_th_cenarios()))

    g1 <- tab_rep_th_cenarios() %>%
      mutate(DP = factor(`Desvio padrão 1`)) %>%
      ggplot(aes(x = `Diferença a ser detectada`,
                 y = `Tamanho da amostra`,
                 color = DP,
                 `n Tratamento` = `n Tratamento`,
                 `n Controle` = `n Controle`))+
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = seq(from = input$rep_th_from, to = input$rep_th_to, by = input$rep_th_by)) +
      xlab("Diferença a ser detectada") +
      ylab("Tamanho total da amostra*") +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(palette = "Set1")

    plotly::ggplotly(g1, tooltip = c("x", "colour", "y", "n Tratamento", "n Controle")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                        showarrow = F, xref='paper', yref='paper',
                                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                        font=list(size=10)))
  })



  output$rep_th_tab <- DT::renderDataTable({

    req(!is.null(tab_rep_th_cenarios()))

    tab_rep_th_cenarios() %>%
      DT::datatable(#extensions = c('FixedColumns'),
                    rownames   = FALSE,
                    filter     = "none",
                    class = "display nowrap",
                    options    = list(pageLength = 10,
                                      scrollX = TRUE,
                                      scrollY = TRUE,
                                      searching = FALSE,
                                      # fixedColumns = list(leftColumns = 1),
                                      dom = 'B<"dwnld">frtip'))
  })


  output$download_rep_th_tab <- downloadHandler(
    filename = function() { "Cenarios_tamanho_amostra_medidas_repetidas.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_rep_th_cenarios(), path = file)}
  )









  # Poder ----

  observeEvent(input$show_th_rep_poder, {
    showModal(
      modalDialog(
        title = "Ajustes",
        fluidPage(

          HTML("<b>Preencha os campos abaixo de acordo com seu estudo para que sirvam de guia no preenchimento dos demais campos</b>."),
          br(), br(),
          textInput(inputId = "rep_poder_nome_desfecho",
                    label   = "Descreva o nome do desfecho",
                    value   = ifelse(input$show_th_rep_poder == 0, "Y", rep_poder_nome_desfecho())),
          HTML(paste0("<i>", str_remove_all(.txt_desfecho, "<br><br>"), "</i>")),
          br(), br(),
          textInput(inputId = "rep_poder_grupoTratamento",
                    label   = "Descreva um nome para o grupo Tratamento",
                    value   = ifelse(input$show_th_rep_poder == 0, "Tratamento", rep_poder_grupoTratamento())),

          HTML("<i>Em alguns estudos o grupo Tratamento também pode ser chamadado de grupo Intervenção ou grupo Exposto.</i><br><br>"),

          textInput(inputId = "rep_poder_grupoControle",
                    label   = "Descreva um nome para o grupo Controle",
                    value   = ifelse(input$show_th_rep_poder == 0, "Controle", rep_poder_grupoControle())),

          HTML("<i>Em alguns estudos o grupo Controle também pode ser chamadado de grupo Placebo/ Sham ou grupo Não exposto.</i>"),


        ),
        easyClose = TRUE,
        footer    = NULL
      )
    )
  })



  rep_poder_grupoControle <- reactive({
    ifelse(is.null(input$rep_poder_grupoControle), "Controle", input$rep_poder_grupoControle)
  })

  rep_poder_grupoTratamento <- reactive({
    ifelse(is.null(input$rep_poder_grupoTratamento), "Tratamento", input$rep_poder_grupoTratamento)
  })

  rep_poder_nome_desfecho <- reactive({
    ifelse(is.null(input$rep_poder_nome_desfecho), "Y", input$rep_poder_nome_desfecho)
  })



  output$rep_poder_formula1 <- renderUI({
    withMathJax(
      paste0("$$H_0: \\mu_{", rep_poder_grupoTratamento(), "} = \\mu_{", rep_poder_grupoControle(), "} $$"))
  })

  output$rep_poder_formula2 <- renderUI({
    withMathJax(
      paste0("$$H_1: \\mu_{", rep_poder_grupoTratamento(), "} \\neq \\mu_{", rep_poder_grupoControle(), "} $$"))
  })



  eval(parse(text = warning_numero_positivo("rep_poder_sigma1")))
  eval(parse(text = warning_numero_positivo("rep_poder_sigma2")))
  eval(parse(text = warning_numero_positivo("rep_poder_ratio")))

  eval(parse(text = warning_prop("rep_poder_ar1_rho1", entre0e1 = TRUE)))
  eval(parse(text = warning_prop("rep_poder_ar1_rho2", entre0e1 = TRUE)))
  eval(parse(text = warning_prop("rep_poder_sig")))
  eval(parse(text = warning_perdas("rep_poder_perdas_recusa")))

  eval(parse(text = warning_inteiro("rep_poder_n1")))
  eval(parse(text = warning_inteiro("rep_poder_n2")))


  output$medidas_repetidas_ui_sided_poder <- renderUI({

    fluidPage(
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            HTML("<b><font size = '2.8'>Hipóteses a serem testadas*</font></b>"),
            uiOutput("rep_poder_formula1"),
            uiOutput("rep_poder_formula2"),
            HTML("<i><font size = '2.8'>*Resposta média no último tempo</font></i>"),
          ),

          actionLink("show_th_rep_poder", "Mudar nomes"), br(), br(),

          numericInput( "rep_poder_n_tempos",
                        "Número de momentos a ser avaliado",
                        value = 4,
                        min = 2,
                        max = Inf,
                        step = 1
          ),

          # Tamanho amostral
          HTML(paste0("<b><font size = '2.95'>Tamanho amostral, no último momento, do grupo</font></b><br>")),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
              numericInput( "rep_poder_n1",
                            rep_poder_grupoTratamento(),
                            value = 95,
                            min = 0,
                            max = Inf,
                            step = 1)
          ),
          div(style="display: inline-block;vertical-align:top; width: 49%;",
              numericInput( "rep_poder_n2",
                            rep_poder_grupoControle(),
                            value = 80,
                            min = 0,
                            max = Inf,
                            step = 1)
          ),

          numericInput( "rep_poder_dif_medias",
                        paste0("Diferença do ", rep_poder_nome_desfecho(), " a ser detectada no último tempo de avaliação: (Média do ", rep_poder_grupoTratamento(),
                               ") - (Média do grupo ", rep_poder_grupoControle(), ")"),
                        value = 0.5,
                        min = -Inf,
                        max = Inf,
                        step = .5
          ) %>% .help_buttom(body = " É a menor diferença considerada clinicamente relevante (que tenha algum valor clínico) no último tempo de internação."),


          selectInput('rep_poder_tipo_matriz_cor',
                      "Selecione o tipo de matriz de correlação",
                      choices = c("AR(1)", "Não estruturada"),
                      selected = 'Não estruturada'
          ),

          conditionalPanel("input.rep_poder_tipo_matriz_cor == 'Não estruturada'",

                           textAreaInput(inputId = "rep_poder_Ra",
                                         label  = paste0("Matriz de correlação do grupo ", rep_poder_grupoTratamento()),
                                         value   = "
1.00, 0.25, 0.25, 0.25,
0.25, 1.00, 0.25, 0.25,
0.25, 0.25, 1.00, 0.25,
0.25, 0.25, 0.25, 1.00
  ",
                                         rows = 4

                           ) %>% .help_buttom(body = paste0("Entre com as matriz de correlação do grupo ", rep_poder_grupoTratamento())),

                           textAreaInput(inputId = "rep_poder_Rb",
                                         label  = paste0("Matriz de correlação do grupo ", rep_poder_grupoControle()),
                                         value   = "
1.00, 0.25, 0.25, 0.25,
0.25, 1.00, 0.25, 0.25,
0.25, 0.25, 1.00, 0.25,
0.25, 0.25, 0.25, 1.00
  ",
                                         rows = 4

                           ) %>% .help_buttom(body = paste0("Entre com as matriz de correlação do grupo ", rep_poder_grupoControle())),

          ),

          conditionalPanel("input.rep_poder_tipo_matriz_cor == 'AR(1)'",

                           # Parâmetro autorregressivo
                           HTML(paste0("<b><font size = '2.95'>Parâmetro autorregressivo no grupo</font></b><br>")),
                           div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                               numericInput( "rep_poder_ar1_rho1",
                                             rep_poder_grupoTratamento(),
                                             value = 0.4,
                                             min = 0,
                                             max = 1,
                                             step = .1
                               )
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 49%;",
                               numericInput( "rep_poder_ar1_rho2",
                                             rep_poder_grupoControle(),
                                             value = 0.2,
                                             min = 0,
                                             max = 1,
                                             step = .1
                               )
                           )
          ),


          # Desvio padrao
          HTML(paste0("<b><font size = '2.95'>Desvio padrão esperado de ", rep_poder_nome_desfecho(), " no grupo</font></b><br>")),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
              numericInput( "rep_poder_sigma1",
                            rep_poder_grupoTratamento(),
                            value = 1,
                            min = 0,
                            max = Inf,
                            step = 1
              )
          ),
          div(style="display: inline-block;vertical-align:top; width: 49%;",
              numericInput( "rep_poder_sigma2",
                            rep_poder_grupoControle(),
                            value = 1,
                            min = 0,
                            max = Inf,
                            step = 1
              ) %>% .help_buttom(body = .txt_dp, title = "Desvio padrão esperado")
          ),


          # Retencao
          HTML(paste0("<b><font size = '2.95'>Retenção esperada (%) no grupo</font></b><br>")),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
              textInput( "rep_poder_retencao1",
                         rep_poder_grupoTratamento(),
                         value = "100, 90, 80, 70")
          ),
          div(style="display: inline-block;vertical-align:top; width: 49%;",
              textInput( "rep_poder_retencao2",
                         rep_poder_grupoControle(),
                         value = "100, 90, 80, 70") %>%
                .help_buttom(body = "Retenção esperada dos grupos (% de sujeitos que ainda estão no estudo em dado ponto no tempo",
                            title = "Retenção esperada")
          ),

          # numericInput( "rep_poder_ratio",
          #               paste0("Balanceamento (", rep_poder_grupoTratamento(), ":", rep_poder_grupoControle(), ")"),
          #               value = 1,
          #               min = 0,
          #               max = Inf,
          #               step = 0.5
          # ) %>% .help_buttom(
          #   paste0("Nº de ", rep_poder_grupoControle(), " para cada ", rep_poder_grupoTratamento(), ". Se colocar o valor 2, será calculado um tamanho de amostra ",
          #          "tal que será necessário 2 ", rep_poder_grupoControle(), " para cada ", rep_poder_grupoTratamento(), " Se colocar o valor 0.5, ",
          #          "será calculado um tamanho de amostra ",
          #          "tal que será necessário 2 ", rep_poder_grupoTratamento(), " para cada ", rep_poder_grupoControle(), ".")
          # ),


          # numericInput( "rep_poder_power",
          #               "Poder (%)",
          #               value = 80,
          #               min = 0,
          #               max = 100,
          #               step = 1
          # ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
          numericInput( "rep_poder_sig",
                        "Nível de significância (%)",
                        value = 5,
                        min = 0,
                        max = 100,
                        step = 1
          ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)")
          # numericInput( "rep_poder_perdas_recusa",
          #               "Perdas/ Recusa (%)",
          #               value = 10,
          #               min = 0,
          #               max = 100,
          #               step = 1
          # ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
        ),

        mainPanel(
          shinycssloaders::withSpinner(htmlOutput("rep_poder_outout_text"), type = 5),
          shinycssloaders::withSpinner(uiOutput("rep_poder_corr_matrix_out"), type = 5),
          shinycssloaders::withSpinner(htmlOutput("rep_poder_out_codigo"), type = 5)

        )
      )
    )
  })



  correlation_rep_power_Ra <- reactive({

    if(input$rep_poder_tipo_matriz_cor == 'Não estruturada'){
      correlation_rep_power_Ra_v <- text_input_to_vector(input$rep_poder_Ra)

    } else if(input$rep_poder_tipo_matriz_cor == 'AR(1)'){
      tempos <- input$rep_poder_n_tempos
      exponent <- abs(matrix(1:tempos - 1, nrow = tempos, ncol = tempos, byrow = TRUE) - (1:tempos - 1))
      correlation_rep_power_Ra_v <- c(input$rep_poder_ar1_rho1^exponent)
    }
    correlation_rep_power_Ra_v
  })

  correlation_rep_power_Rb <- reactive({
    if(input$rep_poder_tipo_matriz_cor == 'Não estruturada'){
      correlation_rep_power_Rb_v <- text_input_to_vector(input$rep_poder_Rb)

    } else if(input$rep_poder_tipo_matriz_cor == 'AR(1)'){
      tempos <- input$rep_poder_n_tempos
      exponent <- abs(matrix(1:tempos - 1, nrow = tempos, ncol = tempos, byrow = TRUE) - (1:tempos - 1))
      correlation_rep_power_Rb_v <- c(input$rep_poder_ar1_rho2^exponent)
    }

    correlation_rep_power_Rb_v
  })





  rep_poder_retencao_A <- reactive({
    text_input_to_vector(input$rep_poder_retencao1)
  })

  rep_poder_retencao_B <- reactive({
    text_input_to_vector(input$rep_poder_retencao2)
  })


  observeEvent(c(input$rep_poder_retencao1, input$rep_poder_n_tempos), {
    if(length(rep_poder_retencao_A()) != input$rep_poder_n_tempos){
      shinyFeedback::showFeedbackWarning(
        inputId = "rep_poder_retencao1",
        text = paste0("Deve ser fornecido ", input$rep_poder_n_tempos, " valores."),
        color = "red")
    } else if(!all(diff(rep_poder_retencao_A()) <= 0) ){
      shinyFeedback::showFeedbackWarning(
        inputId = "rep_poder_retencao1",
        text = "A sequência deve ser descendente",
        color = "red")
    } else if(!all(rep_poder_retencao_A() > 0) | !all(rep_poder_retencao_A() <= 100)){
      shinyFeedback::showFeedbackWarning(
        inputId = "rep_poder_retencao1",
        text = "Todos valores devem estar entre 0% e 100%.",
        color = "red")
    } else {
      shinyFeedback::hideFeedback("rep_poder_retencao1")
    }
  })

  observeEvent(c(input$rep_poder_retencao2, input$rep_poder_n_tempos), {
    if(length(rep_poder_retencao_B()) != input$rep_poder_n_tempos){
      shinyFeedback::showFeedbackWarning(
        inputId = "rep_poder_retencao2",
        text = paste0("Deve ser fornecido ", input$rep_poder_n_tempos, " valores."),
        color = "red")
    } else if(!all(diff(rep_poder_retencao_B()) <= 0)){
      shinyFeedback::showFeedbackWarning(
        inputId = "rep_poder_retencao2",
        text = "A sequência deve ser descendente",
        color = "red")
    } else if(!all(rep_poder_retencao_B() > 0) | !all(rep_poder_retencao_B() <= 100)){
      shinyFeedback::showFeedbackWarning(
        inputId = "rep_poder_retencao2",
        text = "Todos valores devem estar entre 0% e 100%.",
        color = "red")
    } else {
      shinyFeedback::hideFeedback("rep_poder_retencao2")
    }
  })


  observeEvent(c(input$rep_poder_Ra, input$rep_poder_n_tempos), {
    if(sqrt(length(correlation_rep_power_Ra())) != input$rep_poder_n_tempos){
      shinyFeedback::showFeedbackWarning(
        inputId = "rep_poder_Ra",
        text = paste0("Deve ser fornecido uma matriz ", input$rep_poder_n_tempos, " x ", input$rep_poder_n_tempos, "."),
        color = "red")
    } else {
      shinyFeedback::hideFeedback("rep_poder_Ra")
    }
  })

  observeEvent(c(input$rep_poder_Rb, input$rep_poder_n_tempos), {
    if(sqrt(length(correlation_rep_power_Rb())) != input$rep_poder_n_tempos){
      shinyFeedback::showFeedbackWarning(
        inputId = "rep_poder_Rb",
        text = paste0("Deve ser fornecido uma matriz ", input$rep_poder_n_tempos, " x ", input$rep_poder_n_tempos, "."),
        color = "red")
    } else {
      shinyFeedback::hideFeedback("rep_poder_Rb")
    }
  })



  rep_poder_n_calc <- reactive({

    N <- input$rep_poder_n1 + input$rep_poder_n2
    lambda <- input$rep_poder_n1/ input$rep_poder_n2


    n <- longpower::power.mmrm(
      N = N,
      Ra = matrix(data = correlation_rep_power_Ra(), nrow = input$rep_poder_n_tempos, byrow = TRUE),
      ra = rep_poder_retencao_A()/100,
      sigmaa = input$rep_poder_sigma1,
      Rb = matrix(data = correlation_rep_power_Rb(), nrow = input$rep_poder_n_tempos, byrow = TRUE),
      rb = rep_poder_retencao_B()/100,
      sigmab = input$rep_poder_sigma2,
      delta = input$rep_poder_dif_medias,
      lambda = lambda,
      sig.level = input$rep_poder_sig/100,
    )

    validate(need(!is.na(n$power), 'Não foi possível calcular sua solicitação. Verifique as entradas no painel lateral. Se o erro persistir, por favor, envie um e-mail para l-bioestatistica@hcpa.edu.br.'))
    n
  })


  rep_poder_erro_inputs <- reactive({

    input$rep_poder_n_tempos != sqrt(length(correlation_rep_power_Ra())) |
      input$rep_poder_n_tempos != sqrt(length(correlation_rep_power_Rb())) |
      !all(diff(rep_poder_retencao_A()) <= 0) | !all(rep_poder_retencao_A() > 0) | !all(rep_poder_retencao_A() <= 100) |
      !all(diff(rep_poder_retencao_B()) <= 0) | !all(rep_poder_retencao_B() > 0) | !all(rep_poder_retencao_B() <= 100) |
      input$rep_poder_sigma1 <= 0 |
      input$rep_poder_sigma2 <= 0 |
      # input$rep_poder_ratio <= 0 |
      input$rep_poder_sig <= 0 | input$rep_poder_sig >= 100 |
      # input$rep_poder_power <= 0 | input$rep_poder_power >= 100 |
      input$rep_poder_ar1_rho2 <= 0 | input$rep_poder_ar1_rho2 >= 1

  })


  # Render output ----


  output$rep_poder_outout_text <- renderText({

    validate(need(!rep_poder_erro_inputs(), "Não foi possível calcular sua solicitação. Verifique as entradas no painel lateral. Se o erro persistir, por favor, envie um e-mail para l-bioestatistica@hcpa.edu.br."))

    poder  <- rep_poder_n_calc()
    poder <- round(poder$power*100, 1)

    retencaoA <- paste0(rep_poder_retencao_A(), "%", collapse = ", ") %>%
      stringi::stri_replace_last_fixed(",", " e ") %>%
      stringr::str_trim() %>% stringr::str_squish()

    retencaoB <- paste0(rep_poder_retencao_B(), "%", collapse = ", ") %>%
      stringi::stri_replace_last_fixed(",", " e ") %>%
      stringr::str_trim() %>% stringr::str_squish()

    momentos <- paste0(1:input$rep_poder_n_tempos, collapse = ", ") %>%
      stringi::stri_replace_last_fixed(",", " e ") %>%
      stringr::str_trim() %>% stringr::str_squish()

    paste0(
      "<b><font size = '5'>Poder calculado: ", poder, "%",
      "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

      "Tendo em vista um estudo longitudinal que avaliará o (a) <b>", rep_poder_nome_desfecho(), "</b> entre os grupos ", rep_poder_grupoTratamento(), " e ", rep_poder_grupoControle(),
      " em <b>", input$rep_poder_n_tempos, "</b> momentos, ",
      "foi calculado o poder do teste para detectar diferenças na resposta média de <b>", rep_poder_nome_desfecho(), "</b> entre esses grupos no tempo final (momento ", input$rep_poder_n_tempos,
      "), tendo uma diferença de <b>", input$rep_poder_dif_medias, " u.m.</b> como relevante para o estudo. ",

      "Para isso foi utilizado a ", .txt_citacao_tap, ". ",

      "Considerando os seguintes valores para o grupo ", rep_poder_grupoControle(), ":",

      "<ul>", # inicio da lista
      "<li> desvio padrão de <b>", input$rep_poder_sigma2, " u.m.</b>,</li>",
      "<li> retenção de ", retencaoB, " nos momentos ", momentos,", respectivamente,</li>",
      "<li> tamanho amostral de <b>", input$rep_poder_n2, "</b> no momento ", input$rep_poder_n_tempos, ",</li>",



      if(input$rep_poder_tipo_matriz_cor == 'Não estruturada'){
        "<li> matriz de correlação definida abaixo como Rb,</li>"
      } else if(input$rep_poder_tipo_matriz_cor == 'AR(1)'){
        paste0("<li> matriz de correlação autorregressiva de parâmetro ",
               input$rep_poder_ar1_rho2, " (matriz de correlação Rb definida abaixo),</li>")
      },

      "</ul>",

      "e para o grupo ", rep_poder_grupoTratamento(), " (dados de Fulano (1900)):",
      "<ul>", # inicio da lista
      "<li> desvio padrão de <b>", input$rep_poder_sigma1, " u.m.</b>,</li>",
      "<li> retenção de ", retencaoA, " nos momentos ", momentos,", respectivamente,</li>",
      "<li> tamanho amostral de <b>", input$rep_poder_n1, "</b> no momento ", input$rep_poder_n_tempos, ",</li>",


      if(input$rep_poder_tipo_matriz_cor == 'Não estruturada'){
        "<li> matriz de correlação definida abaixo,</li>"
      } else if(input$rep_poder_tipo_matriz_cor == 'AR(1)'){
        paste0("<li> matriz de correlação autorregressiva de parâmetro ",
               input$rep_poder_ar1_rho1, " (matriz de correlação definida abaixo),</li>")
      },


      "</ul>",

      " e nível de significância de <b>", input$rep_poder_sig, "%</b>, ",
      "chegou-se ao poder de <b>", poder , "%</b>.",
      "<br>", "<br>"
    )

  })


  output$rep_poder_corr_matrix_out <- renderUI({

    req(!rep_poder_erro_inputs())

    fluidPage(
      HTML("Matriz de correlação Ra:<br>"),
      renderTable({
        df_ <- matrix(data = correlation_rep_power_Ra(), nrow = input$rep_poder_n_tempos, byrow = TRUE) %>%
          as.data.frame()

        rownames(df_) <- paste0("T", 1:input$rep_poder_n_tempos)
        colnames(df_) <- paste0("T", 1:input$rep_poder_n_tempos)
        df_
      },
      rownames = TRUE,
      colnames = TRUE
      ),

      HTML("<br>Matriz de correlação Rb:<br>"),
      renderTable({
        df2_ <- matrix(data = correlation_rep_power_Rb(), nrow = input$rep_poder_n_tempos, byrow = TRUE) %>%
          as.data.frame()

        rownames(df2_) <- paste0("T", 1:input$rep_poder_n_tempos)
        colnames(df2_) <- paste0("T", 1:input$rep_poder_n_tempos)
        df2_
      },
      rownames = TRUE,
      colnames = TRUE
      )

    )
  })


  output$rep_poder_out_codigo <- renderText({

    req(!rep_poder_erro_inputs())

    code <- paste0("longpower::power.mmrm(",
                   "N = ", input$rep_poder_n1 + input$rep_poder_n2, ", ",
                   "Ra = Ra, ",
                   "ra = c(", paste0(rep_poder_retencao_A(), collapse = ", "), ")/100, ",
                   "sigmaa = ", input$rep_poder_sigma1,  ", ",
                   "Rb = Rb, ",
                   "rb = c(", paste0(rep_poder_retencao_B(), collapse = ", "), ")/100, ",
                   "sigmab = ", input$rep_poder_sigma2, ", ",
                   "delta = ", input$rep_poder_dif_medias, ", ",
                   "lambda = ", input$rep_poder_n1/ input$rep_poder_n2, ", ",
                   "sig.level = ", input$rep_poder_sig, "/100)"
    )


    paste0(
      .txt_referencia_tap,

      "</br></br>",
      "<i>Comando R utilizado:</i><br>",
      "<p style=\"font-family:'Courier New';font-size:100% \">",
      # Ra
      code(paste0("Ra <- matrix(data = c(", paste0(correlation_rep_power_Ra(), collapse = ", "),
                  "), nrow = ", input$rep_poder_n_tempos, ", byrow = TRUE)")),
      "<br>",
      # Rb
      code(paste0("Rb <- matrix(data = c(", paste0(correlation_rep_power_Rb(), collapse = ", "),
                  "), nrow = ", input$rep_poder_n_tempos, ", byrow = TRUE)")),
      "<br>",
      code(code),
      "</p>"
    )

  })




  #______ -----
  # ANOVA -----

  # One way ----


  # observeEvent(input$show_f_anova, {
  #   showModal(
  #     modalDialog(
  #       title = "Medida de efeito (f)",
  #       fluidPage(
  #         withMathJax(
  #           includeMarkdown(file.path("Markdown", "Effect_size_f.Rmd"))
  #         )
  #       ),
  #       easyClose = TRUE,
  #       footer    = NULL,
  #       size      = "l"
  #     )
  #   )
  # })


  output$anova_formula <- renderUI({


    if(input$anova_mean_f){
      k <- input$k_anova_n
    } else{
      k <- text_input_to_vector(input$medias_anova_n) %>%
        length()
    }

    withMathJax(
      paste0("$$H_0: ", paste0("\\mu_", LETTERS[1:k], collapse = " = "), "$$"))
  })


  observeEvent(input$k_anova_n, {
    shinyFeedback::hideFeedback("k_anova_n")

    if(is.na(input$k_anova_n)){
      shinyFeedback::showFeedbackWarning(
        inputId = "k_anova_n",
        text = "Deve ser um número.",
        color = "red"
      )
    } else if(input$k_anova_n%%1 != 0){
      shinyFeedback::showFeedbackWarning(
        inputId = "k_anova_n",
        text = "Deve ser um número inteiro maior ou igual a 2.",
        color = "red"
      )
    } else if (input$k_anova_n < 2) {
      shinyFeedback::showFeedbackWarning(
        inputId = "k_anova_n",
        text = "Deve ser maior ou igual a 2 .",
        color = "red"
      )
    }
  })


  observeEvent(input$medias_anova_n, {
    shinyFeedback::hideFeedback("medias_anova_n")

    medias_anova <- text_input_to_vector(input$medias_anova_n)

    if(all(is.na(medias_anova))){
      shinyFeedback::showFeedbackWarning(
        inputId = "medias_anova_n",
        text = "Deve ser fornecido valores da média.",
        color = "red"
      )
    } else if (length(medias_anova) < 2) {
      shinyFeedback::showFeedbackWarning(
        inputId = "medias_anova_n",
        text = "Deve ser menor fornecido mais valores válidos.",
        color = "red"
      )
    }
  })


  eval(parse(text = warning_numero_positivo("f_anova_n")))
  eval(parse(text = warning_numero_positivo("desvio_anova_n")))
  eval(parse(text = warning_prop("power_anova_n")))
  eval(parse(text = warning_prop("sig_anova_n")))
  eval(parse(text = warning_perdas("one_way_perdas_recusa")))



  output$anova_n <- renderText({

    validate(
      need(!is.na(input$power_anova_n), "É obrigatório fornecer um valor do poder."),
      need(!is.na(input$sig_anova_n),   "É obrigatório fornecer um valor do nível de significância."),

      need(input$power_anova_n > 0, "O poder deve ser maior do que zero."),
      need(input$sig_anova_n > 0,   "O nível de significância deve ser maior do que zero.")
    )

    if(input$anova_mean_f){

      validate(
        need(!is.na(input$k_anova_n), "É obrigatório fornecer um valor para o número de grupos."),
        need(!is.na(input$f_anova_n), "É obrigatório fornecer um valor da magnitude do efeito."),

        need(input$k_anova_n > 1, "O número de grupos deve ser maior do que 1 (um)."),
        need(input$f_anova_n > 0, "A magnitude do efeito deve ser maior do que zero."),
        need(input$k_anova_n%%1 == 0, "Número de grupos inválido.")
      )

      k <- input$k_anova_n

      code <- paste0(
        "pwr::pwr.anova.test(n = NULL, ",
        "k = ", k, ", ",
        "f = ", input$f_anova_n, ", ",
        "sig.level = ", input$sig_anova_n/100, ", ",
        "power = ", input$power_anova_n/100, ")")


      n <- try_n(code)
      eval(parse(text = validate_n("n")))

      n <- ceiling(n$n)
      n_perdas <- ceiling(n*(1+input$one_way_perdas_recusa/100))
      eval(parse(text = validate_n_inf("n")))


      paste0(
        "<b><font size = '5'>Tamanho amostral calculado: ", n*k,  " (<i>", n, " em cada grupo</i>)",
        "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

        "Foi calculado o tamanho da amostra para detectar diferenças, em termos de médias, no <b>Y</b> entre os grupos ",
        paste(LETTERS[1:k], collapse = ", ") %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish(),
        ", utilizando a ", .txt_citacao_tap, ". ",
        "Considerando poder de <b>", input$power_anova_n, "%</b>, nível de significância de <b>", input$sig_anova_n, "%</b> e ",
        "tamanho de efeito f de <b>", input$f_anova_n, "</b> obtido em Fulano (1900) <b>OU</b> escolha do pesquisador, ",
        "chegou-se ao tamanho de amostra total de <b>", n*k, "</b> sujeitos, sendo <b>", n, "</b> em cada grupo. ",
        "Acrescentando <b>", input$one_way_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas*k, "</b>",
        " (<b>", n_perdas, "</b> em cada grupo).",

        .txt_referencia_tap, print_r_code(code)
      )


    } else{
      medias_anova <- text_input_to_vector(input$medias_anova_n)

      validate(
        need(!is.na(medias_anova), "É obrigatório fornecer as médias dos grupos."),
        need(!is.na(input$desvio_anova_n), "É obrigatório fornecer um valor do desvio padrão."),

        need(length(medias_anova) > 1, "É obrigatório fornecer valores válidos para as médias dos grupos."),
        need(input$desvio_anova_n > 0, "O desvio padrão deve ser maior do que zero.")
      )



      k <- length(medias_anova)

      code <- paste0(
        "EnvStats::aovN(mu.vec = c(", paste(medias_anova, collapse = ", "), "), ",
        "sigma  = ", input$desvio_anova_n, ", ",
        "alpha  = ", input$sig_anova_n/100, ", ",
        "power  = ", input$power_anova_n/100, ", ",
        "n.max  = 1E5)"
      )

      n <- try_n(code)
      eval(parse(text = validate_n("n")))
      n_perdas <- n_perdas(n, input$one_way_perdas_recusa)
      eval(parse(text = validate_n_inf("n")))


      paste0(
        "<b><font size = '5'>Tamanho amostral calculado: ", n*k,  " (<i>", n, " em cada grupo</i>)",
        "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

        "Foi calculado o tamanho da amostra para detectar diferenças, em termos de médias, no <b>Y</b> entre os grupos ",
        paste(LETTERS[1:k], collapse = ", ") %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish(),
        ", utilizando a ", .txt_citacao_tap, ". ",
        "Considerando poder de <b>", input$power_anova_n, "%</b>, nível de significância de <b>", input$sig_anova_n, "%</b>, ",
        "desvio padrão esperado de <b>", input$desvio_anova_n, " u.m.</b> e ",
        "médias esperadas de <b>", input$medias_anova_n %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish(),
        " u.m.</b> para os grupos ", paste(LETTERS[1:k], collapse = ", ") %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish(),", respectivamente, obtidas em Fulano (1900), ",
        "chegou-se ao tamanho de amostra total de <b>", n*k, "</b> sujeitos, sendo <b>", n, "</b> em cada grupo. ",
        "Acrescentando <b>", input$one_way_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas*k, "</b>",
        " (<b>", n_perdas, "</b> em cada grupo).",

        .txt_referencia_tap, print_r_code(code)
      )
    }
  })



  ## Cenarios ----


  output$cenarios_anova_oneUi <- renderUI({
    fluidPage(fluidRow(

      conditionalPanel(condition = "input.anova_mean_f == true",
                       br(),
                       HTML('<hr style="color: black;">'),
                       br(),br(),

                       titlePanel("Construção de cenários"),
                       br(),

                       wellPanel("Utilize os argumentos abaixo para construir diferentes cenários.
                                        Você pode especificar valores do poder e uma sequência de valores de magnitude de efeito desejado.
                                        Demais informações serão recuperadas do painel lateral."),


                       HTML("<b>Defina a sequência de valores para a magnitude do efeito f:</b>"),
                       br(),
                       div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
                           numericInput("anovaOne_from", "Mínimo", value = input$f_anova_n, step = 0.5)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 80px;",
                           numericInput("anovaOne_to", "Máximo", value = input$f_anova_n + 0.8, step = 0.5)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 80px;",
                           numericInput("anovaOne_by", "Intervalo", value = 0.1, min = 0, step = 0.1) %>%
                             .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                                         title = "Sequência")
                       ),
                       br(),

                       fluidRow(
                         column(6,
                                textInput(inputId = "anovaOne_power_plot",
                                          label   = "Digite valores de poder para fazer o gráfico",
                                          value   = "80, 90, 95",
                                          width   = "400px") %>%
                                  .help_buttom(body = "Defina os valores de poder desejado.
                                                      Esses valores serão utilizados para criar diferentes linhas no gráfico.
                                                      Separe os valores por vírgula ',' e utilize ponto '.' como decimal.")
                         )
                       ),

                       br(),

                       plotly::plotlyOutput("anovaOne_plot", width = "80%"),
                       br(), br(),
                       downloadButton("download_anovaOne_tab","Download tabela"),
                       DT::dataTableOutput("anovaOne_tab", width = "100%")
      ),

      conditionalPanel(condition = "input.anova_mean_f == false",
                       br(),
                       HTML('<hr style="color: black;">'),
                       br(),br(),

                       titlePanel("Construção de cenários"),
                       br(),

                       wellPanel("Utilize os argumentos abaixo para construir diferentes cenários.
                                        Você pode especificar uma sequência de valores de desvio padrão esperado e valores de poder desejado.
                                        Demais informações serão recuperadas do painel lateral."),

                       HTML("<b>Defina a sequência de valores para o desvio padrão:</b>"),
                       br(),
                       div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
                           numericInput("anovaOne_sd_from", "Mínimo", value = input$desvio_anova_n, step = 0.5)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 80px;",
                           numericInput("anovaOne_sd_to", "Máximo", value = input$desvio_anova_n + 1, step = 0.5)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 80px;",
                           numericInput("anovaOne_sd_by", "Intervalo", value = 0.2, min = 0, step = 0.1) %>%
                             .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                                         title = "Sequência de valores para o desvio padrão esperado")
                       ),
                       br(),
                       fluidRow(
                         column(6,
                                textInput(inputId = "anovaOne_sd_power_plot",
                                          label   = "Digite valores de poder para fazer o gráfico",
                                          value   = "80, 90, 95",
                                          width   = "400px") %>%
                                  .help_buttom(body = "Defina os valores de poder desejado.
                                                      Esses valores serão utilizados para criar diferentes linhas no gráfico.
                                                      Separe os valores por vírgula ',' e utilize ponto '.' como decimal.")
                         )
                       ),
                       br(),

                       plotly::plotlyOutput("anovaOne_sd_plot", width = "80%"),
                       br(), br(),
                       downloadButton("download_anovaOne_sd_tab","Download tabela"),
                       DT::dataTableOutput("anovaOne_sd_tab", width = "100%")
      )

    ))
  })


  eval(parse(text = check_text_input_to_vector("anovaOne_power_plot")))

  tab_anovaOne_cenarios <- reactive({

    power <- text_input_to_vector(input$anovaOne_power_plot)
    req(length(power) > 0)


    expand.grid(`Magnitude do efeito` = seq(from = input$anovaOne_from, to = input$anovaOne_to, by = input$anovaOne_by),
                `Poder (%)` = power,
                `Nível de significância (%)` =  input$sig_anova_n,
                k = input$k_anova_n) %>%
      mutate(`Tamanho da amostra (por grupo)` =
               mapply(function(k, f, sig.level, power){
                 tryCatch({
                   pwr::pwr.anova.test(n = NULL, k = k, f = f, sig.level = sig.level/100, power = power/100)$n %>% ceiling()},
                   warning = function(warning_condition) { NA },
                   error = function(error_condition) { NA })},
                 k, `Magnitude do efeito`, `Nível de significância (%)`, `Poder (%)`),
             `n + perdas/ recusas` = n_perdas(`Tamanho da amostra (por grupo)`, input$one_way_perdas_recusa),
             `% de perdas/ recusas` = input$one_way_perdas_recusa)
  })



  output$anovaOne_plot <- plotly::renderPlotly({

    g1 <- tab_anovaOne_cenarios() %>%
      mutate(`Poder (%)` = factor(`Poder (%)`)) %>%
      ggplot(aes(x = `Magnitude do efeito`, y = `Tamanho da amostra (por grupo)`, color = `Poder (%)`))+
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = seq(from = input$anovaOne_from, to = input$anovaOne_to, by = input$anovaOne_by)) +
      xlab("Magnitude do efeito (f)") +
      ylab("Tamanho da amostra (por grupo)*") +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(palette = "Set1")

    plotly::ggplotly(g1, tooltip = c("x", "colour", "y")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                showarrow = F, xref='paper', yref='paper',
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=10)))
  })



  output$anovaOne_tab <- DT::renderDataTable({
    tab_anovaOne_cenarios() %>%
      DT::datatable(extensions = c('FixedColumns'),
                rownames   = FALSE,
                filter     = "none",
                ##callback   = DT::JS("$('div.dwnld').append($('#download_anovaOne_tab'));"),
                options    = list(pageLength = 10,
                                  scrollX = TRUE,
                                  scrollY = TRUE,
                                  searching = FALSE,
                                  fixedColumns = list(leftColumns = 1),
                                  dom = 'B<"dwnld">frtip'))
  })


  output$download_anovaOne_tab <- downloadHandler(
    filename = function() { "Cenarios_tamanho_amostra_anova_uma_via.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_anovaOne_cenarios(), path = file)}
  )



  # cenarios sd

  eval(parse(text = check_text_input_to_vector("anovaOne_sd_power_plot")))


  tab_anovaOne_sd_cenarios <- reactive({

    power <- text_input_to_vector(input$anovaOne_sd_power_plot)
    req(length(power) > 0)

    expand.grid(`Desvio padrão` = seq(from = input$anovaOne_sd_from, to = input$anovaOne_sd_to, by = input$anovaOne_sd_by),
                `Médias` = input$medias_anova_n,
                `Poder (%)` = power,
                `Nível de significância (%)` =  input$sig_anova_n) %>%

      mutate(`Tamanho da amostra (por grupo)` =
               mapply(function(medias, desvio, sig.level, power){

                 tryCatch({
                 medias_anova <- medias #%>% strsplit(",") %>% unlist() %>% as.numeric() %>% na.omit()

                 code <- paste0(
                   "EnvStats::aovN(mu.vec = c(", paste(medias_anova, collapse = ", "), "), ",
                   "sigma  = ", desvio, ", ",
                   "alpha  = ", sig.level/100, ", ",
                   "power  = ", power/100, ", ",
                   "n.max  = 1E5)")

                 n <- eval(parse(text = code))

                 n
                 }, warning = function(warning_condition) { NA },
                 error = function(error_condition) { NA })},
               `Médias`, `Desvio padrão`, `Nível de significância (%)`, `Poder (%)`),
             `n + perdas/ recusas` = n_perdas(`Tamanho da amostra (por grupo)`, input$one_way_perdas_recusa),
             `% de perdas/ recusas` = input$one_way_perdas_recusa)
  })



  output$anovaOne_sd_plot <- plotly::renderPlotly({

    g1 <- tab_anovaOne_sd_cenarios() %>%
      mutate(`Poder (%)` = factor(`Poder (%)`)) %>%
      ggplot(aes(x = `Desvio padrão`, y = `Tamanho da amostra (por grupo)`, color = `Poder (%)`))+
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = seq(from = input$anovaOne_sd_from, to = input$anovaOne_sd_to, by = input$anovaOne_sd_by)) +
      xlab("Desvio padrão esperado") +
      ylab("Tamanho da amostra (por grupo)*") +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(palette = "Set1")

    plotly::ggplotly(g1, tooltip = c("x", "colour", "y")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                        showarrow = F, xref='paper', yref='paper',
                                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                        font=list(size=10)))
  })



  output$anovaOne_sd_tab <- DT::renderDataTable({
    tab_anovaOne_sd_cenarios() %>%
      DT::datatable(extensions = c('FixedColumns'),
                    rownames   = FALSE,
                    filter     = "none",
                    options    = list(pageLength = 10,
                                      scrollX = TRUE,
                                      scrollY = TRUE,
                                      searching = FALSE,
                                      fixedColumns = list(leftColumns = 1),
                                      dom = 'B<"dwnld">frtip'))
  })


  output$download_anovaOne_sd_tab <- downloadHandler(
    filename = function() { "Cenarios_tamanho_amostra_anova_uma_via.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_anovaOne_sd_cenarios(), path = file)}
  )








  # Poder ----

  eval(parse(text = warning_numero_positivo("n_anova_power")))
  eval(parse(text = warning_numero_positivo("k_anova_power")))
  eval(parse(text = warning_prop("sig_anova_power2")))
  eval(parse(text = warning_numero_positivo("sigma_anova_power2")))

  observeEvent(input$f_anova_power, {
    if(is.na(input$f_anova_power)){
      shinyFeedback::showFeedbackWarning(
        inputId = "f_anova_power",
        text = "Deve ser fornecido um valor.",
        color = "red")
    } else {
      shinyFeedback::hideFeedback("f_anova_power")
    }
  })

  observeEvent(input$medias_anova_power, {
    if(is.na(input$medias_anova_power)){
      shinyFeedback::showFeedbackWarning(
        inputId = "medias_anova_power",
        text = "Deve ser fornecido um valor.",
        color = "red")
    } else {
      shinyFeedback::hideFeedback("medias_anova_power")
    }
  })

  observeEvent(input$n_anova_power2, {
    if(is.na(input$n_anova_power2)){
      shinyFeedback::showFeedbackWarning(
        inputId = "n_anova_power2",
        text = "Deve ser fornecido um valor.",
        color = "red")
    } else {
      shinyFeedback::hideFeedback("n_anova_power2")
    }
  })


  output$anova_power <- renderText({

    if(input$anova_mean_f_power){

      code <- paste0("pwr2::pwr.1way(",
        "n = ", input$n_anova_power, ", ",
        "k = ", input$k_anova_power, ", ",
        "f = ", input$f_anova_power, ", ",
        "alpha = ", input$sig_anova_power2, "/100)"
      )

      poder <- try_n(code)
      eval(parse(text = validate_n("poder")))
      poder <- poder$power*100

    } else{

      medias_anova_power <- input$medias_anova_power
        # strsplit(",") %>%
        # unlist() %>%
        # as.numeric()

      n_anova <- input$n_anova_power2
        # strsplit(",") %>%
        # unlist() %>%
        # as.numeric()

      code <- paste0("EnvStats::aovPower(",
        "n.vec = c(", n_anova, "), ",
        "mu.vec = c(", medias_anova_power, "), ",
        "alpha = ", input$sig_anova_power2, "/100, ",
        "sigma = ", input$sigma_anova_power2, ")"
      )

      poder <- try_n(code)
      eval(parse(text = validate_n("poder")))
      poder <- poder*100

    }

    paste0("<b><font size = '5'>Poder calculado: ", round(poder, digits = 1),
           "%</font></b></br></br><i></i></br></br>",

           .txt_referencia_tap, print_r_code(code))

  })





  # Two way -----


  observeEvent(input$show_th_anova2way, {
    showModal(
      modalDialog(
        title = "Ajustes",
        fluidPage(

          HTML("<b>Preencha os campos abaixo de acordo com seu estudo para que sirvam de guia no preenchimento dos demais campos</b>."),
          br(), br(),
          textInput(inputId = "two_nome_desfechoA",
                    label   = "Descreva o nome do fator A",
                    value   = ifelse(input$show_th_anova2way == 0, "Fator A", nome_fatorA())),
          textInput(inputId = "two_nome_desfechoB",
                    label   = "Descreva o nome do fator B",
                    value   = ifelse(input$show_th_anova2way == 0, "Fator B", nome_fatorB())),
        ),
        easyClose = TRUE,
        footer    = NULL
      )
    )
  })





  nome_fatorA <- reactive({
    ifelse(is.null(input$two_nome_desfechoA), "Fator A", input$two_nome_desfechoA)
  })

  nome_fatorB <- reactive({
    ifelse(is.null(input$two_nome_desfechoB), "Fator B", input$two_nome_desfechoB)
  })


  output$k_anova_n_ui <- renderUI({

    fluidPage(fluidRow(

      HTML("<b>Número de grupos para comparar do </b>"),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
          numericInput( "k_anova_n_A",
                        paste(nome_fatorA()),
                        value = 3,
                        min = 2,
                        max = Inf,
                        step = 1
          )
      ),
      div(style="display: inline-block;vertical-align:top; width: 49%;",
          numericInput( "k_anova_n_B",
                        paste(nome_fatorB()),
                        value = 2,
                        min = 2,
                        max = Inf,
                        step = 1
          )
      )
    ))
  })



  output$f_anova_n_ui <- renderUI({

    fluidPage(fluidRow(

      HTML("<b>Magnitude do efeito do</b>"),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
          numericInput( "f_anova_n_A",
                        paste(nome_fatorA()),
                        value = .4,
                        min = 0,
                        max = Inf,
                        step = .01
          )
      ),
      div(style="display: inline-block;vertical-align:top; width: 49%;",
          numericInput( "f_anova_n_B",
                        paste(nome_fatorB()),
                        value = .2,
                        min = 0,
                        max = Inf,
                        step = .01
          )
      )
    ))


  })


  output$delta_anova_n_ui <- renderUI({



    fluidPage(fluidRow(

      HTML("<b>A menor diferença entre os níveis de</b>"),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
          numericInput( "delta_anova_n_A",
                        paste(nome_fatorA()),
                        value = 1.2,
                        min = 0,
                        max = Inf,
                        step = .01
          )
      ),
      div(style="display: inline-block;vertical-align:top; width: 49%;",
          numericInput( "delta_anova_n_B",
                        paste(nome_fatorB()),
                        value = 2.1,
                        min = 0,
                        max = Inf,
                        step = .01
          )
      )
    ))

  })



  output$sigma_anova_n_ui <- renderUI({

    fluidPage(fluidRow(

      HTML("<b>Desvio padrão esperado do</b>"),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
          numericInput( "sigma_anova_n_A",
                        paste(nome_fatorA()),
                        value = 1.2,
                        min = 0,
                        max = Inf,
                        step = .01
          )
      ),
      div(style="display: inline-block;vertical-align:top; width: 49%;",
          numericInput( "sigma_anova_n_B",
                        paste(nome_fatorB()),
                        value = 1.2,
                        min = 0,
                        max = Inf,
                        step = .01
          )
      )
    ))

  })




  output$anova_n_two <- renderText({

    req(!(is.null(input$k_anova_n_A) | is.null(input$k_anova_n_B) | is.null(input$delta_anova_n_A) | is.null(input$delta_anova_n_B) |
            is.null(input$sigma_anova_n_A) | is.null(input$sigma_anova_n_B)))

    a <- input$k_anova_n_A
    b <- input$k_anova_n_B

    d_a <- input$delta_anova_n_A
    d_b <- input$delta_anova_n_B

    s_a <- input$sigma_anova_n_A
    s_b <- input$sigma_anova_n_B

    f_a <- input$f_anova_n_A
    f_b <- input$f_anova_n_B

    if(input$two_way_cohen){

      req(!is.null(f_a))
      req(!is.null(f_b))


      code <- paste0(
        "pwr2::ss.2way(a = ", a, ", ",
        "b = ", b, ", ",
        "alpha = ", input$sig_anova_n_two/100, ", ",
        "beta  = ", 1 - input$power_anova_n_two/100, ", ",
        "f.A   = ", f_a, ", ",
        "f.B   = ", f_b,  ", ",
        "delta.A = NULL, ",
        "delta.B = NULL, ",
        "sigma.A = NULL, ",
        "sigma.B = NULL, ",
        "B = 1000)"
      )

      n <- try_n(code)
      eval(parse(text = validate_n("n")))
      n <- n$n
      n_perdas <- n_perdas(n, input$two_way_perdas_recusa)
      eval(parse(text = validate_n_inf("n")))


      paste0("<b><font size = '5'>Tamanho amostral calculado: ", n*a*b,
             " (<b>", n, "</b> por grupo de combinação).",
             "</font></b></br></br><i>Sugestão de texto:</i></br></br>",


             "Foi calculado o tamanho de amostra, utilizado a ", .txt_citacao_tap, ", ",
             "para testar os efeitos principais de <b>", nome_fatorA(), "</b> (com <b>", a, "</b> níveis) ",
             " e de <b>", nome_fatorB(), "</b> (com <b>", b, "</b> níveis). ",
             "Considerando poder de <b>", input$power_anova_n_two, "%</b>, nível de significância de <b>", input$sig_anova_n_two, "%, </b>",
             "e fixando o tamanho de efeito f de <b>", f_a, "</b> e <b>", f_b, "</b> para o ", nome_fatorA(), " e ", nome_fatorB(), ", respectivamente (dados de Fulano (1900) <b>OU</b> escolha do pesquisador), ",
             "chegou-se ao tamanho de amostra total de <b>", n*a*b, "</b> sujeitos, sendo <b>", n, "</b> em cada combinação. ",
             "Acrescentando <b>", input$two_way_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas*a*b,
             "</b> (", n_perdas, " em cada grupo de combinação).",

             .txt_referencia_tap, print_r_code(code)
      )


    } else{

      code <- paste0(
        "pwr2::ss.2way(a = ", a, ", ",
                         "b = ", b, ", ",
                         "alpha = ", input$sig_anova_n_two/100, ", ",
                         "beta  = ", 1 - input$power_anova_n_two/100, ", ",
                         "f.A   = NULL, ",
                         "f.B   = NULL, ",
                         "delta.A = ", d_a, ", ",
                         "delta.B = ", d_b, ", ",
                         "sigma.A = ", s_a, ", ",
                         "sigma.B = ", s_b, ", ",
                         "B = 1000)")

      n <- try_n(code)
      eval(parse(text = validate_n("n")))
      n <- n$n
      n_perdas <- n_perdas(n, input$two_way_perdas_recusa)
      eval(parse(text = validate_n_inf("n")))


      paste0("<b><font size = '5'>Tamanho amostral calculado: ", n*a*b,
             " (<b>", n, "</b> por grupo de combinação).",
             "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

             "Foi calculado o tamanho de amostra, utilizado a ", .txt_citacao_tap, ", ",
             "para testar os efeitos principais de <b>", nome_fatorA(), "</b> (com <b>", a, "</b> níveis) ",
             " e de <b>", nome_fatorB(), "</b> (com <b>", b, "</b> níveis). ",
             "Considerando poder de <b>", input$power_anova_n_two, "%</b>, nível de significância de <b>", input$sig_anova_n_two, "%, </b>",
             "a menor diferença a ser detectada de <b>", d_a, " u.m.</b> e <b>", d_b,
             " u.m.</b> e desvio padrão de <b>", s_a,  " u.m.</b> e <b>", s_b, " u.m.</b> para o ", nome_fatorA(), " e ", nome_fatorB(), ", respectivamente (dados de Fulano (1900)), ",
             "chegou-se ao tamanho de amostra total de <b>", n*a*b, "</b> sujeitos, sendo <b>", n, "</b> em cada combinação. ",
             "Acrescentando <b>", input$two_way_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas*a*b,
             "</b> (", n_perdas, " em cada grupo de combinação).",

             .txt_referencia_tap, print_r_code(code)
      )
    }

  })





  #----------------.
  # Interacao ----
  #----------------.


    output$k_anova_n_A_ui2 <- renderUI({
    numericInput( "k_anova_n_A2",
                  paste("Número de grupos de", input$two_nome_desfechoA2),
                  value = 3,
                  min = 2,
                  max = Inf,
                  step = 1
    ) %>% .help_buttom(body = paste("Nº de grupos de", input$two_nome_desfechoA2, "para comparar"))
  })

  output$k_anova_n_B_ui2 <- renderUI({
    numericInput( "k_anova_n_B2",
                  paste("Número de grupos de", input$two_nome_desfechoB2),
                  value = 2,
                  min = 2,
                  max = Inf,
                  step = 1
    ) %>% .help_buttom(body = paste("Nº de grupos de", input$two_nome_desfechoB2, "para comparar"))
  })

  output$f_anova_n_A_ui2 <- renderUI({
    numericInput( "f_anova_interacao_2",
                  paste("Magnitude do efeito da interação ", input$two_nome_desfechoA2, " * ", input$two_nome_desfechoB2),
                  value = .5,
                  min = 0,
                  max = Inf,
                  step = .5
    ) %>% .help_buttom(body = paste("Magnitude do efeito f da interação (0.1 é considerado pequeno)"))
  })




  # Confere inputs -----.

  observeEvent(input$k_anova_n_A2, {
    shinyFeedback::hideFeedback("k_anova_n_A2")

    if(is.na(input$k_anova_n_A2)){
      shinyFeedback::showFeedbackWarning(
        inputId = "k_anova_n_A2",
        text = "Deve ser um número.",
        color = "red"
      )
    } else if(input$k_anova_n_A2%%1 != 0){
      shinyFeedback::showFeedbackWarning(
        inputId = "k_anova_n_A2",
        text = "Deve ser um número inteiro maior ou igual a 2.",
        color = "red"
      )
    } else if (input$k_anova_n_A2 < 2) {
      shinyFeedback::showFeedbackWarning(
        inputId = "k_anova_n_A2",
        text = "Deve ser maior ou igual a 2 .",
        color = "red"
      )
    }
  })


  observeEvent(input$k_anova_n_B2, {
    shinyFeedback::hideFeedback("k_anova_n_B2")

    if(is.na(input$k_anova_n_B2)){
      shinyFeedback::showFeedbackWarning(
        inputId = "k_anova_n_B2",
        text = "Deve ser um número.",
        color = "red"
      )
    } else if(input$k_anova_n_B2%%1 != 0){
      shinyFeedback::showFeedbackWarning(
        inputId = "k_anova_n_B2",
        text = "Deve ser um número inteiro maior ou igual a 2.",
        color = "red"
      )
    } else if (input$k_anova_n_B2 < 2) {
      shinyFeedback::showFeedbackWarning(
        inputId = "k_anova_n_B2",
        text = "Deve ser maior ou igual a 2 .",
        color = "red"
      )
    }
  })


  eval(parse(text = warning_numero_positivo("f_anova_interacao_2")))
  eval(parse(text = warning_prop("power_anova_n_two2")))
  eval(parse(text = warning_prop("sig_anova_n_two2")))
  eval(parse(text = warning_perdas("two_way_perdas_recusa2")))



  eval(parse(text = warning_numero_positivo("desvio_anova_n22")))
  eval(parse(text = warning_prop("power_anova_n_two22")))
  eval(parse(text = warning_prop("sig_anova_n_two22")))
  eval(parse(text = warning_perdas("two_way_perdas_recusa22")))


  output$anova_n_two2 <- renderText({

    if(FALSE){


    } else{

      req(!(is.null(input$k_anova_n_A2) | is.null(input$k_anova_n_B2 | is.null(input$f_anova_interacao_2))))



      a = input$k_anova_n_A2
      b = input$k_anova_n_B2
      efeito_interacao = input$f_anova_interacao_2


      #    A funcao easypower::n.multiway nao permite extrair facilmente as informacoes
      # por isso foi necessario realizar um ajuste tecnico, popularmente conhecido como gambiarra,
      # para extrair a informacao do tamanho amostral

      code <- paste0("easypower::n.multiway(",
                     "iv1 = list(name = 'F1', levels = ", a, ", eta.sq = 0.1), ",
                     "iv2 = list(name = 'F2', levels = ", b, ", eta.sq = 0.1), ",
                     "interaction.eta2 = ", efeito_interacao, ", ",
                     "sig.level = ", input$sig_anova_n_two2/100, ", ",
                     "power = ", input$power_anova_n_two2/100, ")")

      out_n_temp <- capture.output(easypower::n.multiway(iv1 = list(name = "NNNNNN",
                                                                    levels = a, eta.sq = 0.1),
                                                         iv2 = list(name = "NNNNNNNNNNNN", levels = b, eta.sq = 0.1),
                                                         interaction.eta2 = efeito_interacao,
                                                         sig.level = input$sig_anova_n_two2/100,
                                                         power     = input$power_anova_n_two2/100))

      n <- tibble::tibble(texto = out_n_temp) %>%
        dplyr::filter(str_detect(texto, pattern = "NNNNNN\\*NNNNNNNNNNNN")) %>%
        dplyr::pull() %>%
        strsplit("  ") %>%
        unlist() %>%
        dplyr::last() %>%
        as.numeric()
      n_perdas <- n_perdas(n, input$two_way_perdas_recusa2)

      paste0("<b><font size = '5'>Tamanho amostral calculado: ", n*a*b,
             " (<b>", n, "</b> por grupo de combinação).",
             "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

             "Foi calculado o tamanho de amostra, utilizado a ", .txt_citacao_tap, ", ",
             "para testar o efeito de interação entre <b>", input$two_nome_desfechoA2, "</b> (com <b>", a, "</b> níveis) ",
             " e de <b>", input$two_nome_desfechoB2, "</b> (com <b>", b, "</b> níveis). ",

             "Considerando poder de <b>", input$power_anova_n_two2, "%</b>, nível de significância de <b>", input$sig_anova_n_two2, "% </b>",
             "e fixando o tamanho de efeito de <b>", efeito_interacao, "</b> para a interação, como referido por Fulano (1900). ",
             "Chegou-se ao tamanho de amostra total de <b>", n*a*b, "</b> sujeitos, sendo <b>", n, "</b> em cada combinação. ",
             "Acrescentando <b>", input$two_way_perdas_recusa2, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas*a*b,
             "</b> (", n_perdas, " em cada grupo de combinação).",

             .txt_referencia_tap, print_r_code(code)
      )
    }

  })





  sintax_sas <- reactive({

    paste0(
      "
/* Médias */<br>
data medias;<br>
input Fator_A$ Fator_B$ Medias;<br>
cards;
<br>",
      input$medias_sas %>%
        str_replace_all("\\$", "<br>"),
      "<br>
;<br>
<br>
/* Outras definições gerais */<br>
%let alpha  = ", input$sig_anova_n_two22/100, ";<br>
%let power  = ", input$power_anova_n_two22/100, ";<br>
%let desvio = ", input$desvio_anova_n22, ";<br>
%let perdas = ", input$two_way_perdas_recusa22/100, ";<br>
<br>
<br>
/* Calcula o n */<br>
/*ods exclude all;*/ <br>
proc glmpower data = medias;<br>
class Fator_A Fator_B ;<br>
model Medias = Fator_A Fator_B Fator_A*Fator_B;<br>
power<br>
stddev = &desvio<br>
alpha = &alpha<br>
ntotal = .<br>
power = &power;<br>
ods output output = Power;<br>
run;<br>
/*ods exclude none;*/<br>
<br>
<br>
/* Prepara o texto */<br>
data work.power3 ;<br>
set work.power;<br>
if Source = 'Fator_A*Fator_B';<br>
Perdas = &perdas*100;<br>
NominalPower = NominalPower *100;<br>
Alpha = Alpha*100;<br>
NPerdas = ceil(NTotal/(1-&perdas));<br>
length var3 $ 2000;<br>
var3 = 'Foi calculado o tamanho de amostra, utilizando o SAS Studio, para testar o efeito de interação entre Fator A e Fator B. ' ||
 ' Considerando poder de ' || NominalPower ||
 '%, nível de significância de' || Alpha ||
 '%, as médias apresentadas na Tabela Y (...acima...) e desvio padrão esperado de ' || StdDev ||
 ' como referido em Fulano (1900), chegou-se ao tamanho de amostra total de ' || NTotal ||
 '. Acrescentando ' || Perdas ||
 '% para possíveis perdas e recusas o tamanho de amostra deverá ser' || NPerdas || '.';<br>
put _all_;<br>
call symput('teste', var3);<br>
run;<br>
<br>
<br>
<br>
ods proclabel='Tamanho amostral para interação ANOVA de duas vias';<br>
proc print data = medias;<br>
title 'Tabela Y: Médias esperadas dos níveis dos Fatores A e B (dados de Fulano (1900)) utilizadas no cálculo amostral.';<br>
*ods text= 'Adicione esta tabela com estes valores das médias utilizadas no cálculo após o parágrafo.';<br>
footnote1 \"&teste\";<br>
run;<br>
ods text= '*Rotina desenvolvida pela ferramenta PSS.Health: https://hcpa-unidade-bioestatistica.shinyapps.io/PSS_Health/';<br>
<br><br><br>
      "
    )



  })



  # output$anova_two_sas <- renderText({
  #   sintax_sas()
  # })


  output$download_sintax_sas_anova_two_way <- downloadHandler(

    filename = function() {
      paste0("SintaxSasAnovaTwoWay", "_",Sys.time(),".txt")
    },

    content = function(file) {

      write(sintax_sas() %>% str_replace_all("<br>", ""), file)
    }
  )



  #______-----
  # Chisq ----
  #-----------.



  eval(parse(text = warning_prop("sig_chisq_n")))
  eval(parse(text = warning_prop("power_chisq_n")))
  eval(parse(text = warning_numero_positivo("w_chisq_n")))
  eval(parse(text = warning_inteiro("df_chisq_n")))
  eval(parse(text = warning_inteiro("chisq_ncol")))
  eval(parse(text = warning_inteiro("chisq_nrow")))
  eval(parse(text = warning_perdas("chisq_perdas_recusa")))






  output$chis_tabela_cont_valoresUi <- renderUI({
    req(input$chisq_input != 1)

    fluidPage(fluidRow(wellPanel(

      numericInput(inputId = "chisq_nrow",
                   label   = "Número de linhas da tabela de contingência",
                   value   = 2,
                   min     = 2,
                   step    = 1

      ),
      numericInput(inputId = "chisq_ncol",
                   label   = "Número de colunas da tabela de contingência",
                   value   = 2,
                   min     = 2,
                   step    = 1

      ),

      HTML("<i><b>ATENÇÃO!</b> Edite a tabela de contingência no painel principal ao lado. As tabelas são editáveis, basta clicar duas vezes sobre a célula --> </i><br><br>")

    )))
  })




  #  Tabela de contingência

  tabela_chisq <- reactiveValues(tab = data.frame(a = 1))

  observeEvent(c(input$chisq_input, input$chisq_nrow, input$chisq_ncol), {

    req(!is.na(input$chisq_nrow) & !is.na(input$chisq_ncol))
    req(input$chisq_ncol%%1 == 0 & input$chisq_ncol >= 2)
    req(input$chisq_nrow%%1 == 0 & input$chisq_nrow >= 2)

    # Cria uma tabela inicial
    tab_vetor <- round(runif(input$chisq_nrow*input$chisq_ncol, 10, 50), 0)

    if(input$chisq_input == 2){
      tab_vetor <- round(tab_vetor/sum(tab_vetor)*100, 1)
      tab_vetor[1] <- 100 - (sum(tab_vetor) - tab_vetor[1])
      tab_vetor <- round(tab_vetor, 1)
    }


    tab <- matrix(tab_vetor,
                  nrow = input$chisq_nrow,
                  ncol = input$chisq_ncol) %>% as.data.frame()

    colnames(tab) <- paste0("Cat Y", 1:input$chisq_ncol)
    rownames(tab) <- paste0("Cat X", 1:input$chisq_nrow)

    tabela_chisq$tab <- tab
  })



  output$chisq_tab_cont <- DT::renderDT({
    # req(input$chisq_input != 1)
    temp <- input$chisq_tab_cont_cell_edit

    DT::datatable(tabela_chisq$tab,
                  editable = "cell",
                  extensions = c('FixedColumns'),
                  rownames   = TRUE,
                  filter     = "none",
                  options    = list(autoWidth = FALSE,
                                    searching = FALSE,
                                    ordering  = FALSE,
                                    pageLength = 15,
                                    dom = 't',
                                    columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  })





  observeEvent(input$chisq_tab_cont_cell_edit, {
    cell <- input$chisq_tab_cont_cell_edit
    newdf <- tabela_chisq$tab
    new_value <- cell$value %>%
      as.character() %>%
      str_replace(",", ".") %>%
      gsub("[^0-9.-]", "", .) %>%
      as.numeric()

    newdf[cell$row, cell$col] <- new_value

    tabela_chisq$tab <- newdf
  })




  chisq_problema_tab_cont <- reactive({
    problemas <- ""

    if(any(is.na(tabela_chisq$tab))){
      problemas <- paste0('<font color = "red"><b>Todas as células devem ser preenchidas.</b></font><br/>')
    } else if(input$chisq_input == 3 & any(tabela_chisq$tab%%1 != 0)){
      problemas <- paste0('<font color = "red"><b>Todos os valores devem ser inteiros não negativos.</b></font><br/>')
    } else if(input$chisq_input == 2 & sum(tabela_chisq$tab) != 100){
      soma <- round(sum(tabela_chisq$tab), 2)

      if(soma > 100){
        problemas <- paste0('<font color = "red"><b>A soma das proporções deve fechar 100%. Atualmente está somando ', soma, '%, precisa remover ', round(soma - 100, 2), '%.</b></font><br/>')
      } else{
        problemas <- paste0('<font color = "red"><b>A soma das proporções deve fechar 100%. Atualmente está somando ', soma, '%, precisa adicionar ', round(100 - soma, 2), '%.</b></font><br/>')
      }

    }
    problemas
  })




  output$chisq_tab_contUi <- renderUI({
    fluidPage(fluidRow(wellPanel(

      if(chisq_problema_tab_cont() != ""){
        HTML(paste0('<font size = "+0.1"><font color = "red">', "<b>Tabela de contingência editável (",
                    ifelse(input$chisq_input == 2, "% do total", "valores absolutos"),
                    "):</b>", '</font></font><br/>'))
      } else{
        HTML(paste0('<font size = "+0.1">', "<b>Tabela de contingência editável (",
                    ifelse(input$chisq_input == 2, "% do total", "valores absolutos"),
                    "):</b>", '</font><br/>'))
      },

      DT::dataTableOutput("chisq_tab_cont"),

      if(chisq_problema_tab_cont() != ""){
        HTML(chisq_problema_tab_cont())
      },
      br(), br()
    )))
  })




  chisq_w <- reactive({

    if(input$chisq_input == 1){
      list(w = input$w_chisq_n, gl = input$df_chisq_n)
    } else {

      validate(need(chisq_problema_tab_cont() == "", "Não foi possível calcular sua solicitação. Verifique as entradas no painel lateral. Se o erro persistir, por favor, envie um e-mail para l-bioestatistica@hcpa.edu.br."))

      matriz_prop <- as.matrix(tabela_chisq$tab)

      if(input$chisq_input == 2){
        w <- pwr::ES.w2(matriz_prop/100)
      } else{
        w <- pwr::ES.w2(prop.table(as.matrix(tabela_chisq$tab)))
      }
      list(w = round(w, 3),
           gl = (nrow(matriz_prop) - 1) * (ncol(matriz_prop) - 1),
           tabela = matriz_prop
           )



    }
  })




  ## Render output ----


  output$chisq_n <- renderText({

    code <- paste0("pwr::pwr.chisq.test(",
      "N = NULL, ",
      "w = ", chisq_w()$w, ", ",
      "df = ", chisq_w()$gl, ", ",
      "sig.level = ", input$sig_chisq_n, "/100, ",
      "power = ", input$power_chisq_n, "/100)"
    )

    n <- eval(parse(text = code))
    n <- ceiling(n$N)
    eval(parse(text = validate_n("n")))
    eval(parse(text = validate_n_inf("n")))



    paste0(
      "<b><font size = '5'>Tamanho amostral calculado: ", n,
      "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

      "Foi calculado o tamanho de amostra para associar ", input$chisq_desfecho, ", por meio da ", .txt_citacao_tap, ". ",
      "Considerando poder de <b>", input$power_chisq_n, "%</b>, nível de significância de <b>", input$sig_chisq_n, "%</b> e ",
      "tamanho de efeito w de <b>", chisq_w()$w, "</b> e <b>", chisq_w()$gl,
      ifelse(chisq_w()$gl == 1, "</b> grau ", "</b> graus "),
      "de liberdade conforme obtido em Fulano (1900) <b>OU</b> escolha do pesquisador, ",
      "chegou-se ao tamanho de amostra total de <b>", n, "</b> sujeitos. ",
      "Acrescentando <b>", input$chisq_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>",
      n_perdas(n, input$chisq_perdas_recusa), "</b>.",

      .txt_referencia_tap, print_r_code(code)
    )
  })



  ## Cenarios ----


  output$cenarios_chisq_Ui <- renderUI({
    fluidPage(fluidRow(


      br(),
      HTML('<hr style="color: black;">'),
      br(),br(),

      titlePanel("Construção de cenários"),
      br(),

      wellPanel("Utilize os argumentos abaixo para construir diferentes cenários.
                                        Você pode especificar valores do poder e uma sequência de valores de magnitude de efeito w desejado.
                                        Demais informações serão recuperadas do painel lateral."),


      HTML("<b>Defina a sequência de valores para a magnitude do efeito w:</b>"),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
          numericInput("chisq_from", "Mínimo", value = chisq_w()$w, step = 0.5)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("chisq_to", "Máximo", value = chisq_w()$w + 0.8, step = 0.5)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("chisq_by", "Intervalo", value = 0.1, min = 0, step = 0.1) %>%
            .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                         title = "Sequência")
      ),
      br(),

      fluidRow(
        column(6,
               textInput(inputId = "chisq_power_plot",
                         label   = "Digite valores de poder para fazer o gráfico",
                         value   = "80, 90, 95",
                         width   = "400px") %>%
                 .help_buttom(body = "Defina os valores de poder desejado.
                                                      Esses valores serão utilizados para criar diferentes linhas no gráfico.
                                                      Separe os valores por vírgula ',' e utilize ponto '.' como decimal.")
        )
      ),

      br(),

      plotly::plotlyOutput("chisq_plot", width = "80%"),
      br(), br(),
      downloadButton("download_chisq_tab","Download tabela"),
      DT::dataTableOutput("chisq_tab", width = "100%")

    ))
  })


  eval(parse(text = check_text_input_to_vector("chisq_power_plot")))

  tab_chisq_cenarios <- reactive({

    power <- text_input_to_vector(input$chisq_power_plot)
    req(length(power) > 0)


    expand.grid(`Magnitude do efeito w` = seq(from = input$chisq_from, to = input$chisq_to, by = input$chisq_by),
                `Poder (%)` = power,
                `Nível de significância (%)` =  input$sig_chisq_n,
                gl = chisq_w()$gl) %>%
      mutate(`Tamanho da amostra` =
               mapply(function(w, df, sig.level, power){
                 tryCatch({
                   n <- pwr::pwr.chisq.test(N = NULL, w = w, df = df, sig.level = sig.level/100, power = power/100)
                   ceiling(n$N)
                 },
                 warning = function(warning_condition) { NA },
                 error = function(error_condition) { NA })},
                 `Magnitude do efeito w`, gl, `Nível de significância (%)`, `Poder (%)`),
             `n + perdas/ recusas` = n_perdas(`Tamanho da amostra`, input$chisq_perdas_recusa),
             `% de perdas/ recusas` = input$chisq_perdas_recusa)
  })



  output$chisq_plot <- plotly::renderPlotly({

    g1 <- tab_chisq_cenarios() %>%
      mutate(`Poder (%)` = factor(`Poder (%)`)) %>%
      ggplot(aes(x = `Magnitude do efeito w`, y = `Tamanho da amostra`, color = `Poder (%)`))+
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = seq(from = input$chisq_from, to = input$chisq_to, by = input$chisq_by)) +
      xlab("Magnitude do efeito w") +
      ylab("Tamanho da amostra*") +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(palette = "Set1")

    plotly::ggplotly(g1, tooltip = c("x", "colour", "y")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                        showarrow = F, xref='paper', yref='paper',
                                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                        font=list(size=10)))
  })



  output$chisq_tab <- DT::renderDataTable({
    tab_chisq_cenarios() %>%
      DT::datatable(extensions = c('FixedColumns'),
                    rownames   = FALSE,
                    filter     = "none",
                    ##callback   = DT::JS("$('div.dwnld').append($('#download_anovaOne_tab'));"),
                    options    = list(pageLength = 10,
                                      scrollX = TRUE,
                                      scrollY = TRUE,
                                      searching = FALSE,
                                      fixedColumns = list(leftColumns = 1),
                                      dom = 'B<"dwnld">frtip'))
  })


  output$download_chisq_tab <- downloadHandler(
    filename = function() { "Cenarios_tamanho_amostra_associacao.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_chisq_cenarios(), path = file)}
  )




  # output$chisq_power <- renderText({
  #
  #   n <- pwr::pwr.chisq.test(
  #     N = input$n_chisq_power,
  #     w = input$w_chisq_power,
  #     df = input$df_chisq_power,
  #     sig.level = input$sig_chisq_power,
  #     power = NULL
  #   )
  #
  #   paste0("<b><font size = '5'>Poder calculado: ", round(100*n$power, digits = 3),
  #          "%</font></b></br></br><i>Sugestão de texto:</i></br></br>
  #           O poder calculado para o Teste de Associação é igual a ", round(100*n$power, digits = 3), "%,",
  #          " considerando a magnitude do efeito igual a ", input$w_chisq_n,
  #          ", o número de graus de liberdade igual a ", input$w_chisq_n,
  #          ", o nível de significância igual a ", input$sig_chisq_n,
  #          " e o tamanho amostral igual a ", input$n_chisq_n, ".")
  #
  # })








  #___________-----
  # Correlacao ----
  #----------------.

  # Estimar ----
  observeEvent(input$r_r_n_est, {
    shinyFeedback::hideFeedback("r_r_n_est")
    if(is.na(input$r_r_n_est)){
      shinyFeedback::showFeedbackWarning(
        inputId = "r_r_n_est",
        text = "Deve ser fornecido um valor.",
        color = "red"
      )
    } else if (input$r_r_n_est >= 1) {
      shinyFeedback::showFeedbackWarning(
        inputId = "r_r_n_est",
        text = "Deve ser menor do que 1.",
        color = "red"
      )
    } else if (input$r_r_n_est <= -1) {
      shinyFeedback::showFeedbackWarning(
        inputId = "r_r_n_est",
        text = "Deve ser maior do que -1.",
        color = "red"
      )
    }
  })

  eval(parse(text = warning_numero_positivo("precisao_rho")))
  eval(parse(text = warning_prop("conf_r_n_est")))
  eval(parse(text = warning_perdas("corr_TH_perdas_recusa_est")))



  output$r_nIest <- renderText({

    code <- paste0("presize::prec_cor(",
      "r = abs(", input$r_r_n_est, "), ",
      "conf.width = ", input$precisao_rho, ", ",
      "conf.level = ", input$conf_r_n_est, "/100, ",
      "method = '", input$r_r_coeficiente, "')"
    )

    n <- eval(parse(text = code))
    n <- n$n

    poder <- pwr::pwr.r.test(n = n,
                             r = input$r_r_n_est,
                             sig.level = 1 - input$conf_r_n_est/100,
                             power = NULL,
                             alternative = "two.sided")$power

    eval(parse(text = validate_n("poder")))
    eval(parse(text = validate_n_inf("poder")))



    paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
           "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

           "Para o cálculo do tamanho de amostra para estimar o coeficiente de correlação ",

            case_when(input$r_r_coeficiente == "pearson" ~ "linear de Pearson ",
                     input$r_r_coeficiente == "spearman" ~ "de postos de Spearman ",
                     TRUE ~ "de Kendal "),

           "entre <b>", input$corr_est_desfecho, "</b> foi utilizada a ", .txt_citacao_tap, ". ",
           "Considerando nível de confiança de <b>", input$conf_r_n_est, "%</b>, amplitude para o intervalo de confiança de <b>", input$precisao_rho,
           "</b> e correlação esperada de <b>", input$r_r_n_est, "</b> conforme referência de Fulano (1900) <b>OU</b> escolha do pesquisador,",
           " chegou-se ao tamanho de amostra total de <b>", n, "</b> sujeitos. Acrescentando <b>", input$corr_TH_perdas_recusa_est,
           "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas(n, input$corr_TH_perdas_recusa_est), "</b>.",

           .txt_referencia_tap, print_r_code(code),

           if(input$r_r_coeficiente == "pearson"){
             paste0(
               "</br></br></br><i>Obs.:</i> Com esse tamanho de amostra o poder do teste, para testar que a correlação é diferente de zero, será aproximadamente <b>",
               round(poder*100, 1), "</b>%.")
           }
    )

  })


  ## Cenarios ----

  output$cenarios_est_correlacaoUi <- renderUI({
    req(!is.na(input$precisao_rho))

    fluidPage(fluidRow(

      br(),
      HTML('<hr style="color: black;">'),
      br(),br(),

      titlePanel("Construção de cenários"),
      br(),br(),
      wellPanel("Utilize os argumentos abaixo para construir diferentes cenários.
                                        Você pode definir valores da amplitude do intervalo de confiança e definir o intervalo de valores esperados da correlação.
                                        Demais informações serão recuperadas do painel lateral."),

      fluidRow(
        column(6,
               textInput(inputId = "corr_precisoes_plot",
                         label   = "Digite valores da amplitude do intervalo de confiança para fazer o gráfico:",
                         value   = paste0(c(input$precisao_rho, input$precisao_rho + .1, input$precisao_rho + .2), collapse = ", "),
                         width   = "500px") %>%
                 .help_buttom(body = "Esses valores serão utilizados para compor diferentes linhas do gráfico.
                                        Separe os valores por vírgula ',' e utilize ponto '.' como decimal.")
        )
      ),

      fluidRow(
        column(6,
               # tags$head(tags$style(HTML(as.character(
               #   ".js-irs-1 .irs-single, .js-irs-1 .irs-from, .js-irs-1 .irs-to, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar{background: #006338 ;  }"
               # )))),
               sliderInput("range_cor_cenarios",
                           "Intervalo de correlação:",
                           min = 0,
                           max = 1,
                           value = c(0.1, 0.9),
                           step  = 0.05,
                           width = "500px") %>%
                 .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico")
        )
      ),
      plotly::plotlyOutput("cor_est_plot", width = "80%"),
      br(), br(),
      downloadButton("download_cor_est_tab", "Download tabela"),
      DT::dataTableOutput("cor_est_tab", width = "100%")
    ))
  })


  eval(parse(text = check_text_input_to_vector("corr_precisoes_plot")))

  tab_cor_cenarios <- reactive({

    amplitude <- text_input_to_vector(input$corr_precisoes_plot)
    req(length(amplitude) > 0)

    expand.grid(precisao     = amplitude,
                `Correlação` = seq(input$range_cor_cenarios[1], input$range_cor_cenarios[2], 0.05),
                `Nível de confiança (%)` = input$conf_r_n_est,
                Coeficiente = input$r_r_coeficiente,
                stringsAsFactors = FALSE) %>%

      mutate(n = mapply(n_est_corr,
                        `Nível de confiança (%)`,
                        `Correlação`,
                        precisao,
                        Coeficiente) %>% ceiling) %>%

      mutate(`n + perdas/ recusas` = n_perdas(n, input$corr_TH_perdas_recusa_est),
             `Amplitude do intervalo` = factor(precisao),
             `Tamanho da amostra`   = n,
             `% de perdas/ recusas` = input$corr_TH_perdas_recusa_est)
  })



  output$cor_est_plot <- plotly::renderPlotly({

    g2 <- ggplot(tab_cor_cenarios(), aes(x = `Correlação`, y = n, color = `Amplitude do intervalo`))+
      geom_point() +
      geom_line() +
      xlab("Correlação esperada") +
      ylab("Tamanho da amostra*") +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(palette = "Set1")

    plotly::ggplotly(g2,
                     tooltip = c("x", "colour", "y")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                showarrow = F, xref='paper', yref='paper',
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=10)))
  })


  tab_cor_cenarios_print <- reactive({
    req(!is.null(tab_cor_cenarios()))

    tab_cor_cenarios() %>%
      dplyr::select(Coeficiente,
                    `Nível de confiança (%)`,
                    `Correlação`,
                    `Amplitude do intervalo`,
                    `Tamanho da amostra`,
                    `% de perdas/ recusas`,
                    `n + perdas/ recusas`) %>%
      dplyr::rename("Nome do coeficiente" = "Coeficiente")
  })


  output$cor_est_tab <- DT::renderDataTable({
    tab_cor_cenarios_print() %>%
      DT::datatable(extensions = c('FixedColumns'),
                rownames   = FALSE,
                filter     = "none",
                #callback   = DT::JS("$('div.dwnld').append($('#download_cor_est_tab'));"),
                options    = list(pageLength = 10,
                                  scrollX = TRUE,
                                  scrollY = TRUE,
                                  searching = FALSE,
                                  fixedColumns = list(leftColumns = 1),
                                  dom = 'B<"dwnld">frtip'))
  })


  output$download_cor_est_tab <- downloadHandler(
    filename = function() { "Cenarios_estimacao_correlacao.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_cor_cenarios_print(), path = file)}
  )





  # Testar ----


  output$alternative_r_nUi <- renderUI({
    selectInput('alternative_r_n',
                'Tipo de teste de acordo com hipótese alternativa:',
                choices = c(paste0('A correlação é DIFERENTE de ', input$r_r_h0_n),
                            paste0('A correlação é MAIOR do que ', input$r_r_h0_n),
                            paste0('A correlação é MENOR do que ', input$r_r_h0_n)),
                selected = 'two.sided'
    ) %>% .help_buttom(body = "O teste pode ser bilateral, superior ou inferior. Nos dois útilmos casos, a hipótese alternativa é de que o parâmetro é maior ou menor do que o valor de referência, respectivamente.")

  })


  alternative <- reactive({
    case_when(input$alternative_r_n == paste0('A correlação é DIFERENTE de ', input$r_r_h0_n) ~ 'two.sided',
              input$alternative_r_n == paste0('A correlação é MAIOR do que ', input$r_r_h0_n) ~ 'greater',
              input$alternative_r_n == paste0('A correlação é MENOR do que ', input$r_r_h0_n) ~  'less')
  })



  output$correlacao_th_formula <- renderUI({

    sinal_h0 <- case_when(alternative() == 'two.sided' ~ "=",
                          alternative() == 'greater'   ~ "\\leq",
                          alternative() == 'less'      ~ "\\geq")

    sinal_h1 <- case_when(alternative() == 'two.sided' ~ "\\neq",
                          alternative() == 'greater'   ~ ">",
                          alternative() == 'less'      ~ "<")

    withMathJax(
      paste0("$$H_0: \\rho ", sinal_h0, input$r_r_h0_n,
             " \\text{  vs  } H_1: \\rho ", sinal_h1, input$r_r_h0_n, "$$"))
  })



  observeEvent(input$r_r_n, {
    shinyFeedback::hideFeedback("r_r_n")
    if(is.na(input$r_r_n)){
      shinyFeedback::showFeedbackWarning(
        inputId = "r_r_n",
        text = "Deve ser fornecido um valor.",
        color = "red"
      )
    } else if (input$r_r_n >= 1) {
      shinyFeedback::showFeedbackWarning(
        inputId = "r_r_n",
        text = "Deve ser menor do que 1.",
        color = "red"
      )
    } else if (input$r_r_n <= -1) {
      shinyFeedback::showFeedbackWarning(
        inputId = "r_r_n",
        text = "Deve ser maior do que -1.",
        color = "red"
      )
    }
  })



  observeEvent(input$r_r_h0_n, {
    shinyFeedback::hideFeedback("r_r_h0_n")
    if(is.na(input$r_r_h0_n)){
      shinyFeedback::showFeedbackWarning(
        inputId = "r_r_h0_n",
        text = "Deve ser fornecido um valor.",
        color = "red"
      )
    } else if (input$r_r_h0_n >= 1) {
      shinyFeedback::showFeedbackWarning(
        inputId = "r_r_h0_n",
        text = "Deve ser menor do que 1.",
        color = "red"
      )
    } else if (input$r_r_h0_n <= -1) {
      shinyFeedback::showFeedbackWarning(
        inputId = "r_r_h0_n",
        text = "Deve ser maior do que -1.",
        color = "red"
      )
    }
  })

  eval(parse(text = warning_prop("power_r_n")))
  eval(parse(text = warning_prop("sig_r_n")))
  eval(parse(text = warning_perdas("corr_TH_perdas_recusas")))



  # https://cran.r-project.org/web/packages/WebPower/WebPower.pdf
  # Versao 0.5.2
  #   O WebPower eh um pacote muito pesado e eh usando somente uma unicad funcao dele,
  # sendo assim optou por copiar a funcao e fazer somente a citacao, removendo assim
  # uma dependencia do PSS.

  wp.correlation <- function (n = NULL, r = NULL, power = NULL, p = 0, rho0 = 0, alpha = 0.05, alternative = c("two.sided", "less", "greater")){

    alternative <- match.arg(alternative)
    tside <- switch(alternative, less = 1, two.sided = 2, greater = 3)
    if (tside == 2 && !is.null(r))
      r <- abs(r)
    if (tside == 3) {
      p.body <- quote({
        delta <- sqrt(n - 3 - p) * (log((1 + r)/(1 - r))/2 +
                                      r/(n - 1 - p)/2 * (1 + (5 + r^2)/(n - 1 - p)/4 +
                                                           (11 + 2 * r^2 + 3 * r^4)/(n - 1 - p)^2/8) -
                                      log((1 + rho0)/(1 - rho0))/2 - rho0/(n - 1 -
                                                                             p)/2)
        v <- (n - 3 - p)/(n - 1 - p) * (1 + (4 - r^2)/(n -
                                                         1 - p)/2 + (22 - 6 * r^2 - 3 * r^4)/(n - 1 -
                                                                                                p)^2/6)
        zalpha <- qnorm(1 - alpha)
        pnorm((delta - zalpha)/sqrt(v))
      })
    }
    if (tside == 1) {
      p.body <- quote({
        delta <- sqrt(n - 3 - p) * (log((1 + r)/(1 - r))/2 +
                                      r/(n - 1 - p)/2 * (1 + (5 + r^2)/(n - 1 - p)/4 +
                                                           (11 + 2 * r^2 + 3 * r^4)/(n - 1 - p)^2/8) -
                                      log((1 + rho0)/(1 - rho0))/2 - rho0/(n - 1 -
                                                                             p)/2)
        v <- (n - 3 - p)/(n - 1 - p) * (1 + (4 - r^2)/(n -
                                                         1 - p)/2 + (22 - 6 * r^2 - 3 * r^4)/(n - 1 -
                                                                                                p)^2/6)
        zalpha <- qnorm(1 - alpha)
        pnorm((-delta - zalpha)/sqrt(v))
      })
    }
    if (tside == 2) {
      p.body <- quote({
        delta <- sqrt(n - 3 - p) * (log((1 + r)/(1 - r))/2 +
                                      r/(n - 1 - p)/2 * (1 + (5 + r^2)/(n - 1 - p)/4 +
                                                           (11 + 2 * r^2 + 3 * r^4)/(n - 1 - p)^2/8) -
                                      log((1 + rho0)/(1 - rho0))/2 - rho0/(n - 1 -
                                                                             p)/2)
        v <- (n - 3 - p)/(n - 1 - p) * (1 + (4 - r^2)/(n -
                                                         1 - p)/2 + (22 - 6 * r^2 - 3 * r^4)/(n - 1 -
                                                                                                p)^2/6)
        zalpha <- qnorm(1 - alpha/2)
        pnorm((delta - zalpha)/sqrt(v)) + pnorm((-delta -
                                                   zalpha)/sqrt(v))
      })
    }
    if (is.null(power))
      power <- eval(p.body)
    else if (is.null(n))
      n <- uniroot(function(n) eval(p.body) - power, c(4 +
                                                         p + 1e-10, 1e+07))$root
    else if (is.null(r)) {
      if (tside == 2) {
        r <- uniroot(function(r) eval(p.body) - power, c(1e-10,
                                                         1 - 1e-10))$root
      }
      else {
        r <- uniroot(function(r) eval(p.body) - power, c(-1 +
                                                           1e-10, 1 - 1e-10))$root
      }
    }
    else if (is.null(alpha))
      alpha <- uniroot(function(alpha) eval(p.body) - power,
                       c(1e-10, 1 - 1e-10))$root
    else stop("internal error")

    list(n = n, r = r, alpha = alpha, power = power,
         alternative = alternative)
  }




  output$r_n <- renderText({

    req(!is.null(alternative()))
    req(!is.na(alternative()))


    code <- paste0(
      "wp.correlation(",
      "n = NULL, ",
      "r = ", input$r_r_n, ", ",
      "power = ", input$power_r_n/100, ", ",
      "p = ", input$r_n_parcial, ", ",
      "rho0 = ", input$r_r_h0_n, ", ",
      "alpha = ", input$sig_r_n/100,  ", ",
      "alternative = '", alternative(), "')"
    )

    n <- try_n(code)
    eval(parse(text = validate_n("n")))
    n <- ceiling(n$n)
    eval(parse(text = validate_n_inf("n")))

    # fisher_transf <- log((1+input$r_r_n)/ (1-input$r_r_n))/2
    # ic_rho_aprox  <- c(fisher_transf - qnorm(1-(input$sig_r_n/100)/2)*sqrt(1/(n-3)),
    #                    fisher_transf + qnorm(1-(input$sig_r_n/100)/2)*sqrt(1/(n-3)))
    #
    # ic_rho_aprox_rev <- (exp(2*ic_rho_aprox) - 1)/ (exp(2*ic_rho_aprox) + 1)

    ic_rho <- presize::prec_cor(r = input$r_r_n, n = n, conf.level = 1-input$sig_r_n/100, method = 'pearson')


    paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
           "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

           "Foi calculado o tamanho de amostra para testar se o coeficiente de correlação ",
           if(input$r_n_parcial > 0){
             "parcial "
           },

           "linear de Pearson entre <b>", input$corr_testar_desfecho, "</b>",

           if(input$r_n_parcial > 0){
             paste0(", quando controlado pela(s) variável(eis) ", (paste(LETTERS[1:input$r_n_parcial], collapse = ", ") %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish()), ",")
           },


           if(alternative() == "two.sided"){
             " é diferente de "
           } else if(alternative() == "less"){
             " é menor do que "
           } else{
             " é maior do que "
           },

           input$r_r_h0_n,
           "</b>",

           ", por meio da ", .txt_citacao_tap, ". ",
           "Considerando nível de significância de <b>", input$sig_r_n, "%</b>, poder de <b>", input$power_r_n,
           "%</b> e correlação esperada de <b>", input$r_r_n, "</b> conforme referência de Fulano (1900) <b>OU</b> escolha do pesquisador,",
           " chegou-se ao tamanho de amostra total de <b>", n, "</b> sujeitos. Acrescentando <b>", input$corr_TH_perdas_recusas,
           "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas(n, input$corr_TH_perdas_recusas), "</b>.",

           "<br><br><br><br>",
           "<i>Observação.: </i>Com esse tamanho de amostra o intervalo de confiança do coeficiente de correlação será aproximadamente  <b>[", round(ic_rho$lwr, 2), " ; ", round(ic_rho$upr, 2), "]</b>.",


           .txt_referencia_tap,

           print_r_code(
             paste0(
               "WebPower::wp.correlation(",
               "n = NULL, ",
               "r = ", input$r_r_n, ", ",
               "power = ", input$power_r_n/100, ", ",
               "p = ", input$r_n_parcial, ", ",
               "rho0 = ", input$r_r_h0_n, ", ",
               "alpha = ", input$sig_r_n/100,  ", ",
               "alternative = '", alternative(), "')"
             )
           )
    )
  })


  ## Cenarios ----

  output$cenarios_th_correlacaoUi <- renderUI({
    fluidPage(fluidRow(
      br(),
      HTML('<hr style="color: black;">'),
      br(),br(),

      titlePanel("Construção de cenários"),
      br(),br(),
      wellPanel("Utilize os argumentos abaixo para construir diferentes cenários.
                                        Você pode definir valores do poder (%) e definir o intervalo de valores esperados da correlação.
                                        Demais informações serão recuperadas do painel lateral."),

      fluidRow(
        column(6,
               textInput(inputId = "corr_power_th_plot",
                         label   = "Digite valores de poder (%) para fazer o gráfico",
                         value   = "80, 85, 90",
                         width   = "500px") %>%
                 .help_buttom(body = "Esses valores serão utilizados para compor diferentes linhas do gráfico.
                                        Separe os valores por vírgula ',' e utilize ponto '.' como decimal.")
        )
      ),

      fluidRow(
        column(6,
               # tags$head(tags$style(HTML(as.character(
               #   ".js-irs-1 .irs-single, .js-irs-1 .irs-from, .js-irs-1 .irs-to, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar{background: #006338 ;  }"
               # )))),
               sliderInput("range_cor_cenarios_th",
                           "Intervalo de correlação:",
                           min   = 0,
                           max   = 1,
                           value = c(0.1, 0.9),
                           step  = 0.05,
                           width = "500px") %>%
                 .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico")
        )
      ),
      plotly::plotlyOutput("cor_th_plot", width = "80%"),
      br(), br(),
      downloadButton("download_cor_th_tab", "Download tabela"),
      DT::dataTableOutput("cor_th_tab", width = "100%")


    ))
  })



  eval(parse(text = check_text_input_to_vector("corr_power_th_plot")))

  tab_cor_th_cenarios <- reactive({

    req(!is.null(alternative()))
    req(!is.na(alternative()))


    # req(!(is.null(input$input$range_cor_cenarios_th)))
    # req(!(is.na(input$input$range_cor_cenarios_th) | is.null(input$e_mean) | is.null(input$conf_mean)))

    poder <- text_input_to_vector(input$corr_power_th_plot)
    req(length(poder) > 0)


    expand.grid(`Poder (%)`                  = poder,
                `Correlação esperada`        = seq(input$range_cor_cenarios_th[1], input$range_cor_cenarios_th[2], 0.05),
                `Nível de significância (%)` = input$sig_r_n,
                `Hipótese alternativa`       = alternative(),
                `Correlação sob H0`          = input$r_r_h0_n,
                `Variáveis parciais`         = input$r_n_parcial,
                stringsAsFactors = FALSE) %>%

      mutate(`Tamanho da amostra` = purrr::pmap_dbl(
        .l = list(`Correlação esperada`,
                  `Correlação sob H0`,
                  `Nível de significância (%)`,
                  `Poder (%)`,
                  `Hipótese alternativa`,
                  `Variáveis parciais`),
        .f = function(r, rho0, sig.level, power, alternative, p){
          tryCatch({
            wp.correlation(n = NULL,
                           r = r,
                           power = power/100,
                           p = p,  # Number of variables to partial out.
                           rho0 = rho0,
                           alpha = sig.level/100,
                           alternative = alternative)$n %>% ceiling() },
            warning = function(warning_condition) { NA },
            error = function(error_condition) { NA })
        })) %>%

      mutate(`n + perdas/ recusas` = n_perdas(`Tamanho da amostra`, input$corr_TH_perdas_recusas),
             `Poder (%) ` = factor(`Poder (%)`),
             `% de perdas/ recusas` = input$corr_TH_perdas_recusas)
  })



  output$cor_th_plot <- plotly::renderPlotly({

    g21 <- ggplot(tab_cor_th_cenarios(), aes(x = `Correlação esperada`, y = `Tamanho da amostra`, color = `Poder (%) `))+
      geom_point() +
      geom_line() +
      xlab("Correlação esperada") +
      ylab("Tamanho da amostra*")

    plotly::ggplotly(g21,
                     tooltip = c("x", "colour", "y")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                showarrow = F, xref='paper', yref='paper',
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=10)))  })




  output$cor_th_tab <- DT::renderDataTable({
    tab_cor_th_cenarios() %>%
      dplyr::select(`Nível de significância (%)`,
                    `Correlação sob H0`,
                    `Hipótese alternativa`,
                    `Correlação esperada`,
                    `Poder (%)`,
                    `Variáveis parciais`,
                    `Tamanho da amostra`,
                    `% de perdas/ recusas`,
                    `n + perdas/ recusas`) %>%
      DT::datatable(extensions = c('FixedColumns'),
                    rownames   = FALSE,
                filter     = "none",
                #callback   = DT::JS("$('div.dwnld').append($('#download_cor_th_tab'));"),
                options    = list(pageLength = 10,
                                  scrollX = TRUE,
                                  scrollY = TRUE,
                                  searching = FALSE,
                                  fixedColumns = list(leftColumns = 1),
                                  dom = 'B<"dwnld">frtip'))
  })


  output$download_cor_th_tab <- downloadHandler(
    filename = function() { "Cenarios_teste_correlacao.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_cor_th_cenarios() %>%
                                                    dplyr::select(`Nível de significância (%)`,
                                                                  `Correlação sob H0`,
                                                           `Hipótese alternativa`,
                                                           `Correlação esperada`,
                                                           `Poder (%)`,
                                                           `Variáveis parciais`,
                                                           `Tamanho da amostra`,
                                                           `% de perdas/ recusas`,
                                                           `n + perdas/ recusas`),
                                                  path = file)}
  )



  # Poder ----


  observeEvent(input$r_r_power, {
    shinyFeedback::hideFeedback("r_r_power")
    if(is.na(input$r_r_power)){
      shinyFeedback::showFeedbackWarning(
        inputId = "r_r_power",
        text = "Deve ser fornecido um valor.",
        color = "red"
      )
    } else if (input$r_r_power >= 1) {
      shinyFeedback::showFeedbackWarning(
        inputId = "r_r_power",
        text = "Deve ser menor do que 1.",
        color = "red"
      )
    } else if (input$r_r_power <= -1) {
      shinyFeedback::showFeedbackWarning(
        inputId = "r_r_power",
        text = "Deve ser maior do que -1.",
        color = "red"
      )
    }
  })

  eval(parse(text = warning_prop("sig_r_power")))
  eval(parse(text = warning_inteiro("n_r_power")))



  output$r_power <- renderText({
    code <- paste0("pwr::pwr.r.test(",
                   "n = ", input$n_r_power, ", ",
                   "r = ", input$r_r_power, ", ",
                   "sig.level = ", input$sig_r_power, "/100, ",
                   "power = NULL, ",
                   "alternative = 'two.sided')")

    poder <- eval(parse(text = code))

    poder <- poder$power %>%
      round(3) %>%
      multiply_by(100)

    # tipo_teste <- if(input$alternative_r_power == "two.sided") "bilateral" else "unilateral"

    paste0("<b><font size = '5'>Poder calculado: ", poder,
           "%</font></b></br></br><i>Sugestão de texto:</i></br></br>

            Considerando um coeficiente de correlação linear de Pearson igual a <b>", input$r_r_power,
           "</b>, nível de significância igual a <b>", input$sig_r_power, "%</b>",
           ", tamanho amostral igual a <b>", input$n_r_power,
           "</b>, o poder calculado é igual a <b>", poder, "%</b>.",

           .txt_referencia_tap, print_r_code(code)
    )
  })



  #____________----
  # Inclinacao regressao ----

  observeEvent(input$inclinacao_nomes, {
    showModal(
      modalDialog(
        title = "Ajustes",
        fluidPage(

          HTML("<b>Preencha os campos abaixo de acordo com seu estudo para que sirvam de guia no preenchimento dos demais campos</b>."),
          br(), br(),
          textInput(inputId = "inclinacao_nomeY",
                    label   = "Descreva o nome do desfecho",
                    value   = ifelse(input$inclinacao_nomes == 0, "Y", inclinacao_nomeY())),
          HTML(paste0("<i>", str_remove_all(.txt_desfecho, "<br><br>"), "</i>")),
          br(), br(),
          textInput(inputId = "inclinacao_nomeX",
                    label   = "Descreva o nome da variável preditora",
                    value   = ifelse(input$inclinacao_nomes == 0, "X", inclinacao_nomeX()))

        ),
        easyClose = TRUE,
        footer    = NULL
      )
    )
  })

  inclinacao_nomeY <- reactive({
    ifelse(is.null(input$inclinacao_nomeY), "Y", input$inclinacao_nomeY)
  })

  inclinacao_nomeX <- reactive({
    ifelse(is.null(input$inclinacao_nomeX), "X", input$inclinacao_nomeX)
  })


  output$inclinacao_inclinacaoUi <- renderUI({
    req(!input$inclinacao_usar_r2)

    fluidPage(fluidRow(
      numericInput( "inclinacao_reg",
                    "Coeficiente de correlação esperado",
                    value = .8,
                    step = 1
      ) %>% .help_buttom(body = paste0("Coeficiente de correlação esperado (beta) representa o impacto em Y a cada aumento de uma unidade em X. ",
                                       .txt_definido_pesquisador_OU_literatura),
                         title = "Coeficiente de correlação esperado"),
      numericInput( "sd_dependente_Y",
                    "Desvio padrão da variável resposta",
                    value = 0.5,
                    min = 0,
                    max = Inf,
                    step = 1
      ) %>% .help_buttom(body = .txt_dp, title = "Desvio padrão esperado"),
      numericInput( "sd_preditor_X",
                    "Desvio padrão do preditor",
                    value = 0.2,
                    min = 0,
                    max = Inf,
                    step = 1
      ) %>% .help_buttom(body = .txt_dp, title = "Desvio padrão esperado")
    ))
  })


  output$inclinacao_reg_formula <- renderUI({
    withMathJax(paste0("$$H_0: \\beta_{", inclinacao_nomeX(), "} = 0", " \\text{  vs  } H_1: \\beta_", inclinacao_nomeX(), " \\neq 0$$"))
  })


  observeEvent(input$inclinacao_reg, {
    shinyFeedback::hideFeedback("inclinacao_reg")
    if(is.na(input$inclinacao_reg)){
      shinyFeedback::showFeedbackWarning(
        inputId = "inclinacao_reg",
        text = "Deve ser fornecido um valor.",
        color = "red"
      )
    }
  })

  eval(parse(text = warning_prop("power_inclinacao_reg")))
  eval(parse(text = warning_prop("conf_inclinacao_reg")))
  eval(parse(text = warning_numero_positivo("sd_preditor_X")))
  eval(parse(text = warning_numero_positivo("sd_dependente_Y")))
  eval(parse(text = warning_prop("inclinacao_reg_r2", entre0e1 = TRUE)))
  eval(parse(text = warning_perdas("inclinacao_reg_perdas_recusa_est")))


  output$out_inclinacao_reg <- renderText({


    if(!input$inclinacao_usar_r2){
      info_input <- paste0("coeficiente de regressão de ", inclinacao_nomeX(), " esperado de <b>", input$inclinacao_reg, "</b>, ",
                           " desvio padrão de ", inclinacao_nomeX(), " esperado de <b>", input$sd_preditor_X, "</b>",
                           " e desvio padrão de ", inclinacao_nomeY(), " esperado de <b>", input$sd_dependente_Y, "</b>")

      code <- paste0(
        "powerMediation::ss.SLR(",
        "power = ", input$power_inclinacao_reg, "/100, ",
        "lambda.a = ", input$inclinacao_reg, ", ",
        "sigma.x = ", input$sd_preditor_X, ", ",
        "sigma.y = ", input$sd_dependente_Y, ", ",
        "alpha = ", input$conf_inclinacao_reg, "/100, ",
        "verbose = FALSE)"
      )

    } else {
      info_input <- paste0("coeficiente de determinação esperado de <b>", input$inclinacao_reg_r2, "</b>")

      code <- paste0(
        "powerMediation::ss.SLR.rho(",
        "power = ", input$power_inclinacao_reg, "/100, ",
        "alpha =",  input$conf_inclinacao_reg, "/100, ",
        "rho2  =",  input$inclinacao_reg_r2, ", ",
        "verbose = FALSE)"
      )
    }


    n <- try_n(code)
    eval(parse(text = validate_n("n")))
    n <- ceiling(n$n)
    eval(parse(text = validate_n_inf("n")))



    paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
           "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

           "Foi calculado o tamanho de amostra para testar se o coeficiente de regressão de ", inclinacao_nomeX(),
           " ao regredir ", inclinacao_nomeY(),
           " é diferente de zero, ",

           " por meio da ", .txt_citacao_tap, ". ",
           "Considerando nível de significância de <b>", input$conf_inclinacao_reg, "%</b>, poder de <b>", input$power_inclinacao_reg, "%</b>, ",
           info_input, ", conforme referência de Fulano (1900),",
           " chegou-se ao tamanho de amostra total de <b>", n, "</b> sujeitos. Acrescentando <b>", input$inclinacao_reg_perdas_recusa_est,
           "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas(n, input$inclinacao_reg_perdas_recusa_est), "</b>.",

           .txt_referencia_tap, print_r_code(code)
    )
  })




  #____________----
  # Logistica ----


  # Testar ----
  output$rc_logistic_formula <- renderUI({
    withMathJax("$$H_0: RC = 1 \\text{  vs  } H_1: RC \\neq 1$$")
  })


  eval(parse(text = warning_numero_positivo("logistic_or_continuous")))
  eval(parse(text = warning_prop("logistic_rate_mean")))
  eval(parse(text = warning_prop("power_logistic")))
  eval(parse(text = warning_prop("sig_logistic")))
  eval(parse(text = warning_perdas("logistic_perdas_recusa_est")))



  eval(parse(text = warning_numero_positivo("logistic_cat_ratio")))
  eval(parse(text = warning_numero_positivo("logistic_cat_odds")))
  eval(parse(text = warning_numero_positivo("logistic_cat_k")))
  eval(parse(text = warning_prop("logistic_cat_controle")))
  eval(parse(text = warning_prop("logistic_cat_p2")))
  eval(parse(text = warning_prop("power_logistic")))
  eval(parse(text = warning_prop("sig_logistic")))
  eval(parse(text = warning_perdas("logistic_perdas_recusa_est")))



  output$out_logistic <- renderText({

    if(input$logistic_tipo_variavel == 1){
      code <- paste0(
        "powerMediation::SSizeLogisticCon(",
        "p1 = ", input$logistic_rate_mean/100, ", ",
        "OR = ", input$logistic_or_continuous, ", ",
        "alpha = ", input$sig_logistic/100, ", ",
        "power = ", input$power_logistic/100, ")"
      )

      n <- try_n(code)
      eval(parse(text = validate_n("n")))
      n <- ceiling(n)
      eval(parse(text = validate_n_inf("n")))


      paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
             "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

             "Foi calculado o tamanho de amostra para testar se a razão de chances entre <b>X</b> e <b>Y</b> é diferente de 1 (um), ",

             " por meio da ", .txt_citacao_tap, ". ",
             "Considerando nível de significância de <b>", input$sig_logistic, "%</b>, poder de <b>", input$power_logistic,
             "%</b> e uma razão de chances esperada de <b>", input$logistic_or_continuous, "</b>, conforme referência de Fulano (1900),",
             " chegou-se ao tamanho de amostra total de <b>", n, "</b> sujeitos. Acrescentando <b>", input$logistic_perdas_recusa_est,
             "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas(n, input$logistic_perdas_recusa_est), "</b>.",

             .txt_referencia_tap, print_r_code(code)
      )



      # Se for variavel categorica!
    } else{

      if(input$or_th_estatistica == "percent"){
        p2 <- input$logistic_cat_p2/100
        text_just <- paste0("proporção de <b>Y</b> no Expostos de <b>", input$logistic_cat_p2, "% </b>",
                            "e <b>", input$logistic_cat_controle, "%</b> no Não expostos, como é referida em Fulano (1900), ")

      } else if (input$or_th_estatistica == "ratio") {

        p2 <- (input$logistic_cat_controle/100)*input$logistic_cat_ratio
        text_just <- paste0("proporção de <b>Y</b> no Não expostos de <b>", input$logistic_cat_controle, "% </b>",
                            "e risco relativo de <b>", input$logistic_cat_ratio, "</b> como é referida em Fulano (1900), ")

      } else {
        # https://stats.stackexchange.com/questions/324410/converting-odds-ratio-to-percentage-increase-reduction
        prob_control <- input$logistic_cat_controle/100
        p2 <- (input$logistic_cat_odds*prob_control)/ (1 + input$logistic_cat_odds*prob_control - prob_control)
        text_just <- paste0("proporção de <b>Y</b> no Não expostos de <b>", input$logistic_cat_controle, "% </b>",
                            "e razão de chance de <b>", input$logistic_cat_odds, "</b> como é referida em Fulano (1900), ")
      }


      probs <- input$logistic_cat_k/(1+input$logistic_cat_k)

      code <- paste0("powerMediation::SSizeLogisticBin(",
                     # pr(diseased | X = 0)
                     "p1 = ", input$logistic_cat_controle/100, ", ",
                     # pr(diseased | X = 1)
                     "p2 = ", p2, ", ",
                     # pr(X = 1)
                     "B  = ", probs, ", ",
                     "alpha = ", input$sig_logistic/100, ", ",
                     "power = ", input$power_logistic/100, ")")

      n <- try_n(code)
      eval(parse(text = validate_n("n")))


      n1 <- ceiling(n*(1 - probs))
      n2 <- ceiling(n*probs)
      n <- n1 + n2
      eval(parse(text = validate_n_inf("n")))

      nperdas1 <- n_perdas(n1, input$logistic_perdas_recusa_est)
      nperdas2 <- n_perdas(n2, input$logistic_perdas_recusa_est)

      cabecalho <- paste0("<b><font size = '5'>Tamanho amostral calculado: ", n, " (<i>", n1, " no Expostos e ", n2, " no Não expostos</i>)",
                          "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

                          "Foi calculado o tamanho de amostra para testar se a razão de chances para desenvolver <b>Y</b> entre os grupos Expostos e Não expostos é diferente de 1 (um), ",
                          " por meio da ", .txt_citacao_tap, ". ",
                          "Considerando poder de <b>", input$power_logistic, "%</b>, nível de significância de <b>", input$sig_logistic, "% </b>",
                          text_just)


      if(probs == 0.5){

        texto_grupos <- paste0("chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n1, "</b> em cada grupo.",
                               " Acrescentando <b>", input$logistic_perdas_recusa_est, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
                               "deverá ser <b>", nperdas1 + nperdas2, "</b>.")
      } else{

        texto_grupos <- paste0("chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n1, "</b> no Expostos e <b>", n2, "</b> no Não expostos.",
                               " Acrescentando <b>", input$logistic_perdas_recusa_est, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
                               "deverá ser <b>", nperdas1 + nperdas2, "</b> (", nperdas1, " no Expostos e ", nperdas2, " no Não expostos).")
      }

      paste0(cabecalho,
             texto_grupos,
             .txt_referencia_tap, print_r_code(code))

    }
})



  # Estimar ----

  eval(parse(text = warning_numero_positivo("logistic_est_amplitude")))
  eval(parse(text = warning_prop("logistic_est_controle")))
  eval(parse(text = warning_prop("logistic_est_p2")))

  eval(parse(text = warning_numero_positivo("logistic_est_k")))

  eval(parse(text = warning_prop("logistic_est_confiança")))
  eval(parse(text = warning_perdas("logistic_perdas_recusa_estimar")))


  output$out_logistic_estimar <- renderText({

    if(input$or_est_estatistica == "percent"){
      p2 <- input$logistic_est_p2/100
      text_just <- paste0("proporção de <b>Y</b> no Expostos de <b>", input$logistic_est_p2, "% </b>",
                          "e <b>", input$logistic_est_controle, "%</b> no Não expostos, como é referida em Fulano (1900), ")

    } else if (input$or_est_estatistica == "ratio") {

      p2 <- (input$logistic_est_controle/100)*input$logistic_est_ratio
      text_just <- paste0("proporção de <b>Y</b> no Não expostos de <b>", input$logistic_est_controle, "% </b>",
                          "e risco relativo de <b>", input$logistic_est_ratio, "</b> como é referida em Fulano (1900), ")

    } else {
      prob_control <- input$logistic_est_controle/100
      p2 <- (input$logistic_est_odds*prob_control)/ (1 + input$logistic_est_odds*prob_control - prob_control)
      text_just <- paste0("proporção de <b>Y</b> no Não expostos de <b>", input$logistic_est_controle, "% </b>",
                          "e razão de chance de <b>", input$logistic_est_odds, "</b> como é referida em Fulano (1900), ")
    }




    code <- paste0("presize::prec_or(",
                   "p1 = ", p2, ", ",
                   "p2 = ", input$logistic_est_controle/100, ", ",
                   "conf.width = ", input$logistic_est_amplitude, ", ",
                   "r  = ", input$logistic_est_k, ", ",
                   "conf.level = ", input$logistic_est_confiança, "/100, ",
                   "method = '", input$logistic_est_metodo, "')")

    n <- try_n(code)

    n1 <- ceiling(n$n1)
    n2 <- ceiling(n$n2)
    n <- n1 + n2
    eval(parse(text = validate_n("n")))
    eval(parse(text = validate_n_inf("n")))


    nperdas1 <- n_perdas(n1, input$logistic_perdas_recusa_estimar)
    nperdas2 <- n_perdas(n2, input$logistic_perdas_recusa_estimar)

    cabecalho <- paste0("<b><font size = '5'>Tamanho amostral calculado: ", n, " (<i>", n1, " no Expostos e ", n2, " no Não expostos</i>)",
                        "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

                        "Foi calculado o tamanho de amostra para estimar a razão de chances de desenvolver <b>Y</b> entre os grupos Expostos e Não expostos, ",
                        " por meio da ", .txt_citacao_tap, ". ",
                        "Considerando nível de confiança de <b>", input$logistic_est_confiança, "%</b>, ",
                        "amplitude desejada para o intervalo de confiança de <b>", input$logistic_est_amplitude, "</b> ",
                        " utilizando o método ",

                        if(input$logistic_est_metodo == "indip_smooth"){
                          "logit suavizado por independência"
                        } else {
                          str_to_title(input$logistic_est_metodo)
                        },

                        ", ", text_just)


    if(input$logistic_est_k == 1){

      texto_grupos <- paste0("chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n1, "</b> em cada grupo.",
                             " Acrescentando <b>", input$logistic_perdas_recusa_estimar, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
                             "deverá ser <b>", nperdas1 + nperdas2, "</b>.")
    } else{

      texto_grupos <- paste0("chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n1, "</b> no Expostos e <b>", n2, "</b> no Não expostos.",
                             " Acrescentando <b>", input$logistic_perdas_recusa_estimar, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
                             "deverá ser <b>", nperdas1 + nperdas2, "</b> (", nperdas1, " no Expostos e ", nperdas2, " no Não expostos).")
    }

    paste0(cabecalho,
           texto_grupos,
           .txt_referencia_tap, print_r_code(code))

  })







  #____-----
  # Cox ----
  #---------.
  eval(parse(text = warning_numero_positivo("cox_balanceamento")))
  eval(parse(text = warning_numero_positivo("cox_hr_esperado")))
  eval(parse(text = warning_numero_positivo("cox_desvio_padrao")))


  eval(parse(text = warning_prop("cox_failure_trat")))
  eval(parse(text = warning_prop("cox_failure_control")))
  eval(parse(text = warning_prop("cox_failure_continua")))



  eval(parse(text = warning_prop("cox_power")))
  eval(parse(text = warning_prop("cox_significancia")))
  eval(parse(text = warning_perdas("cox_perdas_recusa_est")))




  output$cox_out <- renderText({
    # Equivale ao log rank no winpepi
    # Ver a questão do numero de eventos


    if(input$cox_tipo_variavel == 0){
      code <- paste0("powerSurvEpi::ssizeCT.default(",
                     "k  = ", input$cox_balanceamento, ", ",
                     "pE = ", 1 - input$cox_failure_trat/100, ", ",
                     "pC = ", 1 - input$cox_failure_control/100, ", ",
                     "RR = ", input$cox_hr_esperado,  ", ",
                     "alpha = ", input$cox_significancia/100, ", ",
                     "power = ", input$cox_power/100, ")")
      n <- try_n(code)
      eval(parse(text = validate_n("n")))

      n1 <- n[1]
      n2 <- n[2]

      n <- n1 + n2
      nperdas1 <- n_perdas(n1, input$cox_perdas_recusa_est)
      nperdas2 <- n_perdas(n2, input$cox_perdas_recusa_est)
      eval(parse(text = validate_n_inf("n")))



      paste0(
        "<b><font size = '5'>Tamanho amostral calculado: ", n, " (<i>", n1, " no Grupo Tratamento e ", n2, " no Grupo Controle</i>)",
        "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

        "Foi calculado o tamanho de amostra para testar o <i>hazard ratio</i> entre os grupos Tratamento e Controle para a <b>variável dependente</b>, ",
        "por meio da ", .txt_citacao_tap, ". ",
        "Considerando poder de <b>", input$cox_power, "%</b>, ",
        "nível de significância de <b>", input$cox_significancia, "%</b>, ",
        "probabilidade de sobrevivência até o final do seguimento de <b>", input$cox_failure_trat, "%</b> ",
        if(input$cox_failure_control == input$cox_failure_trat){
          "em ambos grupos "
        } else {
          paste0("para o grupo Tratamento e <b>",
                 input$cox_failure_control, "%</b> para o grupo Controle ")
        },
        "e um <i>hazard ratio</i> esperado de <b>", input$cox_hr_esperado, "</b> (dados de Fulano (1900) <b>OU</b> escolha do pesquisador), ",

        ifelse(input$cox_balanceamento == 1,
               paste0(
                 "chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n1, "</b> em cada grupo.",
                 " Acrescentando <b>", input$cox_perdas_recusa_est, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
                 "deverá ser <b>", nperdas1 + nperdas2, "</b> (", nperdas1, " em cada grupo)."
               ),
               paste0(
                 "chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n1, "</b> no Grupo Tratamento e <b>", n2, "</b> no Grupo Controle.",
                 " Acrescentando <b>", input$cox_perdas_recusa_est, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
                 "deverá ser <b>", nperdas1 + nperdas2, "</b> (", nperdas1, " no Grupo Tratamento e ", nperdas2, " no Grupo Controle)."
               )
        ),

        .txt_referencia_tap, print_r_code(code)
      )



    # Se usar variavel quantitativa
    } else{
      code <- paste0("powerSurvEpi::ssizeEpiCont.default(",
                     "power = ", input$cox_power/100, ", ",
                     "theta = ", input$cox_hr_esperado,  ", ",
                     "sigma2 = ",  input$cox_desvio_padrao, "^2, ",
                     "psi = ", 1 - input$cox_failure_continua/100, ", ",
                     "rho2 = (", input$cox_r2, ")^2, ",
                     "alpha = ", input$cox_significancia/100, ")")


      n <- try_n(code)
      eval(parse(text = validate_n("n")))
      eval(parse(text = validate_n_inf("n")))


      nperdas <- n_perdas(n, input$cox_perdas_recusa_est)

      paste0(
        "<b><font size = '5'>Tamanho amostral calculado: ", n,
        "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

        "Foi calculado o tamanho de amostra para testar o <i>hazard ratio</i> da <b>variável independente contínua</b> sobre a <b>variável dependente</b>, ",
        "por meio da ", .txt_citacao_tap, ". ",
        "Considerando poder de <b>", input$cox_power, "%</b>, ",
        "nível de significância de <b>", input$cox_significancia, "%</b>, ",
        "probabilidade de sobrevivência até o final do seguimento de <b>", input$cox_failure_continua, "%</b>, ",
        "desvio padrão de <b>", input$cox_desvio_padrao, " u.m.</b> ",

        if(input$cox_r2 != 0){
          paste0("coeficiente de correlação múltipla com as demais covariáveis de <b>", input$cox_r2, "</b> ")
        },

        "e um <i>hazard ratio</i> esperado de <b>", input$cox_hr_esperado, "</b> (dados de Fulano (1900) <b>OU</b> escolha do pesquisador), ",
        "chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos.",
        " Acrescentando <b>", input$cox_perdas_recusa_est, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
        "deverá ser <b>", nperdas, "</b>.",
        .txt_referencia_tap, print_r_code(code)
      )
    }

  })




  ## Cenarios ----


  output$cenarios_testar_cox <- renderUI({

    if(input$cox_hr_esperado > 1){
      ratio_start <- input$cox_hr_esperado
      ratio_end  <- input$cox_hr_esperado + 1
      ratio_by   <- 0.1
    } else{
      ratio_start <- max(0, input$cox_hr_esperado - 0.3)
      ratio_end  <- input$cox_hr_esperado
      ratio_by   <- 0.05
    }


    fluidPage(fluidRow(
      conditionalPanel("input.cox_tipo_variavel == 0",
                       br(),
                       HTML('<hr style="color: black;">'),
                       br(),br(),

                       titlePanel("Construção de cenários"),
                       br(),

                       wellPanel("Utilize os argumentos abaixo para construir diferentes cenários.
                                        Você pode especificar valores de poder e uma sequência de HR desejados.
                                        Demais informações serão recuperadas do painel lateral."
                       ),

                       fluidRow(
                         column(6,
                                textInput(inputId = "cox_power_plot",
                                          label   = "Digite valores de poder (%) para fazer o gráfico",
                                          value   = "80, 90, 95",
                                          width   = "400px") %>%
                                  .help_buttom(body = "Defina os valores de poder desejado.
                                                      Esses valores serão utilizados para criar diferentes linhas no gráfico.
                                                      Separe os valores por vírgula ',' e utilize ponto '.' como decimal.")
                         )
                       ),

                       HTML("<b>Defina a sequência de valores para o <i>hazard ratio</i>:</b>"),
                       br(),
                       div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
                           numericInput("cox_from", "Mínimo", value = ratio_start, step = 0.05)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 80px;",
                           numericInput("cox_to", "Máximo", value = ratio_end, step = 0.05)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 80px;",
                           numericInput("cox_by", "Intervalo", value = ratio_by, min = 0, step = 0.05) %>%
                             .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                                         title = "Sequência")
                       ),
                       br(),

                       plotly::plotlyOutput("cox_plot", width = "80%"),
                       br(), br(),
                       downloadButton("download_cox_tab","Download tabela"),
                       DT::dataTableOutput("cox_tab", width = "100%")
      )

    ))
  })

  eval(parse(text = check_text_input_to_vector("cox_power_plot")))

  tab_cox_th_cenarios <- reactive({

    poder <- text_input_to_vector(input$cox_power_plot)
    req(length(poder) > 0)

    df_grid <- expand.grid(`Nível de significância (%)` = input$cox_significancia,
                           `Poder (%)`                  = poder,
                           HR        = seq(input$cox_from, input$cox_to, input$cox_by),
                           `Balanço` = input$cox_balanceamento,
                           pE        = input$cox_failure_trat,
                           pC        = input$cox_failure_control,
                           stringsAsFactors = FALSE)

    df_n <- df_grid %>%
      rename(RR = HR, power = `Poder (%)`, alpha = `Nível de significância (%)`, k = `Balanço`) %$%
      purrr::pmap_dfr(.l = list(power, k, pE, pC, RR, alpha),
                      .f = n_cox_th)

    bind_cols(df_grid, df_n) %>%
      mutate(n = n_trat + n_control)
  })



  output$cox_plot <- plotly::renderPlotly({

    data <- tab_cox_th_cenarios() %>%
      mutate(`Poder (%) ` = factor(`Poder (%)`))

    g1 <- ggplot(data, aes(x = HR,
                           y = n,
                           colour = `Poder (%) `,
                           Tratamento = n_trat,
                           Controle = n_control))+
      geom_point() +
      geom_line() +
      xlab("Hazard ratio esperado") +
      ylab("Tamanho da amostra*") +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(palette = "Set1")



    plotly::ggplotly(g1,
                     tooltip = c("x", "colour", "y", "Tratamento", "Controle")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                showarrow = F, xref='paper', yref='paper',
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=10)))

  })




  output$cox_tab <- DT::renderDataTable({
    tab_cox_th_cenarios() %>%
      DT::datatable(extensions = c('FixedColumns'),
                rownames   = FALSE,
                filter     = "none",
                # #callback   = DT::JS("$('div.dwnld').append($('#download_auc_tab'));"),
                options    = list(pageLength = 10,
                                  scrollX = TRUE,
                                  scrollY = TRUE,
                                  searching = FALSE,
                                  fixedColumns = list(leftColumns = 1),
                                  dom = 'B<"dwnld">frtip'))
  })


  output$download_cox_tab <- downloadHandler(
    filename = function() { "Cenarios_testar_Cox.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_cox_th_cenarios(),
                                                  path = file)}
  )









  #___________------
  # Ferramentas -----


  # obter desvio padrao ----

  desvio_padrao_calc <- reactive({
    # https://handbook-5-1.cochrane.org/chapter_7/7_7_3_data_extraction_for_continuous_outcomes.htm

    ic    <- input$ferramentas_ic_ic
    xbar  <- input$ferramentas_ic_media
    n     <- input$ferramentas_ic_n
    alpha <- input$ferramentas_ic_conf/100


    if(input$ferramentes_desvio_padrao_statistic == "Intervalo de confiança"){

      t_alpha <- qt(1 - (1 - alpha)/2, n-1)

      sd_ <- (ic - xbar)*sqrt(n)/t_alpha
      sd_ <- abs(sd_)

    } else if(input$ferramentes_desvio_padrao_statistic == "Estatística t"){

      standard_error <-  (xbar - input$ferramentas_ic_t_h0)/input$ferramentas_ic_t
      sd_ <- standard_error*sqrt(n)
      sd_ <- abs(sd_)
    } else if(input$ferramentes_desvio_padrao_statistic == "Valor de p"){

      t_calc <- qt(p = input$ferramentas_ic_p/2,
                   df =  n - 1,
                   lower.tail = FALSE)

      standard_error <-  (xbar - input$ferramentas_ic_t_h0)/t_calc
      sd_ <- standard_error*sqrt(n)
      sd_ <- abs(sd_)
    } else if(input$ferramentes_desvio_padrao_statistic == "Erro padrão"){

      sd_ <- input$ferramentas_ep_erro_padrao*sqrt(n)


      # Imputing a change-from-baseline standard deviation using a correlation coefficient
    } else if(input$ferramentes_desvio_padrao_statistic == "Da diferença entre grupos pareados"){

      s1 <- input$ferramentas_sd_baseline
      s2 <- input$ferramentas_sd_follow

      temp <- s1^2 + s2^2 - (2*s1*s2*input$ferramentas_sd_correlation)

      sd_ <- sqrt(temp)
    }

    return(sd_)
  })





  # Imprimi os valores ------.
  output$ferramentas_desvio_padrao_valor <- renderText({

    paste0("<b><font size = '5'>",
           "<i>Desvio padrão</i> = ", round(desvio_padrao_calc(), input$ferramentas_desvio_padrao_decimals),
           "<br><br>",
           "<i>Variância</i> = ", round(desvio_padrao_calc()^2, input$ferramentas_desvio_padrao_decimals),
           "</b>")
  })






  # Imprimi as formulas ------.
  output$ferramentas_desvio_padrao_formulas <- renderUI({

    # Intervalo de confianca
    if(input$ferramentes_desvio_padrao_statistic == "Intervalo de confiança"){

      withMathJax(
        paste0("$$",
               "\\text{Desvio padrão} = \\dfrac{\\text{|} IC - \\bar X \\text{|} }",
               "{t_{\\alpha/2, n-1}} \\sqrt n",
               "$$"
        )
      )


      # Estatistica t
    } else if(input$ferramentes_desvio_padrao_statistic == "Estatística t"){

      withMathJax(
        paste0("$$",
               "\\text{Desvio padrão} = \\dfrac{\\bar X - \\mu_0 }",
               "{t_{\\alpha/2, n-1}} \\sqrt n",
               "$$"
        )
      )


      # Valor de p
    } else if(input$ferramentes_desvio_padrao_statistic == "Valor de p"){

      withMathJax(
        paste0("$$",
               "\\text{Desvio padrão} = \\dfrac{\\bar X - \\mu_0 }",
               "{t_{\\alpha/2, n-1}} \\sqrt n",
               "$$"
        )
      )


      # Erro padrao
    } else if(input$ferramentes_desvio_padrao_statistic == "Erro padrão"){

      # sd_ <- input$ferramentas_ep_erro_padrao*sqrt(input$ferramentas_ep_n)
      #
      # withMathJax(
      #   paste0("$$\\text{Erro padrão} = \\dfrac{\\text{Desvio padrão}}{\\sqrt{\\text{n}}}",
      #          "\\to \\text{Desvio padrão} = \\text{Erro padrão} * \\sqrt{\\text{n}}",
      #          "= ", input$ferramentas_ep_erro_padrao, "*\\sqrt{", input$ferramentas_ep_n, "}",
      #          "\\cong ", round(sd_, trunc(input$ferramentas_ep_decimals)), "$$"
      #   )
      # )
      withMathJax(paste0("$$\\text{Desvio padrão} = \\text{Erro padrão} * \\sqrt{\\text{n}}$$"))





      # Imputing a change-from-baseline standard deviation using a correlation coefficient
    } else if(input$ferramentes_desvio_padrao_statistic == "Da diferença entre grupos pareados"){

      withMathJax(paste0("$$\\text{Desvio padrão}_{\\text{diferença}} = ",
                         "\\sqrt{ \\text{Desvio padrão}_{\\text{Grupo 1}}^2 ",
                         "+ \\text{Desvio padrão}_{\\text{Grupo 2}}^2",
                         " - (2 *\\rho * \\text{Desvio padrão}_{\\text{Grupo 1}} * ",
                         "\\text{Desvio padrão}_{\\text{Grupo 2}})}",
                         "$$"))


    }

  })















  # ferramentas_cohen -----
  # observeEvent(input$link_to_cohen, {
  #   updateTabsetPanel(session, "outras_ferramentas", "panel_d_cohen")
  # })


  output$ferramentas_cohen <- renderText({
    cohen <- cohen_d(mean_diff = input$cohen_mean_dif,
                     n_1 = input$cohen_n1,
                     n_2 = input$cohen_n2,
                     sd_1 = input$cohen_sigma1,
                     sd_2 =input$cohen_sigma2
    )

    paste0("<b><font size = '5'>Cohen'd = ", round(cohen, input$cohen_decimals), "</b>")

  })



  #pooled var -----

  output$pooled_var_sdUi <- renderUI({

    estat_ <- ifelse(input$pooled_eh_sd, "Desvio padrão", "Variância")

    fluidPage(
      HTML(
        paste0("<b><font size = '2.99'>", estat_, " do</font></b><br>")
      ),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
          numericInput( "pooled_sigma1",
                        paste0("do grupo A"),
                        value = 1.2,
                        min = 0,
                        max = Inf,
                        step = .01)
      ),
      div(style="display: inline-block;vertical-align:top; width: 49%;",
          numericInput( "pooled_sigma2",
                        paste0("do grupo B"),
                        value = 1.4,
                        min = 0,
                        max = Inf,
                        step = .01)
      )
    )
  })



  output$ferramentas_pooled <- renderText({

    req(!is.null(input$pooled_sigma1))
    req(!is.null(input$pooled_sigma2))

    s2a <- ifelse(input$pooled_eh_sd, input$pooled_sigma1^2, input$pooled_sigma1)
    s2b <- ifelse(input$pooled_eh_sd, input$pooled_sigma2^2, input$pooled_sigma2)

    n1 <- ifelse(input$pooled_equal_size, 4, input$pooled_n1)
    n2 <- ifelse(input$pooled_equal_size, 4, input$pooled_n2)

    numerador   <- (n1 - 1)*s2a + (n2 - 1)*s2b
    denominador <- n1 + n2 - 2

    s_pooled <- sqrt(numerador/denominador)

    paste0("<b><font size = '5'>",
           "<i>Desvio padrão<sub>combinado</sub></i> = ", round(s_pooled, input$pooled_decimals),
           "<br><br>",
           "<i>Variância<sub>combinada</sub></i> = ", round(s_pooled^2, input$pooled_decimals),
           "</b>")

  })














  #_____ ----
  #AUC -----------

  # Testar ----
  observeEvent(input$auc_auc, {
    shinyFeedback::hideFeedback("auc_auc")
    if(is.na(input$auc_auc)){
      shinyFeedback::showFeedbackWarning(
        inputId = "auc_auc",
        text = "Deve ser fornecido um valor.",
        color = "red"
      )
    } else if (input$auc_auc >= 1) {
      shinyFeedback::showFeedbackWarning(
        inputId = "auc_auc",
        text = "Deve ser menor do que 1.",
        color = "red"
      )
    } else if (input$auc_auc <= 0.5) {
      shinyFeedback::showFeedbackWarning(
        inputId = "auc_auc",
        text = "Deve ser maior do que 0.5.",
        color = "red"
      )
    }
  })

  eval(parse(text = warning_prop("auc_k")))
  eval(parse(text = warning_prop("auc_poder")))
  eval(parse(text = warning_prop("auc_significancia")))
  eval(parse(text = warning_perdas("auc_perdas_recusa")))


  output$auc_output <- renderText({
    # Da pra fazer unilateral!

    prob <- 1 - input$auc_k/100
    kappa <- prob/(1 - prob)

    code <- paste0(
      "pROC::power.roc.test(auc = ", input$auc_auc, ", ",
      "sig.level = ", input$auc_significancia/100,  ", ",
      "power = ", input$auc_poder/100, ", ",
      "kappa = ", kappa, ")")

    n <- try_n(code)
    eval(parse(text = validate_n("n")))


    n_casos <- ceiling(n$ncases)
    n_control <- ceiling(n$ncontrols)

    nperdas_casos <- n_perdas(n_casos, input$auc_perdas_recusa)
    nperdas_controle <- n_perdas(n_control, input$auc_perdas_recusa)

    n <- n_casos + n_control
    nperdas <- nperdas_casos + nperdas_controle
    eval(parse(text = validate_n_inf("n")))

    precisao <- presize::prec_auc(n = n,
                                  auc = input$auc_auc,
                                  prev = input$auc_k/100,
                                  conf.level = 1 - input$auc_significancia/100)


    cabecalho <- paste0("<b><font size = '5'>Tamanho amostral calculado: ", n, " (<i>", n_casos, " Casos e ", n_control, " Controles</i>)",
                        "</font></b></br></br><i>Sugestão de texto:</i></br></br>")

    texto_comparacao <- paste0("Foi calculado o tamanho de amostra para testar se a área sob a curva é diferente de 0.5, ",
                               "utilizando <b><i>uma variável contínua preditora</i></b> para diagnosticar <b><i>uma variável binária resposta</b></i>,",
                               " por meio da ", .txt_citacao_tap, ". ",
                               "Considerando nível de significância de <b>", input$auc_significancia, "%</b>, ",
                               "poder de <b>", input$auc_poder, "%</b>, ",
                               "prevalência de casos de <b>", input$auc_k, "% </b>",
                               "e uma área sob a curva esperada de <b>", input$auc_auc, "</b>, ",
                               "como referida por Fulano (1900), ")


    if(kappa == 1){

      texto_grupos <- paste0("chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n_casos, "</b> em cada grupo.",
                             " Acrescentando <b>", input$auc_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
                             "deverá ser <b>", nperdas, "</b>.")
    } else{

      texto_grupos <- paste0("chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n_casos, "</b> Casos e <b>", n_control, "</b> no Controles.",
                             " Acrescentando <b>", input$auc_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
                             "deverá ser <b>", nperdas , "</b> (", nperdas_casos, " no Caso e ", nperdas_controle, " no Controle).")
    }

    paste0(cabecalho,
           texto_comparacao,
           texto_grupos,

           "</br></br></br><i>Obs.: </i>Com esse tamanho de amostra o intervalo de confiança da área sob a curva será aproximadamente <b>[", round(precisao$lwr, 2), " ; ", round(precisao$upr, 2), "]</b>.",

           .txt_referencia_tap, print_r_code(code)
    )
  })



  ## Cenarios ----

  output$cenarios_roc_th_Ui <- renderUI({

    fluidPage(fluidRow(
      br(),
      HTML('<hr style="color: black;">'),
      br(),br(),

      titlePanel("Construção de cenários"),
      br(),

      wellPanel("Utilize os argumentos abaixo para construir diferentes cenários.
                                        Você pode especificar valores de poder e uma sequência de valores para a área sob a curva.
                                        Demais informações serão recuperadas do painel lateral."),

      fluidRow(
        column(6,
               textInput(inputId = "auc_power_plot",
                         label   = "Digite valores de poder (%) para fazer o gráfico",
                         value   = "80, 90, 95",
                         width   = "400px") %>%
                 .help_buttom(body = "Defina os valores de poder desejado.
                                                      Esses valores serão utilizados para criar diferentes linhas no gráfico.
                                                      Separe os valores por vírgula ',' e utilize ponto '.' como decimal.")
        )
      ),

      HTML("<b>Defina a sequência de valores para a área sob a curva:</b>"),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
          numericInput("auc_from", "Mínimo", value = input$auc_auc, step = 0.05)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("auc_to", "Máximo", value = min(1, input$auc_auc + 0.4), step = 0.05)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("auc_by", "Intervalo", value = 0.05, min = 0, step = 0.05) %>%
            .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                         title = "Sequência")
      ),
      br(),

      plotly::plotlyOutput("auc_plot", width = "80%"),
      br(), br(),
      downloadButton("download_auc_tab","Download tabela"),
      DT::dataTableOutput("auc_tab", width = "100%")
    ))

  })




  eval(parse(text = check_text_input_to_vector("auc_power_plot")))

  tab_auc_th_cenarios <- reactive({

    poder <- text_input_to_vector(input$auc_power_plot)
    req(length(poder) > 0)


    df_grid <- expand.grid(`Poder (%)`                  = poder,
                AUC                          = seq(input$auc_from, input$auc_to, input$auc_by),
                `Nível de significância (%)` = input$auc_significancia,
                `Balanço`                    = input$auc_k,
                stringsAsFactors = FALSE)

    df_n <- df_grid %>%
      rename(auc = AUC, power = `Poder (%)`, sig.level = `Nível de significância (%)`, kappaa = `Balanço`) %$%
      purrr::pmap_dfr(.l = list(auc, power, sig.level, kappaa),
                      .f = auc_th_n)

    bind_cols(df_grid, df_n) %>%
      mutate(n = n_casos + n_control)
  })



  output$auc_plot <- plotly::renderPlotly({

    data <- tab_auc_th_cenarios() %>%
      mutate(`Poder (%) ` = factor(`Poder (%)`))

    g1 <- ggplot(data, aes(x = AUC,
                     y = n,
                     colour = `Poder (%) `,
                     Caso = n_casos,
                     Controle = n_control))+
      geom_point() +
      geom_line() +
      xlab("AUC esperada") +
      ylab("Tamanho da amostra*") +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(palette = "Set1")



  plotly::ggplotly(g1,
                   tooltip = c("x", "colour", "y", "Caso", "Controle")) %>%
    plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                              showarrow = F, xref='paper', yref='paper',
                              xanchor='right', yanchor='auto', xshift=0, yshift=0,
                              font=list(size=10)))

  })




  output$auc_tab <- DT::renderDataTable({
    tab_auc_th_cenarios() %>%
      DT::datatable(extensions = c('FixedColumns'),
                rownames   = FALSE,
                filter     = "none",
                # #callback   = DT::JS("$('div.dwnld').append($('#download_auc_tab'));"),
                options    = list(pageLength = 10,
                                  scrollX = TRUE,
                                  scrollY = TRUE,
                                  searching = FALSE,
                                  fixedColumns = list(leftColumns = 1),
                                  dom = 'B<"dwnld">frtip'))
  })


  output$download_auc_tab <- downloadHandler(
    filename = function() { "Cenarios_testar_auc.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_auc_th_cenarios(),
                                                  path = file)}
  )



  # Estimar ----


  eval(parse(text = warning_prop("auc_est_auc", entre0e1 = TRUE)))
  eval(parse(text = warning_prop("auc_est_k")))
  eval(parse(text = warning_prop("auc_est_confiança")))
  eval(parse(text = warning_numero_positivo("auc_est_amplitude")))
  eval(parse(text = warning_perdas("auc_est_perdas_recusa")))



  output$auc_est_output <- renderText({

    code <- paste0("presize::prec_auc(",
                   "auc = ", input$auc_est_auc, ", ",
                   "prev = ", input$auc_est_k/100, ", ",
                   "conf.level = ", input$auc_est_confiança/100,  ", ",
                   "conf.width = ", input$auc_est_amplitude, ")")

    n <- try_n(code)
    eval(parse(text = validate_n("n")))


    n_casos <- ceiling(n$n1)
    n_control <- ceiling(n$n2)

    nperdas_casos <- n_perdas(n_casos, input$auc_est_perdas_recusa)
    nperdas_controle <- n_perdas(n_control, input$auc_est_perdas_recusa)

    n <- n_casos + n_control
    nperdas <- nperdas_casos + nperdas_controle
    eval(parse(text = validate_n_inf("n")))

    poder_teste <- pROC::power.roc.test(auc = input$auc_est_auc,
                                        sig.level = 1 - input$auc_est_confiança/100,
                                        power = NULL,
                                        kappa = (input$auc_est_k/100)/(1-input$auc_est_k/100),
                                        ncontrols = n_control,
                                        ncases    = n_casos)$power


    cabecalho <- paste0("<b><font size = '5'>Tamanho amostral calculado: ", n, " (<i>", n_casos, " Casos e ", n_control, " Controles</i>)",
                        "</font></b></br></br><i>Sugestão de texto:</i></br></br>")

    texto_comparacao <- paste0("Foi calculado o tamanho de amostra para estimar a área sob a curva com uma amplitude de <b>", input$auc_est_amplitude, "</b> pontos, ",
                               "utilizando <b><i>uma variável contínua preditora</i></b> para diagnosticar <b><i>uma variável binária resposta</b></i>,",
                               " por meio da ", .txt_citacao_tap, ". ",
                               "Considerando nível de confiança de <b>", input$auc_est_confiança, "%</b>, ",
                               "prevalência de casos de <b>", input$auc_est_k, "% </b>",
                               "e uma área sob a curva esperada de <b>", input$auc_est_auc, "</b>, ",
                               "como referida por Fulano (1900), ")


    if(input$auc_est_k == 50){

      texto_grupos <- paste0("chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n_casos, "</b> em cada grupo.",
                             " Acrescentando <b>", input$auc_est_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
                             "deverá ser <b>", nperdas, "</b>.")
    } else{

      texto_grupos <- paste0("chegou-se ao tamanho de amostra total de <b>", n , "</b> sujeitos, sendo <b>", n_casos, "</b> Casos e <b>", n_control, "</b> no Controles.",
                             " Acrescentando <b>", input$auc_est_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra ",
                             "deverá ser <b>", nperdas , "</b> (", nperdas_casos, " no Caso e ", nperdas_controle, " no Controle).")
    }

    paste0(cabecalho,
           texto_comparacao,
           texto_grupos,
           "</br></br><i>Obs.:</i> Com esse tamanho de amostra o poder do teste, para testar se a área sob a curva é diferente de 0,5, será aproximadamente <b>",
           round(poder_teste*100, 1), "</b>%.",
           .txt_referencia_tap, print_r_code(code)
    )
  })




  #_____________________----
  # Sens/ especificidade -----


  observeEvent(input$especif_especificidade, {
    shinyFeedback::hideFeedback("especif_especificidade")

    if(!is.na(input$especif_especificidade)){
      if (input$especif_especificidade >= 100) {
        shinyFeedback::showFeedbackWarning(
          inputId = "especif_especificidade",
          text = "Deve ser menor do que 100%.",
          color = "red"
        )
      } else if (input$especif_especificidade < 0) {
        shinyFeedback::showFeedbackWarning(
          inputId = "especif_especificidade",
          text = "Deve ser maior ou igual a 0.",
          color = "red"
        )
      }
    }
  })


  observeEvent(input$sensibil_sensibilidade, {
    shinyFeedback::hideFeedback("sensibil_sensibilidade")

    if(!is.na(input$sensibil_sensibilidade)){
      if (input$sensibil_sensibilidade >= 100) {
        shinyFeedback::showFeedbackWarning(
          inputId = "sensibil_sensibilidade",
          text = "Deve ser menor do que 100%.",
          color = "red"
        )
      } else if (input$sensibil_sensibilidade < 0) {
        shinyFeedback::showFeedbackWarning(
          inputId = "sensibil_sensibilidade",
          text = "Deve ser maior ou igual a 0.",
          color = "red"
        )
      }
    }
  })


  eval(parse(text = warning_numero_positivo("sensibil_amplitude")))
  eval(parse(text = warning_prop("sensibil_prevalencia")))
  eval(parse(text = warning_prop("sensibil_confianca")))




  output$sensibil_output <- renderText({

    especificidade     = input$especif_especificidade/100
    sensibilidade      = input$sensibil_sensibilidade/100
    prevalencia_doenca = input$sensibil_prevalencia/100
    amplitude          = input$sensibil_amplitude/100
    alpha              = input$sensibil_confianca/100
    metodo             = input$sensibil_metodo


    n_sens = n_espe = 0

    if(!is.na(sensibilidade)){
      if(sensibilidade != 0){
        code_sens <- paste0("presize::prec_sens(",
                            "sens = ", sensibilidade, ", ",
                            # "prev = ", prevalencia_doenca, ", ",
                            "conf.width = ", amplitude, ", ",
                            "conf.level = ", alpha, ", ",
                            "method = '", metodo, "')$n/",
                            prevalencia_doenca)

        n_sens <- eval(parse(text = code_sens))
        n_sens <- ceiling(n_sens)
      }
    }


    if(!is.na(especificidade)){
      if(especificidade != 0){
        code_esp <- paste0("presize::prec_spec(",
                           "spec = ", especificidade, ", ",
                           # "prev = ", prevalencia_doenca, ", ",
                           "conf.width = ", amplitude, ", ",
                           "conf.level = ", alpha, ", ",
                           "method = '", metodo, "')$n/(1 - ",
                           prevalencia_doenca, ")")
        n_espe <- eval(parse(text = code_esp))
        n_espe <- ceiling(n_espe)
      }
    }

    n <- ifelse(n_sens > n_espe, n_sens, n_espe)



    validate(
      need(n_sens != 0 | n_espe != 0, "Deve ser oferecido um valor de sensibilidade ou de especificidade no painel lateral.")
    )

    eval(parse(text = validate_n("n")))

    eval(parse(text = validate_n_inf("n")))


    paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
           "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

           "Foi realizado o cálculo do tamanho de amostra para estimar a ",

            if(n_sens != 0 & n_espe != 0){
             "sensibilidade e a especificidade "
           } else if(n_sens != 0){
             "e sensibilidade "
           } else {
             "e especificidade "
           },

           "do <b><i>teste tal</i></b> para diagnosticar <b><i>tal desfecho</b></i>, ",
           "utilizando a ", .txt_citacao_tap, ". ",
           "Considerando nível de confiança de <b>", input$sensibil_confianca, "%</b>, ",
           "amplitude para o intervalo de confiança desejada de <b>", input$sensibil_amplitude,
           "%</b> utilizando o método de ", str_to_title(input$sensibil_metodo),
           ", prevalência do <b><i>tal desfecho</b></i> de <b>", input$sensibil_prevalencia,

           if(n_sens != 0 & n_espe != 0){
             paste0(
               "%</b>, sensibilidade esperada de <b>", input$sensibil_sensibilidade, "%</b> ",
               "e especificidade esperada de <b>", input$especif_especificidade, "%</b> "
             )
           } else if(n_sens != 0){
             paste0(
               "%</b>, sensibilidade esperada de <b>", input$sensibil_sensibilidade, "%</b> "
             )
           } else {
             paste0(
               "%</b>, especificidade esperada de <b>", input$especif_especificidade, "%</b> "
             )
           },

           "conforme referida em Fulano (1900) <b>OU</b> escolha do pesquisador,",
           " chegou-se ao tamanho de amostra total de <b>", n, "</b> sujeitos. Acrescentando <b>", input$sensibil_perdas_recusa,
           "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas(n, input$sensibil_perdas_recusa), "</b>.",

           .txt_referencia_tap,

           if(n_sens != 0 & n_espe != 0){
             paste0("</br><br><i>Obs.:</i> n<sub>sens</sub> = ", n_sens, ", n<sub>esp</sub> = ", n_espe)
           },

           if(n_sens != 0 & n_espe != 0){
             paste0("</br></br>",
                    "<i>Comando R utilizado:</i><br>",
                    "<p style=\"font-family:'Courier New';font-size:100% \">", code(paste0("n_sens <- ", code_sens)), "</p>",
                    "<p style=\"font-family:'Courier New';font-size:100% \">", code(paste0("n_esp <- ", code_esp)), "</p><br>")
           } else if(n_sens != 0){
             print_r_code(code_sens)
           } else {
             print_r_code(code_esp)
           }
    )


  })





  ## Cenarios ----

  output$cenarios_sensi_espUi <- renderUI({

    fluidPage(fluidRow(
      br(),
      HTML('<hr style="color: black;">'),
      br(),br(),
      titlePanel("Construção de cenários"),
      br(),

      wellPanel("Utilize os argumentos abaixo para construir diferentes cenários.
                                        Você pode especificar valores de prevalência esperada e uma sequência de sensibilidade e especificidade desejados.
                                        Demais informações serão recuperadas do painel lateral."),

      fluidRow(
        column(6,
               textInput(inputId = "sensibil_prev_plot",
                         label   = "Digite valores de prevalência (%) para fazer o gráfico",
                         value   = paste0(c(input$sensibil_prevalencia, input$sensibil_prevalencia + 2.5, input$sensibil_prevalencia + 5), collapse = ", "),
                         width   = "400px") %>%
                 .help_buttom(body = paste0("Defina os valores de prevalência desejado. ",
                                           "Esses valores serão utilizados para criar diferentes linhas no gráfico. Separe os valores por vírgula ',' e utilize ponto '.' como decimal."))
        )
      ),

      HTML("<b>Defina a sequência de valores para a sensibilidade/ especificidade (%):</b>"),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
          numericInput("sensibil_from", "Mínimo", value = 5, step = 5)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("sensibil_to", "Máximo", value = 95, step = 5)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("sensibil_by", "Intervalo", value = 10, min = 0, step = 1) %>%
            .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                        title = "Sequência")
      ),
      br(),

      plotly::plotlyOutput("sensibil_plot", width = "80%"),
      br(), br(),
      downloadButton("download_sensibil_tab","Download tabela"),
      DT::dataTableOutput("sensibil_tab", width = "100%")


    ))
  })


  eval(parse(text = check_text_input_to_vector("sensibil_prev_plot")))


  tab_sensibil_th_cenarios <- reactive({


    n_ <- function(alpha, sens_esp, precisao, prevalencia, method, n_sen = TRUE){
      if(n_sen){
        n <- tryCatch({
          presize::prec_sens(sens = sens_esp,
                             # prev = prevalencia,
                             conf.width = precisao,
                             conf.level = alpha,
                             method = method)$n/prevalencia},
          warning = function(warning_condition) { NA },
          error = function(error_condition) { NA })

      } else{
        n <- tryCatch({
          presize::prec_spec(spec = sens_esp,
                             # prev = prevalencia,
                             conf.width = precisao,
                             conf.level = alpha,
                             method = method)$n/(1 - prevalencia)},
          warning = function(warning_condition) { NA },
          error = function(error_condition) { NA })
      }

      if(class(n) == "logical"){
        NA_real_
      } else{
        ceiling(n)
      }
    }



    prevalencias <- text_input_to_vector(input$sensibil_prev_plot)
    req(length(prevalencias) > 0)

    df_grid <- expand.grid(
      `Método` = input$sensibil_metodo,
      `Prevalência (%)`   = prevalencias,
      `Sensibilidade/ especificidade (%)` = seq(input$sensibil_from, input$sensibil_to, input$sensibil_by),
      `Amplitude (%)`      = input$sensibil_amplitude,
      `Nível de confiança (%)` = input$sensibil_confianca,
      stringsAsFactors = FALSE
    )

    # df_grid <- expand.grid(
    #   `Prevalência (%)`   = c(60, 70, 80, 90),
    #   `Sensibilidade/ especificidade (%)` = seq(0.1, 0.9, 0.1)*100,
    #   `Amplitude (%)`      = 10,
    #   `Nível de confiança (%)` = 95,
    #   `Método` = "wilson",
    #   stringsAsFactors = FALSE
    # )


    df_n_sensibil <- df_grid %>%
      mutate(sens_esp      = `Sensibilidade/ especificidade (%)`/100,
             precisao      = `Amplitude (%)`/100,
             prevalencia   = `Prevalência (%)`/100,
             alpha = `Nível de confiança (%)`/100,
             method = `Método`,
             n_sen = TRUE)  %>%
      mutate(n_sens = purrr::pmap_dbl(.l = list(alpha, sens_esp, precisao, prevalencia, method, n_sen),
                                      .f = n_)) %>%
      mutate(n_sen = FALSE)  %>%
      mutate(n_espec = purrr::pmap_dbl(.l = list(alpha, sens_esp, precisao, prevalencia, method, n_sen),
                                       .f = n_)) %>%
      dplyr::as_tibble()


    bind_cols(df_grid,
              dplyr::select(df_n_sensibil, n_sens, n_espec)) %>%
      mutate(n_maior = pmax(n_sens, n_espec))
  })




  output$sensibil_plot <- plotly::renderPlotly({


    g1 <- tab_sensibil_th_cenarios() %>%
      mutate(`Prevalência (%)` = factor(`Prevalência (%)`)) %>%
      ggplot(aes(y = n_maior,
                 x = `Sensibilidade/ especificidade (%)`,
                 colour = `Prevalência (%)`,
                 n_sens = n_sens,
                 n_espec = n_espec))+
      geom_point() +
      geom_line() +
      xlab("Sensibilidade/ especificidade (%) esperada(s)") +
      ylab("Tamanho da amostra*") +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(palette = "Set1")



    plotly::ggplotly(g1,
                     tooltip = c("x", "colour", "n_sens", "n_espec")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                showarrow = F, xref='paper', yref='paper',
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=10)))

  })




  output$sensibil_tab <- DT::renderDataTable({
    tab_sensibil_th_cenarios() %>%
      DT::datatable(extensions = c('FixedColumns'),
                rownames   = FALSE,
                filter     = "none",
                # #callback   = DT::JS("$('div.dwnld').append($('#download_auc_tab'));"),
                options    = list(pageLength = 10,
                                  scrollX = TRUE,
                                  scrollY = TRUE,
                                  searching = FALSE,
                                  fixedColumns = list(leftColumns = 1),
                                  dom = 'B<"dwnld">frtip'))
  })


  output$download_sensibil_tab <- downloadHandler(
    filename = function() { "Cenarios_Sensibilidade_Especificidade.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_sensibil_th_cenarios(),
                                                  path = file)}
  )






  # aba_especificidade ----.



  # output$especif_output <- renderText({
  #   # https://www.ncbi.nlm.nih.gov/pubmed/8870764/
  #
  #   especificidade     = input$especif_especificidade/100
  #   prevalencia_doenca = input$especif_prevalencia/100
  #   amplitude          = input$especif_amplitude/100
  #   alpha              = 1 - input$especif_confianca/100
  #
  #
  #   n <- (qnorm(1 - alpha/2)^2 * (especificidade*(1-especificidade))/amplitude^2)/(1 - prevalencia_doenca)
  #   n <- ceiling(n)
  #
  #
  #   paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
  #          "</font></b></br></br><i>Sugestão de texto:</i></br></br>",
  #
  #
  #          "Foi realizado o cálculo do tamanho de amostra para estimar a especificidade do ",
  #          "<b><i>teste tal</i></b> para diagnosticar <b><i>tal desfecho</b></i>, ",
  #          "utilizando as fómulas descritas por Buderer, N.M.F. (1996). ",
  #          "Considerando nível de confiança de <b>", input$especif_confianca, "%</b>, precisão desejada de <b>", input$especif_amplitude,
  #          "%</b>, prevalência do desfecho de <b>", input$especif_prevalencia,
  #          "%</b> e especificidade esperada de <b>", input$especif_sensibilidade, "%</b>, conforme referida em Fulano (1900),",
  #          " chegou-se ao tamanho de amostra total de <b>", n, "</b> sujeitos. Acrescentando <b>", input$especif_perdas_recusa,
  #          "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas(n, input$especif_perdas_recusa), "</b>.",
  #
  #
  #
  #          "</br><br><br><br>
  #          Buderer, N.M.F. (1996), Statistical Methodology: I. Incorporating the Prevalence of Disease into the Sample Size Calculation for Sensitivity and Specificity. Academic Emergency Medicine, 3: 895-900. ",
  #          "<a href=' https://doi.org/10.1111/j.1553-2712.1996.tb03538.x' target='_blank'>  https://doi.org/10.1111/j.1553-2712.1996.tb03538.x </a>."
  #
  #   )
  #
  #
  # })






  #_______-----
  # Kappa    ----
  #---------------------.

  # Testar ----

  output$kappa_th_formula <- renderUI({
    withMathJax(paste0("$$H_0: \\kappa = ", input$kappa_h0,
             " \\text{  vs  } H_1: \\kappa  \\neq ", input$kappa_h0, "$$"))
  })




  output$kappa_prevk_Ui <- renderUI({
    req(!input$kappa_k_categorias == 2)

    prob_start <- dplyr::case_when(input$kappa_k_categorias == 3 ~ "20.5, 50, 29.5",
                                   input$kappa_k_categorias == 4 ~ "20.5, 30.5, 30, 19",
                                   input$kappa_k_categorias == 5 ~ "20.5, 30.5, 30, 10, 9")

    textInput( "kappa_prevk",
               paste0("Probabilidade de ocorrência de cada uma das ", input$kappa_k_categorias, " categorias (%)"),
               value = prob_start) %>%
      .help_buttom(body = paste0("Probabilidade de ocorrência de cada uma das ", input$kappa_k_categorias, " categorias (%)"))

  })


  observeEvent(input$kappa_raters, {
    shinyFeedback::hideFeedback("kappa_raters")

    if(is.na(input$kappa_raters)){
      shinyFeedback::showFeedbackWarning(
        inputId = "kappa_raters",
        text = "Deve ser um número.",
        color = "red"
      )
    } else if(input$kappa_raters%%1 != 0){
      shinyFeedback::showFeedbackWarning(
        inputId = "kappa_raters",
        text = "Deve ser um número inteiro maior ou igual a 2.",
        color = "red"
      )
    } else if (input$kappa_raters < 2) {
      shinyFeedback::showFeedbackWarning(
        inputId = "kappa_raters",
        text = "Deve ser maior ou igual a 2.",
        color = "red"
      )
    }
  })


  observeEvent(input$kappa_prevk, {
    req(!is.null(input$kappa_prevk))

    probs <- text_input_to_vector(input$kappa_prevk)

    shinyFeedback::hideFeedback("kappa_prevk")

    if(is.na(input$kappa_prevk)){
      shinyFeedback::showFeedbackWarning(
        inputId = "kappa_prevk",
        text = "Deve ser fornecido os valores.",
        color = "red"
      )
    } else if(sum(probs) != 100){
      shinyFeedback::showFeedbackWarning(
        inputId = "kappa_prevk",
        text = "A soma das probabilidades das categorias de resposta deve ser 100%.",
        color = "red"
      )
    }
  })


  eval(parse(text = warning_prop("kappa_kappa_esperado", entre0e1 = TRUE)))
  eval(parse(text = warning_prop("kappa_h0", entre0e1 = TRUE)))
  eval(parse(text = warning_prop("kappa_prev2")))
  eval(parse(text = warning_prop("kappa_power")))
  eval(parse(text = warning_prop("kappa_significancia")))
  eval(parse(text = warning_perdas("kappa_perdas_recusa")))





  output$kappa_output <- renderText({
    # "Bujang, Mohamad Adam & Baharum, N.. (2017). Guidelines of the minimum sample size requirements for Cohen’s Kappa. Epidemiology Biostatistics and Public Health. 14. e12267-1. 10.2427/12267. "

    if(input$kappa_k_categorias == 2){
      probs <- input$kappa_prev2
    } else{
      req(!is.null(input$kappa_prevk))

      probs <- text_input_to_vector(input$kappa_prevk)

      categorias <- paste(letters[1:length(probs)], collapse = ", ") %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish()
      probs_categorias <- input$kappa_prevk %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish()

      validate(
        need(sum(probs) == 100,
             paste0("A soma das probabilidades das categorias de resposta deve ser 100%. ",
                    "Com os dados atuais temos ", str_replace_all(input$kappa_prevk, ",", fixed(" + ")),
                    " = ", sum(probs, na.rm = TRUE), "%. Faltam ", 100 - sum(probs, na.rm = TRUE), "%."
             ))

      )
    }



    foo <- ifelse(input$kappa_k_categorias == 2, "kappaSize::PowerBinary",
                  ifelse(input$kappa_k_categorias == 3,  "kappaSize::Power3Cats",
                         ifelse(input$kappa_k_categorias == 4,  "kappaSize::Power4Cats",
                                ifelse(input$kappa_k_categorias == 5, "kappaSize::Power5Cats",
                                       NULL))))




    code <- paste0(
      foo, "(kappa0 = ", input$kappa_h0, ", ",
      "kappa1 = ", input$kappa_kappa_esperado, ", ",
      "props  = c(", paste(probs/100, collapse = ", "),  "), ",
      "raters = ", input$kappa_raters, ", ",
      "alpha  = ", input$kappa_significancia/100, ", ",
      "power  = ", input$kappa_power/100, ")"
    )

    n <- try_n(code)
    eval(parse(text = validate_n("n")))

    n <- ceiling(n$N)
    eval(parse(text = validate_n_inf("n")))


    metodos <- paste(LETTERS[1:input$kappa_raters], collapse = ", ") %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish()

    paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
           "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

           "Foi calculado o tamanho de amostra para testar se o kappa, que avaliará o nível de ",
           "concordância entre os <b>métodos/ avaliadores ", metodos, "</b>, ",

           # if(sided){
           paste0("é diferente de <b>", input$kappa_h0, "</b>"),
           # } else{
           #   paste0("é maior do que <b>", input$kappa_h0, "</b>")
           # },

           "</b>, por meio da ", .txt_citacao_tap, ". ",
           "Considerando nível de significância de <b>", input$kappa_significancia, "%</b>, poder de <b>", input$kappa_power,
           "%</b>, kappa esperado de <b>", input$kappa_kappa_esperado, "</b> ",
           # "e probabilidades de diagnóstico positivo de <b>", input$kappa_prev1, "%</b> e <b>", input$kappa_prev2, "%</b> ",
           # "para os métodos A e B, respectivamente, conforme referência de Fulano (1900),",
           if(input$kappa_k_categorias == 2){
             paste0("e probabilidades de diagnóstico positivo de <b>", input$kappa_prev2, "%</b> ")
           } else{
             paste0("e probabilidades de ocorrência de <b>", probs_categorias, "%</b> para as categorias <b>", categorias, "</b>, respectivamente, ")
           },
           "conforme referência de Fulano (1900),",
           " chegou-se ao tamanho de amostra total de <b>", n, "</b> unidades amostrais. Acrescentando <b>", input$kappa_perdas_recusa,
           "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas(n, input$kappa_perdas_recusa), "</b>.",


           .txt_referencia_tap, print_r_code(code)
    )


  })



  # Estimar ----

  output$kappa_est_prevk_Ui <- renderUI({
    req(!input$kappa_est_k_categorias == 2)

    prob_start <- dplyr::case_when(input$kappa_est_k_categorias == 3 ~ "20.5, 50, 29.5",
                                   input$kappa_est_k_categorias == 4 ~ "20.5, 30.5, 30, 19",
                                   input$kappa_est_k_categorias == 5 ~ "20.5, 30.5, 30, 10, 9")

    textInput( "kappa_est_prevk",
               paste0("Probabilidade de ocorrência de cada uma das ", input$kappa_est_k_categorias, " categorias (%)"),
               value = prob_start
    ) %>% .help_buttom(body = paste0("Probabilidade de ocorrência de cada uma das ", input$kappa_est_k_categorias, " categorias (%)"))

  })

  observeEvent(input$kappa_est_raters, {
    shinyFeedback::hideFeedback("kappa_est_raters")

    if(is.na(input$kappa_est_raters)){
      shinyFeedback::showFeedbackWarning(
        inputId = "kappa_est_raters",
        text = "Deve ser um número.",
        color = "red"
      )
    } else if(input$kappa_est_raters%%1 != 0){
      shinyFeedback::showFeedbackWarning(
        inputId = "kappa_est_raters",
        text = "Deve ser um número inteiro maior ou igual a 2.",
        color = "red"
      )
    } else if (input$kappa_est_raters < 2) {
      shinyFeedback::showFeedbackWarning(
        inputId = "kappa_est_raters",
        text = "Deve ser maior ou igual a 2.",
        color = "red"
      )
    }
  })

  observeEvent(input$kappa_est_prevk, {
    req(!is.null(input$kappa_est_prevk))

    probs <- text_input_to_vector(input$kappa_est_prevk)

    shinyFeedback::hideFeedback("kappa_est_prevk")

    if(is.na(input$kappa_est_prevk)){
      shinyFeedback::showFeedbackWarning(
        inputId = "kappa_est_prevk",
        text = "Deve ser fornecido os valores.",
        color = "red"
      )
    } else if(sum(probs) != 100){
      shinyFeedback::showFeedbackWarning(
        inputId = "kappa_est_prevk",
        text = "A soma das probabilidades das categorias de resposta deve ser 100%.",
        color = "red"
      )
    }
  })


  eval(parse(text = warning_prop("kappa_est_kappa_esperado", entre0e1 = TRUE)))
  eval(parse(text = warning_numero_positivo("kappa_est_amplitude")))
  eval(parse(text = warning_prop("kappa_est_k_categorias")))
  eval(parse(text = warning_prop("kappa_est_confianca")))
  eval(parse(text = warning_perdas("kappa_est_perdas_recusa")))




  output$kappa_est_output <- renderText({
    # "Bujang, Mohamad Adam & Baharum, N.. (2017). Guidelines of the minimum sample size requirements for Cohen’s Kappa. Epidemiology Biostatistics and Public Health. 14. e12267-1. 10.2427/12267. "

    if(input$kappa_est_k_categorias == 2){
      probs <- input$kappa_est_prev2
    } else{
      req(!is.null(input$kappa_est_prevk))

      probs <- text_input_to_vector(input$kappa_est_prevk)

      categorias <- paste(letters[1:length(probs)], collapse = ", ") %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish()
      probs_categorias <- input$kappa_est_prevk %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish()

      validate(
        need(sum(probs) == 100,
             paste0("A soma das probabilidades das categorias de resposta deve ser 100%. ",
                    "Com os dados atuais temos ", str_replace_all(input$kappa_est_prevk, ",", fixed(" + ")),
                    " = ", sum(probs, na.rm = TRUE), "%. Faltam ", 100 - sum(probs, na.rm = TRUE), "%."
             ))

      )
    }

    code <- paste0(
      "presize::prec_kappa(",
      "kappa = ", input$kappa_est_kappa_esperado, ", ",
      "conf.width = ", input$kappa_est_amplitude, ", ",
      "raters = ", input$kappa_est_raters, ", ",
      "n_category = ", input$kappa_est_k_categorias, ", ",
      "props  = c(", paste(probs/100, collapse = ", "),  "), ",
      "conf.level  = ", input$kappa_est_confianca, "/100)"
    )

    n <- try_n(code)
    eval(parse(text = validate_n("n")))

    n <- ceiling(n$n)
    eval(parse(text = validate_n_inf("n")))


    metodos <- paste(LETTERS[1:input$kappa_est_raters], collapse = ", ") %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish()

    paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
           "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

           "Foi calculado o tamanho de amostra para estimar o kappa de Cohen, que avaliará o nível de ",
           "concordância entre os <b>métodos/ avaliadores ", metodos, "</b>, ",
           "por meio da ", .txt_citacao_tap, ". ",
           "Considerando nível de confiança de <b>", input$kappa_est_confianca, "%</b>, ",
           "kappa esperado de <b>", input$kappa_est_kappa_esperado, "</b> ",

           if(input$kappa_est_k_categorias == 2){
             paste0("e probabilidades de diagnóstico positivo de <b>", input$kappa_est_prev2, "%</b> ")
           } else{
             paste0("e probabilidades de ocorrência de <b>", probs_categorias, "%</b> para as categorias <b>", categorias, "</b>, respectivamente, ")
           },

           "conforme referência de Fulano (1900),",
           " chegou-se ao tamanho de amostra total de <b>", n, "</b> unidades amostrais. Acrescentando <b>", input$kappa_est_perdas_recusa,
           "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas(n, input$kappa_est_perdas_recusa), "</b>.",


           .txt_referencia_tap, print_r_code(code)
    )


  })





  #______----
  #   ICC      ----
  #----------------------.


  # Testar ----

  output$icc_sidedUi <- renderUI({
    selectInput('icc_sided',
                'Tipo de teste de acordo com hipótese alternativa:',
                choices = c(paste0('O ICC é DIFERENTE de ', input$icc_h0),
                            paste0('O ICC é MAIOR do que ', input$icc_h0))
    ) %>% .help_buttom(body = "O teste pode ser bilateral, superior ou inferior. Nos dois útilmos casos, a hipótese alternativa é de que o parâmetro é maior ou menor do que o valor de referência, respectivamente.")
  })



  output$icc_th_formula <- renderUI({

    sinal_h0 <- case_when(input$icc_sided == paste0('O ICC é DIFERENTE de ', input$icc_h0) ~  "=",
                          TRUE ~ "\\leq")

    sinal_h1 <- case_when(input$icc_sided == paste0('O ICC é DIFERENTE de ', input$icc_h0) ~  "\\neq",
                          TRUE ~ ">")

    withMathJax(
      paste0("$$H_0: ICC ", sinal_h0, input$icc_h0,
             " \\text{  vs  } H_1: ICC ", sinal_h1, input$icc_h0, "$$"))
  })


  observeEvent(input$icc_ratings, {
    shinyFeedback::hideFeedback("icc_ratings")

    if(is.na(input$icc_ratings)){
      shinyFeedback::showFeedbackWarning(
        inputId = "icc_ratings",
        text = "Deve ser um número.",
        color = "red"
      )
    } else if(input$icc_ratings%%1 != 0){
      shinyFeedback::showFeedbackWarning(
        inputId = "icc_ratings",
        text = "Deve ser um número inteiro maior ou igual a 2.",
        color = "red"
      )
    } else if (input$icc_ratings < 2) {
      shinyFeedback::showFeedbackWarning(
        inputId = "icc_ratings",
        text = "Deve ser maior ou igual a 2.",
        color = "red"
      )
    }
  })


  eval(parse(text = warning_prop("icc_icc_esperado", entre0e1 = TRUE)))
  eval(parse(text = warning_prop("icc_h0", entre0e1 = TRUE)))
  eval(parse(text = warning_prop("icc_power")))
  eval(parse(text = warning_prop("icc_significancia")))
  eval(parse(text = warning_perdas("icc_perdas_recusa")))



  output$icc_output <- renderText({

    req(!(is.null(input$icc_sided)))


    sided   <- ifelse(input$icc_sided == paste0('O ICC é DIFERENTE de ', input$icc_h0), 2, 1)
    methods <- paste(LETTERS[1:input$icc_ratings], collapse = ", ") %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish()

    code <- paste0(
      "ICC.Sample.Size::calculateIccSampleSize(",
      "p  = ", input$icc_icc_esperado, ", ",
      "p0 = ", input$icc_h0, ", ",
      "k  = ", input$icc_ratings, ", ",
      "alpha = ", input$icc_significancia/100, ", ",
      "power = ", input$icc_power/100, ", ",
      "tails = ", sided, ")"
    )

    n <- try_n(code)
    eval(parse(text = validate_n("n")))

    n <- n[[1]]$N
    eval(parse(text = validate_n_inf("n")))


    paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
           "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

           "Foi calculado o tamanho de amostra para testar se o coeficiente de correlação intraclasse, ",
           "que avaliará o nível de concordância entre os <b>métodos/ avaliadores ", methods, "</b>, ",

           if(sided == 2){
             paste0("é diferente de <b>", input$icc_h0, "</b>")
           } else{
             paste0("é maior do que <b>", input$icc_h0, "</b>")
           },

           "</b>, por meio da ", .txt_citacao_tap, ". ",
           "Considerando nível de significância de <b>", input$icc_significancia, "%</b>, poder de <b>", input$icc_power,
           "%</b> e coeficiente de correlação intraclasse esperado de <b>", input$icc_icc_esperado, "</b>, ",
           "conforme referência de Fulano (1900),",
           " chegou-se ao tamanho de amostra total de <b>", n, "</b> unidades amostrais. Acrescentando <b>", input$icc_perdas_recusa,
           "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas(n, input$icc_perdas_recusa), "</b>.",

           .txt_referencia_tap, print_r_code(code)
    )

  })



  ## Cenarios ----

  output$cenarios_icc_thUi <- renderUI({

    fluidPage(fluidRow(
      br(),
      HTML('<hr style="color: black;">'),
      br(),br(),
      titlePanel("Construção de cenários"),
      br(),

      wellPanel(
        paste0("Utilize os argumentos abaixo para construir diferentes cenários.
                                        Você pode especificar valores de ICC desejados.
                                        Serão utilizados o nível de significância, ICC sob Ho e o número de avaliadores
                                             definidos no painel lateral."
        )
      ),

      fluidRow(
        column(6,
               textInput(inputId = "icc_power_plot",
                         label   = "Digite valores de poder (%) para fazer o gráfico",
                         value   = "80, 90, 95",
                         width   = "400px") %>%
                 .help_buttom(body = paste0("Defina os valores de poder desejado. ",
                                           "Esses valores serão utilizados para criar diferentes linhas no gráfico. Separe os valores por vírgula ',' e utilize ponto '.' como decimal."))
        )
      ),

      HTML("<b>Defina a sequência de valores para o ICC:</b>"),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
          numericInput("icc_from", "Mínimo", value = input$icc_icc_esperado, step = 0.05)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("icc_to", "Máximo", value = min(1, input$icc_icc_esperado + 0.2), step = 0.05)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("icc_by", "Intervalo", value = 0.05, min = 0, step = 0.05) %>%
            .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                        title = "Sequência")
      ),
      br(),

      plotly::plotlyOutput("icc_plot", width = "80%"),
      br(), br(),
      downloadButton("download_icc_tab","Download tabela"),
      DT::dataTableOutput("icc_tab", width = "100%")


    ))
  })

  eval(parse(text = check_text_input_to_vector("icc_power_plot")))


  tab_icc_th_cenarios <- reactive({
    poder <- text_input_to_vector(input$icc_power_plot)
    req(length(poder) > 0)

    sided   <- ifelse(input$icc_sided == paste0('O ICC é DIFERENTE de ', input$icc_h0), 2, 1)

    df_grid <- expand.grid(`Nível de significância (%)` = input$icc_significancia,
                           `Poder (%)` = poder,
                           ICC         = seq(input$icc_from, input$icc_to, input$icc_by),
                           raters      = input$icc_ratings,
                           icc_h0      = input$icc_h0,
                           tails       = sided,
                           `Hipótese alternativa` = paste0('O ICC é DIFERENTE de ', input$icc_h0),
                           stringsAsFactors = FALSE)

    df_n <- df_grid %>%
      rename(p = ICC,
             p0 = icc_h0,
             k = raters,
             power = `Poder (%)`,
             alpha = `Nível de significância (%)`) %$%
      purrr::pmap_dfr(.l = list(p, p0, k, alpha, power, tails),
                      .f = n_icc_th)

    bind_cols(df_grid, df_n) %>%
      dplyr::select(-tails) %>%
      dplyr::select(-icc_h0)
  })



  output$icc_plot <- plotly::renderPlotly({

    req(!is.null(tab_icc_th_cenarios()))
    data <- tab_icc_th_cenarios() %>%
      mutate(`Poder (%)` = factor(`Poder (%)`))

    g1 <- ggplot(data, aes(x = ICC,
                           y = n,
                           colour = `Poder (%)`))+
      geom_point() +
      geom_line() +
      xlab("ICC esperado") +
      ylab("Tamanho da amostra*") +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(palette = "Set1")



    plotly::ggplotly(g1,
                     tooltip = c("x", "colour", "y")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                showarrow = F, xref='paper', yref='paper',
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=10)))

  })




  output$icc_tab <- DT::renderDataTable({
    tab_icc_th_cenarios() %>%
      DT::datatable(extensions = c('FixedColumns'),
                rownames   = FALSE,
                filter     = "none",
                # #callback   = DT::JS("$('div.dwnld').append($('#download_auc_tab'));"),
                options    = list(pageLength = 10,
                                  scrollX = TRUE,
                                  scrollY = TRUE,
                                  searching = FALSE,
                                  fixedColumns = list(leftColumns = 1),
                                  dom = 'B<"dwnld">frtip'))
  })


  output$download_icc_tab <- downloadHandler(
    filename = function() { "Cenarios_testar_ICC.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_icc_th_cenarios(),
                                                  path = file)}
  )


  # Estimar ----




  eval(parse(text = warning_prop("icc_est_icc_esperado", entre0e1 = TRUE)))
  eval(parse(text = warning_prop("icc_est_confiança")))
  eval(parse(text = warning_numero_positivo("icc_est_amplitude")))
  eval(parse(text = warning_perdas("icc_est_perdas_recusa")))


  output$icc_est_output <- renderText({

    code <- paste0("presize::prec_icc(",
      "rho  = ", input$icc_est_icc_esperado, ", ",
      "k  = ", input$icc_est_ratings, ", ",
      "conf.level = ", input$icc_est_confiança/100, ", ",
      "conf.width = ", input$icc_est_amplitude, ")"
    )

    n <- try_n(code)
    eval(parse(text = validate_n("n")))

    methods <- paste(LETTERS[1:input$icc_est_ratings], collapse = ", ") %>% stringi::stri_replace_last_fixed(",", " e ") %>% stringr::str_trim() %>% stringr::str_squish()
    n <- n$n
    eval(parse(text = validate_n_inf("n")))


    paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
           "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

           "Foi calculado o tamanho de amostra para estimar o coeficiente de correlação intraclasse, ",
           "que avaliará o nível de concordância entre os <b>métodos/ avaliadores ", methods, "</b>, ",
           "</b> por meio da ", .txt_citacao_tap, ". ",
           "Considerando nível de confiança de <b>", input$icc_est_confiança, "%</b>, amplitude desejada de <b>", input$icc_est_amplitude,
           "</b> e coeficiente de correlação intraclasse esperado de <b>", input$icc_est_icc_esperado, "</b>, ",
           "conforme referência de Fulano (1900),",
           " chegou-se ao tamanho de amostra total de <b>", n, "</b> unidades amostrais. Acrescentando <b>", input$icc_est_perdas_recusa,
           "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas(n, input$icc_est_perdas_recusa), "</b>.",

           .txt_referencia_tap, print_r_code(code)
    )

  })





  #___________----
  # Bland Altman ----


  eval(parse(text = warning_numero_positivo("bland_amplitude")))
  eval(parse(text = warning_prop("bland_confianca")))
  eval(parse(text = warning_perdas("bland_perdas_recusa")))



  output$bland_est <- renderText({

    code <- paste0("presize::prec_lim_agree(n = NULL, ",
                   "conf.width = ", input$bland_amplitude, ", ",
                   "conf.level = ", input$bland_confianca, "/100)"
    )

    n <- eval(parse(text = code))
    eval(parse(text = validate_n("n")))

    n <- ceiling(n$n)
    eval(parse(text = validate_n_inf("n")))
    eval(parse(text = validate_n_inf("n")))



    paste0("<b><font size = '5'>Tamanho amostral calculado: ", n,
           "</font></b></br></br><i>Sugestão de texto:</i></br></br>",

           "Foi calculado o tamanho de amostra para estimar os limites de concordância do gráfico de Bland-Altman, entre os métodos <b>X</b> e <b>Y</b>, ",
           "com uma amplitude do intervalo de confiança desejada de <b>", input$bland_amplitude, "</b>, ",
           "por meio da ", .txt_citacao_tap, ". ",
           "Considerando um nível de confiança de <b>", input$bland_confianca, "%</b>, ",
           "chegou-se ao tamanho de amostra de <b>", n, "</b> sujeitos. ",
           "Acrescentando <b>", input$bland_perdas_recusa, "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>",
           n_perdas(n, input$bland_perdas_recusa), "</b>.",
           .txt_referencia_tap, print_r_code(code)


    )

  })


  ## Cenarios ----


  eval(parse(text = check_text_input_to_vector("bland_cenarios_confianca")))


  tab_bland_cenarios <- reactive({

    cenarios_confianca <- text_input_to_vector(input$bland_cenarios_confianca)
    req(length(cenarios_confianca) > 0)


    expand.grid(Amplitude = seq(from = input$bland_from, to = input$bland_to, by = input$bland_by),
                `Nível de confiança (%)` = cenarios_confianca) %>%
      mutate(`Tamanho da amostra` = mapply(
        function(conf.width, conf.level){ presize::prec_lim_agree(n = NULL, conf.width = conf.width, conf.level = conf.level/100)$n },
        Amplitude, `Nível de confiança (%)`),
        `Tamanho da amostra`   = ceiling(`Tamanho da amostra`),
        `% de perdas/ recusas` = input$bland_perdas_recusa,
        `n + perdas/ recusas`  = n_perdas(`Tamanho da amostra`, input$bland_perdas_recusa))
  })



  output$bland_cenarios_plot <- plotly::renderPlotly({


    g1 <- tab_bland_cenarios() %>%
      mutate(`Nível de confiança (%)` = factor(`Nível de confiança (%)`)) %>%
      ggplot(aes(x = Amplitude, y = `Tamanho da amostra`, color = `Nível de confiança (%)`))+
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = seq(from = input$bland_from, to = input$bland_to, by = input$bland_by)) +
      xlab("Amplitude") +
      ylab("Tamanho da amostra*") +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(palette = "Set1")

    plotly::ggplotly(g1, tooltip = c("x", "colour", "y")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = "* sem considerar perdas/ recusas.",
                                        showarrow = F, xref='paper', yref='paper',
                                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                        font=list(size=10)))
  })



  output$bland_cenarios_tab <- DT::renderDataTable({


    tab_bland_cenarios() %>%
      DT::datatable(extensions = c('FixedColumns'),
                    rownames   = FALSE,
                    filter     = "none",
                    options    = list(pageLength = 10,
                                      scrollX = TRUE,
                                      scrollY = TRUE,
                                      searching = FALSE,
                                      fixedColumns = list(leftColumns = 1),
                                      dom = 'B<"dwnld">frtip'))
  })


  output$bland_cenarios_download <- downloadHandler(
    filename = function() { "Cenarios_tamanho_amostra_bland.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_bland_cenarios(), path = file)}
  )





}# fecha o server !!!!
