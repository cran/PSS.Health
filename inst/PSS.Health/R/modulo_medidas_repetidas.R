
mod_medidas_repetidas_Ui <- function(id){

  ns <- NS(id)

  tagList(

    uiOutput(ns("medidas_repetidas_ui_sided")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_medidas_repetidas_server <- function(id, tipo, txt_ajuda,
                                         translation_pss, linguagem, .rodape, try_n, validate_n, ajuda_cenarios_multiplos_valores, validate_n_inf, n_perdas, print_r_code, text_input_to_vector, check_text_input_to_vector,
                                         warning_prop, warning_numero_positivo, warning_inteiro, warning_perdas, warning_numero

){
  shiny::moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns




      # Modal de mudar nomes ----

      observeEvent(input$show_th_rep, {
        showModal(
          modalDialog(
            title = translation_pss("Ajustes", linguagem()),
            fluidPage(

              HTML("<b>Preencha os campos abaixo de acordo com seu estudo para que sirvam de guia no preenchimento dos demais campos</b>."),
              br(), br(),
              textInput(inputId = ns("rep_nome_desfecho"),
                        label   = translation_pss("Descreva o nome do desfecho", linguagem()),
                        value   = ifelse(input$show_th_rep == 0, "Y", rep_nome_desfecho())),
              HTML("<i>", gsub("<br><br>", "", txt_ajuda()$txt_desfecho), "</i>"),
              br(), br(),
              textInput(inputId = ns("rep_grupoTratamento"),
                        label   = translation_pss("Descreva um nome para o grupo Tratamento", linguagem()),
                        value   = ifelse(input$show_th_rep == 0, translation_pss("Tratamento", linguagem()), rep_grupoTratamento())),

              HTML("<i>Em alguns estudos o grupo Tratamento também pode ser chamado de grupo Intervenção ou grupo Exposto.</i><br><br>"),

              textInput(inputId = ns("rep_grupoControle"),
                        label   = translation_pss("Descreva um nome para o grupo Controle", linguagem()),
                        value   = ifelse(input$show_th_rep == 0, translation_pss("Controle", linguagem()), rep_grupoControle())),

              HTML("<i>Em alguns estudos o grupo Controle também pode ser chamado de grupo Placebo/ Sham ou grupo Não exposto.</i>")


            ),
            easyClose = TRUE,
            footer    = NULL
          )
        )
      })



      rep_grupoControle <- reactive({
        ifelse(is.null(input$rep_grupoControle), translation_pss("Controle", linguagem()), input$rep_grupoControle)
      })

      rep_grupoTratamento <- reactive({
        ifelse(is.null(input$rep_grupoTratamento), translation_pss("Tratamento", linguagem()), input$rep_grupoTratamento)
      })

      rep_nome_desfecho <- reactive({
        ifelse(is.null(input$rep_nome_desfecho), "Y", input$rep_nome_desfecho)
      })



      # Formulas do TH ----
      output$rep_formula1 <- renderUI({
        withMathJax(
          paste0("$$H_0: \\mu_{", rep_grupoTratamento(), "} = \\mu_{", rep_grupoControle(), "} $$"))
      })

      output$rep_formula2 <- renderUI({
        withMathJax(
          paste0("$$H_1: \\mu_{", rep_grupoTratamento(), "} \\neq \\mu_{", rep_grupoControle(), "} $$"))
      })



      # CHECK inputs ----

      eval(parse(text = warning_inteiro("rep_n_tempos")))

      eval(parse(text = warning_numero("rep_dif_medias")))

      eval(parse(text = warning_numero_positivo("rep_sigma1")))
      eval(parse(text = warning_numero_positivo("rep_sigma2")))
      eval(parse(text = warning_numero_positivo("rep_ratio")))

      eval(parse(text = warning_prop("rep_ar1_rho1", entre0e1 = TRUE)))
      eval(parse(text = warning_prop("rep_ar1_rho2", entre0e1 = TRUE)))

      eval(parse(text = warning_prop("rep_cs_rho1", entre0e1 = TRUE)))
      eval(parse(text = warning_prop("rep_cs_rho2", entre0e1 = TRUE)))

      eval(parse(text = warning_prop("rep_power")))
      eval(parse(text = warning_prop("rep_sig")))
      eval(parse(text = warning_perdas("rep_perdas_recusa")))






      # Ui matrizes de trabalho ----

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
        new_value <- gsub(",", ".", as.character(cell$value)) %>%
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
        new_value <- gsub(",", ".", as.character(cell$value)) %>%
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

        req(input$rep_tipo_matriz_cor == 'Não estruturada')

        fluidPage(fluidRow(wellPanel(

          # Tratamento
          if(rep_problemas_matriz_correlacao1() != ""){
            HTML(paste0('<font size = "+0.1"><font color = "red">', "<b>Matriz de correlação do grupo ", rep_grupoTratamento(), ":</b>", '</font></font><br/>'))
          } else{
            HTML(paste0("<b>Matriz de correlação do grupo ", rep_grupoTratamento()), ":</b>")
          },

          DT::dataTableOutput(ns("rep_cor_grupo1")),

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

          DT::dataTableOutput(ns("rep_cor_grupo2")),

          if(rep_problemas_matriz_correlacao2() != ""){
            HTML(rep_problemas_matriz_correlacao2())
          },




          br(), br()
        )))
      })



      output$rep_retencao1 <- renderUI({
        len <- as.integer(input$rep_n_tempos)
        lapply(2:len, function(i) {
          div(
            numericInput(
              inputId = ns(paste0("retencao1_", i)),
              label = paste0("Momento ", i),
              value = 100 + 1 - i
            )
          )
        })
      })


      output$rep_retencao2 <- renderUI({
        len <- as.integer(input$rep_n_tempos)
        lapply(2:len, function(i) {
          div(
            numericInput(
              inputId = ns(paste0("retencao2_", i)),
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

        req(!is.null(input[[ns(paste0("retencao1_", len))]]))

        lapply(2:len, function(i) {
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

        lapply(2:len, function(i) {
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







      # Ui input e tamanho ----

      matrizes_correlacao <- reactive({
        tipos <- c("AR(1)",
                   "Componente permutável",
                   "Não estruturada")

        if (linguagem() == "pt") {
          names(tipos) <- tipos
        } else {
          names(tipos) <- c("AR(1)", "Exchangeable", "Unstructured")
        }

        tipos

      })

      output$chamada_matriz_correlacao <- renderUI({
        selectInput(
          ns('rep_tipo_matriz_cor'),
          translation_pss("Selecione o tipo de matriz de correlação", linguagem()),
          choices = matrizes_correlacao(),
          selected = 'AR(1)'
        ) %>%
          .help_buttom(
            body = txt_ajuda()$wellPanel_txt_matriz_correlacao,
            title = translation_pss("Matriz de correlação", linguagem())
          )
      })

      output$tipo_matriz_correlcao <- renderUI({

        req(!is.null(input$rep_tipo_matriz_cor))

        if (input$rep_tipo_matriz_cor == 'Não estruturada') {
          HTML("<i><b>ATENÇÃO!</b> Edite as matrizes de correlação no painel principal ao lado. As matrizes são editáveis, basta clicar duas vezes sobre a célula --> </i><br><br>")

        } else if (input$rep_tipo_matriz_cor == 'AR(1)') {
          fluidPage(fluidRow(
            HTML(paste0("<b><font size = '2.95'>Parâmetro autorregressivo no grupo</font></b><br>")),
            div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                numericInput(ns("rep_ar1_rho1"),
                             rep_grupoTratamento(),
                             value = 0.4,
                             min = 0,
                             max = 1,
                             step = .1
                )
            ),
            div(style = "display: inline-block;vertical-align:top; width: 49%;",
                numericInput( ns("rep_ar1_rho2"),
                              rep_grupoControle(),
                              value = 0.2,
                              min = 0,
                              max = 1,
                              step = .1
                )
            )
          ))
        } else {

          fluidPage(fluidRow(
            HTML(paste0("<b><font size = '2.95'>Valor da correlação entre as medidas no grupo</font></b><br>")),
            div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                numericInput( ns("rep_cs_rho1"),
                              rep_grupoTratamento(),
                              value = 0.7,
                              min = -1,
                              max = 1,
                              step = .1
                )
            ),
            div(style = "display: inline-block;vertical-align:top; width: 49%;",
                numericInput(ns( "rep_cs_rho2"),
                             rep_grupoControle(),
                             value = 0.7,
                             min = -1,
                             max = 1,
                             step = .1
                )
            )
          ))
        }
      })




      output$medidas_repetidas_ui_sided <- renderUI({

        fluidPage(fluidRow(
          sidebarLayout(
            sidebarPanel(
              wellPanel(HTML('<b><a href="https://youtu.be/oErZbk_cpWY" target="_blank">Vídeo: PSS Health para comparar duas médias</a></b><br>')),

              wellPanel(
                HTML(
                  paste0(
                    "<b><font size = '2.8'> ", translation_pss("Hipóteses a serem testadas", linguagem()), "*</font></b>"
                  )
                ),
                uiOutput(ns("rep_formula1")),
                uiOutput(ns("rep_formula2")),
                HTML("<i><font size = '2.8'>*Resposta média no último tempo</font></i>")
              ),

              actionLink(ns("show_th_rep"), translation_pss("Mudar nomes", linguagem())),
              br(), br(),

              numericInput( ns("rep_n_tempos"),
                            "Número de momentos a ser avaliado",
                            value = 4,
                            min = 2,
                            max = Inf,
                            step = 1
              ),

              numericInput(inputId = ns("rep_dif_medias"),
                           label = paste0(
                             translation_pss("Diferença mínima a ser detectada", linguagem()),
                             " (",
                             translation_pss("em", linguagem()), " ",
                             translation_pss("u.m.", linguagem()),
                             ")"
                           ),
                           value = 0.5,
                           min = -Inf,
                           max = Inf,
                           step = .5
              ) %>% .help_buttom(body = txt_ajuda()$txt_diferenca_clinica, title = translation_pss("Diferença mínima a ser detectada", linguagem())),

              if (tipo == "poder") {

                fluidPage(fluidRow(
                  HTML(paste0("<b><font size = '2.95'>Tamanho amostral do grupo no último momento</font></b><br>")),
                  div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                      numericInput( ns("n1"),
                                    rep_grupoTratamento(),
                                    value = 50,
                                    min = 0,
                                    max = Inf,
                                    step = 1
                      )
                  ),
                  div(style = "display: inline-block;vertical-align:top; width: 49%;",
                      numericInput( ns("n2"),
                                    rep_grupoControle(),
                                    value = 60,
                                    min = 0,
                                    max = Inf,
                                    step = 1
                      ) %>% .help_buttom(body = "Tamanho amostral", title = "Tamanho amostral")
                  )
                ))

              },



              # Desvio padrao
              HTML(paste0("<b><font size = '2.95'>Desvio padrão esperado de ", rep_nome_desfecho(), " no grupo</font></b><br>")),
              div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                  numericInput( ns("rep_sigma1"),
                                rep_grupoTratamento(),
                                value = 1,
                                min = 0,
                                max = Inf,
                                step = 1
                  )
              ),
              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                  numericInput( ns("rep_sigma2"),
                                rep_grupoControle(),
                                value = 1,
                                min = 0,
                                max = Inf,
                                step = 1
                  ) %>% .help_buttom(body = txt_ajuda()$txt_dp, title = translation_pss("Desvio padrão", linguagem()))
              ),

              wellPanel(
                uiOutput(ns("chamada_matriz_correlacao")),
                uiOutput(ns("tipo_matriz_correlcao"))
              ),




              # Retencao
              # HTML('<hr style="color: black;">'),
              # br(),
              wellPanel(
                HTML(paste0("<b>Retenção esperada (%) no grupo</b><br><br>"))  %>%
                  .help_buttom(body = paste0("Percentual de indivíduos que é esperado ter naquele momento. ", txt_ajuda()$txt_definido_pesquisador_OU_literatura),
                               title = "Retenção esperada"),
                fluidPage(fluidRow(
                  div(style = "display: inline-block;vertical-align:top; width: 49%;",
                      wellPanel(
                        HTML(paste0("<b>", rep_grupoTratamento(), ":</b>")),
                        br(), br(),
                        uiOutput(ns("rep_retencao1"))
                      )),
                  div(style = "display: inline-block;vertical-align:top; width: 49%;", wellPanel(
                    HTML(paste0("<b>", rep_grupoControle(), ":</b>")),
                    br(), br(),
                    uiOutput(ns("rep_retencao2"))
                  ))
                ))
              ),

              if (tipo == "tamanho_amostral") {
                fluidPage(fluidRow(

                  numericInput( ns("rep_ratio"),
                                paste0("Balanceamento (", rep_grupoTratamento(), ":", rep_grupoControle(), ")"),
                                value = 1,
                                min = 0,
                                max = Inf,
                                step = 0.5
                  ) %>% .help_buttom(body = txt_ajuda()$txt_balanceamento, title = "Balanceamento"),

                  numericInput( ns("rep_power"),
                                "Poder (%)",
                                value = 80,
                                min = 0,
                                max = 100,
                                step = 1
                  ) %>% .help_buttom(body = txt_ajuda()$txt_power, title = "Poder (%)")
                ))
              },

              numericInput( ns("rep_sig"),
                            "Nível de significância (%)",
                            value = 5,
                            min = 0,
                            max = 100,
                            step = 1
              ) %>% .help_buttom(body = txt_ajuda()$txt_significancia, title = "Nível de significância (%)"),
              numericInput( ns("rep_perdas_recusa"),
                            "Perdas/ Recusa (%)",
                            value = 10,
                            min = 0,
                            max = 100,
                            step = 1
              ) %>% .help_buttom(body = txt_ajuda()$txt_perdas_recusas, title = "Perdas/ Recusas (%)")
            ),

            mainPanel(

              # Input da matriz de correlacao nao estruturada
              uiOutput(ns("rep_print_matriz_cor")) %>%
                shinycssloaders::withSpinner(type = 5),

              # Texto do tamanho amostral
              shinycssloaders::withSpinner(htmlOutput(ns("rep_outout_text")), type = 5),

              # Imprimi as matrizes de correlacao
              shinycssloaders::withSpinner(uiOutput(ns("rep_corr_matrix_out")), type = 5),

              # Codigo utilizado
              shinycssloaders::withSpinner(htmlOutput(ns("rep_out_codigo")), type = 5),

              # Cenarios
              uiOutput(ns("cenarios_medidas_rep_thUi"))

            )
          )
        ))
      })




      # Aqui eh definido a matriz que sera utilizada no calculo -----.

      correlation_Ra <- reactive({

        req(!is.null(input$rep_tipo_matriz_cor))
        # req(!is.null(input$tipo_matriz_correlcao))

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

        } else if (input$rep_tipo_matriz_cor == 'AR(1)'){
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
        req(!is.null(input$retencao1_2))
        rete <- lapply(2:input$rep_n_tempos, function(i) {
          input[[paste0("retencao1_", i)]]
        }) %>% unlist()

        c(100, rete)
      })

      rep_retencao_B <- reactive({
        req(!is.null(input$retencao2_2))

        rete <- lapply(2:input$rep_n_tempos, function(i) {
          input[[paste0("retencao2_", i)]]
        }) %>% unlist()

        c(100, rete)
      })





      rep_n_calc <- reactive({
        req(!is.null(input$rep_n_tempos))
        req(rep_problemas_matriz_correlacao1() == "")

        if (tipo == "tamanho_amostral"){

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
        } else {


          n <- longpower::power.mmrm(
            N = input$n1 + input$n2,
            Ra = matrix(data = correlation_Ra(), nrow = input$rep_n_tempos, byrow = TRUE),
            ra = rep_retencao_A()/100,
            sigmaa = input$rep_sigma1,
            Rb = matrix(data = correlation_Rb(), nrow = input$rep_n_tempos, byrow = TRUE),
            rb = rep_retencao_B()/100,
            sigmab = input$rep_sigma2,
            delta = input$rep_dif_medias,
            lambda = input$n1/input$n2,
            sig.level = input$rep_sig/100
          )
        }

        validate(need(!is.na(n$n1), 'Não foi possível calcular sua solicitação. Verifique as entradas no painel lateral. Se o erro persistir, por favor, envie um e-mail para l-bioestatistica@hcpa.edu.br.'))
        n
      })


      rep_erro_inputs <- reactive({
        req(!is.null(input$rep_n_tempos))

        erros <- input$rep_n_tempos != sqrt(length(correlation_Ra())) |
          input$rep_n_tempos != sqrt(length(correlation_Rb())) |
          !all(diff(rep_retencao_A()) <= 0) | !all(rep_retencao_A() > 0) | !all(rep_retencao_A() <= 100) |
          !all(diff(rep_retencao_B()) <= 0) | !all(rep_retencao_B() > 0) | !all(rep_retencao_B() <= 100) |
          input$rep_sigma1 <= 0 |
          input$rep_sigma2 <= 0  |
          input$rep_sig <= 0 | input$rep_sig >= 100 |
          input$rep_ar1_rho2 <= 0 | input$rep_ar1_rho2 >= 1 |
          rep_problemas_matriz_correlacao1() != "" |
          input$rep_n_tempos%%1 != 0 | input$rep_n_tempos < 1

        if (tipo == "tamanho_amostral") {
          erros |
            input$rep_ratio <= 0 |
            input$rep_power <= 0 | input$rep_power >= 100
        } else {
          erros |
            input$n1 <= 0 |
            input$n2 <= 0
        }

      })


      eval(parse(text = warning_inteiro("rep_n_tempos")))


      # Render output ----


      output$rep_outout_text <- renderText({

        validate(need(!rep_erro_inputs(), "Não foi possível calcular sua solicitação. Verifique as entradas no painel lateral. Se o erro persistir, por favor, envie um e-mail para l-bioestatistica@hcpa.edu.br."))


        if (tipo == "tamanho_amostral"){
          n  <- rep_n_calc()
          n1 <- ceiling(n$n1)
          n2 <- ceiling(n$n2)
          n  <- n1 + n2

          nperdas1 <- n_perdas(n1, input$rep_perdas_recusa)
          nperdas2 <- n_perdas(n2, input$rep_perdas_recusa)
        } else {
          poder  <- rep_n_calc()
          poder <- round(poder$power*100, 1)
        }

        retencaoA <- paste0(rep_retencao_A()[-1], "%", collapse = ", ") %>%
          stringi::stri_replace_last_fixed(",", " e ") %>%
          stringr::str_trim() %>% stringr::str_squish()

        retencaoB <- paste0(rep_retencao_B()[-1], "%", collapse = ", ") %>%
          stringi::stri_replace_last_fixed(",", " e ") %>%
          stringr::str_trim() %>% stringr::str_squish()

        momentos <- paste0(1:input$rep_n_tempos, collapse = ", ") %>%
          stringi::stri_replace_last_fixed(",", " e ") %>%
          stringr::str_trim() %>% stringr::str_squish()

        momentos2 <- paste0(2:input$rep_n_tempos, collapse = ", ") %>%
          stringi::stri_replace_last_fixed(",", " e ") %>%
          stringr::str_trim() %>% stringr::str_squish()


        if (tipo == "tamanho_amostral") {
          cabecalho <-  paste0(
            "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
            if (n1 != n2) {
              paste0(
                " (<i>", n1, " ", rep_grupoTratamento(), " e ", n2, " ", rep_grupoControle(), "</i>)"
              )
            } else {
              paste0(
                " (<i>", n1, " para cada grupo</i>)"
              )
            },
            "</font></b></br></br><i>", translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",



            "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos ",
            if (n1 != n2) {
              paste0(
                "(", n1, " no grupo ", rep_grupoTratamento(), " e ", n2, " no grupo ", rep_grupoControle(), ") "
              )
            } else {
              paste0(
                "(", n1, " para cada grupo) "
              )
            },


            "para testar se existe uma diferença mínima de <b>", input$rep_dif_medias, " u.m.</b> entre as médias de <b>",
            rep_nome_desfecho(), "</b> dos grupos <i>", rep_grupoTratamento(), "</i> e <i>", rep_grupoControle(), "</i> ",
            "no último momento (momento <b>", input$rep_n_tempos, "</b>) do estudo de medidas repetidas ",


            if (n1 == n2) {
              paste0("(com o acréscimo de <b>", input$rep_perdas_recusa, "%</b> para possíveis perdas e recusas este número deve ser <b>", nperdas1 + nperdas2, "</b>). ")
            } else {
              paste0("(com o acréscimo de <b>", input$rep_perdas_recusa, "%</b> para possíveis perdas e recusas este número deve ser ", nperdas1, " ", nome_grupo_tratamento(), " e ", nperdas2, " ", nome_grupo_controle(), "). ")
            },

            "O cálculo considerou poder de <b>", input$rep_power, "%</b>, nível de significância de <b>", input$rep_sig, "%</b> "
          )



          # Poder
        } else {

          cabecalho <-  paste0(
            "<b><font size = '5'>Poder calculado: ", poder, "%</font></b></br></br><i>Sugestão de texto:</i></br></br>",

            "O poder para testar se existe uma diferença mínima de <b>", input$rep_dif_medias, " u.m.</b> entre as médias de <b>",
            rep_nome_desfecho(), "</b> dos grupos <i>", rep_grupoTratamento(), "</i> e <i>", rep_grupoControle(), "</i> ",
            "no último momento (momento <b>", input$rep_n_tempos, "</b>) do estudo de medidas repetidas é <b>", poder, "%</b>. ",

            "Este valor foi obtido considerando nível de significância de <b>", input$rep_sig, "</b>%, ",
            "tamanho amostral no momento ", input$rep_n_tempos, " de <b>",

            if (input$n1 == input$n2) {
              paste0(
                input$n1, "</b> de sujeitos em cada grupo "
              )
            } else {
              paste0(
                input$n1, "</b> e <b>", input$n2, "</b> sujeitos para o grupo <i>", rep_grupoTratamento(), "</i> e <i>", rep_grupoControle(), "</i>",
                ", respectivamente,"
              )
            }

          )
        }


        elementos <- paste0(
          "e os elementos descritos a seguir para o grupo <i>", rep_grupoTratamento(), "</i>: ",
          "<ul>", # inicio da lista
          "<li> desvio padrão de <b>", input$rep_sigma1, " u.m.</b>,</li>",
          "<li> retenção de ", retencaoA, " nos momentos ", momentos2,", respectivamente,</li>",

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

          "e para o grupo ", rep_grupoControle(), " (dados de Fulano (1900)):",
          "<ul>", # inicio da lista
          "<li> desvio padrão de <b>", input$rep_sigma2, " u.m.</b>,</li>",
          "<li> retenção de ", retencaoB, " nos momentos ", momentos2,", respectivamente,</li>",

          if(input$rep_tipo_matriz_cor == 'Não estruturada'){
            paste0("<li> matriz de correlação não estruturada (definida abaixo),</li>")
          } else if(input$rep_tipo_matriz_cor == 'AR(1)'){
            paste0("<li> matriz de correlação autorregressiva de parâmetro ",
                   input$rep_ar1_rho2, " (definida abaixo),</li>")
          } else if(input$rep_tipo_matriz_cor == 'Componente permutável'){
            paste0("<li> matriz de correlação permutável com correlação ",
                   input$rep_cs_rho2, " (definida abaixo)</li>")
          },
          "</ul>"
        )


        paste0(
          cabecalho,
          elementos,
          .txt_citacao_pss
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


      # Codigo ----

      output$rep_out_codigo <- renderText({

        req(!rep_erro_inputs())

        if (tipo == "tamanho_amostral") {
          code <- paste0(
            "longpower::power.mmrm(",
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
        } else {
          code <- paste0(
            "longpower::power.mmrm(",
            "N = ", input$n1, " + ", input$n2, ", ",
            "Ra = Ra, ",
            "ra = c(", paste0(rep_retencao_A(), collapse = ", "), ")/100, ",
            "sigmaa = ", input$rep_sigma1,  ", ",
            "Rb = Rb, ",
            "rb = c(", paste0(rep_retencao_B(), collapse = ", "), ")/100, ",
            "sigmab = ", input$rep_sigma2, ", ",
            "delta = ", input$rep_dif_medias, ", ",
            "lambda = ", input$n1, "/ ", input$n2, ", ",
            "sig.level = ", input$rep_sig, "/100)"
          )

        }


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
        req(tipo == "tamanho_amostral")

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

          HTML(
            "<b>",
            translation_pss("Defina a sequência de valores para a diferença a ser detectada", linguagem()),
            ":</b>"
          ),
          br(),
          div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
              numericInput(ns("rep_th_from"), "Mínimo", value = dif_start, step = 1)
          ),
          div(style = "display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("rep_th_to"), "Máximo", value = dif_end, step = 1)
          ),
          div(style = "display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("rep_th_by"), "Intervalo", value = dif_by, min = 0, step = 0.5) %>%
                .help_buttom(body = "Defina a sequência de margem de erro absoluta. Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                             title = "Sequência da margem de erro absoluta")
          ),

          fluidRow(
            column(6,
                   textInput(inputId = ns("rep_sd_plot"),
                             label   = paste0("Digite valores de desvio padrão (DP) do grupo ", rep_grupoTratamento(), " para fazer o gráfico:"),
                             value   = paste0(c(input$rep_sigma1, input$rep_sigma1 + 0.2, input$rep_sigma1 + 0.5), collapse = ", "),
                             width   = "100%") %>%
                     .help_buttom(body = "Defina os valores de desvio padrão.
                                                      Esses valores serão utilizados para criar diferentes linhas no gráfico.
                                                      Separe os valores por vírgula ',' e utilize ponto '.' como decimal.")
            )
          ),

          plotly::plotlyOutput(ns("rep_th_plot"), width = "80%") %>%
            shinycssloaders::withSpinner(type = 5),
          br(), br(),
          downloadButton(ns("download_rep_th_tab"),"Download tabela"),

          DT::dataTableOutput(ns("rep_th_tab"), width = "100%") %>%
            shinycssloaders::withSpinner(type = 5)

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

        grid %>%
          mutate(
            `n Controle` = mapply(
              function(delta, sigmaa, sigmab, sig.level, power, lambda){
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

                  n$n2
                },
                warning = function(warning_condition) { NA },
                error = function(error_condition) { NA })
              }, `Diferença a ser detectada`,
              `Desvio padrão 1`,
              `Desvio padrão 2`,
              `Nível de significância (%)`,
              `Poder (%)`,
              `Balanceamento`
            ),

            `n Tratamento` = `n Controle`*Balanceamento,

            dplyr::across(
              c(`n Controle`, `n Tratamento`),
              ceiling
            ),
            `Tamanho da amostra` = `n Tratamento` + `n Controle`,
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
                     `n Controle` = `n Controle`)) +
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



    }
  )

}
