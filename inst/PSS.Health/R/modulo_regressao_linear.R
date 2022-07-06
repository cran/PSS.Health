
mod_regressao_linear_Ui <- function(id) {

  ns <- NS(id)

  tagList(

    uiOutput(ns("aba")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_regressao_linear_server <- function(id, tipo = "tamanho_amostral", txt_ajuda,
                                        translation_pss, linguagem, .rodape, try_n, validate_n, ajuda_cenarios_multiplos_valores, validate_n_inf, n_perdas, print_r_code, text_input_to_vector, check_text_input_to_vector,
                                        warning_prop, warning_numero_positivo, warning_inteiro, warning_perdas, warning_numero) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      eval(parse(text = warning_prop("r2", entre0e1 = TRUE)))
      eval(parse(text = warning_numero("beta")))
      eval(parse(text = warning_numero_positivo("sigmaX")))
      eval(parse(text = warning_numero_positivo("sigmaY")))

      eval(parse(text = warning_numero_positivo("amplitude")))
      eval(parse(text = warning_prop("poder")))
      eval(parse(text = warning_prop("alpha")))
      eval(parse(text = warning_prop("confianca")))
      eval(parse(text = warning_perdas("perc_perdas")))

      eval(parse(text = warning_inteiro("n")))


      output$aba <- renderUI({
        sidebarLayout(
          sidebarPanel(

            if (tipo %in% c("tamanho_amostral", "poder")) {
              tagList(
                wellPanel(
                  HTML(
                    paste0(
                      "<b><font size = '2.8'> ", translation_pss("Hipóteses a serem testadas", linguagem()), " </font></b>"
                    )
                  ),
                  withMathJax(paste0("$$H_0: \\beta_{", nome_preditora(), "} = 0$$")),
                  withMathJax(paste0("$$H_1: \\beta_{", nome_preditora(), "} \\neq 0$$"))
                ),

                checkboxInput(
                  inputId = ns("usar_r2"),
                  label   = "Calcular com base no valor do R²",
                  value = FALSE
                )
              )
            },

            actionLink(ns("mudar_nomes"), translation_pss("Mudar nomes", linguagem())),
            br(), br(),

            if (tipo == "estimar") {
              numericInput( ns("amplitude"),
                            translation_pss("Amplitude do intervalo", linguagem()),
                            value = 0.1,
                            min = 0,
                            step = 0.1
              ) %>% .help_buttom(body = txt_ajuda()$txt_amplitude, title = translation_pss("Amplitude do intervalo", linguagem()))
            },

            uiOutput(ns("usar_r2Ui")),

            if (tipo %in% c("tamanho_amostral")) {
              numericInput( ns("poder"),
                            translation_pss("Poder (%)", linguagem()),
                            value = 80,
                            min = 0,
                            max = 100,
                            step = 1
              ) %>% .help_buttom(body = txt_ajuda()$txt_power, title = translation_pss("Poder (%)", linguagem()))
            } else if (tipo == "poder") {
              numericInput( ns("n"),
                            translation_pss("Tamanho amostral", linguagem()),
                            value = 200,
                            min = 4,
                            step = 1
              )
            },

            if (tipo %in% c("tamanho_amostral", "poder")) {
              numericInput( ns("alpha"),
                            translation_pss("Nível de significância (%)", linguagem()),
                            value = 5,
                            min = 0,
                            max = 100,
                            step = 1
              ) %>% .help_buttom(body = txt_ajuda()$txt_significancia, title = translation_pss("Nível de significância (%)", linguagem()))
            } else {
              tagList(
                numericInput( ns("confianca"),
                              translation_pss("Nível de confiança (%)", linguagem()),
                              value = 5,
                              min = 0,
                              max = 100,
                              step = 1
                ) %>% .help_buttom(body = txt_ajuda()$txt_confianca, title = translation_pss("Nível de confiança (%)", linguagem()))
              )
            },

            if (tipo %in% c("tamanho_amostral", "estimar")) {
              numericInput( ns("perc_perdas"),
                            translation_pss("Perdas/ Recusas (%)", linguagem()),
                            value = 10,
                            min = 0,
                            max = 100,
                            step = 1
              ) %>% .help_buttom(body = txt_ajuda()$txt_perdas_recusas, title = translation_pss("Perdas/ Recusas (%)", linguagem()))
            }
          ),

          mainPanel(
            htmlOutput(ns("texto_principal")) |>
              shinycssloaders::withSpinner(type = 5),

            uiOutput(ns("cenarios"))

          )
        )
      })




      # Mudar nomes -----

      observeEvent(input$mudar_nomes, {
        showModal(
          modalDialog(
            title = translation_pss("Ajustes", linguagem()),
            fluidPage(

              HTML(translation_pss("<b>Preencha os campos abaixo de acordo com seu estudo para que sirvam de guia no preenchimento dos demais campos</b>.", linguagem())),
              br(), br(),
              textInput(inputId = ns("nome_desfecho"),
                        label   = translation_pss("Descreva o nome do desfecho", linguagem()),
                        value   = ifelse(input$mudar_nomes == 0, "Y", nome_desfecho())),
              HTML("<i>", gsub("<br><br>", "", txt_ajuda()$txt_desfecho), "</i>"),
              br(), br(),

              textInput(inputId = ns("nome_preditora"),
                        label   = translation_pss("Descreva o nome da variável preditora", linguagem()),
                        value   = ifelse(input$mudar_nomes == 0, "X", nome_preditora()))

            ),
            easyClose = TRUE,
            footer    = NULL
          )
        )
      })


      nome_desfecho <- reactive({
        ifelse(is.null(input$nome_desfecho), "Y", input$nome_desfecho)
      })

      nome_preditora <- reactive({
        ifelse(is.null(input$nome_preditora), "X", input$nome_preditora)
      })





      # Correlacao ou R²? -----

      output$usar_r2Ui <- renderUI({

        if (input$usar_r2) {
          numericInput( ns("r2"),
                        translation_pss("Coeficiente de determinação esperado", linguagem()),
                        value = 0.2,
                        min = 0,
                        max = Inf,
                        step = 1
          ) %>% .help_buttom(
            body = txt_ajuda()$txt_coef_determinacao,
            title = translation_pss("Coeficiente de determinação esperado", linguagem())
          )

        } else {

          tagList(
            numericInput(ns("beta"),
                         translation_pss("Coeficiente de correlação esperado", linguagem()),
                         value = .8,
                         step = 1
            ) %>% .help_buttom(
              body = txt_ajuda()$txt_correlacao,
              title = translation_pss("Coeficiente de correlação esperado", linguagem()
              )
            ),
            numericInput( ns("sigmaY"),
                          paste0(
                            translation_pss("Desvio padrão esperado de", linguagem()),
                            " ",
                            nome_desfecho()
                          ),
                          value = 0.5,
                          min = 0,
                          max = Inf,
                          step = 1
            ) %>% .help_buttom(body = txt_ajuda()$txt_dp, title = translation_pss("Desvio padrão esperado", linguagem())),
            numericInput(ns("sigmaX"),
                         paste0(
                           translation_pss("Desvio padrão esperado de", linguagem()),
                           " ",
                           nome_preditora()
                         ),
                         value = 0.2,
                         min = 0,
                         max = Inf,
                         step = 1
            ) %>% .help_buttom(body = txt_ajuda()$txt_dp, title = translation_pss("Desvio padrão esperado", linguagem()))
          )
        }
      })





      # Texto ----

      output$texto_principal <- renderText({


        if (tipo == "tamanho_amostral") {

          code <- ifelse(!input$usar_r2,
                         paste0(
                           "powerMediation::ss.SLR(",
                           "power = ", input$poder, "/100, ",
                           "lambda.a = ", input$beta, ", ",
                           "sigma.x = ", input$sigmaX, ", ",
                           "sigma.y = ", input$sigmaY, ", ",
                           "alpha = ", input$alpha, "/100, ",
                           "verbose = FALSE)"
                         ),
                         paste0(
                           "powerMediation::ss.SLR.rho(",
                           "power = ", input$poder, "/100, ",
                           "alpha =",  input$alpha, "/100, ",
                           "rho2 = ",  input$r2, ", ",
                           "verbose = FALSE)"
                         )
          )


          n <- try_n(code)
          eval(parse(text = validate_n("n")))
          n <- ceiling(n$n)
          eval(parse(text = validate_n_inf("n")))


          paste0("<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
                 "</font></b></br></br><i>", translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",


                 "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos ",
                 "para testar se o coeficiente de regressão de <i>", nome_preditora(), "</i> ao regredir <i>", nome_desfecho(),
                 "</i> é diferente de 0 ",
                 "(com o acréscimo de ", input$perc_perdas, "% para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$perc_perdas), "</b>). ",
                 "O cálculo considerou um poder de <b>", input$poder, "%</b>, nível de significância de <b>", input$alpha, "%</b>",
                 if (input$usar_r2) {
                   paste0(
                     " e coeficiente de determinação esperado de <b>", input$r2, "</b> ",
                     "como é referido em Fulano (1900) OU escolha do pesquisador ."
                   )
                 } else {
                   paste0(
                     ", coeficiente de regressão esperado de <b>", input$beta, "</b>, ",
                     " desvios padrões esperados de <b>", input$sigmaX, "</b> e <b>", input$sigmaY, "</b> ",
                     translation_pss("u.m.", linguagem()),
                     "para <i>", nome_preditora(), "</i> e <i>", nome_desfecho(), "</i>",
                     ", respectivamente, ",
                     "como são referidos em Fulano (1900). "
                   )
                 },
                 .txt_citacao_pss,
                 .txt_referencia_tap,
                 print_r_code(code)
          )



          # Se for variavel categorica!
        } else if (tipo == "poder") {


          code <- ifelse(!input$usar_r2,
                         paste0(
                           "powerMediation::power.SLR(",
                           "n = ", input$n, ", ",
                           "lambda.a = ", input$beta, ", ",
                           "sigma.x = ", input$sigmaX, ", ",
                           "sigma.y = ", input$sigmaY, ", ",
                           "alpha = ", input$alpha, "/100, ",
                           "verbose = FALSE)"
                         ),
                         paste0(
                           "powerMediation::power.SLR.rho(",
                           "n = ", input$n, ", ",
                           "alpha =",  input$alpha, "/100, ",
                           "rho2 = ",  input$r2, ", ",
                           "verbose = FALSE)"
                         )
          )


          poder <- eval(parse(text = code))
          poder <- poder$power

          paste0(
            "<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", round(poder*100, digits = 1),
            "%</font></b></br></br>",

            "O poder para testar se o coeficiente de regressão de <i>", nome_preditora(), "</i> ao regredir <i>", nome_desfecho(),
            "</i> é diferente de 0 é <b>",
            round(poder*100, digits = 1), "%</b>. ",

            "Este valor foi obtido considerando nível de significância de <b>", input$alpha, "%</b>, ",
            "tamanho amostral igual a <b>", input$n, "</b> sujeitos",
            if (input$usar_r2) {
              paste0(
                " e coeficiente de determinação esperado de <b>", input$r2, "</b> ",
                "como é referido em Fulano (1900) OU escolha do pesquisador. "
              )
            } else {
              paste0(
                ", coeficiente de regressão esperado de <b>", input$beta, "</b>, ",
                " desvios padrões esperados de <b>", input$sigmaX, "</b> e <b>", input$sigmaY, "</b> ",
                translation_pss("u.m.", linguagem()),
                "para <i>", nome_preditora(), "</i> e <i>", nome_desfecho(), "</i>",
                ", respectivamente, ",
                "como são referidos em Fulano (1900). "
              )
            },
            .txt_citacao_pss,
            .txt_referencia_tap,
            print_r_code(code)
          )
        }

      })





      # Cenarios ----


      output$cenarios <- renderUI({

        req(2 < 1)

        req(tipo == "tamanho_amostral")
        # req(input$tipo_variavel == 0)

        req(!is.null(input$estatistica_tratamento))

        if (input$tipo_variavel == 1) {
          razao_usada <- input$rc_continua

          if (razao_usada > 1) {
            ratio_start <- razao_usada
            ratio_end  <- razao_usada + 1
            ratio_by   <- 0.1
            ratio_max  <- Inf
          } else{
            ratio_start <- max(0, razao_usada - 0.3)
            ratio_end  <- razao_usada
            ratio_by   <- 0.05
            ratio_max  <- Inf
          }

        } else if (input$estatistica_tratamento == 'percent') {
          ratio_start <- min(c(5, input$perc_tratamento))
          ratio_end  <- max(c(95, input$perc_tratamento))
          ratio_by   <- 10
          ratio_max  <- 100
        } else {
          razao_usada <- ifelse(input$estatistica_tratamento == 'ratio', input$rr, input$rc)

          if (razao_usada > 1) {
            ratio_start <- razao_usada
            ratio_end  <- razao_usada + 1
            ratio_by   <- 0.1
            ratio_max  <- Inf
          } else{
            ratio_start <- max(0, razao_usada - 0.3)
            ratio_end  <- razao_usada
            ratio_by   <- 0.05
            ratio_max  <- Inf
          }
        }


        fluidPage(fluidRow(
          br(),
          HTML('<hr style="color: black;">'),
          br(),br(),
          titlePanel(translation_pss("Construção de cenários", linguagem())),
          br(),
          wellPanel(translation_pss(
            "Utilize os argumentos abaixo para construir diferentes cenários. Demais informações serão recuperadas do painel lateral.",
            linguagem())
          ),

          if (input$tipo_variavel == 1) {
            HTML(
              paste0("<b>", translation_pss("Defina a sequência da razão de chances", linguagem()), ":</b>")
            )
          } else if (input$estatistica_tratamento == 'percent') {
            HTML(
              paste0("<b>", translation_pss("Defina a sequência de valores (%) para o grupo", linguagem()) , nome_grupo_tratamento(), ":</b>")
            )
          } else if (input$estatistica_tratamento == 'ratio') {
            HTML(
              paste0("<b>", translation_pss("Defina a sequência do risco relativo", linguagem()),":</b>")
            )
          } else {
            HTML(
              paste0("<b>", translation_pss("Defina a sequência da razão de chances", linguagem()), ":</b>")
            )
          },


          br(),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
              numericInput(ns("from"), translation_pss("Mínimo", linguagem()), value = ratio_start, step = .1, min = 0, max = ratio_max)
          ),
          div(style="display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("to"), translation_pss("Máximo", linguagem()), value = ratio_end, step = .1, min = 0, max = ratio_max)
          ),
          div(style="display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("by"), translation_pss("Intervalo", linguagem()), value = ratio_by, min = 0, step = .1) %>%
                .help_buttom(body = translation_pss("Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.", linguagem()),
                             title = "Sequência")
          ),


          fluidRow(
            column(6,
                   textInput(inputId = ns("poder_cenarios"),
                             label   = translation_pss("Digite valores de poder (%) para fazer o gráfico:", linguagem()),
                             value   = "80, 90, 95",
                             width   = "400px") %>%
                     .help_buttom(body = ajuda_cenarios_multiplos_valores())
            )
          ),

          plotly::plotlyOutput(ns("grafico_cenarios"), width = "80%") %>%
            shinycssloaders::withSpinner(type = 5),

          br(), br(),
          downloadButton(ns("download_tabela_cenarios"), translation_pss("Download tabela", linguagem())),
          DT::dataTableOutput(ns("tabela_cenarios"), width = "100%") %>%
            shinycssloaders::withSpinner(type = 5)

        ))

      })



      eval(parse(text = check_text_input_to_vector("poder_cenarios")))



      tab_p2_TH_cenarios <- reactive({

        poder <- text_input_to_vector(input$poder_cenarios)
        req(length(poder) > 0)

        if (input$tipo_variavel == 1) {

          rrrr <- NA
          odssss <- seq(from = input$from, to = input$to, by = input$by)
          prob_control <- NA
          prop_tratamento <- odssss

        } else if (input$estatistica_tratamento == "percent") {
          rrrr <- NA
          odssss <- NA
          prop_tratamento <- seq(from = input$from, to = input$to, by = input$by)

        } else if (input$estatistica_tratamento == "ratio") {

          rrrr <- seq(from = input$from, to = input$to, by = input$by)
          odssss <- NA
          prop_tratamento <- input$perc_controle*rrrr

        } else {

          rrrr <- NA
          odssss <- seq(from = input$from, to = input$to, by = input$by)
          prob_control <- input$perc_controle/100
          prop_tratamento <- (odssss*prob_control)/ (1 + odssss*prob_control - prob_control) * 100
        }


        df_inputs_prop <- tibble::tibble(
          `Risco relativo` = rrrr,
          `Razão de chance` = odssss,
          prop_tratamento
        )


        simul_n <- expand.grid(`% Controle`   = ifelse(input$tipo_variavel == 0,
                                                       input$perc_controle,
                                                       input$logistic_rate_mean),
                               `% Tratamento` = prop_tratamento,
                               `Nível de significância (%)` = input$alpha,
                               `Poder (%)`   = poder,
                               Balanceamento = input$balanceamento,
                               stringsAsFactors = FALSE) %>%
          dplyr::filter(`% Tratamento` != `% Controle`)

        if (input$tipo_variavel == 0) {

          simul_n <- simul_n %>%
            mutate(
              probs = Balanceamento/(1 + Balanceamento),

              ntemp = powerMediation::SSizeLogisticBin(
                p1 = `% Controle`/100,
                p2 = `% Tratamento`/100,
                B = probs,
                alpha = `Nível de significância (%)`/100,
                power = `Poder (%)`/100
              ),

              `n Tratamento` = ceiling(ntemp*(1 - probs)),
              `n Controle`   = ceiling(ntemp*probs),
              `n total` = `n Tratamento` + `n Controle`
            ) %>%

            dplyr::filter(!is.na(`n Tratamento`) & !is.na(`n Controle`))

        } else {

          simul_n <- simul_n %>%
            mutate(
              `n total` = powerMediation::SSizeLogisticCon(
                p1 = input$logistic_rate_mean/100,
                OR = `% Tratamento`,
                alpha = `Nível de significância (%)`/100,
                power = `Poder (%)`/100
              ),

              `n Tratamento` = NA,
              `n Controle` = NA
            )

        }

        simul_n %>%
          left_join(df_inputs_prop, by = c("% Tratamento" = "prop_tratamento"))


      })


      output$grafico_cenarios <- plotly::renderPlotly({

        metrica <- case_when(
          input$tipo_variavel == 1 ~ "Razão de chance",
          input$estatistica_tratamento == 'percent' ~ "% Tratamento",
          input$estatistica_tratamento == 'ratio'   ~ "Risco relativo",
          TRUE ~ "Razão de chance"
        )

        xlab <- case_when(
          input$tipo_variavel == 1 ~ translation_pss("Razão de chance", linguagem()),
          input$estatistica_tratamento == 'percent' ~ paste0("% ", nome_grupo_tratamento()),
          input$estatistica_tratamento == 'ratio'   ~ translation_pss("Risco relativo", linguagem()),
          TRUE ~ translation_pss("Razão de chance", linguagem())
        )

        g1 <- tab_p2_TH_cenarios() %>%
          mutate(
            `Poder (%)` = factor(`Poder (%)`)
          ) %>%
          ggplot(
            aes(x = !! sym(metrica),
                y = `n total`,
                color = `Poder (%)`,
                Tratamento = `n Tratamento`,
                Controle   = `n Controle`
            )
          ) +
          geom_line() +
          geom_point() +
          scale_x_continuous(breaks = seq(from = input$from, to = input$to, by = input$by)) +
          xlab(xlab) +
          ylab(translation_pss("Tamanho da amostra*", linguagem())) +
          theme_bw() +
          theme(axis.text = element_text(colour = "black")) +
          scale_color_brewer(name = translation_pss("Poder (%)", linguagem()), palette = "Set1")


        plotly::ggplotly(g1,
                         tooltip = c("x", "colour", "y", translation_pss("Tratamento", linguagem()), translation_pss("Controle", linguagem()))) %>%
          plotly::layout(annotations = list(x = 1, y = -0.1, text = translation_pss("* sem considerar perdas/ recusas.", linguagem()),
                                            showarrow = F, xref='paper', yref='paper',
                                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                            font=list(size=10)))
      })








      return_table_tabela_cenarios <- reactive({

        metrica <- case_when(
          input$tipo_variavel == 1 ~ "Razão de chance",
          input$estatistica_tratamento == 'percent' ~ "% Tratamento",
          input$estatistica_tratamento == 'ratio'   ~ "Risco relativo",
          TRUE ~ "Razão de chance"
        )

        xlabb <- case_when(
          input$tipo_variavel == 1 ~ translation_pss("Razão de chance", linguagem()),
          input$estatistica_tratamento == 'percent' ~ paste0("% ", nome_grupo_tratamento()),
          input$estatistica_tratamento == 'ratio'   ~ translation_pss("Risco relativo", linguagem()),
          TRUE ~ translation_pss("Razão de chance", linguagem())
        )

        df_ <- tab_p2_TH_cenarios() %>%
          dplyr::select(
            c("% Controle",
              all_of(metrica),
              "n total",
              "n Tratamento",
              "n Controle",
              "Nível de significância (%)",
              "Poder (%)",
              "Balanceamento"
            )
          )

        colnames(df_) <- c(
          ifelse (input$tipo_variavel == 1,
                  "% de eventos na média da variável preditora",
                  paste0("% ", nome_grupo_controle())
          ),
          xlabb,
          translation_pss("Tamanho amostral", linguagem()),
          paste0("n ", nome_grupo_tratamento()),
          paste0("n ", nome_grupo_controle()),
          translation_pss("Nível de significância (%)", linguagem()),
          translation_pss("Poder (%)", linguagem()),
          translation_pss("Balanceamento", linguagem())
        )

        if (input$tipo_variavel == 1) {
          df_ %>%
            select(
              -c(
                paste0("n ", nome_grupo_tratamento()),
                paste0("n ", nome_grupo_controle()),
                translation_pss("Balanceamento", linguagem())
              )
            )
        } else {
          df_
        }

      })



      output$tabela_cenarios <- DT::renderDataTable({

        return_table_tabela_cenarios() %>%
          DT::datatable(extensions = c('FixedColumns'),
                        rownames   = FALSE,
                        filter     = "none",
                        options    = list(pageLength = 10,
                                          scrollX = TRUE,
                                          scrollY = TRUE,
                                          searching = FALSE,
                                          fixedColumns = list(leftColumns = 1),
                                          dom = 'B<"dwnld">frtip'
                        )
          )
      })


      output$download_tabela_cenarios <- downloadHandler(
        filename = function() { "Cenarios_tamanho_amostra_regressao_logistica.xlsx"},
        content = function(file) {writexl::write_xlsx(return_table_tabela_cenarios(),
                                                      path = file)}
      )







    } # Nao mexer!!!
  )
}




