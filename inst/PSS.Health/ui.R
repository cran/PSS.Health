

#__________________________----

#### 1 Media ####


aba_estimacao_uma_media <- tabPanel(
  "Uma amostra",
  shinyFeedback::useShinyFeedback(),
  titlePanel("Uma média"),
  wellPanel(
    # includeMarkdown(file.path("Markdown", "Caput_estimar_uma_media.Rmd")),
    "Um estudo pode ter como objetivo estimar ou testar o valor médio de uma variável quantitativa referente à população de interesse. ",
    'Mais detalhes sobre o uso dessa aba em ',
    HTML('<b><a https://doi.org/10.22491/2357-9730.112466" target="_blank">PSS Health: como calcular tamanho de amostra para estimar média, proporção e correlação</a></b>.')
  ),
  tabsetPanel(
    tabPanel("Estimar",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "mean_delineamento",
                             label   = "Processo de amostragem aleatória",
                             choices = c("Aleatória simples" = "aas",
                                         "Conglomerados em um único estágio" = "ac1",
                                         "Estratificada proporcional ao tamanho" = "aae"
                             ),
                             selected = "aas"),


                 textInput(inputId = "mean_nome_desfecho",
                           label   = "Descreva o nome do desfecho",
                           value   = "Y") %>%
                   .help_buttom(body = .txt_desfecho, title = "Nome do desfecho"),
                 uiOutput("mean_um"),
                 uiOutput("e_meanUi"),
                 uiOutput("mean_sd"),
                 uiOutput("mean_aae_ui"),
                 uiOutput("mean_ac1Ui"),
                 numericInput( "conf_mean",
                               "Nível de confiança (%)",
                               value = 95,
                               min   = 0,
                               max   = 100,
                               step  = 1
                 ) %>% .help_buttom(body = .txt_confianca, title = "Nível de confiança (%)"),
                 numericInput( "mean_perdas_recusa",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
               ),

               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("mean"), type = 5),

                 ###  CENARIOS  ####.
                 uiOutput("cenarios_uma_media_estUi")

               ) # Fecha main panel
             ),

             rodape
    ),


    tabPanel("Testar",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   HTML(
                     "<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"
                   ),
                   uiOutput("th_mean_formula1"),
                   uiOutput("th_mean_formula2")
                 ),

                 numericInput( "media_TH_mean",
                               "Média esperada",
                               value = 2,
                               min = -Inf,
                               max = Inf,
                               step = 1
                 ) %>% .help_buttom(body = paste0("Valor da média que se espera observar na amostra.", .txt_definido_literatura)),
                 numericInput( "margin_TH_mean",
                               "Valor de referência sob a hipótese nula",
                               value = 0,
                               min = -Inf,
                               max = Inf,
                               step = 1
                 ) %>% .help_buttom(body = paste0(
                   "Valor da média sob a hipótese nula ($\\mu_0$) ",
                   "Maiores informações em ",
                   '<a href="https://seer.ufrgs.br/hcpa/article/view/93649/pdf" target="_blank">Hirakata et al. 2019</a>.',
                   .txt_definido_pesquisador
                 )),
                 numericInput( "sigma_TH_mean",
                               "Desvio padrão esperado",
                               value = 10,
                               min = 0,
                               max = Inf,
                               step = 1
                 ) %>% .help_buttom(body = .txt_dp, title = "Desvio padrão esperado"),
                 numericInput( "alpha_TH_mean",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
                 numericInput( "beta_TH_mean",
                               "Poder (%)",
                               value = 80,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),

                 numericInput( "mean_TH_recusa",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")

               ),


               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("THmean"), type = 5),
               )
             )
    ),
    tabPanel("Poder",
             sidebarLayout(
               sidebarPanel(
                 numericInput( "mean1_power_diff",
                               "Diferença a ser detectada",
                               value = 5,
                               min = -Inf,
                               max = Inf,
                               step = 1
                 ) %>% .help_buttom(body = .txt_diferenca_clinica, title = "Diferença a ser detectada"),
                 numericInput( "mean1_power_sigma",
                               "Desvio padrão esperado",
                               value = 15,
                               min = 0,
                               max = Inf,
                               step = 5
                 ) %>% .help_buttom(body = .txt_dp, title = "Desvio padrão esperado"),
                 numericInput( "mean1_power_n",
                               "Tamanho amostral",
                               value = 20,
                               min = 1,
                               max = Inf,
                               step = 1
                 ) %>% .help_buttom(body = "Qual o n de cada grupo. Assume grupos de mesmo tamanho."),
                 numericInput( "mean1_power_sig",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)")

               ),

               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("poder_TH1_mean"), type = 5)
               )
             )
    )
  )
)





#### 1 Proporcao ####
aba_estimacao_uma_prop <- tabPanel(
  "Uma amostra",
  titlePanel("Uma proporção"),
  wellPanel("No caso de estudos em que o objetivo seja alcançado através de variáveis categóricas,
                      o parâmetro de interesse é a proporção de ocorrência das categorias de resposta
                      destas variáveis. No caso de se estimar uma proporção, o motivo principal de se
                      calcular o tamanho da amostra é garantir uma determinada precisão na estimativa
                      que será obtida. ",
            'Mais detalhes sobre o uso dessa aba em ',
            HTML('<b><a https://doi.org/10.22491/2357-9730.112466" target="_blank">PSS Health: como calcular tamanho de amostra para estimar média, proporção e correlação</a></b>.')),

  tabsetPanel(
    tabPanel("Estimar",
             sidebarLayout(
               sidebarPanel(
                 textInput(inputId = "prop_nome_desfecho",
                           label   = "Descreva o nome do desfecho",
                           value   = "Y") %>% .help_buttom(body = .txt_desfecho),

                 # checkboxInput("n_size_prop", "Tamanho de amostra finito", value = FALSE
                 # ) %>% .help_buttom(body = "Caso considere o tamanho da população finito, clique aqui para especificar o tamanho da população."),
                 # conditionalPanel(condition = "input.n_size_prop == true",
                 #                  numericInput( "N_pop_prop",
                 #                                "Tamanho populacional",
                 #                                value = 500,
                 #                                min = 0,
                 #                                max = Inf,
                 #                                step = 1
                 #                  ) %>% .help_buttom(body = "Especifique o tamanho da população.")
                 # ),
                 # numericInput( "e_prop",
                 #               "Margem de erro (%)",
                 #               value = 5,
                 #               min = 0,
                 #               max = 100,
                 #               step = 1
                 # ) %>% .help_buttom(body = "É a metade da amplitude do intervalo de confiança."),
                 numericInput( "e_prop",
                               "Amplitude do intervalo (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 5
                 ) %>% .help_buttom(body = .txt_amplitude, title = "Amplitude do intervalo"),
                 numericInput( "p_prop",
                               "Percentual esperado (%)",
                               value = 50,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = paste0(.txt_perc_esperado, .txt_definido_literatura), title = "Percentual esperado (%)"),
                 selectInput("p1_metodo",
                             "Método utilizado para calcular o intervalo de confiança",
                             choices = c("wilson", "agresti-coull", "exact", "wald"),
                             selected = "wald"
                 ) %>% .help_buttom(body = .txt_per_method),
                 numericInput( "conf_prop",
                               "Nível de confiança (%)",
                               value = 95,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_confianca, title = "Nível de confiança (%)"),
                 numericInput( "prop_perdas_recusa",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)"),
               ),

               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("prop"), type = 5),


                 ###  CENARIOS  ####.
                 uiOutput("prop_precisao_cenarioUi")
               )
             )
    ),


    tabPanel("Testar",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   HTML("<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"),
                   uiOutput("th_prop_formula1")
                 ),

                 numericInput( "p_TH_observado",
                               "Percentual esperado (%)",
                               value = 30,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perc_esperado, title = "Percentual esperado (%)"),
                 numericInput( "p_TH_h0",
                               "Percentual sob a hipótese nula (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ),
                 numericInput( "alpha_TH_prop",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ),
                 numericInput( "beta_TH_prop",
                               "Poder (%)",
                               value = 80,
                               min = 0,
                               max = 100,
                               step = 1
                 ),

                 numericInput( "prop_1th_perdas_recusa",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)"),


                 checkboxInput("prop_1th_approx", "Calcular utilizando a aproximação pela normal", value = FALSE
                 ) %>% .help_buttom(body = paste0("Calcular utilizando a aproximação pela normal?
                                   Se esta opção estiver desmarcada será utilizado o método exato.",
                                   .txt_definido_pesquisador)
                 ),

                 conditionalPanel("input.prop_1th_approx == true",
                                  checkboxInput("prop_1th_correction", "Aplicar correção de continuidade", value = TRUE
                                  ) %>% .help_buttom(body = paste0("Clique aqui para calcular um tamanho de amostra para um teste com correção de continuidade", .txt_definido_pesquisador))
                 )

               ),
               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("TH1prop"), type = 5)
               )
             )
    ),


    tabPanel("Poder",
             sidebarLayout(
               sidebarPanel(

                 wellPanel(
                   HTML("<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"),
                   uiOutput("p_power_th")),

                 numericInput( "p_power_observado",
                               "Percentual esperado (%)",
                               value = 30,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perc_esperado, title = "Percentual esperado (%)"),
                 numericInput( "p_power_h0",
                               "Percentual sob a hipótese nula (%)",
                               value = 20,
                               min = 0,
                               max = 100,
                               step = 1
                 ),
                 numericInput( "p_power_n",
                               "Tamanho amostral",
                               value = 150,
                               min = 0,
                               max = Inf,
                               step = 1
                 ),
                 numericInput( "p_power_alpha",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ),

                 checkboxInput("p_power_approx", "Calcular utilizando a aproximação pela normal", value = FALSE
                 ) %>% .help_buttom(body = paste0("Calcular utilizando a aproximação pela normal?
                                   Se esta opção estiver desmarcada será utilizado o método exato.", .txt_definido_pesquisador )
                 ),

                 conditionalPanel("input.p_power_approx == true",
                                  checkboxInput("p_power_correction", "Aplicar correção de continuidade", value = TRUE
                                  ) %>% .help_buttom(body = paste0("Clique aqui para calcular um tamanho de amostra para um teste com correção de continuidade", .txt_definido_pesquisador))
                 )

               ),
               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("p_power_output"), type = 5)
               )
             )
    )
  ),


  rodape
)





# Cronbach  ----


aba_estimacao_Cronbach <- tabPanel("Cronbach",
                                   titlePanel("Estimação de Cronbach"),
                                   wellPanel("O alfa de Cronbach quantifica o grau de confiabilidade de um instrumento. Valores próximos de 1 indicam alta confiabilidade."),

                                   sidebarLayout(
                                     sidebarPanel(
                                       numericInput( "k_Cronbach",
                                                     "Nº de itens do instrumento",
                                                     value = 10,
                                                     min = 2,
                                                     max = Inf,
                                                     step = 1
                                       ) %>% .help_buttom(body = paste0("Número de itens do instrumento.", .txt_definido_pesquisador)),
                                       numericInput( "Cronbach_espected",
                                                     "Cronbach esperado",
                                                     value = 0.7,
                                                     min = 0,
                                                     max = 1,
                                                     step = .1
                                       ) %>% .help_buttom(body = paste0("Cronbach esperado.", .txt_definido_pesquisador_OU_literatura)),
                                       numericInput( "Cronbach_precisao",
                                                     "Amplitude do intervalo",
                                                     value = 0.2,
                                                     min = 0,
                                                     max = 1,
                                                     step = .1
                                       ) %>% .help_buttom(body = .txt_amplitude, title = "Amplitude do intervalo"),
                                       numericInput( "conf_Cronbach",
                                                     "Nível de confiança (%)",
                                                     value = 95,
                                                     min = 0,
                                                     max = 100,
                                                     step = 1
                                       ) %>% .help_buttom(body = .txt_confianca, title = "Nível de confiança (%)"),
                                       numericInput( "Cronbach_perdas_recusa",
                                                     "Perdas/ Recusa (%)",
                                                     value = 10,
                                                     min = 0,
                                                     max = 100,
                                                     step = 1
                                       ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)"),
                                     ),

                                     mainPanel(
                                       shinycssloaders::withSpinner(htmlOutput("Cronbach_est"), type = 5),

                                       htmlOutput("cronbach_code")

                                       ###  CENARIOS  ####.

                                       # br(),br(),
                                       # HTML('<hr style="color: black;">'),
                                       # br(),br(),
                                       #
                                       # titlePanel("Construção de cenários"),
                                       # br(),br(),
                                       #
                                       #
                                       # textInput(inputId = "prop_precisoes_plot",
                                       #           label   = "Digite valores de precisão (%) para fazer o gráfico",
                                       #           value   = "3, 5, 5.5, 10"
                                       # ),
                                       # p("Obs.: serão utilizados o nível de confiança, o percentual de perdas/ recusas e o tamanho populacional definidos acima."),
                                       #
                                       # plotly::plotlyOutput("prop_plot", width = "80%"),
                                       # br(), br(),
                                       # downloadButton("download_prop_tab","xlsx"),
                                       # DT::dataTableOutput("prop_tab", width = "100%")

                                     )
                                   ),

                                   rodape
)







#__________________--------
# TH para 2 amostra  ----
#--------------------------.


#### Media ####

aba_TH_duas_amostra_media <- tabPanel(
  "Dois grupos independentes",
  titlePanel("Comparação entre duas médias de grupos independentes"),
  wellPanel("Nesta técnica deseja-se calcular o tamanho amostral de um estudo cujo objetivo é ",
            "comparar se a média de dois grupos independentes diferem ou não em relação à ",
            "variável interesse."
            # "\n\nAo calcular o tamanho da amostra para este objetivo, ",
            # "estaremos supondo que a variável de interesse, em cada um dos grupos, segue uma distribuição normal, ",
            # "e que será utilizado o teste t independente para comparação de médias."
  ),
  tabsetPanel(
    tabPanel("Testar",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   HTML("<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"),
                   uiOutput("th2_mean_formula1"),
                   uiOutput("th2_mean_formula2")
                 ),

                 actionLink("show_th_2mean", "Mudar nomes"),
                 br(), br(),

                 wellPanel(
                   checkboxInput("th2_mean_cohen", "Calcular usando o d de Cohen", value = FALSE),
                   # %>% .help_buttom(body = "Clique aqui para usar d de Cohen ao invés das diferenças e desvio padrão."),
                   conditionalPanel(condition = "input.th2_mean_cohen == true",

                                    # HTML("<i>Você também pode calcular o d de Cohen na aba 'Outras ferramentas' --> 'd de Cohen'</i><br>"),
                                    # br(),
                                    numericInput( "cohen_TH2_mean_pwr",
                                                  "Tamanho do efeito (d de Cohen)",
                                                  value = 0.4,
                                                  min = 0,
                                                  max = Inf,
                                                  step = 0.1) %>%
                                      shinyhelper::helper(type = "markdown",
                                                          title = "Tamanho de efeito d",
                                                          content = "Effect_size_d_Cohen",
                                                          buttonLabel = "Fechar",
                                                          fade = TRUE,
                                                          colour = "#006338",
                                                          size = "l"),
                                    # actionLink("show_d_cohen", "O que é o d de Cohen?"),
                                    # br(), br(),
                                    # ) %>% shinyhelper::helper(
                                    #   type = "markdown",
                                    #   title = "D de Cohen",
                                    #   content = "D_de_cohen.Rmd",
                                    #   buttonLabel = "Fechar",
                                    #   fade = TRUE,
                                    #   size = "l"),


                                    # actionLink("show_d_cohen",
                                    #            "Você também pode calcular a d de Cohen em 'Outras ferramentas' --> 'd de Cohen'"
                                    #            ),
                                    # br()

                   ),

                   conditionalPanel(condition = "input.th2_mean_cohen == false",
                                    uiOutput("mean2Ui"))
                 ),
                 numericInput( "power_TH2_mean_pwr",
                               "Poder (%)",
                               value = 80,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
                 numericInput( "sig_TH2_mean_pwr",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),

                 selectInput('alternative_TH2_mean_pwr',
                             'Tipo de teste de acordo com hipótese alternativa:',
                             choices = c("Bilateral" = "two.sided", "Unilateral" = "one.sided"),
                             selected = 'two.sided'
                 ) %>% .help_buttom(body = .txt_h1),

                 numericInput( "TH_mean_perdas_recusa",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
               ),

               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("THmean2"), type = 5),

                 ###  CENARIOS  ####.

                 uiOutput("cenarios_duas_medias_thUi")
               )
             )
    ),


    tabPanel("Poder",
             sidebarLayout(
               sidebarPanel(

                 wellPanel(
                   HTML("<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"),
                   uiOutput("poder_th2_mean_formula1"),
                   uiOutput("poder_th2_mean_formula2")
                 ),

                 wellPanel(
                   checkboxInput("th2_pwr_mean_cohen", "Usar d de Cohen", value = FALSE),
                   conditionalPanel(condition = "input.th2_pwr_mean_cohen == true",

                                    numericInput( "cohen_TH2_mean_pwr_poder",
                                                  "Tamanho do efeito (d de Cohen)",
                                                  value = 0.4,
                                                  min = 0,
                                                  max = Inf,
                                                  step = 0.1) %>%
                                      shinyhelper::helper(type = "markdown",
                                                          title = "Tamanho de efeito d",
                                                          content = "Effect_size_d_Cohen",
                                                          buttonLabel = "Fechar",
                                                          fade = TRUE,
                                                          colour = "#006338",
                                                          size = "l")
                   ),

                   conditionalPanel(condition = "input.th2_pwr_mean_cohen == false",
                                    numericInput( "poder_TH2_mean_margin",
                                                  "Diferença a ser detectada (Grupo A - Grupo B)",
                                                  value = 1,
                                                  min = -Inf,
                                                  max = Inf,
                                                  step = .5
                                    ) %>% .help_buttom(body = .txt_diferenca_clinica, title = "Diferença a ser detectada"),

                                    HTML(paste0("<b><font size = '2.95'>Desvio padrão do</font></b><br>")),
                                    div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                                        numericInput( "poder_sigma1_TH2_mean_pwr",
                                                      "Grupo A",
                                                      value = 1.2,
                                                      min = 0,
                                                      max = Inf,
                                                      step = 1
                                        )
                                    ),
                                    div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                        numericInput( "poder_sigma2_TH2_mean_pwr",
                                                      "Grupo B",
                                                      value = 1.5,
                                                      min = 0,
                                                      max = Inf,
                                                      step = 1
                                        ) %>% .help_buttom(body = .txt_dp, title = "Desvio padrão esperado")
                                    )
                   )
                 ),


                 HTML(paste0("<b><font size = '2.95'>Tamanho amostral do </font></b><br>")),
                 div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                     numericInput( "poder_n1_TH2_mean_pwr",
                                   "Grupo A",
                                   value = 30,
                                   min = 2,
                                   max = Inf,
                                   step = 1
                     )
                 ),
                 div(style = "display: inline-block;vertical-align:top; width: 49%;",
                     numericInput( "poder_n2_TH2_mean_pwr",
                                   "Grupo B",
                                   value = 45,
                                   min = 2,
                                   max = Inf,
                                   step = 1
                     ) %>% .help_buttom(body = "Tamanho amostral", title = "Tamanho amostral")
                 ),

                 numericInput( "poder_sig_TH2_mean_pwr",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
                 selectInput('poder_alternative_TH2_mean_pwr',
                             'Tipo de teste de acordo com hipótese alternativa:',
                             choices = c("Bilateral" = "two.sided", "Unilateral" = "one.sided"),
                             selected = 'two.sided'
                 ) %>% .help_buttom(body = .txt_h1),
                 # selectInput('poder_alternative_TH2_mean_pwr',
                 #             'Tipo de teste de acordo com hipótese alternativa',
                 #             choices = c('Bilateral' = 'two.sided','Unilateral Superior' = 'greater','Unilateral Inferior' =  'less'),
                 #             selected = 'two.sided'
                 # ) %>% .help_buttom(body = .txt_h1)
               ),

               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("THmean2_power"), type = 5)
               )
             )
    ),

    tabPanel("Estimar",
             sidebarLayout(
               sidebarPanel(
                 textInput(inputId = "mean2TH_nome_desfecho",
                           label   = "Descreva o nome do desfecho",
                           value   = "Y") %>% .help_buttom(body = .txt_desfecho),
                 uiOutput("mean2TH_um"),
                 numericInput( "TH2_mean_precisao",
                               "Margem de erro ou semi-amplitude",
                               value = 1,
                               min = 0,
                               max = Inf,
                               step = .5
                 ) %>% .help_buttom(body = .txt_precisao, title = "Margem de erro ou semi-amplitude"),
                 numericInput( "sigma_TH2_mean_est",
                               "Desvio padrão esperado",
                               value = 1.2,
                               min = 0,
                               max = Inf,
                               step = .01
                 ) %>% .help_buttom(body = .txt_dp, title = "Desvio padrão esperado"),
                 numericInput( "conf_TH2_mean_pwr",
                               "Nível de confiança (%)",
                               value = 95,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_confianca, title = "Nível de confiança (%)"),
                 numericInput( "TH_mean_perdas_recusa_est",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
               ),
               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("THmean2_est"), type = 5)
               )
             )
    )
  ),

  rodape
)





#### Proporcao ####

aba_TH_duas_amostra_prop <- tabPanel(
  "Dois grupos independentes",
  titlePanel("Comparação entre duas proporções de grupos independentes"),
  wellPanel(p("Nesta técnica deseja-se calcular o tamanho amostral de um estudo cujo objetivo é ",
              "comparar se as proporções em dois grupos distintos diferem ou não em relação à ",
              "variável de interesse, com um certo nível de significância e poder, ou calcular o poder do teste quando o tamanho amostral é conhecido.",
              "Os cálculos do teste e do poder são realizados utilizando a aproximação pela distribuição normal, ",
              "por isso tenha cautela no uso dos resultados para amostras muito pequenas.")
  ),
  tabsetPanel(
    tabPanel("Testar",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   HTML("<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"),
                   uiOutput("th2_prop_formula1"),
                   uiOutput("th2_prop_formula2"),
                 ),

                 actionLink("show_th_2prop", "Mudar nomes"),
                 br(), br(),

                 uiOutput("perc_controle_testar"),

                 wellPanel(
                   uiOutput("prop2_estatistica_BUi"),

                   conditionalPanel(condition = 'input.prop2_estatistica_B == "percent"',
                                    uiOutput("perc_tratamento_testar")
                   ),
                   conditionalPanel(condition = 'input.prop2_estatistica_B == "ratio"',
                                    numericInput( "p2_TH_ratio",
                                                  "Risco relativo/ Razão de prevalências",
                                                  value = 2,
                                                  min = 0,
                                                  max = Inf,
                                                  step = 0.1
                                    )  %>% .help_buttom(body = .txt_risco_relativo, title = "Risco relativo/ Razão de prevalências")
                   ),
                   conditionalPanel(condition = 'input.prop2_estatistica_B == "odds"',
                                    numericInput( "p2_TH_odds",
                                                  "Razão de chance",
                                                  value = 2,
                                                  min = 0,
                                                  max = Inf,
                                                  step = 0.1
                                    ) %>% .help_buttom(body = .txt_razao_chance, title = "Razão de chance")
                   )
                 ),
                 uiOutput("k_TH_prop2Ui"),
                 numericInput( "beta_TH_prop2",
                               "Poder (%)",
                               value = 80,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
                 numericInput( "alpha_TH_prop2",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
                 # uiOutput("alternative_TH2_prop_pwr2Ui"),

                 selectInput('alternative_TH2_prop_pwr2',
                             'Tipo de teste de acordo com hipótese alternativa:',
                             choices = c("Unilateral", "Bilateral"),
                             selected = 'Bilateral'
                 ) %>% .help_buttom(body = .txt_h1),

                 numericInput( "TH_prop_perdas_recusa",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)"),

                 checkboxInput("prop_correction", "Aplicar correção de continuidade", value = TRUE
                 ) %>% .help_buttom(body = paste0("Clique aqui para calcular um tamanho de amostra para um teste com correção de continuidade", .txt_definido_pesquisador)),
                 # actionButton("help_TH2_prop_trialsize", "Ajuda")
               ),

               mainPanel(
                 # verbatimTextOutput("lala"),
                 shinycssloaders::withSpinner(htmlOutput("THprop2"), type = 5),

                 ###  CENARIOS  ####.
                 uiOutput("cenarios_duas_prop_thUi")
               )
             )
    ),
    tabPanel("Poder",
             sidebarLayout(
               sidebarPanel(

                 wellPanel(
                   HTML("<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"),
                   withMathJax("$$H_0: \\pi_{Grupo A} = \\pi_{Grupo B}$$"),
                   withMathJax("$$H_1: \\pi_{Grupo A} \\neq \\pi_{Grupo B}$$")
                 ),

                 HTML(paste0("<b><font size = '2.95'>Proporção (%) de desfechos no</font></b><br>")),
                 div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                     numericInput( "prop2a_th_power",
                                   "Grupo A",
                                   value = 35,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     )
                 ),
                 div(style = "display: inline-block;vertical-align:top; width: 49%;",
                     numericInput( "prop2b_th_power",
                                   "Grupo B",
                                   value = 60,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     )
                 ),


                 HTML(paste0("<b><font size = '2.95'>Tamanho amostral no</font></b><br>")),
                 div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                     numericInput( "prop2a_th_n",
                                   "Grupo A",
                                   value = 35,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     )
                 ),
                 div(style = "display: inline-block;vertical-align:top; width: 49%;",
                     numericInput( "prop2b_th_n",
                                   "Grupo B",
                                   value = 60,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     )
                 ),
                 numericInput( "prop2_th_power_sig",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),

                 checkboxInput("prop2_th_power_correction", "Aplicar correção de continuidade", value = TRUE
                 ) %>% .help_buttom(body = paste0("Clique aqui para calcular um tamanho de amostra para um teste com correção de continuidade", .txt_definido_pesquisador))
                 # selectInput('alternative_TH2_prop_pwr2_power',
                 #             'Tipo de teste de acordo com hipótese alternativa:',
                 #             choices = c('A % em A é DIFERENTE da % em B' = 'two.sided',
                 #                         'A % em A é MAIOR do que a % em B' = 'greater',
                 #                         'A % em A é MENOR do que a % em B' =  'less'),
                 #             selected = 'two.sided'
                 # ) %>% .help_buttom(body = .txt_h1)


               ),
               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("THprop2_power"), type = 5)
               )
             )
    ),
    tabPanel("Estimar",
             sidebarLayout(
               sidebarPanel(

                 textInput(inputId = "prop2_nome_desfecho_est",
                           label   = "Descreva o nome do desfecho",
                           value   = "Y") %>% .help_buttom(body = .txt_desfecho),
                 uiOutput("perc_controle_estimar"),
                 radioButtons('prop2_estatistica_B_est',
                              'Medida do grupo tratamento:',
                              choices = c("Razão de chance (Tratamento/Controle)" = "odds",
                                          "Risco relativo (Tratamento/Controle)" = "ratio",
                                          "% esperado no grupo Tratamento" = "percent"),
                              selected = 'percent'
                 ), #%>% .help_buttom(body = "Escolha a maneira de entrar com os dados do grupo Tratamento."),


                 conditionalPanel(condition = 'input.prop2_estatistica_B_est == "percent"',
                                  uiOutput("perc_tratamento_estimar")
                 ),
                 conditionalPanel(condition = 'input.prop2_estatistica_B_est == "ratio"',
                                  numericInput( "p2_TH_ratio_est",
                                                "Risco relativo (Tratamento/Controle)",
                                                value = 2,
                                                min = 0,
                                                max = Inf,
                                                step = 0.1
                                  ) %>% .help_buttom(body = .txt_risco_relativo, title = "Risco relativo/ Razão de prevalências")
                 ),
                 conditionalPanel(condition = 'input.prop2_estatistica_B_est == "odds"',
                                  numericInput( "p2_TH_odds_est",
                                                "Razão de chance (Tratamento/Controle)",
                                                value = 2,
                                                min = 0,
                                                max = Inf,
                                                step = 0.1
                                  ) %>% .help_buttom(body = .txt_razao_chance, title = "Razão de chance")
                 ),
                 numericInput( "prop2_TH_precisao",
                               "Margem de erro ou semi-amplitude (%)",
                               value = 15,
                               min = 0,
                               max = 100,
                               step = 0.1
                 ) %>% .help_buttom(body = .txt_precisao, title = "Margem de erro ou semi-amplitude"),
                 numericInput( "k_TH_prop2_est",
                               "Balanceamento (Controle:Tratamento)",
                               value = 1,
                               min = 0,
                               max = Inf,
                               step = .5
                 ) %>% .help_buttom(body = .txt_balanceamento, title = "Balanceamento"),
                 numericInput( "conf_TH_prop2",
                               "Nível de confiança (%)",
                               value = 95,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_confianca, title = "Nível de confiança (%)"),
                 numericInput( "TH_prop_perdas_recusa_est",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)"),
                 selectInput('prop_TH_est_method',
                             'Método utilizado na construção do intervalo de confiança',
                             choices = c("score", "adjusted Wald"),
                             selected = 'score'
                 ) %>% .help_buttom(body = paste0("Método utilizado na construção do intervalo de confiança.", .txt_definido_pesquisador)),


                 checkboxInput("prop_correction_est", "Aplicar correção de continuidade", value = TRUE
                 ) %>% .help_buttom(body = paste0("Clique aqui para calcular um tamanho de amostra para um teste com correção de continuidade", .txt_definido_pesquisador)),
                 # actionButton("help_TH2_prop_trialsize_est", "Ajuda")
               ),

               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("THprop2_est"), type = 5)
                 ,

                 # br(), br(),
                 # textOutput("lala"),

                 ###  CENARIOS  ####.

                 # conditionalPanel(condition = "input.prop2_estatistica_B_est == 'percent'",
                 #
                 #                  br(),
                 #                  HTML('<hr style="color: black;">'),
                 #                  br(),br(),
                 #
                 #                  titlePanel("Construção de cenários"),
                 #                  br(),
                 #
                 #
                 #
                 #                  ###
                 #                  # conditionalPanel(condition = "input.prop2_estatistica_B_est == 'percent'",
                 #
                 #                  wellPanel("Utilize os argumentos abaixo para construir diferentes cenários.
                 #                        Você pode definir um intervalo de % para o grupo Tratamento e especificar valores do poder (%).
                 #                        Demais informações serão recuperadas do painel lateral."
                 #                  ),
                 #                  HTML("<b>Defina a sequência de valores (%) para o grupo Tratamento:</b>"),
                 #                  # bsplus::shiny_iconlink(name = "question-circle") %>%
                 #                  #   bsplus::bs_embed_popover(title = "Defina a sequência (%) para o grupo Tratamento. Essa sequência será utilizada para compor o eixo x do gráfico",
                 #                  #                            placement = "left"),
                 #                  br(),
                 #                  div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
                 #                      numericInput("p2_EST_from", "Mínimo", value = 5, step = 1, min = 0, max = 99)
                 #                  ),
                 #                  div(style = "display: inline-block;vertical-align:top; width: 80px;",
                 #                      numericInput("p2_EST_to", "Máximo", value = 95, step = 1, min = 1, max = 100)
                 #                  ),
                 #                  div(style = "display: inline-block;vertical-align:top; width: 80px;",
                 #                      numericInput("p2_EST_by", "Intervalo", value = 5, min = 0, step = 1, max = 99) %>%
                 #                        .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                 #                                    title = "Sequência")
                 #                  ),
                 #                  # ),
                 #
                 #
                 #                  # ratio
                 #                  # conditionalPanel(condition = "input.prop2_estatistica_B_est == 'ratio'",
                 #                  #
                 #                  #
                 #                  #                  wellPanel("Utilize os argumentos abaixo para construir diferentes cenários.
                 #                  #                        Você pode definir um intervalo para o Risco relativo (Tratamento/Controle) e especificar valores do poder (%).
                 #                  #                        Serão utilizados o % do grupo Controle, nível de confiança, tipo de teste, aplicação da correção de continuidade,
                 #                  #                        balanceamento da amostra e percentual de perdas/ recusas definidos no painel lateral."
                 #                  #                  ),
                 #                  #                  HTML("<b>Defina a sequência do risco relativo:</b>"),
                 #                  #                  # bsplus::shiny_iconlink(name = "question-circle") %>%
                 #                  #                  #   bsplus::bs_embed_popover(title = "Defina a sequência do risco relativo. Essa sequência será utilizada para compor o eixo x do gráfico",
                 #                  #                  #                            placement = "left"),
                 #                  #                  br(),
                 #                  #                  div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
                 #                  #                      numericInput("rr_p2_EST_from", "Mínimo", value = 1.2, step = .1, min = 0, max = Inf)
                 #                  #                  ),
                 #                  #                  div(style = "display: inline-block;vertical-align:top; width: 80px;",
                 #                  #                      numericInput("rr_p2_EST_to", "Máximo", value = 1.7, step = .1, min = 0, max = Inf)
                 #                  #                  ),
                 #                  #                  div(style = "display: inline-block;vertical-align:top; width: 80px;",
                 #                  #                      numericInput("rr_p2_EST_by", "Intervalo", value = 0.1, min = 0, step = .1) %>%
                 #                  #                        .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                 #                  #                                    title = "Sequência")
                 #                  #                  )
                 #                  #
                 #                  # ),
                 #                  #
                 #                  # # odds
                 #                  # conditionalPanel(condition = "input.prop2_estatistica_B_est == 'odds'",
                 #                  #
                 #                  #
                 #                  #                  wellPanel("Utilize os argumentos abaixo para construir diferentes cenários.
                 #                  #                        Você pode definir um intervalo para a Razão de chances (Tratamento/Controle) e especificar valores do poder (%).
                 #                  #                        Serão utilizados o % do grupo Controle, nível de confiança, tipo de teste, aplicação da correção de continuidade,
                 #                  #                        balanceamento da amostra e percentual de perdas/ recusas definidos no painel lateral."
                 #                  #                  ),
                 #                  #                  HTML("<b>Defina a sequência do risco relativo:</b>"),
                 #                  #                  # bsplus::shiny_iconlink(name = "question-circle") %>%
                 #                  #                  #   bsplus::bs_embed_popover(title = "Defina a sequência da razão de chances. Essa sequência será utilizada para compor o eixo x do gráfico",
                 #                  #                  #                            placement = "left"),
                 #                  #                  br(),
                 #                  #                  div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
                 #                  #                      numericInput("ods_p2_EST_from", "Mínimo", value = 1.2, step = .1, min = 0, max = Inf)
                 #                  #                  ),
                 #                  #                  div(style = "display: inline-block;vertical-align:top; width: 80px;",
                 #                  #                      numericInput("ods_p2_EST_to", "Máximo", value = 1.7, step = .1, min = 0, max = Inf)
                 #                  #                  ),
                 #                  #                  div(style = "display: inline-block;vertical-align:top; width: 80px;",
                 #                  #                      numericInput("ods_p2_EST_by", "Intervalo", value = 0.1, min = 0, step = .1) %>%
                 #                  #                        .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                 #                  #                                    title = "Sequência")
                 #                  #                  )
                 #                  #
                 #                  # ),
                 #
                 #                  fluidRow(
                 #                    column(6,
                 #                           textInput(inputId = "precisao_p2_EST_plot",
                 #                                     label   = "Digite valores de precisão (%) para fazer o gráfico:",
                 #                                     value   = "10, 15, 20",
                 #                                     width   = "600px") %>%
                 #                             .help_buttom(body = "Defina os valores de precisão (%).
                 #                                      Esses valores serão utilizados para criar diferentes linhas no gráfico.
                 #                                      Separe os valores por vírgula ',' e utilize ponto '.' como decimal.")
                 #                    )
                 #                  ),
                 #
                 #
                 #
                 #
                 #                  shinycssloaders::withSpinner(plotly::plotlyOutput("p2_EST_plot", width = "80%"), type = 5),
                 #                  p("Obs.: a linha tracejada representa a % no grupo controle definida no painel lateral."),
                 #                  br(), br(),
                 #                  downloadButton("download_p2_EST_tab","Download tabela"),
                 #                  shinycssloaders::withSpinner(DT::dataTableOutput("p2_EST_tab", width = "100%"), type = 5)
                 # ),
               )
             )

    )
  ),

  rodape
)



#### Equiv. 2 médias ####
aba_TH_duas_amostra_media_equivalencia <- tabPanel(
  "Dois grupos independentes (Inf/ Equi/ Sup)",
  titlePanel("Inf/ Equi/ Sup de duas médias de grupos independentes"),
  withMathJax(),
  wellPanel(
    HTML(paste0(
      "Alguns estudos podem querer verificar se um novo tratamento é melhor (estudo de superioridade) do que o padrão,
fixado um limite superior; que ele não é inferior (estudo de não-inferioridade) ao padrão, fixado um limite
inferior; ou tão eficaz (estudo de equivalência) quanto o padrão, fixados um limite inferior e um superior. ",
'Qual a diferença entre teste de comparação, de superioridade, de equivalência e de não-inferioridade? Leia o artigo: ',
'<b><a href="https://seer.ufrgs.br/hcpa/article/view/96394/pdf" target="_blank">Bioestatística e epidemiologia: perguntas que você sempre quis fazer, mas nunca teve coragem</a></b>'

    ))
  ),
#helpText("$$H_0: \\mu_{Tratamento} - \\mu_{Controle} \\le \\delta$$"),

# helpText('An irrational number \\(\\sqrt{2}\\) and a fraction $$1-\\frac{1}{2}$$'),
tabsetPanel(
  tabPanel("Testar",
           sidebarLayout(
             sidebarPanel(
               wellPanel(
                 selectInput('mean_test_inf_eq_sup',
                             'Selecione o tipo de teste',
                             choices = c("Não inferioridade",
                                         "Equivalência",
                                         "Superioridade"
                             ),
                             selected = 'Não inferioridade'
                 ),
                 uiOutput("inf_sup_nomesUi")
               ),
               uiOutput("inf_sup_complementoUi")
             ),

             mainPanel(
               shinycssloaders::withSpinner(htmlOutput("mean_equivalence_2_ind"), type = 5),
               br(), br(),
               wellPanel(
                 HTML("<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"),
                 uiOutput("th2_equi_mean_formula1"),
                 uiOutput("th2_equi_mean_formula2")
               ),
               br(),
               fluidRow(column(12, align="center",plotOutput("plot_eq_medias", width = "70%")))
             )
           )
  )
),

rodape
)



#### Equiv. 2 prop ####

aba_TH_duas_amostra_prop_equivalencia <- tabPanel(
  "Dois grupos independentes (Inf/ Equi/ Sup)",


  titlePanel("Para duas proporções de grupos independentes"),
  withMathJax(),
  wellPanel(
    HTML(paste0(
      "Alguns estudos podem querer verificar se um novo tratamento é melhor (estudo de superioridade) do que o padrão,
fixado um limite superior; que ele não é inferior (estudo de não-inferioridade) ao padrão, fixado um limite
inferior; ou tão eficaz (estudo de equivalência) quanto o padrão, fixados um limite inferior e um superior. ",
'Qual a diferença entre teste de comparação, de superioridade, de equivalência e de não-inferioridade? Leia o artigo: ',
'<b><a href="https://seer.ufrgs.br/hcpa/article/view/96394/pdf" target="_blank">Bioestatística e epidemiologia: perguntas que você sempre quis fazer, mas nunca teve coragem</a></b>'

    ))
  ),
tabsetPanel(
  tabPanel("Testar",
           sidebarLayout(
             sidebarPanel(

               wellPanel(
                 selectInput('prop_test_inf_eq_sup',
                             'Selecione o tipo de teste',
                             choices = c("Não inferioridade",
                                         "Equivalência",
                                         "Superioridade"
                             ),
                             selected = 'Não inferioridade'
                 ),
                 uiOutput("inf_sup_nomesUi_prop")
               ),
               uiOutput("side_bar_prop_inf")
             ),

             mainPanel(
               shinycssloaders::withSpinner(htmlOutput("prop_equivalence_2_ind"), type = 5)
             )
           )
  )
),

rodape
)


#### Media 2 dependentes ####

aba_TH_duas_amostra_mean_pareado <- tabPanel(
  "Dois grupos dependentes",


  titlePanel("Comparação entre duas médias de grupos dependentes"),
  wellPanel("Nesta técnica deseja-se calcular o tamanho amostral de um estudo cujo objetivo é
                    comparar se as médias de dois grupos dependentes, ou seja, dois grupos relacionados
                    ou comumente dito pareados, diferem ou não em relação à resposta de interesse, com
                    um certo nível de significância e poder."),
  tabsetPanel(
    tabPanel("Testar",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   HTML(
                     "<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"
                   ),
                   uiOutput("mean_paired_formula"),
                 ),
                 numericInput( "mean_paired_n_differenca",
                               "Média das diferenças a ser detectada",
                               value = 5,
                               min = -Inf,
                               max = Inf,
                               step = 1
                 ) %>% .help_buttom(body = .txt_diferenca_clinica, title = "Média das diferenças a ser detectada"),
                 numericInput( "sigma_mean_paired_n",
                               "Desvio padrão da diferença",
                               value = 15,
                               min = 0,
                               max = Inf,
                               step = 5
                 ) %>% .help_buttom(body = .txt_dp, title = "Desvio padrão esperado"),
                 actionLink("show_desvio_tpareado", "Obter o desvio padrão da diferença entre grupos pareados"),
                 br(), br(),

                 numericInput( "sig_mean_paired_n",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
                 numericInput( "power_mean_paired_n",
                               "Poder (%)",
                               value = 80,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
                 numericInput( "mean_paired_n_perdas_recusa",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)"),
                 selectInput( "alternative_mean_paired_n",
                              'Tipo de teste de acordo com hipótese alternativa',
                              # choices = c('A média do grupo 1 é DIFERENTE da média do grupo 2' = 'two.sided',
                              #             'A média do grupo 1 é MAIOR do que a média do grupo 2' = 'greater',
                              #             'A média do grupo 1 é MENOR do que a média do grupo 2' =  'less'),
                              choices = c("Bilateral" = "two.sided",
                                          "Unilateral" = "one.sided"),
                              selected = 'two.sided'
                 ) %>% .help_buttom(body = .txt_h1)
               ),

               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("mean_paired_n"), type = 5),
                 uiOutput("cenarios_duas_medias_dep_thUi")
               )
             )
    ),
    tabPanel("Poder",
             sidebarLayout(
               sidebarPanel(
                 numericInput( "mean_paired_poder_differenca",
                               "Diferença a ser detectada",
                               value = 5,
                               min = -Inf,
                               max = Inf,
                               step = 1
                 ) %>% .help_buttom(body = .txt_diferenca_clinica, title = "Diferença a ser detectada"),
                 numericInput( "sigma_mean_paired_power",
                               "Desvio padrão da diferença",
                               value = 15,
                               min = 0,
                               max = Inf,
                               step = 5
                 ) %>% .help_buttom(body = .txt_dp, title = "Desvio padrão esperado"),
                 actionLink("show_desvio_tpareado2", "Obter o desvio padrão da diferença entre grupos pareados"),
                 br(), br(),
                 numericInput( "n_mean_paired_power",
                               "Tamanho amostral",
                               value = 20,
                               min = 1,
                               max = Inf,
                               step = 1
                 ) %>% .help_buttom(body = "Qual o n de cada grupo. Assume grupos de mesmo tamanho."),
                 numericInput( "sig_mean_paired_power",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)")

               ),

               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("mean_paired_power"), type = 5)
               )
             )
    )
  ),

  rodape
)




## 2 prop dependentes ----

aba_TH_duas_amostra_prop_dep <- tabPanel(
  "Dois grupos dependentes",


  titlePanel("Duas proporções de grupos dependentes"),
  withMathJax(),
  wellPanel(
    includeMarkdown(file.path("www", "Teste_Mcnemar.Rmd"))
    # includeHTML("www/Teste_Mcnemar.html")
  ),
  tabsetPanel(
    tabPanel("Testar",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   HTML("<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"),
                   uiOutput("prop2n_dep_th")
                 ),
                 actionLink("show_prop2n_dep", "Mudar nomes"),
                 br(), br(),
                 uiOutput("prop2n_dep_sideUi")
               ),
               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("prop2n_dep_resultadoUi"), type = 5),

                 uiOutput("cenarios_duas_prop_dep_thUi")
               )
             )
    )

  ),

  rodape

)





#_______________-----
# Medidas repetidas ----


aba_TH_medidas_repetidas <- tabPanel(
  "Medidas repetidas",
  titlePanel("Medidas repetidas"),
  wellPanel("Nesta aba é calculado o tamanho de amostra e o poder do teste para análises de medidas repetidas.
  O objetivo é detectar diferenças médias entre os grupos no último momento da coleta de dados.
             É assumido que a variável de tempo será tratada como categórica."),
  tabsetPanel(
    tabPanel("Testar",
             mod_medidas_repetidas_Ui("tamanho_amostral"),
             rodape
    ),

    tabPanel("Poder",
             mod_medidas_repetidas_Ui("poder"),
             rodape
    )
  )
)



# Dois grupos independentes ----------.

aba_TH_duas_amostra_media_2tempos <- tabPanel(
  "Delta de dois grupos independentes",

  titlePanel("Comparação da mudança média ao longo do tempo de grupos independentes"),
  wellPanel(
    includeMarkdown(file.path("www", "Delta_two_groups_independents.Rmd"))
  ),

  sidebarLayout(
    sidebarPanel(
      wellPanel(
        HTML("<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"),
        uiOutput("delta2_mean_formula1"),
        uiOutput("delta2_mean_formula2")
      ),

      actionLink("show_th_2delta", "Mudar nomes"),
      br(), br(),



      uiOutput("delta2_painelUi"),

      numericInput( "th2_mean_dep_balanceamento",
                    "Balanceamento (Tratamento:Controle)",
                    value = 1,
                    min = 0,
                    max = Inf,
                    step = .5
      ) %>% .help_buttom(body = .txt_balanceamento, title = "Balanceamento"),


      numericInput( "th2_mean_dep_pwr",
                    "Poder (%)",
                    value = 80,
                    min = 0,
                    max = 100,
                    step = 1
      ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
      numericInput( "th2_mean_dep_sig",
                    "Nível de significância (%)",
                    value = 5,
                    min = 0,
                    max = 100,
                    step = 1
      ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
      # selectInput('alternative_TH2_mean_pwr',
      #             'Tipo de teste de acordo com hipótese alternativa',
      #             choices = c('A média do grupo A é DIFERENTE da média do grupo B' = 'two.sided',
      #                         'A média do grupo A é MAIOR do que a média do grupo B' = 'greater',
      #                         'A média do grupo A é MENOR do que a média do grupo B' =  'less'),
      #             selected = 'two.sided'
      # ) %>% .help_buttom(body = .txt_h1),
      numericInput( "th2_mean_dep_perdas_recusa",
                    "Perdas/ Recusa (%)",
                    value = 10,
                    min = 0,
                    max = 100,
                    step = 1
      ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
    ),

    mainPanel(
      shinycssloaders::withSpinner(htmlOutput("th2_mean_dep_out"), type = 5)
    )
  ),

  rodape
)








#______------
# ANOVA  ----
#-----------.


# aba_anova_one_way <- navbarMenu("ANOVA",
#
#                                 #### One way ####

aba_anova_one_way <- tabPanel(
  "ANOVA de uma via",

  titlePanel("ANOVA de uma via"),
  wellPanel(paste0("A Análise de Variância, mais conhecida como ANOVA, é utilizada para comparar as
                     médias de três ou mais grupos independentes com o objetivo de saber se os grupos
                     diferem ou não em relação à resposta média de interesse."
                   #   "Ao calcular o tamanho da amostra para este objetivo, ",
                   # "estaremos supondo que a variável de interesse, am cada um dos grupos, segue uma distribuição normal, ",
                   # "com uma determinada média e uma determinada variância."
  )),
  tabsetPanel(
    tabPanel("Tamanho amostral por grupo",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   HTML(
                     "<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"
                   ),
                   uiOutput("anova_formula"),
                 ),

                 wellPanel(
                   checkboxInput("anova_mean_f", "Usar tamanho de efeito f", value = FALSE),

                   conditionalPanel(condition = "input.anova_mean_f == true",
                                    # actionLink("show_f_anova", "O que é a magnitude do efeito (f)?"),
                                    # br(), br(),
                                    numericInput( "f_anova_n",
                                                  "Magnitude do efeito (f)",
                                                  value = .4,
                                                  min = 0,
                                                  max = 1,
                                                  step = .01
                                    )  %>%
                                      shinyhelper::helper(type = "markdown",
                                                          title = "Tamanho de efeito f",
                                                          content = "Effect_size_f", #,includeMarkdown(file.path("Markdown", "Effect_size_f.Rmd")),
                                                          buttonLabel = "Fechar",
                                                          fade = TRUE,
                                                          colour = "#006338",
                                                          size = "l"),
                                    # %>% .help_buttom(body = paste0("Magnitude do efeito f (0.1 é considerado pequeno). ", .txt_definido_pesquisador_OU_literatura)),
                                    numericInput( "k_anova_n",
                                                  "Número de grupos",
                                                  value = 3,
                                                  min = 2,
                                                  max = Inf,
                                                  step = 1
                                    ) %>% .help_buttom(body = paste0("Nº de grupos para comparar.", .txt_definido_pesquisador))
                   ),
                   conditionalPanel(condition = "input.anova_mean_f == false",
                                    textInput( "medias_anova_n",
                                               "Médias dos grupos",
                                               value = "12.6, 14.9, 16"
                                    ) %>% .help_buttom(body = paste0("Insira as médias esperadas dos grupos separadas por vígula. Use ponto '.' como separador decimal.",
                                                                     .txt_definido_pesquisador_OU_literatura)
                                    ),
                                    numericInput( "desvio_anova_n",
                                                  "Desvio padrão esperado (homocedasticidade)",
                                                  value = 4,
                                                  min = 0,
                                                  max = Inf,
                                                  step = 1
                                    ) %>% .help_buttom(body = .txt_dp, title = "Desvio padrão esperado")
                   )
                 ),

                 numericInput( "power_anova_n",
                               "Poder (%)",
                               value = 80,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
                 numericInput( "sig_anova_n",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
                 numericInput( "one_way_perdas_recusa",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
                 # actionButton("help_anova_n", "Ajuda")
               ),

               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("anova_n"), type = 5),


                 ###  CENARIOS  ####.
                 uiOutput("cenarios_anova_oneUi")
               )
             )
    ),
    tabPanel("Poder",
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("anova_mean_f_power", "Usar tamanho de efeito f", value = FALSE),

                 conditionalPanel(condition = "input.anova_mean_f_power == true",
                                  numericInput( "n_anova_power",
                                                "Número de observações (por grupo)",
                                                value = 15,
                                                min = 1,
                                                max = Inf,
                                                step = 1
                                  ) %>% .help_buttom(body = paste0("Nº de observações por grupo, assume-se que os grupos sejam balanceados.", .txt_definido_pesquisador)),
                                  numericInput( "k_anova_power",
                                                "Número de grupos",
                                                value = 3,
                                                min = 2,
                                                max = Inf,
                                                step = 1
                                  ) %>% .help_buttom(body = "Nº de grupos para comparar."),
                                  numericInput( "f_anova_power",
                                                "Magnitude do efeito f",
                                                value = .4,
                                                min = 0,
                                                max = 1,
                                                step = .01
                                  )  %>%
                                    shinyhelper::helper(type = "markdown",
                                                        title = "Tamanho de efeito f",
                                                        content = "Effect_size_f", #,includeMarkdown(file.path("Markdown", "Effect_size_f.Rmd")),
                                                        buttonLabel = "Fechar",
                                                        fade = TRUE,
                                                        colour = "#006338",
                                                        size = "l")
                 ),
                 conditionalPanel(condition = "input.anova_mean_f_power == false",
                                  textInput( "medias_anova_power",
                                             "Médias dos grupos",
                                             value = "12.6, 14.9, 16"
                                  ) %>% .help_buttom(body = paste0("Insira as médias dos grupos separadas por vígula. Use ponto '.' como separador decimal.", .txt_definido_pesquisador_OU_literatura)),
                                  textInput( "n_anova_power2",
                                             "Tamanho amostral de cada grupo",
                                             value = "15, 16, 12"
                                  ) %>% .help_buttom(body = "Insira o tamanho amostral de cada grupo separadas por vígula. Use ponto '.' como separador decimal."),

                                  numericInput( "sigma_anova_power2",
                                                "Desvio padrão esperado (homocedasticidade)",
                                                value = 4.5,
                                                min = 0,
                                                max = Inf,
                                                step = 0.5
                                  ) %>% .help_buttom(body = .txt_dp, title = "Desvio padrão esperado")
                 ),


                 numericInput( "sig_anova_power2",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)")

               ),

               mainPanel(
                 # textOutput("anova_power")
                 htmlOutput("anova_power") %>%
                   shinycssloaders::withSpinner(type = 5)
               )
             )
    )
  ),

  rodape
)


# Two way ####

aba_anova_two_way <- tabPanel(
  "ANOVA de duas vias",


  titlePanel("ANOVA de duas vias"),
  wellPanel(paste0("A Análise de Variância de duas vias, é utilizada para comparar as
                     médias dos níveis de dois fatores."
                   #   "Ao calcular o tamanho da amostra para este objetivo, ",
                   # "estaremos supondo que a variável de interesse, am cada um dos grupos, segue uma distribuição normal, ",
                   # "com uma determinada média e uma determinada variância."
  )),
  tabsetPanel(
    tabPanel("Efeitos principais",
             sidebarLayout(
               sidebarPanel(
                 HTML("<b>ATENÇÃO!</b><br>
                             Nesta aba é calculado o tamanho de amostra para testar os efeitos principais da ANOVA de duas vias.
                             Caso deseje tamanho de amostra para o efeito de interação, utilize a aba 'Efeito de interação'.
                             <br>
                             <br> "),

                 actionLink("show_th_anova2way", "Mudar nomes"),
                 br(), br(),


                 uiOutput("k_anova_n_ui"),

                 checkboxInput("two_way_cohen", "Usar magnitude de efeito f", value = FALSE),

                 conditionalPanel(condition = "input.two_way_cohen == true", uiOutput("f_anova_n_ui")),

                 conditionalPanel(condition = "input.two_way_cohen == false",
                                  uiOutput("delta_anova_n_ui"),
                                  # uiOutput("delta_anova_n_B_ui"),
                                  # uiOutput("sigma_anova_n_A_ui"),
                                  uiOutput("sigma_anova_n_ui")
                 ),
                 HTML('<hr style="color: black;">'),
                 numericInput( "power_anova_n_two",
                               "Poder (%)",
                               value = 80,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
                 numericInput( "sig_anova_n_two",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
                 numericInput( "two_way_perdas_recusa",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
                 # actionButton("help_anova_n_two", "Ajuda")
               ),

               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("anova_n_two"), type =  5),
               )
             )
    ),

    tabPanel("Efeito da interação",
             sidebarLayout(
               sidebarPanel(
                 HTML("<b>ATENÇÃO!</b><br>
                             Nesta aba é calculado o tamanho de amostra para testar o efeito de interação da ANOVA de duas vias.
                             Caso deseje tamanho de amostra para os efeitos principais, utilize a aba 'Efeitos principais'.
                             <br>
                             <br> "),


                 checkboxInput("two_way_usar_medias", "Usar valores das médias e desvio padrão", value = FALSE),
                 conditionalPanel(condition = "input.two_way_usar_medias == true",

                                  textAreaInput(inputId = "medias_sas",
                                                label  = "Médias das combinações dos níveis",
                                                value   = "
a1 b1 13.2 $
a1 b2 22.7 $
a1 b3 26.1 $
a2 b1 7.98 $
a2 b2 16.8 $
a2 b3 26.1",
rows = 6

                                  ) %>% .help_buttom(body = paste0("Entre com as medias conforme leitura do SAS. O simbolo $ representa a entrada de novos dados.", .txt_definido_pesquisador_OU_literatura)),

numericInput( "desvio_anova_n22",
              "Desvio padrão esperado (homocedasticidade)",
              value = 4,
              min = 0,
              max = Inf,
              step = 1
) %>% .help_buttom(body = .txt_dp, title = "Desvio padrão esperado"),
numericInput( "power_anova_n_two22",
              "Poder (%)",
              value = 80,
              min = 0,
              max = 100,
              step = 1
) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
numericInput( "sig_anova_n_two22",
              "Nível de significância (%)",
              value = 5,
              min = 0,
              max = 100,
              step = 1
) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
numericInput( "two_way_perdas_recusa22",
              "Perdas/ Recusa (%)",
              value = 10,
              min = 0,
              max = 100,
              step = 1
) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
                 ),

conditionalPanel(condition = "input.two_way_usar_medias == false",
                 textInput(inputId = "two_nome_desfechoA2",
                           label   = "Descreva o nome do fator A",
                           value   = "Fator A"
                 ) %>% .help_buttom(body = .txt_outros_desfechos("Descreva o nome do fator A para que sirvam de guia no preenchimento dos valores.")),
                 textInput(inputId = "two_nome_desfechoB2",
                           label   = "Descreva o nome do fator B",
                           value   = "Fator B"
                 ) %>% .help_buttom(body = .txt_outros_desfechos("Descreva o nome do fator B para que sirvam de guia no preenchimento dos valores.")),
                 uiOutput("k_anova_n_A_ui2"),
                 uiOutput("k_anova_n_B_ui2"),
                 uiOutput("f_anova_n_A_ui2"),
                 numericInput( "power_anova_n_two2",
                               "Poder (%)",
                               value = 80,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
                 numericInput( "sig_anova_n_two2",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
                 numericInput( "two_way_perdas_recusa2",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
)
               ),

mainPanel(
  conditionalPanel(condition = "input.two_way_usar_medias == true",
                   br(), br(),
                   HTML(
                     paste0(
                       "Preencha com os dados no painel lateral e após rode a sintaxe no <a href='https://welcome.oda.sas.com/' target='_blank'> SAS Studio</a>.<br><br>"
                     )
                   ),
                   downloadButton("download_sintax_sas_anova_two_way", "Download sintax!"),
                   br(), br(),
                   HTML(
                     paste0(
                       "
                                             Se você ainda não tem uma conta SAS, <a href='https://support.sas.com/ondemand/manuals/SASStudio.pdf' target='_blank'> clique aqui</a> para criar e acessar o SAS Studio.",

                       "<br><br>Após logado e com o SAS Studio aberto, siga os passos abaixo:<br><br>",
                       "
                                             <ol>
                                             <li>Abra o SAS Studio, clique F4 para abrir uma janela de edição (CODE);</li>
                                             <li> Copie a sintaxe gerada  e cole  na janela de edição;</li>
                                             <li>Execute a sintaxe e confira os resultados na janela RESULTS.</li>
                                             </ol>
                                             "
                     )
                   )
  ),
  conditionalPanel(condition = "input.two_way_usar_medias == false",
                   htmlOutput("anova_n_two2") %>%
                     shinycssloaders::withSpinner(type =  5)
  )
)
             )
    )
# tabPanel("Poder",
#          sidebarLayout(
#            sidebarPanel(
#              radioButtons('anovabtt', 'Selecione o pacote',
#                           choices = c('pwr', 'pwr2'), selected = 'pwr'),
#              conditionalPanel("input.anovabtt == 'pwr'",
#                               numericInput( "n_anova_power",
#                                             "Número de observações (por grupo)",
#                                             value = 15,
#                                             min = 1,
#                                             max = Inf,
#                                             step = 1
#                               ),
#                               numericInput( "k_anova_power",
#                                             "Número de grupos",
#                                             value = 3,
#                                             min = 1,
#                                             max = Inf,
#                                             step = 1
#                               ),
#                               numericInput( "f_anova_power",
#                                             "Magnitude do efeito",
#                                             value = .4,
#                                             min = 0,
#                                             max = 1,
#                                             step = .1
#                               ),
#                               numericInput( "sig_anova_power",
#                                             "Nível de significância",
#                                             value = .05,
#                                             min = 0,
#                                             max = 1,
#                                             step = .01
#                               ),
#                               actionButton("help_anova_power", "Ajuda")
#              ),conditionalPanel("input.anovabtt == 'pwr2'",
#                                 numericInput( "n_anova_power2",
#                                               "Número de observações (por grupo)",
#                                               value = 15,
#                                               min = 1,
#                                               max = Inf,
#                                               step = 1
#                                 ),
#                                 numericInput( "k_anova_power2",
#                                               "Número de grupos",
#                                               value = 3,
#                                               min = 1,
#                                               max = Inf,
#                                               step = 1
#                                 ),
#                                 numericInput( "sig_anova_power2",
#                                               "Nível de significância",
#                                               value = .05,
#                                               min = 0,
#                                               max = 1,
#                                               step = .01
#                                 )
#                                 ,
#                                 numericInput( "sigma_anova_power2",
#                                               "Desvio padrão da variável de interesse",
#                                               value = .7,
#                                               min = 0,
#                                               max = Inf,
#                                               step = .01
#                                 ),
#                                 numericInput( "delta_anova_power2",
#                                               "Menor diferença entre os grupos",
#                                               value = .8,
#                                               min = 0,
#                                               max = Inf,
#                                               step = .01
#                                 ),
#                                 actionButton("help_anova_power2", "Ajuda")
#              )
#            ),
#
#            mainPanel(
#              htmlOutput("anova_power")
#            )
#          )
# )
  ),

rodape
)






#___________-------
# Associacao   ----
#-----------------.


aba_associacao <- tabPanel(
  "Qui-quadrado",


  titlePanel("Teste de Associação para variáveis categóricas"),
  wellPanel("Essa técnica é utilizada quando se deseja descobrir se existe associação entre duas
                     variáveis categóricas, geralmente agrupadas em tabelas de contingência."),
  tabsetPanel(
    tabPanel("Tamanho amostral",
             sidebarLayout(
               sidebarPanel(
                 textInput(inputId = "chisq_desfecho",
                           label   = "Descreva as variáveis que deseja associar",
                           value   = "X1 e X2"
                 ) %>% .help_buttom(body = .txt_outros_desfechos("Descreva o nome das variáveis que desejas associar para que sirvam de guia no preenchimento.")),

                 # checkboxInput(inputId = "chisq_entrar_tabela",
                 #               label   = "Entrar com tabela contingência",
                 #               value   = FALSE
                 #
                 # ),

                 selectInput(inputId = "chisq_input",
                             label = "Qual o tipo de informação de entrada?",
                             choices = c("Magnitude do efeito w" = 1,
                                         "Tabela de contingência com proporções" = 2,
                                         "Tabela de contingência com valores absolutos" = 3),
                             selected = 1
                 ),


                 conditionalPanel("input.chisq_input == 1",
                                  numericInput( "w_chisq_n",
                                                "Magnitude do efeito w",
                                                value = .3,
                                                min = 0,
                                                max = 1,
                                                step = .01
                                  ) %>% .help_buttom(body = paste0("Magnitude do efeito w (0.1 é considerado pequeno)", .txt_definido_pesquisador_OU_literatura)),


                                  numericInput( "df_chisq_n",
                                                "Graus de liberdade",
                                                value = 3,
                                                min = 0,
                                                max = Inf,
                                                step = 1
                                  ) %>% .help_buttom(body = paste0("Número de graus de liberdade da estatística de teste. Pode ser obtido realizando o cálculo (L-1)x(C-1), onde L é o número de categorias da primeira variável e C da segunda variável.", .txt_definido_pesquisador_OU_literatura))
                 ),

                 uiOutput("chis_tabela_contUi"),
                 uiOutput("chis_tabela_cont_valoresUi"),

                 numericInput( "power_chisq_n",
                               "Poder (%)",
                               value = 80,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
                 numericInput( "sig_chisq_n",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
                 numericInput( "chisq_perdas_recusa",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
                 # actionButton("help_chisq_n", "Ajuda")
               ),

               mainPanel(
                 conditionalPanel("input.chisq_input != 1",
                                  uiOutput("chisq_tab_contUi") %>%
                                    shinycssloaders::withSpinner(type = 5)

                 ),
                 shinycssloaders::withSpinner(htmlOutput("chisq_n"), type = 5),

                 uiOutput("cenarios_chisq_Ui")
               )
             )
    )
    # tabPanel("Poder",
    #          sidebarLayout(
    #            sidebarPanel(
    #              numericInput( "n_chisq_power",
    #                            "Número total de observações",
    #                            value = 100,
    #                            min = 1,
    #                            max = Inf,
    #                            step = 1
    #              ),
    #              numericInput( "w_chisq_power",
    #                            "Magnitude do efeito",
    #                            value = .3,
    #                            min = 0,
    #                            max = 1,
    #                            step = .01
    #              ),
    #              numericInput( "df_chisq_power",
    #                            "Graus de liberdade",
    #                            value = 3,
    #                            min = 0,
    #                            max = Inf,
    #                            step = 1
    #              ),
    #              numericInput( "sig_chisq_power",
    #                            "Nível de significância",
    #                            value = .05,
    #                            min = 0,
    #                            max = 1,
    #                            step = .01
    #              )
    #              # actionButton("help_chisq_power", "Ajuda")
    #            ),
    #
    #                mainPanel(
    #                  shinycssloaders::withSpinner(htmlOutput("chisq_power"), type = 5)
    #                )
    #              )
    #     )
  ),

  rodape
)







#___________-------
# Correlacao   ----
#-----------------.

#tabPanel("Correlação",
aba_correlacao <- tabPanel(
  "Correlação",

  tags$head(
    tags$style(
      HTML("div.MathJax_Display{
                               text-align: left !important;
                                    }")
    )
  ),
  # tags$head(tags$style(HTML(as.character(
  #   ".js-irs-0 .irs-single, .js-irs-0 .irs-from, .js-irs-0 .irs-to, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar{ background: DeepPink ;}"
  # )))),
  titlePanel("Coeficientes de correlação"),
  wellPanel("Muitas vezes o objetivo do estudo é analisar se duas variáveis variam conjuntamente. Nestes casos, a estatística de interesse é o coeficiente de correlação.
                           Os coeficiente de correlação avaliam a direção e o grau de alinhamento entre duas variáveis.
                           Assume valores que variam de -1 (correlação negativa perfeita) a 1 (correlação positiva perfeita).",
            'Mais detalhes sobre o uso dessa aba em ',
            HTML('<b><a https://doi.org/10.22491/2357-9730.112466" target="_blank">PSS Health: como calcular tamanho de amostra para estimar média, proporção e correlação</a></b>.')
  ),
  tabsetPanel(

    tabPanel("Estimar",
             sidebarLayout(
               sidebarPanel(
                 # textInput(inputId = "cor_nome_desfechos",
                 #           label   = "Descreva os nomes dos desfechos",
                 #           value   = "X1 e X2"
                 # ) %>% .help_buttom(body = "Descreva o nome das variáveis para que sirvam de guia no preenchimento."),
                 radioButtons(inputId = "r_r_coeficiente",
                              label   = "Calcular para",
                              choices = c("Pearson"  = "pearson",
                                          "Spearman" = "spearman",
                                          "Kendall"  = "kendall"),
                              selected = "pearson",
                              inline   = TRUE),

                 textInput(inputId = "corr_est_desfecho",
                           label   = "Descreva as variáveis que deseja correlacionar",
                           value   = "X1 e X2"
                 ) %>% .help_buttom(body = .txt_outros_desfechos("Descreva o nome das variáveis que desejas correlacionar para que sirvam de guia no preenchimento.")),

                 numericInput( "r_r_n_est",
                               "Coeficiente esperado",
                               value = .5,
                               min = 0,
                               max = 1,
                               step = .01
                 ) %>% .help_buttom(body =
                                      paste0(
                                        "Coeficiente de correlação esperado, assumindo valores entre -1 e 1. Quando mais distante de zero, maior é a correlação entre as variáveis. ",
                                        "Maiores informações em ",
                                        '<a href="https://seer.ufrgs.br/hcpa/article/view/98944/pdf" target="_blank">Leotti et al. 2019</a>.',
                                        .txt_definido_pesquisador_OU_literatura
                                      )
                 ),
                 numericInput( "precisao_rho",
                               "Amplitude do intervalo",
                               value = .4,
                               min = 0,
                               max = 1,
                               step = .1
                 ) %>% .help_buttom(body = .txt_amplitude, title = "Amplitude do intervalo"),
                 numericInput( "conf_r_n_est",
                               "Nível de confiança (%)",
                               value = 95,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_confianca, title = "Nível de confiança (%)"),
                 numericInput( "corr_TH_perdas_recusa_est",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
                 # actionButton("help_r_n_est", "Ajuda")
               ),

               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("r_nIest"), type = 5),

                 ###  CENARIOS  ####.


                 uiOutput("cenarios_est_correlacaoUi")
               )
             )

    ),
    tabPanel("Testar",
             sidebarLayout(
               sidebarPanel(
                 HTML("<b>Cálculo realizado para o coeficiente de correlação linear de Pearson</b>"),
                 wellPanel(
                   HTML(
                     "<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"
                   ),
                   uiOutput("correlacao_th_formula"),
                 ),
                 textInput(inputId = "corr_testar_desfecho",
                           label   = "Descreva as variáveis que deseja correlacionar",
                           value   = "X1 e X2"
                 ) %>% .help_buttom(body = .txt_outros_desfechos("Descreva o nome das variáveis para que sirvam de guia no preenchimento.")),

                 numericInput( "r_r_n",
                               "Coeficiente esperado",
                               value = .5,
                               min = 0,
                               max = 1,
                               step = .1
                 ) %>% .help_buttom(body =
                                      paste0(
                                        "Coeficiente de correlação linear de Pearson esperado, assumindo valores entre -1 e 1. Quando mais distante de zero, maior é a correlação entre as variáveis. ",
                                        "Maiores informações em ",
                                        '<a href="https://seer.ufrgs.br/hcpa/article/view/98944/pdf" target="_blank">Leotti et al. 2019</a>.',
                                        .txt_definido_pesquisador_OU_literatura
                                      )
                 ),
                 numericInput( "r_r_h0_n",
                               "Coeficiente sob a hipótese nula",
                               value = 0,
                               min = -1,
                               max = 1,
                               step = .1
                 ) %>% .help_buttom(body = paste0(
                   "Coeficiente de correlação linear de Pearson sob a hipótese nula. ",
                   "Maiores informações em ",
                   '<a href="https://seer.ufrgs.br/hcpa/article/view/93649/pdf" target="_blank">Hirakata et al. 2019</a>.',
                   .txt_definido_pesquisador
                 )),
                 numericInput( "r_n_parcial",
                               "Número de variáveis para correlação parcial",
                               value = 0,
                               min = 0,
                               max = Inf,
                               step = 1
                 ) %>% .help_buttom(body = paste0("Número de variáveis para correlação parcial.", .txt_definido_pesquisador)),
                 numericInput( "power_r_n",
                               "Poder (%)",
                               value = 80,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
                 numericInput( "sig_r_n",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
                 uiOutput('alternative_r_nUi'),
                 numericInput( "corr_TH_perdas_recusas",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
                 # actionButton("help_r_n", "Ajuda")
               ),

               mainPanel(

                 shinycssloaders::withSpinner(htmlOutput("r_n"), type = 5),
                 # shinycssloaders::withSpinner(htmlOutput("r_interval_th"), type = 5),

                 ###  CENARIOS  ####.
                 uiOutput("cenarios_th_correlacaoUi")

               )
             )
    ),

    tabPanel("Poder",
             sidebarLayout(
               sidebarPanel(
                 HTML("<b>Cálculo realizado para o coeficiente de correlação linear de Pearson</b>"),

                 numericInput( "n_r_power",
                               "Número de observações",
                               value = 20,
                               min = 1,
                               max = Inf,
                               step = 1
                 ) %>% .help_buttom(body = "Número de observações"),
                 numericInput( "r_r_power",
                               "Coeficiente esperado",
                               value = .5,
                               min = 0,
                               max = 1,
                               step = .01
                 ) %>% .help_buttom(body = paste0(
                   "Coeficiente de correlação linear de Pearson sob a hipótese nula. ",
                   "Maiores informações em ",
                   '<a href="https://seer.ufrgs.br/hcpa/article/view/93649/pdf" target="_blank">Hirakata et al. 2019</a>.',
                   .txt_definido_pesquisador
                 )),
                 numericInput( "sig_r_power",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
                 # selectInput('alternative_r_power',
                 #             'Tipo de teste de acordo com hipótese alternativa:',
                 #             choices = c('A correlação é DIFERENTE de zero.' = 'two.sided',
                 #                         'A correlação é MAIOR do que zero.' = 'greater',
                 #                         'A correlação é MENOR do que zero.' =  'less'
                 #             ),
                 #             selected = 'two.sided'
                 # ) %>% .help_buttom(body = "O teste pode ser bilateral, superior ou inferior. Nos dois útilmos casos, a hipótese alternativa é de que o parâmetro é maior ou menor do que o valor de referência, respectivamente.")
               ),

               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("r_power"), type = 5)
               )
             )
    )
  ),

  rodape
)






#___________-----
# Inclinacao  ----

aba_inclinacao_linear <- tabPanel(
  "Linear",
  titlePanel("Coeficiente de inclinação da reta para um modelo de regressão linear simples."),
  wellPanel("Quando um estudo quer observar a variação conjunta de duas variáveis, supondo uma relação linear, ",
            "o pesquisador pode estar interessado em estimar o coeficiente de inclinação da relação entre elas. "
            # "Ao calcular o tamanho da amostra para este objetivo, ",
            # "estaremos supondo que o desfecho de interesse segue uma distribuição normal, ",
            # "com uma determinada média e uma determinada variância constante ao longo da reta de regressão."
  ),
  tabsetPanel(

    tabPanel("Testar",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   HTML(
                     "<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"
                   ),
                   uiOutput("inclinacao_reg_formula"),
                 ),
                 actionLink("inclinacao_nomes", "Mudar nomes"),
                 br(), br(),

                 checkboxInput(inputId = "inclinacao_usar_r2",
                               label   = "Calcular com base no valor do R²",
                               value   = FALSE
                 ),
                 uiOutput("inclinacao_inclinacaoUi"),
                 conditionalPanel("input.inclinacao_usar_r2 == true",
                                  numericInput( "inclinacao_reg_r2",
                                                "Coeficiente de determinação esperado",
                                                value = 0.2,
                                                min = 0,
                                                max = Inf,
                                                step = 1
                                  ) %>% .help_buttom(body = paste0(
                                    "O coeficiente de determinação é uma medida de quão bem o modelo de regressão descreve os dados observados. ",
                                    "É o % da variação total de Y que é explicada pela variação de X. ",
                                    "Por exemplo, suponhamos um modelo com um R² = 0,49, então 49% da variação de Y pode ser explicada pela variação de X",
                                    .txt_definido_pesquisador_OU_literatura),
                                    title = "Coeficiente de determinação esperado (R²)")
                 ),
                 numericInput( "power_inclinacao_reg",
                               "Poder (%)",
                               value = 80,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
                 numericInput( "conf_inclinacao_reg",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
                 numericInput( "inclinacao_reg_perdas_recusa_est",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
                 # actionButton("help_r_n_est", "Ajuda")
               ),

               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("out_inclinacao_reg"), type = 5)
               )
             )
    )
  )
)





#___________-----
# Logistica  ----
aba_logistica <- tabPanel(
  "Logística",
  titlePanel("Regressão logística univariável"),
  wellPanel("Nesta aba é possível calcular o tamanho de amostra para testar se a razão de chances (RC)",
            "é igual a 1 em um modelo de regressão logística simples (univariável).",
            "No caso em que a variável preditora é contínua, estaremos supondo que sua distribuição adere à normal.",

  ),
  tabsetPanel(

    tabPanel("Testar",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(HTML("<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"),
                           uiOutput("rc_logistic_formula")),

                 radioButtons(inputId = "logistic_tipo_variavel",
                              label   = "Qual tipo de variável independente?",
                              choices =  c("Binária"  = 0,
                                           "Contínua" = 1),
                              selected = 0,
                              inline   = TRUE),

                 conditionalPanel("input.logistic_tipo_variavel == 1",

                                  numericInput( "logistic_or_continuous",
                                                "Razão de chances esperada",
                                                value = 0.5,
                                                min = 0,
                                                max = Inf,
                                                step = 1
                                  ) %>% .help_buttom(body = .txt_razao_chance, title = "Razão de chance"),
                                  numericInput( "logistic_rate_mean",
                                                "% de eventos na média da variável preditora",
                                                value = 50,
                                                min  = 0,
                                                max  = 100,
                                                step = 1
                                  ) %>% .help_buttom(body = paste0("O percentual de eventos esperados na média da variável preditora contínua.", .txt_definido_pesquisador_OU_literatura))
                 ),

                 conditionalPanel("input.logistic_tipo_variavel == 0",
                                  numericInput( "logistic_cat_controle",
                                                paste0("% esperado no grupo Não expostos"),
                                                value = 15,
                                                min = 0,
                                                max = 100,
                                                step = 1
                                  ) %>% .help_buttom(body = paste0("O percentual esperado de eventos no grupo Não expostos, geralmente é utilizado algum valor com base na literatura.", .txt_definido_pesquisador_OU_literatura)),

                                  selectInput('or_th_estatistica',
                                              'Medida do grupo expostos:',
                                              choices = c("Razão de chance (Expostos/Não expostos)" = "odds",
                                                          "Risco relativo (Expostos/Não expostos)" = "ratio",
                                                          "% esperado no grupo Expostos" = "percent"),
                                              selected = 'odds'
                                  ),

                                  conditionalPanel(condition = 'input.or_th_estatistica == "percent"',
                                                   numericInput( "logistic_cat_p2",
                                                                 paste0("% esperado no grupo Expostos"),
                                                                 value = 45,
                                                                 min = 0,
                                                                 max = 100,
                                                                 step = 1
                                                   ) %>% .help_buttom(body = paste0("O percentual esperado de eventos no grupo Expostos, geralmente é utilizado algum valor com base na literatura.", .txt_definido_pesquisador_OU_literatura))
                                  ),
                                  conditionalPanel(condition = 'input.or_th_estatistica == "ratio"',
                                                   numericInput( "logistic_cat_ratio",
                                                                 "Risco relativo (Expostos/Não expostos)",
                                                                 value = 2,
                                                                 min = 0,
                                                                 max = Inf,
                                                                 step = 0.1
                                                   )  %>% .help_buttom(body = .txt_risco_relativo, title = "Risco relativo/ Razão de prevalências")
                                  ),
                                  conditionalPanel(condition = 'input.or_th_estatistica == "odds"',
                                                   numericInput( "logistic_cat_odds",
                                                                 "Razão de chance (Expostos/Não expostos)",
                                                                 value = 2,
                                                                 min = 0,
                                                                 max = Inf,
                                                                 step = 0.1
                                                   ) %>% .help_buttom(body = .txt_razao_chance, title = "Razão de chance")
                                  ),
                                  numericInput( "logistic_cat_k",
                                                "Balanço da amostra (Não expostos:Expostos).",
                                                value = 1,
                                                min   = 0,
                                                max   = Inf,
                                                step  = .5
                                  ) %>% .help_buttom(body = .txt_balanceamento, title = "Balanceamento")

                 ),

                 numericInput( "power_logistic",
                               "Poder (%)",
                               value = 80,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
                 numericInput( "sig_logistic",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
                 numericInput( "logistic_perdas_recusa_est",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
               ),

               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("out_logistic"), type = 5)
               )
             )
    ),

    tabPanel("Estimar",
             sidebarLayout(
               sidebarPanel(
                 numericInput( "logistic_est_controle",
                               paste0("% esperado no grupo Não exposto"),
                               value = 45,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = paste0("O percentual esperado de eventos no grupo Não exposto.", .txt_definido_pesquisador_OU_literatura)),

                 selectInput('or_est_estatistica',
                             'Medida do grupo expostos:',
                             choices = c("Razão de chance (Expostos/Não expostos)" = "odds",
                                         "Risco relativo (Expostos/Não expostos)" = "ratio",
                                         "% esperado no grupo Expostos" = "percent"),
                             selected = 'odds'
                 ),

                 conditionalPanel(condition = 'input.or_est_estatistica == "percent"',
                                  numericInput( "logistic_est_p2",
                                                paste0("% esperado no grupo Expostos"),
                                                value = 15,
                                                min = 0,
                                                max = 100,
                                                step = 1
                                  ) %>% .help_buttom(body = paste0("O percentual esperado de eventos no grupo Expostos, geralmente é utilizado algum valor com base na literatura.", .txt_definido_pesquisador_OU_literatura))
                 ),
                 conditionalPanel(condition = 'input.or_est_estatistica == "ratio"',
                                  numericInput( "logistic_est_ratio",
                                                "Risco relativo (Expostos/Não expostos)",
                                                value = 0.5,
                                                min = 0,
                                                max = Inf,
                                                step = 0.1
                                  )  %>% .help_buttom(body = .txt_risco_relativo, title = "Risco relativo/ Razão de prevalências")
                 ),
                 conditionalPanel(condition = 'input.or_est_estatistica == "odds"',
                                  numericInput( "logistic_est_odds",
                                                "Razão de chance (Expostos/Não expostos)",
                                                value = 0.5,
                                                min = 0,
                                                max = Inf,
                                                step = 0.1
                                  ) %>% .help_buttom(body = .txt_razao_chance, title = "Razão de chance")
                 ),
                 numericInput( "logistic_est_k",
                               "Balanço da amostra (Não expostos:Expostos).",
                               value = 1,
                               min   = 0,
                               max   = Inf,
                               step  = .5
                 ) %>% .help_buttom(body = .txt_balanceamento, title = "Balanceamento"),


                 numericInput( "logistic_est_amplitude",
                               "Amplitude desejada",
                               value = 0.5,
                               min = 0,
                               max = Inf,
                               step = .1
                 ) %>% .help_buttom(body = .txt_amplitude, title = "Amplitude do intervalo"),
                 numericInput( "logistic_est_confiança",
                               "Nível de confiança (%)",
                               value = 95,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_confianca, title = "Nível de confiança (%)"),
                 selectInput("logistic_est_metodo",
                             "Método utilizado para calcular o intervalo de confiança",
                             choices = c("indip_smooth", "gart", "woolf"),
                             selected = "indip_smooth"
                 ),
                 numericInput( "logistic_perdas_recusa_estimar",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
               ),

               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("out_logistic_estimar"), type = 5)
               )
             )
    )
  ),

  rodape
)



#____----
# Cox ----
#---------.

aba_surv_cox <- tabPanel(
  "Cox (sobrevivência)",
  titlePanel("Regressão de cox"),
  wellPanel("Cálculo do tamanho da amostra para a comparação de curvas de sobrevivência entre dois grupos sob o modelo de riscos proporcionais de Cox para ensaios clínicos."
  ),
  tabsetPanel(

    tabPanel("Testar",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(HTML("<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"),
                           withMathJax("$$H_0: HR = 1 \\text{  vs  } H_1: HR \\neq 1$$")),

                 radioButtons(inputId = "cox_tipo_variavel",
                              label   = "Qual tipo de variável independente?",
                              choices =  c("Binária"  = 0,
                                           "Contínua" = 1),
                              selected = 0,
                              inline   = TRUE),
                 numericInput( "cox_hr_esperado",
                               "Hazard ratio esperado",
                               value = 2,
                               min = 0,
                               max = Inf,
                               step = 1
                 ) %>% .help_buttom(body = paste0("O hazard ratio (HR), ou razão de riscos proporcionais, é calculado quando o desfecho de interesse é o tempo até o evento ocorrer. ",
                                                  "Desta forma, a razão de riscos proporcionais é obtida dividindo o risco (hazard) de desenvolver o evento em um grupo pelo risco de outro grupo. ",
                                                  "Maiores informações em ",
                                                  '<a href="https://seer.ufrgs.br/hcpa/article/view/96394/pdf" target="_blank">Castro et al. 2019</a>.',
                                                  .txt_definido_pesquisador_OU_literatura
                 ), title = "Hazard ratio"
                 ),


                 conditionalPanel("input.cox_tipo_variavel == 0",

                                  HTML(
                                    "<b><font size = '2.99'>Probabilidade (%) de sobrevivência até o final do seguimento no grupo</font></b><br>"
                                  ),

                                  div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                                      numericInput( "cox_failure_trat",
                                                    "Tratamento",
                                                    value = 20,
                                                    min  = 0,
                                                    max  = 100,
                                                    step = 1)
                                  ),
                                  div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                      numericInput( "cox_failure_control",
                                                    "Controle",
                                                    value = 20,
                                                    min  = 0,
                                                    max  = 100,
                                                    step = 1
                                      ) %>% .help_buttom(body = paste0("Probabilidade (%) de sobrevivência até o final do seguimento", .txt_definido_literatura))
                                  ),

                                  numericInput( "cox_balanceamento",
                                                "Balanceamento da amostra (Tratamento:Controle)",
                                                value = 1,
                                                min = 0,
                                                max = Inf,
                                                step = 0.5
                                  ) %>% .help_buttom(body = .txt_balanceamento, title = "Balanceamento")
                 ),



                 conditionalPanel("input.cox_tipo_variavel == 1",
                                  numericInput( "cox_failure_continua",
                                                "Probabilidade (%) de sobrevivência até o final do seguimento",
                                                value = 100 -73.8,
                                                min  = 0,
                                                max  = 100,
                                                step = 1
                                  ) %>% .help_buttom(body = paste0("Probabilidade (%) de sobrevivência até o final do seguimento.", .txt_definido_literatura)),

                                  numericInput( "cox_desvio_padrao",
                                                "Desvio padrão da variável independente",
                                                value = 0.3126,
                                                min  = 0,
                                                max  = Inf,
                                                step = 1
                                  ) %>% .help_buttom(body = .txt_dp, title = "Desvio padrão esperado"),

                                  numericInput( "cox_r2",
                                                "Coeficiente de correlação múltipla",
                                                value = 0,
                                                min  = -1,
                                                max  = 1,
                                                step = .2
                                  ) %>% .help_buttom(body = paste0("Coeficiente de correlação múltipla entre a covariável de interesse e outras covariáveis.
                                                     Defina zero (default) se não haverá outras variáveis independentes.", .txt_definido_pesquisador_OU_literatura))
                 ),


                 numericInput( "cox_power",
                               "Poder (%)",
                               value = 80,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
                 numericInput( "cox_significancia",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
                 numericInput( "cox_perdas_recusa_est",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
                 # actionButton("help_r_n_est", "Ajuda")
               ),

               mainPanel(
                 shinycssloaders::withSpinner(htmlOutput("cox_out"), type = 5),

                 ###  CENARIOS  ####.
                 uiOutput("cenarios_testar_cox")
               )
             )
    )
  ),

  rodape
)






#__________----
# aba_obter_dp -----

aba_obter_dp <- tabPanel(
  "Obter desvio padrão",
  titlePanel("Obter o desvio padrão de outras estatísticas"),



  wellPanel(
    paste0("Em muitos cálculos de tamanho amostral é necessário informar o desvio padrão da variável de interesse, ",
           "no entanto é encontrado na literatura somente outras estatísticas, como ",
           "o erro padrão, intervalo de confiança, valor de t ou p. "
           # "As fórmula utilizadas supõem que a amostra foi selecionada através de uma amostragem aleatória simples, ",
           # "as observações são independentes e seguem uma distribuição normal."
    )
  ),


  sidebarLayout(
    sidebarPanel(

      wellPanel(
        selectInput(
          inputId = "ferramentes_desvio_padrao_statistic",
          label   = "Escolha a estatística",
          choices = c("Erro padrão",
                      "Intervalo de confiança",
                      "Estatística t",
                      "Valor de p",
                      "Da diferença entre grupos pareados"),
          selected = "Erro padrão"
        )
      ),

      # Erro padrao
      conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Erro padrão"',

                       numericInput( "ferramentas_ep_erro_padrao",
                                     "Erro padrão",
                                     value = 10,
                                     min = 0,
                                     max = Inf,
                                     step = 1
                       ) %>% .help_buttom(body = "Erro padrão relatado na literatura, supondo que as condições descritas acima estejam satisfeitas.")
      ),


      # Intervalo de confianca
      conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Intervalo de confiança"',
                       numericInput( "ferramentas_ic_ic",
                                     "Limite do intervalo de confiança",
                                     value = 4.06,
                                     min = -Inf,
                                     max = Inf,
                                     step = 1
                       ) %>% .help_buttom(body = "Um dos valores do intervalo de confiança relatado na literatura, supondo que as condições descritas acima estejam satisfeitas.
                                          Pode ser o limite inferior ou o superior.")
      ),

      # Estatistica t
      conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Estatística t"',
                       numericInput( "ferramentas_ic_t",
                                     "Estatística t",
                                     value = 4.06,
                                     min = -Inf,
                                     max = Inf,
                                     step = 1
                       ) %>% .help_buttom(body = "Estatística t.")
      ),

      # Valor de p
      conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Valor de p"',
                       numericInput( "ferramentas_ic_p",
                                     "Valor de p",
                                     value = 0.003,
                                     min = 0,
                                     max = 1,
                                     step = .01
                       ) %>% .help_buttom(body = "Valor de p."),
                       p("ATENÇÃO! Quando o p não for definido de forma exata o cálculo não será correto. ",
                         "Utilizar o maior número de casas decimais possíveis, principalmente se p < 0.001.")
      ),


      # Ho para estatística t e valor de p
      conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Estatística t" || input.ferramentes_desvio_padrao_statistic == "Valor de p"',

                       numericInput( "ferramentas_ic_t_h0",
                                     "Estatística sob hipótese nula",
                                     value = 0,
                                     min = -Inf,
                                     max = Inf,
                                     step = 1
                       ) %>% .help_buttom(body = "Estatística sob H0.")
      ),


      # Nao precisa da media para o erro padrao
      conditionalPanel('input.ferramentes_desvio_padrao_statistic != "Erro padrão" && input.ferramentes_desvio_padrao_statistic != "Da diferença entre grupos pareados"',

                       numericInput( "ferramentas_ic_media",
                                     "Média",
                                     value = 5,
                                     min = -Inf,
                                     max = Inf,
                                     step = 1
                       ) %>% .help_buttom(body = "Média do estudo na qual o intervalo de confiança foi extraído.")
      ),


      # Nao usa na Da diferença do baseline
      conditionalPanel('input.ferramentes_desvio_padrao_statistic != "Da diferença entre grupos pareados"',

                       numericInput( "ferramentas_ic_n",
                                     "Tamanho da amostra",
                                     value = 20,
                                     min = 2,
                                     max = Inf,
                                     step = 1
                       ) %>% .help_buttom(body = "Tamanho da amostra do estudo na qual o intervalo de confiança foi extraído.")
      ),


      # So para o IC
      conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Intervalo de confiança"',
                       numericInput( "ferramentas_ic_conf",
                                     "Nível de confiança (%)",
                                     value = 95,
                                     min = 0,
                                     max = 100,
                                     step = 1
                       ) %>% .help_buttom(body = .txt_confianca, title = "Nível de confiança (%)")
      ),


      # Imputing a change-from-baseline standard deviation using a correlation coefficient
      conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Da diferença entre grupos pareados"',

                       numericInput( "ferramentas_sd_baseline",
                                     "Desvio padrão do Grupo 1",
                                     value = 4,
                                     min = 0,
                                     max = Inf,
                                     step = 1) %>%
                         .help_buttom(body = "Em estudos longitudinais o Grupo 1 pode ser entendido como o Momento 1."),

                       numericInput( "ferramentas_sd_follow",
                                     "Desvio padrão do Grupo 2",
                                     value = 4.4,
                                     min = 0,
                                     max = Inf,
                                     step = 1) %>%
                         .help_buttom(body = "Em estudos longitudinais o Grupo 2 pode ser entendido como o Momento 2."),

                       numericInput( "ferramentas_sd_correlation",
                                     "Correlação entre as medidas do Grupo1 e Grupo2",
                                     value = 0.8,
                                     min = -1,
                                     max = 1,
                                     step = .1)

      ),




      numericInput( "ferramentas_desvio_padrao_decimals",
                    "Número de casas decimais",
                    value = 3,
                    min = 0,
                    max = Inf,
                    step = 1
      ) %>% .help_buttom(body = "Número de casas decimais para exibir o desvio padrão calculado.")

    ), # Fecha sidebar


    mainPanel(

      conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Da diferença do baseline"',
                       HTML(
                         '<a href="https://handbook-5-1.cochrane.org/chapter_16/16_1_3_2_imputing_standard_deviations_for_changes_from_baseline.htm" target="_blank">Maiores informações aqui.</a>'
                       ),
                       br()
      ),


      htmlOutput("ferramentas_desvio_padrao_valor") %>%
        shinycssloaders::withSpinner(type = 5),

      br(), br(), br(),
      p("Fórmula utilizada:"),

      br(),
      uiOutput("ferramentas_desvio_padrao_formulas") %>%
        shinycssloaders::withSpinner(type = 5)


    ) # Fecha main painel
  ) # Fecha layout de painel

  ,

  rodape

)






# aba_ic_sd ----.

#
# aba_ic_sd <- tabPanel("IC/ t / p valor para desvio padrão",
#                       titlePanel("Converte o intervalo de confiança / estatística t / p valor para o desvio padrão"),
#                       wellPanel(paste0("Em muitos cálculos é necessário informar o desvio padrão da variável de interesse, ",
#                                        "no entanto é encontrado na literatura somente o valor do intervalo de confiança da estimativa da média. ",
#                                        "Acreditando que o cálculo do intervalo de confiança foi realizado supondo que a amostra foi selecionada através de uma amostragem aleatória simples e ",
#                                        "que as observações são independentes e seguem uma distribuição normal, ",
#                                        "esta ferramenta pode ser utilizada para fazer a conversão."
#                       )
#                       ),
#                       sidebarLayout(
#                         sidebarPanel(
#
#                           selectInput(
#                             inputId = "ferramentes_desvio_padrao_statistic",
#                             label   = "Escolha a estatística",
#                             choices = c("Intervalo de confiança",
#                                         "Estatística t",
#                                         "Valor de p"),
#                             selected = "Intervalo de confiança"
#                           ),
#
#                           conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Intervalo de confiança"',
#                                            numericInput( "ferramentas_ic_ic",
#                                                          "Limite do intervalo de confiança",
#                                                          value = 4.06,
#                                                          min = -Inf,
#                                                          max = Inf,
#                                                          step = 1
#                                            ) %>% .help_buttom(body = "Um dos valores do intervalo de confiança relatado na literatura, supondo que as condições descritas acima estejam satisfeitas.
#                                           Pode ser o limite inferior ou o superior.")
#                           ),
#
#                           conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Estatística t"',
#                                            numericInput( "ferramentas_ic_t",
#                                                          "Estatística t",
#                                                          value = 4.06,
#                                                          min = -Inf,
#                                                          max = Inf,
#                                                          step = 1
#                                            ) %>% .help_buttom(body = "Estatística t.")
#                           ),
#
#                           conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Valor de p"',
#                                            numericInput( "ferramentas_ic_p",
#                                                          "Valor de p",
#                                                          value = 0.003,
#                                                          min = 0,
#                                                          max = 1,
#                                                          step = .01
#                                            ) %>% .help_buttom(body = "Valor de p."),
#                                            p("ATENÇÃO! Quando o não for definido de forma exata o cálculo não será correto. ",
#                                              "Utilizar o maior número de casas decimais possíveis, principalmente se p < 0.001.")
#                           ),
#
#                           conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Estatística t" || input.ferramentes_desvio_padrao_statistic == "Valor de p"',
#
#                                            numericInput( "ferramentas_ic_t_h0",
#                                                          "Estatística sob hipótese nula",
#                                                          value = 0,
#                                                          min = -Inf,
#                                                          max = Inf,
#                                                          step = 1
#                                            ) %>% .help_buttom(body = "Estatística sob H0.")
#                           ),
#
#                           numericInput( "ferramentas_ic_media",
#                                         "Média",
#                                         value = 5,
#                                         min = -Inf,
#                                         max = Inf,
#                                         step = 1
#                           ) %>% .help_buttom(body = "Média do estudo na qual o intervalo de confiança foi extraído."),
#                           numericInput( "ferramentas_ic_n",
#                                         "Tamanho da amostra",
#                                         value = 20,
#                                         min = 2,
#                                         max = Inf,
#                                         step = 1
#                           ) %>% .help_buttom(body = "Tamanho da amostra do estudo na qual o intervalo de confiança foi extraído."),
#
#                           conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Intervalo de confiança"',
#                                            numericInput( "ferramentas_ic_conf",
#                                                          "Nível de confiança (%)",
#                                                          value = 95,
#                                                          min = 0,
#                                                          max = 100,
#                                                          step = 1
#                                            ) %>% .help_buttom(body = .txt_confianca, title = "Nível de confiança (%)")
#                           ),
#
#                           numericInput( "ferramentas_ic_decimals",
#                                         "Número de casas decimais",
#                                         value = 3,
#                                         min = 0,
#                                         max = Inf,
#                                         step = 1
#                           ) %>% .help_buttom(body = "Número de casas decimais para exibir o desvio padrão calculado.")
#                         ),
#
#                         mainPanel(
#                           # p("Pelos dados fornecidos, temos que"),
#                           htmlOutput("ferramentas_icOut") %>%
#                             shinycssloaders::withSpinner(type = 5),
#
#                           br(), br(),
#                           p("Fórmula utilizada:"),
#
#
#                           #https://stackoverflow.com/questions/23479249/r-shiny-display-formula
#                           htmlOutput("ferramentas_epOut1"),
#                           br(), br(),
#                           p("Fórmula utilizada:"),
#                           br(), br(),
#                           uiOutput("ferramentas_epOut2")
#
#
#                           br(), br(),
#                           conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Intervalo de confiança"',
#
#                                            withMathJax(
#                                              paste0("$$",
#                                                     "\\text{Desvio padrão} = \\dfrac{\\text{|} IC - \\bar X \\text{|} }",
#                                                     "{t_{\\alpha/2, n-1}} \\sqrt n",
#                                                     "$$"
#                                              )
#                                            )
#                           ),
#
#                           conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Estatística t" || input.ferramentes_desvio_padrao_statistic == "Valor de p"',
#
#                                            withMathJax(
#                                              paste0("$$",
#                                                     "\\text{Desvio padrão} = \\dfrac{\\bar X - \\mu_0 }",
#                                                     "{t_{\\alpha/2, n-1}} \\sqrt n",
#                                                     "$$"
#                                              )
#                                            )
#                           ),
#
#
#                         )
#                       )
# )




# aba_cohen d ----


aba_cohen <- tabPanel("d de Cohen",
                      titlePanel("Calcula o d de Cohen"),
                      # wellPanel(paste0("Em muitos cálculos é necessário informar o desvio padrão da variável de interesse, ",
                      #                  "no entanto é encontrado na literatura somente o valor do intervalo de confiança da estimava da média. ",
                      #                  "Acreditando que o cálculo do intervalo de confiança foi realizado supondo que a amostra foi selecionada através de uma amostragem aleatória simples e ",
                      #                  "que as observações são independentes e seguem uma distribuição normal, ",
                      #                  "esta ferramenta pode ser utilizada para fazer a conversão."
                      # )
                      # ),
                      sidebarLayout(
                        sidebarPanel(

                          numericInput( "cohen_mean_dif",
                                        "Diferença entre as médias",
                                        value = 4.06,
                                        min = -Inf,
                                        max = Inf,
                                        step = 1
                          ) %>% .help_buttom(body = "Diferença observada entre os dois grupos."),
                          numericInput( "cohen_sigma1",
                                        "Desvio padrão do grupo A",
                                        value = 1.2,
                                        min = 0,
                                        max = Inf,
                                        step = .01
                          ),
                          numericInput( "cohen_sigma2",
                                        "Desvio padrão do grupo B",
                                        value = 1.4,
                                        min = 0,
                                        max = Inf,
                                        step = .01
                          ),

                          numericInput( "cohen_n1",
                                        "Tamanho amostral do grupo A",
                                        value = 20,
                                        min = 3,
                                        max = Inf,
                                        step = 1
                          ) %>% .help_buttom(body = "Tamanho amostral do grupo A."),
                          numericInput( "cohen_n2",
                                        "Tamanho amostral do grupo B",
                                        value = 20,
                                        min = 3,
                                        max = Inf,
                                        step = 1
                          ) %>% .help_buttom(body = "Tamanho amostral do grupo B."),

                          numericInput( "cohen_decimals",
                                        "Número de casas decimais",
                                        value = 3,
                                        min = 0,
                                        max = Inf,
                                        step = 1
                          ) %>% .help_buttom(body = "Número de casas decimais para exibir o desvio padrão calculado.")
                        ),

                        mainPanel(
                          htmlOutput("ferramentas_cohen"),
                          br(), br(),
                          p("Foi utilizado a fórmula"),
                          withMathJax(
                            "$$\\text{Cohen'd} = \\dfrac{\\text{Diferença das médias}}{s_{pooled}}$$"
                          ),
                          br(),
                          p("onde"),
                          withMathJax(
                            "$$s_{pooled} = \\sqrt{ \\dfrac{(n_A - 1)s_A^2 + (n_B - 1)s_B^2}{n_A+n_B-2} } .$$"
                          )
                        )
                      ),

                      rodape

)




# aba_pooled_var -----

aba_pooled_var <- tabPanel("Desvio padrão combinado",
                           titlePanel("Desvio padrão combinado de dois grupos independentes"),
                           wellPanel(paste0("Usamos o desvio padrão combinado quando temos a informação da variância de dois grupos independentes e ",
                                            "queremos agrega-las para ter um único desvio padrão")
                           ),

                           sidebarLayout(
                             sidebarPanel(


                               checkboxInput(inputId = "pooled_eh_sd",
                                             label   = "Entrar com os valores do desvio padrão.",
                                             value   = TRUE),

                               uiOutput("pooled_var_sdUi"),

                               checkboxInput(inputId = "pooled_equal_size",
                                             label   = "Os grupos possuem o mesmo tamanho.",
                                             value   = FALSE),

                               conditionalPanel('input.pooled_equal_size == false',

                                                # HTML(
                                                #   paste0("<b><font size = '2.99'>", estat_, " do</font></b><br>")
                                                # ),
                                                div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                                                    numericInput( "pooled_n1",
                                                                  "n do grupo A",
                                                                  value = 20,
                                                                  min = 3,
                                                                  max = Inf,
                                                                  step = 1)
                                                ),
                                                div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                    numericInput( "pooled_n2",
                                                                  "n do grupo B",
                                                                  value = 30,
                                                                  min = 3,
                                                                  max = Inf,
                                                                  step = 1)
                                                )
                               ),

                               numericInput( "pooled_decimals",
                                             "Número de casas decimais",
                                             value = 3,
                                             min = 0,
                                             max = Inf,
                                             step = 1
                               )
                             ),

                             mainPanel(
                               htmlOutput("ferramentas_pooled"),
                               br(), br(),
                               p("Foi utilizado a fórmula"),
                               # withMathJax(
                               #   "$$\\text{Cohen'd} = \\dfrac{\\text{Diferença das médias}}{s_{pooled}}$$"
                               # ),
                               # br(),
                               # p("onde"),
                               withMathJax(
                                 "$$s_{pooled} = \\sqrt{ \\dfrac{(n_A - 1)s_A^2 + (n_B - 1)s_B^2}{n_A+n_B-2} } .$$"
                               )
                             )
                           ),

                           rodape

)




# aba_obter_correlacao -----

aba_obter_correlacao <- tabPanel(
  "Obter a correlação",
  titlePanel("Obter a correlação de Pearson entre duas variáveis utilizando os desvios padrões"),
  # wellPanel(paste0("Usamos o desvio padrão combinado quando temos a informação da variância de dois grupos independentes e ",
  #                  "queremos agrega-las para ter um único desvio padrão")
  # ),

  sidebarLayout(
    sidebarPanel(

      numericInput( "outras_ferr_correlacaoA",
                    "Desvio padrão da variável A",
                    value = 6.4,
                    min = 0,
                    max = Inf,
                    step = 1
      ),

      p("Em estudos longitudinais a variável A pode ser entendido como o Momento 1 (basal)."),

      br(),

      numericInput( "outras_ferr_correlacaoB",
                    "Desvio padrão da variável B",
                    value = 7.1,
                    min = 0,
                    max = Inf,
                    step = 1
      ),

      p("Em estudos longitudinais a variável B pode ser entendido como o Momento 2 (follow-up)."),


      br(),
      numericInput( "outras_ferr_correlacaoAeB",
                    "Desvio padrão da diferença entre as variáveis A e B",
                    value = 4.5,
                    min = 0,
                    max = Inf,
                    step = 1
      )



    ),

    mainPanel(
      br(), br(),
      htmlOutput("correlacao_outras_ferramentas"),

      br(), br(),
      p("Foi utilizado a fórmula:"),
      br(),
      withMathJax(
        "$$\\rho_{AeB} = \\dfrac{SD^2_A + SD^2_B - SD^2_{AeB}}{2*SD^2_A*SD^2_B} $$"
      ),
      br(), br(),
      # p("onde:"),
      # br(), br(),


      rodape

    )
  )
)







#____________-----
# Curva ROC  -----

aba_curva_roc <- tabPanel(
  "AUC",
  titlePanel("Área sobre a curva ROC"),
  wellPanel(paste0("A area sobre a curva ROC é uma medida utilizada para avaliar a performance de classificação de um teste. ",
                   "É uma medida que varia de 0 (todas as classificações estão incorretas) a 1 (todas as classificação estão corretas).")),
  tabsetPanel(
    tabPanel("Testar",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   HTML(
                     "<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"
                   ),
                   withMathJax(
                     "$$H_0: AUC =0,5 \\text{  vs  } H_1: AUC \\neq 0,5$$"
                   ),
                 ),
                 numericInput( "auc_auc",
                               "Área sobre a curva esperada",
                               value = 0.7,
                               min = 0.5,
                               max = 1,
                               step = .1
                 ) %>% .help_buttom(body = paste0("Área sobre a curva que se espera encontrar.", .txt_definido_pesquisador_OU_literatura)),
                 # numericInput( "auc_k",
                 #               "Balanço da amostra (Controle:Casos)",
                 #               value = 1,
                 #               min = 0,
                 #               max = Inf,
                 #               step = .5
                 # ) %>% .help_buttom(body = .txt_balanceamento, title = "Balanceamento"),
                 numericInput( "auc_k",
                               "Prevalência de casos (%)",
                               value = 50,
                               min = 0,
                               max = Inf,
                               step = .5
                 ) %>% .help_buttom(paste0("Prevalência de casos (%).", .txt_definido_pesquisador_OU_literatura)),
                 numericInput( "auc_poder",
                               "Poder (%)",
                               value = 80,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
                 numericInput( "auc_significancia",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
                 numericInput( "auc_perdas_recusa",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
               ),

               mainPanel(
                 htmlOutput("auc_output") %>%
                   shinycssloaders::withSpinner(type = 5),

                 ###  CENARIOS  ####.
                 uiOutput("cenarios_roc_th_Ui")

               )
             )
    ),

    tabPanel("Estimar",
             sidebarLayout(
               sidebarPanel(
                 numericInput( "auc_est_auc",
                               "Área sobre a curva esperada",
                               value = 0.8,
                               min = 0.1,
                               max = 1,
                               step = .1
                 ) %>% .help_buttom(body = paste0("Área sobre a curva que se espera encontrar.", .txt_definido_pesquisador_OU_literatura)),
                 numericInput( "auc_est_amplitude",
                               "Amplitude desejada",
                               value = 0.3,
                               min = 0,
                               max = 1,
                               step = .1
                 ) %>% .help_buttom(body = .txt_amplitude, title = "Amplitude do intervalo"),
                 numericInput( "auc_est_k",
                               "Prevalênica de casos (%)",
                               value = 50,
                               min = 0,
                               max = Inf,
                               step = .5
                 ) %>% .help_buttom(paste0("Prevalência de casos (%).", .txt_definido_pesquisador_OU_literatura)),
                 numericInput( "auc_est_confiança",
                               "Nível de confiança (%)",
                               value = 95,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_confianca, title = "Nível de confiança (%)"),
                 numericInput( "auc_est_perdas_recusa",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
               ),

               mainPanel(
                 htmlOutput("auc_est_output") %>%
                   shinycssloaders::withSpinner(type = 5)
               )
             )
    )
  ),

  rodape
)




# aba_sens/ epecificidade ----

aba_sensibilidade <- tabPanel("Sensibilidade/ Especificidade",
                              titlePanel("Sensibilidade e especificidade de um teste"),
                              wellPanel(paste0("A sensibilidade e a especificidade são medidas para avaliar a performance de ",
                                               "classificação de um teste. Nesta aba é possível determinar o tamanho amostral para ",
                                               "estimar a sensibilidade/ especificidade de um teste com a precisão desejada levando em ",
                                               "consideração a prevalência do desfecho.")),
                              tabsetPanel(
                                tabPanel("Estimar",
                                         sidebarLayout(
                                           sidebarPanel(
                                             p("Se desejas estimar somente a sensibilidade, coloque especificidade igual a 0% ou em branco (vice-versa). ",
                                               "Caso os dois valores sejam especificados, o tamanho amostral será com base no maior n."),

                                             HTML(
                                               "<b><font size = '2.9'>Valores esperados (em %) de:</font></b><br>"
                                             ),
                                             div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                                                 numericInput( "sensibil_sensibilidade",
                                                               "sensibilidade",
                                                               value = 75,
                                                               min = 0,
                                                               max = 100,
                                                               step = 1)
                                             ),
                                             div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                 numericInput( "especif_especificidade",
                                                               "e especificidade",
                                                               value = 75,
                                                               min = 0,
                                                               max = 100,
                                                               step = 1)
                                             ),

                                             numericInput( "sensibil_prevalencia",
                                                           "Prevalência esperada do desfecho (%)",
                                                           value = 60,
                                                           min = 0,
                                                           max = 100,
                                                           step = 1
                                             ) %>% .help_buttom(body = "Prevalência do desfecho que se espera encontrar."),
                                             numericInput( "sensibil_amplitude",
                                                           "Amplitude (%)",
                                                           value = 20,
                                                           min = 0,
                                                           max = 100,
                                                           step = 1
                                             ) %>% .help_buttom(body = .txt_amplitude, title = "Amplitude do intervalo"),
                                             numericInput( "sensibil_confianca",
                                                           "Nível de confiança (%)",
                                                           value = 95,
                                                           min = 0,
                                                           max = 100,
                                                           step = 1
                                             ) %>% .help_buttom(body = .txt_confianca, title = "Nível de confiança (%)"),
                                             selectInput("sensibil_metodo",
                                                         "Método utilizado para calcular o intervalo de confiança",
                                                         choices = c("wilson", "agresti-coull", "exact", "wald"),
                                                         selected = "wald"
                                             ) %>% .help_buttom(body = .txt_per_method),
                                             numericInput( "sensibil_perdas_recusa",
                                                           "Perdas/ Recusa (%)",
                                                           value = 10,
                                                           min = 0,
                                                           max = 100,
                                                           step = 1
                                             ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
                                           ),

                                           mainPanel(
                                             htmlOutput("sensibil_output") %>%
                                               shinycssloaders::withSpinner(type = 5),


                                             ###  CENARIOS  ####.
                                             uiOutput("cenarios_sensi_espUi")
                                           )
                                         )
                                )
                              ),

                              rodape

)



# aba_especificidade ----


aba_especificidade <- tabPanel("Especificidade",
                               titlePanel("Especificidade"),
                               # wellPanel(paste0("Especificidade..."))
                               # tabsetPanel(
                               #   tabPanel("Estimar",
                               #            sidebarLayout(
                               #              sidebarPanel(
                               #                numericInput( "especif_especificidade",
                               #                              "Especificidade esperada (%)",
                               #                              value = 75,
                               #                              min = 0,
                               #                              max = 100,
                               #                              step = 1
                               #                ) %>% .help_buttom(body = "Especificidade que se espera encontrar."),
                               #                numericInput( "especif_prevalencia",
                               #                              "Prevalência esperada do desfecho (%)",
                               #                              value = 30,
                               #                              min = 0,
                               #                              max = 100,
                               #                              step = 1
                               #                ) %>% .help_buttom(body = "Prevalência do desfecho que se espera encontrar."),
                               #                numericInput( "especif_amplitude",
                               #                              "Precisão (%)",
                               #                              value = 10,
                               #                              min = 0,
                               #                              max = 100,
                               #                              step = 1
                               #                ) %>% .help_buttom(body = "É a metade da amplitude do intervalo de confiança."),
                               #                numericInput( "especif_confianca",
                               #                              "Nível de confiança (%)",
                               #                              value = 95,
                               #                              min = 0,
                               #                              max = 100,
                               #                              step = 1
                               #                ) %>% .help_buttom(body = .txt_confianca, title = "Nível de confiança (%)"),
                               #                numericInput( "especif_perdas_recusa",
                               #                              "Perdas/ Recusa (%)",
                               #                              value = 10,
                               #                              min = 0,
                               #                              max = 100,
                               #                              step = 1
                               #                ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
                               #              ),
                               #
                               #              mainPanel(
                               #                htmlOutput("especif_output") %>%
                               #                  shinycssloaders::withSpinner(type = 5)
                               #              )
                               #            )
                               #   )
                               # )
)




# aba_kappa ----


aba_kappa <- tabPanel(
  "Kappa de Cohen",
  titlePanel("Kappa de Cohen"),
  wellPanel(paste0("O Kappa de Cohen pode ser utilizado para avaliar o grau de concordância entre avaliadores quanto a uma variável qualitativa. ",
                   "Quanto mais próximo do valor 1 (um) maior o grau de concordância e quanto mais próximo de 0 (zero) maior a discordância entre os avaliadores."
  )),
  tabsetPanel(
    tabPanel("Testar",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   HTML(
                     "<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"
                   ),
                   uiOutput("kappa_th_formula"),
                 ),
                 selectInput(inputId = "kappa_k_categorias",
                             label   = "Número de categorias da variável resposta",
                             choices = c("Duas/ binária" = 2,
                                         "Três" = 3,
                                         "Quatro" = 4,
                                         "Cinco" = 5),
                             selected = 2
                 ),
                 numericInput( "kappa_kappa_esperado",
                               "Kappa esperado",
                               value = 0.85,
                               min = 0,
                               max = 1,
                               step = .1
                 ) %>% .help_buttom(body = paste0("Valor de kappa que se espera encontrar.", .txt_definido_pesquisador_OU_literatura)),
                 numericInput( "kappa_h0",
                               "Kappa sob H0",
                               value = 0.5,
                               min = 0,
                               max = 1,
                               step = .1
                 ) %>% .help_buttom(body = paste0("Kappa para testar em H0", .txt_definido_pesquisador)),
                 conditionalPanel("input.kappa_k_categorias == 2",
                                  numericInput( "kappa_prev2",
                                                "Probabilidade diagnóstico positivo (%)",
                                                value = 60,
                                                min = 0,
                                                max = 100,
                                                step = 5
                                  ) %>% .help_buttom(body = paste0("Probabilidade diagnóstico positivo (%)", .txt_definido_literatura))
                 ),
                 uiOutput("kappa_prevk_Ui"),

                 # numericInput( "kappa_prev2",
                 #               "Probabilidade diagnóstico positivo pelo método B (%)",
                 #               value = 65,
                 #               min = 0,
                 #               max = 100,
                 #               step = 5
                 # ) %>% .help_buttom(body = "Probabilidade diagnóstico positivo pelo método B (%)"),
                 numericInput( "kappa_raters",
                               "Número de métodos/ avaliadores",
                               value = 2,
                               min = 2,
                               max = 6,
                               step = 1
                 ) %>% .help_buttom(body = paste0("The number of raters that are available", .txt_definido_pesquisador)),
                 numericInput( "kappa_power",
                               "Poder (%)",
                               value = 80,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
                 numericInput( "kappa_significancia",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
                 # selectInput(inputId = "kappa_sided",
                 #             label   = "Direção do teste",
                 #             choices = c("Bilateral", "Unilateral"),
                 #             selected = "Bilateral"
                 # ),
                 numericInput( "kappa_perdas_recusa",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
               ),

               mainPanel(
                 htmlOutput("kappa_output") %>%
                   shinycssloaders::withSpinner(type = 5)
               )
             )
    ),

    tabPanel("Estimar",
             sidebarLayout(
               sidebarPanel(
                 # selectInput(inputId = "kappa_est_k_categorias",
                 #             label   = "Número de categorias da variável resposta",
                 #             choices = c("Duas/ binária" = 2,
                 #                         "Três" = 3,
                 #                         "Quatro" = 4,
                 #                         "Cinco" = 5),
                 #             selected = 2
                 # ),
                 sliderInput("kappa_est_k_categorias",
                             "Número de categorias da variável resposta",
                             min = 2,
                             max = 5,
                             value = 2,
                             step = 1,
                             post = " categorias"
                 ),

                 numericInput( "kappa_est_kappa_esperado",
                               "Kappa esperado",
                               value = 0.70,
                               min = 0,
                               max = 1,
                               step = .1
                 ) %>% .help_buttom(body = "Valor de kappa que se espera encontrar."),
                 numericInput( "kappa_est_amplitude",
                               "Amplitude desejada",
                               value = 0.2,
                               min = 0,
                               max = 1,
                               step = .1
                 ) %>% .help_buttom(body = .txt_amplitude, title = "Amplitude do intervalo"),
                 conditionalPanel("input.kappa_est_k_categorias == 2",
                                  numericInput( "kappa_est_prev2",
                                                "Probabilidade diagnóstico positivo (%)",
                                                value = 60,
                                                min = 0,
                                                max = 100,
                                                step = 5
                                  ) %>% .help_buttom(body = paste0("Probabilidade diagnóstico positivo (%)", .txt_definido_literatura))
                 ),
                 uiOutput("kappa_est_prevk_Ui"),

                 # numericInput( "kappa_prev2",
                 #               "Probabilidade diagnóstico positivo pelo método B (%)",
                 #               value = 65,
                 #               min = 0,
                 #               max = 100,
                 #               step = 5
                 # ) %>% .help_buttom(body = "Probabilidade diagnóstico positivo pelo método B (%)"),
                 numericInput( "kappa_est_raters",
                               "Número de métodos/ avaliadores",
                               value = 2,
                               min = 2,
                               max = 6,
                               step = 1
                 ) %>% .help_buttom(body = "The number of raters that are available"),
                 numericInput( "kappa_est_confianca",
                               "Nível de confiança (%)",
                               value = 95,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_confianca, title = "Nível de confiança (%)"),
                 numericInput( "kappa_est_perdas_recusa",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
               ),

               mainPanel(
                 htmlOutput("kappa_est_output") %>%
                   shinycssloaders::withSpinner(type = 5)
               )
             )
    )
  ),

  rodape
)


# aba_icc ------


aba_icc <- tabPanel(
  "ICC",
  titlePanel("Coeficiente de correlação intraclasse"),
  wellPanel(paste0("O coeficiente de correlação intraclasse pode ser utilizado para avaliar o grau de concordância entre avaliadores quanto a uma variável quantitativa. ",
                   "Quanto mais próximo do valor 1 (um) maior o grau de concordância e quanto mais próximo de 0 (zero) maior a discordância entre os avaliadores."
  )),
  tabsetPanel(
    tabPanel("Testar",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   HTML(
                     "<b><font size = '2.8'>Hipóteses a serem testadas</font></b>"
                   ),
                   uiOutput("icc_th_formula"),
                 ),
                 numericInput( "icc_icc_esperado",
                               "ICC esperado",
                               value = 0.6,
                               min = 0,
                               max = 1,
                               step = .1
                 ) %>% .help_buttom(body = paste0("Valor do coeficiente de correlação intraclasse que se espera encontrar.", .txt_definido_pesquisador_OU_literatura)),
                 numericInput( "icc_h0",
                               "ICC sob H0",
                               value = 0.5,
                               min = 0,
                               max = 1,
                               step = .1
                 ) %>% .help_buttom(body = paste0("ICC para testar em H0", .txt_definido_pesquisador)),
                 numericInput( "icc_ratings",
                               "Número de avaliadores",
                               value = 2,
                               min = 2,
                               max = Inf,
                               step = 1
                 ) %>% .help_buttom(body = paste0("Número de avaliadores por unidade amostral.", .txt_definido_pesquisador)),
                 numericInput( "icc_power",
                               "Poder (%)",
                               value = 80,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_power, title = "Poder (%)"),
                 numericInput( "icc_significancia",
                               "Nível de significância (%)",
                               value = 5,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_significancia, title = "Nível de significância (%)"),
                 # selectInput(inputId = "icc_sided",
                 #             label   = "Direção do teste",
                 #             choices = c("Bilateral", "Unilateral"),
                 #             selected = "Bilateral"
                 # ),
                 uiOutput("icc_sidedUi"),
                 numericInput( "icc_perdas_recusa",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
               ),

               mainPanel(
                 htmlOutput("icc_output") %>%
                   shinycssloaders::withSpinner(type = 5),

                 ###  CENARIOS  ####.
                 uiOutput("cenarios_icc_thUi")
               )
             )
    ),

    tabPanel("Estimar",
             sidebarLayout(
               sidebarPanel(
                 numericInput( "icc_est_icc_esperado",
                               "ICC esperado",
                               value = 0.70,
                               min = 0,
                               max = 1,
                               step = .1
                 ) %>% .help_buttom(body = paste0("Valor do coeficiente de correlação intraclasse que se espera encontrar.", .txt_definido_pesquisador_OU_literatura)),
                 numericInput( "icc_est_amplitude",
                               "Amplitude desejada",
                               value = 0.2,
                               min = 0,
                               max = 1,
                               step = .1
                 ) %>% .help_buttom(body = .txt_amplitude, title = "Amplitude do intervalo"),
                 numericInput( "icc_est_ratings",
                               "Número de avaliadores",
                               value = 2,
                               min = 2,
                               max = Inf,
                               step = 1
                 ) %>% .help_buttom(body = paste0("Número de avaliadores por unidade amostral.", .txt_definido_pesquisador)),
                 numericInput( "icc_est_confiança",
                               "Nível de confiança (%)",
                               value = 95,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_confianca, title = "Nível de confiança (%)"),

                 numericInput( "icc_est_perdas_recusa",
                               "Perdas/ Recusa (%)",
                               value = 10,
                               min = 0,
                               max = 100,
                               step = 1
                 ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)")
               ),

               mainPanel(
                 htmlOutput("icc_est_output") %>%
                   shinycssloaders::withSpinner(type = 5)
               )
             )
    )
  ),

  rodape
)


#_____________-------
# Bland Altman  ----
aba_estimacao_bland <- tabPanel(
  "Bland-Altman",
  titlePanel("Precisão para os limites de concordância"),
  wellPanel("Precisão para os limites de concordância do gráfico de Bland-Altman."),

  sidebarLayout(
    sidebarPanel(
      numericInput( "bland_amplitude",
                    "Amplitude do intervalo de confiança",
                    value = 1.2,
                    min = 0,
                    max = Inf,
                    step = 0.5
      ) %>%
        shinyhelper::helper(type = "inline",
                            title = "Amplitude do intervalo de confiança",
                            content = includeMarkdown(file.path("www", "Bland_altman_plot.md")),
                            buttonLabel = "Fechar",
                            fade = TRUE,
                            colour = "#006338",
                            size = "l"),

      # .help_buttom(body = "É a amplitude do intervalo de confiança (limite superior menos limite inferior)."),
      numericInput( "bland_confianca",
                    "Nível de confiança (%)",
                    value = 95,
                    min = 0,
                    max = 100,
                    step = 1
      ) %>% .help_buttom(body = .txt_confianca, title = "Nível de confiança (%)"),
      numericInput( "bland_perdas_recusa",
                    "Perdas/ Recusa (%)",
                    value = 10,
                    min = 0,
                    max = 100,
                    step = 1
      ) %>% .help_buttom(body = .txt_perdas_recusas, title = "Perdas/ Recusas (%)"),
    ),

    mainPanel(
      shinycssloaders::withSpinner(htmlOutput("bland_est"), type = 5),

      ###  CENARIOS  ####.


      br(),
      HTML('<hr style="color: black;">'),
      br(),br(),

      titlePanel("Construção de cenários"),
      br(),

      wellPanel("Utilize os argumentos abaixo para construir diferentes cenários. ",
                "Você pode especificar uma sequência de valores para a amplitude e valores de nível de confiança desejada.
                Demais informações serão recuperadas do painel lateral."),

      fluidRow(
        column(6,
               textInput(inputId = "bland_cenarios_confianca",
                         label   = "Digite valores de nível de confiança (%) para fazer o gráfico",
                         value   = "90, 95, 99",
                         width   = "400px") %>%
                 .help_buttom(body = "Defina os valores de nível de confiança desejado.
                                                      Esses valores serão utilizados para criar diferentes linhas no gráfico.
                                                      Separe os valores por vírgula ',' e utilize ponto '.' como decimal.")
        )
      ),

      HTML("<b>Defina a sequência de valores para a amplitude do intervalo de confiança:</b>"),
      br(),
      div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
          numericInput("bland_from", "Mínimo", value = 0.3, step = 0.5)
      ),
      div(style = "display: inline-block;vertical-align:top; width: 80px;",
          numericInput("bland_to", "Máximo", value = 1.1, step = 0.5)
      ),
      div(style = "display: inline-block;vertical-align:top; width: 80px;",
          numericInput("bland_by", "Intervalo", value = 0.1, min = 0, step = 0.1) %>%
            .help_buttom(body = "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.",
                         title = "Sequência")
      ),
      br(),

      plotly::plotlyOutput("bland_cenarios_plot", width = "80%"),
      br(), br(),
      downloadButton("bland_cenarios_download","Download tabela"),
      DT::dataTableOutput("bland_cenarios_tab", width = "100%")

    )
  ),

  rodape
)







#_____----
# ui ----------------
#_____-----


# require("fresh")
# mytheme <- create_theme(
#   theme = "default",
#   bs_vars_navbar(
#     default_bg = "#006338", # barra superios
#     default_color = "#f0f5f5", # Letras quando passa o mouse por cima
#     default_link_color = "#ffffff", # Letras do menu
#     default_link_active_color = "#ffffff",# Letras do menu quando selecionadas
#     default_link_active_bg = "#008048", # Fundo do menu quando selecionado
#     default_link_hover_color = "f0f5f5",
#     default_link_hover_bg = "#00663a",
#     inverse_link_hover_color   = "#ff0000"
#   ),
#   bs_vars_button(
#     default_color = "#000000", # cor do texto dos botoes (download...)
#     default_bg = "#75a38c", # Background color for default buttons.
#     default_border = "#29473d"
#   ),
#
#   bs_vars_global(
#     link_color = "#006338",
#     text_color = "#000000"
#   ),
#
#   bs_vars_dropdown(
#     link_active_bg  = "#009645"
#   ),
#
#   bs_vars_tabs(
#     active_link_hover_bg = "#f5f5ef",
#     active_link_hover_color = "#000000"
#   ),
#
#   bs_vars_alert(
#     info_bg  = "#009645"
#   ),
#
#   bs_vars_input(
#     border_focus = "#009645"
#   ),
#
#   output_file = NULL
#   # output_file = "inst\\PSS.Health\\www\\theme_pss.css"
# )



ui <- fluidPage(fluidRow(
  navbarPage( windowTitle = "PSS Health",
              theme = "theme_pss.css",

              # header = tagList(
              #   use_theme(mytheme) # <-- use your theme
              # ),





              tabPanel(""),


              # Página de entrada #

              tabPanel("Boas vindas!",
                       tags$head(tags$link(rel = "icon", href = "PSS.png")),
                       # Muda a cor dos sliderInput
                       tags$head(tags$style(HTML(as.character(paste0(
                         ".js-irs-1 .irs-single, .js-irs-1 .irs-from, .js-irs-1 .irs-to, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar{background: #006338 ;  }",
                         ".js-irs-2 .irs-single, .js-irs-2 .irs-from, .js-irs-2 .irs-to, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar{background: #006338 ;  }",
                         ".js-irs-3 .irs-single, .js-irs-3 .irs-from, .js-irs-3 .irs-to, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar{background: #006338 ;  }",
                         ".js-irs-4 .irs-single, .js-irs-4 .irs-from, .js-irs-4 .irs-to, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar{background: #006338 ;  }"
                       ))))),
                       # tags$head(includeHTML(("google-analytics.html"))),

                       # tags$script(HTML("var header = $('.navbar > .container-fluid');
                       # header.append('<div style=\"float:right\"><ahref=\"URL\"><img src=\"Favicon.gif\" alt=\"alt\" style=\"float:right;width:33px;height:41px;padding-top:10px;\"> </a>`</div>');
                       #     console.log(header)")
                       # ),



                       # titlePanel(HTML("PSS Health")),
                       # HTML("<b>P</b>ower and <b>S</b>ample <b>S</b>ize for Health Researchers"),

                       fluidPage(fluidRow(
                         column(11,
                                fluidRow(
                                  h2("PSS Health"),
                                  HTML(paste0(
                                    "<b>P</b>ower and <b>S</b>ample <b>S</b>ize for Health Researchers (versão ",
                                    if (!.versao_online){
                                      packageVersion("PSS.Health")
                                    } else{
                                      "on-line"
                                    }, ")")
                                  )
                                )
                         ),
                         column(1, img(src = 'PSS.png', align = "right", width = "150%"))
                       )),

                       # h2("PSS Health"),
                       # HTML(paste0(
                       #   "<b>P</b>ower and <b>S</b>ample <b>S</b>ize for Health Researchers (versão ", if(!.versao_online){
                       #     packageVersion("PSS.Health")
                       #   } else{
                       #     "on-line"
                       #   }, ")")
                       # ),

                       # fluidPage(
                       #   column(1, img(src = 'PSS.png', align = "right", width = "150%"))
                       # ),


                       # uiOutput("PSS_CRAN"),

                       br(),br(),
                       HTML("Navegue entre as abas para encontrar o cenário correspondente ao objetivo do estudo. Altere os parâmetros conforme necessidade e utilize o texto como auxílio para entender o cálculo.",
                            "<br>",
                            "<b>Sempre</b> procure um profissional de estatística para orientações no planejamento do estudo.",
                            ""),

                       br(),br(),br(),
                       # h3("PSS Health no CRAN"),
                       HTML(paste0("O PSS Health está disponível no ",
                                   "<a href='https://cran.r-project.org/web/packages/PSS.Health/index.html' target='_blank'>CRAN</a>",
                                   " e pode ser utilizado pelo computador por meio do pacote ", code("PSS.Health"), ".")),
                       br(), br(),
                       code("install.packages('PSS.Health')"),
                       br(),
                       code("PSS.Health::PSS_Health()"),

                       br(),br(),br(),
                       h3("Leituras recomendadas"),
                       HTML("Frequentemente a Unidade de Bioestatística do Grupo de Pesquisa e Pós-Graduação do Hospital de Clínicas de Porto Alegre publica artigos na seção de bioestatística da revista ",
                            "<a href='https://seer.ufrgs.br/hcpa' target='_blank'>Clinical & Biomedical Research</a>",
                            ". Nessas publicações são abordadas temas que podem te auxiliar na definição do tamanho amostral e do poder do teste.",

                            "<br><br>",
                            "<ul>", # inicio da lista

                            "<li><b>Artigo sobre o PSS Health:</b></li>",
                            "<ul>",
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/109542/pdf" target="_blank">Power and Sample Size for Health Researchers: uma ferramenta para cálculo de tamanho amostral e poder do teste voltado a pesquisadores da área da saúde</a></b></li>',
                            '<li><b><a href="https://doi.org/10.22491/2357-9730.112466" target="_blank">PSS Health: como calcular tamanho de amostra para estimar média, proporção e correlação</a></b></li>',
                            "</ul>",
                            "<br><br>",
                            "<li><b>Principais conceitos em Epidemiologia:</b> Têm dúvidas sobre os tipos de delineamento e métodos de amostragem?</li>",
                            "<ul>",
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/42338/27237" target="_blank">Os principais delineamentos na Epidemiologia</a></b></li>',
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/44253/28281" target="_blank">Os principais delineamentos na Epidemiologia: Ensaios Clínicos I</a></b></li>',
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/44657/28397" target="_blank">Os principais delineamentos na Epidemiologia: Ensaios Clínicos II </a></b></li>',
                            "</ul>",

                            "<br><br>",
                            "<li><b>Série \"Perguntas que você sempre quis fazer, mas nunca teve coragem\":</b> Têm dúvidas sobre conceitos comumente utilizados em estatística e epidemiologia?</li>",
                            "<ul>",
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/89242/pdf" target="_blank">Estatística Descritiva: Perguntas que você sempre quis fazer, mas nunca teve coragem</a></b></li>',
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/96394/pdf" target="_blank">Bioestatística e epidemiologia: perguntas que você sempre quis fazer, mas nunca teve coragem</a></b></li>',
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/93649/pdf" target="_blank">Teste de hipóteses: perguntas que você sempre quis fazer, mas nunca teve coragem</a></b></li>',
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/98944/pdf" target="_blank">Modelagem estatística: Perguntas que você sempre quis fazer, mas nunca teve coragem</a></b></li>',
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/101299/pdf" target="_blank">Equívocos Estatísticos: Perguntas que você sempre quis fazer, mas nunca teve coragem</a></b></li>',
                            "</ul>",

                            "<br><br>",
                            "<li><b>Outras publicações:</b> </li>",
                            "<ul>",
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/27267/16646" target="_blank">Um alerta sobre o uso de amostras pequenas na regressão logística</a></b></li>',
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/23574/15837" target="_blank">Cálculo de tamanho de amostra: proporções</a></b></li>',
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/9737/5819" target="_blank">Estudos transversais e longitudinais com desfechos binários: qual a melhor medida de efeito a ser utilizada? </a></b></li>',
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/33160/22836" target="_blank">Calculando o tamanho de efeito no SPSS</a></b></li>',
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/14766/8828" target="_blank">Beanplot uma nova ferramenta gráfica</a></b></li>',
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/11727/7021" target="_blank">Análise de concordância entre métodos de Bland-Altman</a></b></li>',
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/36971/23993" target="_blank">Uso do Modelo de Equações de Estimações Generalizadas na análise de dados longitudinais</a></b></li>',
                            '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/29874/19186" target="_blank">Normalidade de variáveis: métodos de verificação e comparação de alguns testes não-paramétricos por simulação</a></b></li>',
                            "</ul>",

                            "</ul>" # fim da lista de artigos
                       ),

                       br(), br(),
                       h3("Sobre"),
                       HTML("Esse aplicativo foi concebido no trabalho de conclusão do curso de Bacharelado em estatística do aluno ",
                            '<a href="https://lume.ufrgs.br/handle/10183/212679" target="_blank">Guilherme Azambuja</a>',
                            ", sob a orientação da professora Stela Castro. Recebeu incrementos, novas funcionalidades e é mantido pela equipe da Unidade de Bioestatística Grupo de Pesquisa e Pós-Graduação do Hospital de Clínicas de Porto Alegre:<br><br>",

                            "<ul>", # inicio da lista

                            "<li> Aline Castello Branco Mancuso ",
                            "<a href='https://orcid.org/0000-0001-6033-8335' target='_blank'>(Orcid iD)</a> ",
                            "<a href='http://lattes.cnpq.br/3041495053719418' target='_blank'>(Lattes iD)</a></li>",


                            "<li> Rogério Boff Borges ",
                            "<a href='https://orcid.org/0000-0002-2548-1889' target='_blank'>(Orcid iD)</a>",
                            "<a href='http://lattes.cnpq.br/4664814523190366' target='_blank'>(Lattes iD)</a></li>",

                            "<li> Stela Maris de Jezus Castro ",
                            "<a href='https://orcid.org/0000-0001-5862-6709' target='_blank'>(Orcid iD)</a>",
                            "<a href='http://lattes.cnpq.br/3433964793739774' target='_blank'>(Lattes iD)</a></li>",


                            "<li>Suzi Alves Camey ",
                            "<a href='https://orcid.org/0000-0002-5564-081X' target='_blank'>(Orcid iD)</a>",
                            "<a href='http://lattes.cnpq.br/8280035478871760' target='_blank'>(Lattes iD)</a></li>",


                            "<li>Vanessa Bielefeldt Leotti ",
                            "<a href='https://orcid.org/0000-0003-3860-9367' target='_blank'>(Orcid iD)</a>",
                            "<a href='http://lattes.cnpq.br/5223855158009832' target='_blank'>(Lattes iD)</a></li>",


                            "<li>Vânia Naomi Hirakata ",
                            "<a href='https://orcid.org/0000-0003-4645-2080' target='_blank'>(Orcid iD)</a>",
                            "<a href='http://lattes.cnpq.br/4647357908962910' target='_blank'>(Lattes iD)</a></li>",


                            "</ul>" # fim da lista de autores
                       ),
                       hr(),
                       HTML("<br>", '<b>Sugestão, críticas ou bugs?</b> Mande um e-mail para <a href="mailto:l-bioestatistica@hcpa.edu.br">l-bioestatistica@hcpa.edu.br</a>.<br><br><br><br>' )


              ),


              #### Estimacao de parametros ####.
              # navbarMenu( "Estimação de Parâmetros",
              #             aba_estimacao_uma_media,
              #             aba_estimacao_uma_prop,
              #             aba_estimacao_Cronbach
              # ),

              #### Teste de hipoteses para uma amostra ####.
              # navbarMenu("Teste de Hipótese para uma amostra",
              #            aba_TH_uma_media,
              #            aba_TH_uma_prop,
              #            aba_TH_uma_media_equivalencia,
              #            aba_TH_uma_prop_equivalencia
              #            ),

              #### Teste de hipotese para duas amostras ####.
              navbarMenu("Médias",
                         aba_estimacao_uma_media,
                         aba_TH_duas_amostra_media,
                         aba_TH_duas_amostra_media_equivalencia,
                         aba_TH_duas_amostra_mean_pareado,
                         aba_TH_duas_amostra_media_2tempos,
                         aba_TH_medidas_repetidas,
                         aba_anova_one_way,
                         aba_anova_two_way
              ),
              navbarMenu("Proporções",
                         aba_estimacao_uma_prop,
                         aba_TH_duas_amostra_prop,
                         aba_TH_duas_amostra_prop_equivalencia,
                         aba_TH_duas_amostra_prop_dep
              ),


              aba_associacao,
              aba_correlacao,

              navbarMenu("Regressão",
                         aba_inclinacao_linear,
                         aba_logistica,
                         aba_surv_cox
              ),

              # Classificacao ----.
              navbarMenu("Classificação",
                         aba_curva_roc,
                         aba_sensibilidade
              ),

              # Concordancia ----.
              navbarMenu("Concordância",
                         aba_kappa,
                         aba_icc,
                         aba_estimacao_bland
              ),


              #### Cronbach  ####.
              aba_estimacao_Cronbach,


              navbarMenu("Outras ferramentas",
                         aba_obter_dp,
                         aba_obter_correlacao,
                         aba_pooled_var,
                         aba_cohen
              )





  )
))
