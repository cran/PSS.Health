
mod_desvio_padrao_agrupado_Ui <- function(id){

  ns <- NS(id)

  tagList(

    uiOutput(ns("abaa")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_desvio_padrao_agrupado_server <- function(id,
                                              txt_ajuda, translation_pss, linguagem,
                                              grupo_A, grupo_B,
                                              warning_numero_positivo, warning_inteiro){
  shiny::moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns


      eval(parse(text = warning_numero_positivo("sigma_A")))
      eval(parse(text = warning_numero_positivo("sigma_B")))
      eval(parse(text = warning_inteiro("n_A")))
      eval(parse(text = warning_inteiro("n_B")))


      # Aba ----

      output$abaa <- renderUI({

        fluidPage(

          HTML("<b><font size = '3'>", translation_pss("Desvio padrão", linguagem()), " do</font></b><br>"),

          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
              numericInput( ns("sigma_A"),
                            grupo_A,
                            value = 2.0,
                            min = 0,
                            max = Inf,
                            step = .5)),
          div(style="display: inline-block;vertical-align:top; width: 49%;",
              numericInput( ns("sigma_B"),
                            grupo_B,
                            value = 1.5,
                            min = 0,
                            max = Inf,
                            step = .5)),


          HTML(paste0("<b><font size = '3'>", translation_pss("Tamanho amostral", linguagem()), " do</font></b><br>")),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
              numericInput( ns("n_A"),
                            grupo_A,
                            value = 20,
                            min = 3,
                            max = Inf,
                            step = 1)),
          div(style="display: inline-block;vertical-align:top; width: 49%;",
              numericInput( ns("n_B"),
                            grupo_B,
                            value = 30,
                            min = 3,
                            max = Inf,
                            step = 1)),

          htmlOutput(ns("resultado")),

          p(translation_pss("Foi utilizado a fórmula", linguagem())),
          withMathJax(
            paste0(
              "$$s_{", translation_pss("combinado", linguagem()), "} = \\sqrt{ \\dfrac{(n_A - 1)s_A^2 + (n_B - 1)s_B^2}{n_A+n_B-2} }$$"
              )
          )
        )
      })



      output$resultado <- renderText({

        s2a <- input$sigma_A^2
        s2b <- input$sigma_B^2

        n1 <- input$n_A
        n2 <- input$n_A

        numerador   <- (n1 - 1)*s2a + (n2 - 1)*s2b
        denominador <- n1 + n2 - 2

        s_pooled <- sqrt(numerador/denominador)

        paste0("<br><br><b><font size = '5'>",
               "<i>", translation_pss("Desvio padrão", linguagem()), "<sub>", translation_pss("combinado", linguagem()), "</sub></i> = ", round(s_pooled, 4),
               "<br><br><br>")
      })




    } # Nao mexer!!!
  )

}
