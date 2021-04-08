#'
#' @title This function will start the Shiny application
#' @description Run locally an interactive Shiny application for Power and Sample Size determination.
#' @importFrom DT dataTableOutput  renderDataTable datatable JS
#' @importFrom easypower n.multiway
#' @import epiR
#' @importFrom EnvStats aovN aovPower ciNormN ciBinomN propTestPower propTestN
#' @importFrom ICC.Sample.Size calculateIccSampleSize
#' @importFrom kappaSize PowerBinary Power3Cats Power4Cats Power5Cats
#' @importFrom longpower power.mmrm
#' @import plotly
#' @importFrom powerMediation ssLongFull ss.SLR ss.SLR.rho SSizeLogisticCon SSizeLogisticBin
#' @importFrom powerSurvEpi ssizeCT.default ssizeEpiCont.default
#' @import presize
#' @importFrom pROC power.roc.test
#' @importFrom pwr pwr.t.test pwr.p.test pwr.2p.test ES.h pwr.chisq.test pwr.r.test pwr.anova.test
#' @importFrom pwr2 pwr.1way ss.2way
#' @importFrom shinycssloaders withSpinner
#' @importFrom shiny actionButton	br	checkboxInput	code	column	conditionalPanel	div	downloadButton	downloadHandler	fluidPage	fluidRow	h3	hr	HTML	includeMarkdown	mainPanel	navbarMenu	navbarPage	numericInput	observeEvent	p	plotOutput	radioButtons	reactive	renderPlot	renderText	renderUI	req	selectInput	sidebarLayout	sidebarPanel	sliderInput	tabPanel	tabsetPanel	textAreaInput	textInput	titlePanel	uiOutput	validate	wellPanel	withMathJax
#' @import shinyFeedback
#' @importFrom  shinyhelper helper observe_helpers
#' @import tidyverse
#' @importFrom  writexl write_xlsx
#' @export
#' @author Unidade de Bioestatística, Grupo de Pesquisa em Pós Graduação, Hospital de Clínicas de Porto Alegre.
#' @note You can also use the online version at \url{https://hcpa-unidade-bioestatistica.shinyapps.io/PSS_Health/}
#' @encoding UTF-8
#' @examples
#' if(interactive()){
#' PSS_Health()
#' }
#'
#' @seealso {
#'
#' Agranonik, M., & Hirakata, V. N. (2011). Cálculo de tamanho de amostra: Proporções. Clinical & Biomedical Research, 31(3), Article 3. \url{https://seer.ufrgs.br/hcpa/article/view/23574}
#'
#' Borges, R. B., Leotti, V. B., Mancuso, A. C. B., Castro, S. M. de J., Hirakata, V. N., & Camey, S. A. (2020). Equívocos Estatísticos: Perguntas que você sempre quis fazer, mas nunca teve coragem. Clinical & Biomedical Research, 40(1), Article 1. \url{https://seer.ufrgs.br/hcpa/article/view/101299}
#'
#' Coster, R., Torman, V. B. L., & Camey, S. A. (2012). Um alerta sobre o uso de amostras pequenas na regressão logística. Clinical & Biomedical Research, 32(1), Article 1. \url{https://seer.ufrgs.br/hcpa/article/view/27267}
#'
#' de Jezus Castro, S. M., Mancuso, A. C. B., Leotti, V. B., Hirakata, V. N., & Camey, S. A. (2020). Bioestatística e Epidemiologia: Perguntas que você sempre quis fazer, mas nunca teve coragem. Clinical & Biomedical Research, 39(3). \url{https://seer.ufrgs.br/hcpa/article/view/96394}
#'
#' Douglas G. Bonett. (2020). Sample Size Planning for Behavioral Science Research. Sample Size Planning for Behavioral Science Research.  \url{https://people.ucsc.edu/~dgbonett/sample.html}
#'
#' Hirakata, V. N., Mancuso, A. C. B., & Castro, S. M. de J. (2019). Teste de Hipóteses: Perguntas que você sempre quis fazer, mas nunca teve coragem. Clinical & Biomedical Research, 39(2), Article 2. \url{https://seer.ufrgs.br/hcpa/article/view/93649}
#'
#' Leotti, V. B., Mancuso, A. C. B., Borges, R. B., Castro, S. M. de J., Hirakata, V. N., & Camey, S. A. (2019). Modelagem estatística: Perguntas que você sempre quis fazer, mas nunca teve coragem. Clinical & Biomedical Research, 39(4), Article 4. \url{https://seer.ufrgs.br/hcpa/article/view/98944}
#' }



PSS_Health <- function(){

  appDir <- system.file("PSS.Health", package = "PSS.Health")
  if (appDir == "") {
    stop("Could not find PSS.Health package. Try re-installing `PSS.Health`.", call. = FALSE)
  }

  shiny::runApp(appDir = appDir, launch.browser = TRUE)
}

