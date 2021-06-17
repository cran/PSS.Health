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
#' Borges, R., Mancuso, A., Camey, S., Leotti, V., Hirakata, V., Azambuja, G., & Castro, S. (2021). Power and Sample Size for Health Researchers: uma ferramenta para cálculo de tamanho amostral e poder do teste voltado a pesquisadores da área da saúde. Clinical & Biomedical Research, 40(4). Retrieved from \url{https://seer.ufrgs.br/hcpa/article/view/109542}
#'
#' }



PSS_Health <- function(){

  appDir <- system.file("PSS.Health", package = "PSS.Health")
  if (appDir == "") {
    stop("Could not find PSS.Health package. Try re-installing `PSS.Health`.", call. = FALSE)
  }

  shiny::runApp(appDir = appDir, launch.browser = TRUE)
}

