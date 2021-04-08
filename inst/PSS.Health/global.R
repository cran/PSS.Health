
require("shiny")
require("tidyverse")
require("magrittr")



.txt_definido_pesquisador_OU_literatura <- "<br><br><b>Pode ser um valor da literatura ou um valor que o pesquisador deseja encontrar e que tenha relevância clínica.</b>"
.txt_definido_pesquisador <- "<br><br><b>Deve ser definido pelo pesquisador.</b>"
.txt_definido_literatura <- "<br><br><b>Deve ser obtido de estudos anteriores (de preferência com a mesma população alvo) onde esta variável foi mensurada ou de amostras piloto.</b>"



.txt_um <- paste0(
  "Descreva a unidade de medida em que seu desfecho será mensurado para que sirva de guia no preenchimento dos demais valores. ",
  "<br><br>Por exemplo, se seu desfecho de interesse é o colesterol, a unidade de medida pode ser <b>mg/dl</b> ou <b>mmol/l</b>; ",
  "se o interesse é a altura, a unidade de medida pode ser <b>metros</b> ou <b>centímetros</b>."
)

.txt_desfecho <- paste0(
  "Descreva o nome do desfecho para que sirva de guia no preenchimento dos demais valores. ",
  "Essa informação completará o texto do sugerido do tamanho amostral calculado para relatar nos projetos de pesquisa ou nos trabalhos científicos.",
  "<br><br>O desfecho é a variável mais relevante do estudo, que servirá para testar a hipótese em questão ",
  '<a href="https://seer.ufrgs.br/hcpa/article/view/96394/pdf" target="_blank">(Castro et al. 2019)</a>.'
)


.txt_outros_desfechos <- function(frase1){
  paste0(
    frase1,
    "Essa informação completará o texto sugerido do tamanho amostral calculado para relatar nos projetos de pesquisa ou nos trabalhos científicos."
  )
}

.txt_diferenca_clinica <- paste0(
  "É a menor diferença considerada clinicamente relevante (que tenha algum valor clínico). ",
  "<br><br>Maiores detalhes na seção \"<i>QUAL A DIFERENÇA ENTRE SIGNIFICÂNCIA ESTATÍSTICA E RELEVÂNCIA CLÍNICA?</i>\" de ",
  '<a href="https://seer.ufrgs.br/hcpa/article/view/96394/pdf" target="_blank">Castro et al. 2019</a>.',
  .txt_definido_pesquisador
)

.txt_amplitude <- paste0(
  "É a largura total do intervalo de confiança (limite superior menos limite inferior). ",
  "<br><br>Quanto menor seu valor maior será a precisão da estimativa, porém o tamanho de amostra necessário também será maior.",
  .txt_definido_pesquisador
)

.txt_precisao <- paste0(
  "A margem de erro ou semi-amplitude representa a metade da largura total do intervalo de confiança. ",
  "<br><br>Quanto menor seu valor maior será a precisão da estimativa, porém o tamanho de amostra necessário também será maior.",
  .txt_definido_pesquisador
)

.txt_perc_esperado <- paste0(
  "O percentual (%) esperado é uma medida de frequência de ocorrência de um determinado evento (por exemplo, ocorrência de óbito, ocorrência de uma doença, etc.). ",
  "<br><br>Dependendo do delineamento do estudo, esse percentual é conhecido como prevalência ou incidência. ",
  "Maiores detalhes na seção \"<i>QUAL A DIFERENÇA ENTRE PREVALÊNCIA E INCIDÊNCIA?</i>\" de ",
  '<a href="https://seer.ufrgs.br/hcpa/article/view/96394/pdf" target="_blank">Castro et al. 2019</a>.'
)

.txt_per_method <- paste0(
  "Segundo ",
  '<a href="https://cran.r-project.org/web/packages/presize/presize.pdf" target="_blank">documentação da função ', code("prec_prop"), ' do pacote ', code("presize"), '</a>',
  ", o método Wilson é sugerido para n pequeno (<40), o método Agresti-Coull é sugerido para n maior e o método Wald não é sugerido. ",
  "Maiores detalhes na ",
  '<a href="https://cran.r-project.org/web/packages/presize/presize.pdf" target="_blank">documentação original do pacote.</a>',

  .txt_definido_pesquisador
)


.txt_perdas_recusas <- paste0(
  "Percentual de perdas previstas ao longo da pesquisa. O cálculo do tamanho da amostra é ajustado para compensá-las.",
  "<br><br>Considerando <b><i>R</i></b> como o percentual de perdas/ recusas previstas e <b><i>n</b></i> como o tamanho de amostra necessário sem considerar as perdas/ recusas, ",
  "esse <b><i>n</b></i> é multiplicado por <b>1 / [1 - (<i>R</i> / 100)]</b>.",
  .txt_definido_pesquisador_OU_literatura
)


.txt_dp <- paste0(
  "O desvio padrão é uma medida de variabilidade.",
  "<br><br>Você também pode obter o desvio padrão de outras estatísticas, veja a aba ",
  "<i>Outras ferramentas ---> Obter o desvio padrão de outras estatísticas</i>",
  .txt_definido_literatura
)


.txt_confianca <- paste0(
  "O nível de confiança representa a porcentagem de intervalos que iriam incluir o parâmetro populacional se você reunisse amostras do mesmo tamanho, da mesma população, repetidas vezes. ",
  "<br><br>Por exemplo, quando se tem 95% de confiança significa que dos inúmeros intervalos de confiança construídos a partir das amostras de mesmo tamanho, 95% deles conterão o valor do parâmetro populacional. ",
  "<br><br>Quanto maior o nível de confiança, maior o tamanho de amostra necessário.",
  .txt_definido_pesquisador
)


.txt_significancia <- paste0(
  "O nível de significância é utilizado como um ponto de corte na probabilidade de se cometer um erro ao tomarmos a decisão estatística de rejeitar a hipótese nula (erro tipo I). ",
  "Maiores informações em ",
  '<a href="https://seer.ufrgs.br/hcpa/article/view/93649/pdf" target="_blank">Hirakata et al. 2019</a>.',
  .txt_definido_pesquisador
)


.txt_power <- paste0(
  "O poder de um teste estatístico é a probabilidade de se tomar uma decisão correta, rejeitar a hipótese nula se ela realmente for falsa. ",
  "Maiores informações em ",
  '<a href="https://seer.ufrgs.br/hcpa/article/view/93649/pdf" target="_blank">Hirakata et al. 2019</a>.',
  .txt_definido_pesquisador
)


.txt_h1 <- paste0(
  "Tipo de teste de acordo com a hipótese alternativa: O teste pode ser bilateral, superior ou inferior. Nos dois útilmos casos, a hipótese alternativa é de que o parâmetro é maior ou menor do que o valor de referência, respectivamente. ",
  "Maiores informações em ",
  '<a href="https://seer.ufrgs.br/hcpa/article/view/93649/pdf" target="_blank">Hirakata et al. 2019</a>.',
  .txt_definido_pesquisador
)


.txt_razao_chance <- paste0(
  "A razão de chances ou <i>odds ratio</i>, é calculada, principalmente, em estudos do tipo caso-controle, embora também possa ser calculado em estudos transversais ou longitudinais, quando o desfecho for raro. ",
  "É obtida dividindo a chance de desenvolver o evento em um grupo pela chance de outro grupo. ",
  "Maiores informações em ",
  '<a href="https://seer.ufrgs.br/hcpa/article/view/96394/pdf" target="_blank">Castro et al. 2019</a>.',
  .txt_definido_pesquisador_OU_literatura
)



.txt_risco_relativo <- paste0(
  "O risco relativo é um termo amplo para designar razões de taxas (<i>rate ratio</i>) ou razões de risco (<i>risk ratio</i>), utilizado em estudos ",
  "longitudinais. É definido como a razão entre o risco de desenvolver o desfecho nos expostos e o risco de desenvolver o desfecho nos não expostos a partir das taxas de incidência. ",
  "<br><br>",
  "A razão de prevalências, como o próprio nome diz, é obtida pela razão entre a prevalência da doença nos expostos e a prevalência da doença nos não expostos. ",
  "<br><br>Maiores informações em ",
  '<a href="https://seer.ufrgs.br/hcpa/article/view/96394/pdf" target="_blank">Castro et al. 2019</a>.',
  .txt_definido_pesquisador_OU_literatura
)


.txt_balanceamento <- paste0(
  "O balanceamento é uma razão entre tamanhos de dois grupos. Para exemplificar, vamos considerar que faremos o balanceamento para os grupos A e B, isto é <b>A:B</b>:<br><br>",
  "<ul>", # inicio da lista
  "<li><b>Balanceamento igual a 1:</b> será calculando o tamanho de amostra tal que para cada indivíduo do Grupo A teremos outro indivíduo do grupo B (<b>1:1</b>);</li><br>",
  "<li><b>Balanceamento igual a 2:</b> será calculando o tamanho de amostra tal que teremos dois indivíduos do Grupo A para cada indivíduo do grupo B (<b>2:1</b>);</li><br>",
  "<li><b>Balanceamento igual a 0.5:</b> será calculando o tamanho de amostra tal que para cada indivíduo do Grupo A teremos dois indivíduos do grupo B (<b>1:2</b>).</li>",
  "</ul>", # fim da
  .txt_definido_pesquisador
)



.txt_margem_nao_inferior <- paste0(
  "A margem de não inferioridade quantifica a diferença máxima clinicamente aceitável para que o grupo Tratamento possa ser considerado não inferior ao Controle. ",
  "Esta margem se aplica quando o novo Tratamento traz uma vantagem prática que vale a pena abdicar de uma pequena parte do benefício obtido com o Controle. ",
  "<br><br>Quanto mais próximo de zero esta margem, maior o tamanho de amostra necessário. ",
  "<br><br>Maiores detalhes na seção \"<i>QUAL A DIFERENÇA ENTRE ENSAIO CLÍNICO DE COMPARAÇÃO, ENSAIO CLÍNICO DE SUPERIORIDADE, DE EQUIVALÊNCIA E DE NÃO-INFERIORIDADE?</i>\" de ",
  '<a href="https://seer.ufrgs.br/hcpa/article/view/96394/pdf" target="_blank">Castro et al. 2019</a>.',
  .txt_definido_pesquisador
)


.txt_margem_superior <- paste0(
  "A margem superioridade quantifica a diferença mínica clinicamente aceitável para que o grupo Tratamento possa ser considerado superior ao Controle. ",
  "<br><br>Quanto mais próximo de zero esta margem, maior o tamanho de amostra necessário. ",
  "<br><br>Maiores detalhes na seção \"<i>QUAL A DIFERENÇA ENTRE ENSAIO CLÍNICO DE COMPARAÇÃO, ENSAIO CLÍNICO DE SUPERIORIDADE, DE EQUIVALÊNCIA E DE NÃO-INFERIORIDADE?</i>\" de ",
  '<a href="https://seer.ufrgs.br/hcpa/article/view/96394/pdf" target="_blank">Castro et al. 2019</a>.',
  .txt_definido_pesquisador
)

.txt_margem_equivalencia <- paste0(
  "A margem equivalência quantifica a diferença clinicamente aceitável para que o grupo Tratamento possa ser considerado equivalente ao Controle. ",
  "<br><br>Quanto mais próximo de zero esta margem, maior o tamanho de amostra necessário. ",
  "<br><br>Maiores detalhes na seção \"<i>QUAL A DIFERENÇA ENTRE ENSAIO CLÍNICO DE COMPARAÇÃO, ENSAIO CLÍNICO DE SUPERIORIDADE, DE EQUIVALÊNCIA E DE NÃO-INFERIORIDADE?</i>\" de ",
  '<a href="https://seer.ufrgs.br/hcpa/article/view/96394/pdf" target="_blank">Castro et al. 2019</a>.',
  .txt_definido_pesquisador
)





.help_buttom <- function(local, body, title = "Ajuda"){

  shinyhelper::helper(shiny_tag = local,
                      type = "inline",
                      title = title,
                      content = body,
                      buttonLabel = "Fechar",
                      fade = TRUE,
                      colour = "#006338",
                      size = "m")

}




#-----------------.
# Referencias ----
#-----------------.



.txt_citacao_tap <- paste("ferramenta PSS Health versão",
                          packageVersion("PSS.Health"),
                          "(citação abaixo)")

.txt_referencia_tap <-  paste0(
  "<br><br><br><i><b>Sugestões de citação:</b></i><br>",
  "<p style=\"font-size:75% \">",
  paste0("Borges RB, Azambuja GS, Mancuso ACB, Leotti VB, Hirakata VN, Camey SA, Castro SMJ (2021). PSS.Health: Power and Sample Size for Health Researchers via Shiny. R package version 0.1.6. ",
         "Available from: https://CRAN.R-project.org/package=PSS.Health"),
  "</p>")
