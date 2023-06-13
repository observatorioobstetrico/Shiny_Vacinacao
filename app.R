library(shiny)
library(dplyr)
library(magrittr)
library(readxl)
library(shinydashboard)
library(questionr)
library(kableExtra)
library(ggplot2)
library(highcharter)
library(summarytools)
library(modelsummary)
library(abjData)
library(leaflet)
library(leaflet.extras)
library(stringr)
library(reactable)
library(htmltools)
library(zoo)
library(plotly)
library(lubridate)
library(googlesheets4)
library(shinyjs)
library(viridis)

# Carregando a base de dados ----
dados_vac <- readRDS(file = "dados_vac_select.rds")
dados_vac_bruto <- readRDS("dados_vac_select_bruto.rds")

# Alterações iniciais na base ----

## Relevel dados de vacinação ----
dados_vac$raca1 <- dados_vac$raca
dados_vac$raca1 <-
  ifelse(is.na(dados_vac$raca), "não informado", dados_vac$raca)

dados_vac$qual_vacina1 <- dados_vac$qual_vacina
dados_vac$qual_vacina1 <-
  ifelse(is.na(dados_vac$qual_vacina),
         "não informado",
         dados_vac$qual_vacina)

## Criação de vetor de Estados ----
estadosChoices <- c(
  "AC",
  "AL",
  "AM",
  "AP",
  "BA",
  "CE",
  "DF",
  "ES",
  "GO",
  "MA",
  "MG",
  "MS",
  "MT",
  "PA",
  "PB",
  "PE",
  "PI",
  "PR",
  "RJ",
  "RN",
  "RO",
  "RR",
  "RS",
  "SC",
  "SE",
  "SP",
  "TO"
)

estadosChoices1 <- c(
  "AC",
  "AL",
  "AM",
  "AP",
  "BA",
  "CE",
  "DF",
  "ES",
  "GO",
  "MA",
  "MG",
  "MS",
  "MT",
  "PA",
  "PB",
  "PE",
  "PI",
  "PR",
  "RJ",
  "RN",
  "RO",
  "RR",
  "RS",
  "SC",
  "SE",
  "SP",
  "TO"
)

## Base com os nomes dos municípios ----
aux_muni <- abjData::muni %>%
  dplyr::select(muni_id,
                muni_nm_clean,
                uf_nm,
                uf_sigla,
                regiao_nm,
                lon,
                lat) %>%
  mutate_at("muni_id", as.character)  %>%
  mutate(cod_mun = stringr::str_sub(muni_id, 1, 6))

aux_extra_muni <- aux_muni[,c("cod_mun","muni_nm_clean", "muni_id", "uf_sigla", "regiao_nm")]
colnames(aux_extra_muni) <- c("resid_muni","muni_names", "muni_id", "uf_sigla", "regiao_nm")
aux_extra_muni$resid_muni <- as.numeric(aux_extra_muni$resid_muni)
#janitor::get_dupes(aux_extra_muni, resid_muni)
dados_vac$resid_muni <- as.numeric(dados_vac$resid_muni)

dados_vac <- left_join(dados_vac, aux_extra_muni, by = "resid_muni")

oi <- dados_vac %>%
  filter(is.na(muni_names))

dados_vac <- dados_vac %>%
  mutate(muni_names = ifelse(is.na(muni_names), paciente_endereco_nmMunicipio, muni_names))


#tem municipios e os estados
# dados_sem_muni_nome <- dados_vac %>%
#   filter(is.na(resid_muni)) %>%
#   select(resid_muni, muni_id, resid_uf, uf_sigla, paciente_endereco_nmMunicipio, muni_names)
#
# dados_sem_uf_nome <- dados_vac %>%
#   filter(is.na(resid_uf)) %>%
#   select(resid_muni, muni_id, resid_uf, uf_sigla, paciente_endereco_nmMunicipio, muni_names)
#

dados_vac <- dados_vac %>%
  mutate(muni_estado = paste(muni_names, "-", resid_uf))

# Nomes de municípios e estados
dados_aux <- filter(dados_vac, (resid_uf != "")) # Retira NA - ""
dados_aux <- filter(dados_aux, (is.na(muni_names) == FALSE)) # Retira NA

municipiosChoices <- unique(dados_aux$muni_estado)[order(unique(dados_aux$muni_estado))]

estadosChoices2 <- unique(dados_aux$resid_uf)[order(unique(dados_aux$resid_uf))]


## Aux dos estados ----
regiao_tab <- aux_muni %>%
  dplyr::group_by(uf_sigla) %>%
  dplyr::summarise(regiao_nm = unique(regiao_nm))

# Tabelas ----

## Medidas resumo ----
media <- function(x)
  mean(x, na.rm = TRUE)
mediana <- function(x)
  median(x, na.rm = TRUE)
DP <- function(x)
  sd(x, na.rm = TRUE)
minimo <- function(x)
  base::min(x, na.rm = TRUE)
maximo <- function(x)
  base::max(x, na.rm = TRUE)
q25 <- function(x)
  stats::quantile(x, p = 0.25, na.rm = TRUE)
q75 <- function(x)
  stats::quantile(x, p = 0.75, na.rm = TRUE)
IQR <- function(x)
  round(q75(x) - q25(x), 2)
n <- function(x)
  sum(!is.na(x))


## Vacinação em Municipios ----
dados_vac_muni <-
  function(mes1 =  5,
           mes2 = 7,
           grupo = c("gesta", "puerp"),
           idade1 = 10,
           idade2 = 55,
           vacina = c("AstraZeneca",
                      "Coronavac", "Covishield", "Janssen", "Pfizer"),
           raca_cor = c("amarela", "branca", "indigena", "parda", "preta")) {
    vac_muni <- dados_vac %>%
      dplyr::filter(
        mes_aplic >= mes1 & mes_aplic <= mes2,
        gest_puerp %in% grupo,
        idade_anos >= idade1 & idade_anos <= idade2,
        qual_vacina1 %in% vacina,
        raca1 %in% raca_cor
      ) %>%
      dplyr::mutate_at("resid_muni", as.character) %>%
      dplyr::mutate(resid_muni = ifelse(is.na(resid_muni), "sem informação", resid_muni)) %>%
      dplyr::mutate(resid_muni = ifelse(is.na(resid_muni), "sem informação", resid_muni)) %>%
      dplyr::group_by(resid_muni, resid_uf) %>%
      dplyr::summarize(
        #1a dose
        n1dose = sum(num_dose == "1a dose", na.rm = TRUE),
        #1ª Dose Revacinação
        n1doserevac = sum(num_dose == "1ª Dose Revacinação", na.rm = TRUE),
        #2a dose
        n2dose = sum(num_dose == "2a dose", na.rm = TRUE),
        #2ª Dose Revacinação
        n2doserevac = sum(num_dose == "2ª Dose Revacinação", na.rm = TRUE),
        #2º Reforço
        n2reforco = sum(num_dose == "2º Reforço", na.rm = TRUE),
        #3a dose
        n3dose = sum(num_dose == "3a dose", na.rm = TRUE),
        #3ª Dose Revacinação
        n3doserevac = sum(num_dose == "3ª Dose Revacinação", na.rm = TRUE),
        #3º Reforço
        n3reforco = sum(num_dose == "3º Reforço", na.rm = TRUE),
        #4a dose
        n4dose = sum(num_dose == "4a dose", na.rm = TRUE),
        #4ª Dose Revacinação
        n4doserevac = sum(num_dose == "4ª Dose Revacinação", na.rm = TRUE),
        #5a dose
        n5dose = sum(num_dose == "5ª Dose", na.rm = TRUE),
        #5ª Dose Revacinação
        n5doserevac = sum(num_dose == "5ª Dose Revacinação", na.rm = TRUE),
        #Dose adicional + 1º reforço + reforço
        adicionalreforco = sum(num_dose == "Dose adicional + 1º reforço + reforço", na.rm = TRUE),
        #dose única
        nunica = sum(num_dose == "Dose+Única", na.rm = TRUE),
        #Revacinação
        revacinacao = sum(num_dose == "Revacinação", na.rm = TRUE)
      )
    # %>%
    # dplyr::rename(cod_mun = resid_muni)

    # Fazer a fusão dos dois bancos de dados
    dados_br_tab <- aux_extra_muni %>%
      dplyr::mutate_at("resid_muni", as.character) %>%
      dplyr::full_join(vac_muni, by = "resid_muni")

    dados_br_tab <- dados_br_tab %>%
      dplyr::mutate(
        n1dose = ifelse(is.na(n1dose), 0, n1dose),
        n1doserevac = ifelse(is.na(n1doserevac), 0, n1doserevac),
        n2dose = ifelse(is.na(n2dose), 0, n2dose),
        n2doserevac = ifelse(is.na(n2doserevac), 0, n2doserevac),
        n2reforco = ifelse(is.na(n2reforco), 0, n2reforco),
        n3dose = ifelse(is.na(n3dose), 0, n3dose),
        n3doserevac = ifelse(is.na(n3doserevac), 0, n3doserevac),
        n3reforco = ifelse(is.na(n3reforco), 0, n3reforco),
        n4dose = ifelse(is.na(n4dose), 0, n4dose),
        n4doserevac = ifelse(is.na(n4doserevac), 0, n4doserevac),
        n5dose = ifelse(is.na(n5dose), 0, n5dose),
        n5doserevac = ifelse(is.na(n5doserevac), 0, n5doserevac),
        adicionalreforco = ifelse(is.na(adicionalreforco), 0, adicionalreforco),
        nunica = ifelse(is.na(nunica), 0, nunica),
        revacinacao = ifelse(is.na(revacinacao), 0, revacinacao),
        muni_names = ifelse(resid_muni == "sem informação", "sem informação", muni_names),
        muni_names = ifelse(is.na(muni_names), "sem informação", muni_names)
      )


    dados_tabela <- dados_br_tab %>%
      select(
        Cod_IBGE = resid_muni,
        Municipio = muni_names,
        Estado = resid_uf,
        Regiao = regiao_nm,
        n1dose,
        n1doserevac,
        n2dose,
        n2doserevac,
        n2reforco,
        n3dose,
        n3doserevac,
        n3reforco,
        n4dose,
        n4doserevac,
        n5dose,
        n5doserevac,
        adicionalreforco,
        nunica,
        revacinacao
      )

    return(dados_tabela)
  }

## Vacinação em Estados ----
dados_vac_est <-
  function(mes1 =  5,
           mes2 = 7,
           grupo = c("gesta", "puerp"),
           idade1 = 10,
           idade2 = 55,
           vacina = c("AstraZeneca",
                      "Coronavac", "Covishield", "Janssen", "Pfizer"),
           raca_cor = c("amarela", "branca", "indigena", "parda", "preta")) {
    vac_est <- dados_vac %>%
      dplyr::filter(
        mes_aplic >= mes1 & mes_aplic <= mes2,
        gest_puerp %in% grupo,
        idade_anos >= idade1 & idade_anos <= idade2,
        qual_vacina1 %in% vacina,
        raca1 %in% raca_cor
      ) %>%
      dplyr::mutate_at("resid_uf", as.character) %>%
      dplyr::mutate(resid_uf = ifelse(is.na(resid_uf), "sem informação", resid_uf)) %>%
      dplyr::filter(resid_uf != "sem informação") |>
      dplyr::group_by(resid_uf) %>%
      dplyr::summarize(
        #1a dose
        n1dose = sum(num_dose == "1a dose", na.rm = TRUE),
        #1ª Dose Revacinação
        n1doserevac = sum(num_dose == "1ª Dose Revacinação", na.rm = TRUE),
        #2a dose
        n2dose = sum(num_dose == "2a dose", na.rm = TRUE),
        #2ª Dose Revacinação
        n2doserevac = sum(num_dose == "2ª Dose Revacinação", na.rm = TRUE),
        #2º Reforço
        n2reforco = sum(num_dose == "2º Reforço", na.rm = TRUE),
        #3a dose
        n3dose = sum(num_dose == "3a dose", na.rm = TRUE),
        #3ª Dose Revacinação
        n3doserevac = sum(num_dose == "3ª Dose Revacinação", na.rm = TRUE),
        #3º Reforço
        n3reforco = sum(num_dose == "3º Reforço", na.rm = TRUE),
        #4a dose
        n4dose = sum(num_dose == "4a dose", na.rm = TRUE),
        #4ª Dose Revacinação
        n4doserevac = sum(num_dose == "4ª Dose Revacinação", na.rm = TRUE),
        #5a dose
        n5dose = sum(num_dose == "5ª Dose", na.rm = TRUE),
        #5ª Dose Revacinação
        n5doserevac = sum(num_dose == "5ª Dose Revacinação", na.rm = TRUE),
        #Dose adicional + 1º reforço + reforço
        adicionalreforco = sum(num_dose == "Dose adicional + 1º reforço + reforço", na.rm = TRUE),
        #dose única
        nunica = sum(num_dose == "Dose+Única", na.rm = TRUE),
        #Revacinação
        revacinacao = sum(num_dose == "Revacinação", na.rm = TRUE)
      )  %>%
      dplyr::rename(uf_sigla = resid_uf)

    # Fazer a fusão dos dois bancos de dados
    dados_br_tab <- regiao_tab %>%
      dplyr::full_join(vac_est, by = "uf_sigla")

    dados_br_tab <- dados_br_tab %>%
      dplyr::mutate(
        n1dose = ifelse(is.na(n1dose), 0, n1dose),
        n1doserevac = ifelse(is.na(n1doserevac), 0, n1doserevac),
        n2dose = ifelse(is.na(n2dose), 0, n2dose),
        n2doserevac = ifelse(is.na(n2doserevac), 0, n2doserevac),
        n2reforco = ifelse(is.na(n2reforco), 0, n2reforco),
        n3dose = ifelse(is.na(n3dose), 0, n3dose),
        n3doserevac = ifelse(is.na(n3doserevac), 0, n3doserevac),
        n3reforco = ifelse(is.na(n3reforco), 0, n3reforco),
        n4dose = ifelse(is.na(n4dose), 0, n4dose),
        n4doserevac = ifelse(is.na(n4doserevac), 0, n4doserevac),
        n5dose = ifelse(is.na(n5dose), 0, n5dose),
        n5doserevac = ifelse(is.na(n5doserevac), 0, n5doserevac),
        adicionalreforco = ifelse(is.na(adicionalreforco), 0, adicionalreforco),
        nunica = ifelse(is.na(nunica), 0, nunica),
        revacinacao = ifelse(is.na(revacinacao), 0, revacinacao)
      )

    dados_tabela <- dados_br_tab %>%
      select(Estado = uf_sigla,
             Regiao = regiao_nm,
             n1dose,
             n1doserevac,
             n2dose,
             n2doserevac,
             n2reforco,
             n3dose,
             n3doserevac,
             n3reforco,
             n4dose,
             n4doserevac,
             n5dose,
             n5doserevac,
             adicionalreforco,
             nunica,
             revacinacao)
    return(dados_tabela)
  }

sticky_style <-
  list(
    position = "sticky",
    left = 0,
    background = "#fff",
    zIndex = 1,
    borderRight = "1px solid #eee"
  )

# Dia de hoje ----
hoje <- Sys.Date()

# Que vamos usar para medidas resumo ----
media <- function(x)
  mean(x, na.rm = TRUE)
mediana <- function(x)
  median(x, na.rm = TRUE)
DP <- function(x)
  sd(x, na.rm = TRUE)
minimo <- function(x)
  base::min(x, na.rm = TRUE)
maximo <- function(x)
  base::max(x, na.rm = TRUE)
q25 <- function(x)
  stats::quantile(x, p = 0.25, na.rm = TRUE)
q75 <- function(x)
  stats::quantile(x, p = 0.75, na.rm = TRUE)
IQR <- function(x)
  round(q75(x) - q25(x), 2)
n <- function(x)
  round(sum(!is.na(x)), digits = 0)
faltantes <- function(x)
  round(sum(is.na(x)), digits = 0)
# Inscrição para Newsletter ----
gs4_auth(cache = ".secrets",
         email = TRUE,
         use_oob = TRUE)

# which fields get saved
fieldsAll <- c("nome", "email")

# which fields are mandatory
fieldsMandatory <- c("nome", "email")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(label,
          span("*", class = "mandatory_star"))
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

table <- "responses"

saveData <- function(data) {
  data <- data  %>% as.list() %>% data.frame()
  Selfie <-
    gs4_get(
      'https://docs.google.com/spreadsheets/d/1Zs3jMI3CKr637QBGaveP8_doGEVW-ZaGtw97KnAqa7E/edit?usp=sharing'
    )
  sheet_append(Selfie, data)
}


# directory where responses get stored
responsesDir <- file.path("responses")

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

# info for sharing this app on facebook/twitter
share <- list(title = "Inscrição na Newsletter")

# User Interface ----
ui <-
  dashboardPage(
    title = "OOBr Vacinação",
    dashboardHeader(title = 'OOBr Vacinação'),
    dashboardSidebar(
      ## Menu ----
      sidebarMenu(
        style = "position: fixed; overflow: visible",
        menuItem("Início", tabName = "inicio"),
        menuItem("Documentação", tabName = "doc"),
        menuItem("Vacinação COVID-19", tabName = "vac_casos"),
        menuItem("Vacinação estado e município", tabName = "vac_est_muni"),
        menuItem("Inconsistências vacinação", tabName = "incons_vac")
      )
    ),
    ### Item Inicio ----
    dashboardBody(
      tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #0A1E3C;
                              }
        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #0A1E3C;
                              }
        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #0A1E3C;
                              }
        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #0A1E3C;
                              }
        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #32A0FF;
                              }
        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #0A1E3C;
                              color: #FFFFFF;
                              }
        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #32A0FF;
                              }
        /* toggle button when hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #32A0FF;
                              }
                              '),
        HTML("hr {border-top: 1px solid #0A1E3C;}")
        )
        ),
      tabItems(
        tabItem(
          tabName = "inicio",
          div(img(
            src = "logo_oobr_curto.png",
            height = 100,
            width = 175
          ), style = "text-align: center;"),
          # Inscrição na Newsletter
          # h1("Inscrição na Newsletter"),
          # ("Se inscreva para receber as atualizações do OOBr"),
          # fluidRow(
          #   shinyjs::useShinyjs(),
          #   shinyjs::inlineCSS(appCSS),
          #   column(
          #     6,
          #     div(
          #       id = "form",
          #       textInput("nome", labelMandatory("Nome"), ""),
          #       textInput("email", labelMandatory("Email")),
          #       actionButton("submit", "Inscreva-se", class = "btn-primary"),
          #
          #       shinyjs::hidden(span(id = "submit_msg", "Submetendo..."),
          #                       div(id = "error",
          #                           div(
          #                             br(), tags$b("Erro: "), span(id = "error_msg")
          #                           )))
          #     ),
          #
          #     shinyjs::hidden(div(
          #       id = "thankyou_msg",
          #       h3("Obrigada, sua inscrição foi enviada!")
          #     ))
          #   )
          # ),
          br(),
          br(),
          fluidRow(column(width = 12)),
          fluidRow(
            valueBox(
              nrow(dados_vac),
              "Doses de vacina em gestantes e puérperas",
              icon = icon(""),
              color = "navy"
            ),
            valueBox(
              nrow(dados_vac[dados_vac$num_dose == "1a dose", ]),
              "Primeira dose em gestantes e puérperas",
              color = "blue"
            ),
            valueBox(
              nrow(dados_vac[(dados_vac$num_dose == "2a dose" | dados_vac$num_dose == "Dose+Única") &
                               !is.na(dados_vac$num_dose), ]),
              "Gestantes e puérperas com 2ª dose ou dose única",
              icon = icon(""),
              color = "light-blue"
            )
          ),
          strong(
            "Dados de Vacinação atualizados em 06/junho/2023. Para maiores informações, ver 'Documentação'."
          ),
          h1(strong("Quem somos")),
          p(
            "O Observatório Obstétrico Brasileiro COVID-19 Vacinação (OOBr Vacinação) é um painel
        dinâmico com análises dos casos de vacinação contra COVID-19 em gestantes e puérperas informadas pela Campanha Nacional de Vacinação contra Covid-19 (https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao). Para maiores informações sobre os dados e sobre
        as análises, veja a aba 'Documentação'."
          ),
          p(
            "O OOBr Vacinação faz parte do projeto Observatório Obstétrico Brasileiro. Este projeto visa disponibilizar uma plataforma interativa de monitoramento,
      análise de dados públicos cientificamente embasadas e disseminação informações de qualidade e relevantes da área de Saúde Materno-Infantil.
      Nesse observatório são disponibilizadas as análises exploratórias dos dados, com visualização online, dinâmica e com filtragens escolhidas pelo usuário,
      além dos resultados de modelos para os desfechos obstétricos."
          ),
          # ("O Observatório Obstétrico Brasileiro conta com pesquisadores da"),
          # a("UFES,", href = "https://estatistica.ufes.br/"),
          # a("USP e", href = "https://www5.usp.br/tag/departamento-de-ginecologia-e-obstetricia/"),
          # ("e tem o financiamento da"),
          # a("Fundação Bill-Melinda Gates,", href = "https://www.gatesfoundation.org/"),
          # a("CNPq,", href = "https://www.gov.br/cnpq/pt-br"),
          # a("DECIT e", href = "https://www.gov.br/saude/pt-br"),
          # a("FAPES.", href = "https://fapes.es.gov.br/"),
          # br(),
          # ("Além disso, o OOBr conta com a parceria da "),
          # a("PCDaS.", href = "https://pcdas.icict.fiocruz.br/"),
          # ("Para mais informações sobre o Observatório,"),
          # a("clique aqui.", href = "https://observatorioobstetricobr.org/"),
          h2(strong("Como citar o OOBr Vacinação")),
          (
            "Observatório Obstétrico Brasileiro. OOBr Vacinação COVID-19: vacinação contra COVID-19 de gestantes e puérperas, 2021.
            Disponível em"
            ),
          a("https://observatorioobstetrico.shinyapps.io/vacinacao-covid19.", href = "https://observatorioobstetrico.shinyapps.io/vacinacao-covid19"),
          ("DOI:"),
          a("https://doi.org/10.7303/syn44679538", href = "https://doi.org/10.7303/syn44679538"),
          # p(
          #   "[1] Francisco, R., Lacerda, L., & Rodrigues, A. S. (2021). Obstetric Observatory BRAZIL - COVID-19: 1031 maternal deaths because of COVID-19 and the unequal access to health care services. Clinics (Sao Paulo, Brazil), 76, e3120. https://doi.org/10.6061/clinics/2021/e3120"
          # ),
          # p(
          #   "[2] Rodrigues, A. and Lacerda, L. and Francisco, R.P.V. 'Brazilian Obstetric Observatory' arXiv preprint arXiv:2105.06534 (2021)."
          # ),
          h2(strong("Contato")),
          p(
            "Para comentários, sugestões e colaborações científicas, por favor, envie mensagem para nós em observatorioobstetricobr@gmail.com"
          ),
          h2(strong("Painéis do OOBr")),
          br(),
          div(img(
            src = "paineis.jpg",
            heigth = 200,
            width = 550
          ), style = "text-align: center;"),
          br(),
          br(),
          p(
            "Os painéis de visualização do OOBr visam dar visibilidade aos dados de gestantes, puérperas e crianças de até 2 anos e oferecer ferramentas para análise e fundamentação de políticas públicas
            para atenção à saúde materno-infantil no Brasil. Acesse-os no link abaixo:"
          ),
          a("https://observatorioobstetricobr.org/paineis/", href = "https://observatorioobstetricobr.org/paineis/"),
          br(),
      hr(),

      fluidRow(
        tags$style(
          HTML(
            "
        .inner {
          width: 100%;
          display: inline-block;
        }
        .outer {
          width: 100%;
          text-align: center;
        }
        "
          )
        ),
        column(
          width = 12,
          HTML(
            "
            <h2> <b> Realização </b> </h2>

            <div class = 'outer'>
              <div class = 'inner'>
                <img src = 'logo_oobr.png' width = '170px' style = 'margin-right:20px>
                <img src = 'logo_daslab.png' width = '170px'>
                <img src = 'logo_ufes.png' width = '170px'>
                <img src = 'logo_medicina_usp.png' width = '170px'>
              </div>
            </div>


            "
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          HTML(
            "
            <h2> <b> Financiadores </b> </h2>

            <div class = 'outer'>
              <div class = 'inner'>
                <img src = 'logo_bill_melinda.png' width = '170px' style = 'margin-right:20px>
                <img src = 'logo_cnpq.png' width = '170px'>
                <img src = 'logo_ms.png' width = '450px'>
                <img src = 'logo_fapes.png' width = '450px'>
              </div>
            </div>


            "
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
           HTML(
            "
            <h2> <b> Apoio </b> </h2>

            <div class = 'outer'>
              <div class = 'inner'>
                <img src = 'logo_pcdas.png' width = '130px' style = 'margin-right:20px>
                <img src = 'logo_odd2.png' width = '130px'>
                <img src = 'logo_odd.png' width = '130px'>
              </div>
            </div>


            "
          )
        )
      )
      ),
        ### Item Newsletter ----
        tabItem(
          shinyjs::useShinyjs(),
          shinyjs::inlineCSS(appCSS),
          tabName = "newsletter",
          fluidRow(column(
            6,
            div(
              id = "form",
              textInput("nome", labelMandatory("Nome"), ""),
              textInput("email", labelMandatory("Email")),
              actionButton("submit", "Inscreva-se", class = "btn-primary"),

              shinyjs::hidden(span(id = "submit_msg", "Submetendo..."),
                              div(id = "error",
                                  div(
                                    br(), tags$b("Erro: "), span(id = "error_msg")
                                  )))
            ),

            shinyjs::hidden(div(
              id = "thankyou_msg",
              h3("Obrigada, sua inscrição foi enviada!")#,
              # actionLink("submit_another", "Submit another response")
            ))
          )
          # column(6,
          #        uiOutput("adminPanelContainer")
          # ))
          )),
        ### Item Documentação ----
        tabItem(
          tabName = "doc",
          h1(strong("Fontes")),
          h2(strong("Dados sobre Vacinação")),
          (
            "Utilizamos os registros das notificações de vacinação
            da 'Campanha Nacional de Vacinação contra Covid-19'
            "
          ),
          br(),
          br(),
          #("Mais informações sobre as variáveis podem ser encontrados em: "),
          #a(" Informações sobre vacinação contra Covid-19.", href = "https://opendatasus.saude.gov.br/dataset/b772ee55-07cd-44d8-958f-b12edd004e0b/resource/38ead83d-b115-4219-852e-7244792bc311/download/dicionario-de-dados-vacinacao.pdf"),
          #br(),
          #br(),
          (
            "A atualização desta base é disponibilizada pelo Ministério da Saúde pelo portal"
          ),
          a(" Open Data SUS.", href = "https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao"),
          br(),
          br(),
          (
            "A última atualização foi realizada em 07/junho/2023 (dados atualizados pelo MS do dia anterior)."
          ),
          br(),
          br(),
          p(
            "São disponibilizados aqui os casos definidos como gestante ou puérpera,
            vacinados nos anos 2021, 2022 e 2023 do sexo F, entre 10 e 55 anos."
          ),
          # downloadButton(
          #   outputId = "download_dados_csv",
          #   label = "Download da base de dados em formato .csv"
          # ),
          # downloadButton(
          #   outputId = "download_dados_xlsx",
          #   label = "Download da base de dados em formato .xlsx"
          # ),
          actionButton("generateVac", "Gerar pdf da documentação de Vacinação"),
          uiOutput("pdfview_vac"),
          br(),
          br(),
          h2(
            "Como baixar os dados desse painel:"
          ),
          (
            "Rodrigues, A. S.; Francisco, R. P. V. Vacinação contra COVID-19 em
            gestantes e puérperas [banco de dados], 2021, Observatório Obstétrico Brasileiro (OOBr)."
          ),
          ("Disponível em DOI:"),
          a("https://doi.org/10.7303/syn50680046", href = "https://doi.org/10.7303/syn50680046")
        ),
        ### Item Vacinação Graficos ----
        tabItem(tabName = "vac_casos",
                fluidRow(
                  box(
                    collapsible = TRUE,
                    width = 4,
                    title = "Selecione",
                    status = "primary",
                    solidHeader = FALSE,
                    selectInput(
                      "selectestvac",
                      "Selecionar o estado? (estado de residência)",
                      c("Não" = "br",
                        "Sim" = "est")
                    ),
                    # Only show this panel if est
                    conditionalPanel(
                      condition = "input.selectestvac == 'est'",
                      selectInput(
                        "estado_vac",
                        "Estado",
                        choices = estadosChoices1,
                        selected = "ES"
                      )
                    ),
                    checkboxGroupInput(
                      inputId = "gest_puerp",
                      label = "Gestante ou puérpera:",
                      choices = c("Gestante" = "gesta",
                                  "Puérpera" = "puerp"),
                      selected = c("gesta", "puerp")
                    ),
                    sliderInput(
                      inputId = "idade_vac",
                      label = "Intervalo de idade:",
                      min = min(dados_vac$idade_anos),
                      max = max(dados_vac$idade_anos),
                      value = c(min(dados_vac$idade_anos), max(dados_vac$idade_anos))
                    ),
                    checkboxGroupInput(
                      inputId = "raca_vac",
                      label = "Raça:",
                      choices = c(
                        "Amarela" = "amarela",
                        "Branca" = "branca",
                        "Indígena" = "indigena",
                        "Parda" = "parda",
                        "Preta" = "preta",
                        "Não informado" = "não informado"
                      ),
                      selected = c(
                        "amarela",
                        "branca",
                        "indigena",
                        "parda",
                        "perda",
                        "preta",
                        "não informado"
                      )
                    ),
                    checkboxGroupInput(
                      inputId = "tipo_vac",
                      label = "Tipo de vacina:",
                      choices = c(
                        "AstraZeneca" = "AstraZeneca",
                        "Bivalente" = "Bivalente",
                        "Coronavac" = "Coronavac",
                        "Janssen" = "Janssen",
                        "Pfizer" = "Pfizer",
                        "Sem informação" = "não informado"
                      ),
                      selected = c(
                        "AstraZeneca",
                        "Coronavac",
                        "Bivalente",
                        "Janssen",
                        "Pfizer",
                        "não informado"
                      )
                    )
                  ),
                  box(
                    width = 8,
                    status = "primary",
                    solidHeader = FALSE,
                    plotly::plotlyOutput("vacinacao_perDay"),
                    plotly::plotlyOutput("vacinacao_acum")
                  )
                )),
        ### Item Vacinação Tabelas ----
        tabItem(tabName = "vac_est_muni",
                fluidRow(
                  box(
                    collapsible = TRUE,
                    width = 3,
                    title = "Selecione",
                    status = "primary",
                    solidHeader = FALSE,
                    checkboxGroupInput(
                      inputId = "ano_vacina",
                      label = "Ano de vacinação",
                      choices = c("2021" = "2021",
                                       "2022" = "2022",
                                       "2023" = "2023"),
                      selected = c("2021", "2022", "2023")
                    ),
                    sliderInput(
                      inputId = "mes_vac",
                      label = "Mês de aplicação: (1 - Janeiro, 2 - Fevereiro, ...)",
                      min = min(dados_vac$mes_aplic),
                      max = max(dados_vac$mes_aplic),
                      value = c(min(dados_vac$mes_aplic), max(dados_vac$mes_aplic)),
                      step = 1
                    ),
                    checkboxGroupInput(
                      inputId = "gest_puerp_est_muni",
                      label = "Gestante ou puérpera:",
                      choices = c("Gestante" = "gesta",
                                  "Puérpera" = "puerp"),
                      selected = c("gesta", "puerp")
                    ),
                    sliderInput(
                      inputId = "idade_vac_est_muni",
                      label = "Intervalo de idade:",
                      min = min(dados_vac$idade_anos),
                      max = max(dados_vac$idade_anos),
                      value = c(min(dados_vac$idade_anos), max(dados_vac$idade_anos))
                    ),
                    checkboxGroupInput(
                      inputId = "tipo_vac_est_muni",
                      label = "Tipo de vacina:",
                      choices = c(
                        "AstraZeneca" = "AstraZeneca",
                        "Bivalente" = "Bivalente",
                        "Coronavac" = "Coronavac",
                        "Janssen" = "Janssen",
                        "Pfizer" = "Pfizer",
                        "Sem informação" = "não informado"
                      ),
                      selected = c(
                        "AstraZeneca",
                        "Coronavac",
                        "Bivalente",
                        "Janssen",
                        "Pfizer",
                        "não informado"
                      )
                    ),
                    checkboxGroupInput(
                      inputId = "raca_vac_est_muni",
                      label = "Raça:",
                      choices = c(
                        "Amarela" = "amarela",
                        "Branca" = "branca",
                        "Indígena" = "indigena",
                        "Parda" = "parda",
                        "Preta" = "preta",
                        "Não informado" = "não informado"
                      ),
                      selected = c(
                        "amarela",
                        "branca",
                        "indigena",
                        "parda",
                        "perda",
                        "preta",
                        "não informado"
                      )
                    ),
                    selectInput(
                      "sel_notific_down2",
                      "Fazer download?",
                      c("Não" = "nano",
                        "Sim" = "sino")
                    ),
                    # Only show this panel if 2020 ou 2021
                    conditionalPanel(
                      condition = "input.sel_notific_down2 != 'nano'",
                      selectInput(
                        "sel_notific_down_tipo2",
                        "Tipo do arquivo",
                        c(
                          "Csv" = "csv",
                          "Excel" = "excel"
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.sel_notific_down_tipo2 == 'csv' & input.sel_notific_down2 != 'nano'",
                      downloadButton('download_vac_est_csv', "Download tabela estado"),
                      downloadButton('download_vac_muni_csv', "Download tabela municipio")
                    ),
                    conditionalPanel(
                      condition = "input.sel_notific_down_tipo2 == 'excel' & input.sel_notific_down2 != 'nano'",
                      downloadButton('download_vac_est_excel', "Download tabela estado"),
                      downloadButton('download_vac_muni_excel', "Download tabela municipio")
                    )
                  ),
                  box(
                    width = 9,
                    status = "primary",
                    solidHeader = FALSE,
                    h1("Estados e municípios de residência"),
                    div(tabsetPanel(
                      tabPanel("Estados",
                               reactableOutput("table_vac_estado")),
                      tabPanel("Municípios",
                               reactableOutput("table_vac_muni"))
                    )
                    )
                  )
                )
        ),
        ### Item de inconsistências nas vacinas ----
        tabItem(tabName = "incons_vac",
                fluidRow(
                  valueBox(nrow(dados_vac_bruto[dados_vac_bruto$sexo == "M", ]),
                           "Gestantes/Puerperas com sexo masculino",
                           color = "navy"),
                  valueBox(nrow(dados_vac_bruto[year(dados_vac_bruto$dt_aplic) < 2021, ]),
                           "vacinas aplicadas em gestantes/puerperas no ano de 2020",
                           color = "blue"),
                  valueBox(nrow(dados_vac_bruto[dados_vac_bruto$idade_anos < 10 | dados_vac_bruto$idade_anos > 55, ]),
                           "vacinas aplicadas em gestantes/puerperas com idade menor que 10 e maior que 55 anos",
                           color = "light-blue"),
                  box(
                    width = 12,
                    status = "primary",
                    solidHeader = FALSE,
                    div(tabsetPanel(
                      tabPanel("Sexo masculino",
                               reactableOutput("table_vac_masc")),
                      tabPanel("Vacinas aplicadas no ano de 2020",
                               reactableOutput("table_vac_2020")),
                      tabPanel("Idade > 55 ou Idade < 10",
                               reactableOutput("table_vac_idade_incons"))
                    )
                    )
                  )
                ))
      )
    )
  )

# Server ----
server <- function(input, output, session) {
  ## download base do painel ----
  output$download_dados_xlsx <- downloadHandler(
    filename = function() {
      "dados.xlsx"
    },
    content = function(fname) {
      writexl::write_xlsx(
        dados_vac,
        fname
      )
    })

  output$download_dados_csv <- downloadHandler(
    filename = function() {
      "dados.csv"
    },
    content = function(fname) {
      write.csv(
        dados_vac,
        fname
      )
    })

  ## Geração de PDF de Documentação ----
  observeEvent(input$generateVac, {
    output$pdfview_vac <- renderUI({
      tags$iframe(style = "height:600px; width:100%", src = "doc_vacinacao_covid-Gesta-Puerp_OOBrCOVID-19.pdf")
    })
  })

  ## Sétima base de dados com filtragem por inputs ----
  selectData7 <- reactive({
    dados_vac %>%
      dplyr::filter(idade_anos >= input$idade_vac[1]) %>%
      dplyr::filter(idade_anos <= input$idade_vac[2]) %>%
      dplyr::filter(gest_puerp %in% input$gest_puerp) %>%
      dplyr::filter(qual_vacina1 %in% input$tipo_vac) %>%
      dplyr::filter(raca1 %in% input$raca_vac) %>%
      dplyr::filter(if (input$selectestvac == "est")
        resid_uf == input$estado_vac
        else
          is.na(resid_uf) | !is.na(resid_uf))
  })

  ### Gráfico de vacinação por dia ----
  output$vacinacao_perDay <- plotly::renderPlotly({
    dados_graf_vac <- selectData7() %>%
      count(dt_aplic, num_dose) %>%
      mutate(dt_aplic = as.Date(dt_aplic))

    p1 <- ggplot(dados_graf_vac) +
      geom_col(aes(x = dt_aplic, y = n, fill = num_dose)) +
      labs(title = "N° de vacinações diárias", x = "Data de aplicação", y = NULL, fill = "Dose") +
      scale_fill_viridis(option = "turbo", discrete = TRUE) +
      theme_minimal()

    ggplotly(p1, dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x")
  })

  ### Gráfico de vacinação acumulada ----
  output$vacinacao_acum <- plotly::renderPlotly({
    dados_graf_vac <- selectData7() %>%
      count(dt_aplic, num_dose) %>%
      arrange(num_dose, dt_aplic)

    dados_aux <- dados_graf_vac %>%
      group_by(num_dose) %>%
      summarise(acumulado = cumsum(n))

    dados_graf_vac <- cbind(dados_graf_vac, dados_aux[, 2])

    p1 <-
      ggplot(dados_graf_vac,
             aes(
               x = dt_aplic,
               y = acumulado,
               fill = num_dose,
               colour = num_dose,
               group = 1
             )) +
      geom_line(aes(text = paste0("Data de aplicação: ", dt_aplic,
                                 "<br>Número de vacinas aplicadas até esse momento: ", acumulado,
                                 "<br>Dose: ", num_dose))) +
      geom_ribbon(aes(ymin = 0, ymax = acumulado), alpha = .5) +
      labs(title = "N° de vacinações acumuladas", x = "Data", y = NULL, colour = "Dose", fill = "Dose") +
      scale_fill_viridis(option = "turbo", discrete = TRUE) +
      scale_color_viridis(option = "turbo", discrete = TRUE) +
      theme_minimal()

    ggplotly(p1, dynamicTicks = TRUE, tooltip = "text") %>%
      rangeslider() %>%
      layout(hovermode = "x")
  })

  ### Tabela para vacinas em municipios ----
  dados_vac_muni_teste <- reactive({
    dados_vac |>
      dplyr::filter(ano_aplic %in% input$ano_vacina) |>
      dplyr::filter(mes_aplic >= mes_vac[1]) |>
      dplyr::filter(mes_aplic <= mes_vac[1]) |>
      dplyr::filter(gest_puerp %in% input$gest_puerp_est_muni) |>
      dplyr::filter(idade_anos >= input$idade_vac_est_muni[1]) |>
      dplyr::filter(idade_anos <= input$idade_vac_est_muni[2]) |>
      dplyr::filter(qual_vacina %in% input$tipo_vac_est_muni) |>
      dplyr::filter(raca %in% input$raca_vac_est_muni)
  })


  ### Tabela para vacinas em municipios ----
  output$table_vac_muni <- renderReactable({
    reactable(
      dados_vac_muni(
        mes1 = input$mes_vac[1],
        mes2 = input$mes_vac[2],
        grupo = input$gest_puerp_est_muni,
        idade1 = input$idade_vac_est_muni[1],
        idade2 = input$idade_vac_est_muni[2],
        vacina = input$tipo_vac_est_muni,
        raca_cor = input$raca_vac_est_muni
      ),
      filterable = TRUE,
      showSortable = TRUE,
      searchable = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 15, 30),
      defaultPageSize = 15,
      striped = TRUE,
      highlight = TRUE,
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "8px 12px",
        style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
        searchInputStyle = list(width = "100%")
      ),
      columns = list(
        Municipio = colDef(
          name = "Municípios",
          style = sticky_style,
          headerStyle = sticky_style
        ),
        Estado = colDef(name = "Estado"),
        Regiao = colDef(name = "Região"),
        n1dose = colDef(name = "1ª dose"),
        n1doserevac = colDef(name = "1ª dose revacinação"),
        n2dose = colDef(name = "2ª dose"),
        n2doserevac = colDef(name = "2ª dose revacinação"),
        n2reforco = colDef(name = "2ª dose reforço"),
        n3dose = colDef(name = "3ª dose"),
        n3doserevac = colDef(name = "3ª dose revacinação"),
        n3reforco = colDef(name = "3ª dose reforço"),
        n4dose = colDef(name = "4ª dose"),
        n4doserevac = colDef(name = "4ª dose revacinação"),
        n5dose = colDef(name = "5ª dose"),
        n5doserevac = colDef(name = "5ª dose revacinação"),
        adicionalreforco = colDef(name = "Dose adicional + reforço"),
        nunica = colDef(name = "Dose única"),
        revacinacao = colDef(name = "Revacinação")
      ),
      defaultColDef = colDef(minWidth = 150)
    )
  })

  ### Tabela para vacinas em estados ----
  output$table_vac_estado <- renderReactable({
    reactable(
      dados_vac_est(
        mes1 = input$mes_vac[1],
        mes2 = input$mes_vac[2],
        grupo = input$gest_puerp_est_muni,
        idade1 = input$idade_vac_est_muni[1],
        idade2 = input$idade_vac_est_muni[2],
        vacina = input$tipo_vac_est_muni,
        raca_cor = input$raca_vac_est_muni
      ),
      filterable = TRUE,
      showSortable = TRUE,
      searchable = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 15, 30),
      defaultPageSize = 15,
      striped = TRUE,
      highlight = TRUE,
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "8px 12px",
        style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
        searchInputStyle = list(width = "100%")
      ),
      columns = list(
        Estado = colDef(style = sticky_style,
                        headerStyle = sticky_style),
        Estado = colDef(name = "Estado"),
        Regiao = colDef(name = "Região"),
        n1dose = colDef(name = "1ª dose"),
        n1doserevac = colDef(name = "1ª dose revacinação"),
        n2dose = colDef(name = "2ª dose"),
        n2doserevac = colDef(name = "2ª dose revacinação"),
        n2reforco = colDef(name = "2ª dose reforço"),
        n3dose = colDef(name = "3ª dose"),
        n3doserevac = colDef(name = "3ª dose revacinação"),
        n3reforco = colDef(name = "3ª dose reforço"),
        n4dose = colDef(name = "4ª dose"),
        n4doserevac = colDef(name = "4ª dose revacinação"),
        n5dose = colDef(name = "5ª dose"),
        n5doserevac = colDef(name = "5ª dose revacinação"),
        adicionalreforco = colDef(name = "Dose adicional + reforço"),
        nunica = colDef(name = "Dose única"),
        revacinacao = colDef(name = "Revacinação")
      ),
      defaultColDef = colDef(minWidth = 150)
    )
  })


  ## Para download de tabelas de vacinação ----
  ### csv estado ----
  output$download_vac_est_csv <- downloadHandler(
    filename = function() {
      "table_est_vac.csv"
    },
    content = function(fname2) {
      write.csv(
        dados_vac_est(

          mes1 = input$mes_vac[1],
          mes2 = input$mes_vac[2],
          grupo = input$gest_puerp_est_muni,
          idade1 = input$idade_vac_est_muni[1],
          idade2 = input$idade_vac_est_muni[2],
          vacina = input$tipo_vac_est_muni,
          raca_cor = input$raca_vac_est_muni
        ),
        fname2
      )
    })

  ### csv municipio ----
  output$download_vac_muni_csv <- downloadHandler(
    filename = function() {
      "table_muni_vac.csv"
    },
    content = function(fname) {
      write.csv(
        dados_vac_muni(
          ano_vac = input$ano_vacina,
          mes1 = input$mes_vac[1],
          mes2 = input$mes_vac[2],
          grupo = input$gest_puerp_est_muni,
          idade1 = input$idade_vac_est_muni[1],
          idade2 = input$idade_vac_est_muni[2],
          vacina = input$tipo_vac_est_muni,
          raca_cor = input$raca_vac_est_muni
        ),
        fname
      )
    })

  ### xls estado ----
  output$download_vac_est_excel <- downloadHandler(
    filename = function() {
      "table_est_vac.xlsx"
    },
    content = function(fname) {
      writexl::write_xlsx(
        dados_vac_est(
          mes1 = input$mes_vac[1],
          mes2 = input$mes_vac[2],
          grupo = input$gest_puerp_est_muni,
          idade1 = input$idade_vac_est_muni[1],
          idade2 = input$idade_vac_est_muni[2],
          vacina = input$tipo_vac_est_muni,
          raca_cor = input$raca_vac_est_muni
        ),
        fname
      )
    })

  ### xls municipio ----
  output$download_vac_muni_excel <- downloadHandler(
    filename = function() {
      "table_muni_vac.xlsx"
    },
    content = function(fname) {
      writexl::write_xlsx(
        dados_vac_muni(
          mes1 = input$mes_vac[1],
          mes2 = input$mes_vac[2],
          grupo = input$gest_puerp_est_muni,
          idade1 = input$idade_vac_est_muni[1],
          idade2 = input$idade_vac_est_muni[2],
          vacina = input$tipo_vac_est_muni,
          raca_cor = input$raca_vac_est_muni
        ),
        fname
      )
    })

  ### Para inscrição na Newsletter ----
  # Enable the Submit button when all mandatory fields are filled out
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)

    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })

  # Gather all the form inputs (and add timestamp)
  formData <- reactive({
    data <- sapply(fieldsAll, function(x)
      input[[x]])
    data <- c(data, timestamp = humanTime())
    data <- t(data)
    data
  })

  # When the Submit button is clicked, submit the response
  observeEvent(input$submit, {
    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")

    # Save the data (show an error message in case of error)
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error",
                    anim = TRUE,
                    animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })

  # submit another response
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })

  ### tabela de dados inconsistentes de vacinação ----
  output$table_vac_masc <- reactable::renderReactable({
    dados_masc <-
      dados_vac_bruto[dados_vac_bruto$sexo == "M", c(
        "resid_uf",
        "aplicacao_muni",
        "resid_muni",
        "dt_aplic",
        "gest_puerp",
        "idade_anos",
        "raca",
        "qual_vacina",
        "num_dose",
        "sexo"
      )]


    reactable(dados_masc, groupBy = "resid_uf",
              columns = list(
                resid_uf = colDef(name = "UF residência"),
                aplicacao_muni = colDef(name = "Município de aplicação"),
                resid_muni = colDef(name = "Município de residência"),
                dt_aplic = colDef(name = "Data de aplicação"),
                gest_puerp = colDef(name = "Gestante ou puérpera"),
                idade_anos = colDef(name = "Idade"),
                raca = colDef(name = "Raça"),
                qual_vacina = colDef(name = "Vacina"),
                num_dose = colDef(name = "Dose"),
                sexo = colDef(name = "Sexo")
              ))
  })

  output$table_vac_2020 <- reactable::renderReactable({
    dados_2020 <-
      dados_vac_bruto[year(dados_vac_bruto$dt_aplic) < 2021, c(
        "resid_uf",
        "aplicacao_muni",
        "resid_muni",
        "dt_aplic",
        "gest_puerp",
        "idade_anos",
        "raca",
        "qual_vacina",
        "num_dose",
        "sexo"
      )]

    reactable(dados_2020, groupBy = "resid_uf",
              columns = list(
                resid_uf = colDef(name = "UF residência"),
                aplicacao_muni = colDef(name = "Município de aplicação"),
                resid_muni = colDef(name = "Município de residência"),
                dt_aplic = colDef(name = "Data de aplicação"),
                gest_puerp = colDef(name = "Gestante ou puérpera"),
                idade_anos = colDef(name = "Idade"),
                raca = colDef(name = "Raça"),
                qual_vacina = colDef(name = "Vacina"),
                num_dose = colDef(name = "Dose"),
                sexo = colDef(name = "Sexo")
              ))
  })

  output$table_vac_idade_incons <- reactable::renderReactable({
    dados_vac_idade_incons <-
      dados_vac_bruto[dados_vac_bruto$idade_anos < 10 | dados_vac_bruto$idade_anos > 55, c(
        "resid_uf",
        "aplicacao_muni",
        "resid_muni",
        "dt_aplic",
        "gest_puerp",
        "idade_anos",
        "raca",
        "qual_vacina",
        "num_dose",
        "sexo"
      )]

    reactable(dados_vac_idade_incons, groupBy = "resid_uf",
              columns = list(
                resid_uf = colDef(name = "UF residência"),
                aplicacao_muni = colDef(name = "Município de aplicação"),
                resid_muni = colDef(name = "Município de residência"),
                dt_aplic = colDef(name = "Data de aplicação"),
                gest_puerp = colDef(name = "Gestante ou puérpera"),
                idade_anos = colDef(name = "Idade"),
                raca = colDef(name = "Raça"),
                qual_vacina = colDef(name = "Vacina"),
                num_dose = colDef(name = "Dose"),
                sexo = colDef(name = "Sexo")
              ))
  })
}

shinyApp(ui, server)
