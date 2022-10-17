# -------------------------- carregamento dos pacotes --------------------------
pacotes <- c(
  "tidyverse",
  "tsibble",
  "lubridate",
  "zoo",
  "scales",
  "feasts",
  "gridExtra",
  "fable"
)

if (sum(as.numeric(!pacotes %in% installed.packages())) != 0) {
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for (i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()
  }
  sapply(pacotes, require, character = T)
  
} else {
  sapply(pacotes, require, character = T)
}
# Créditos: Prof. Rafael Souza e Prof. Luis Paulo Fávero


# ---------------------------- importação dos dados ----------------------------
#
# fonte: Our world in data
#
# campos: país,
#         data,
#         mortes no dia
#
# pacotes: readr, tidyverse, tsibble, lubridate, zoo

covid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  select(location,
         date,
         new_deaths) %>%
  as_tsibble(index = date,
             key = location) %>%
  filter(date < today(),
         location == "Brazil") %>%
  group_by(location) %>%
  mutate(
    #Substituindo NA por 0 para evitar erros nas funções
    new_deaths  = replace_na(new_deaths,  0),
    #Criando variáveis com a média movél de 7 dias
    ma_deaths   = rollmean(
      new_deaths,
      k = 7,
      fill = 0,
      align = "right"
    ),
  )


# -------------------------- visualização dos dados ----------------------------

#mortes diárias com média móvel de 7 dias
#pacotes: +scales
covid %>%
  autoplot(new_deaths, color = "yellow2") +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%Y %b") +
  geom_line(aes(y = ma_deaths)) +
  labs(x = "Dias",
       y = "Mortes",
       # title = "Mortes por COVID-19 no Brasil com média móvel (7 dias)",
       # caption = "Fonte: covid.ourworldindata.org"
       ) +
  scale_y_continuous(labels = function(new_deaths) format(new_deaths, 
                                                         big.mark = ".", 
                                                         scientific = FALSE)) +
  theme_light()


# ----------------------------- análise dos dados ------------------------------

analysis_start_date <- as.Date("2020-03-01")
analysis_end_date   <- analysis_start_date + 365.25

#verificando estacionariedade
covid %>%
  filter(
    date >= analysis_start_date,
    date <= analysis_end_date
  ) %>%
  features(new_deaths, unitroot_kpss)
  # features(difference(new_deaths, differences = 1), unitroot_kpss)
  # features(new_deaths, unitroot_ndiffs)
  # features(new_deaths, unitroot_nsdiffs)

#verificando ACF e PACF dos mortes de covid
#pacotes: +feasts e gridExtra
if(TRUE){
  covidDeathsPlotACF <- covid %>%
    filter(
      date >= analysis_start_date,
      date <= analysis_end_date) %>%
    #ACF(new_deaths) %>%
    ACF(difference(new_deaths, differences = 1, lag = 7)) %>%
    autoplot() +
    scale_color_viridis_d() +
    labs(y = "ACF") +
    #ggtitle("Mortes por COVID-19 - Correlação dos dados") +
    theme_light()
  
  covidDeathsPlotPACF <- covid %>%
    filter(
      date >= analysis_start_date,
      date <= analysis_end_date) %>%
    #PACF(new_deaths) %>%
    PACF(difference(new_deaths, differences = 1, lag = 7)) %>%
    autoplot() +
    scale_color_viridis_d() +
    labs(y = "PACF") +
    #ggtitle("Mortes por COVID-19 - Correlação parcial dos dados") +
    theme_light()
  
  grid.arrange(covidDeathsPlotACF, covidDeathsPlotPACF, nrow = 1)
}

#decompondo séries de mortes diárias
covid %>%
  filter(
    date >= analysis_start_date,
    date <= analysis_end_date) %>%
  model(STL(new_deaths)) %>%
  components() %>%
  autoplot() +
  scale_color_viridis_d() +
  labs(y = "Mortes", x = "Dias") +
  ggtitle("Mortes por COVID-19 Brasil - Componentes da série temporal") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%Y %b", date_breaks = "6 month") +
  theme(title = element_text(hjust = 0.5))


# ------------------------- gerando modelos de previsão ------------------------

#separando dados para treinamento e teste (previsão)
fc_months <- 3

training_end_date <- as.Date(analysis_end_date) - fc_months * 30.4375

#gerando modelos
#pacotes: +fable
covidDeaths_fit <- covid %>%
  filter(
    date >= analysis_start_date,
    date <= training_end_date) %>%
  model(
    arima = ARIMA(new_deaths),
    arima_ACF  = ARIMA(new_deaths ~ pdq(0,1,1) + PDQ(0,1,1)),
    arima_PACF = ARIMA(new_deaths ~ pdq(1,1,0) + PDQ(0,1,1)),
  )

#visualizando os parâmetros dos modelos obtidos
covidDeaths_fit

#gerando previsões a partir dos modelos
covidDeaths_fc = covidDeaths_fit %>%
  forecast(h = paste(as.character(fc_months),"months"))

#gerando gráficos das previsões a partir dos modelos
#juntamente com os dados reais
covidDeaths_fc %>%
  autoplot(covid %>%
             filter(
               date >= analysis_start_date,
               date <= analysis_end_date)
           ) +
  labs(x = "Dias",
       y = "Mortes",
       # title = paste("Mortes por COVID-19 - Previsão para",
       #               as.character(fc_months),
       #               "meses a partir dos modelos")
       ) +
  scale_x_date(date_labels = "%Y %b") +
  scale_y_continuous(labels = function(new_deaths) format(new_deaths, 
                                                          big.mark = ".", 
                                                          scientific = FALSE)) +
  scale_color_viridis_d() +
  theme_light()


# --------------------------- analisando resultados ----------------------------

#verificando os residuos dos modelos para mortes de covid
augment(covidDeaths_fit) %>%
  #filter(.model == "arima") %>%
  autoplot(.resid) +
  scale_color_viridis_d() +
  labs(x = "Dias",
       y = "Resíduos") +
  scale_x_date(date_labels = "%Y %b") +
  scale_y_continuous(labels = function(new_cases) format(new_cases, 
                                                         big.mark = ".", 
                                                         scientific = FALSE)) +
  #ggtitle("Mortes por COVID-19 - Resíduos dos modelos") +
  theme_light()

augment(covidDeaths_fit) %>%
  #filter(.model == "arima") %>%
  ACF(.resid) %>%
  autoplot() +
  scale_color_viridis_d() +
  scale_y_continuous(labels = comma) +
  labs(y = "ACF") +
  #ggtitle("Mortes por COVID-19 - Correlação do resíduos dos modelos") +
  theme_light()

augment(covidDeaths_fit) %>%
  features(.resid, ljung_box)

#verificando acurácia do modelos com base nas previsões (MAPE)
accuracy(covidDeaths_fc, covid)[c("location",".model","MAPE")] %>%
  arrange(MAPE)

#verificando o AICc dos modelos de previsão
glance(covidDeaths_fit)[c("location",".model","log_lik","AICc")] %>% 
  arrange(AICc)
