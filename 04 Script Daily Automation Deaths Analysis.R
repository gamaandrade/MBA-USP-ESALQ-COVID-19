# -------------------------- carregamento dos pacotes --------------------------
pacotes <- c(
  "tidyverse",
  "tsibble",
  "lubridate",
  "feasts",
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
#         novas mortes no dia

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
    new_deaths  = replace_na(new_deaths,  0)
  )


# ----------------------------- análise dos dados ------------------------------

if(TRUE){
  #criando dataframe para armazenar resultados
  columns = c(
    "analysis_start",
    "training_end",
    "analysis_end",
    "model",
    "p_value",
    "aicc",
    "mape"
  )
  
  results = data.frame(matrix(nrow = 0, ncol = length(columns))) 
  colnames(results) = columns
  
  #estabelecendo início do primeiro periodo de análise
  analysis_start_date <- as.Date("2020-03-01")
  
  #intervalo de dados para treinamento e teste (previsão em meses)
  fc_months <- 3
  
  #realizando análise para 12 meses
  for (i in 1:18) {
  
    analysis_end_date <- analysis_start_date + 365.25
    training_end_date <- as.Date(analysis_end_date) - fc_months * 30.4375
    
    #gerando modelo
    #pacotes: +fable
    covidDeaths_fit <- covid %>%
      filter(
        date >= analysis_start_date,
        date <= training_end_date) %>%
      model(
        arima = ARIMA(new_deaths),
      )
    
    #gerando previsões a partir do modelo
    covidDeaths_fc = covidDeaths_fit %>%
      forecast(h = paste(as.character(fc_months),"months"))
    
    #obtendo os parâmetros do modelo obtido
    model <- as.character(covidDeaths_fit$arima)
    
    #obtendo residuos do modelo
    lb_pvalue <- augment(covidDeaths_fit) %>%
      features(.resid, ljung_box)
    lb_pvalue <- lb_pvalue$lb_pvalue
    
    #obtendo o AICc do modelo de previsão
    aicc <- glance(covidDeaths_fit)
    aicc <- aicc$AICc
    
    #obtendo acurácia do modelo com base nas previsões (MAPE)
    mape <- accuracy(covidDeaths_fc, covid)
    mape <- mape$MAPE
    
    #salvando dados obtidos
    results[nrow(results) + 1,] <- c(
                                      as.character(analysis_start_date),
                                      as.character(training_end_date),
                                      as.character(analysis_end_date),
                                      model,
                                      format(lb_pvalue, digits = 3),
                                      format(aicc, nsmall = 3),
                                      format(mape, digits = 5)
    )
    
    #seguindo para o próximo intervalo
    analysis_start_date <- analysis_start_date + 30.4375
  }
}

# -------------------------- visualizando resultados ---------------------------

results %>%
  arrange(desc(p_value))

results %>%
  arrange(aicc)


# --------------------------- exportando resultados ----------------------------

write.csv(results, file = "Covid deaths - Daily analysis results.csv")
