# Carregar pacotes
library(rbcb)
library(dplyr)
library(lubridate)
library(tsibble)
library(fabletools)
library(fable)
library(ggplot2)

# Carregar dados
pop <- rbcb::get_series(code = c("pop" = 21774))

# Tratar dados
pop_ts <- pop |>
  dplyr::mutate(date = lubridate::year(date), pop = pop / 1000) |>
  tsibble::as_tsibble(index = date)

# Separar amostras
pop_teste <- tail(pop_ts, 8)
pop_treino <- dplyr::filter(pop_ts, !date %in% pop_teste$date)

# Estimar modelo
modelo_holt <- pop_treino |>
  fabletools::model(
    holt = fable::ETS(pop ~ error("A") + trend("A") + season("N"))
  )

# Sumário do modelo
fabletools::report(modelo_holt)

# Produzir previsões
previsao_holt <- fabletools::forecast(modelo_holt, h = nrow(pop_teste))

# Acurácia de treino e teste
dplyr::bind_rows(
  fabletools::accuracy(modelo_holt),
  fabletools::accuracy(previsao_holt, pop_teste)
)

# Gráfico de valores observados e estimados/previstos
previsao_holt |>
  fabletools::autoplot(pop_ts) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(y = .fitted),
    color = "#b22200",
    data = fabletools::augment(modelo_holt)
  ) +
  ggplot2::labs(
    title = "Brasil: População (método de previsão Holt)",
    subtitle = "Em milhões",
    x = NULL,
    y = NULL,
    caption = "Dados: IBGE | Elaboração: Matheus"
  )
