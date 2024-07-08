# Carregar pacotes
library(rbcb)
library(dplyr)
library(lubridate)
library(tsibble)
library(fabletools)
library(fable)
library(ggplot2)

# Carregar dados
energia <- rbcb::get_series(code = c("consumo" = 1403), start_date = "2002-01-01")

# Tratar dados
energia_ts <- energia |>
  dplyr::mutate(date = tsibble::yearmonth(date)) |>
  tsibble::as_tsibble(index = date)

# Separar amostras
energia_teste <- tail(energia_ts, 24)
energia_treino <- dplyr::filter(energia_ts, !date %in% energia_teste$date)

# Estimar modelo
modelo_sazonal <- energia_treino |>
  fabletools::model(
    aditiva = fable::ETS(consumo ~ error("A") + trend("A") + season("A")),
    multiplicativa = fable::ETS(consumo ~ error("M") + trend("A") + season("M"))
  )

# Sumário do modelo
modelo_sazonal |> dplyr::select(aditiva) |> fabletools::report()
modelo_sazonal |> dplyr::select(multiplicativa) |> fabletools::report()

# Produzir previsões
previsao_sazonal <- fabletools::forecast(modelo_sazonal, h = nrow(energia_teste))

# Acurácia de treino e teste
dplyr::bind_rows(
  fabletools::accuracy(modelo_sazonal),
  fabletools::accuracy(previsao_sazonal, energia_teste)
)

# Gráfico de valores observados e estimados/previstos
previsao_sazonal |>
  fabletools::autoplot(energia_ts, size = 1) +
  ggplot2::labs(
    title = "Brasil: Consumo de energia elétrica - Residencial",
    subtitle = "Em Gwh (método de previsão Holt e Winters)",
    x = NULL,
    y = NULL,
    caption = "Dados: BCB | Elaboração: Matheus"
  )
