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

# Estimar modelo
modelos <- pop_ts |>
  fabletools::model(
    holt = fable::ETS(pop ~ error("A") + trend("A") + season("N")),
    amortecido = fable::ETS(pop ~ error("A") + trend("Ad") + season("N"))
  )

# Sumário do modelo
modelos |> dplyr::select(amortecido) |> fabletools::report()

# Produzir previsões
previsoes <- fabletools::forecast(modelos, h = 30)

# Gráfico de valores observados e estimados/previstos
previsoes |>
  fabletools::autoplot(pop_ts, level = NULL) +
  ggplot2::labs(
    title = "Brasil: População",
    subtitle = "Em milhões (método de previsão Holt e Holt Amortecido)",
    x = NULL,
    y = NULL,
    caption = "Dados: IBGE | Elaboração: Matheus"
  )
