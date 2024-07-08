# Carregar pacotes
library(rbcb)
library(dplyr)
library(lubridate)
library(tsibble)
library(fabletools)
library(fable)
library(ggplot2)

# Carregar dados
pib <- rbcb::get_series(code = c("pib" = 7326), start_date = "1980-01-01")

# Tratar dados
pib_ts <- pib |>
  dplyr::mutate(date = lubridate::year(date)) |>
  tsibble::as_tsibble(index = date)

# Separar amostras
pib_teste <- tail(pib_ts, 8)
pib_treino <- dplyr::filter(pib_ts, !date %in% pib_teste$date)

# Estimar modelo
modelo_ses <- pib_treino |>
  fabletools::model(
    ses = fable::ETS(pib ~ error("A") + trend("N") + season("N"))
  )

# Sumário do modelo
fabletools::report(modelo_ses)

# Produzir previsões
previsao_ses <- fabletools::forecast(modelo_ses, h = nrow(pib_teste))

# Acurácia de treino e teste
dplyr::bind_rows(
  fabletools::accuracy(modelo_ses),
  fabletools::accuracy(previsao_ses, pib_teste)
)

# Gráfico de valores observados e estimados/previstos
previsao_ses |>
  fabletools::autoplot(pib_ts) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(y = .fitted),
    color = "#b22200",
    data = fabletools::augment(modelo_ses)
  ) +
  ggplot2::labs(
    title = "Brasil: Produto Interno Bruto (método de previsão SES)",
    subtitle = "Taxa de variação real",
    x = NULL,
    y = NULL,
    caption = "Dados: BCB | Elaboração: Matheus"
  )
