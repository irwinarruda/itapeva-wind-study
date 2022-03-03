library(openair)

setwd("C:/Users/arrud/Desktop/Projects/PRO/itapeva-wind-study")

data <- read.csv(
  file = "vento_itapeva_tres_anos.csv",
  header = T,
  sep = ";",
  dec = ",",
#fileEncoding = "ISO-8859-1"
)

createWindRose <- function(df) {
  windRose(df, ws = "ws", wd = "wd",
    angle = 30,
    breaks = c(0, 3, 5, 8, 11, 17),
    auto.text = FALSE,
    paddle = FALSE,
    annotate = FALSE,
    grid.line = 5,
    key = list(
     labels = c(
       ">0 - 3",
       ">3 - 5",
       ">5 - 8",
       ">8 - 11",
       ">11 - 17"
     )
    ),
    key.footer = "Velocidade do vento (m/s)",
    key.position = "bottom",
    par.settings = list(axis.line = list(col = "lightgray")),
    col = c("#4f4f4f", "#0a7cb9", "#f9be00", "#ff7f2f", "#d7153a")
  )
}

filterDfByWd <- function(start, end) {
  return(data[data[['wd']] > start & data[['wd']] <= end,])
}

getWsByRange <- function(range = c(0, 3, 5, 8, 11, 17), df = data) {
  occurrencesList <- c()
  for (i in 1:(length(range) - 1)) {
    occurrences <- nrow(df[df[['ws']] >= range[i] & df[['ws']] < range[i + 1],])
    occurrencesList <- c(occurrencesList, occurrences)
  }
  return(occurrencesList)
}

# Anemograma do local
createWindRose(data)

Nr <- filterDfByWd(0, 15)
Nl <- filterDfByWd(345, 360)

N <- rbind(Nr, Nl)
NNE <- filterDfByWd(15, 45)
ESE <- filterDfByWd(105, 135)
SSE <- filterDfByWd(135, 165)
S <- filterDfByWd(165, 195)
SSO <- filterDfByWd(195, 225)
ONO <- filterDfByWd(285, 315)
NNO <- filterDfByWd(315, 345)

# Filtrando apenas os dados de ventos laterais à pista
outsideFieldRangeDf <- rbind(N, NNE, ESE, SSE, S, SSO, ONO, NNO)
outsideFieldRangeDf

# Anemograma apenas dos ventos laterais
createWindRose(outsideFieldRangeDf)

# Ocorrência de ventos total
allWs <- getWsByRange()
allOccurrences <- allWs[1] + allWs[2] + allWs[3] + allWs[4] + allWs[5]

# Ocorrência de ventos relevantes nas laterais da pista
outsideFieldWs <- getWsByRange(df = outsideFieldRangeDf)
outsideFieldOccurrencesFrom2to17 <- outsideFieldWs[2] + outsideFieldWs[3] + outsideFieldWs[4] + outsideFieldWs[5]

# Relação entre ventos laterais e ventos totais
percentage <- outsideFieldOccurrencesFrom2to17 / allOccurrences
percentage

# Condição que verifica se a pista está sendo feita em um ângulo favorável aos ventos.
isValidField <- if (percentage <= 5) "Yes!" else "NO"

  print(paste("Is the chosend field valid? ", isValidField))
