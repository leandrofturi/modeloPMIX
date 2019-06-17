source ("entrada.R")
source ("correlograma.R")

facH = function ( ) {
  fileSerieH = file.choose ( )
  serieH = entrada (fileSerieH)$serieH
  escreveFacMensal (serieH, "FACMensalHistorica.csv")
  escreveFacAnual (serieH, "FACAnualHistorica.csv")
}

facS = function ( ) {
  fileSerieS = file.choose ( )
  serie = read.csv2 (fileSerieS, header = T, sep = ";", dec = ",")
  serie = serie[, -1]
  serie = as.matrix (serie)
  escreveFacMensal (serie, "FACMensalSintetica.csv")
  escreveFacAnual (serie, "FACAnualSintetica.csv")
}

leitura = function (fileSeries) {
  series = lapply (fileSeries, function (x)
                               read.csv2 (x, header = T, sep = ";", dec = ","))
  series = lapply (series, function (x)
                           x[, -1])
  series = lapply (series, as.matrix)
  return (series)
}

escreveFacMensal = function (serie, nomeArq) {
  fac = autocorrelacaoMensal (serie, 12)
  arquivo = data.frame (fac)
  colnames (arquivo) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
  rownames (arquivo) = paste0 ("lag", (0:12))
  write.csv2 (arquivo, nomeArq)
}

escreveFacAnual = function (serie, nomeArq) {
  serieAnual = apply (serie, 1, sum)
  fac = autocorrelacaoAnual (serieAnual, 12)
  arquivo = data.frame (fac)
  rownames (arquivo) = paste0 ("lag", (0:12))
  write.csv2 (arquivo, nomeArq)
}