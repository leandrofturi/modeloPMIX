MESES = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

nPOPULACAO = 50

graficoSeries = function (serieH, p) {
  series = leituraArquivos ( )
  inicializaGrafico (serieH)
  cores = rainbow (nPOPULACAO)
  lapply (p, function (x)
             graficoSerie (series[[x]], cores[x]))
  graficoSerie (serieH, 'black')
}

leituraArquivos = function ( ) {
  p = 1:nPOPULACAO
  nomes = sapply (p, function (x)
                     paste0 ("serie_", x, ".csv"))
  series = lapply (p, function (x)
                      read.csv (nomes[x], header = TRUE, sep = ";", dec = ","))
  series = lapply (p, function (x)
                      as.matrix (series[[x]][-1]))
  return (series)
}

inicializaGrafico = function (serieH) {
  media = apply (serieH, 2, mean)
  desvio = apply (serieH, 2, sd)
  par (lwd = 1, col= 'black')
  plot (NA, main = "Medias e Desvios", xlim = c (1,12), ylim = c (0, 2*(max (media + desvio))), xlab = "", ylab = "", axes = F, type = "n")
  axis (1, 1:12, MESES)
  box ()
}

graficoSerie = function (serie, cor) {
  media = apply (serie, 2, mean)
  desvio = apply (serie, 2, sd)
  par (lty = 1)
  par (lwd = 2, lty = 1, col= cor)
  lines (1:12, media)
  par (lty = 2)
  lines (1:12, media + desvio)
  lines (1:12, media - desvio)
}