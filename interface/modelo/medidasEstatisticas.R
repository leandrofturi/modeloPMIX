Hurst = function (serie) {
  n = length (serie)
  media = mean (serie)
  serieNormalizada = serie - media
  
  somaAcumulada = sapply (1:n, function (x)
                               sum (serieNormalizada[1:x]))

  R = max (somaAcumulada) - min (somaAcumulada)
  S =  sd (serie)
  Hurst = log (R/S) / (log((n)/2))
  
  return (Hurst)
}