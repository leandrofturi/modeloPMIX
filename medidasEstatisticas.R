Hurst = function (serie) {
  serie = as.vector (t (serie))
  
  n = length (serie)
  media = mean (serie)
  serieNormalizada = (serie - media)^2

  R = max (serieNormalizada) - min (serieNormalizada)
  S =  sd (serie)
  Hurst = log (R/S) / (log((n)/2))
  
  return (Hurst)
}