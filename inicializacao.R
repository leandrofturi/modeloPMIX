source('sumQuadRes.R')
source('entrada.R')

PRECISAO = 11 + 1
TAM_POPULACAO = 60
TAM_INDIVIDUO = 0

inicializaPop = function(tamIndividuo) {
  populacao = matrix (round (runif (tamIndividuo * TAM_POPULACAO, 0,1)), nrow = TAM_POPULACAO)
  
  return (populacao)
}

binParaDec = function (b) {
  sinal = b[1]
  b = b[-1]
  d = sum (b* (2 ^ seq (length(b)-1, 0)))
  
  if (sinal == 1) d = (-1)*d
  
  return (d)
}

cortaParametros = function (individuo) {
  
  matrizParam = matrix (individuo, ncol = PRECISAO, byrow = T)
  parametros = apply (matrizParam, 1, function(x) binParaDec(x))
  parametros = parametros / 1000
  return (parametros)
}

