nSINTETICA <<- 10000
nPopulacao = 50
ProbCruzamento = 0.8
ProbMutacao = 0.05
MAXCiclos = 10000
MAXDiferenca = 0.2

modeloPMIX = function (dados) {
  lags = list ( )
  lags[[1]] = c (1,0,0,0)
  lags[[2]] = c (1,0,1,0)
  lags[[3]] = c (1,1,0,0)
  lags[[4]] = c (1,1,1,0)
  lags[[5]] = c (1,1,1,1)
  lags[[6]] = c (2,0,0,0)
  lags[[7]] = c (2,0,1,0)
  lags[[8]] = c (2,1,0,0)
  lags[[9]] = c (2,1,1,0)
  #lags[[10]] = c (2,1,1,1)
  
  lapply (lags, function (x)
                NSGA (dados, x, nPopulacao, ProbCruzamento, ProbMutacao, MAXCiclos, MAXDiferenca))
}