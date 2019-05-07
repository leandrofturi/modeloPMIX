source ('algoritmo/entrada.R')
source ('modelo/correlograma.R')
source ('modelo/sumQuadRes.R')


iteracoesMAX <<- 500
EPS <<- 10^-5

powell = function (serie, lags, P0) {
  ordem = sum (lags)
  passo = matrix (0, ncol = (ordem*12), nrow = (ordem*12))
  for (i in 1 : (ordem*12)) passo[i, i] = 1
  
  E0 = residuos (serie, P0, lags)$somRes
  Pi = P0
  Ei = E0
  
  for (ciclos in (1 : iteracoesMAX)) {
    Pdec = 1
    delta = 0
    for (i in (1 : (ordem*12))) {
      Eiant = Ei
      busca = tamanhoPasso (serie, lags, Pi, passo[, i])
      Pi = busca$ponto
      Ei = busca$somRes
      
      if (Eiant - Ei > delta) {
        delta = Eiant - Ei
        Pdec = i
      }
    }
    
    if (E0 - Ei < EPS) {
      final = list (parametros = Pi, somRes = Ei, ciclos = ciclos)
      return (final)
    }
    
    Pe = 2*Pi - P0
    Ee = residuos(serie, Pe, lags)$somRes
    if (Ee < E0) {
      if (2*(E0 - 2*Ei + Ee)*((E0 - Ei - delta)^2) < ((E0 - Ee)^2)*delta) {
        S = Pi - P0
        busca = tamanhoPasso (serie, lags, S, Pi)
        Ps = busca$ponto
        Es = busca$somRes
        if (Pdec < ordem*12)
          passo[, (Pdec:((ordem*12)-1))] = passo[, ((Pdec+1):(ordem*12))]
        
        passo[, (ordem*12)] = S
        Pi = Ps
        Ei = Es
      }
    }
    P0 = Pi
    E0 = Ei
  }

  final = list (parametros = Pi, somRes = Ei, ciclos = ciclos)
  return (final)
}

tamanhoPasso = function (serie, lags, ponto, passo) {
  passoMin = -1
  passoMax = 1
  otimizacao = optimize (function (x) residuos (serie, (ponto + x*passo), lags)$somRes,
                         c (passoMin, passoMax), maximum = FALSE, tol = EPS)
  
  final = list (ponto = ponto + otimizacao$minimum*passo, somRes = otimizacao$objective)
  return (final)
}

PMIXs = function (dados, lags, n, ordem) {
  serie = entrada(dados)$serieHN
  
  p = 1:n
  Pinicial = lapply (p, function (x)
                        geraPinicial (lags))
  
  saidas = parLapply (cl, Pinicial, function (x)
                                    powell (serie, lags, x))
  
  return (saidas)
}

geraPinicial = function (lags) {
  phi = numeric (0)
  tht = numeric (0)
  PHI = numeric (0)
  THT = numeric (0)
  if (lags[1] > 0)
    phi = (runif (12*lags[1], 0.5, 1.5)) * (sapply ((round (runif (12*lags[1], -1, 0))), function (x) if (x == 0) x = 1 else x = -1))
  if (lags[2] > 0)
    tht = runif (12*lags[2], -0.5, 0.5)
  if (lags[3] > 0)
    PHI = (runif (12*lags[3], 0.5, 1.5)) * (sapply ((round (runif (12*lags[3], -1, 0))), function (x) if (x == 0) x = 1 else x = -1))
  if (lags[4] > 0)
    THT = runif (12*lags[4], -0.5, 0.5)
  
  Pinicial = c (phi, tht, PHI, THT)
  
  return (Pinicial)
}

PMIX = function (dados, lags) {
  inicio = Sys.time ( )
  
  entrada = entrada(dados)
  serie = entrada$serieHN
  Pinicial = c (rep (1, 12*lags[1]), rep (0, 12*lags[2]), rep (1, 12*lags[3]), rep (0, 12*lags[4]))
  
  saida = powell (serie, lags, Pinicial)

  paramPowell = c (saida$ciclos, saida$somRes, saida$parametros)
  paramPowell = t (paramPowell)
  
  fim = Sys.time ( )
  duracao = as.numeric (difftime (fim, inicio))
  
  dpRes = residuos (serie, saida$parametros, lags)$dpRes
  serieS = serieSint (saida$parametros, dpRes, lags, (nSINTETICA + 50))
  serieS = serieS[-(1:50), ]
  serieS = t ((t (serieS) * entrada$dpHL) + entrada$mediaHL)
  serieS = exp (serieS)
  
  arqPowell = data.frame (paramPowell)
  colnames (arqPowell) = c ("ciclos", "somRes", rep (c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"), sum (lags)))
  rownames (arqPowell) = c ("1")
  
  final = list (arqSeries = serieS, arqAvaliacoes = arqPowell, duracao = duracao)
  return (final)
}