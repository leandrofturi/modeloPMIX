source ('entrada.R')
source ('correlograma.R')
source ('sumQuadRes.R')


iteracoesMAX <<- 500
#EPS <<- .Machine$double.eps^0.5
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
    
    #CRITERIO DE PARADA
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

PMIXs = function (dados, lags, n) {
  serie = entrada(dados)$serieHN
  print ("obtendo parametros pelo metodo de Powell")
  p = 1:n
  Pinicial = list ()
  Pinicial = lapply (p, function (x)
                        geraPinicial ( ))
  saidas = lapply (Pinicial, function (x)
                             powell (serie, lags, x))
  
  parametros = sapply (saidas, function (x) x$parametros)
  somRes = sapply (saidas, function (x) x$somRes)
  ciclos = sapply (saidas, function (x) x$ciclos)
  paramPowell = sapply (p, function (x) c (ciclos[x], somRes[x], parametros[, x]))
  paramPowell = t (paramPowell)
  
  arqPowell = data.frame (paramPowell)
  rownames (arqPowell) = p
  colnames (arqPowell) = c ("ciclos", "somRes", rep (c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"), sum (lags)))
  write.csv2 (arqPowell, "parametrosIniciais.csv")
  
  return (saidas)
}

geraPinicial = function ( ) {
  phi = (runif (12*lags[1], 0.5, 1.5)) * (sapply ((round (runif (12*lags[1], -1, 0))), function (x) if (x == 0) x = 1 else x = -1))
  tht = runif (12*lags[2], -0.5, 0.5)
  PHI = (runif (12*lags[3], 0.5, 1.5)) * (sapply ((round (runif (12*lags[3], -1, 0))), function (x) if (x == 0) x = 1 else x = -1))
  THT = runif (12*lags[4], -0.5, 0.5)
  Pinicial = c (phi, tht, PHI, THT)
  
  return (Pinicial)
}