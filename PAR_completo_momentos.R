entrada = function(serie)                                                           # TRATAMENTO DOS DADOS DE ENTRADA.
{
  leitura = read.table(serie, header = T, sep = ";", dec = ",")
  leitura = leitura[, -1]
  serieHist = matrix(leitura, ncol = 12, byrow = T)
  mediaHist = apply(serieHist, 2, mean)                                             # CALCULO DOS MOMENTOS DA SERIE HISTORICA.
  sdHist = apply(serieHist, 2, sd)
  varHist = apply(serieHist, 2, var)
  
  serieHistN = log(serieHist)                                                       # NORMALIZACAO.                  
  
  mediaMensal = apply(serieHistN, 2, mean)
  sdMensal = apply(serieHistN, 2, sd)
  
  serieHistN = t((t(serieHistN) - mediaMensal) / sdMensal)                          # PADRONIZACAO.
  
  final = list(serieHist = serieHist, mediaHist = mediaHist, sdHist = sdHist, varHist = varHist,
               serieNormal = serieHistN, mediaNormal = mediaMensal, sdNormal = sdMensal)
  return(final)
}


autocorrelacao = function(serie){                                                    # FUNCAO DE AUTOCORRELACAO (FAC)
  mediaMensal = apply(serie, 2, mean)
  sdMensal = apply(serie, 2, sd)
  serie = t((t(serie) - mediaMensal))
  serieV = as.vector(t(serie))
  n = length(serie)/12
  fac = matrix(1, nrow = 13, ncol = 12)
  
  for(lag in 1:12){
    serieLag = serieV[-(1:lag)]                                                      # VETOR CONTENDO A SERIE COM UM LAG.
    serieAux = serieV[1:((n*12)-lag)]                                                # RETIRADA DOS ULTIMOS VALORES QUE NAO SERAO CORRELACIONADOS.
    product = c(rep(0, lag), serieLag * serieAux)                                    # (Yt - media mensal) * (Yt[LAG] - media mensal[LAG]).                             
    serie = matrix(product, ncol = 12, byrow = T)
    
    for(mes in 1:12){
      meslag = (mes - lag - 1) %% 12 + 1
      fac[(lag+1), mes] = sum(serie[, mes]) / ((n-1) * sdMensal[mes] * sdMensal[meslag])
    }
  }
  return(fac)
}


paramPhi = function(serieHist, p)                                                   # CALCULO DA AUTOCORRELACAO E PARAMETROS DO MODELO.
{
  fac = autocorrelacao(serieHist)
  fac = fac[(1:(p+1)), ]
  serie = as.vector(t(serieHist))                                                   # VETOR CONTENDO TODA A SERIE ORIGINAL.
  n = length(serieHist)/12
  
  tablePhi = matrix(numeric(1), nrow = p, ncol = 12)
  varEpsilon = rep(1, 12)
  
  for(mes in 1:12){                                                                 # CALCULO DOS PARAMETROS UTILIZANDO METODO DOS MOMENTOS.
    Pk = matrix(numeric(1), nrow = p, ncol = p)
    rok = rep(1, p)
    I = matrix(numeric(1), nrow = p, ncol = p)
    
    for(j in 1:p){
      meslag = (mes - j - 1) %% 12 + 1
      for(i in j:p){
        k = abs(j - i) + 1
        Pk[i, j] = fac[k, meslag]                                                   # PARTE TRIANGULAR INFERIOR DA MATRIZ DE AUTOCORRELACAO.
        
        if(i == j){
          I[i, j] = 1
        }
      }
    }
    Pk = Pk + t(Pk) - I                                                             # MATRIZ DE AUTOCORRELACAO SAZONAL.
    rok = fac[-1, mes]
    tablePhi[, mes] = solve(Pk, rok)                                                # SOLUCAO DAS EQUACOES DE YULE-WALKER.
    
    varEpsilon[mes] = 1 - sum(tablePhi[, mes] * fac[-1, mes])                       # VARIANCIA DO EPSILON.
  }
  
  final = list(phi = tablePhi, varEpsilon = varEpsilon)
  return(final)
}


serieSint = function(serie, p, n)                                                   # GERACAO DA SERIE SINTETICA.
{
  entrada = entrada(serie)
  serieNormal = entrada$serieNormal
  mediaNormal = entrada$mediaNormal
  sdNormal = entrada$sdNormal
  nHist = length(serieNormal)/12
  paramPhi = paramPhi(serieNormal, p)
  phi = paramPhi$phi
  varEpsilon = paramPhi$varEpsilon
  sdEpsilon = sqrt(varEpsilon)
  
  nTotal = (nHist + n)*12                                                           # AQUECENDO O MODELO COM 50+N ITERACOES.
  serieSint = numeric(nTotal)
  
  VAleat = matrix(rnorm(nTotal), ncol = 12)
  meanAleat = apply(VAleat, 2, mean)                                                # FORCAR QUE O EPSILON TENHA DISTRIBUICAO NORMAL.
  sdAleat = apply(VAleat, 2, sd)
  VAleat = t((t(VAleat) - meanAleat) / sdAleat)
  VAleat = t(t(VAleat) * sdEpsilon)
  epsilon = as.vector(t(VAleat))
  
  for(t in (p+1):nTotal){
    mes = t %% 12
          if(mes == 0) mes = 12
    
    for(lag in 1:p){
      serieSint[t] = phi[lag, mes] * serieSint[t-lag] + serieSint[t]                # GERACAO DA SERIE SINTETICA.
    }
    serieSint[t] = serieSint[t] + epsilon[t]                                        # SERIE FINAL.
  }
  serieSint = serieSint[-(1:(nHist*12))]                                            # RETIRADA DOS TERMOS DO AQUECIMENTO.
  
  serieFinal = matrix(serieSint, ncol = 12, byrow = T)
  serieFinal = t((t(serieFinal) * sdNormal) + mediaNormal)                          # DESPADRONIZACAO.
  serieFinal = exp(serieFinal)                                                      # DESNORMALIZACAO.
  
  mediaFinal = apply(serieFinal, 2, mean)                                           # MOMENTOS DA SERIE FINAL.
  sdFinal = apply(serieFinal, 2, sd)
  
  final = list(Phi = phi, VarEpsilon = varEpsilon, serieSintetica = serieFinal, mediaMensal = mediaFinal, sdMensal =  sdFinal)
  return(final)
}


PAR = function(serie, p, n, seriePlot){
  serieHist = entrada(serie)
  serieH = serieHist$serieHist
  mediaH = serieHist$mediaHist
  sdH = serieHist$sdHist
  facH = autocorrelacao(serieH)
  intervalop = 1.96 / sqrt(length(serieH)/12)
  
  serieSint = serieSint(serie, p, n)
  serieS = serieSint$serieSintetica
  phi = serieSint$Phi
  varEpsilon = serieSint$VarEpsilon
  mediaS = serieSint$mediaMensal
  sdS = serieSint$sdMensal
  facS = autocorrelacao(serieS)
  meses = c("JANEIRO", "FEVEREIRO", "MARCO", "ABRIL", "MAIO", "JUNHO", "JULHO", "AGOSTO", "SETEMBRO", "OUTUBRO", "NOVEMBRO", "DEZEMBRO")
  facNome = paste("FAC", (0:12))
  phiNome = paste("PHI", (1:p))
  
  data = format(Sys.time(), "%F %Hh%M")                                             # OS DADOS SERAO SALVOS NO DIRETORIO DE USO DO R
  data = paste0("PAR(", p, ") ", data)
  dir.create(file.path("./", data))
  
  tabelaDadosH = data.frame(facH)                                                # PARAMETROS DOS DADOS HISTÓRICOS PUROS
  rownames(tabelaDadosH) = c(facNome)
  colnames(tabelaDadosH) = meses
  write.csv2(tabelaDadosH, paste0(data, "/Autocorrelacao Historica.csv"))
  
  tabelaDadosS = data.frame(facS)                                                # PARAMETROS DOS DADOS SINTETICOS
  rownames(tabelaDadosS) = c(facNome)
  colnames(tabelaDadosS) = meses
  write.csv2(tabelaDadosS, paste0(data, "/Autocorrelacao Sintetica.csv"))
  
  tabelaParametros = data.frame(mediaH, sdH, t(phi), varEpsilon, mediaS, sdS)
  rownames(tabelaParametros) = meses
  colnames(tabelaParametros) = c("MEDIA HIST", "SD HIST", phiNome, "VAR EPSILON", "MEDIA SINT", "SD SINT")
  write.csv2(tabelaParametros, paste0(data, "/Parametros.csv"))
  
  tabelaserieS = data.frame(serieS)                                                 # SERIE SINTETICA
  rownames(tabelaserieS) = c(1:n)
  colnames(tabelaserieS) = meses
  write.csv2(tabelaserieS, paste0(data, "/Serie Sintetica.csv"))
  
  if (seriePlot){
    par(lwd = 0.5, col = 'grey')
    plot(main = "Serie Sintética", mediaS, col= 'red', xlim = c(1,12), ylim = c(0, max(serieS)),
         xlab = "meses", ylab = "valores gerados", type = "n")
    for(ano in 1:n){
      points(1:12, serieS[ano, ])
    }
    par(col = 'black')
    legend("topright", c("Dados Históricos", "Dados Sintéticos"), bty = "n", col = c('Blue', 'Red'), pch = 19, cex=0.8)
    par(lwd = 2)
    lines(1:12, mediaH, col= 'blue')
    lines(1:12, mediaS, col= 'red')
    par(lwd = 0.5)
    lines(1:12, mediaH + sdH, col= 'blue', lty = 2)
    lines(1:12, mediaH - sdH, col= 'blue', lty = 2)
    lines(1:12, mediaS + sdS, col= 'red', lty = 2)
    lines(1:12, mediaS - sdS, col= 'red', lty = 2)
    
    plot(1, type = "n", xlab = "period", ylab = "", xlim = c(1,13), ylim = c(0, 12))
    for(mes in 1:12) {
      segments(mes, 0, mes, 12)
      segments(mes + intervalop, 0, mes + intervalop, 12, lty = 2)
      for(lag in 0:12) {
        segments(mes, lag, (mes + facS[(lag+1), mes]), lag)
      }
    }
  }
  
  final = list(serieHistorica = serieH,
               mediaHistorica = mediaH,
               sdHistorico = sdH,
               facHistorico = facH,
               serieSintetica = serieS,
               mediaSintetica = mediaS,
               sdSintetico = sdS,
               facSintetico = facS,
               intervaloConfianca = intervalop)
  
  return(final)
}
