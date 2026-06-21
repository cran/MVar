CI <- function(data, sigma = NA, conf = 0.95, type = "T") {
  # Esta funcao encontra os typeos de confianca uni e multivariado
  # desenvolvida por Paulo Cesar Ossani em 20/06/2026
  
  # Entrada:
  # data  - Dados dados com as variaveis para encontrar o CI
  # sigma - matriz de variancia e covariancia, caso contrario o CI sera pela variancia amostral (default sigma = NA)
  # conf  - Nivel de confianca do CI (default conf = 95%)
  # type  - "T" = T^2 de Hotelling e "B" = Bonferroni
  
  # Retorna:
  # cim - Intervalo de confianca multivariado com "sign" de signifciancia
  # ciu - Intervalo de confianca univariado com "sign" de signifciancia
  
  if (!is.data.frame(data) && !is.matrix(data))
     stop("Input 'data' is incorrect; it must be of type data frame or matrix. Please check!")
  
  type = toupper(type) # torna minusculo
  if (!(type %in% c("T", "B")))
     stop("Input for 'type' is incorrect; it must be 'T' or 'B'. Please check!")
  
  if (all(!is.na(sigma))) {
    if (!is.matrix(sigma) && (nrow(sigma) != ncol(sigma)) && !isSymmetric(as.matrix(sigma))) 
       stop("Input for 'sigma' is incorrect; it must be a variance-covariance matrix. Please check!")
    
    if (any(eigen(sigma, only.values = TRUE)$values < - 1e-8)) 
       stop("Input for 'sigma' is incorrect; it must be a variance-covariance matrix. Please check!")
  }
  
  if (conf <= 0 || conf > 1) 
     stop("Input for 'conf' is incorrect; it must be a value between 0 and 1. Please check!")
  
  data <- as.matrix(data[, sapply(data, is.numeric), drop = FALSE]) # seleciona apenas as colunas numercias
  
  sign <- 1 - conf # nivel de signifciancia
  n <- nrow(data) # numero de observacoes 
  m <- ncol(data) # numero de variaveis
  mi.amo <- colMeans(data) # vetor media amostral

  ll <- diag(rep(1,ncol(data))) # vetores l's
  p  <- ncol(data) # grau de liberdade

  CIm <- matrix(NA, ncol = 3, nrow = ncol(data)) # matriz com os CI's multivariado
  colnames(CIm) <- c("Means", "Lower Limit", "Upper Limit")
  rownames(CIm) <- colnames(data)
  CIm[,1] <- mi.amo
  
  CIu <- CIm # matriz com os CI's uniivariado
  
  if (!all(is.na(sigma))) { 
     
     text <- "- Known Variance"
    
     ### Inciio - Variancia Conhecida Inivariada ###
     for(i in 1:ncol(data)) {
       erro.media <- sqrt(sigma[i,i] / n) 
       z <- qnorm(1 - sign / 2) 
       CIu[i,2] <- CIu[i,1] - z * erro.media 
       CIu[i,3] <- CIu[i,1] + z * erro.media 
     }
     ### Fim - Variancia Conhecida Inivariada ###
    
     ### Inciio - Variancia Conhecida Multivariada ###
     if (type == "T") { # T^2 Hotelling
        QQ = qchisq(1 - sign, df = p)
        for(i in 1:ncol(data)) {
          erro.media <- sqrt(QQ) * sqrt((t(ll[,i]) %*% sigma %*% ll[,i]) / n)
          CIm[i,2] <- t(ll[,i]) %*% mi.amo - erro.media
          CIm[i,3] <- t(ll[,i]) %*% mi.amo + erro.media
        }
     }
    
     if (type == "B") { # Bonferroni
        for(i in 1:ncol(data)) {
          erro.media <- qnorm(1 - sign / (2 * m)) * sqrt((t(ll[,i]) %*% sigma %*% ll[,i]) / n)
          CIm[i,2] <- t(ll[,i]) %*% mi.amo - erro.media
          CIm[i,3] <- t(ll[,i]) %*% mi.amo + erro.media
        }
     } 
     ### Fim - Variancia Conhecida Multivariada ###
    
  } else { 
    
     text <- "- Unknown Variance"
     
     sigma <- cov(data)
     
     ### Inciio - Variancia Desconhecida Univariada ###
     for(i in 1:ncol(data)) {
       erro.media <- sqrt(sigma[i,i] / n) 
       tcrit <- qt(1 - sign / 2, df = n - 1) 
       CIu[i,2] <- CIu[i,1] - tcrit * erro.media 
       CIu[i,3] <- CIu[i,1] + tcrit * erro.media 
     }
     ### Fim - Variancia Desconhecida Univariada ###
     
     ### Inciio - Variancia Desconhecida Multivariada ###
     v <- nrow(data) - 1 # grau de liberdade
     
     if (type == "T") { # T^2 Hotelling
         TB = v * p / (v - p + 1) * qf(1 - sign, df1 = p, df2 = v - p + 1, ncp = 0)
         for(i in 1:ncol(data)) {
           erro.media <- sqrt(TB) * sqrt((t(ll[,i]) %*% sigma %*% ll[,i]) / n)
           CIm[i,2] <- t(ll[,i]) %*% mi.amo - erro.media
           CIm[i,3] <- t(ll[,i]) %*% mi.amo + erro.media
        }
      }
    
     if (type == "B") { # Bonferroni
        for(i in 1:ncol(data)) {
          erro.media <- qt(1 - sign / (2 * m), df = v ) * sqrt((t(ll[,i]) %*% sigma %*% ll[,i]) / n)
          CIm[i,2] <- t(ll[,i]) %*% mi.amo - erro.media
          CIm[i,3] <- t(ll[,i]) %*% mi.amo + erro.media
        }
     }
     ### Fim - Variancia Desconhecida Multivariada ###
  }
  
  resu <- list(title = paste("Univariate CI", text), CIu = CIu)  
  resm <- list(title = paste("Multivariate CI", text), CIm = CIm)
  
  lista <- list(cim = resm, ciu = resu)
  
  return(lista)
}
