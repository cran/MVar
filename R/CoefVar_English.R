CoefVar <- function(data, type = 1) {
  # Encontra o Coeficiente de Variacao dos dados, 
  # funcao desenvolvida por Paulo Cesar Ossani em 05/2016
  
  # data - Dados a serem analizados
  # type - 1 Coefiente de variacao global (default)
  #        2 Coefiente de variacao por coluna
  
  # Retorna:
  # CVar - Coeficiente de variacao
  
  if (!is.data.frame(data) && !is.matrix(data)) 
     stop("'data' input is incorrect, it should be of type data frame. Verify!")
  
  if (type!=1 && type!=2) 
     stop("'type' input is incorrect, it should be numeric, being 1 or 2. Verify!")
  
  data <- as.matrix(data)  # Dados a serem analizados
  
  if (type==1) { # Coeficiente de variacao global
    CVar <- as.matrix(sd(data)/mean(data) * 100)
    colnames(CVar) <- c("C.V. in %")
  }
  
  if (type==2) { # Coeficiente de variacao por coluna
    Media  <- apply(data, 2, mean) # encontra as medias por colunas
    data   <- sweep(data, 2, Media, FUN = "-") # Centraliza na media
    Desvio <- sqrt(colSums(data^2)/(nrow(data)-1)) # raiz da soma do quadrado - desvio padrao amostral
    CVar   <- as.matrix(Desvio/Media * 100)
    colnames(CVar) <- c("C.V. em %")
  }
  
  return(CVar)
}
