GSVD <- function(Data, PLin = NULL, PCol = NULL) {
  # Funcao que executa a Decomposicao dos Valores 
  # Singulares Generalizados de um matriz (Data)
  
  # Entrada:
  # Data - Matriz usada para a decomposicao
  # PLin - Peso para as linhas
  # PCol - Peso para as colunas
  
  # Retorna:
  # d - Vector linha com os valores singulares da decomposicao
  # u - Autovetores referentes das linhas
  # v - Autovetores referentes das colunas
  
  if (is.null(PCol)) PCol <- rep(1, ncol(Data))
  
  if (is.null(PLin)) PLin <- rep(1, nrow(Data))
  
  else if (is.numeric(PLin)) PLin = PLin / sum(PLin)
  
  if (!is.numeric(PLin))
     stop("'PLin' input is incorrect, it should be numeric vector. Verify!")
  
  if (!is.numeric(PCol))
     stop("'PCol' input is incorrect, it should be numeric vector. Verify!")
  
  if (nrow(Data) != length(PLin))
     stop("The number of elements in 'Plin' should be equal to the number of rows in 'Data'. Verify!")
  
  if (ncol(Data) != length(PCol))
     stop("The number of elements in 'PCol' should be equal to the number of columns in 'Data'. Verify!")
  
  PLin <- as.vector(PLin)
  
  PCol <- as.vector(PCol)
  
  ncv <- min(nrow(Data)-1,ncol(Data)) # numero de colunas validas
  
  AA = sweep(Data, 2, sqrt(PCol), FUN = "*")
  
  AA = sweep(AA, 1, sqrt(PLin), FUN = "*")
  
  MSVD <- svd(AA)
  d <- MSVD$d
  P <- MSVD$u
  Q <- MSVD$v
  
  MU <- diag(sqrt(1/PLin))%*%P
  
  MV <- diag(sqrt(1/PCol))%*%Q
  
  Resp <- list(d = d[1:ncv], u = MU[,1:ncv], v = MV[,1:ncv])
  
  return(Resp)
}