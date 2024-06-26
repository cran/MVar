PCA <- function(data, type = 1) {
  # Funcao Executa a Analise dos Componentes Principais - PCA 
  # Desenvolvida por Paulo Cesar Ossani em 07/2013
  
  # Entrada:
  # data - Dados a serem a analizados
  # type - 1 para analise utilizando a matriz de covariancia (default)
  #        2 para analise utilizando a matriz de correlacao
  
  # Retorna:
  # mtxC      - Matriz de Covariancia ou de Correlacao conforme type
  # mtxAutvlr - Matriz de Autovalores (Variancias) com as proporcoes e proporcoes acumuladas
  # mtxAutvec - Matriz de Autovetores - Componentes Principais
  # mtxVCP    - Matriz da Covariancia dos Componentes Principais com as Variaveis Originais
  # mtxCCP    - Matriz da Correlacao dos Componentes Principais com as Variaveis Originais
  # mtxscores - Matriz com os escores dos Componentes Principais
  
  if (!is.data.frame(data) && !is.matrix(data)) 
     stop("'data' input is incorrect, it should be of type data frame or matrix. Verify!")
  
  if (type!=1 && type!=2) 
     stop("'type' input is incorrect, it should be numeric, being 1 or 2. Verify!")
  
  if (type == 2) data <- scale(data) # normaliza os dados
  
  MC <- cov(data) # Matriz de Covariancia
  
  num.comp <- min(dim(data)) # numero de componentes
  
  ## Encontrando a Matriz de Decomposicao Expectral
  MAV <- eigen(MC) # Encontra a matriz de autovalor e autovetor
  MAutoVlr <- MAV$values[1:num.comp]  # Matriz de Autovalores - Variancias
  MAutoVec <- MAV$vectors # Matriz de Autovetores - Componentes Principais
  
  ## Matriz das Variancias
  MEigen <- as.data.frame(matrix(NA, length(MAutoVlr), 3))
  rownames(MEigen) <- paste("Comp", 1:length(MAutoVlr))
  colnames(MEigen) <- c("Eigenvalue", "Proporcion of the variance","Cumulative proportion of the variance")
  MEigen[, "Eigenvalue"] <- MAutoVlr
  MEigen[, "Proporcion of the variance"] <- (MAutoVlr/sum(MAutoVlr)) * 100
  MEigen[, "Cumulative proportion of the variance"] <- cumsum(MEigen[,"Proporcion of the variance"])
  
  ## Matriz de Autovetores,ou seja, os Componentes Principais
  colnames(MAutoVec) <- paste("Comp.", 1:nrow(MC), sep = " ")
  rownames(MAutoVec) <- colnames(data)  
  
  ## Covariancia dos Componentes Principais com as Variaveis Originais
  VCP <- diag(MAutoVlr,nrow(MC),ncol(MC))%*%t(MAutoVec)
  rownames(VCP) <- paste("Comp", 1:nrow(MC))
  
  ## Correlacao dos Componentes Principais com as Variaveis Originais
  CCP <- diag(sqrt(MAutoVlr),nrow(MC),ncol(MC))%*%t(MAutoVec)%*%diag(1/sqrt(diag(MC)),nrow(MC),ncol(MC))
  colnames(CCP) <- colnames(data) # Nomeia as linhas
  rownames(CCP) <- paste("Comp", 1:nrow(MC))
  
  Esc = as.matrix(data)%*%MAutoVec # Escores do componentes principais
  rownames(Esc) <- rownames(data)
  
  Lista <- list(mtxC = MC, mtxAutvlr = MEigen,
                mtxAutvec = MAutoVec, mtxVCP = VCP, 
                mtxCCP = t(CCP), mtxscores = Esc)
  
  return(Lista)
}
