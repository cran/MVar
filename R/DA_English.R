DA <- function(Data, Class = NA, Type = "lda", Validation = "Learning", 
               Method = "moment", Prior = NA, Testing = NA) {

  # Esta funcao executa a Analide discriminante linear e quadratica
  # desenvolvida por Paulo Cesar Ossani em 05/2019
  
  # Entrada:
  # Data  - Dados a serem a classificados.
  # Class - Vetor com os nomes das classes dos dados.
  # Type  - "lda": analise discriminante linear (default), ou "qda": analise discriminante quadratica.
  # Validation - Tipo de validacao: "Learning" - Treinamento dos dados (default), ou "Testing" - classifica os dados do vetor "Testing".
  # Method  - Metodo de classificacao: "mle" para MLEs, "mve" para usar cov.mv, "moment" (default) para estimadores padrao da media e variancia ou "t" para estimativas robustas baseadas em uma distribuicao t.
  # Prior   - Probabilidades de ocorrencia das classes. Se nao especificado, tomara as proporcoes das classes. Se especificado, as probabilidades devem seguir a ordem dos niveis dos fatores.
  # Testing - Vetor com os indices que serao utilizados em Data como teste. Para Validation = "Learning", tem-se Testing = NA.
  
  # Retorna:
  # Confusion - Tabela de confusao.
  # Error.rate - Proporcao global de erro.
  # Prior - Probabilidade das classes.
  # Type  - Tipo de analise discriminante.
  # Validation - Tipo de validacao.
  # Num.Class	- Numero de classes.
  # Class.Names	- Nomes das classes.
  # Method  - Metodo de classificacao.
  # Num.Correct - Numero de observacoes corretas.
  # Results - Matriz com resultados comparativos das classificacoes.

  if (!is.data.frame(Data)) 
     stop("'Data' input is incorrect, it should be of type data frame or matrix. Verify!")
  
  if (!is.na(Class[1])) {

     Class <- as.matrix(Class)

     if (nrow(Data) != length(Class))
        stop("'Class' or 'Data' input is incorrect, they should contain the same number of lines. Verify!")
  }
  
  Type = tolower(Type) # torna minusculo
  if (!(Type %in% c("lda", "qda")))
     stop("'Type' input is incorrect, it should be: 'lda' or 'qda'. Verify!")
  
  if (!is.na(Prior[1]) && sum(Prior) != 1)
     stop("The sum of the elements in 'Prior' must be equal to one. Verify!")
  
  Validation = tolower(Validation) # torna minusculo
  if (!(Validation %in% c("learning","testing")))
     stop("'Validation' input is incorrect, it should be: 'Learning' or 'Testing'. Verify!")
  
  if (Validation == "testing" && is.na(Testing[1]))
     stop("Input for Validation = 'Testing', the 'Testing' vector must be added. Verify!")
  
  Method = tolower(Method) # torna minusculo
  if (!(Method %in% c("mle","mve","moment","t")))
     stop("'Method' input is incorrect, it should be: 'mle', 'mve', 'moment' or 't'. Verify!")
  
  if (Validation == "learning" && !is.na(Testing[1]))
     stop("For Validation = 'learning', 'Testing' should be equal to 'NA'. Verify!")
  
  if (!is.na(Class[1])) {
     Class.Table <- table(Class)        # cria tabela com as quantidade dos elementos das classes
     Class.Names <- names(Class.Table)  # nomes das classses
     Num.Class   <- length(Class.Table) # numero de classes
  } else {
     Num.Class <- 1
  }

  if (!is.na(Prior[1]) && length(Prior) != Num.Class)
     stop("The number of elements in 'Prior' must be equal to the number of classes. Verify!")
  
  if (is.na(Prior[1])) # caso probabilidade a priori nao seja informada
     Prior <- as.double(rep(1,Num.Class)/Num.Class)
  
  if (Validation == "learning")
     Learning = as.integer(rownames(Data))
  
  if (Validation == "testing" && !is.na(Testing[1]))
     Learning = as.integer(rownames(Data[-Testing,]))
     
  ## Analise Discriminante Linear
  if (Type == "lda") {
     disc <- MASS::lda(Class~., Data, prior = Prior, method = Method, subset = Learning)
  }
  
  ## Analise Discriminante quadratica
  if (Type == "qda") {
     disc <- MASS::qda(Class~., Data, prior = Prior, method = Method, subset = Learning)
  }
  
  if (Validation == "learning" || is.na(Testing[1]))
     Learning = -Learning
  
  Predict <- predict(disc, Data[-Learning,])$class
  
  MClass <- cbind(as.vector(Class[-Learning]), as.vector(Predict), " ")
  MClass[MClass[,1]!=MClass[,2],3] = "*" # acrescenta * quando divergir
  MClass <- as.data.frame(MClass)
  colnames(MClass) <- c("Initial classes", "Predicted classes", "Divergence")

  confusion <- table(Class[-Learning], Predict) # tabela de confunsao
  
  Num.Correct <- sum(diag(confusion)) # numero de observacoes corretas
  
  error_rate <- 1 - Num.Correct / sum(confusion) # taxa de erro
  
  total <- colSums(confusion)
  prop  <- round(diag(confusion)/total,4)
  confusion <- rbind(confusion, total) # total real
  confusion <- rbind(confusion, diag(confusion)) # total de acertos
  confusion <- as.table(rbind(confusion, as.character(prop)))
  rownames(confusion) <- c(colnames(confusion), "Total", "Number of hits", "Proportion of hits")
  
  Lista <- list(Confusion = confusion, Error.rate = error_rate, Prior = disc$prior, Type = Type,
                Num.Class = Num.Class, Class.Names = Class.Names, Method = Method, 
                Validation = Validation, Num.Correct = Num.Correct, Results = MClass)

  return(Lista)
}
