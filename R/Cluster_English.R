Cluster <- function(Data, Titles = NA, Hierarquico = TRUE, Analise = "Obs",  
                    CorAbs = FALSE, Normaliza = FALSE, Distance = "euclidean",  
                    Method = "complete", Horizontal = FALSE, NumGrupos = 0,
                    Casc = TRUE) {
  # Esta funcao executa a Analise de Agrupamentos Hierarquicos e
  # Nao-Hierarquicos, desenvolvida por Paulo Cesar Ossani em 07/2016
  
  # Entrada:
  # Data - Dados a serem a analizados
  # Titles - Titulos para os graficos.
  # Hierarquico - Agrupamentos hierarquicos (default = TRUE), 
  #               para agrupamentos nao hierarquicos (Method K-Means), 
  #               somente para caso Analise = "Obs".
  # Analise - "Obs" para analises nas observacoes (default),
  #           "Var" para analises nas variaveis.
  # CorAbs  - Matriz de correlacao absoluta caso Analise = "Var" (default = FALSE).
  # Normaliza - Normalizar os dados somente para caso Analise = "Obs" (default = TRUE).
  # Distance - Metrica das distancias caso agrupamentos hierarquicos:
  #            "euclidean" (default), "maximum", "manhattan",
  #            "canberra", "binary" ou "minkowski". Caso Analise = "Var" a metrica
  #            sera a matriz de correlacao, conforme CorAbs.
  # Method - Metodo para analises caso agrupamentos hierarquicos:
  #          "complete" (default), "ward.D", "ward.D2", "single",
  #          "average", "mcquitty", "median" ou "centroid".
  # Horizontal - Dendrograma na horizontal (default = FALSE).
  # NumGrupos - Numero de grupos a formar.
  # Casc    - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Varios graficos.
  # TabRes - Tabela com as similaridades e distancias dos grupos formados.
  # Groups - Dados originais com os grupos formados.
  # ResGroups - Resultados dos grupos formados.
  # SQT - Soma do quadrado total.
  # MatrixD - Matriz das distancias.
  
  if (!is.data.frame(Data)) 
     stop("'Data' input is incorrect, it should be of type data frame. Verify!!")

  if (!is.logical(Hierarquico)) 
     stop("'Hierarquico' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  Analise <- toupper(Analise)
  
  if (Analise != "OBS" && Analise != "VAR") 
     stop("'Analise' input is incorrect, it should be 'Obs' for the observations or 'Var' for the variables. Verify!")

  if (!is.logical(CorAbs)) 
     stop("'CorAbs' input is incorrect, it should be TRUE or FALSE. Verify!")

  if (!is.logical(Normaliza)) 
     stop("'Normaliza' input is incorrect, it should be TRUE or FALSE. Verify!")

  Distance <- tolower(Distance) # torna minusculo
  
  DISTANCE <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
  #if (is.na(pmatch(Distance, DISTANCE)))
  if (!(Distance %in% DISTANCE))
     stop("'Distance' input is incorrect, it should be: 'euclidean', 
          'maximum', 'manhattan', 'canberra', 'binary' ou 'minkowski'. Verify!")
  
  METHODS <- c("complete", "ward.D", "ward.D2", "single", "average", "mcquitty", "median" , "centroid")
  # if (is.na(pmatch(Method, METHODS)))
  if (!(Method %in% METHODS)) 
     stop("'Method' input is incorrect, it should be: 'complete', 'ward.D', 
          'ward.D2', 'single', 'average', 'mcquitty', 'median' ou 'centroid'. Verify!")
  
  if (!is.logical(Horizontal)) 
     stop("'Horizontal' input is incorrect, it should be TRUE or FALSE. Verify!")

  if (is.na(NumGrupos)) NumGrupos <- 0 # numero de grupos a formar
  
  if (NumGrupos >= nrow(Data) )
     stop("'NumGroups' input is high. Verify!")
  
  if (NumGrupos < 0)
     stop("'NumGrupos' input is incorrect, it should be positive integer numbers, or zero. Verify!")
  
  if (!Hierarquico && Analise == "VAR")
     stop("The non-hierarchical method is valid only for observations. Verify!")
  
  if (!Hierarquico && NumGrupos < 2)
     stop("For the non-hierarchical method it is necessary NumGrupo > 1. Verify!")
           
  if (!is.logical(Casc))
     stop("'Casc' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  DataNew <- Data # dados a serem analizados
  
  if (Normaliza && Analise == "OBS")
     DataNew <- NormData(DataNew, 2) # normaliza por colunas os dados

  # Cria Titulos para os graficos caso nao existam
  if (!is.character(Titles[1]) || is.na(Titles[1])) Titles[1] = c("Graph of the similarity\n within groups")
  if (!is.character(Titles[2]) || is.na(Titles[2])) Titles[2] = c("Graph of the distances\n within of the groups")
  if (!is.character(Titles[3]) || is.na(Titles[3])) Titles[3] = c("Dendrogram")
  
  ### INICIO - Agrupamentos hierarquicos ###
  if (Hierarquico) {
     
     if (Analise == "OBS") # analise nas observacoes
         Md <- dist(DataNew, method = Distance) # matrix das distancias
     
     if (Analise == "VAR") {# analise nas variaveis
        if (CorAbs) # matrix de correlacao absoluta
            Md <- as.dist(1 - abs(cor(Data))) # matrix das distancias
        
        if (!CorAbs) # matrix de correlacao
            Md <- as.dist(1 - cor(Data)) # matrix das distancias
     }
     
     hc <- hclust(Md, method = Method) # procedimento hierarquico
     
     Grupos <- 0
     if (NumGrupos!=0) 
        Grupos <- cutree(hc, k = NumGrupos) # grupos formados

     if (Analise == "OBS") # novos grupos para as observacoes
        MGrupos  <- cbind(Data, Grupos) # matriz com dados originais mais os grupos formados
     
     if (Analise == "VAR") {# novos grupos para as variaveis
        MGrupos <- cbind(colnames(Data), Grupos) # matriz com dados originais mais os grupos formados
        colnames(MGrupos) <- c("Variables", "Groups")
        rownames(MGrupos) <- NULL
     }
     
     ## INICIO - Tabelas com as Similaridade e as Distancias ##
     if (NumGrupos == 0) Distancia <- hc$height else # Distancias dos agrupamentos
        Distancia <- hc$height[(length(hc$height) - NumGrupos):length(hc$height)]

     Sim <- (1 - Distancia/max(Md)) # calculo das similaridades
     
     Passos <- 1:length(Sim)
     SimGrupos <- length(Sim):1
     Similaridade <- Sim*100
     Tab <- cbind(Passos, SimGrupos, round(Similaridade,3), round(Distancia,2))
     colnames(Tab) <- c("Steps", "Groups", "Similarity", "Distance")
     ## FIM - Tabela com as similirades e distancias ##
     
     ## INICIO - Screen plots ##
     if (Analise == "OBS") {
        
        if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
       
        plot(length(Sim):1, 1/Sim, 
             type = "b", 
             xlab = "Number of clusters", 
             ylab = "Similarity within groups",
             main = Titles[1]) # Titulo)
             
        abline(v=NumGrupos, cex = 1.5, lty = 2) # cria o eixo no agrupamento desejado
        
        if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
        
        plot(length(Distancia):1, Distancia, 
             type ="b", 
             xlab ="Number of clusters", 
             ylab ="Distances within groups",
             main = Titles[2]) # Titulo)
             
        abline(v=NumGrupos, cex = 1.5, lty = 2) # cria o eixo no agrupamento desejado
     }
     ## FIM - Screen plots ##
 
     if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
     
     ## INICIO - Plotagem do Dendrograma ##
     Dendo <- as.dendrogram(hc)
     plot(Dendo, # cordenadas para plotar
          ylab   = "Distance",  # Nomeia Eixo Y
          main   = Titles[3],   # Titulo
          center = TRUE,        # centraliza o grafico
          horiz  = Horizontal,  # posicao do grafico
          cex    = 1) # Tamanho dos pontos
     
     if (NumGrupos > 1 && !Horizontal) 
        rect.hclust(hc, k = NumGrupos, border = "red") # coloca retangulos nos agrupamentos de acordo com NumGrupos
     ## FIM - Plotagem do Dendrograma ##
  }
  ### FIM - Agrupamentos hierarquicos ###

  ### INICIO - Method K-Means ###
  if (!Hierarquico && Analise=="OBS") {
    
     set.seed(7) # semente para fixar processo heuristico
      
     hc <- kmeans(DataNew, NumGrupos, iter.max = 100) # executa o Method K-Means
     #,iter.max = 100, nstart = 21, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen")) # cria particoes pelo Method K-means
      
     #fitted(hc, method = c("centers", "classes"))
     Grupos <- hc$cluster
     
     MGrupos  <- cbind(Data, Grupos) # matriz com dados originais mais os grupos formados
     
     Tab <- NA # tabelas com as similiridades e distancias
     Md  <- NA # matrix das distancias
  }
  ### FIM - Method K-Means ###
  
  ### INICIO - Analises dos grupos ###
  Sqt <- NA # soma do quadrado total 
  TabResGrupos = NA # tabela com os resultados dos grupos
  if (Analise == "OBS" && NumGrupos > 1) {
     TabResGrupos <- NULL
     MGr <- cbind(DataNew, Grupos) # matriz com dados originais mais os grupos formados
     for (i in 1:NumGrupos) { 
        NewGroups <- subset(MGr, Grupos == i) 
        GrupCalc  <- NewGroups[,1:(ncol(NewGroups)-1)]
        Qtd.Elementos <- nrow(NewGroups)
        
        if (Qtd.Elementos==1) Media <- GrupCalc else
           Media <- apply(GrupCalc, 2, mean)
        
        if (Qtd.Elementos==1) SqG <- 0 else # soma dos quadrados dos grupos
           SqG <- sum(sweep(GrupCalc,2, Media)^2) # soma dos quadrados dos grupos
        
        TabResGrupos <- rbind(TabResGrupos,c(i, Qtd.Elementos, SqG, Media))
     }
     colnames(TabResGrupos) <- c("Groups", "Number of Elements", "Sum of Squares",paste("Mean", colnames(TabResGrupos[,4:(ncol(TabResGrupos))])))
    
     Sqt <- sum(sweep(DataNew,2,apply(DataNew, 2, mean))^2) # soma do quadrado total 
  }
  ### FIM - Analises dos grupos ###
  
  Lista <- list(TabRes = Tab, Groups = MGrupos, 
                ResGroups = TabResGrupos, SQT = Sqt,
                MatrixD = Md)
  
  return(Lista)
}