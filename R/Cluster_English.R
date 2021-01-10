Cluster <- function(data, titles = NA, hierarquico = TRUE, analise = "Obs",  
                    corabs = FALSE, normaliza = FALSE, distance = "euclidean",  
                    method = "complete", horizontal = FALSE, numgrupos = 0,
                    lambda = 2, savptc = FALSE, width = 3236, height = 2000, 
                    res = 300, casc = TRUE) {
  
  # Esta funcao executa a analise de Agrupamentos hierarquicos e
  # Nao-hierarquicos, desenvolvida por Paulo Cesar Ossani em 07/2016
  
  # Entrada:
  # data - Dados a serem a analizados
  # titles - Titulos para os graficos.
  # hierarquico - Agrupamentos hierarquicos (default = TRUE), 
  #               para agrupamentos nao hierarquicos (method K-Means), 
  #               somente para caso analise = "Obs".
  # analise - "Obs" para analises nas observacoes (default),
  #           "Var" para analises nas variaveis.
  # corabs  - Matriz de correlacao absoluta caso analise = "Var" (default = FALSE).
  # normaliza - normalizar os dados somente para caso analise = "Obs" (default = TRUE).
  # distance - Metrica das distancias caso agrupamentos hierarquicos:
  #            "euclidean" (default), "maximum", "manhattan",
  #            "canberra", "binary" ou "minkowski". Caso analise = "Var" a metrica
  #            sera a matriz de correlacao, conforme corabs.
  # method - Metodo para analises caso agrupamentos hierarquicos:
  #          "complete" (default), "ward.D", "ward.D2", "single",
  #          "average", "mcquitty", "median" ou "centroid".
  # horizontal - Dendrograma na horizontal (default = FALSE).
  # numgrupos - Numero de grupos a formar.
  # lambda    - Valor usado na distancia de minkowski.
  # savptc   - Salva as imagens dos graficos em arquivos (default = FALSE).
  # width    - Largura do grafico quanto savptc = TRUE (defaul = 3236).
  # height   - Altura do grafico quanto savptc = TRUE (default = 2000).
  # res      - Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).
  # casc    - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Varios graficos.
  # tabres - Tabela com as similaridades e distancias dos grupos formados.
  # groups - Dados originais com os grupos formados.
  # resgroups - Resultados dos grupos formados.
  # sqt - Soma do quadrado total.
  # mtxD - Matriz das distancias.
  
  if (!is.data.frame(data)) 
     stop("'data' input is incorrect, it should be of type data frame. Verify!!")

  if (!is.logical(hierarquico)) 
     stop("'hierarquico' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  analise <- toupper(analise)
  
  if (analise != "OBS" && analise != "VAR") 
     stop("'analise' input is incorrect, it should be 'Obs' for the observations or 'Var' for the variables. Verify!")

  if (!is.logical(corabs)) 
     stop("'corabs' input is incorrect, it should be TRUE or FALSE. Verify!")

  if (!is.logical(normaliza)) 
     stop("'normaliza' input is incorrect, it should be TRUE or FALSE. Verify!")

  distance <- tolower(distance) # torna minusculo
  
  Distances <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
  #if (is.na(pmatch(distance, distance)))
  if (!(distance %in% Distances))
     stop("'distance' input is incorrect, it should be: 'euclidean', 
          'maximum', 'manhattan', 'canberra', 'binary' ou 'minkowski'. Verify!")
  
  Methods <- c("complete", "ward.D", "ward.D2", "single", "average", "mcquitty", "median" , "centroid")
  # if (is.na(pmatch(method, methodS)))
  if (!(method %in% Methods)) 
     stop("'method' input is incorrect, it should be: 'complete', 'ward.D', 
          'ward.D2', 'single', 'average', 'mcquitty', 'median' ou 'centroid'. Verify!")
  
  if (!is.logical(horizontal)) 
     stop("'horizontal' input is incorrect, it should be TRUE or FALSE. Verify!")

  if (is.na(numgrupos)) numgrupos <- 0 # numero de grupos a formar
  
  if (numgrupos >= nrow(data) )
     stop("'Numgroups' input is high. Verify!")
  
  if (numgrupos < 0)
     stop("'numgrupos' input is incorrect, it should be positive integer numbers, or zero. Verify!")
  
  if (!hierarquico && analise == "VAR")
     stop("The non-hierarchical method is valid only for observations. Verify!")
  
  if (!hierarquico && numgrupos < 2)
     stop("For the non-hierarchical method it is necessary NumGrupo > 1. Verify!")
  
  if (!is.numeric(lambda) || lambda <= 0)
     stop("'lambda' input is incorrect, it is necessary lambda > 0. Verify!") 
           
  if (!is.logical(savptc))
     stop("'savptc' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.numeric(width) || width <= 0)
     stop("'width' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.numeric(height) || height <= 0)
     stop("'height' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.numeric(res) || res <= 0)
     stop("'res' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.logical(casc && !savptc))
     stop("'casc' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  dataNew <- data # dados a serem analizados
  
  if (normaliza && analise == "OBS")
     dataNew <- NormData(dataNew, 2) # normaliza por colunas os dados

  # Cria Titulos para os graficos caso nao existam
  if (!is.character(titles[1]) || is.na(titles[1])) titles[1] = c("Graph of the similarity\n within groups")
  if (!is.character(titles[2]) || is.na(titles[2])) titles[2] = c("Graph of the distances\n within of the groups")
  if (!is.character(titles[3]) || is.na(titles[3])) titles[3] = c("Dendrogram")
  
  ### INICIO - Agrupamentos hierarquicos ###
  if (hierarquico) {
    
     if (savptc) {
        cat("\014") # limpa a tela
        cat("\n\n Saving graphics to hard disk. Wait for the end!")
     }
    
     if (analise == "OBS") # analise nas observacoes
         Md <- dist(dataNew, method = distance, p = lambda) # matrix das distancias
     
     if (analise == "VAR") {# analise nas variaveis
        if (corabs) # matrix de correlacao absoluta
            Md <- as.dist(1 - abs(cor(data))) # matrix das distancias
        
        if (!corabs) # matrix de correlacao
            Md <- as.dist(1 - cor(data)) # matrix das distancias
     }
     
     hc <- hclust(Md, method = method) # procedimento hierarquico
     
     Grupos <- 0
     if (numgrupos!=0) 
        Grupos <- cutree(hc, k = numgrupos) # grupos formados

     if (analise == "OBS") # novos grupos para as observacoes
        MGrupos  <- cbind(data, Grupos) # matriz com dados originais mais os grupos formados
     
     if (analise == "VAR") {# novos grupos para as variaveis
        MGrupos <- cbind(colnames(data), Grupos) # matriz com dados originais mais os grupos formados
        colnames(MGrupos) <- c("Variables", "groups")
        rownames(MGrupos) <- NULL
     }
     
     ## INICIO - Tabelas com as Similaridade e as Distancias ##
     if (numgrupos == 0) Distancia <- hc$height else # Distancias dos agrupamentos
        Distancia <- hc$height[(length(hc$height) - numgrupos):length(hc$height)]

     Sim <- (1 - Distancia/max(Md)) # calculo das similaridades
     
     Passos <- 1:length(Sim)
     SimGrupos <- length(Sim):1
     Similaridade <- Sim*100
     Tab <- cbind(Passos, SimGrupos, round(Similaridade,3), round(Distancia,2))
     colnames(Tab) <- c("Steps", "groups", "Similarity", "distance")
     ## FIM - Tabela com as similirades e distancias ##
     
     ## INICIO - Screen plots ##
     if (analise == "OBS") {
        
        if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
       
        if (savptc) png(filename = "Figure Cluster Screen plots 1.png", width = width, height = height, res = res) # salva os graficos em arquivos
       
        plot(length(Sim):1, 1/Sim, 
             type = "b", 
             xlab = "Number of clusters", 
             ylab = "Similarity within groups",
             main = titles[1]) # Titulo)
             
        abline(v=numgrupos, cex = 1.5, lty = 2) # cria o eixo no agrupamento desejado
        
        if (savptc) dev.off() 
        
        if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
        
        if (savptc) png(filename = "Figure Cluster Screen plots 2.png", width = width, height = height, res = res) # salva os graficos em arquivos
        
        plot(length(Distancia):1, Distancia, 
             type ="b", 
             xlab ="Number of clusters", 
             ylab ="Distances within groups",
             main = titles[2]) # Titulo)
             
        abline(v=numgrupos, cex = 1.5, lty = 2) # cria o eixo no agrupamento desejado
        
        if (savptc) dev.off() 
     }
     ## FIM - Screen plots ##
 
     if (casc && !savptc) dev.new()  # efeito cascata na apresentacao dos graficos
     
     ## INICIO - Plotagem do Dendrograma ##
     if (savptc) png(filename = "Figure Cluster Dendrogram.png", width = width, height = height, res = res) # salva os graficos em arquivos
     
     Dendo <- as.dendrogram(hc)
     plot(Dendo, # cordenadas para plotar
          ylab   = "Distance",  # Nomeia Eixo Y
          main   = titles[3],   # Titulo
          center = TRUE,        # centraliza o grafico
          horiz  = horizontal,  # posicao do grafico
          cex    = 1) # Tamanho dos pontos
     
     if (numgrupos > 1 && !horizontal) 
        rect.hclust(hc, k = numgrupos, border = "red") # coloca retangulos nos agrupamentos de acordo com numgrupos
     
     if (savptc) { 
        dev.off() 
        cat("\n \n End!")
     }
     ## FIM - Plotagem do Dendrograma ##
  }
  ### FIM - Agrupamentos hierarquicos ###

  ### INICIO - method K-Means ###
  if (!hierarquico && analise=="OBS") {
    
     set.seed(7) # semente para fixar processo heuristico
      
     hc <- kmeans(dataNew, numgrupos, iter.max = 100) # executa o method K-Means
     #,iter.max = 100, nstart = 21, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen")) # cria particoes pelo method K-means
      
     #fitted(hc, method = c("centers", "classes"))
     Grupos <- hc$cluster
     
     MGrupos  <- cbind(data, Grupos) # matriz com dados originais mais os grupos formados
     
     Tab <- NA # tabelas com as similiridades e distancias
     Md  <- NA # matrix das distancias
  }
  ### FIM - method K-Means ###
  
  ### INICIO - analises dos grupos ###
  sqt <- NA # soma do quadrado total 
  tabresGrupos = NA # tabela com os resultados dos grupos
  if (analise == "OBS" && numgrupos > 1) {
     tabresGrupos <- NULL
     MGr <- cbind(dataNew, Grupos) # matriz com dados originais mais os grupos formados
     for (i in 1:numgrupos) { 
        Newgroups <- subset(MGr, Grupos == i) 
        GrupCalc  <- Newgroups[,1:(ncol(Newgroups)-1)]
        Qtd.Elementos <- nrow(Newgroups)
        
        if (Qtd.Elementos==1) Media <- GrupCalc else
           Media <- apply(GrupCalc, 2, mean)
        
        if (Qtd.Elementos==1) SqG <- 0 else # soma dos quadrados dos grupos
           SqG <- sum(sweep(GrupCalc,2, Media)^2) # soma dos quadrados dos grupos
        
        tabresGrupos <- rbind(tabresGrupos,c(i, Qtd.Elementos, SqG, Media))
     }
     colnames(tabresGrupos) <- c("groups", "Number of Elements", "Sum of Squares",paste("Mean", colnames(tabresGrupos[,4:(ncol(tabresGrupos))])))
    
     sqt <- sum(sweep(dataNew,2,apply(dataNew, 2, mean))^2) # soma do quadrado total 
  }
  ### FIM - analises dos grupos ###
  
  Lista <- list(tabres = Tab, groups = MGrupos, 
                resgroups = tabresGrupos, sqt = sqt,
                mtxD = Md)
  
  return(Lista)
}