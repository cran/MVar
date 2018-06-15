MDS <- function(Data, Distance = "euclidean", Axis = TRUE, 
                Title = NA, xlabel = NA, ylabel = NA, 
                Color = TRUE, LinLab = NA) {
  # Esta funcao executa a Escalonamento Multidimensional
  # desenvolvida por Paulo Cesar Ossani em 07/2016
  
  # Entrada:
  # Data - Dados a serem a analizados.
  # Distance - Metrica das distancias: "euclidean" (default), "maximum", 
  #            "manhattan", "canberra", "binary" ou "minkowski".
  # Color  - Graficos coloridos (default = TRUE).
  # Axis  - Coloca eixos nos graficos (default = TRUE).
  # Title  - Titulo do grafico, se nulo retorna padrao.
  # xlabel - Nomeia o eixo X, se nulo retorna padrao.
  # ylabel - Nomeia o eixo Y, se nulo retorna padrao.
  # LinLab - Vetor com o rotulo para as linhas, se nao
  #          informado retorna o padrao dos dados.
  
  # Retorna:
  # Grafico de escalonamento multidimensional.
  # MatrixD - Matriz das distancias.
  
  if (!is.data.frame(Data)) 
     stop("'Data' input is incorrect, it should be of type data frame. Verify!")
 
  Distance <- tolower(Distance) # torna minuscula
  
  DISTANCE <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
  #if (is.na(pmatch(Distance, DISTANCE)))
  if (!(Distance %in% DISTANCE))
     stop("'Distance' input is incorrect, it should be: 'euclidean', 
          'maximum', 'manhattan', 'canberra', 'binary' or 'minkowski'. Verify!")

  if (!is.logical(Axis)) 
     stop("'Axis' input is incorrect, it should be TRUE or FALSE. Verify!")

  if (!is.character(Title) && !is.na(Title))
     stop("'Title' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.logical(Color))
     stop("'Color' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.na(LinLab) && length(LinLab)!=nrow(Data))
     stop("The number of label elements for rows 'LinLab', differs from the number of rows in the database. Verify!")
  
  if (is.na(Title))
     Title = "Multidimensional Scaling" # Titulo
  
  if (!is.character(xlabel) && !is.na(xlabel))
     stop("'xlabel' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.character(ylabel) && !is.na(ylabel))
     stop("'ylabel' input is incorrect, it should be of type character or string. Verify!")
  
  if (is.na(xlabel))
     xlabel = "X-Axis" # Nomeia Eixo X  
  
  if (is.na(ylabel))
     ylabel = "Y-Axis" # Nomeia Eixo Y  
  
  if (is.na(LinLab[1]))
     LinLab <- rownames(Data)
  
  if (!is.na(LinLab) && !is.character(LinLab))
     stop("'LinLab' input is incorrect, it should be of type character or string. Verify!")

  Md <- dist(Data, method = Distance) # matrix das distancias
  
  fit <- cmdscale(Md) # gera dos dados para o grafico

  x <- fit[,1] # valores eixo x
  y <- fit[,2] # valores eixo y
  
  plot(x,y, # cria grafico para as coordenadas linhas x e colunas y
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Title,  # Titulo
       asp  = 1,  # Aspecto do Grafico
       pch  = 19, # Formato dos pontos 
       cex  = 1,  # Tamanho dos pontos
       xlim = c(min(x)-0.5,max(x)+0.5), # Dimensao para as linhas do grafico
       ylim = c(min(y)-0.5,max(y)+0.5), # Dimensao para as colunas do grafico
       col  = ifelse(Color,"red","black"))  # Cor dos pontos
  
  if (Axis) # coloca Axis no grafico
     abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  #text(fit, cex = 1, pos = 3, labels = LinLab)  # Coloca os nomes dos pontos das coordenadas
  LocLab(x, y, cex = 1, LinLab)

  Lista <- list(MatrixD = Md)
  
  return(Lista)
}