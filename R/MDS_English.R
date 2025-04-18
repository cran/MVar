MDS <- function(data, distance = "euclidean", title = NA, xlabel = NA,
                ylabel = NA, posleg = 2,  boxleg = TRUE, axes = TRUE, 
                size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
                class = NA, classcolor = NA, savptc = FALSE, width = 3236, 
                height = 2000, res = 300) {
  
  # Esta funcao executa a Escalonamento Multidimensional
  # desenvolvida por Paulo Cesar Ossani em 07/2016
  
  # Entrada:
  # data - Dados a serem a analizados.
  # distance - Metrica das distancias: "euclidean" (default), "maximum", 
  #            "manhattan", "canberra", "binary" ou "minkowski".
  # title  - Titulo do grafico, se nulo retorna padrao.  
  # xlabel - Nomeia o eixo X, se nulo retorna padrao.
  # ylabel - Nomeia o eixo Y, se nulo retorna padrao. 
  # posleg - 0 sem legenda,
  #            1 para legenda no canto superior esquerdo,
  #            2 para legenda no canto superior direito (default),
  #            3 para legenda no canto inferior direito,
  #            4 para legenda no canto inferior esquerdo.  
  # boxleg - Colocar moldura na legenda (default = TRUE).  
  # axes   - Coloca eixos no grafico (default = TRUE).
  # size   - Tamanho dos pontos no grafico.
  # grid   - Coloca grade nos graficos.
  # color  - Graficos coloridos (default = TRUE).
  # linlab - Vetor com os rotulos para as observacoes. 
  # class  - Vetor com os nomes das classes dos dados.
  # classcolor - Vetor com as cores das classes.
  # savptc - Salva as imagens dos graficos em arquivos (default = FALSE).
  # width  - Largura do grafico quanto savptc = TRUE (defaul = 3236).
  # height - Altura do grafico quanto savptc = TRUE (default = 2000).
  # res    - Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).
  
  # Retorna:
  # Grafico de escalonamento multidimensional.
  # mtxD - Matriz das distancias.
  
  if (!is.data.frame(data) && !is.matrix(data)) 
     stop("'data' input is incorrect, it should be of type data frame or matrix. Verify!")
 
  if (!is.na(class[1])) {
    
     class <- as.matrix(class)
    
     if (nrow(data) != length(class))
       stop("'class' or 'data' input is incorrect, they should contain the same number of lines. Verify!")
  }
  
  distance <- tolower(distance) # torna minuscula
  
  Distances <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
  
  if (!(distance %in% Distances))
     stop("'distance' input is incorrect, it should be: 'euclidean', 
          'maximum', 'manhattan', 'canberra', 'binary' or 'minkowski'. Verify!")

  if (!is.logical(axes)) 
     stop("'axes' input is incorrect, it should be TRUE or FALSE. Verify!")

  if (!is.character(title) && !is.na(title[1]))
     stop("'title' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.numeric(posleg) || posleg < 0 || posleg > 4 || (floor(posleg)-posleg) != 0)
     stop("'posleg' input is incorrect, it should be a integer number between [0,4]. Verify!")
  
  if (!is.numeric(size) || size < 0)
     stop("'size' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.logical(grid))
     stop("'grid' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.logical(color))
     stop("'color' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.logical(boxleg)) 
     stop("'boxleg' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.na(linlab[1]) && length(linlab)!=nrow(data))
     stop("The number of label elements for rows 'linlab', differs from the number of rows in the database. Verify!")
  
  if (is.na(title[1]))
     title = "Multidimensional Scaling" # Titulo
  
  if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("'xlabel' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("'ylabel' input is incorrect, it should be of type character or string. Verify!")

  if (!is.na(linlab[1]) && !is.character(linlab))
     stop("'linlab' input is incorrect, it should be of type character or string. Verify!")

  if (!is.logical(savptc))
     stop("'savptc' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.numeric(width) || width <= 0)
     stop("'width' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.numeric(height) || height <= 0)
     stop("'height' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.numeric(res) || res <= 0)
     stop("'res' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  if (savptc) {
     message("\014") # limpa a tela
     message("\n\n Saving graphics to hard disk. Wait for the end!")
  }
  
  if (is.na(xlabel[1])) xlabel = "X-axis" # Nomeia Eixo X  
  
  if (is.na(ylabel[1])) ylabel = "Y-axis" # Nomeia Eixo Y 

  if (posleg==1) posleg = "topleft"     # posicao das legendas nos graficos
  if (posleg==2) posleg = "topright"
  if (posleg==3) posleg = "bottomright"
  if (posleg==4) posleg = "bottomleft"
  
  boxleg = ifelse(boxleg,"o","n") # moldura nas legendas, "n" sem moldura, "o" com moldura
  
  Num.class = 0
  if (!is.na(class[1])) {
     class.Table <- table(class)       # cria tabela com as quantidade dos elementos das classes
     class.Names <- names(class.Table)  # nomes das classses
     Num.class   <- length(class.Table) # numero de classes
     NomeLinhas  <- as.matrix(class)
  } 

  if (Num.class != 0 && length(classcolor) != Num.class && !is.na(classcolor) ||
      Num.class == 0 && length(classcolor) != 1 && !is.na(classcolor))
     stop("'classcolor' input is incorrect, it should be in an amount equal to the number of classes in 'class'. Verify!")
  
  cor <- 1 # cor inicial dos pontos e legendas
  ##### FIM - Informacoes usadas nos Graficos #####
  
  Md <- dist(data, method = distance) # matrix das distancias
  
  fit <- cmdscale(Md) # gera dos dados para o grafico
  
  x <- fit[,1] # valores eixo x
  y <- fit[,2] # valores eixo y
  
  if (!is.na(classcolor[1])) {
    cor.classe <- classcolor
  }
  else { cor.classe <- c("red") }
  
  if (savptc) png(filename = "Figure_MDS.png", width = width, height = height, res = res) # salva os graficos em arquivo
  
  plot(x,y, # cria grafico para as coordenadas linhas x e colunas y
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       type = "n",    # tipo de grafico  
       main = title,  # Titulo
       xlim = c(min(x)-0.5,max(x)+0.5), # Dimensao para as linhas do grafico
       ylim = c(min(y)-0.5,max(y)+0.5)) # Dimensao para as colunas do grafico
  
  if (grid) {
    
    args <- append(as.list(par('usr')), c('gray93','gray93'))
    
    names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
    do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
    grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  if (Num.class == 0) {
     points(x,y, # cria grafico para as coordenadas linhas x e colunas y
            pch = 19,   # Formato dos pontos 
            cex = size, # tamanho dos pontos         
            col = ifelse(color, cor.classe, "Black"))
    
  } else {
    
    Newdata <- cbind(x,y)
    
    Init.Form <- 14 # formato inicial dos pontos
    
    for (i in 1:Num.class) {
      
      Point.Form <- Init.Form + i # fomato dos pontos de cada classe
      
      if (!is.na(classcolor[1])) {
         cor1 <- ifelse(color, cor.classe[i], "black")
      }
      else { cor1 <- ifelse(color, cor + i, "black") }
      
      Point.data <- Newdata[which(class == class.Names[i]),]
      
      points(Point.data,
             pch = Point.Form, # Formato dos pontos
             cex = size,  # Tamanho dos pontos  
             col = cor1) # adiciona ao grafico as coordenadas principais das colunas
    }
    
  }
  
  if (posleg != 0 && Num.class > 0) {
    
    if (color) cor <- 2
    
    Init.Form <- 15
    
    cor <- ifelse(color, 2, 1)
    
    if (color) {
       if (!is.na(classcolor[1])) {
          color_b <- classcolor
       }
       else { color_b <- cor:(cor + Num.class) }
    }
    else { color_b <- cor }
    
    legend(posleg, class.Names, pch = (Init.Form):(Init.Form + Num.class), col = color_b,
           text.col = color_b, bty = boxleg, text.font = 6, y.intersp = 0.8, xpd = TRUE) # cria a legenda
  }
  
  if (axes) abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  if (!is.na(linlab[1])) LocLab(x, y, cex = 1, linlab)
  
  if (savptc) {
     box(col = 'white') 
     dev.off()
     message("\n \n End!")
  }
  
  Lista <- list(mtxD = Md)
  
  return(Lista)
}