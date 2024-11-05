Plot.PCA <- function(PC, titles = NA, xlabel = NA, ylabel = NA, size = 1.1, 
                     grid = TRUE, color = TRUE, linlab = NA, axes = TRUE, class = NA, 
                     classcolor = NA, posleg = 2, boxleg = TRUE, savptc = FALSE,
                     width = 3236, height = 2000, res = 300, casc = TRUE) {
  
  # Rotina para Plotar Graficos do Metodo PCA desenvolvida 
  # por Paulo Cesar Ossani em 11/2014
  
  # Entrada:
  # PC     - Dados da funcao PCA.
  # titles - Titulos para os graficos.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # size   - Tamanho dos pontos nos graficos.
  # grid   - Coloca grade nos graficos.
  # color  - Graficos coloridos (default = TRUE).
  # linlab - Vetor com os rotulos das observacoes.
  # axes   - Coloca eixos no grafico (default = TRUE).
  # class  - Vetor com os nomes das classes dos dados.
  # classcolor - Vetor com as cores das classes.
  # posleg  - 0 sem legenda,
  #           1 para legenda no canto superior esquerdo,
  #           2 para legenda no canto superior direito (default),
  #           3 para legenda no canto inferior direito,
  #           4 para legenda no canto inferior esquerdo.  
  # boxleg - Colocar moldura na legenda (default = TRUE). 
  # savptc - Salva as imagens dos graficos em arquivos (default = FALSE).
  # width  - Largura do grafico quanto savptc = TRUE (defaul = 3236).
  # height - Altura do grafico quanto savptc = TRUE (default = 2000).
  # res    - Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).
  # casc   - Efeito cascata na apresentacao dos graficos (default = TRUE)
  
  # Retorna:
  # Varios graficos
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  # Cria Titulos para os graficos caso nao existam
  if (!is.character(titles[1]) || is.na(titles[1])) titles[1] = c("Scree-plot of the components variances")
  if (!is.character(titles[2]) || is.na(titles[2])) titles[2] = c("Graph corresponding to the rows (observations)")
  if (!is.character(titles[3]) || is.na(titles[3])) titles[3] = c("Graph corresponding to the columns (variables)")
  
  if (!is.na(class[1])) {
    
     class <- as.matrix(class)
    
     if (nrow(PC$mtxscores) != length(class))
        stop("'class' or 'data' input is incorrect, they should contain the same number of lines. Verify!")
  }
  
  if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("'xlabel' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("'ylabel' input is incorrect, it should be of type character or string. Verify!")
   
  if (!is.logical(color))
     stop("'color' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.numeric(size) || size < 0)
     stop("'size' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.logical(grid))
     stop("'grid' input is incorrect, it should be TRUE or FALSE. Verify!")

  if (!is.na(linlab[1]) && length(linlab) != nrow(PC$mtxscores))
     stop("'linlab' input is incorrect, it should have the same number of rows as the input in the database. Verify!")
  
  if (!is.numeric(posleg) || posleg < 0 || posleg > 4 || (floor(posleg)-posleg) != 0)
     stop("'posleg' input is incorrect, it should be a integer number between [0,4]. Verify!")
  
  if (!is.logical(boxleg)) 
     stop("'boxleg' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.logical(axes)) 
     stop("'axes' input is incorrect, it should be TRUE or FALSE. Verify!")
  
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
 
  if (is.na(xlabel[1]))
     xlabel = paste("First coordinate (",round(PC$mtxAutvlr[1,2],2),"%)",sep="")
  
  if (is.na(ylabel[1]))
     ylabel = paste("Second coordinate (",round(PC$mtxAutvlr[2,2],2),"%)",sep="")
  
  if (posleg==1) posleg = "topleft"     # posicao das legendas nos graficos
  if (posleg==2) posleg = "topright"
  if (posleg==3) posleg = "bottomright"
  if (posleg==4) posleg = "bottomleft"
  
  boxleg = ifelse(boxleg,"o","n") # moldura nas legendas, "n" sem moldura, "o" com moldura
  
  num.class = 0
  if (!is.na(class[1])) {
     class.Table <- table(class)        # cria tabela com as quantidade dos elementos das classes
     class.Names <- names(class.Table)  # nomes das classses
     num.class   <- length(class.Table) # numero de classes
     NomeLinhas  <- as.matrix(class)
  } 
  
  if (num.class != 0 && length(classcolor) != num.class && !is.na(classcolor) ||
      num.class == 0 && length(classcolor) != 1 && !is.na(classcolor))
      stop("'classcolor' input is incorrect, it should be in an amount equal to the number of classes in 'class'. Verify!")
  
  #####   FIM - Informacoes usadas nos Graficos  #####
  
  if (savptc) {
     message("\014") # limpa a tela
     message("\n\n Saving graphics to hard disk. Wait for the end!")
  }
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos Autovalores #####
  if (savptc) png(filename = "Figure PCA Variances.png", width = width, height = height, res = res) # salva os graficos em arquivos
  
  mp <- barplot(PC$mtxAutvlr[,1],names.arg=paste(round(PC$mtxAutvlr[,2],2),"%",sep=""),
                main = "Variance of the components")
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Plotagem dos Autovalores #####
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Scree-plot dos componentes #####
  if (savptc) png(filename = "Figure PCA Scree Plot.png", width = width, height = height, res = res) # salva os graficos em arquivos
  
  plot(1:length(PC$mtxAutvlr[,1]), PC$mtxAutvlr[,1],
       type = "n", # nao plota pontos
       xlab = "Order of the components", 
       ylab = "Variance",
       xaxt = "n", # tira o eixo x
       main = titles[1])
  
  axis(1, c(1:length(PC$mtxAutvlr[,1])), c(1:length(PC$mtxAutvlr[,1])))
  
  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  points(1:length(PC$mtxAutvlr[,1]), PC$mtxAutvlr[,1], type = "b")
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Scree-plot dos componentes #####
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos Dados das linhas #####
  if (savptc) png(filename = "Figure PCA Observations.png", width = width, height = height, res = res) # salva os graficos em arquivo
  
  plot(PC$mtxscores, # cria grafico para as coordenadas principais das linhas
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       type = "n", # nao plota pontos
       main = titles[2], # Titulo
       # asp  = 1,  # Aspecto do Grafico
       xlim = c(min(PC$mtxscores[,1])-0.05,max(PC$mtxscores[,1])+0.05), # Dimensao para as linhas do grafico
       ylim = c(min(PC$mtxscores[,2])-0.05,max(PC$mtxscores[,2])+0.05)) # Dimensao para as colunas do grafico
  
  if (grid) {
    
    args <- append(as.list(par('usr')), c('gray93','gray93'))
    
    names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
    do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
    grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  if (num.class == 0) {
    
    points(PC$mtxscores, # cria grafico para as coordenadas principais das linhas
           pch = 16,   # Formato dos pontos 
           cex = size, # Tamanho dos pontos  
           col = ifelse(color,"red","black"))  # Cor dos pontos
    
  } else {
    
    if (!is.na(classcolor[1])) {
      cor.classe <- classcolor
    }
    else { cor.classe <- c("red") }
    
    newdata <- PC$mtxscores
    
    init.form <- 14 # formato inicial dos pontos
    
    cor <- 1 # cor inicial
    
    for (i in 1:num.class) {
      
      point.form <- init.form + i # fomato dos pontos de cada classe
      
      if (!is.na(classcolor[1])) {
        cor1 <- ifelse(color, cor.classe[i], "black")
      }
      else { cor1 <- ifelse(color, cor + i, "black") }
      
      point.data <- newdata[which(class == class.Names[i]),] 
      
      points(point.data,
             pch = point.form, # Formato dos pontos
             cex = size,  # Tamanho dos pontos
             col = cor1) # adiciona ao grafico as coordenadas principais das colunas
    }
    
    if (posleg != 0 && num.class > 0) {
      
      if (color) cor <- 2
      
      init.form <- 15
      
      cor <- ifelse(color, 2, 1)
      
      if (color) {
        if (!is.na(classcolor[1])) {
          color_b <- classcolor
        }
        else { color_b <- cor:(cor + num.class) }
      }
      else { color_b <- cor }
      
      legend(posleg, class.Names, pch = (init.form):(init.form + num.class), col = color_b,
             text.col = color_b, bty = boxleg, text.font = 6, y.intersp = 0.8, xpd = TRUE) # cria a legenda
    }
    
  }
  
  if (axes) abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  if (!is.na(linlab[1])) LocLab(PC$mtxscores, cex = 1, linlab)
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Plotagem dos Dados das linhas #####
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem das Correlacoes dos Componentes Principais com as Variaveis Originais #####
  if (savptc) png(filename = "Figure PCA Correlations.png", width = width, height = height, res = res) # salva os graficos em arquivo
  
  plot(0,0, # cria grafico para as coordenadas das Correlacoes dos Componentes Principais com as Variaveis Originais
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = titles[3], # Titulo
       asp  = 1, # Aspecto do Grafico
       axes = F,
       type = "n", # nao plota pontos
       xlim = c(-1.1,1.1), # Dimensao para as linhas do grafico
       ylim = c(-1.1,1.1)) # Dimensao para as colunas do grafico
  
  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  symbols(0, 0, circles = 1, inches = FALSE, fg = 1, add = TRUE) # cria um circulo
  
  if (axes) abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  arrows(0,0,PC$mtxCCP[,1],PC$mtxCCP[,2], lty=1, code = 2, length = 0.08, angle = 25, col = ifelse(color,"Red","Black")) # cria a seta apontando para cada coordenada principal
  
  LocLab(t(PC$mtxCCP), cex = 1, rownames(PC$mtxCCP) , col = ifelse(color,"Blue","Black"), xpd = TRUE)
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Plotagem das Correlacoes dos Componentes Principais com as Variaveis Originais #####
  
  if (savptc) message("\n \n End!")
  
}