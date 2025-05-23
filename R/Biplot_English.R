Biplot <- function(data, alpha = 0.5, title = NA, xlabel = NA, ylabel = NA,
                   size = 1.1, grid = TRUE, color = TRUE, var = TRUE,
                   obs = TRUE, linlab = NA, class = NA, classcolor = NA,
                   posleg = 2, boxleg = TRUE, axes = TRUE, savptc = FALSE,
                   width = 3236, height = 2000, res = 300) {
  
  # Rotina para gerar Biplot desenvolvida 
  # por Paulo Cesar Ossani em 20/06/2015
  
  # Entrada:
  # data   - Dados para plotagem.
  # alpha  - Representatividade dos individuos (alpha), 
  #         representatividade das variaveis (1-alpha). 
  #         Sendo 0.5 o default.
  # title  - Titulo para o grafico. Se nao for definido assume texto padrao.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # size   - Tamanho dos pontos nos graficos.
  # grid   - Coloca grade nos graficos.
  # color  - Graficos coloridos (default = TRUE).
  # var    - Acrescenta as projecoes das variaveis ao grafico (default = TRUE).
  # obs    - Acrescenta as observacoes ao grafico (default = TRUE).
  # linlab - Vetor com o rotulo para as linhas.
  # class   - Vetor com os nomes das classes dos dados.
  # classcolor - Vetor com as cores das classes.
  # posleg  - 0 sem legenda,
  #           1 para legenda no canto superior esquerdo,
  #           2 para legenda no canto superior direito (default),
  #           3 para legenda no canto inferior direito,
  #           4 para legenda no canto inferior esquerdo.  
  # boxleg  - Colocar moldura na legenda (default = TRUE).
  # axes    - Plota os eixos X e Y (default = TRUE).
  # savptc  - Salva as imagens dos graficos em arquivos (default = FALSE).
  # width   - Largura do grafico quanto savptc = TRUE (defaul = 3236).
  # height  - Altura do grafico quanto savptc = TRUE (default = 2000).
  # res     - Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).
  
  # Retorna:
  # Grafico Biplot.
  # Md - Matriz autovalores.
  # Mu - Matriz U (autovetores).
  # Mv - Matriz V (autovetores).
  # coorI - Coordenadas dos individuos.
  # coorV - Coordenadas das variaveis.
  # pvar   - Proporcao dos componentes principais.
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  
  if (!is.data.frame(data) && !is.matrix(data))
     stop("'data' input is incorrect, it should be of type data frame or matrix. Verify!")
  
  if (!is.na(class[1])) {
     
     class <- as.matrix(class)
    
     if (nrow(data) != length(class))
        stop("'class' or 'data' input is incorrect, they should contain the same number of lines. Verify!")
  }
  
  if (!is.numeric(alpha) || alpha < 0 || alpha > 1)
     stop("'alpha' input is incorrect, it should be numerical, with a value between 0 and 1. Check!")
  
  if (!is.character(title) && !is.na(title[1]))
     stop("'title' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("'xlabel' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("'ylabel' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.numeric(size) || size < 0)
     stop("'size' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.logical(grid))
     stop("'grid' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.logical(color))
     stop("'color' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.logical(var)) 
     stop("'var' input is incorrect, it should be TRUE or FALSE. Verify!")

  if (!is.logical(obs)) 
    stop("'obs' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!var && !obs)
     stop("'var' or 'obs' input is incorrect, it should be TRUE ou FALSE, both cannot be FALSE. Verify!")
  
  if (!is.na(linlab[1]) && length(linlab)!=nrow(data))
     stop("The number of label elements to lines 'linlab', differs from the number of rows in the database. Verify!")
  
  if (!is.logical(boxleg)) 
     stop("'boxleg' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.logical(axes)) 
     stop("'axes' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.numeric(posleg) || posleg < 0 || posleg > 4 || (floor(posleg)-posleg) != 0)
     stop("'posleg' input is incorrect, it should be a integer number between [0,4]. Verify!")

  if (!is.logical(savptc))
     stop("'savptc' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.numeric(width) || width <= 0)
     stop("'width' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.numeric(height) || height <= 0)
     stop("'height' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.numeric(res) || res <= 0)
     stop("'res' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  # if (is.na(linlab[1])) linlab <- rownames(data)
  
  if (is.na(title[1])) title = "Biplot Graphic"  
  
  if (savptc) {
     message("\014") # limpa a tela
     message("\n\n Saving graphics to hard disk. Wait for the end!")
  }
  
  LinNames <- linlab # nomes das observacoes
  
  Mdata = as.matrix(data) # transforma dados em matriz
  
  ### Centraliza os dados na media
  Media <- apply(Mdata, 2, mean) # medias por colunas
  Mdata <- sweep(Mdata, 2, Media, FUN = "-") # Centraliza na media
  
  ### Decompondo Singularmente a Matriz de Dados
  dim  <- 2 # dimenssao 
  Mdvs <- svd(Mdata) # Matriz de Decomposicao Valor Singular
  Md = Mdvs$d # Matriz autovalores
  Mu = Mdvs$u # Matriz U (autovetores)
  Mv = Mdvs$v # Matriz V (autovetores)
  
  coorI <- Mu[,1:dim]%*%diag(Md[1:dim])^alpha     # coordenadas individuos
  coorV <- Mv[,1:dim]%*%diag(Md[1:dim])^(1-alpha) # coordenadas variaveis
  
  pvar <- (Md^2/sum(Md^2)) * 100 # Proporcao dos primeiros (dim) componentes principais
  
  if (is.na(xlabel[1]))
     xlabel = paste("First coordinate (",round(pvar[1],2),"%)",sep="")
  
  if (is.na(ylabel[1]))
     ylabel = paste("Second coordinate (",round(pvar[2],2),"%)",sep="")
  
  if (posleg==1) posleg = "topleft"   # posicao das legendas nos graficos
  if (posleg==2) posleg = "topright"
  if (posleg==3) posleg = "bottomright"
  if (posleg==4) posleg = "bottomleft"
  
  boxleg = ifelse(boxleg,"o","n") # moldura nas legendas, "n" sem moldura, "o" com moldura
  
  Num.class = 0
  if (!is.na(class[1])) {
     class.Table <- table(class)        # cria tabela com as quantidade dos elementos das classes
     class.Names <- names(class.Table)  # nomes das classses
     Num.class   <- length(class.Table) # numero de classes
     NomeLinhas  <- as.matrix(class)
  } 
  
  if (Num.class != 0 && length(classcolor) != Num.class && !is.na(classcolor) ||
      Num.class == 0 && length(classcolor) != 1 && !is.na(classcolor))
     stop("'classcolor' input is incorrect, it should be in an amount equal to the number of classes in 'class'. Verify!")
  
  MaxX <- max(coorI[,1],coorV[,1]) + 1 # Dimenssoes maximas das linhas
  MinX <- min(coorI[,1],coorV[,1]) - 1 # Dimenssoes minimas das linhas
  MaxY <- max(coorI[,2],coorV[,2]) + 1 # Dimenssoes maximas das colunas
  MinY <- min(coorI[,2],coorV[,2]) - 1 # Dimenssoes minimas das colunas
  
  ##### INICIO - Grafico Biplot #####  
  
  if (savptc) png(filename = "Figure_Biplot.png", width = width, height = height, res = res) # salva os graficos em arquivos
  
  plot(0,0, # Plota as variaveis
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = title,  # Titulo
       type = "n",    # nao plota pontos
       xlim = c(MinX,MaxX), # Dimensao para as linhas do grafico
       ylim = c(MinY,MaxY)) # Dimensao para as colunas do grafico
  
  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  if (axes) abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  if (var) {
     arrows(0,0,coorV[,1],coorV[,2], lwd = 1, code = 2, length = 0.08, angle = 25, col = ifelse(color==TRUE,"Red","Black")) # cria a seta apontando para cada variavel  
    
     NomeVar <- colnames(Mdata) # nomes das variaveis
     
     LocLab(coorV[,1:2], NomeVar, col = ifelse(color,"Blue","Black"))  # Coloca os nomes das variaveis
  }
  
  if (!is.na(classcolor[1])) {
     cor.classe <- classcolor
  }
  else { cor.classe <- c("red") }
  
  if (obs) {
    
    NomeVar <- LinNames # nomes das observacoes
    
    if (Num.class == 0) {
      
      points(coorI,      # coloca pontos nas posicoes dos individuos
             pch = 15,   # formato dos pontos 
             cex = size, # tamanho dos pontos         
             col = ifelse(color, cor.classe, "Black"))
      
    } else {
      
      cor <- 1 # cor inicial dos pontos e legendas
      
      Init.Form <- 14 # formato inicial dos pontos
      
      for (k in 1:Num.class) {
        
        Point.Form <- Init.Form + k # fomato dos pontos de cada classe
        
        if (!is.na(classcolor[1])) {
           cor1 <- ifelse(color, cor.classe[k], "black")
        }
        else { cor1 <- ifelse(color, cor + k, "black") }
        
        Point.data <- coorI[which(class == class.Names[k]),]
        
        points(Point.data,
               pch = Point.Form, # formato dos pontos
               cex = size, # tamanho dos pontos  
               col = cor1) # adiciona ao grafico as coordenadas principais das colunas
      }
      
    }
    
    if (!is.na(NomeVar[1]))
       LocLab(coorI[,1:2], NomeVar, col = "Black") # Coloca os nomes dos individuos
    
    if (posleg != 0 && Num.class > 0) {
      
       Init.Form <- 15 # codigo formato ponto inicial
      
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
    
  }
  
  ##### FIM - Grafico Biplot #####
  
  if (savptc) {
     box(col = 'white') 
     dev.off()
     message("\n \n End!")
  }
  
  Lista <- list(Md = Md, Mu = Mu, Mv = Mv, coorI = coorI,
                coorV = coorV, pvar = pvar)
  
  return (Lista) 
  
}