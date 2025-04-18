Plot.CA <- function(CA, titles = NA, xlabel = NA, ylabel = NA,
                    size = 1.1, grid = TRUE, color = TRUE, linlab = NA,
                    axes = TRUE, savptc = FALSE, width = 3236, 
                    height = 2000, res = 300, casc = TRUE) {
  # Rotina para Plotar Graficos do Metodo AC desenvolvida 
  # por Paulo Cesar Ossani em 11/2014
  
  # Entrada:
  # CA     - Dados da funcao CA.
  # titles - Titulos para os graficos.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # size   - Tamanho dos pontos nos graficos.
  # grid   - Coloca grade nos graficos.
  # color  - Graficos coloridos (default = TRUE).
  # linlab - Vetor com os rotulos para as observacoes.
  # axes   - Coloca eixos no grafico (default = TRUE).
  # savptc - Salva as imagens dos graficos em arquivos (default = FALSE).
  # width  - Largura do grafico quanto savptc = TRUE (defaul = 3236).
  # height - Altura do grafico quanto savptc = TRUE (default = 2000).
  # res    - Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).
  # casc   - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Varios graficos
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  # Cria Titulos para os graficos caso nao existam
  if (!is.character(titles[1]) || is.na(titles[1])) titles[1] = c("Scree-plot of the components variances")
  if (!is.character(titles[2]) || is.na(titles[2])) titles[2] = c("Graph corresponding to the rows (observations)")
  if (!is.character(titles[3]) || is.na(titles[3])) titles[3] = c("Graph corresponding to the columns (variables)")
  if (!is.character(titles[4]) || is.na(titles[4])) titles[4] = c("Graph corresponding to observations and variables")
  
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
  
  if (!is.na(linlab[1]) && length(linlab)!=nrow(CA$mtxX) && CA$typdata=="F")
     stop("'linlab' input is incorrect, it should have the same number of rows as the input in the database. Verify!")
  
  if (!is.logical(savptc))
     stop("'savptc' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.numeric(width) || width <= 0)
     stop("'width' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.numeric(height) || height <= 0)
     stop("'height' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.numeric(res) || res <= 0)
     stop("'res' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.logical(casc))
     stop("'casc' input is incorrect, it should be TRUE or FALSE. Verify!")

  if (is.na(xlabel[1]))
     xlabel = paste("First coordinate (",round(CA$mtxAutvlr[1,2],2),"%)",sep="")
  
  if (is.na(ylabel[1]))
     ylabel = paste("Second coordinate (",round(CA$mtxAutvlr[2,2],2),"%)",sep="")
  
  #####   FIM - Informacoes usadas nos Graficos  #####
  
  if (savptc) {
     cat("\014") # limpa a tela
     cat("\n\n Saving graphics to hard disk. Wait for the end!")
  }
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos Autovalores #####
  if (savptc) png(filename = "Figure_CA_Variances.png", width = width, height = height, res = res) # salva os graficos em arquivos
  
  mp <- barplot(CA$mtxAutvlr[,1],names.arg=paste(round(CA$mtxAutvlr[,2],2),"%",sep=""),
                main = "Component variances")
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Plotagem dos Autovalores #####
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Scree-plot dos componentes #####
  if (savptc) png(filename = "Figure_CA_Scree_Plot.png", width = width, height = height, res = res) # salva os graficos em arquivos
  
  plot(1:length(CA$mtxAutvlr[,1]), CA$mtxAutvlr[,1], 
       type = "n", # nao plota pontos
       xlab = "Component order", 
       ylab = "Variance",
       xaxt = "n", # tira o eixo x
       main = titles[1])
  
  axis(1, c(1:length(CA$mtxAutvlr[,1])), c(1:length(CA$mtxAutvlr[,1])))
  
  if (grid) {
    
    args <- append(as.list(par('usr')), c('gray93','gray93'))
    
    names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
    do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
    grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  points(1:length(CA$mtxAutvlr[,1]), CA$mtxAutvlr[,1], type = "b")
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Scree-plot dos componentes #####  
  
  ##### INICIO - Plotagem dos Dados das linhas #####
  if (CA$typdata == "F") { # plota se nao for analise de correspondencia multipla
    
    if (savptc) png(filename = "Figure_CA_Observations.png", width = width, height = height, res = res) # salva os graficos em arquivos
    
    if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
    
    plot(0, # cria grafico para as coordenadas principais das linhas
         xlab = xlabel, # Nomeia Eixo X
         ylab = ylabel, # Nomeia Eixo Y
         main = titles[2], # Titulo
         type = "n", # nao plota pontos 
         xlim = c(min(CA$mtxX[,1])-0.1,max(CA$mtxX[,1])+0.1), # Dimensao para as linhas do grafico
         ylim = c(min(CA$mtxX[,2]-0.1),max(CA$mtxX[,2])+0.1)) # Dimensao para as colunas do grafico
    
    if (grid) {
      
      args <- append(as.list(par('usr')), c('gray93','gray93'))
      
      names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
      
      do.call(rect, args) # chama a funcao rect com os argumentos (args)
      
      grid(col = "white", lwd = 2, lty = 7, equilogs = T)
      
    }
    
    points(CA$mtxX, # cria grafico para as coordenadas principais das linhas
           pch = 17, # Formato dos pontos 
           cex = size,  # Tamanho dos pontos  
           col = ifelse(color,"red","black")) # Cor dos pontos
    
    if (axes) abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
    
    if (!is.na(linlab[1])) LocLab(CA$mtxX,cex=1, linlab)
    
    if (savptc) { box(col = 'white'); dev.off() }
  }
  
  ##### FIM - Plotagem dos Dados das linhas #####
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem Dados das colunas #####
  if (savptc) png(filename = "Figure_CA_Variables.png", width = width, height = height, res = res) # salva os graficos em arquivos
  
  plot(0, # cria grafico para as coordenadas principais das linhas
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = titles[3], # Titulo
       type = "n", # nao plota pontos 
       xlim = c(min(CA$mtxY[,1])-0.1,max(CA$mtxY[,1])+0.1), # Dimensao para as linhas do grafico
       ylim = c(min(CA$mtxY[,2]-0.1),max(CA$mtxY[,2])+0.1)) # Dimensao para as colunas do grafico
  
  if (grid) {
    
    args <- append(as.list(par('usr')), c('gray93','gray93'))
    
    names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
    do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
    grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  points(CA$mtxY, # cria grafico para as coordenadas principais das linhas
         pch = ifelse(CA$typdata=="C",17,16), # Formato dos pontos 
         cex = size,  # Tamanho dos pontos  
         col = ifelse(color,ifelse(CA$typdata=="C","red","blue"),"black"))             # Cor dos pontos
  
  if (axes) abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  LocLab(CA$mtxY, cex=1, rownames(CA$mtxY))
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Plotagem Dados das colunas #####
  
  ##### INICIO - Plotagem dos Dados das linhas e colunas conjuntamente #####
  if (CA$typdata == "F") { # plota se nao for analise de correspondencia multipla
    
     if (savptc) png(filename = "Figure_CA_Variables_Observations.png", width = width, height = height, res = res) # salva os graficos em arquivos
    
     if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
    
     plot(0,    # cria grafico para as coordenadas principais das linhas
          xlab = xlabel, # Nomeia Eixo X
          ylab = ylabel, # Nomeia Eixo Y
          type = "n", # nao plota pontos 
          xlim = c(min(CA$mtxX[,1],CA$mtxY[,1])-0.1,max(CA$mtxX[,1],CA$mtxY[,1])+0.1), # Dimensao para as linhas do grafico
          ylim = c(min(CA$mtxX[,2],CA$mtxY[,2])-0.1,max(CA$mtxX[,2],CA$mtxY[,2])+0.1)) # Dimensao para as colunas do grafico
    
     if (grid) {
      
        args <- append(as.list(par('usr')), c('gray93','gray93'))
      
        names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
      
        do.call(rect, args) # chama a funcao rect com os argumentos (args)
      
        grid(col = "white", lwd = 2, lty = 7, equilogs = T)
      
     }
    
     points(CA$mtxX, # cria grafico para as coordenadas principais das linhas
            pch = 17,   # Formato dos pontos 
            cex = size, # Tamanho dos pontos  
            col = ifelse(color,"red","black")) # Cor dos pontos
    
     points(CA$mtxY, pch = 16, cex = size, col = ifelse(color,"blue","black")) # adiciona ao grafico as coordenadas principais das colunas
    
     if (axes) abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
    
     if (!is.na(linlab[1])) LocLab(rbind(CA$mtxX[,1:2], CA$mtxY[,1:2]), cex=1, rbind(as.matrix(linlab), as.matrix(rownames(CA$mtxY))))
  
     if (savptc) { box(col = 'white'); dev.off() }
  }
  
  ##### FIM - Plotagem dos Dados das linhas e colunas conjuntamente #####
  
  if (savptc) cat("\n \n End!")
}