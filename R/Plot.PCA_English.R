Plot.PCA <- function(PC, titles = NA, xlabel = NA, ylabel = NA, size = 1.1,
                     grid = TRUE, color = TRUE, linlab = NA, casc = TRUE) {
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
  # linlab - Vetor com os rotulos das observacoes
  # casc   - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Varios graficos
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  # Cria Titulos para os graficos caso nao existam
  if (!is.character(titles[1]) || is.na(titles[1])) titles[1] = c("Scree-plot of the components variances")
  if (!is.character(titles[2]) || is.na(titles[2])) titles[2] = c("Graph corresponding to the rows (observations)")
  if (!is.character(titles[3]) || is.na(titles[3])) titles[3] = c("Graph corresponding to the columns (variables)")
  
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

  if (!is.na(linlab[1]) && length(linlab)!=nrow(PC$mtxscores))
     stop("'linlab' input is incorrect, it should have the same number of rows as the input in the database. Verify!")
  
  if (!is.logical(casc))
     stop("'casc' input is incorrect, it should be TRUE or FALSE. Verify!")
 
  if (is.na(xlabel[1]))
     xlabel = paste("First coordinate (",round(PC$mtxAutvlr[1,2],2),"%)",sep="")
  
  if (is.na(ylabel[1]))
     ylabel = paste("Second coordinate (",round(PC$mtxAutvlr[2,2],2),"%)",sep="")
  #####   FIM - Informacoes usadas nos Graficos  #####
  
  if (casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos Autovalores #####
  mp <- barplot(PC$mtxAutvlr[,1],names.arg=paste(round(PC$mtxAutvlr[,2],2),"%",sep=""),
                main = "Variance of the components")
  ##### FIM - Plotagem dos Autovalores #####
  
  if (casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Scree-plot dos componentes #####
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
  ##### FIM - Scree-plot dos componentes #####
  
  if (casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos Dados das linhas #####
  plot(PC$mtxscores, # cria grafico para as coordenadas principais das linhas
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       type = "n", # nao plota pontos
       main = titles[2], # Titulo
       # asp  = 1,  # Aspecto do Grafico
       xlim = c(min(PC$mtxscores[,1])-0.05,max(PC$mtxscores[,1])+0.05), # Dimensao para as linhas do grafico
       ylim = c(min(PC$mtxscores[,2])-0.05,max(PC$mtxscores[,2])+0.05)) # Dimensao para as colunas do grafico
  
  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray95','gray95'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 1, lty = 7, equilogs = T)
    
  }
  
  points(PC$mtxscores, # cria grafico para as coordenadas principais das linhas
         pch = 15,   # Formato dos pontos 
         cex = size, # Tamanho dos pontos  
         col = ifelse(color,"red","black"))  # Cor dos pontos
  
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  if (!is.na(linlab[1])) LocLab(PC$mtxscores, cex = 1, linlab)
  ##### FIM - Plotagem dos Dados das linhas #####
  
  if (casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem das Correlacoes dos Componentes Principais com as Variaveis Originais #####
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
  
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  arrows(0,0,PC$mtxCCP[1,],PC$mtxCCP[2,], lty=1, code = 2, length = 0.08, angle = 25, col = ifelse(color,"Red","Black")) # cria a seta apontando para cada coordenada principal
  
  LocLab(t(PC$mtxCCP), cex=1, colnames(PC$mtxCCP) , col = ifelse(color,"Blue","Black"), xpd = TRUE)
  ##### FIM - Plotagem das Correlacoes dos Componentes Principais com as Variaveis Originais #####
}