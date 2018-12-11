Plot.PCA <- function(PC, Titles = NA, xlabel = NA, ylabel = NA,
                     Color = TRUE, LinLab = NA, Casc = TRUE) {
  # Rotina para Plotar Graficos do Metodo PCA desenvolvida 
  # por Paulo Cesar Ossani em 11/2014
  
  # Entrada:
  # PC     - Dados da funcao PCA.
  # Titles - Titulos para os graficos.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # Color  - Graficos coloridos (default = TRUE).
  # LinLab - Vetor com o rotulo para as linhas, se nao
  #          informado retorna padrao.
  # Casc    - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Varios graficos
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  # Cria Titulos para os graficos caso nao existam
  if (!is.character(Titles[1]) || is.na(Titles[1])) Titles[1] = c("Scree-plot of the components variances")
  if (!is.character(Titles[2]) || is.na(Titles[2])) Titles[2] = c("Graph corresponding to the rows (observations)")
  if (!is.character(Titles[3]) || is.na(Titles[3])) Titles[3] = c("Graph corresponding to the columns (variables)")
  
  if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("'xlabel' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("'ylabel' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.logical(Color))
     stop("'Color' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.na(LinLab[1]) && length(LinLab)!=nrow(PC$MatrixEsc))
     stop("'LinLab' input is incorrect, it should have the same number of rows as the input in the database. Verify!")
  
  if (!is.logical(Casc))
     stop("'Casc' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (is.na(LinLab[1]))
     LinLab <- rownames(PC$MatrixEsc)
  
  if (is.na(xlabel[1]))
     xlabel = paste("First coordinate (",round(PC$MatrixAutoVlr[1,2],2),"%)",sep="")
  
  if (is.na(ylabel[1]))
     ylabel = paste("Second coordinate (",round(PC$MatrixAutoVlr[2,2],2),"%)",sep="")
  #####   FIM - Informacoes usadas nos Graficos  #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos Autovalores #####
  mp <- barplot(PC$MatrixAutoVlr[,1],names.arg=paste(round(PC$MatrixAutoVlr[,2],2),"%",sep=""),main = "Variance of the components")
  ##### FIM - Plotagem dos Autovalores #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Scree-plot dos componentes #####
  plot(1:length(PC$MatrixAutoVlr[,1]), PC$MatrixAutoVlr[,1], type = "b", 
       xlab = "Order of the components", 
       ylab = "Variance",
       main = Titles[1])
  ##### FIM - Scree-plot dos componentes #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos Dados das linhas #####
  plot(PC$MatrixEsc, # cria grafico para as coordenadas principais das linhas
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Titles[2], # Titulo
       asp  = 1,  # Aspecto do Grafico
       pch  = 15, # Formato dos pontos 
       cex  = 1,  # Tamanho dos pontos
       xlim = c(min(PC$MatrixEsc[,1])-0.05,max(PC$MatrixEsc[,1])+0.05), # Dimensao para as linhas do grafico
       ylim = c(min(PC$MatrixEsc[,2])-0.05,max(PC$MatrixEsc[,2])+0.05), # Dimensao para as colunas do grafico
       col  = ifelse(Color,"red","black"))  # Cor dos pontos
  
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  #text(PC$MatrixEsc, cex = 1, pos = 3, LinLab)  # Coloca os nomes dos pontos das coordenadas principais das linhas
  LocLab(PC$MatrixEsc, cex = 1, LinLab)
  ##### FIM - Plotagem dos Dados das linhas #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem das Correlacoes dos Componentes Principais com as Variaveis Originais #####
  plot(0,0, # cria grafico para as coordenadas das Correlacoes dos Componentes Principais com as Variaveis Originais
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Titles[3], # Titulo
       asp  = 1, # Aspecto do Grafico
       cex  = 0, # Tamanho dos pontos
       xlim = c(-1.1,1.1), # Dimensao para as linhas do grafico
       ylim = c(-1.1,1.1)) # Dimensao para as colunas do grafico
  
  symbols(0, 0, circles = 1, inches = FALSE, fg = 1, add = TRUE) # cria um circulo
  
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  arrows(0,0,PC$MatrixCCP[1,],PC$MatrixCCP[2,], lty=1, code = 2, length = 0.08, angle = 25, col = ifelse(Color,"Red","Black")) # cria a seta apontando para cada coordenada principal
  
  #text(t(PC$MatrixCCP), cex=1, colnames(PC$MatrixCCP) , col = ifelse(Color,"Blue","Black"), pos = 3, xpd = TRUE)  # Coloca os nomes dos pontos das coordenadas principais
  LocLab(t(PC$MatrixCCP), cex=1, colnames(PC$MatrixCCP) , col = ifelse(Color,"Blue","Black"), xpd = TRUE)
  ##### FIM - Plotagem das Correlacoes dos Componentes Principais com as Variaveis Originais #####
}