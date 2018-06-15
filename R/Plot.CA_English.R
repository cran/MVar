Plot.CA <- function(CA, Titles = NA, xlabel = NA, ylabel = NA,
                    Color = TRUE, LinLab = NA, Casc = TRUE) {
  # Rotina para Plotar Graficos do Metodo CA desenvolvida 
  # por Paulo Cesar Ossani em 11/2014
  
  # Entrada:
  # CA     - Dados da funcao CA.
  # Titles - Titulos para os graficos.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # Color  - Graficos coloridos (default = TRUE).
  # LinLab - Vetor com o rotulo para as linhas para dados de frequencia,
  #          se nao informado retorna padrao.
  # Casc    - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Varios graficos
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  # Cria Titulos para os graficos caso nao existam
  if (!is.character(Titles[1]) || is.na(Titles[1])) Titles[1] = c("Scree-plot of the components variances")
  if (!is.character(Titles[2]) || is.na(Titles[2])) Titles[2] = c("Graph corresponding to the rows (observations)")
  if (!is.character(Titles[3]) || is.na(Titles[3])) Titles[3] = c("Graph corresponding to the columns (variables)")
  if (!is.character(Titles[4]) || is.na(Titles[4])) Titles[4] = c("Graph corresponding to observations and variables")
  
  if (!is.character(xlabel) && !is.na(xlabel))
     stop("'xlabel' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.character(ylabel) && !is.na(ylabel))
     stop("'ylabel' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.logical(Color))
     stop("'Color' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.na(LinLab) && length(LinLab)!=nrow(CA$MatrixX) && CA$TypData=="F")
     stop("'LinLab' input is incorrect, it should have the same number of rows as the input in the database. Verify!")
  
  if (!is.logical(Casc))
     stop("'Casc' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (is.na(LinLab) && CA$TypData=="F")
     LinLab <- rownames(CA$MatrixX)
  
  if (is.na(xlabel))
     xlabel = paste("First coordinate (",round(CA$MatrixAutoVlr[1,2],2),"%)",sep="")
  
  if (is.na(ylabel))
     ylabel = paste("Second coordinate (",round(CA$MatrixAutoVlr[2,2],2),"%)",sep="")
  
  #####   FIM - Informacoes usadas nos Graficos  #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos Autovalores #####
  mp <- barplot(CA$MatrixAutoVlr[,1],names.arg=paste(round(CA$MatrixAutoVlr[,2],2),"%",sep=""),main = "Variance of the components")
  ##### FIM - Plotagem dos Autovalores #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Scree-plot dos componentes #####
  plot(1:length(CA$MatrixAutoVlr[,1]), CA$MatrixAutoVlr[,1], type = "b", 
       xlab = "Order of the components", 
       ylab = "Variance",
       main = Titles[1])
  ##### FIM - Scree-plot dos componentes #####
  
  ##### INICIO - Plotagem dos Dados das linhas #####
  if (CA$TypData=="F") { # plota se nao for analise de correspondencia multipla
    
    if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
    
    plot(CA$MatrixX, # cria grafico para as coordenadas principais das linhas
         xlab = xlabel, # Nomeia Eixo X
         ylab = ylabel, # Nomeia Eixo Y
         main = Titles[2], # Titulo
         asp  = 1,  # Aspecto do Grafico
         pch  = 17, # Formato dos pontos 
         cex  = 1,  # Tamanho dos pontos
         xlim = c(min(CA$MatrixX[,1])-0.1,max(CA$MatrixX[,1])+0.1), # Dimensao para as linhas do grafico
         ylim = c(min(CA$MatrixX[,2]-0.1),max(CA$MatrixX[,2])+0.1), # Dimensao para as colunas do grafico
         col  = ifelse(Color,"red","black")) # Cor dos pontos
    
    abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
    
    #text(CA$MatrixX,cex=1, pos=3, LinLab)  # Coloca os nomes dos pontos das coordenadas principais das linhas
    LocLab(CA$MatrixX,cex=1, LinLab)
  }
  ##### FIM - Plotagem dos Dados das linhas #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem Dados das colunas #####
  plot(CA$MatrixY, # cria grafico para as coordenadas principais das linhas
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Titles[3], # Titulo
       asp  = 1,   # Aspecto do Grafico
       pch  = ifelse(CA$TypData=="C",17,16), # Formato dos pontos 
       cex  = 1.2, # Tamanho dos pontos
       xlim = c(min(CA$MatrixY[,1])-0.1,max(CA$MatrixY[,1])+0.1), # Dimensao para as linhas do grafico
       ylim = c(min(CA$MatrixY[,2]-0.1),max(CA$MatrixY[,2])+0.1), # Dimensao para as colunas do grafico
       col  = ifelse(Color,ifelse(CA$TypData=="C","red","blue"),"black"))             # Cor dos pontos
  
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  #text(CA$MatrixY, cex=1, pos=3, rownames(CA$MatrixY))  # Coloca os nomes dos pontos das coordenadas principais das colunas
  LocLab(CA$MatrixY, cex=1, rownames(CA$MatrixY))
  ##### FIM - Plotagem Dados das colunas #####
  
  ##### INICIO - Plotagem dos Dados das linhas e colunas conjuntamente #####
  if (CA$TypData=="F") { # plota se nao for analise de correspondencia multipla
    
     if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
    
     plot(CA$MatrixX,    # cria grafico para as coordenadas principais das linhas
          xlab = xlabel, # Nomeia Eixo X
          ylab = ylabel, # Nomeia Eixo Y
          main = Titles[4], # Titulo
          asp  = 1,  # Aspecto do Grafico
          pch  = 17, # Formato dos pontos 
          cex  = 1,  # Tamanho dos pontos
          xlim = c(min(CA$MatrixX[,1],CA$MatrixY)-0.1,max(CA$MatrixX[,1],CA$MatrixY)+0.1), # Dimensao para as linhas do grafico
          ylim = c(min(CA$MatrixX[,2],CA$MatrixY)-0.1,max(CA$MatrixX[,2],CA$MatrixY)+0.1), # Dimensao para as colunas do grafico
          col  = ifelse(Color,"red","black")) # Cor dos pontos
    
     points(CA$MatrixY, pch = 16, cex = 1.2, col = ifelse(Color,"blue","black")) # adiciona ao grafico as coordenadas principais das colunas
    
     abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
    
     #text(CA$MatrixX, cex=1,  pos=3, LinLab)  # Coloca os nomes dos pontos das coordenadas principais das linhas
     #LocLab(CA$MatrixX, cex=1,LinLab)
    
     #text(CA$MatrixY, cex=1, pos=3, rownames(CA$MatrixY))  # Coloca os nomes dos pontos das coordenadas principais das colunas
     #LocLab(CA$MatrixY, cex=1, rownames(CA$MatrixY))

     LocLab(rbind(CA$MatrixX[,1:2],CA$MatrixY[,1:2]), cex=1, rbind(as.matrix(LinLab),as.matrix(rownames(CA$MatrixY))))
  }
  ##### FIM - Plotagem dos Dados das linhas e colunas conjuntamente #####
}