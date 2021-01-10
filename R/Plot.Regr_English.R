Plot.Regr <- function(Reg, typegraf = "Scatterplot", title = NA, xlabel = NA, 
                      ylabel = NA, namevary = NA, namevarx = NA, size = 1.1, 
                      grid = TRUE, color = TRUE, intconf = TRUE, intprev = TRUE,
                      savptc = FALSE, width = 3236, height = 2000, res = 300, 
                      casc = TRUE) {
  # Esta funcao gera graficos da Analise de Regressao
  # desenvolvida por Paulo Cesar Ossani em 06/2016
  
  # Entrada:
  # Reg      - Dados da funcao de regressao.
  # typegraf - Tipo de grafico: 
  #            "Scatterplot" - Grafico de dispersao 2 a 2,
  #            "Regression"  - Grafico da regressao linear,
  #            "QQPlot"      - Grafico de probabilidade normal dos residuos,
  #            "Histogram"   - Histograma dos residuos,
  #            "Fits"        - Grafico dos valores ajustados versus os residuos,
  #            "Order"       - Grafico da ordem das observacoes versus os residuos.
  # title    - Titulos para os graficos, se nulo retorna padrao.
  # xlabel   - Nomeia o eixo X, se nulo retorna padrao.
  # ylabel   - Nomeia o eixo Y, se nulo retorna padrao.
  # namevary - Nome da variavel Y, se nulo retorna padrao.
  # namevarx - Nome da variavel, ou variaveis X, se nulo retorna padrao.
  # size     - Tamanho dos pontos nos graficos.
  # grid     - Coloca grade nos graficos.
  # color    - Graficos coloridos (default = TRUE).
  # intconf  - Caso typegraf = "Regression":
  #            Graficos com intervalo de confianca (default = TRUE).
  # intprev  - Caso typegraf = "Regression":
  #            Graficos com intervalo de previsao (default = TRUE).
  # savptc   - Salva as imagens dos graficos em arquivos (default = FALSE).
  # width    - Largura do grafico quanto savptc = TRUE (defaul = 3236).
  # height   - Altura do grafico quanto savptc = TRUE (default = 2000).
  # res      - Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).
  # casc     - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Varios graficos.

  Graphic <- c("Scatterplot", "Regression", "QQPlot", "Histogram", "Fits", "Order")
  if (is.na(pmatch(typegraf, Graphic))) 
     stop("'typegraf' input is incorrect, it should be: 'Scatterplot', 
          'Regression', 'QQPlot', 'Histogram', 'Fits' ou 'Order'. Verify!")

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

  if (!is.logical(intconf))
     stop("'intconf' input is incorrect, it should be TRUE or FALSE. Verify!")

  if (!is.logical(intprev))
     stop("'intprev' input is incorrect, it should be TRUE or FALSE. Verify!")
 
  if (Reg$intercepts) X <- as.matrix(Reg$X[,2:ncol(Reg$X)]) else X <- as.matrix(Reg$X)
  
  if (is.na(namevary[1]))
     namevary <- c("Y")
  
  if (is.na(namevarx[1]))
     namevarx <- c(paste("X",1:ncol(X),sep=""))
   
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
  
  if (savptc) {
     cat("\014") # limpa a tela
     cat("\n\n Saving graphics to hard disk. Wait for the end!")
  }
  
  ## Inicio - Scatterplot
  if (typegraf == "Scatterplot") {
    
     if (is.na(title[1]))
        title = c("Scatterplot 2 to 2")
     
     if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
     
     if (savptc) png(filename = "Figure Regression Scatterplot.png", width = width, height = height, res = res) # salva os graficos em arquivos
     
     Dat <- as.data.frame(cbind(Reg$Y,X))
     colnames(Dat) <- c(namevary,namevarx)
     if (color) cor <- c(2:(ncol(X)+1)) else cor <- c(rep("black", ncol(X)))
     pairs(Dat, # Scatterplot
           main = title, # Titulo
           pch  = 21,    # Formato dos pontos 
           cex  = size,  # Tamanho dos pontos
           bg   = cor)
     
     if (savptc) { box(col = 'white'); dev.off() }
  }
  ## Fim - Scatterplot
  
  ## Inicio - Grafico da regressao
  if (typegraf == "Regression") {
    if (ncol(X)!=1) 
       print("Warning! The regression graph is only possible for a regressor variable.")
    
    if (ncol(X)==1) { # para calculos de regressao simples
      
      if (savptc) png(filename = "Figure Regression Simples.png", width = width, height = height, res = res) # salva os graficos em arquivos
      
      if (is.na(xlabel[1]))
         xlabel = "X-axis"  # Nomeia Eixo X  
      
      if (is.na(ylabel[1]))
         ylabel = "Y-axis"  # Nomeia Eixo Y
      
      if (is.na(title[1]))
         title = c("Linear regression graph")
      
      Y <- Reg$Y
      
      X <- as.numeric(X)
      
      if (Reg$intercepts) Modelo <- lm(Y~X) else Modelo <- lm(Y~-1+X)
      
      if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
      
      # Intervalo para abcissa
      Inter <- ifelse(length(Reg$Y)<100,150,length(Reg$Y)*1.5) # intervalo para o eixo X
      New.X <- data.frame(X=seq(min(X), max(X), by=((max(X)-min(X))/Inter)))
      
      # Predicao
      Predicao  <- predict(Modelo, New.X)
      
      # Intervalo de confianca
      Inter.Conf <- predict(Modelo, New.X, level=0.95, interval=c("confidence"))
      
      # Intervalo de Predicao
      Inter.Pred <- predict(Modelo, New.X, level=0.95, interval=c("prediction"))  
      
      plot(X,Reg$Y, # cria grafico
           xlab = xlabel, # Nomeia Eixo X
           ylab = ylabel, # Nomeia Eixo Y
           type = "n", # nao plota pontos
           main = title,  # Titulo
           xlim = c(min(X)-0.1,max(X)+0.1), # Dimensao para as linhas do grafico
           ylim = c(min(cbind(Predicao,Inter.Conf[,2:3],Inter.Pred[,2:3])),
                    max(cbind(Predicao,Inter.Conf[,2:3],Inter.Pred[,2:3]))+0.1)) # Dimensao para as colunas do grafico
      
      if (grid) {
        
         args <- append(as.list(par('usr')), c('gray93','gray93'))
        
         names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
        
         do.call(rect, args) # chama a funcao rect com os argumentos (args)
        
         grid(col = "white", lwd = 2, lty = 7, equilogs = T)
        
      }
      
      points(X,Reg$Y, # cria grafico
             pch = 15, # Formato dos pontos 
             cex = size,  # Tamanho dos pontos  
             col = ifelse(color,"red","black")) # Cor dos pontos
      
      ## Inicio - Acrescenta a reta ajustada
      lines(cbind(New.X, Predicao)) # acrescenta a reta ajustada
      ## Fim - Acrescenta a reta ajustada
      
      ## Inicio - Acrescenta o Intervalo de Confianca das previsoes
      if (intconf) {
        lines(cbind(New.X,Inter.Conf[,2]), lty=3) # acrescenta I.C. Lim.Infereior
        lines(cbind(New.X,Inter.Conf[,3]), lty=3) # acrescenta I.C. Lim.Superior
      }
      ## Fim - Acrescenta o Intervalo de Confianca das previsoes
      
      ## Inicio - Acrescenta o Intervalo das previsoes
      if (intprev) {
        lines(cbind(New.X,Inter.Pred[,2]), lty=2) # acrescenta I.P. Lim.Infereior
        lines(cbind(New.X,Inter.Pred[,3]), lty=2) # acrescenta I.P. Lim.Superior
      }
      ## Fim - Acrescenta o Intervalo das previsoes
      
      if (savptc) { box(col = 'white'); dev.off() }
    }
    
  }
  ## Fim - Grafico da regressao
  
  ## Inicio - Grafico da probalidade normal
  if (typegraf == "QQPlot") {
    
    if (savptc) png(filename = "Figure Regression QQPlot.png", width = width, height = height, res = res) # salva os graficos em arquivos
    
    if (is.na(xlabel[1]))
       xlabel = "Quantiles"  # Nomeia Eixo X  
    
    if (is.na(ylabel[1]))
       ylabel = "Sample of the quantiles"  # Nomeia Eixo Y
    
    if (is.na(title[1]))
       title = c("Graph of the normal probability\n of the residue")
    
    if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
    
    qqnorm(Reg$error,
           xlab = xlabel, # Nomeia Eixo X
           ylab = ylabel, # Nomeia Eixo Y
           main = title,  # Titulo
           pch  = 19,     # Formato dos pontos 
           bty  = ifelse(grid,"l","o"),
           panel.first = grid(col = ifelse(grid,"gray","white"), lwd = 1, lty = 7, equilogs = T),
           cex = size)    # Tamanho dos pontos 
    qqline(Reg$error, col = ifelse(color,"red","black"))
    
    if (savptc) { box(col = 'white'); dev.off() }
  }
  ## Fim - Grafico da probalidade normal
  
  ## Inicio - Grafico da probalidade normal
  if (typegraf == "Histogram") {
    
    if (savptc) png(filename = "Figure Regression Histogram.png", width = width, height = height, res = res) # salva os graficos em arquivos
    
    if (is.na(xlabel[1]))
      xlabel = "Residue"  # Nomeia Eixo X  
    
    if (is.na(ylabel[1]))
      ylabel = "Frequence"  # Nomeia Eixo Y
    
    if (is.na(title[1]))
      title = c("Residue Histogram")
    
    if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
    
    hist(Reg$error,
         xlab = xlabel, # Nomeia Eixo X
         ylab = ylabel, # Nomeia Eixo Y
         main = title,  # Titulo
         pch  = 19,     # Formato dos pontos 
         cex = size)    # Tamanho dos pontos
    
    if (savptc) { box(col = 'white'); dev.off() }
  }
  ## Fim - Grafico da probalidade normal
  
  ## Inicio - Grafico dos valores ajustados com os residuos
  if (typegraf == "Fits") {
    
    if (savptc) png(filename = "Figure Regression Fits.png", width = width, height = height, res = res) # salva os graficos em arquivos
    
    if (is.na(xlabel[1]))
      xlabel = "Fits values"  # Nomeia Eixo X  
    
    if (is.na(ylabel[1]))
      ylabel = "Residue"  # Nomeia Eixo Y
    
    if (is.na(title[1]))
      title = c("Fits values vs. residues")
    
    if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
    
    plot(Reg$prev,Reg$error, # cria grafico
         xlab = xlabel, # Nomeia Eixo X
         ylab = ylabel, # Nomeia Eixo Y
         type = "n",    # nao plota pontos
         main = title)  # Titulo
    
    if (grid) {
      
       args <- append(as.list(par('usr')), c('gray93','gray93'))
      
       names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
      
       do.call(rect, args) # chama a funcao rect com os argumentos (args)
      
       grid(col = "white", lwd = 2, lty = 7, equilogs = T)
       
    }
    
    points(Reg$prev,Reg$error, # cria grafico
           pch = 19, # Formato dos pontos 
           cex = size,  # Tamanho dos pontos 
           col = ifelse(color,"red","black"))  # Cor dos pontos
    
    abline(0,0, lty = 2) # acrescenta a reta do eixo X
    
    if (savptc) { box(col = 'white'); dev.off() }
  }
  ## Fim - Grafico dos valores ajustados com os residuos
  
  ## Inicio - Grafico com ordem das observacoes versus os residuos
  if (typegraf == "Order") {
    
    if (savptc) png(filename = "Figure Regression Order.png", width = width, height = height, res = res) # salva os graficos em arquivos
    
    if (is.na(xlabel[1]))
       xlabel = "Order of the observations"  # Nomeia Eixo X  
    
    if (is.na(ylabel[1]))
       ylabel = "Residue" # Nomeia Eixo Y
    
    if (is.na(title[1]))
       title = c("Order of the observations\n vs. residues")
    
    if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
    
    plot(1:length(Reg$error),Reg$error, # cria grafico
         xlab = xlabel, # Nomeia Eixo X
         ylab = ylabel, # Nomeia Eixo Y
         type = "n",    # nao plota pontos
         main = title)  # Titulo
    
    if (grid) {
      
       args <- append(as.list(par('usr')), c('gray93','gray93'))
      
       names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
      
       do.call(rect, args) # chama a funcao rect com os argumentos (args)
      
       grid(col = "white", lwd = 2, lty = 7, equilogs = T)
      
    }
    
    points(1:length(Reg$error),Reg$error, # cria grafico
           type = "o", # linhas com pontos
           pch  = 19,  # Formato dos pontos 
           cex  = size,  # Tamanho dos pontos  
           col  = ifelse(color,"blue","black")) # Cor dos pontos
    
    abline(0,0, lty = 2) # acrescenta a reta do eixo X
    
    if (savptc) { box(col = 'white'); dev.off() }
    
  }
  ## Fim - Grafico com as ordem das observacoes versus os residuos
  
  if (savptc) cat("\n \n End!")
}