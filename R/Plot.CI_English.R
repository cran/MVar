Plot.CI <- function(ci, title = NA, xlabel = NA, ylabel = NA, size = 0.9, 
                    grid = TRUE, color = TRUE, savptc = FALSE, width = 3236,
                    height = 2000, res = 300, casc = FALSE) {
  
  # Rotina para Plotar Grafcios para Intervalos de Confianca 
  # por Paulo Cesar Ossani em 20/06/2026
  
  # Entrada:
  # ci     - Dados da funcao CI.
  # title  - Titulo para o grafcio.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # size   - Tamanho dos pontos nos grafcios.
  # grid   - Coloca grade nos grafcios.
  # color  - Grafcio colorido (default = TRUE).
  # savptc - Salva a imagem do grafcio em arquivo (default = FALSE).
  # width  - Largura do grafcio quanto savptc = TRUE (defaul = 3236).
  # height - Altura do grafcio quanto savptc = TRUE (default = 2000).
  # res    - Resolucao nominal em ppi do grafcio quanto savptc = TRUE (default = 300).
  # casc   - Efeito cascata na apresentacao do grafcio (default = TRUE).
  
  # Retorna:
  # Grafcio os intervalos
  
  if (!is.character(title[1]) || is.na(title[1]))
     title <- c("Confidence Interval")
  
  if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("Invalid input for 'xlabel': must be of type character or string. Check it!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("Invalid input for 'ylabel': must be of type character or string. Check it!")
  
  if (!is.logical(color))
     stop("Invalid input for 'color': must be TRUE or FALSE. Check it!")
  
  if (!is.numeric(size) || size < 0)
     stop("Invalid input for 'size': must be numeric and greater than zero. Check it!")
  
  if (!is.logical(grid))
     stop("Invalid input for 'grid': must be TRUE or FALSE. Check it!")
  
  if (!is.logical(savptc))
     stop("Invalid input for 'savptc': must be TRUE or FALSE. Check it!")
  
  if (!is.numeric(width) || width <= 0)
    stop("Invalid input for 'width': must be numeric and greater than zero. Check it!")
  
  if (!is.numeric(height) || height <= 0)
     stop("Invalid input for 'height': must be numeric and greater than zero. Check it!")
  
  if (!is.numeric(res) || res <= 0)
     stop("Invalid input for 'res': must be numeric and greater than zero. Check it!")
  
  if (!is.logical(casc))
     stop("Invalid input for 'casc': must be TRUE or FALSE. Check it!")
  
  if (is.na(xlabel[1])) xlabel <- "Intervals"
  
  if (is.na(ylabel[1])) ylabel <- "Variables"
  
  if (savptc) {
     message("\014") # limpa a tela
     message("\n\n Saving graphs to disk. Please wait until completion!")
  }
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos grafcios
  
  if (savptc) png(filename = "Figure_Confidence_Interval.png", width = width, 
                  height = height, res = res) # salva os grafcios em arquivos
  
  y <- seq(nrow(ci)) # Posições no eixo y
  
  plot(ci[,1], y, # Cria o gráfcio vazio
       xlim = range(c(ci[,2], ci[,3])),
       ylim = c(1,nrow(ci)),
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       yaxt = "n",
       axes = F,
       type = "n",
       main = title[1])
  
  if (grid) {
     args <- append(as.list(par("usr")), c("gray93", "gray93"))
     names(args) <- c("xleft", "xright", "ybottom", "ytop", 
                      "col", "border")
     do.call(rect, args)
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
  }
  
  axis(1, at = pretty(c(ci[,2], ci[,3])), labels = pretty(c(ci[,2], ci[,3])))
  
  axis(2, at = y, labels = rownames(ci), las = 1) # Adciiona os nomes das variáveis no eixo y
  
  segments(x0 = ci[,2], y0 = y, col = ifelse(color,"blue","black"), # seguimentos dos intervalos
           x1 = ci[,3], y1 = y, lwd = size * 2)
  
  segments(x0 = ci[,2], y0 = y - 0.1, col = ifelse(color,"blue","black"), # barra lateral esquerda
           x1 = ci[,2], y1 = y + 0.1, lwd = size * 2)
  
  segments(x0 = ci[,3], y0 = y - 0.1, col = ifelse(color,"blue","black"), # barra lateral direita
           x1 = ci[,3], y1 = y + 0.1, lwd = size * 2)
  
  points(ci[,1], y, pch = 19, cex = size * 0.8, col = ifelse(color,"blue","black")) # pontos das medias
  
  if (savptc) { box(col = 'white'); dev.off() }

  if (savptc) message("\n \n Done!")
  
}