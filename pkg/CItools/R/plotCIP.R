#################################################################
## Plot of CIP 
plotCIP = function(cip, type = c('index', 'ranking'), mc = NULL, ...){
  nams = rownames(cip$index)
  n = length(nams)
  xlim = c(1,n)
  if (type[1] == 'index'){
    x = cip$index
    ylim = range( x , na.rm = TRUE)
    if (!is.null(mc)){
      y = mc$indexMC
      ylim = range(c(ylim, range(y, na.rm = TRUE)))
    }
    ord = order(x, na.last = TRUE, decreasing = TRUE)
  }
  else if (type[1] == 'ranking'){
    x = cip$ranking
    ylim = range(x, na.rm = TRUE)
    if (!is.null(mc)){
      y = mc$rankingMC
      ylim = range(c(ylim, range(y, na.rm = TRUE)))
    }
    ord = order(x, na.last = TRUE, decreasing = FALSE)
  }
  else{
    stop ('type not well defined')
  }
  
  plot(1:n, x[ord], pch = 20, xlim = xlim, ylim = ylim, xlab = "country", ylab = type[1], axes = FALSE, ...)
  axis(side = 1, at = c(1:n)[seq(1,length(ord),length.out=10)], labels = nams[ord[seq(1,length(ord),length.out=10)]], las = 2)
  axis(side = 2, las = 1)
  box()
  if (!is.null(mc)){
    par(new = TRUE)
    boxplot(t(y[ord,]), xlim = xlim, ylim = ylim, axes = FALSE, xlab = "country", ylab = type[1], las = 1, range=0)
  }
  lines(1:n, x[ord], pch = 20,   ...)  
}