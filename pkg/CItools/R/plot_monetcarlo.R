###################################################################
#  Plot for for montecarlo
# Arg:
#  object belonging to monetcarlo class
#
# Returns:
#  Plots the graphs for montecarlo results 

plot.montecarlo <- function(x, type = c('ranking', 'index'))
{  
    type= match.arg(type)
    nams = rownames(x$index$index)
    n = length(nams)
    xlim = c(1,n)
    if (type == 'index'){
      x.axis = x$index$index
      ylim = range( x.axis , na.rm = TRUE)
        y.axis = x$indexMC
        ylim = range(c(ylim, range(y.axis, na.rm = TRUE)))
      ord = order(x.axis, na.last = TRUE, decreasing = TRUE)
    }
    else if (type == 'ranking'){
      x.axis = x$index$ranking
      ylim = range(x.axis, na.rm = TRUE)
        y.axis = x$rankingMC
        ylim = range(c(ylim, range(y.axis, na.rm = TRUE)))
      ord = order(x.axis, na.last = TRUE, decreasing = FALSE)
    }
    else{
      stop ('type not well defined')
    }
    
    plot(1:n, x.axis[ord], pch = 20, xlim = xlim, ylim = ylim, xlab = "entity", ylab = type, axes = FALSE)
    axis(side = 1, at = c(1:n)[seq(1,length(ord),length.out=n)], labels = nams[ord[seq(1,length(ord),length.out=n)]], las = 2)
    axis(side = 2, las = 1)
    box()
    par(new = TRUE)
    boxplot(t(y.axis[ord,]), xlim = xlim, ylim = ylim, axes = FALSE, xlab = "entity", ylab = type, las = 1, range=0)
    lines(1:n, x.axis[ord], pch = 20)  
  }