##General function to overlay distributions
#######################################################
#"Distrs" is a data frame or matrix with simulation results. Assumes at least 2 data columns
#"Type" is either "cumulative" or "histogram"
#Notice that this function is just a template.  More details should be added to make it more automated.  For example, axis rescaling when multiple distributions are overlayed

overlay=function(distrs, type="cumulative", verticals=T, legendloc="bottomright", bwd=.5 ,...){

  distrs=na.omit(distrs)#Removes missing values  
  if(type=="cumulative") {
    plot(ecdf(distrs[,1]), verticals=verticals)#Creates first cum plot
    for(i in 2:ncol(distrs)){#Overlays remaining cum plots
      lines(ecdf(distrs[,i]), verticals=verticals, col.hor=i, col.ver=i) 
    }
    legend(legendloc, legend=colnames(distrs), lty=1, lwd=2,col= 1:i, bty="n")
  } else
  
  if(type=="histogram") {
    if(exists(deparse(substitute(n.breaks)))==F) {n.breaks="fd"}#if no breaks specified, uses FD as default
    mainhist=hist(distrs[,1], breaks=n.breaks)#Creates first histogram
    brks=length(mainhist$breaks)
    for(i in 2:ncol(distrs)){#Overlays remaining histograms
      hist(distrs[,i], col=i, add=T, breaks=brks) 
    }
    legend(legendloc, legend=colnames(distrs), lty=1, lwd=2,col= 1:i, bty="n")
  } else

  if(type=="density") {
    maindens=plot(density(distrs[,1], bw=bwd))#Creates first density plot
    for(i in 2:ncol(distrs)){#Overlays remaining density lines
      lines(density(distrs[,i], bw=bwd), col=i) 
    }
    legend(legendloc, legend=colnames(distrs), lty=1, lwd=2,col= 1:i, bty="n")
  } else
  print("Error: the graph types allowed are 'cumulative', 'histogram', or 'density'")

}
