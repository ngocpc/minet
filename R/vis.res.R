#This file belong to
#minet: Mutual Information NETworks, <http://minet.meyerp.com>
#a package that implements various algorithms for inferring mutual information networks from data.

#Copyright (Creative Commons by-nc-sa) July 2010  Patrick Emmanuel Meyer <software@meyerp.com>
#<License full notice: at the root of the package 
#and at http://creativecommons.org/licenses/by-nc-sa/3.0/legalcode> 

pr <- function( table )
{
      precision <- table$tp / (table$tp + table$fp)
      recall <- table$tp / (table$tp + table$fn)
      precision[is.nan(precision)]<-1
      res <- data.frame(precision,recall)
      
      names(res) <- c("p","r")
      res
}
rates <- function( table )
{
      tp.rate <- table$tp / (table$tp + table$fn)
      fp.rate <- table$fp / (table$fp + table$tn)
      res <- data.frame(tp.rate,fp.rate)
      names(res) <- c("tpr","fpr")
      res
}

show.pr <- function( table, device=-1, ... )
{
      pr <- pr(table)
      if(device==-1) {
			dev.new()
            plot( pr$r,pr$p, xlab="recall",
                  ylab="precision",
                  main="PR-Curve",
                  xlim=0:1,ylim=0:1,...)
      }else{
            dev.set(device)
            points( pr$r,pr$p, xlab="recall",
                  ylab="precision", 
                  main="PR-Curve",
                  xlim=0:1,ylim=0:1,... )
      }
      dev.cur()
}
show.roc <- function( table, device=-1, ... )
{
      rates <- rates(table)
      if(device==-1) {
            dev.new()
            plot( rates$fpr,rates$tpr,
                  xlab="FP rate", 
                  ylab="TP rate",
                  main="ROC-Curve",
                  xlim=0:1,ylim=0:1,...)
      }else{
            dev.set(device)
            points( rates$fpr,rates$tpr,
                  xlab="FP rate",
                  ylab="TP rate",
                  main="ROC-Curve",
                  xlim=0:1,ylim=0:1,... )
      }
      lines( 0:1, 0:1, col="black" )
      dev.cur()
}

fscores <- function(table,beta=1)
{
      pr <- pr(table)
      res <- ((beta*beta+1)*pr$p*pr$r) / (beta*beta*pr$p+pr$r)
      res[which(is.nan(res))]=0
      res[which(is.infinite(res))]=0 
      #res <- as.data.frame(res)
	  #names(res) <- "fscores"
	res
}

auc.roc <- function(table)
{ 
	rates <- rates(table)
    return(sum(diff(rates$fpr)*(rates$tpr[-1]+rates$tpr[-length(rates$fpr)]))/2)
}

auc.pr <- function(table)
{
	pr <- pr(table)
	return(sum(diff(pr$r)*(pr$p[-1]+pr$p[-length(pr$p)]))/2)
}

