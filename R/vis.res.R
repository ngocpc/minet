pr <- function( table )
{
      precision <- table$tp / (table$tp + table$fp)
      recall <- table$tp / (table$tp + table$fn)
      res <- cbind(0,max(recall))
      res <- rbind(res,cbind(precision,recall))      
      res <- rbind(res,cbind(max(precision),0))
      res <- as.data.frame(res)
      names(res) <- c("p","r")
      res
}
rates <- function( table )
{
      tp.rate <- table$tp / (table$tp + table$fn)
      fp.rate <- table$fp / (table$fp + table$tn)
      res <- cbind(1,1)
      res <- rbind(res,cbind(tp.rate,fp.rate))
      res <- rbind(res,cbind(0,0))
      res <- as.data.frame(res)
      names(res) <- c("tpr","fpr")
      res
}
show.pr <- function( table, device=-1, ... )
{
      pr <- pr(table)
      n <- nrow(pr)
      p <- 0
      for(i in 1:n)
	    p[i] <- max(pr[,1][1:i],na.rm=TRUE)
      if(device==-1) {
			dev.new()
            plot( pr$r,p, xlab="recall",
                  ylab="precision",
                  main="PR-Curve",
                  xlim=0:1,ylim=0:1,...)
      }else{
            dev.set(device)
            points( pr$r,p, xlab="recall",
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
      res <- ((beta+1)*pr[,1]*pr[,2]) / (beta*pr[,1]+pr[,2])
      res[which(is.nan(res))]=0
      res[which(is.infinite(res))]=0 
      res <- as.data.frame(res)
	names(res) <- "fscores"
	res
}
