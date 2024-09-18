rm(list=ls())
library(ismev)
library(evd)

wd <- "~/my/working/directory/"
setwd(wd)

##########################################################################################
### Function computing high quantiles based on a GPD fit to high threshold exceedances ###
##########################################################################################
# data: entire data
# u: threshold probability level
# ps: vector of probability levels used to compute high quantiles
gpd.quant <- function(data,u,ps){
  thr <- quantile(data,u)
  fgpd <- gpd.fit(data,thr,show=FALSE)
  sig <- fgpd$mle[1]
  xi <- fgpd$mle[2]
  quants <- thr + (sig/xi)*(((1-ps)/(1-u))^(-xi)-1)
  return(quants)
}

###########################
### Simulation settings ###
###########################
R <- 10000 #number of simulation repetitions
ns <- c(500,1000,2000) #sample sizes
ps <- seq(0.99,0.99999,by=0.00001) #probability levels
us <- c(0.9,0.95,0.98,0.99) #GPD thresholds

PDF <- TRUE #should a PDF be created?
COMPUTE <- TRUE #should the quantiles all be recomputed? (if not, it will attempt to load results stored in a file)
SAVE <- FALSE #should the results be saved?


##############################
### Starting simulation... ###
##############################

if(!COMPUTE){
  load(file="Figure1_Chapter21.RData")
}

if(PDF){
  pdf(file="Figure1_Chapter21.pdf",width=6,height=6)
}

par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(4,3,3,1.5))


#################
## NORMAL CASE ##
#################
mu <- 0
sig <- 1

if(COMPUTE){
  quant.true.norm <- qnorm(p=ps,mean=mu,sd=sig) ## true quantiles
  exp.max.norm <- c()

  res.empir.norm <- array(dim=c(length(ns),length(ps),R)) ## array where empirical quantiles will be stored
  res.gpd.norm <- array(dim=c(length(ns),length(ps),length(us),R)) ## array where model-based GPD quantiles will be stored
  set.seed(111) ## set random seed for simulation

  for(i in 1:length(ns)){ ## loop over sample sizes
    n <- ns[i]
    data.norm <- matrix(rnorm(R*n,mean=mu,sd=sig),nrow=R,ncol=n)
    res.empir.norm[i,,] <- apply(X=data.norm,MARGIN=1,FUN=quantile,probs=ps,type=1)
    exp.max.norm[i] <- mean(apply(X=data.norm,MARGIN=1,FUN=max))
    for(j in 1:length(us)){ ## loop over threshold levels
      u <- us[j]
      print(paste0("n=",n,", u=",u,"; (i=",i,",j=",j,"); ",(i-1)*length(us)+j,"/",length(ns)*length(us)," (",round(100*((i-1)*length(us)+j)/(length(ns)*length(us)),digits=1),"%)"))
      res.gpd.norm[i,,j,] <- apply(X=data.norm,MARGIN=1,FUN=gpd.quant,u=u,ps=ps)
    }
  }

  ## Compute mean and lower/upper percentiles of empirical quantiles, across the R simulations
  quant.empir.mean.norm <- apply(X=res.empir.norm,MARGIN=c(1,2),FUN=mean)
  quant.empir.LB.norm <- apply(X=res.empir.norm,MARGIN=c(1,2),FUN=quantile,probs=0.025)
  quant.empir.UB.norm <- apply(X=res.empir.norm,MARGIN=c(1,2),FUN=quantile,probs=0.975)
  
  ## Compute mean and lower/upper percentiles of model-based GPD quantiles, across the R simulations
  quant.gpd.mean.norm <- apply(X=res.gpd.norm,MARGIN=c(1,2,3),FUN=mean)
  quant.gpd.LB.norm <- apply(X=res.gpd.norm,MARGIN=c(1,2,3),FUN=quantile,probs=0.025)
  quant.gpd.UB.norm <- apply(X=res.gpd.norm,MARGIN=c(1,2,3),FUN=quantile,probs=0.975)
}
  
## Plot the results
plot(quant.true.norm,quant.empir.mean.norm[2,],log="xy",type="n",asp=1,xlab="Theoretical exceedance probabilities",ylab="Estimated quantiles",lty=2,xlim=range(quant.true.norm),ylim=range(quant.true.norm),xaxt="n",main="Normal distribution")
ps.lab <- c(0.99,0.999,0.9999,0.99999)
quant.true.norm.lab <- qnorm(p=ps.lab,mean=mu,sd=sig)
axis(side=1,at=quant.true.norm.lab,labels=c("1e-2","1e-3","1e-4","1e-5"))
polygon(x=c(quant.true.norm,quant.true.norm[length(quant.true.norm):1]),y=c(quant.empir.LB.norm[2,],quant.empir.UB.norm[2,length(quant.true.norm):1]),col="lightgrey",border=NA)
polygon(x=c(quant.true.norm,quant.true.norm[length(quant.true.norm):1]),y=c(quant.gpd.LB.norm[2,,2],quant.gpd.UB.norm[2,length(quant.true.norm):1,2]),col=rgb(0,0,1,alpha=0.5),border=NA)
lines(quant.true.norm,quant.empir.mean.norm[2,],lty=2,lwd=1.5)
lines(quant.true.norm,quant.gpd.mean.norm[2,,2],col=rgb(0,0,1,alpha=1),lwd=1.5)
abline(0,1,col="black",lwd=2.5)
abline(h=exp.max.norm[2],lty=3,col="lightgrey",lwd=2)


################
## GAMMA CASE ##
################
scale <- 0.25
shape <- 4

if(COMPUTE){
  quant.true.gamma <- qgamma(p=ps,scale=scale,shape=shape) ## true quantiles
  exp.max.gamma <- c()

  res.empir.gamma <- array(dim=c(length(ns),length(ps),R)) ## array where empirical quantiles will be stored
  res.gpd.gamma <- array(dim=c(length(ns),length(ps),length(us),R)) ## array where model-based GPD quantiles will be stored
  set.seed(2222) ## set random seed for simulation

  for(i in 1:length(ns)){ ## loop over sample sizes
    n <- ns[i]
    data.gamma <- matrix(rgamma(R*n,scale=scale,shape=shape),nrow=R,ncol=n)
    res.empir.gamma[i,,] <- apply(X=data.gamma,MARGIN=1,FUN=quantile,probs=ps,type=1)
    exp.max.gamma[i] <- mean(apply(X=data.gamma,MARGIN=1,FUN=max))
    for(j in 1:length(us)){ ## loop over threshold levels
      u <- us[j]
      print(paste0("n=",n,", u=",u,"; (i=",i,",j=",j,"); ",(i-1)*length(us)+j,"/",length(ns)*length(us)," (",round(100*((i-1)*length(us)+j)/(length(ns)*length(us)),digits=1),"%)"))
      res.gpd.gamma[i,,j,] <- apply(X=data.gamma,MARGIN=1,FUN=gpd.quant,u=u,ps=ps)
    }
  }

  ## Compute mean and lower/upper percentiles of empirical quantiles, across the R simulations
  quant.empir.mean.gamma <- apply(X=res.empir.gamma,MARGIN=c(1,2),FUN=mean)
  quant.empir.LB.gamma <- apply(X=res.empir.gamma,MARGIN=c(1,2),FUN=quantile,probs=0.025)
  quant.empir.UB.gamma <- apply(X=res.empir.gamma,MARGIN=c(1,2),FUN=quantile,probs=0.975)
  
  ## Compute mean and lower/upper percentiles of model-based GPD quantiles, across the R simulations
  quant.gpd.mean.gamma <- apply(X=res.gpd.gamma,MARGIN=c(1,2,3),FUN=mean)
  quant.gpd.LB.gamma <- apply(X=res.gpd.gamma,MARGIN=c(1,2,3),FUN=quantile,probs=0.025)
  quant.gpd.UB.gamma <- apply(X=res.gpd.gamma,MARGIN=c(1,2,3),FUN=quantile,probs=0.975)
}
  
## Plot the results
plot(quant.true.gamma,quant.empir.mean.gamma[2,],log="xy",type="n",asp=1,xlab="Theoretical exceedance probabilities",ylab="Estimated quantiles",lty=2,xlim=range(quant.true.gamma),ylim=range(quant.true.gamma),xaxt="n",main="Gamma distribution")
ps.lab <- c(0.99,0.999,0.9999,0.99999)
quant.true.gamma.lab <- qgamma(p=ps.lab,scale=scale,shape=shape)
axis(side=1,at=quant.true.gamma.lab,labels=c("1e-2","1e-3","1e-4","1e-5"))
polygon(x=c(quant.true.gamma,quant.true.gamma[length(quant.true.gamma):1]),y=c(quant.empir.LB.gamma[2,],quant.empir.UB.gamma[2,length(quant.true.gamma):1]),col="lightgrey",border=NA)
polygon(x=c(quant.true.gamma,quant.true.gamma[length(quant.true.gamma):1]),y=c(quant.gpd.LB.gamma[2,,2],quant.gpd.UB.gamma[2,length(quant.true.gamma):1,2]),col=rgb(0.5,0.5,1,alpha=0.5),border=NA)
lines(quant.true.gamma,quant.empir.mean.gamma[2,],lty=2,lwd=1.5)
lines(quant.true.gamma,quant.gpd.mean.gamma[2,,2],col=rgb(0.5,0.5,1,alpha=1),lwd=1.5)
abline(0,1,col="black",lwd=2.5)
abline(h=exp.max.gamma[2],lty=3,col="lightgrey",lwd=2)


#####################
## LOG-NORMAL CASE ##
#####################
mu <- 0
sig <- 1

if(COMPUTE){
  quant.true.ln <- qlnorm(p=ps,meanlog=mu,sdlog=sig) ## true quantiles
  exp.max.ln <- c()

  res.empir.ln <- array(dim=c(length(ns),length(ps),R)) ## array where empirical quantiles will be stored
  res.gpd.ln <- array(dim=c(length(ns),length(ps),length(us),R)) ## array where model-based GPD quantiles will be stored
  set.seed(33333) ## set random seed for simulation

  for(i in 1:length(ns)){ ## loop over sample sizes
    n <- ns[i]
    data.ln <- matrix(rlnorm(R*n,meanlog=mu,sdlog=sig),nrow=R,ncol=n)
    res.empir.ln[i,,] <- apply(X=data.ln,MARGIN=1,FUN=quantile,probs=ps,type=1)
    exp.max.ln[i] <- mean(apply(X=data.ln,MARGIN=1,FUN=max))
    for(j in 1:length(us)){ ## loop over threshold levels
      u <- us[j]
      print(paste0("n=",n,", u=",u,"; (i=",i,",j=",j,"); ",(i-1)*length(us)+j,"/",length(ns)*length(us)," (",round(100*((i-1)*length(us)+j)/(length(ns)*length(us)),digits=1),"%)"))
      res.gpd.ln[i,,j,] <- apply(X=data.ln,MARGIN=1,FUN=gpd.quant,u=u,ps=ps)
    }
  }

  ## Compute mean and lower/upper percentiles of empirical quantiles, across the R simulations
  quant.empir.mean.ln <- apply(X=res.empir.ln,MARGIN=c(1,2),FUN=mean)
  quant.empir.LB.ln <- apply(X=res.empir.ln,MARGIN=c(1,2),FUN=quantile,probs=0.025)
  quant.empir.UB.ln <- apply(X=res.empir.ln,MARGIN=c(1,2),FUN=quantile,probs=0.975)

  ## Compute mean and lower/upper percentiles of model-based GPD quantiles, across the R simulations
  quant.gpd.mean.ln <- apply(X=res.gpd.ln,MARGIN=c(1,2,3),FUN=mean)
  quant.gpd.LB.ln <- apply(X=res.gpd.ln,MARGIN=c(1,2,3),FUN=quantile,probs=0.025)
  quant.gpd.UB.ln <- apply(X=res.gpd.ln,MARGIN=c(1,2,3),FUN=quantile,probs=0.975)
}
  
## Plot the results
plot(quant.true.ln,quant.empir.mean.ln[2,],log="xy",type="n",asp=1,xlab="Theoretical exceedance probabilities",ylab="Estimated quantiles",lty=2,xlim=range(quant.true.ln),ylim=range(quant.true.ln),xaxt="n",main="Log-normal distribution")
ps.lab <- c(0.99,0.999,0.9999,0.99999)
quant.true.ln.lab <- qlnorm(p=ps.lab,meanlog=mu,sdlog=sig)
axis(side=1,at=quant.true.ln.lab,labels=c("1e-2","1e-3","1e-4","1e-5"))
polygon(x=c(quant.true.ln,quant.true.ln[length(quant.true.ln):1]),y=c(quant.empir.LB.ln[2,],quant.empir.UB.ln[2,length(quant.true.ln):1]),col="lightgrey",border=NA)
polygon(x=c(quant.true.ln,quant.true.ln[length(quant.true.ln):1]),y=c(quant.gpd.LB.ln[2,,2],quant.gpd.UB.ln[2,length(quant.true.ln):1,2]),col=rgb(1,0.5,0.5,alpha=0.5),border=NA)
lines(quant.true.ln,quant.empir.mean.ln[2,],lty=2,lwd=1.5)
lines(quant.true.ln,quant.gpd.mean.ln[2,,2],col=rgb(1,0.5,0.5,alpha=1),lwd=1.5)
abline(0,1,col="black",lwd=2.5)
abline(h=exp.max.ln[2],lty=3,col="lightgrey",lwd=2)


##################
## FRECHET CASE ##
##################
loc <- 0
scale <- 1
shape <- 3

if(COMPUTE){
  quant.true.frechet <- qfrechet(p=ps,loc=loc,scale=scale,shape=shape) ## true quantiles
  exp.max.frechet <- c()

  res.empir.frechet <- array(dim=c(length(ns),length(ps),R)) ## array where empirical quantiles will be stored
  res.gpd.frechet <- array(dim=c(length(ns),length(ps),length(us),R)) ## array where model-based GPD quantiles will be stored
  set.seed(444444) ## set random seed for simulation

  for(i in 1:length(ns)){ ## loop over sample sizes
    n <- ns[i]
    data.frechet <- matrix(rfrechet(R*n,loc=loc,scale=scale,shape=shape),nrow=R,ncol=n)
    res.empir.frechet[i,,] <- apply(X=data.frechet,MARGIN=1,FUN=quantile,probs=ps,type=1)
    exp.max.frechet[i] <- mean(apply(X=data.frechet,MARGIN=1,FUN=max))
    for(j in 1:length(us)){ ## loop over threshold levels
      u <- us[j]
      print(paste0("n=",n,", u=",u,"; (i=",i,",j=",j,"); ",(i-1)*length(us)+j,"/",length(ns)*length(us)," (",round(100*((i-1)*length(us)+j)/(length(ns)*length(us)),digits=1),"%)"))
      res.gpd.frechet[i,,j,] <- apply(X=data.frechet,MARGIN=1,FUN=gpd.quant,u=u,ps=ps)
    }
  }

  ## Compute mean and lower/upper percentiles of empirical quantiles, across the R simulations
  quant.empir.mean.frechet <- apply(X=res.empir.frechet,MARGIN=c(1,2),FUN=mean)
  quant.empir.LB.frechet <- apply(X=res.empir.frechet,MARGIN=c(1,2),FUN=quantile,probs=0.025)
  quant.empir.UB.frechet <- apply(X=res.empir.frechet,MARGIN=c(1,2),FUN=quantile,probs=0.975)

  ## Compute mean and lower/upper percentiles of model-based GPD quantiles, across the R simulations
  quant.gpd.mean.frechet <- apply(X=res.gpd.frechet,MARGIN=c(1,2,3),FUN=mean)
  quant.gpd.LB.frechet <- apply(X=res.gpd.frechet,MARGIN=c(1,2,3),FUN=quantile,probs=0.025)
  quant.gpd.UB.frechet <- apply(X=res.gpd.frechet,MARGIN=c(1,2,3),FUN=quantile,probs=0.975)
}
 
## Plot the results 
plot(quant.true.frechet,quant.empir.mean.frechet[2,],log="xy",type="n",asp=1,xlab="Theoretical exceedance probabilities",ylab="Estimated quantiles",lty=2,xlim=range(quant.true.frechet),ylim=range(quant.true.frechet),xaxt="n",main="Fréchet distribution")
ps.lab <- c(0.99,0.999,0.9999,0.99999)
quant.true.frechet.lab <- qfrechet(p=ps.lab,loc=loc,scale=scale,shape=shape)
axis(side=1,at=quant.true.frechet.lab,labels=c("1e-2","1e-3","1e-4","1e-5"))
polygon(x=c(quant.true.frechet,quant.true.frechet[length(quant.true.frechet):1]),y=c(quant.empir.LB.frechet[2,],quant.empir.UB.frechet[2,length(quant.true.frechet):1]),col="lightgrey",border=NA)
polygon(x=c(quant.true.frechet,quant.true.frechet[length(quant.true.frechet):1]),y=c(quant.gpd.LB.frechet[2,,2],quant.gpd.UB.frechet[2,length(quant.true.frechet):1,2]),col=rgb(1,0,0,alpha=0.5),border=NA)
lines(quant.true.frechet,quant.empir.mean.frechet[2,],lty=2,lwd=1.5)
lines(quant.true.frechet,quant.gpd.mean.frechet[2,,2],col=rgb(1,0,0,alpha=1),lwd=1.5)
abline(0,1,col="black",lwd=2.5)
abline(h=exp.max.frechet[2],lty=3,col="lightgrey",lwd=2)


if(PDF){
  dev.off() 
}

if(SAVE){
  save(list=c("quant.true.norm","exp.max.norm","res.empir.norm","quant.empir.mean.norm","quant.empir.LB.norm","quant.empir.UB.norm","res.gpd.norm","quant.gpd.mean.norm","quant.gpd.LB.norm","quant.gpd.UB.norm","quant.true.gamma","exp.max.gamma","res.empir.gamma","quant.empir.mean.gamma","quant.empir.LB.gamma","quant.empir.UB.gamma","res.gpd.gamma","quant.gpd.mean.gamma","quant.gpd.LB.gamma","quant.gpd.UB.gamma","quant.true.ln","exp.max.ln","res.empir.ln","quant.empir.mean.ln","quant.empir.LB.ln","quant.empir.UB.ln","res.gpd.ln","quant.gpd.mean.ln","quant.gpd.LB.ln","quant.gpd.UB.ln","quant.true.frechet","exp.max.frechet","res.empir.frechet","quant.empir.mean.frechet","quant.empir.LB.frechet","quant.empir.UB.frechet","res.gpd.frechet","quant.gpd.mean.frechet","quant.gpd.LB.frechet","quant.gpd.UB.frechet"),file="Figure1_Chapter21.RData")
}
