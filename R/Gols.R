#' @export
Gols  <- function(Nrep,y,x,b0,l0,shape,scale,tau0){
        source('rmvn.R')
        k           = length(b0)
        denom       = t(x)%*%x
        nom         = t(x) %*% y
        l0b0        = l0 %*% b0

        sigmabar    = solve((tau0 * denom) + l0)
        betabar     = sigmabar %*% ((tau0 * nom) + (l0b0))

        beta        = rmvn(u = betabar,sigma = sigmabar)
        tau         = rgamma(1,shape,scale)
        nshape      = length(y)/2 + shape

        betarep     = matrix(rep(NA,Nrep*k), nrow=Nrep, ncol=k)
        taurep      = rep(NA,Nrep)

        betarep[1,] = t(beta)
        taurep[1]   = tau

        for(i in 2:Nrep) {
        sigmabar    = solve((as.numeric(taurep[i-1]) * denom) + l0)
        betabar     = sigmabar %*% ((as.numeric(taurep[i-1]) * nom) + l0b0)
        
        e           = y - x %*% betarep[i-1,]
        nscale      = solve( (t(e) %*% e) /2 + 1/scale )

        beta        = rmvn(u = betabar,sigma = sigmabar)
        tau         = rgamma(1,nshape,scale = nscale) 
        
        betarep[i,] = t(beta)
        taurep[i]   = tau
        }
    return(list('beta'=betarep,'tau'=taurep))   
    }
