#' @export
Gprobit  <- function(Nrep, y,x, priorcoef=0, priorprec=0){
        x      = as.matrix(x)
        y      = as.matrix(y)
        n      = nrow(y)
        k      = ncol(x)
        z      = 2*y - 1
        ytp    = which(z>0)
        ytn    = which(z<=0)
		xtx    = crossprod(x)
		sig    = solve(xtx + priorprec)
		priorprecoef = priorprec %X% priorcoef
		mu     = mean(y) * rep(1, n)
		meanx  = colSums(x)/n
		z      = y
		beta   = matrix(rep(NA, Nrep*k), ncol = Nrep)
		margin = matrix(rep(NA, Nrep*k), ncol = Nrep)

        for(j in 1:Nrep) {
            betabar    	= sig %*% (crossprod(x,z) + priorprecoef)
            beta[,j]    = rmvn(u = betabar,sigma = sig)%>% t
			margin[,j]	= beta[,j] * dnorm(meanx%*%beta[,j])
            mu          = x %*% beta[,j]
            up          = runif(length(ytp))
            un          = runif(length(ytn))
            z[ytp]      = qnorm(up * (1 - pnorm(-mu[ytp])) + pnorm(-mu[ytp]) ) + mu[ytp]
			z[ytn]		= qnorm(un * pnorm(-mu[ytn])) + mu[ytn]
        }
        return(list('beta'=t(beta), 'margin' = t(margin)))
    }
