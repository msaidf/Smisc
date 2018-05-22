#' @export
mixcov <- function(nrep, y, xout=matrix(1,nrow=length(y)), xtreat, n_group, prioralpha, priorbeta, alphaprec, betaprec, shape, scale) {
nobs = length(y)

xtx    = crossprod(xtreat)
alphasig = solve(xtx + alphaprec)

D=sample(c(0,1),size = length(y),replace = T)
Dstar=2*D - 1
D = cbind(D, 1-D)
sig = cbind(1,1)
alphas = prioralpha
betas = priorbeta

alpharep = array(rep(NA, nrep*enrow(alphas)),dim = c(enrow(alphas), nrep))
betarep = array(rep(NA, nrep*n_group*enrow(betas)),dim = c(enrow(betas),n_group, nrep))
sigmarep =array(rep(NA, nrep * n_group), dim = c(nrep, n_group))
Prep = array(rep(NA, nrep * nobs), dim=c(nobs, nrep))

alpharep[,1]=alphas
betarep[,,1]=betas
sigmarep[1,] =sig
Prep[,1] = rep(.5,nobs)
##################################
# Gibbs Sampler
###################################
for(j in 2:nrep) {
	# coefficients
	for(l in 1:n_group) {
		use    = which(D[,l]==1)
		xout_group   = xout[use,]
		y_group   = y[use]
		betasig    = solve(crossprod(xout_group)/sig[,l] + betaprec[,l])
		betabar    = crossprod(xout_group,y_group)/sig[,l] + priorbeta[,l] * betaprec[,l]
		betas[,l] = rmvn(1,betasig %*% betabar, betasig)
	# variances
		resids    = .5 * crossprod(y_group - xout_group %*% matrix(betas[,l]))
		sig[,l]   = 1/rgamma(n = 1,
							 shape = length(y_group)/2 + shape[l,1],
							 scale = solve(resids + solve(scale[l,1])))
	}
	# treatment assignment
	alphabar   = crossprod(xtreat,Dstar) + alphaprec %*% prioralpha 
	alphas     = rmvn(1,alphasig %*% alphabar, alphasig) %>% t
    mu         = xtreat %*% alphas
	# Component probabilites
	P          = pnorm(mu)
	# Component indicator (D)
	
	probs_1    = P * dnorm(x = y, mean = xout %*% matrix(betas[,1]), sd = sqrt(sig[,1]))
	probs_0    = (1-P) * dnorm(x = y, mean = xout %*% matrix(betas[,2]), sd = sqrt(sig[,2]))
	normalized = probs_1/ (probs_1 + probs_0)
	D          = rbinom(nobs, 1, normalized)

	while(sum(D) <= 1 | sum(D) >= nobs-1)  D = rbinom(nobs, 1, normalized)

    ytp        = which(D==1)
    ytn        = which(D==0)
    up         = runif(length(ytp))
    un         = runif(length(ytn))
    Dstar[ytp] = qnorm(up * (1 - pnorm(-mu[ytp])) + pnorm(-mu[ytp]) ) + mu[ytp]
	Dstar[ytn] = qnorm(un * pnorm(-mu[ytn])) + mu[ytn]

	D = cbind(D, 1-D)


	alpharep[,j]  = alphas
	betarep[,,j]   = betas
	Prep[,j]        = P
	sigmarep[j,]   = sig

}
dimnames(betarep)[[2]] = c('treatment', 'control')
return(list('alphas' = alpharep, 'betas' = betarep, 'Ps' = Prep, 'sigmas' = sigmarep))
}
