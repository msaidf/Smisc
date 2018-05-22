#' Caution: this function fails to converge if prior precision is set too high (>5). Better set diffuse prior.
#' @export
mixreg <- function(nrep, y, x, n_group, priorp, priorbeta, priorprec, a1, a2, P, betas, sig, tau) {
nobs = length(y)

betarep = array(rep(NA, nrep*n_group*nrow(betas)),dim = c(nrow(betas),n_group, nrep))
					# matlab::zeros(nrep, n_group)
sigmarep =array(rep(NA, nrep * n_group), dim = c(nrep, n_group))
Prep = array(rep(NA, nrep * n_group), dim = c(nrep, n_group))

betarep[,,1]=betas
sigmarep[1,] =sig
Prep[1,] = P
##################################
# Gibbs Sampler
###################################
for(j in 2:nrep) {
	# coefficients
	for(l in 1:n_group) {
		points = which(tau[,l]==1)
		x_group = x[points,]
		y_group = y[points]
		D_beta = solve(crossprod(x_group)/sig[,l] + priorprec[,l])
		d_beta = crossprod(x_group,y_group)/sig[,l] + priorbeta[,l] * priorprec[,l]
		# H = chol(D_beta)
		# betas[,l] = D_beta %*% d_beta + crossprod(H, rnorm(ncol(x)))
		betas[,l] = D_beta %*% d_beta 
	# variances
		resids       = .5 * crossprod(y_group - x_group %*% betas[,l])
		sig[,l] = 1/rgamma(n = 1, shape = length(y_group)/2 + a1[l,1], scale = solve(resids + solve(a2[l,1])))
	}
	# Component indicator
	tempp1           = dnorm(x = y, mean = betas[,1], sd = sqrt(sig[,1]))
	tempp2           = dnorm(x = y, mean = betas[,2], sd = sqrt(sig[,2]))
	probs_1          = P[1] * tempp1
	probs_2          = P[2] * tempp2
	normalized       = probs_1/ (probs_1 + probs_2)

	tau1             = rbinom(nobs, 1, normalized)
	while(sum(tau1)<= 1 | sum(tau1)>= nobs-1)  tau1 = rbinom(nobs, 1, normalized)
	tau              = cbind( tau1, 1 - tau1)
	# Component probabilites
	nn               = apply(tau,2,sum)
	P                = MCMCpack::rdirichlet(1,c(nn[1] + priorp[1],
								 nn[2] + priorp[2]))
	betarep[,,j] = betas
	Prep[j,]      = P
	sigmarep[j,]    = sig
}
return(list('betas' = betarep, 'Ps' = Prep, 'sigmas' = sigmarep))
} 
