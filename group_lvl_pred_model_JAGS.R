model{

	for(i in 1:(n)){
		y[i] ~ dnorm(y.hat[i], tau.y)
		y.hat[i] <- inprod(B[species[i],], X[i,])
	}

tau.y <- pow(sigma.y, -2)
sigma.y ~ dunif(0,100)

for(k in 1:(n)){
	for(j in 1:(nspec)){
		B[j,k] <- xi[k] * B.raw[j,k]
	}
	xi[k] ~ dunif(0,100)
}

for(j in 1:(nspec)){
	B.raw[j,1:ncof] ~ dmnorm(B.raw.hat[j,], Tau.B.raw[,])
	for(k in 1:(ncof)){
		B.raw.hat[j,k] <- inprod(G.raw[k,],U[j,])
	}
}

for(k in 1:(ncof)){
	for(l in 1:(nmig)){
		G[k,l] <- xi[k]*G.raw[k,l]
		G.raw[k,l] ~ dnorm(0, 0.001)
	}
}
Tau.B.raw[1:ncof, 1:ncof] ~ dwish(W[,], df)
df <- ncof + 1
Sigma.B.raw[1:ncof, 1:ncof] <- inverse(Tau.B.raw[,])
for(k in 1:ncof){
	for(k.prime in 1:ncof){
		rho.B[k,k.prime] <- Sigma.B.raw[k.k.prime]/
			sqrt(Sigma.B.raw[k,k] * Sigma.B.raw[k.prime,k.prime])
	}
	sigma.B[k] <- abs(xi[k]) * sqrt(Sigma.B.raw[k,k])
  }

}
