model_Means <- function(model){
	if(!(any(names(model@frame) == "Habitat"))){stop(print("Function only works when using variable called 'Habitat'"))}
	
	newdat <- expand.grid(Habitat = levels(model@frame$Habitat), response= 0)
	names(newdat)[2] <- names(model@frame)[1]
	newdat[2] <- predict(model,newdat,re.form=NA)
	mm <- model.matrix(terms(model),newdat)
	pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
	tvar1 <- pvar1+VarCorr(model)$Study.ID[1]  ## must be adapted for more complex models
	cmult <- 1.96 ## could use 1.96
	newdat <- data.frame(
	    newdat
	    , plo = newdat$Richness-cmult*sqrt(pvar1) ## Fixed effects uncertanty only
	    , phi = newdat$Richness +cmult*sqrt(pvar1)
	    , tlo = newdat$Richness-cmult*sqrt(tvar1) ## Fixed effects uncertanty and RE variance
	    , thi = newdat$Richness +cmult*sqrt(tvar1)
	)

	return(newdat)
}
