model_Means <- function(model){
	#if(!(any(names(model@frame) == "Habitat"))){stop(print("Function only works when using variable called 'Habitat'"))}
	
	newdat <- expand.grid(predictor = levels(model@frame[,2]), response= 0)
	names(newdat)[2] <- names(model@frame)[1]
	names(newdat)[1] <- names(model@frame)[2]
	
	newdat[2] <- predict(model,newdat,re.form=NA)
	mm <- model.matrix(terms(model),newdat)
	pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
	tvar1 <- pvar1+VarCorr(model)$Study.ID[1]  ## must be adapted for more complex models
	cmult <- 1.96 ## could use 1.96
	newdat <- data.frame(
	    newdat
	    , plo = newdat[2]-cmult*sqrt(pvar1) ## Fixed effects uncertanty only
	    , phi = newdat[2] +cmult*sqrt(pvar1)
	    , tlo = newdat[2]-cmult*sqrt(tvar1) ## Fixed effects uncertanty and RE variance
	    , thi = newdat[2] +cmult*sqrt(tvar1)
	)

	return(newdat)
}
