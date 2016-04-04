probToClass <- function(outputProfile, ## matrika napovedi z imeni stolpcev [for(i in bls){for(j in classes){bl(j)_class(i)}}]
											## npr: gbm_neg gbm_pos rf_neg rf_pos ...
						## thrshld = 0.5, ## 
						baseLearner, ## imena modelove ki nas zanimajo
						classes ## razredi, ki jih lahko napovedujemo
						)
						#################
						## funkcija matriko z napovedmi verjetnosti posameznega classa za
						## vsak base learner prevede v matriko napovedi vsakega base learnerja
						{
	
	matrikaNapovedi <- data.frame(matrix(NA, nrow(outputProfile), length(baseLearner)))
	colnames(matrikaNapovedi) <- baseLearner
	for(bl in baseLearner){
		izberi <- paste0(bl, '_',classes)
		matrikaNapovedi[, bl] <- factor(classes[apply(outputProfile[,izberi],1 , which.max)], levels = classes)
	}
	return(matrikaNapovedi)
}



