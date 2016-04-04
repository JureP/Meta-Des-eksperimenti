## ucenje base learnerjev
set.seed(10)

OkoljeShrani <- 'C:/Users/Podlogar/Documents/DES_eksperiment/WP/Podatki'
OkoljeShraniBL_model <- 'C:/Users/Podlogar/Documents/DES_eksperiment/WP/BaseLearner/model'
OkoljeShraniBL_koncniModel <- 'C:/Users/Podlogar/Documents/DES_eksperiment/WP/BaseLearner/koncni model'
OkoljeShraniBL_OP <- 'C:/Users/Podlogar/Documents/DES_eksperiment/WP/BaseLearner/OP'
dir.create(OkoljeShrani)
dir.create(OkoljeShraniBL_model)
dir.create(OkoljeShraniBL_koncniModel)
dir.create(OkoljeShraniBL_OP)


library(mlbench)
library(caret)
library(adabag)

## parametri
stModelov <- 100
	

## podatki
data(PimaIndiansDiabetes)
osnovniPodatki <- na.omit(PimaIndiansDiabetes)
summary(osnovniPodatki)
onsovniPodatki_response <- osnovniPodatki[, 9]
onsovniPodatki_FM <- osnovniPodatki[, -9]

{## razdelitev podatkov v clanku!!
	## 20 ponovitev,
	## 25% test set
	## 25% za dynamic selection set
	## 25% meta Train set
	## 25% bl train set
	
	for(ponovitev in 1:20){
		podatki <- list()
		cvRazdelitev <- createFolds(onsovniPodatki_response, k = 4, list = TRUE, returnTrain = FALSE)
		for(ime in names(cvRazdelitev)){
			podatki <- c(podatki, list(onsovniPodatki_FM[cvRazdelitev[[ime]], ]))
			podatki <- c(podatki, list(onsovniPodatki_response[cvRazdelitev[[ime]]]))
		}
		names(podatki) <- paste0(rep(names(cvRazdelitev), each = 2), rep(c('_FM', '_Y'), 4))
		setwd(OkoljeShrani)
		saveRDS(podatki, paste0('razdeljeniPodatki_Del_',ponovitev , '.rds'))
	}

}


{## ucenje BL 
	## 100 bagging modelov
	## v clanku uporabljen perceptroni namesto dreves
	for(del in 1:20){
		setwd(OkoljeShrani)
		podatki <- readRDS(paste0('razdeljeniPodatki_Del_', del , '.rds'))
		pd <- cbind(y = podatki[['Fold1_Y']], podatki[['Fold1_FM']])
		mdl <- bagging(y ~ ., data = pd, mfinal = stModelov)
		setwd(OkoljeShraniBL_koncniModel)
		saveRDS(mdl, paste0('CelBbaggingModel_Del_', del, '.rds'))
		## Shranjevanje posameznih modelov in output profilov
		OkoljeModel <- paste0(OkoljeShraniBL_model, '/podatki', del)
		OkoljeOP <- paste0(OkoljeShraniBL_OP, '/podatki', del)
		dir.create(OkoljeModel)
		dir.create(OkoljeOP)
		## OP
		OP_fold2 <-  data.frame(row.names=1:nrow(podatki[['Fold2_FM']])) ## metaTrain
		OP_fold3 <- data.frame(row.names=1:nrow(podatki[['Fold3_FM']])) ## Dsel
		OP_fold4 <- data.frame(row.names=1:nrow(podatki[['Fold4_FM']])) ## Test
		for(i in 1:stModelov){
			setwd(OkoljeModel)
			saveRDS(mdl$trees[i], paste0('baggingModel_', i,'_Del_',del , '.rds'))
			OP_fold2 <- cbind(OP_fold2, data.frame(predict(mdl$trees[i], podatki[['Fold2_FM']])))
			OP_fold3 <- cbind(OP_fold2, data.frame(predict(mdl$trees[i], podatki[['Fold3_FM']])))
			OP_fold4 <- cbind(OP_fold2, data.frame(predict(mdl$trees[i], podatki[['Fold4_FM']])))
		}
		colnames(OP_fold2) <- paste0(rep(paste0('model_', 1:stModelov, '_'), each = 2), colnames(OP_fold2))
		colnames(OP_fold3) <- paste0(rep(paste0('model_', 1:stModelov, '_'), each = 2), colnames(OP_fold3))
		colnames(OP_fold4) <- paste0(rep(paste0('model_', 1:stModelov, '_'), each = 2), colnames(OP_fold4))
		setwd(OkoljeOP)
		saveRDS(OP_fold2, paste0('OP_metaTrain_', del,'.rds'))
		saveRDS(OP_fold3, paste0('OP_Dsel_', del,'.rds'))
		saveRDS(OP_fold4, paste0('OP_test_', del,'.rds'))
	}

}





