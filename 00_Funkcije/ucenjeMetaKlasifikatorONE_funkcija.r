library(caret)
# OkoljeFunkcije <- 'C:/Users/Podlogar/Documents/DynamicEnsembleSelection/Izvedba/Funkcije'
# setwd(OkoljeFunckie)
# source("probToClass_funkcija.r")



ucenjeMetaKlasifikatorONE <- function(imenaMnozic = c('trainBL','sosedSet', 'metaSet'), ## ime mnozice: [sosede katerih iscemo, iz katere so sosedje]
									OP_meta, ## output profile (verjetnosti) za mnozico metaSet 
											## imena stolpcev: baseLearer1_class(1) baseLearner1_ class(2) ... baseLearner1_class(n) ... baseLearnerK_class(n)
									classOsnovni, ## imena classov pri osnovnem problemu kot vektor (npr: c('neg', 'pos')
									baseLearner, ## ime base learnerjev (vektor)
									K = 5, ## stevilo sosedov za region of competence (vektor)
									Kp = 5, ## stevilo sosedov iz output profile (vektor)
									knnALG = "kd_tree", ## algoritem po katerem get.knnx isce sosede
									metaALG = c('rf', 'plr'), ## klasifikacijski algoritmi uporabljeni pri ucenju meta klasifikatorja
									hC = c(0.95, 1), ## kako razlicne morajo biti napovedi base learnerjev, za x iz metaSet da ga vkljucimo v ucenje
									OkoljeMetaPrblm, ## okolje kjer so shranjene metaProblemi 
									OkoljeMetaKlasifikator ## okolje, kamor se shranijo meta klasifikatorji
									)
									## nauci meta klasifikatorje za razlicne parametre (en klasifikator za vse BL)
									{

	stHypParOpt <- length(K)*length(Kp)*length(knnALG)*length(metaALG)*length(hC)
	zeIrac <- 0
	for(nSosedi in K){
			for(OPnSosedi in Kp){
				for(alg in knnALG){
					for(meja in hC){
						FM <- NULL
						response <- NULL
						for(bl in baseLearner){
							setwd(OkoljeMetaPrblm)
							imeMetaProblem <- paste0('matrikaProblem[BL]', bl, '[trainBL]', imenaMnozic[1], '[sosedSet]', 
													imenaMnozic[2], '[metaSet]', imenaMnozic[3], '[K]',nSosedi, 
													'[Kp]',OPnSosedi, '[knnALG]', alg, '.rds')
							metaPodatki <- readRDS(imeMetaProblem)
							## izbor samo tistih x iz metaSet, pri katerih imajo bl-ji dovolj razlicne napovedi
							matrikaNapovedi <- probToClass(OP_meta, baseLearner, classes = classOsnovni)
							razlicnostNapovedi <- data.frame(matrix(NA, nrow(OP_meta),length(classOsnovni)))
							colnames(razlicnostNapovedi) <- classOsnovni
							for(k in classOsnovni){
								razlicnostNapovedi[, k] <- rowSums(matrikaNapovedi == k)
							}
							izborR <- NULL
							for(v in 1:nrow(razlicnostNapovedi)){
								a <- razlicnostNapovedi[v,]/sum(razlicnostNapovedi[v,])
								bst <- sort(a, decreasing = TRUE)[1]
								if(bst <= meja){
									izborR <- c(izborR, v)
								}
							}
							FM <-  rbind(FM, metaPodatki$metaFM[izborR,])
							response <- c(as.character(response), as.character(metaPodatki$metaClass[izborR]))
						}
						if(nrow(FM) < 50){
							print(paste('Samo', nrow(FM), 'primerov iz metaSet-a ima dovolj razlicne napovedi (hC)!!!'))
						}
						if(nrow(FM) < 2){
							stop('noben primer ne preseze praga razlicnosti napovedi hC!!')
						}
						##################################################################
						### UCENJE META KLASIFIKATORJA ###################################
						for(algM in metaALG){
							imeMetaKlasifikator <- paste0('metaKlasifikator[ONE]', '[trainBL]', imenaMnozic[1], 
														'[sosedSet]', imenaMnozic[2], '[metaSet]', imenaMnozic[3], 
														'[K]',nSosedi, '[Kp]',OPnSosedi,'[knnALG]', alg, '[metaALG]',
														algM, '[cH]', meja, '.rds')
							setwd(OkoljeMetaKlasifikator)
							if(imeMetaKlasifikator %in% dir()){
								print(paste('ZE NAUCEN:', imeMetaKlasifikator))
							}else {
								metaModel <- train(FM, factor(response), method = algM)
								## shranjevanje
								saveRDS(metaModel, imeMetaKlasifikator)
								## izpis poteka
								zeIrac <- zeIrac + 1
								print(paste0('IZRACUNANO: ', round(zeIrac/stHypParOpt*100, 2), '%'))
							}
						}
					}
				}
			}
		}
}
									


	
