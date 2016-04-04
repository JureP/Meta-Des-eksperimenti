	
optMETA_DES_CARET <- function(## parametri potrebni za ucenje base learnerja
						baseLearner, ## ime base learnerjev, ki jih podpira caret
						trainBL, ## podatki za ucenje base learnerja
						yTrainBL, ## respns vektor za ucenje base learnerja
						OkoljeBaseLearner, ## okolje kamor se shranijo base learner modeli
						OkoljeRezultatOutputProfile, ## okolje kamor se shranijo output profili

						## parametri sestavljanja meta problema ###########
						imenaMnozic = c('trainBL','sosedSet', 'metaSet', 'validSet'),  ## ime mnozice: [sosede katerih iscemo, iz katere so sosedje]
						metaSet, ## mnozica iz katere se sestavi meta problem
						yMetaSet, ## response vektor meta seta
							### 'n' za sestavitev metaFM, brez meta respons vektorja 
						sosedSet, ## mnozica iz katere so izbrane sosedi za metaSet
						ySosedSet, ## response vektor sosed seta
						# OP_meta, ## output profile (verjetnosti) za mnozico metaSet 
							## imena stolpcev: baseLearer1_class(1) baseLearner1_ class(2) ... baseLearner1_class(n) ... baseLearnerK_class(n)
						# OP_sosedi, ## output profile (verjetnosti) za mnozico sosedSet
							## imena stolpcev: baseLearer1_class(1) baseLearner1_ class(2) ... baseLearner1_class(n) ... baseLearnerK_class(n)
						K = c(5,10), ## stevilo sosedov za region of competence (vektor)
						Kp = c(5,10), ## stevilo sosedov iz output profile (vektor)
						knnALG = "kd_tree", ## algoritem po katerem get.knnx isce sosede
						OkoljeMetaPrblm, ## okolje kamor naj se shranijo matrike meta problemov
						OkoljeSosedi = paste0(OkoljeMetaPrblm, '/matrikaSosedi'), ## okolje kamor naj se shrani matrike sosedov
						
						## dodatni parametri ucenja meta klasifikatorja
						hC = c(0.95, 1), ## meja razlicnosti napovedi baseLearnerjev za vkljucitev v podatke za meta ucenje
						hC_ONE = c(0.8,1), ## meja razlicnosti napovedi baseLearnerjev za vkljucitev v podatke za meta ucenje
						MetaMode = 'individual', ## 'one'
						metaALG = c('rf', 'gbm'), ## algoriti uproabljeni za ucenje meta problema
						OkoljeMetaKlasifikator, ## okolje kamor se shranijo meta klasifikatorji
						
						## dodatni parametri za napovedovanje META-DES
						imenaMnozicVALID = c('trainBL', 'metaSet', 'validSet'),
						validSet,
						yValidSet,
						# OP_valid,
						sosedSet_VALID = metaSet,
						ySosedSet_VALID = yMetaSet,
						# OP_sosedi_VALID = OP_meta, ## za iskanje sosedov na validaciji je uporabljena ista mnozica kot metaSet pri ucenju
						OkoljeMetaPrblm_VALID,
						
						## napovedi meta klasifikatorja in ensembla
						OkoljeKompetentnost, ## okolje kamor se shranijo matrike kompetentnosti
						kompetThrsh = c(0.5, 0.8), ## meja kompetentnosti za vklucitev v ensemble
						OkoljeNapovedEnsemble ## okolje kamor se shranijo napovedi ensembla
						){

	
	## UCENJE BASE LEARNERJEV #################################################
	###########################################################################


	for(bl in baseLearner){
		setwd(OkoljeBaseLearner)
		ime <- paste0(bl, '.rds')
		if(ime %in% dir()){
			print(paste(bl, 'ze naucen'))
		}else{
			BLmodel <- train(trainBL, yTrainBL, method = bl)
			setwd(OkoljeBaseLearner)
			ime <- paste0(bl, '.rds')
			saveRDS(BLmodel, ime)
		}
		
	}

	## NAPOVEDI BASE LEARNERJEV ###############################################
	###########################################################################						

	# i <- 2
	# bl <- baseLearner[i]
	# warnings()
	# http://r.789695.n4.nabble.com/klaR-package-NaiveBayes-warning-message-numerical-0-probability-td3025567.html
	
	OP_sosedi <- data.frame(matrix(NA, length(ySosedSet), 0))
	OP_meta <- data.frame(matrix(NA, length(yMetaSet), 0))
	OP_valid <- data.frame(matrix(NA, length(yValidSet), 0))
	
	for(bl in baseLearner){
		imeBL <-  paste0(bl, '.rds')
		imeStolpca <- paste0(bl, '_', levels(yTrainBL))
		setwd(OkoljeBaseLearner)
		model <- readRDS(imeBL)
		napoved <- predict(model, sosedSet, type = 'prob')
		OP_sosedi[, imeStolpca] <- napoved
		napoved <- predict(model, metaSet, type = 'prob')
		OP_meta[, imeStolpca] <- napoved
		napoved <- predict(model, validSet, type = 'prob')
		OP_valid[, imeStolpca] <- napoved
	}
	

	setwd(OkoljeRezultatOutputProfile)
	saveRDS(OP_sosedi, 'OP_sosedi_verjetnost.rds')
	saveRDS(OP_meta, 'OP_meta_verjetnost.rds')
	saveRDS(OP_valid, 'OP_valid_verjetnost.rds')
	
	
	
	## SESTAVLJANJE META PROBLEMA #############################################
	###########################################################################
	## in: SosedSed, MetaSet, OP_SosedSet?, OP_MetaSet,
	##		K, Kp, metrika, folder, baseLearner
	## out: metaFM, metaResponse 
	
	## delovanje: pogleda v folder, ce so potrebne metaProblemi ze izracunani, 
	## ce da vrne ustrezno, sicer izracuna in shrani v folder
	
	## razlicni K, Kp, hC, nacini iskanja sosedov
	## za vsak K, Kp svoj problem, 
	## hC pobres ven iz meta problema relavantne
	metaProblemSAVE(imenaMnozic[1:3], ## ime mnozice: [sosede katerih iscemo, iz katere so sosedje]
						metaSet, ## mnozica iz katere se sestavi meta problem
						yMetaSet, ## response vektor meta seta
							### 'n' za sestavitev metaFM, brez meta respons vektorja 
						sosedSet, ## mnozica iz katere so izbrane sosedi za metaSet
						ySosedSet, ## response vektor sosed seta
						baseLearner, ## ime base learnerjev (vektor)
						OP_meta, ## output profile (verjetnosti) za mnozico metaSet 
							## imena stolpcev: baseLearer1_class(1) baseLearner1_ class(2) ... baseLearner1_class(n) ... baseLearnerK_class(n)
						OP_sosedi, ## output profile (verjetnosti) za mnozico sosedSet
							## imena stolpcev: baseLearer1_class(1) baseLearner1_ class(2) ... baseLearner1_class(n) ... baseLearnerK_class(n)
						K, ## stevilo sosedov za region of competence (vektor)
						Kp, ## stevilo sosedov iz output profile (vektor)
						knnALG, ## algoritem po katerem get.knnx isce sosede
						OkoljeMetaPrblm, ## okolje kamor naj se shranijo matrike meta problemov
						OkoljeSosedi ## okolje kamor naj se shrani matrike sosedov
						)
	

	## UCENJE META KLASIFIKATORJA #############################################
	###########################################################################
	## za razlicne K,Kp, knnALG
	## za razlicne hC, razlicni klasifikatorji [log reg, elm, ...], mode = one ali individual
	
	## imena mnozic 
	## stevec poteka: izracunal 50% primerov
	############################
	
	
	## model za vsak baseLearner
	if('individual' %in% MetaMode){
		############################
		ucenjeMetaKlasifikator(imenaMnozic[1:3], ## ime mnozice: [sosede katerih iscemo, iz katere so sosedje]
							OP_meta, ## output profile (verjetnosti) za mnozico metaSet 
									## imena stolpcev: baseLearer1_class(1) baseLearner1_ class(2) ... baseLearner1_class(n) ... baseLearnerK_class(n)
							classOsnovni =  levels(ySosedSet), ## imena classov pri osnovnem problemu kot vektor (npr: c('neg', 'pos')
							baseLearner, ## ime base learnerjev (vektor)
							K, ## stevilo sosedov za region of competence (vektor)
							Kp, ## stevilo sosedov iz output profile (vektor)
							knnALG, ## algoritem po katerem get.knnx isce sosede
							metaALG, ## klasifikacijski algoritmi uporabljeni pri ucenju meta klasifikatorja
							hC, ## kako razlicne morajo biti napovedi base learnerjev, za x iz metaSet da ga vkljucimo v ucenje
							OkoljeMetaPrblm, ## okolje kjer so shranjene metaProblemi 
							OkoljeMetaKlasifikator ## okolje, kamor se shranijo meta klasifikatorji
							)
		############################		
	}

	if('one' %in% MetaMode){
	############################
	ucenjeMetaKlasifikatorONE(imenaMnozic[1:3], ## ime mnozice: [sosede katerih iscemo, iz katere so sosedje]
						OP_meta, ## output profile (verjetnosti) za mnozico metaSet 
								## imena stolpcev: baseLearer1_class(1) baseLearner1_ class(2) ... baseLearner1_class(n) ... baseLearnerK_class(n)
						classOsnovni =  levels(ySosedSet), ## imena classov pri osnovnem problemu kot vektor (npr: c('neg', 'pos')
						baseLearner, ## ime base learnerjev (vektor)
						K, ## stevilo sosedov za region of competence (vektor)
						Kp, ## stevilo sosedov iz output profile (vektor)
						knnALG, ## algoritem po katerem get.knnx isce sosede
						metaALG, ## klasifikacijski algoritmi uporabljeni pri ucenju meta klasifikatorja
						hC_ONE, ## kako razlicne morajo biti napovedi base learnerjev, za x iz metaSet da ga vkljucimo v ucenje
						OkoljeMetaPrblm, ## okolje kjer so shranjene metaProblemi 
						OkoljeMetaKlasifikator ## okolje, kamor se shranijo meta klasifikatorji
							)
	############################
	}
	
	## IZRACUN MATRIKE KOMPETENTNOSTI #########################################
	###########################################################################
	
	## izracunaj metaFM na validSet, shrani v mapo metaFM validSet
	
	## preglej retultate vseh meta klasifikatorjev kombinacij (za bl) 
	
	
	## ALI JE SMISELNO SHRANJEVATI metaProbleme (cas loadanja vs cas racunanja) 30/1 sekund
	## IZRACUN IN SHRANITEV metaFM
	
	
	metaProblemSAVE(imenaMnozicVALID, ## ime mnozice: [sosede katerih iscemo, iz katere so sosedje]
					metaSet = validSet, ## mnozica iz katere se sestavi meta problem
					yMetaSet = yValidSet, ## response vektor meta seta
						### 'n' za sestavitev metaFM, brez meta respons vektorja 
					sosedSet = sosedSet_VALID, ## mnozica iz katere so izbrane sosedi za metaSet
					ySosedSet = ySosedSet_VALID, ## response vektor sosed seta
					baseLearner, ## ime base learnerjev (vektor)
					OP_meta = OP_valid, ## output profile (verjetnosti) za mnozico metaSet 
						## imena stolpcev: baseLearer1_class(1) baseLearner1_ class(2) ... baseLearner1_class(n) ... baseLearnerK_class(n)
					OP_sosedi = OP_meta, ## output profile (verjetnosti) za mnozico sosedSet
						## imena stolpcev: baseLearer1_class(1) baseLearner1_ class(2) ... baseLearner1_class(n) ... baseLearnerK_class(n)
					K, ## stevilo sosedov za region of competence (vektor)
					Kp, ## stevilo sosedov iz output profile (vektor)
					knnALG, ## algoritem po katerem get.knnx isce sosede
					OkoljeMetaPrblm = OkoljeMetaPrblm_VALID, ## okolje kamor naj se shranijo matrike meta problemov
					OkoljeSosedi = paste0(OkoljeMetaPrblm_VALID, '/matrikaSosedi') ## okolje kamor naj se shrani matrike sosedov
					)
					

	if('individual' %in% MetaMode){
		## izracun kompetentnosti klasifikatorja (vsak)
		##############################################
		for(nSosedi in K){
			for(OPnSosedi in Kp){
				for(alg in knnALG){
					for(algM in metaALG){
						for(meja in hC){
							kompetentnost <- NULL
							for(bl in baseLearner){
								## loadanje metaProblema
								setwd(OkoljeMetaPrblm_VALID)
								imeMetaProblem <- paste0('matrikaProblem[BL]', bl,'[trainBL]', imenaMnozicVALID[1],
														'[sosedSet]',imenaMnozicVALID[2], '[metaSet]',imenaMnozicVALID[3],
														'[K]',nSosedi, '[Kp]', OPnSosedi, '[knnALG]', alg, '.rds')
								metaFM <- readRDS(imeMetaProblem)$metaFM
								## loadanje meta klasifikatorja
								setwd(OkoljeMetaKlasifikator)
								imeMetaKlasifikator <- paste0('metaKlasifikator[BL]', bl,'[trainBL]', imenaMnozic[1],'[sosedSet]',
																	imenaMnozic[2],'[metaSet]', imenaMnozic[3], '[K]',nSosedi, 
																	'[Kp]',OPnSosedi, '[knnALG]', alg, '[metaALG]', algM, '[cH]', meja, '.rds')
								metaKlasifikator <- readRDS(imeMetaKlasifikator)
								## napovedi meta klasifikatorja
								napoved <- predict(metaKlasifikator, metaFM, type = 'prob')[2]
								kompetentnost <- cbind(kompetentnost, as.matrix(napoved))
							}
							kompetentnost <- data.frame(kompetentnost)
							colnames(kompetentnost) <- baseLearner
							## shranjevanje matrike kompetentnosti
							setwd(OkoljeKompetentnost)
							imeKompetentnost <- paste0('kompetentnost[trainBL]', imenaMnozic[1],'[sosedSet]',
																	imenaMnozic[2], '[metaSet]',imenaMnozic[3],
																	'[validSet]',imenaMnozic[4], '[K]',nSosedi, 
																	'[Kp]',OPnSosedi, '[knnALG]', alg, 
																	'[metaALG]', algM, '[cH]', meja, '.rds')
							saveRDS(kompetentnost, imeKompetentnost)
						}				
					}
				}
			}
		}
		
# N <- napoved
# N[napoved < 0.7] <- 'N'
# N[napoved >= 0.7] <- 'Y'
# N <- factor(N[,1])
# table(N, metaProblem$metaClass)
# confusionMatrix(table(N, metaProblem$metaClass))		
		## napovedi ensembla
		##############################################

		for(nSosedi in K){
			for(OPnSosedi in Kp){
				for(alg in knnALG){
					for(algM in metaALG){
						for(meja in hC){
							## loadanje matrik kompetentnosti
							setwd(OkoljeKompetentnost)
							imeKompetentnost <- paste0('kompetentnost[trainBL]', imenaMnozic[1],'[sosedSet]',
																	imenaMnozic[2], '[metaSet]',imenaMnozic[3],
																	'[validSet]',imenaMnozic[4], '[K]',nSosedi, 
																	'[Kp]',OPnSosedi, '[knnALG]', alg, 
																	'[metaALG]', algM, '[cH]', meja, '.rds')
							kompetentnost <- readRDS(imeKompetentnost)
							## sestavljanje napovedi ensembla
							imena <- paste0(baseLearner, '_', levels(ySosedSet_VALID)[1])
							napovedBL <- OP_valid[, imena]
							for(k in kompetThrsh){
								ensembleNapoved <- napovedEnsemble(napovedBL,
																	kompetentnost,
																	k,
																	bup = 4)
								setwd(OkoljeNapovedEnsemble)
								imeNapovedEns <- paste0('napovedEns[trainBL]', imenaMnozic[1],'[sosedSet]',
																	imenaMnozic[2], '[metaSet]',imenaMnozic[3],
																	'[validSet]',imenaMnozic[4], '[K]',nSosedi, 
																	'[Kp]',OPnSosedi, '[knnALG]', alg, 
																	'[metaALG]', algM, '[cH]', meja, '.rds')
								saveRDS(ensembleNapoved,imeNapovedEns)
							}
						}				
					}
				}
			}
		}
		

		## pravilnost napovedi, shrani v eno matriko (vsi parametri)
		##############################################
		imeStolpcev <- c('trainBL' ,'sosedSet', 'metaSet', 'validSet', 'K', 'Kp', 'knnALG', 
					'metaALG', 'hC', 'kompetThrsh', 'Accuracy', 'Sensitivity', 'Specificity')
		rezultati <- data.frame(matrix(NA, 0,length(imeStolpcev)))
		colnames(rezultati) <- imeStolpcev
		
		for(nSosedi in K){
			for(OPnSosedi in Kp){
				for(alg in knnALG){
					for(algM in metaALG){
						for(meja in hC){
							for(k in kompetThrsh){
								setwd(OkoljeNapovedEnsemble)
								imeNapovedEns <- paste0('napovedEns[trainBL]', imenaMnozic[1],'[sosedSet]',
																	imenaMnozic[2], '[metaSet]',imenaMnozic[3],
																	'[validSet]',imenaMnozic[4], '[K]',nSosedi, 
																	'[Kp]',OPnSosedi, '[knnALG]', alg, 
																	'[metaALG]', algM, '[cH]', meja, '.rds')
								napovedEnsembla <- readRDS(imeNapovedEns)
								napovedClass <- napovedEnsembla
								thr <- 0.5
								napovedClass[napovedEnsembla < thr] <- levels(ySosedSet_VALID)[2]
								napovedClass[napovedEnsembla >= thr] <- levels(ySosedSet_VALID)[1]
								napovedClass <- factor(napovedClass)
								A <- confusionMatrix(table(napovedClass, yValidSet))
								# print(A)
						
								rezultati[nrow(rezultati)+1,] <- c(imenaMnozic[1], imenaMnozic[2], imenaMnozic[3],
																	imenaMnozic[4],	nSosedi, OPnSosedi, alg, algM,
																	meja, k, A[[3]]['Accuracy'], A[[4]]['Sensitivity'],
																	A[[4]]['Specificity'])	
							}
						}				
					}
				}
			}
		}
			

		
		setwd(OkoljeAccuracy)
		ime <- paste0('accuracy[trainBL]', imenaMnozic[1],'[sosedSet]',	imenaMnozic[2],
						'[metaSet]',imenaMnozic[3], '[validSet]',imenaMnozic[4],'.rds')
		saveRDS(rezultati, ime)
	}
	
	if('one' %in% MetaMode){
		## izracun kompetentnosti klasifikatorja (vsak)
		##############################################
		for(nSosedi in K){
			for(OPnSosedi in Kp){
				for(alg in knnALG){
					for(algM in metaALG){
						for(meja in hC){
							kompetentnost <- NULL
							for(bl in baseLearner){
								## loadanje metaProblema
								setwd(OkoljeMetaPrblm_VALID)
								imeMetaProblem <- paste0('matrikaProblem[BL]', bl,'[trainBL]', imenaMnozicVALID[1],
														'[sosedSet]',imenaMnozicVALID[2], '[metaSet]',imenaMnozicVALID[3],
														'[K]',nSosedi, '[Kp]', OPnSosedi, '[knnALG]', alg, '.rds')
								metaFM <- readRDS(imeMetaProblem)$metaFM
								## loadanje meta klasifikatorja
								setwd(OkoljeMetaKlasifikator)
								imeMetaKlasifikator <- paste0('metaKlasifikator[ONE]','[trainBL]', imenaMnozic[1],'[sosedSet]',
																	imenaMnozic[2],'[metaSet]', imenaMnozic[3], '[K]',nSosedi, 
																	'[Kp]',OPnSosedi, '[knnALG]', alg, '[metaALG]', algM, '[cH]', meja, '.rds')
								metaKlasifikator <- readRDS(imeMetaKlasifikator)
								## napovedi meta klasifikatorja
								napoved <- predict(metaKlasifikator, metaFM, type = 'prob')[2]
								kompetentnost <- cbind(kompetentnost, as.matrix(napoved))
							}
							kompetentnost <- data.frame(kompetentnost)
							colnames(kompetentnost) <- baseLearner
							## shranjevanje matrike kompetentnosti
							setwd(OkoljeKompetentnost)
							imeKompetentnost <- paste0('kompetentnostONE[trainBL]', imenaMnozic[1],'[sosedSet]',
																	imenaMnozic[2], '[metaSet]',imenaMnozic[3],
																	'[validSet]',imenaMnozic[4], '[K]',nSosedi, 
																	'[Kp]',OPnSosedi, '[knnALG]', alg, 
																	'[metaALG]', algM, '[cH]', meja, '.rds')
							saveRDS(kompetentnost, imeKompetentnost)
						}				
					}
				}
			}
		}
		

		
		## napovedi ensembla
		##############################################

		for(nSosedi in K){
			for(OPnSosedi in Kp){
				for(alg in knnALG){
					for(algM in metaALG){
						for(meja in hC){
							## loadanje matrik kompetentnosti
							setwd(OkoljeKompetentnost)
							imeKompetentnost <- paste0('kompetentnostONE[trainBL]', imenaMnozic[1],'[sosedSet]',
																	imenaMnozic[2], '[metaSet]',imenaMnozic[3],
																	'[validSet]',imenaMnozic[4], '[K]',nSosedi, 
																	'[Kp]',OPnSosedi, '[knnALG]', alg, 
																	'[metaALG]', algM, '[cH]', meja, '.rds')
							kompetentnost <- readRDS(imeKompetentnost)
							## sestavljanje napovedi ensembla
							imena <- paste0(baseLearner, '_', levels(ySosedSet_VALID)[1])
							napovedBL <- OP_valid[, imena]
							for(k in kompetThrsh){
								ensembleNapoved <- napovedEnsemble(napovedBL,
																	kompetentnost,
																	k,
																	bup = 4)
								setwd(OkoljeNapovedEnsemble)
								imeNapovedEns <- paste0('napovedEnsONE[trainBL]', imenaMnozic[1],'[sosedSet]',
																	imenaMnozic[2], '[metaSet]',imenaMnozic[3],
																	'[validSet]',imenaMnozic[4], '[K]',nSosedi, 
																	'[Kp]',OPnSosedi, '[knnALG]', alg, 
																	'[metaALG]', algM, '[cH]', meja, '.rds')
								saveRDS(ensembleNapoved,imeNapovedEns)
							}
						}				
					}
				}
			}
		}
		

		## pravilnost napovedi, shrani v eno matriko (vsi parametri)
		##############################################
		imeStolpcev <- c('trainBL' ,'sosedSet', 'metaSet', 'validSet', 'K', 'Kp', 'knnALG', 
					'metaALG', 'hC', 'kompetThrsh', 'Accuracy', 'Sensitivity', 'Specificity')
		rezultati <- data.frame(matrix(NA, 0,length(imeStolpcev)))
		colnames(rezultati) <- imeStolpcev
		
		for(nSosedi in K){
			for(OPnSosedi in Kp){
				for(alg in knnALG){
					for(algM in metaALG){
						for(meja in hC){
							for(k in kompetThrsh){
								setwd(OkoljeNapovedEnsemble)
								imeNapovedEns <- paste0('napovedEnsONE[trainBL]', imenaMnozic[1],'[sosedSet]',
																	imenaMnozic[2], '[metaSet]',imenaMnozic[3],
																	'[validSet]',imenaMnozic[4], '[K]',nSosedi, 
																	'[Kp]',OPnSosedi, '[knnALG]', alg, 
																	'[metaALG]', algM, '[cH]', meja, '.rds')
								napovedEnsembla <- readRDS(imeNapovedEns)
								napovedClass <- napovedEnsembla
								thr <- 0.5
								napovedClass[napovedEnsembla < thr] <- levels(ySosedSet_VALID)[2]
								napovedClass[napovedEnsembla >= thr] <- levels(ySosedSet_VALID)[1]
								napovedClass <- factor(napovedClass)
								A <- confusionMatrix(table(napovedClass, yValidSet))
								# print(A)
						
								rezultati[nrow(rezultati)+1,] <- c(imenaMnozic[1], imenaMnozic[2], imenaMnozic[3],
																	imenaMnozic[4],	nSosedi, OPnSosedi, alg, algM,
																	meja, k, A[[3]]['Accuracy'], A[[4]]['Sensitivity'],
																	A[[4]]['Specificity'])	
							}
						}				
					}
				}
			}
		}
			

		
		setwd(OkoljeAccuracy)
		ime <- paste0('accuracyONE[trainBL]', imenaMnozic[1],'[sosedSet]',	imenaMnozic[2],
						'[metaSet]',imenaMnozic[3], '[validSet]',imenaMnozic[4],'.rds')
		saveRDS(rezultati, ime)
	}
	
}
						
			
