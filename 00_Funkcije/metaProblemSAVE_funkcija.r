## shrani matriko sosednosti (v posebno mapo), poglej ce ze obstaja za:
	## SosedSet, MetaSet, K, metrika
	## OP_SosedSet, OP_MetaSet, Kp, metrika


library(FNN)

## razlicne funkcije s katerimi izbere zgornja funkcija izbere ali bo x vkljucen v sestavljanje FM ali ne


metaProblemSAVE <- function(imenaMnozic = c('trainBL','sosedSet', 'metaSet'), ## ime mnozice: [sosede katerih iscemo, iz katere so sosedje]
						metaSet, ## mnozica iz katere se sestavi meta problem
						yMetaSet = 'n', ## response vektor meta seta
							### 'n' za sestavitev metaFM, brez meta respons vektorja 
						sosedSet, ## mnozica iz katere so izbrane sosedi za metaSet
						ySosedSet, ## response vektor sosed seta
						baseLearner, ## ime base learnerjev (vektor)
						OP_meta, ## output profile (verjetnosti) za mnozico metaSet 
							## imena stolpcev: baseLearer1_class(1) baseLearner1_ class(2) ... baseLearner1_class(n) ... baseLearnerK_class(n)
						OP_sosedi, ## output profile (verjetnosti) za mnozico sosedSet
							## imena stolpcev: baseLearer1_class(1) baseLearner1_ class(2) ... baseLearner1_class(n) ... baseLearnerK_class(n)
						K = 5, ## stevilo sosedov za region of competence (vektor)
						Kp = 5, ## stevilo sosedov iz output profile (vektor)
						knnALG = "kd_tree", ## algoritem po katerem get.knnx isce sosede
						OkoljeMetaPrblm, ## okolje kamor naj se shranijo matrike meta problemov
						OkoljeSosedi = paste0(OkoljeMetaPrblm, '/matrikaSosedi') ## okolje kamor naj se shrani matrike sosedov
						)
						## funkcija, ki sestavi meta feature matriko in 
						## response vektor metaproblema (kot list) za vse K, Kp, knnALG in 
						## baseLearner in jih shrani v OkoljeMetaPrblm (ce je tam se ni)
						
						## ce je yMetaSet == 'n' vrne samo meta feature matriko (kot matriko)
						## ce sta mnozici metaSet in sosedSet enaki poisce sosede znotraj preostale mnozice 
						{
						
						
	
	## preveri ali je baseLearner med imeni output profilov in ali OP 
	
	
	if(!(all(paste0(baseLearner, '_', levels(ySosedSet)) %in% colnames(OP_meta)))){
		stop(paste('noben base learner med output profili nima imena', baseLearner,'ali pa so classi v ySosedSet in OP_meta razlicni'))
	}
	
	## preveri ali so imena stolpcev output profilov enaki
	if(!(setequal(colnames(OP_meta), colnames(OP_sosedi)))){
				stop('output profili imajo razlicne imena stolpcev')
	}
	## preveri ce je dimenzija OP_meta enak dimenziji metaSet
	if(nrow(metaSet) != nrow(OP_meta)){
				stop('metaSet in OP_meta imata razlicno stevilo vrstic!!')
	}
	
	dir.create(OkoljeMetaPrblm)
	dir.create(OkoljeSosedi)
	
	for(nSosedi in K){
		for(OPnSosedi in Kp){
			for(alg in knnALG){
				imeSosedi <- paste0('matrikaSosedje[trainBL]',imenaMnozic[1] ,'[sosedSet]',imenaMnozic[2], 
									'[metaSet]', imenaMnozic[3], '[K]',nSosedi, '[knnALG]', alg, '.rds')
				imeSosediOP <- paste0('matrikaSosedje_OP[trainBL]', imenaMnozic[1],'[sosedSet]',imenaMnozic[2], 
									'[metaSet]',imenaMnozic[3], '[Kp]',OPnSosedi, '[knnALG]', alg, '.rds')
				############################################################
				## ISKANJE SOSEDVO
				setwd(OkoljeSosedi)
				if(!(imeSosedi %in% dir())){
					## region of competence
					if(all(metaSet == sosedSet)){
						kNN_RC <- get.knnx(sosedSet, metaSet,  k=nSosedi + 1, algorith = alg)
						matrika_sosediRC <- kNN_RC$nn.index[, -1]
					}else{
						kNN_RC <- get.knnx(sosedSet, metaSet,  k=nSosedi, algorith = alg)
						matrika_sosediRC <- kNN_RC$nn.index
					}
					saveRDS(matrika_sosediRC, imeSosedi)
				}else{
					matrika_sosediRC <- readRDS(imeSosedi)
				}

				if(!(imeSosediOP %in% dir())){
					## sosedi na output profile
					if(all(OP_sosedi == OP_meta)){
						kNN_OP <- get.knnx(OP_sosedi, OP_meta,  k=nSosedi + 1, algorith = alg)
						matrika_sosediOP <- kNN_OP$nn.index[, -1]
					}else{
						kNN_OP <- get.knnx(OP_sosedi, OP_meta,  k=nSosedi, algorith = alg)
						matrika_sosediOP <- kNN_OP$nn.index
					}
					saveRDS(matrika_sosediOP, imeSosediOP)
				}else{
					matrika_sosediOP <- readRDS(imeSosediOP)
				}
			
				############################################################
				## SESTAVLJANJE FM
				for(bl in baseLearner){
					## ali je ze shranjen 
					setwd(OkoljeMetaPrblm)
					imeMetaFM <- paste0('metaFM[BL]', bl,'[trainBL]', imenaMnozic[1],'[sosedSet]',
										imenaMnozic[2], '[sosedSet]',imenaMnozic[3],'[K]',nSosedi,
										'[Kp]',OPnSosedi, '[knnALG]', alg, '.rds')
					imeMetaProblem <- paste0('matrikaProblem[BL]', bl,'[trainBL]', imenaMnozic[1],
											'[sosedSet]',imenaMnozic[2], '[metaSet]',imenaMnozic[3], 
											'[K]',nSosedi, '[Kp]',OPnSosedi, '[knnALG]', alg, '.rds')
					if(is.character(yMetaSet) & imeMetaFM %in% dir()){
						print(paste(imeMetaFM, 'JE ZE SHRANJEN!!'))
					}else if(imeMetaProblem %in% dir()){
						print(paste(imeMetaProblem, 'JE ZE SHRANJEN!!'))
					} else {
					## ce se ni shranjen se izracuna metaFM
						metaFM <- NULL
						for(i in 1:nrow(metaSet)){
							if(i %% 1000 == 0){
								print(paste0(100*i/nrow(metaSet), '% matrike sestavljene'))
							}
							## region of competence
							sosediRC <- matrika_sosediRC[i,]
							## sosedi po output profilu
							sosediOP <- matrika_sosediOP[i,]
							## napovedi base learnerjev za region of competence (sosediRC)
								imeBL <- paste0(bl, '_', levels(ySosedSet))
								## verjetnosti posameznega classa (sosedov iz region of competences)
									sosedi_verjetnostRC <- OP_sosedi[sosediRC, imeBL]
								## napoved classa (sosedov iz region of competences)
									klas <- apply(sosedi_verjetnostRC, 1, which.max)
									sosedi_classRC <- NULL
									for(i in klas){
										sosedi_classRC <- c(sosedi_classRC, levels(ySosedSet)[i])
									}
									## kateri class napove klasifikator baseLearner na sosedih
									sosedi_classRC <- factor(sosedi_classRC, levels = levels(ySosedSet))

							## napovedi base learnerjev za sosede na output profil (sosediOP)
								sosedi_verjetnostOP <- OP_sosedi[sosediOP, imeBL]
								klas <- apply(sosedi_verjetnostOP, 1, which.max)
								sosedi_classOP <- NULL
								for(i in klas){
									sosedi_classOP <- c(sosedi_classOP, levels(ySosedSet)[i])
								}
								## kateri class napove klasifikator baseLearner na sosedih
								sosedi_classOP <- factor(sosedi_classOP, levels = levels(ySosedSet))
											
							
							## FEATURE
							## f1: pravilna/napacna napoved baseLearner na region of competence (sosediRC)
							f1 <- as.numeric(sosedi_classRC == ySosedSet[sosediRC])

							## f2: verjetnosti na region of competence (sosediRC)
							f2 <- NULL
							for(i in 1:nrow(sosedi_verjetnostRC)){
								f2 <- c(f2, sosedi_verjetnostRC[i, ySosedSet[sosediRC][i]])
							}	
							
							## f3: accuracy na celotni region of competence (sosediRC)
							f3 <- sum(as.numeric(sosedi_classRC == ySosedSet[sosediRC]))/length(sosediRC)
							
							## f4: pravilno/napacna napoved baseLearner na sosedih po output profile 
							f4 <- as.numeric(sosedi_classOP == ySosedSet[sosediOP])
							
							## f5: razdalja med ...??
							f5 <- as.numeric(OP_meta[i,imeBL][2])

							metaFM <- rbind(metaFM, c(f1,f2,f3,f4, f5))	
							colnames(metaFM) <- c(paste0('f1_', 1:length(f1)), paste0('f2_', 1:length(f2)),
													paste0('f3_', 1:length(f3)), paste0('f4_', 1:length(f4)),
													paste0('f5_', 1:length(f5)))
						}
						
						### Ce zelimo samo metaFM (brez metaClass = response vektro)
						if(is.character(yMetaSet)){						
							saveRDS(metaFM, imeMetaFM)
							print(paste('shranila se je metaFM matrika:', imeMetaFM))
						}else{
							## meta class
							## napovedani classa baseLearner-ja
							imeBL <- paste0(bl, '_', levels(yMetaSet))
							klas <- apply(OP_meta[, imeBL], 1, which.max)
							napovedanClass_metaSet <- levels(yMetaSet)[klas]
							napovedanClass_metaSet <- factor(napovedanClass_metaSet, levels = levels(yMetaSet))
							
							
							metaClass <- as.numeric(yMetaSet == napovedanClass_metaSet, levels = c(''))
							metaClass[metaClass == 0] <- 'N'
							metaClass[metaClass == 1] <- 'Y'
							metaClass <- factor(metaClass)
							metaPrblm <- list('metaFM' = metaFM, 'metaClass' = metaClass)
							saveRDS(metaPrblm, imeMetaProblem)
							print(paste('shranila se je metaProblem list:', imeMetaProblem))	
						}
					}
				## baselearner
				}
			## knnALG
			}
		## Kp
		}
	## K
	}
}


