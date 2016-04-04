library(FNN)

## razlicne funkcije s katerimi izbere zgornja funkcija izbere ali bo x vkljucen v sestavljanje FM ali ne



metaProblem <- function(metaSet, ## mnozica iz katere se sestavi meta problem
						yMetaSet = 'n', ## response vektor meta seta
							### 'n' za sestavitev metaFM, brez meta respons vektorja
						sosedSet, ## mnozica iz katere so izbrane sosedi za metaSet
						ySosedSet, ## response vektor sosed seta
						baseLearner, ##
						OP_meta, ## output profile (verjetnosti) za mnozico metaSet 
							## imena stolpcev: baseLearer1_class(1) baseLearner1_ class(2) ... baseLearner1_class(n) ... baseLearnerK_class(n)
						OP_sosedi, ## output profile (verjetnosti) za mnozico sosedSet
							## imena stolpcev: baseLearer1_class(1) baseLearner1_ class(2) ... baseLearner1_class(n) ... baseLearnerK_class(n)
						K = 5, ## stevilo sosedov za region of competence
						Kp = 5 ## stevilo sosedov iz output profile
						)
						## funkcija, ki sestavi meta feature matriko in 
						## response vektor metaproblema (kot list)
						
						## ce je yMetaSet == 'n' vrne samo meta feature matriko (kot matriko)
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
	
	
	## ISKANJE SOSEDVO
	## region of competence
	kNN_RC <- get.knnx(sosedSet, metaSet,  k=K, algorith = "kd_tree")
		## algorithm=c("kd_tree", "cover_tree", "brute")
	matrika_sosediRC <- kNN_RC$nn.index
	
	## sosedi na output profile
	kNN_OP <- get.knnx(OP_sosedi, OP_meta,  k=Kp, algorith = "kd_tree")
	matrika_sosediOP <- kNN_OP$nn.index
	
	## SESTAVLJANJE FM
	metaFM <- NULL
	for(i in 1:nrow(metaSet)){
		## region of competence
		sosediRC <- matrika_sosediRC[i,]
		## sosedi po output profilu
		sosediOP <- matrika_sosediOP[i,]


		## napovedi base learnerjev za region of competence (sosediRC)
			imeBL <- paste0(baseLearner, '_', levels(ySosedSet))
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
		
		## f6: oddaljenost od sosedov
			
		metaFM <- rbind(metaFM, c(f1,f2,f3,f4))	
		colnames(metaFM) <- c(paste0('f1_', 1:length(f1)), paste0('f2_', 1:length(f2)), paste0('f3_', 1:length(f3)), paste0('f4_', 1:length(f4)))
	}
	
	### Ce zelimo samo metaFM (brez metaClass = response vektro)
	if(is.character(yMetaSet)){
		return(metaFM)	
	}else{
		## meta class
		## napovedani class baseLearner-ja
		imeBL <- paste0(baseLearner, '_', levels(yMetaSet))
		klas <- apply(OP_meta[, imeBL], 1, which.max)
		napovedanClass_metaSet <- NULL
		for(i in klas){
			napovedanClass_metaSet <- c(napovedanClass_metaSet, levels(yMetaSet)[i])
		}
		napovedanClass_metaSet <- factor(napovedanClass_metaSet, levels = levels(yMetaSet))
		
		
		metaClass <- as.numeric(yMetaSet == napovedanClass_metaSet, levels = c(''))
		metaClass[metaClass == 0] <- 'N'
		metaClass[metaClass == 1] <- 'Y'
		metaClass <- factor(metaClass)
		metaPrblm <- list('metaFM' = metaFM, 'metaClass' = metaClass)
		return(metaPrblm)	
	}
	
	
	
}


