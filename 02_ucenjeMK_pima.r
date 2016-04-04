## ucenje meta-klasifikatorja

OkoljePodatki <- 'C:/Users/Podlogar/Documents/DES_eksperiment/WP/Podatki'
Okolje_OP <- 'C:/Users/Podlogar/Documents/DES_eksperiment/WP/BaseLearner/OP'
OkoljeShraniMetaKlasifikator <- 'C:/Users/Podlogar/Documents/DES_eksperiment/WP/MetaKlasifikator'
dir.create(OkoljeShraniMetaKlasifikator)


OkoljeFunkcije <- 'C:/Users/Podlogar/Documents/DES_eksperiment/WP/00_Funkcije'
setwd(OkoljeFunkcije)
source('metaProblemSAVE_funkcija.r')
source('ucenjeMetaKlasifikatorONE_funkcija.r')
source('probToClass_funkcija.r')

stModelov <- 10
K <- 7
Kp <- 5
hC <- 0.7
metaALG <- 'rf'

## priprava meta problemov
for(del in 1:20){
	setwd(OkoljePodatki)
	podatki <- readRDS(paste0('razdeljeniPodatki_Del_', del , '.rds'))
	
	imenaMnozic <- paste0(c('Fold1','Fold2', 'Fold2'), '_Podatki', del)
	metaSet <- podatki[['Fold2_FM']]
	yMetaSet <- podatki[['Fold2_Y']]
	
	sosedSet <- podatki[['Fold2_FM']]
	ySosedSet <- podatki[['Fold2_Y']]
	
	baseLearner <-  paste0('model_', 1:stModelov)
	
	OkoljeOP_del <- paste0(Okolje_OP, '/podatki', del)
	setwd(OkoljeOP_del)
	OP_meta <- readRDS(paste0('OP_metaTrain_', del,'.rds'))
	OP_sosedi <- readRDS(paste0('OP_metaTrain_', del,'.rds'))
	
	OkoljeMetaPrblm <- paste0(OkoljeShraniMetaKlasifikator, '/podatki', del, '/metaProblem')
	dir.create(OkoljeMetaPrblm, recursive = TRUE)
	
	metaProblemSAVE(imenaMnozic, ## ime mnozice: [sosede katerih iscemo, iz katere so sosedje]
					metaSet, ## mnozica iz katere se sestavi meta problem
					yMetaSet, ## response vektor meta seta
					sosedSet, ## mnozica iz katere so izbrane sosedi za metaSet
					ySosedSet, ## response vektor sosed seta
					baseLearner, ## ime base learnerjev (vektor)
					OP_meta, ## output profile (verjetnosti) za mnozico metaSet 
					OP_sosedi, ## output profile (verjetnosti) za mnozico sosedSet
					K, ## stevilo sosedov za region of competence (vektor)
					Kp, ## stevilo sosedov iz output profile (vektor)
					knnALG = "kd_tree", ## algoritem po katerem get.knnx isce sosede
					OkoljeMetaPrblm, ## okolje kamor naj se shranijo matrike meta problemov
					OkoljeSosedi = paste0(OkoljeMetaPrblm, '/matrikaSosedi') ## okolje kamor naj se shrani matrike sosedov
					)	
	
}


## ucenje meta klasifikatorjev
for(del in 1:20){
	setwd(OkoljePodatki)
	podatki <- readRDS(paste0('razdeljeniPodatki_Del_', del , '.rds'))
	
	imenaMnozic <- paste0(c('Fold1','Fold2', 'Fold2'), '_Podatki', del)
		
	ySosedSet <- podatki[['Fold2_Y']]
	classOsnovni <- levels(ySosedSet)
	
	baseLearner <-  paste0('model_', 1:stModelov)
	
	OkoljeOP_del <- paste0(Okolje_OP, '/podatki', del)
	setwd(OkoljeOP_del)
	OP_meta <- readRDS(paste0('OP_metaTrain_', del,'.rds'))
	
	OkoljeMetaPrblm <- paste0(OkoljeShraniMetaKlasifikator, '/podatki', del, '/metaProblem')
	dir.create(OkoljeMetaPrblm, recursive = TRUE)
	
	OkoljeMetaKlasifikator <- paste0(OkoljeShraniMetaKlasifikator, '/podatki', del, '/metaKlasifikatorONE_stBL', stModelov)
	dir.create(OkoljeMetaKlasifikator, recursive = TRUE)
	
	ucenjeMetaKlasifikatorONE(imenaMnozic, ## ime mnozice: [sosede katerih iscemo, iz katere so sosedje]
							OP_meta, ## output profile (verjetnosti) za mnozico metaSet 
							classOsnovni, ## imena classov pri osnovnem problemu kot vektor (npr: c('neg', 'pos')
							baseLearner, ## ime base learnerjev (vektor)
							K, ## stevilo sosedov za region of competence (vektor)
							Kp, ## stevilo sosedov iz output profile (vektor)
							knnALG = "kd_tree", ## algoritem po katerem get.knnx isce sosede
							metaALG, ## klasifikacijski algoritmi uporabljeni pri ucenju meta klasifikatorja
							hC, ## kako razlicne morajo biti napovedi base learnerjev, za x iz metaSet da ga vkljucimo v ucenje
							OkoljeMetaPrblm, ## okolje kjer so shranjene metaProblemi 
							OkoljeMetaKlasifikator ## okolje, kamor se shranijo meta klasifikatorji
							)
	
}






