napovedEnsemble <- function(napovedBL,
							kompetentnost,
							kompetThrsh = 0.6,
							bup = 2 ## koliko najboljsih base learnerjev gre v ensemble ce ni nobenega dovolj kompetentnega
							){
	komp_abThrsh <- kompetentnost
	komp_abThrsh[kompetentnost < kompetThrsh] <- 0
	## kaj narediti, ce noben ne preseze komptetThrsh
		## deli matrike v katerih ni nobenega kompetentnega klasifikatorja
		niKompetentnega <- which(apply(komp_abThrsh == 0,1, all) == TRUE)
		if(length(niKompetentnega) > 0){
			## druga najvecja vrednosti v vrstici (tistih ki nimajo kompetentnega klasifikatorja)
			meja <- t(apply(kompetentnost[niKompetentnega,],1,sort))[,ncol(kompetentnost[niKompetentnega,])-bup+1]
			komp_abThrsh[niKompetentnega,] <- kompetentnost[niKompetentnega,]
			komp_abThrsh[niKompetentnega,][kompetentnost[niKompetentnega,] < meja] <- 0
		}
		
	##
	utez <- komp_abThrsh/rowSums(komp_abThrsh)
	probPred <- rowSums(napovedBL*utez)
	return(probPred)
}