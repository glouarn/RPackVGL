
################### boucle en sequentiel


library(RPackVGL)
library(ineq) #pas chargee avec



# dossier avec les zip au bon nom
#"C:\\devel\\l-egume\\legume\\test\\test_champ"
dir <- choose.dir()
setwd(dir)
ls_files <- list.files(dir)
ls_zip <- ls_files[grepl('.zip', ls_files)]


for (zfile in ls_zip)
{
  toto <- build_dtoto_binary_zip1(zfile, DOYdeb=60, DOYScoupe=c(187,229,282,334), ls_parsd=c("Len","Lfeuille","phyllochron", "Vmax2", "ELmax", "PPtreshh"), cote=16, nblignes=16, opt_saveindices=F)
  #ecriture incluse dans build_dtoto_binary_zip1
}


# lit et concatene les fichiers produits
ls_files <- list.files(dir)
ls_dtoto <- ls_files[grepl('dtoto_', ls_files)]

dtoto <- do.call("rbind", read_ltoto(ls_dtoto))
row.names(dtoto) <- 1:dim(dtoto)[1]





######################### en parallelle

library(RPackVGL)
#library(ineq)



# dossier avec les zip au bon nom
#"C:\\devel\\l-egume\\legume\\test\\test_champ"
dir <- choose.dir()
setwd(dir)
ls_files <- list.files(dir)
ls_zip <- ls_files[grepl('.zip', ls_files)]



# example parrallel computing on local computer
#https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html


library(parallel)
library(foreach)
library(doParallel)



numCores <- detectCores()
registerDoParallel(numCores-2)


foreach (i=1:length(ls_zip), .combine=cbind, .packages=c("RPackVGL", "ineq")) %dopar%
{
  zfile <- ls_zip[i]
  toto <- build_dtoto_binary_zip1(zfile, DOYdeb=60, DOYScoupe=c(187,229,282,334), ls_parsd=c("Len","Lfeuille","phyllochron", "Vmax2", "ELmax", "PPtreshh"), cote=16, nblignes=16, opt_saveindices=F)
  #ecriture incluse dans build_dtoto_binary_zip1
}



# lit et concatene les fichiers produits
ls_files <- list.files(dir)
ls_dtoto <- ls_files[grepl('dtoto_', ls_files)]

dtoto <- do.call("rbind", read_ltoto(ls_dtoto))
row.names(dtoto) <- 1:dim(dtoto)[1]


