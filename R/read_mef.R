


#################################
## lecture fichiers toto



#' This function reads the content of the simulation file "toto" of a series of USM and stocks the dynamic daily output into a list
#'
#' @param ls_toto A list of toto files paths
#'
#' @return A list of data frames. Each element of this list correspond to the simulation output of a single USM
#' @export
#'
#' @examples
#' path_ltoto1 <- system.file("extdata", "toto_6129_l-egume_Orca-Orca_homogeneous0_scenario12-12_DigitLuz10LD_0_DigitLuz10_-_.csv", package = "RPackVGL")
#' path_ltoto2 <- system.file("extdata", "toto_6130_l-egume_Orca-Orca_homogeneous0_scenario12-12_DigitLuz10LD_1_DigitLuz10_-_.csv", package = "RPackVGL")
#' path_ltoto3 <- system.file("extdata", "toto_6131_l-egume_Orca-Orca_homogeneous0_scenario12-12_DigitLuz10LD_2_DigitLuz10_-_.csv", package = "RPackVGL")
#'
#' ls_toto <- c(path_ltoto1, path_ltoto2, path_ltoto3)
#' my_ltoto <- read_ltoto(ls_toto)
read_ltoto <- function(ls_toto)
{
  #recuperer par paquet les fichiers toto du dossier de travail dans une liste ltoto
  # for dynamic daily outputs
  ltoto <- vector('list', length(ls_toto))
  names(ltoto) <- ls_toto

  for (i in 1:length(ls_toto))
  {
    name <- ls_toto[i]
    ltoto[[name]] <- read.table(name, header=T, sep=';')
  }
  ltoto
}




read_lsSD_MStot <- function(ltoto, ls_paramSD, param_name = "Len")
{

  #recuperation par paquet des fichiers de base (pas de stockage de l'ensemble des fichiers en memoire)
  #ltoto <- read_ltoto(ls_toto_paquet)

  #lit la liste des fichier Sd et les MStot pour une liste de ltoto

  ls_MStot <- vector("list",length(ltoto))
  names(ls_MStot) <- names(ltoto)


  ls_tabSD <- vector("list",length(ltoto))
  names(ls_tabSD) <- names(ltoto)

  for (nomfichier in names(ltoto))
  {
    dat <- ltoto[[nomfichier]]

    num_usm <- strsplit(nomfichier, '_')[[1]][2]
    scenar <- strsplit(nomfichier, '_')[[1]][6]
    graine <- strsplit(nomfichier, '_')[[1]][8]
    secenarSD <- strsplit(nomfichier, '_')[[1]][10]
    esps <- strsplit(nomfichier, '_')[[1]][4]
    damier <- strsplit(nomfichier, '_')[[1]][5]
    titre <- paste(num_usm, scenar, secenarSD,  damier, graine)#esps,

    #lecture fichier paramSD de l'USM dans tabSD
    nomSD <- ls_paramSD[grepl(paste("paramSD_",num_usm,"_",sep=""), ls_paramSD)]
    #param_name <- "Len"
    tabSD <- read.table(nomSD, header=T, sep=';')

    nb <- dim(dat)[2]-2
    MStot <- dat[dat$V1=='MStot',3:(3+nb-1)] #ajout de MStot
    tabSD$MStotfin <- as.numeric(MStot[dim(MStot)[1],])#derniere ligne
    tabSD$id <- titre
    tabSD$graine <- graine

    #split de tabSD par espece et ajout des decile
    sp_tabSD <- split(tabSD, tabSD$name)

    sp <- unique(as.character(tabSD$name))[1]#"Fix2"#"nonFixSimTest"#
    valparams <- sp_tabSD[[sp]][,c(param_name)]
    sp_tabSD[[sp]]$decile <- Which_decile(valparams)
    sp <- unique(as.character(tabSD$name))[2]#"nonFixSimTest"#
    valparams <- sp_tabSD[[sp]][,c(param_name)]
    sp_tabSD[[sp]]$decile <- Which_decile(valparams)

    tabSD <- do.call("rbind", sp_tabSD)

    #stocke dans ls_tabSD et ls_MStot
    ls_tabSD[[nomfichier]] <- tabSD
    ls_MStot[[nomfichier]] <- MStot
  }
  res <- list(ls_tabSD, ls_MStot)
  names(res) <- c("ls_tabSD","ls_MStot")
  res
}




build_dtoto <- function(sp_dtoto, key, DOYdeb, DOYScoupe)
{
  ls_toto_paquet <- sp_dtoto[[key]]$name

  #recuperation par paquet des fichiers de base (pas de stockage de l'ensemble des fichiers en memoire)
  ltoto <- read_ltoto(ls_toto_paquet)
  #version locale du paquet de doto
  dtoto <- sp_dtoto[[key]]

  #recup du nom des esp
  mix <- strsplit(ls_toto_paquet[1], '_')[[1]][4] #suppose paquet fait par traitement
  esp <- strsplit(mix, '-')[[1]][1] #'Fix2'
  esp2 <- strsplit(mix, '-')[[1]][2] #'nonFixSimTest'

  #visu des rendement moyen m2 / a un DOY
  surfsolref <- NULL
  nbplt <- NULL
  nbplt1 <- NULL
  nbplt2 <- NULL

  #DOYScoupe <- c(165,199,231,271,334)#Avignon
  #DOYScoupe <- c(187,229,282,334)#Lusignan
  #DOYdeb <- 60
  idDOYScoupe <- DOYScoupe - DOYdeb
  Ytot <- NULL
  Ycoupe <- NULL

  YEsp1 <- NULL
  YEsp2 <- NULL

  QNfix <- NULL
  QNupttot <- NULL
  QNuptleg <- NULL

  PARi1 <- NULL
  PARi2 <- NULL
  Surf1 <- NULL
  Surf2 <- NULL
  LRac1 <- NULL
  LRac2 <- NULL
  MRac1 <- NULL
  MRac2 <- NULL
  MGini1 <- NULL
  MGini2 <- NULL
  MAlive1 <- NULL
  MAlive2 <- NULL

  for (i in 1:length(ls_toto_paquet))#(ls_toto))
  {
    name <- ls_toto_paquet[i]
    damier <- strsplit(name, '_')[[1]][5]
    dat <- ltoto[[name]]
    s <- dat[dat$V1=='pattern',3]#m2
    surfsolref <- cbind(surfsolref, as.numeric(as.character(s)))
    nb <- length(dat)-2
    nbplt <- cbind(nbplt, nb)

    #Y Totaux
    MSaerien <- as.matrix(dat[dat$V1=='MSaerien' & dat$steps %in% DOYScoupe,3:(3+nb-1)], ncol=nb)
    ProdIaer <- rowSums(MSaerien) / s
    Ycoupe <- rbind(Ycoupe, ProdIaer)
    Ytot <- cbind(Ytot, sum(ProdIaer))#cumul des 5 coupes


    #N totaux et fixation
    Qfix <- as.matrix(dat[dat$V1=='Qfix',3:(3+nb-1)], ncol=nb)
    Qfix <- as.numeric(rowSums(Qfix) / s)
    Nuptake_sol_tot <- as.matrix(dat[dat$V1=='Nuptake_sol',3:(3+nb-1)], ncol=nb)
    Nuptake_sol_tot <- as.numeric(rowSums(Nuptake_sol_tot) / s)
    QNfix <-cbind(QNfix, sum(Qfix))
    QNupttot <- cbind(QNupttot, sum(Nuptake_sol_tot))

    #YEsp1
    #esp <- 'Fix2'#'Fix3'#'Fix1'#'Fix' #pourquoi c'est ce nom au lieu de Fix???
    #esp2 <- 'nonFixSimTest'#'nonFix1'#'nonFix0' #pourquoi c'est ce nom au lieu de Fix???

    nomcol <- names(ltoto[[name]])
    #if (esp==esp2 & grep('damier', damier)==1)#si deux fois le meme nom d'espece, mais mixture damier
    #{
    #  idcols <- as.logical(didcols[didcols$damier==damier,1:66])#idcols lu dans fichier qui leve les ambiguite
    #} else
    #{
    idcols <- grepl(esp, nomcol) & !grepl(esp2, nomcol)#contient esp1 et pas esp2
    #}

    dat1 <- cbind(ltoto[[name]][,c(1:2)], ltoto[[name]][,idcols])
    nb1 <- length(dat1)-2
    nbplt1 <- cbind(nbplt1, nb1)
    if (nb1>0)
    {
      MS1 <- as.matrix(dat1[dat1$V1=='MSaerien' & dat1$steps %in% DOYScoupe,3:(3+nb1-1)], ncol=nb1)
      ProdIaer1 <- rowSums(MS1) / s
      Nuptake_sol_leg <- as.matrix(dat1[dat1$V1=='Nuptake_sol',3:(3+nb1-1)], ncol=nb1)
      Nuptake_sol_leg <- as.numeric(rowSums(Nuptake_sol_leg) / s)
      jPARi1 <- rowSums(as.matrix(dat1[dat1$V1=='PARiPlante' & dat1$steps %in% DOYScoupe,3:(3+nb1-1)], ncol=nb1)) / s
      jSurf1 <- rowSums(as.matrix(dat1[dat1$V1=='SurfPlante' & dat1$steps %in% DOYScoupe,3:(3+nb1-1)], ncol=nb1)) / s
      jLRac1 <- rowSums(as.matrix(dat1[dat1$V1=='RLTot' & dat1$steps %in% DOYScoupe,3:(3+nb1-1)], ncol=nb1)) / s
      jMRac1 <- rowSums(as.matrix(dat1[dat1$V1=='MS_rac_fine' & dat1$steps %in% DOYScoupe,3:(3+nb1-1)], ncol=nb1)) / s
      jMPiv1 <- rowSums(as.matrix(dat1[dat1$V1=='MS_pivot' & dat1$steps %in% DOYScoupe,3:(3+nb1-1)], ncol=nb1)) / s
      #gini par date sur MSA (ttes les plantes)
      Gini1 <- NULL
      for (k in 1:length(DOYScoupe))
      { Gini1 <- cbind(Gini1, ineq(MS1[k,], type="Gini"))}

      #survie
      matSV1 <- as.matrix(dat1[dat1$V1 == "aliveB" & dat1$steps %in% DOYScoupe,3:(3+nb1-1)], ncol=nb1)
      matSV1[matSV1>0] <- 1
      aliveP1 <-  as.numeric(nbplt)-as.matrix(rowSums(matSV1))
      aliveDens1 <- t(aliveP1 / s)
      #MortDens1 <-  as.matrix(rowSums(matSV1)) / s


    } else
    {
      ProdIaer1 <- 0 #pas de plante de l'esp1
      Nuptake_sol_leg <- 0
      jPARi1 <- 0
      jSurf1 <- 0
      jLRac1 <- 0
      jMRac1 <- 0
      jMPiv1 <- 0
      Gini1 <- c(NA,NA,NA,NA)
      aliveDens1 <- c(0,0,0,0)
    }
    YEsp1 <- cbind(YEsp1, sum(ProdIaer1))#cumul des 5 coupes
    QNuptleg <- cbind(QNuptleg, sum(Nuptake_sol_leg))
    PARi1 <- cbind(PARi1, sum(jPARi1))
    Surf1 <- cbind(Surf1, sum(jSurf1))
    LRac1 <- cbind(LRac1, max(jLRac1))
    MRac1 <- cbind(MRac1, max(jMRac1)+max(jMPiv1))
    MGini1 <- rbind(MGini1, Gini1)
    MAlive1 <- rbind(MAlive1, aliveDens1)

    #YEsp2
    #if (esp==esp2 & grep('damier', damier)==1)#si deux fois le meme nom d'espece, mais mixture damier
    #{
    #  idcols <- !as.logical(didcols[didcols$damier==damier,1:66])#idcols lu dans fichier qui leve les ambiguite
    #  idcols[1:2] <- FALSE #remet a faux les deux premieres colonnes
    #} else
    #{
    idcols <- grepl(esp2, nomcol)#contient esp2
    #}

    dat2 <- cbind(ltoto[[name]][,c(1:2)], ltoto[[name]][,idcols])
    nb2 <- length(dat2)-2
    nbplt2 <- cbind(nbplt2, nb2)
    if (nb2>0)
    {
      MS2 <- as.matrix(dat2[dat2$V1=='MSaerien' & dat2$steps %in% DOYScoupe,3:(3+nb2-1)], ncol=nb2)
      ProdIaer2 <- rowSums(MS2) / s
      jPARi2 <- rowSums(as.matrix(dat2[dat2$V1=='PARiPlante' & dat2$steps %in% DOYScoupe,3:(3+nb2-1)], ncol=nb2)) / s
      jSurf2 <- rowSums(as.matrix(dat2[dat2$V1=='SurfPlante' & dat2$steps %in% DOYScoupe,3:(3+nb2-1)], ncol=nb2)) / s
      jLRac2 <- rowSums(as.matrix(dat2[dat2$V1=='RLTot' & dat2$steps %in% DOYScoupe,3:(3+nb2-1)], ncol=nb2)) / s
      jMRac2 <- rowSums(as.matrix(dat2[dat2$V1=='MS_rac_fine' & dat2$steps %in% DOYScoupe,3:(3+nb2-1)], ncol=nb2)) / s
      jMPiv2 <- rowSums(as.matrix(dat2[dat2$V1=='MS_pivot' & dat2$steps %in% DOYScoupe,3:(3+nb2-1)], ncol=nb2)) / s
      #gini par date sur MSA (ttes les plantes)
      Gini2 <- NULL
      for (k in 1:length(DOYScoupe))
      { Gini2 <- cbind(Gini2, ineq(MS2[k,], type="Gini"))}

      #survie
      matSV2 <- as.matrix(dat2[dat2$V1 == "aliveB" & dat2$steps %in% DOYScoupe,3:(3+nb1-1)], ncol=nb2)
      matSV2[matSV2>0] <- 1
      aliveP2 <-  as.numeric(nbplt)-as.matrix(rowSums(matSV2))
      aliveDens2 <- t(aliveP2 / s)
      #MortDens2 <-  as.matrix(rowSums(matSV2)) / s
    }
    else
    {
      ProdIaer2 <- 0 #pas de plante de l'esp2
      jPARi2 <- 0
      jSurf2 <- 0
      jLRac2 <- 0
      jMRac2 <- 0
      jMPiv2 <- 0
      Gini2 <- c(NA,NA,NA,NA)
      aliveDens2 <- c(0,0,0,0)
    }
    YEsp2 <- cbind(YEsp2, sum(ProdIaer2))#cumul des 5 coupes
    #YEsp2 <- cbind(YEsp2, sum(ProdIaer2))#cumul des 5 coupes
    PARi2 <- cbind(PARi2, sum(jPARi2))
    Surf2 <- cbind(Surf2, sum(jSurf2))
    LRac2 <- cbind(LRac2, max(jLRac2))
    MRac2 <- cbind(MRac2, max(jMRac2)+max(jMPiv2))
    MGini2 <- rbind(MGini2, Gini2)
    MAlive2 <- rbind(MAlive2, aliveDens2)
  }

  dtoto$surfsolref <- as.numeric(surfsolref)
  dtoto$nbplt <- as.numeric(nbplt)
  dtoto$nbplt1 <- as.numeric(nbplt1)
  dtoto$nbplt2 <- as.numeric(nbplt2)
  dtoto$Ytot <- as.numeric(Ytot)
  dtoto$densite <- dtoto$nbplt/dtoto$surfsolref
  dtoto$densite1 <- dtoto$nbplt1/dtoto$surfsolref
  dtoto$YEsp1 <- as.numeric(YEsp1)
  dtoto$densite2 <- dtoto$nbplt2/dtoto$surfsolref
  dtoto$YEsp2 <- as.numeric(YEsp2)
  dtoto$Semprop1 <- dtoto$densite1/dtoto$densite
  dtoto$Yprop1 <- dtoto$YEsp1 / (dtoto$YEsp1 +dtoto$YEsp2)
  dtoto$Yprop2 <- dtoto$YEsp2 / (dtoto$YEsp1 +dtoto$YEsp2)
  dtoto$QNfix <- as.numeric(QNfix)
  dtoto$QNupttot <- as.numeric(QNupttot)
  dtoto$QNuptleg <- as.numeric(QNuptleg)
  dtoto$QNtot <- dtoto$QNfix + dtoto$QNupttot

  #new var
  dtoto$Pari1 <- as.numeric(PARi1)
  dtoto$Pari2 <- as.numeric(PARi2)
  dtoto$Surf1 <- as.numeric(Surf1)
  dtoto$Surf2 <- as.numeric(Surf2)
  dtoto$PhiSurf1 <- as.numeric(PARi1) / (as.numeric(Surf1) + 10e-12)#Phi Surf
  dtoto$PhiSurf2 <- as.numeric(PARi2) / (as.numeric(Surf2) + 10e-12)
  dtoto$PhiMass1 <- as.numeric(PARi1) / (as.numeric(YEsp1) + 10e-12)#Phi Mass
  dtoto$PhiMass2 <- as.numeric(PARi2) / (as.numeric(YEsp2) + 10e-12)
  dtoto$LRac1 <- as.numeric(LRac1)
  dtoto$LRac2 <- as.numeric(LRac2)
  dtoto$MRac1 <- as.numeric(MRac1)
  dtoto$MRac2 <- as.numeric(MRac2)
  dtoto$UptNLen1 <- (as.numeric(QNupttot) - as.numeric(QNuptleg)) / (as.numeric(LRac1) + 10e-12)#Uptake par Len
  dtoto$UptNLen2 <- as.numeric(QNuptleg) / (as.numeric(LRac2) + 10e-12)
  dtoto$UptNMass1 <- (as.numeric(QNupttot) - as.numeric(QNuptleg)) / (as.numeric(MRac1) + 10e-12)#Uptake par Mass root
  dtoto$UptNMass2 <- as.numeric(QNuptleg) / (as.numeric(MRac2) + 10e-12)
  dtoto$gini1 <- rowMeans(MGini1) #moyenne des gini de ttes les dates (sans retirer pltes mortes)
  dtoto$gini2 <- rowMeans(MGini2)
  dtoto$alive1 <- as.numeric(MAlive1[,dim(MAlive1)[2]]) #survie derniere date coupe
  dtoto$alive2 <- as.numeric(MAlive2[,dim(MAlive2)[2]]) #survie derniere date coupe

  dtoto

}
#fonction a generaliser et a bouger ailleurs
# a reprendre -pour asso binaire
# build synthetic df des valeur globale par simul



#################################
## fonction de mise en formse des simule



#' Calculate the the average or standard deviation of a series of simulations
#'
#' @param ltoto A list of data frames. Each element of this list correspond to the simulation output of a single USM
#' @param lsusm The list of USMs to consider in ltoto
#' @param var Name of the variable to consider for calculation
#' @param esp An optional parameter to specify the name of a species to consider in case of multi-species simulations
#' @param optSD An optional parameter to specify if standard deviation should be calculated instead of mean
#'
#' @return A vector of average or stand deviation of the variable 'var'
#' @export
#' @examples
#' ltoto <- ltoto_exemple
#' NBI <- moysimval(ltoto, lsusm=names(ltoto), var='NBI')
#' sdNBI <- moysimval(ltoto, lsusm=names(ltoto), var='NBI', optSD=T)
#' plot(NBI, type='l')
#' # add standard deviations
#' segments(1:length(NBI), NBI, 1:length(NBI), NBI+sdNBI, col=2)
#' segments(1:length(NBI), NBI, 1:length(NBI), NBI-sdNBI, col=2)
moysimval <- function(ltoto, lsusm, var,esp=NA, optSD=F)
{
  # Fait moyenne de la somme pour toute les plantes d'une variable var pour une liste d'usm simulee
  #utilise pour construire le tableau simmoy
  #version GL adapt lucas (v4)
  #optSD=T renvoie standard deviation de la somme des individus
  #esp = NA pour tous le couvert
  #esp pour definir pour une espece du couvert

  res <- vector("list",length(lsusm))
  names(res) <- lsusm
  for (usm in lsusm)
  {

    if (is.na(esp))
    {dat <- ltoto[[usm]]
    } else
    {
      #garde uniquement col esp
      nomcol <- names(ltoto[[usm]])
      idcols <- grepl(esp, nomcol)
      dat <- cbind(ltoto[[usm]][,c(1:2)], ltoto[[usm]][,idcols])
    }

    nbplt <- length(dat)-2
    xplt <- as.matrix(dat[dat$V1==var,3:(3+nbplt-1)], ncol=nbplt)
    xsum <- rowSums(xplt)
    res[[usm]] <- xsum
  }
  if (optSD==F)
  {
    #fait moyenne des sim
    xav <- rowSums(as.data.frame(res))/length(lsusm)
  }else
  {
    #calcule standard deviation des sim
    xav <- apply(as.data.frame(res),MARGIN=1,sd)
  }

  xav
}
#LAI <- moysimval(ltoto, lsusm=names(ltoto), var='SurfPlante')/ surfsolref
#LAIsd <- moysimval(ltoto, lsusm=names(ltoto), var='SurfPlante',optSD=T)/ surfsolref





#' Calculate the the average or standard deviation of a series of simulations for a set of predefined variables
#'
#' @param ltoto A list of data frames. Each element of this list correspond to the simulation output of a single USM
#' @param lsusm The list of USMs to consider in ltoto
#' @param esp An optional parameter to specify the name of a species to consider in case of multi-species simulations
#' @param optSD An optional parameter to specify if standard deviation should be calculated instead of mean
#'
#' @return A simmoy data.frame with variables : STEPS, TT, NBI, NBphyto, LAI, MSA, MSArec, MSAnonrec, MSpiv, MSracfine, MSrac, RDepth, Hmax, FTSW, NNI, R_DemandC_Root, cutNB, Npc_aer, Ndfa, Epsi, NBsh
#' @export
#' @examples
#' ltoto <- ltoto_exemple
#' simmoy <- build_simmoy(ltoto, lsusm=names(ltoto))
#' plot(simmoy$STEPS, simmoy$NBI, type='l')
build_simmoy <- function(ltoto, lsusm, esp=NA, optSD=F)
{
  #moy des simul des differentes graines d'un meme usm avec moysimval (pour variables dynamiques)

  #recup info generale sur la premier usm
  #dat <- ltoto[[lsusm[1]]]
  if (is.na(esp))
  {dat <- ltoto[[lsusm[1]]]
  } else
  {
    #garde uniquement col esp
    nomcol <- names(ltoto[[lsusm[1]]])
    idcols <- grepl(esp, nomcol)
    dat <- cbind(ltoto[[lsusm[1]]][,c(1:2)], ltoto[[lsusm[1]]][,idcols])
  }

  TT <- dat[dat$V1=='TT',3] #peut changer selon les plantes!
  STEPS <- dat[dat$V1=='TT',2]
  nbplt <- length(dat)-2
  surfsolref <- dat[dat$V1=='pattern',3] #m2
  ls_varOUT <- unique(dat$V1)

  # TT et STEPS variables obligatoires
  simmoy <- data.frame(STEPS, TT)

  if ('SurfPlante' %in% ls_varOUT)
  { simmoy$LAI <- moysimval(ltoto, lsusm, var='SurfPlante', esp, optSD)/ surfsolref }
  if ('MSaerien' %in% ls_varOUT)
  { simmoy$MSA <- moysimval(ltoto,lsusm, var='MSaerien', esp, optSD)/ surfsolref }
  if ('MSaerienRec' %in% ls_varOUT)
  { simmoy$MSArec <- moysimval(ltoto,lsusm, var='MSaerienRec', esp, optSD)/ surfsolref }
  if ('MSaerienNonRec' %in% ls_varOUT)
  { simmoy$MSAnonrec <- moysimval(ltoto,lsusm, var='MSaerienNonRec', esp, optSD)/ surfsolref}
  if ('MS_pivot' %in% ls_varOUT)
  { simmoy$MSpiv <- moysimval(ltoto,lsusm, var='MS_pivot', esp, optSD)/ surfsolref }
  if ('MS_rac_fine' %in% ls_varOUT)
  { simmoy$MSracfine <- moysimval(ltoto,lsusm, var='MS_rac_fine', esp, optSD)/ surfsolref}
  if ('MS_pivot' %in% ls_varOUT & 'MS_rac_fine' %in% ls_varOUT)
  {  simmoy$MSrac <- simmoy$MSpiv + simmoy$MSracfine }

  if ('NBI' %in% ls_varOUT)
  {
    NBI <- moysimval(ltoto,lsusm, var='NBI', esp, optSD)/ nbplt
    simmoy$NBI <- pmax(0, NBI - 0.75) #correction des simuls pour les comptages decimaux
  }
  #NBIquart <- quantsimval(ltoto,lsusm, var_='NBI',esp=esp)

  if ('NBphyto' %in% ls_varOUT)
  { simmoy$NBphyto <- moysimval(ltoto, lsusm, var='NBphyto', esp, optSD)/ surfsolref}
  if ('NBapexAct' %in% ls_varOUT)
  { simmoy$Nbapex <- moysimval(ltoto, lsusm, var='NBapexAct', esp, optSD)/ surfsolref }
  if ('NBphyto' %in% ls_varOUT & 'NBapexAct' %in% ls_varOUT)
  { simmoy$NBphyto <- pmax(0,simmoy$NBphyto - 0.5*simmoy$Nbapex) }
  #correction simuls pour les comptages decimaux

  if ('NBsh' %in% ls_varOUT)
  { simmoy$NBsh <- moysimval(ltoto, lsusm, var='NBsh', esp, optSD)/ surfsolref}
  if ('RDepth' %in% ls_varOUT)
  { simmoy$RDepth <- moysimval(ltoto,lsusm, var='RDepth', esp, optSD)/ nbplt}
  if ('Hplante' %in% ls_varOUT)
  { simmoy$Hmax <- moysimval(ltoto,lsusm, var='Hplante', esp, optSD)/ nbplt}
  if ('FTSW' %in% ls_varOUT)
  { simmoy$FTSW <- moysimval(ltoto,lsusm, var='FTSW', esp, optSD)/ nbplt}
  if ('NNI' %in% ls_varOUT)
  { simmoy$NNI <- moysimval(ltoto,lsusm, var='NNI', esp, optSD)/ nbplt}
  if ('R_DemandC_Root' %in% ls_varOUT)
  { simmoy$R_DemandC_Root <- moysimval(ltoto,lsusm, var='R_DemandC_Root', esp, optSD)/ nbplt}
  if ('cutNB' %in% ls_varOUT)
  { simmoy$cutNB <- moysimval(ltoto,lsusm, var='cutNB', esp, optSD)/ nbplt}
  if ('Npc_aer' %in% ls_varOUT)
  {
    simmoy$Npc_aer <- moysimval(ltoto,lsusm, var='Npc_aer', esp, optSD)/ nbplt
    #!! reprendre et ponderer par biomasse aerienne!!
  }
  if ('Ndfa' %in% ls_varOUT)
  {
    simmoy$Ndfa <- moysimval(ltoto,lsusm, var='Ndfa', esp, optSD)/ nbplt
    #!! reprendre et ponderer par biomasse aerienne!!
  }
  if ('epsi' %in% ls_varOUT)
  { simmoy$Epsi <- moysimval(ltoto,lsusm, var='epsi', esp, optSD)}


  simmoy

}
#version revue par Lucas tient cmpte du nom de l'espece dans les assos
#simmoy <- build_simmoy(ltoto, lsusm=names(ltoto))
#simmoy <- build_simmoy(ltoto, lsusm=names(ltoto), esp="timbale")





# build_simmoy_old <- function(ltoto, lsusm, esp=NA, optSD=F)
# {
#   #moy des simul des differentes graines d'un meme usm avec moysimval (pour variables dynamiques)
#
#   #recup info generale sur la premier usm
#   #dat <- ltoto[[lsusm[1]]]
#   if (is.na(esp))
#   {dat <- ltoto[[lsusm[1]]]
#   } else
#   {
#     #garde uniquement col esp
#     nomcol <- names(ltoto[[lsusm[1]]])
#     idcols <- grepl(esp, nomcol)
#     dat <- cbind(ltoto[[lsusm[1]]][,c(1:2)], ltoto[[lsusm[1]]][,idcols])
#   }
#
#   TT <- dat[dat$V1=='TT',3] #peut changer selon les plantes!
#   STEPS <- dat[dat$V1=='TT',2]
#   nbplt <- length(dat)-2
#   surfsolref <- dat[dat$V1=='pattern',3] #m2
#
#   LAI <- moysimval(ltoto, lsusm, var='SurfPlante', esp, optSD)/ surfsolref
#   MSA <- moysimval(ltoto,lsusm, var='MSaerien', esp, optSD)/ surfsolref
#   MSArec <- moysimval(ltoto,lsusm, var='MSaerienRec', esp, optSD)/ surfsolref
#   MSAnonrec <- moysimval(ltoto,lsusm, var='MSaerienNonRec', esp, optSD)/ surfsolref
#   MSpiv <- moysimval(ltoto,lsusm, var='MS_pivot', esp, optSD)/ surfsolref
#   MSracfine <- moysimval(ltoto,lsusm, var='MS_rac_fine', esp, optSD)/ surfsolref
#   MSrac <- MSpiv + MSracfine
#   NBI <- moysimval(ltoto,lsusm, var='NBI', esp, optSD)/ nbplt
#   NBI <- pmax(0, NBI - 0.75) #correction des simuls pour les comptages decimaux
#   #NBIquart <- quantsimval(ltoto,lsusm, var_='NBI',esp=esp)
#   NBphyto <- moysimval(ltoto, lsusm, var='NBphyto', esp, optSD)/ surfsolref
#   Nbapex <- moysimval(ltoto, lsusm, var='NBapexAct', esp, optSD)/ surfsolref
#   NBphyto <- pmax(0,NBphyto - 0.5*Nbapex) #correction simuls pour les comptages decimaux
#   NBsh <- moysimval(ltoto, lsusm, var='NBsh', esp, optSD)/ surfsolref
#
#   RDepth <- moysimval(ltoto,lsusm, var='RDepth', esp, optSD)/ nbplt
#   Hmax <- moysimval(ltoto,lsusm, var='Hplante', esp, optSD)/ nbplt
#   FTSW <- moysimval(ltoto,lsusm, var='FTSW', esp, optSD)/ nbplt
#   NNI <- moysimval(ltoto,lsusm, var='NNI', esp, optSD)/ nbplt
#   R_DemandC_Root <- moysimval(ltoto,lsusm, var='R_DemandC_Root', esp, optSD)/ nbplt
#   cutNB <- moysimval(ltoto,lsusm, var='cutNB', esp, optSD)/ nbplt
#   Npc_aer <- moysimval(ltoto,lsusm, var='Npc_aer', esp, optSD)/ nbplt
#   Ndfa <- moysimval(ltoto,lsusm, var='Ndfa', esp, optSD)/ nbplt
#   Epsi <- moysimval(ltoto,lsusm, var='epsi', esp, optSD)
#
#   simmoy <- data.frame(STEPS, TT, NBI, NBphyto, LAI, MSA, MSArec, MSAnonrec, MSpiv, MSracfine, MSrac, RDepth, Hmax, FTSW, NNI, R_DemandC_Root, cutNB, Npc_aer,Ndfa,Epsi,NBsh)
#   simmoy
# }#version revue par Lucas tient cmpte du nom de l'espece dans les assos
#
# #simmoy <- build_simmoy(ltoto, lsusm=names(ltoto))
# #simmoy <- build_simmoy(ltoto, lsusm=names(ltoto), esp="timbale")
# #a revoir avec liste de variables a checker + test pour rendre plus plastique
#









