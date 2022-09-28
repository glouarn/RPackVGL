


#################################
## lecture fichiers toto



#' This function reads the content of the simulation file "toto" of a series of USM and stocks the dynamic daily output into a list
#'
#' @param totos A list of toto files paths or a data frame of corresponding zip and toto files
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
read_ltoto <- function(totos, dezip=F, var_="ls_toto")
{
  #recuperer par paquet les fichiers toto du dossier de travail dans une liste ltoto
  # for dynamic daily outputs
  if (dezip==F)
  {
    # totos = liste des fichiers toto
    ls_toto <- totos
    ltoto <- vector('list', length(ls_toto))
    names(ltoto) <- ls_toto

    for (i in 1:length(ls_toto))
    {
      name <- ls_toto[i]
      ltoto[[name]] <- read.table(name, header=T, sep=';')
    }
  }
  else
  {
    # totos = tableau correspondance des fichiers zip et toto
    tab_f <- totos
    ls_toto <- as.vector(tab_f[,var_]) #peut passer une autre variable si besoin

    ltoto <- vector('list', length(ls_toto))
    names(ltoto) <- ls_toto

    for (i in 1:length(ls_toto))
    {
      name <- tab_f[i,var_]#[i]
      zfile <- tab_f$ls_zip[i]
      ltoto[[name]] <- read.table(unz(zfile, name), header=T, sep=';')
    }

  }


  ltoto
}
# acn also be used for reading list of other files: paramSD...


#' This function reads the content of the simulation file "toto" and "paramSD" of a list of USM and stocks "ls_tabSD" and "ls_MStot"
#'
#' @export
read_lsSD_MStot <- function(ltoto, lparamSD, param_name = "Len")
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

    # #lecture fichier paramSD de l'USM dans tabSD
    # nomSD <- ls_paramSD[grepl(paste("paramSD_",num_usm,"_",sep=""), ls_paramSD)]
    # #param_name <- "Len"
    # tabSD <- read.table(nomSD, header=T, sep=';')
    tabSD <- lparamSD[[nomfichier]]

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
# change ls_paramSD (nom de fichier) par lparamSD (liste des fichiers avec nom de la liste = non des toto)
# !! attend nom de toto avec un certain format: pour avoir bon id et graine




# read_lsSD_MStot_old <- function(ltoto, ls_paramSD, param_name = "Len")
# {
#
#   #recuperation par paquet des fichiers de base (pas de stockage de l'ensemble des fichiers en memoire)
#   #ltoto <- read_ltoto(ls_toto_paquet)
#
#   #lit la liste des fichier Sd et les MStot pour une liste de ltoto
#
#   ls_MStot <- vector("list",length(ltoto))
#   names(ls_MStot) <- names(ltoto)
#
#
#   ls_tabSD <- vector("list",length(ltoto))
#   names(ls_tabSD) <- names(ltoto)
#
#   for (nomfichier in names(ltoto))
#   {
#     dat <- ltoto[[nomfichier]]
#
#     num_usm <- strsplit(nomfichier, '_')[[1]][2]
#     scenar <- strsplit(nomfichier, '_')[[1]][6]
#     graine <- strsplit(nomfichier, '_')[[1]][8]
#     secenarSD <- strsplit(nomfichier, '_')[[1]][10]
#     esps <- strsplit(nomfichier, '_')[[1]][4]
#     damier <- strsplit(nomfichier, '_')[[1]][5]
#     titre <- paste(num_usm, scenar, secenarSD,  damier, graine)#esps,
#
#     #lecture fichier paramSD de l'USM dans tabSD
#     nomSD <- ls_paramSD[grepl(paste("paramSD_",num_usm,"_",sep=""), ls_paramSD)]
#     #param_name <- "Len"
#     tabSD <- read.table(nomSD, header=T, sep=';')
#
#     nb <- dim(dat)[2]-2
#     MStot <- dat[dat$V1=='MStot',3:(3+nb-1)] #ajout de MStot
#     tabSD$MStotfin <- as.numeric(MStot[dim(MStot)[1],])#derniere ligne
#     tabSD$id <- titre
#     tabSD$graine <- graine
#
#     #split de tabSD par espece et ajout des decile
#     sp_tabSD <- split(tabSD, tabSD$name)
#
#     sp <- unique(as.character(tabSD$name))[1]#"Fix2"#"nonFixSimTest"#
#     valparams <- sp_tabSD[[sp]][,c(param_name)]
#     sp_tabSD[[sp]]$decile <- Which_decile(valparams)
#     sp <- unique(as.character(tabSD$name))[2]#"nonFixSimTest"#
#     valparams <- sp_tabSD[[sp]][,c(param_name)]
#     sp_tabSD[[sp]]$decile <- Which_decile(valparams)
#
#     tabSD <- do.call("rbind", sp_tabSD)
#
#     #stocke dans ls_tabSD et ls_MStot
#     ls_tabSD[[nomfichier]] <- tabSD
#     ls_MStot[[nomfichier]] <- MStot
#   }
#   res <- list(ls_tabSD, ls_MStot)
#   names(res) <- c("ls_tabSD","ls_MStot")
#   res
# }
# # marche pas ds exemple car nomSD determine automatiquement
# # et read.table dans fontion plante
# # a revoir / reprendre / adapter




#################################
## fonction de mise en forme des simuls dynamique



#' Calculate the the average or standard deviation of a series of simulations
#'
#' @param ltoto A list of data frames. Each element of this list correspond to the dynamic simulation output of a single USM
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
    #!! reprendre et ponderer par biomasse aerienne!! cumul Naerien / cumul biomasse
  }

  if ('Ndfa' %in% ls_varOUT)
  {
    simmoy$Ndfa <- moysimval(ltoto,lsusm, var='Ndfa', esp, optSD)/ nbplt
    #!! reprendre et ponderer par biomasse aerienne!! cumul Qfix / cumul biomasse
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





#################################
## fonction de mise en forme des tableau dtoto de synthese


#' This function ...calcul indices par espece
#'
#' @export
build_dtoto_binarySim <- function(ltoto, dtotoBase, ls_toto_paquet, DOYdeb, DOYScoupe)
{

  dtoto <- dtotoBase#sp_dtoto[[key]]

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





#' This function ...calcul indices par espece pour un single zip file / unitary process
#'
#' @export
build_dtoto_binary_zip1 <- function(zfile, DOYdeb=60, DOYScoupe=c(187,229,282,334), ls_parsd=c("Len","Lfeuille","phyllochron", "Vmax2", "ELmax", "PPtreshh"), cote=16, nblignes=16, opt_saveindices=F, outpath="")
{


  ### traite un zfile

  #zfile <- ls_zip[1]
  #recup nom
  nomf <- strsplit(zfile, '.zip')[[1]]
  nomf <- strsplit(nomf, 'toto')[[1]][2]

  #list file in the zip
  lsf <- unzip(zipfile = zfile, list = TRUE)$Name
  ls_toto <- lsf[grepl('toto_', lsf)]
  ls_paramSD <- lsf[grepl('paramSD_', lsf)]

  #table correspondance fichiers
  tab_file <- data.frame(ls_zip=zfile, ls_toto, ls_paramSD)




  # prepare dtotoBase  (11 col (avec sd))
  cols_ <- strsplit(ls_toto, '_')
  test_long <- as.numeric(lapply(cols_, length)) #pour separer selon nb de champs (avec sd)

  dtoto <- as.data.frame(t(as.data.frame(cols_[test_long==11])))#as.data.frame(t(as.data.frame(strsplit(ls_toto, '_'))))#
  row.names(dtoto) <- 1: length(dtoto[,1])
  dtoto <- dtoto[,c(2,3,4,5,6,7,8,10)]
  names(dtoto) <- c('usm','lsystem','mix','damier','scenario','Mng', 'seed','sd')
  dtoto$name <- ls_toto[test_long==11]
  dtoto$seed <- as.numeric(as.character(dtoto$seed))#substr(as.character(dtoto$seed), 1, 1)
  dtoto$scenario <- substr(as.character(dtoto$scenario), 9, nchar(as.character(dtoto$scenario)))

  #dtoto <- rbind(temp, dtoto) #merge des 2
  dtoto$keysc <- paste(dtoto$scenario, dtoto$mix, dtoto$Mng, dtoto$sd)# ajout d'une cle unique par scenario
  #dtoto$damier <- as.numeric(substr(as.character(dtoto$damier), 7, 7))

  nomscenar <- as.data.frame(t(as.data.frame(strsplit(dtoto$scenario, "-"))))
  names(nomscenar) <- c("scenario2", "scenario1")#inverse?
  row.names(nomscenar) <- 1:length(nomscenar$scenario1)
  dtoto$scenario1 <- as.character(nomscenar$scenario1)
  dtoto$scenario2 <- as.character(nomscenar$scenario2)

  #split de dtoto et stockage dans une liste de scenatios
  dtoto$keysc <- paste(dtoto$keysc, dtoto$usm) #modif cle pour le cas histor
  sp_dtoto <- split(dtoto, dtoto$keysc)

  #pour recup des fichiers SD
  ls_tabSDall <- vector("list", length(names(sp_dtoto)))
  ls_MStotall <- vector("list", length(names(sp_dtoto)))

  #pour recuperation des correlation MSindividuelles
  ls_res_cor <- vector("list", length=length(names(sp_dtoto)))
  names(ls_res_cor) <- names(sp_dtoto)




  # traitement -> certaines a passer en input

  # #pour intra (old a la fin si pb)
  # DOYdeb<- 60
  # #DOYScoupe <- c(165,199,231,271,334) #Avignon - an 1
  # DOYScoupe <- c(187,229,282,334) #Lusignan - an 1
  #
  # ls_parsd <- c("Len","Lfeuille","phyllochron", "Vmax2", "ELmax", "PPtreshh")#c("Len","Vmax2", "ELmax", "Lfeuille", "phyllochron", "PPtreshh") #a lire dans fichier sd idealement

  # #indices des voisins pour un damier 1*16 a 50/50
  # cote <- 16
  # nblignes <- 16
  ls_idv <- def_indice_vois5050(cote, nblignes)
  ls_idvois <- ls_idv[[1]]
  ls_idKin <- ls_idv[[2]]
  ls_idnonKin <- ls_idv[[3]]
  # a generaliser si besoin!


  # repris de boucle
  key <- names(sp_dtoto)[1] #1 seule key

  ls_toto_paquet <- sp_dtoto[[key]]$name
  dtotoBase <- sp_dtoto[[key]]


  #recuperation par paquet des fichiers de base (pas de stockage de l'ensemble des fichiers en memoire)
  tab_f <- tab_file[tab_file$ls_toto %in% ls_toto_paquet,]
  ltoto <- read_ltoto(tab_f, dezip=T, var_="ls_toto")

  dtoto <- build_dtoto_binarySim(ltoto, dtotoBase, ls_toto_paquet, DOYdeb, DOYScoupe)

  #remise du dtoto local dans sp_dtoto
  sp_dtoto[[key]] <- dtoto

  #lecture fichier SD et de tables MStot, puis calcul des indices de decile par esp et parametre
  lparamSD<- read_ltoto(tab_f, dezip=T, var_="ls_paramSD")
  names(lparamSD) <- names(ltoto) #remet noms de toto

  resdec <- data.frame(key)
  for (param_name in ls_parsd) #c("Len","Vmax2"))
  {
    #calcul decile /param
    resread <- read_lsSD_MStot(ltoto, lparamSD, param_name)

    #marche pour cas de resread simul unitaire
    sp_tabSD <- split(resread[["ls_tabSD"]][[1]], resread[["ls_tabSD"]][[1]]$name)
    MStot <- resread[["ls_MStot"]][[1]]
    res <- BuildResDecil(MStot, sp_tabSD, param_name)

    resdec <- cbind(resdec, res)

  }



  #calcul des correlations MSindividuelles
  ls_res_cor_i <- Calc_MSindiv_Corr(ltoto, ls_toto_paquet, lparamSD, key, ls_idvois, ls_idKin, ls_idnonKin, cote, nblignes, lspar=ls_parsd)
  ls_res_cor[[key]] <- ls_res_cor_i[["tabCorMSindiv"]]
  tabindices <- ls_res_cor_i[["datIndices"]] #a sauvegarder eventuellement

  #ajout variance locale
  resdec$var_PARivois <- var(tabindices$PARi / (tabindices$PARivois/8))
  resdec$var_PARiKin <- var(tabindices$PARi / (tabindices$PARiKin/4))
  resdec$var_PARinonKin <- var(tabindices$PARi / (tabindices$PARinonKin/4))
  ls_tabSDall[[key]] <- resdec


  #optional
  if (opt_saveindices==T)
  {write.table(tabindices, paste("savetabindices ", key,".csv"), sep=";", col.names = T, row.names = F)}



  #reagrege dtoto
  dtoto <- do.call("rbind", sp_dtoto)
  row.names(dtoto) <- 1:length(dtoto$usm)

  dresdec <- do.call("rbind", ls_tabSDall)
  row.names(dresdec) <- 1:dim(dresdec)[1]

  res_cor <- do.call("rbind",ls_res_cor)
  row.names(res_cor) <- 1:dim(res_cor)[1]

  #pourquoi merge fonctionne pas?
  dtoto <- cbind(dtoto, dresdec[,2:dim(dresdec)[2]])
  dtoto <- cbind(dtoto, res_cor[,1:(dim(res_cor)[2]-1)])

  #return
  nomf <- paste("dtoto", nomf, ".csv", sep="")
  path_outf <- if (outpath == "") nomf else file.path(outpath , nomf)

  write.table(dtoto, path_outf, sep=";", col.names = T, row.names = F)
  list(dtoto=dtoto, key=key, name=ls_toto)
}



# separe lecture fichier de traitement et calcul ->build_dtoto_binarySim

# build_dtoto <- function(sp_dtoto, key, DOYdeb, DOYScoupe)
# {
#   ls_toto_paquet <- sp_dtoto[[key]]$name
#
#   #recuperation par paquet des fichiers de base (pas de stockage de l'ensemble des fichiers en memoire)
#   ltoto <- read_ltoto(ls_toto_paquet)
#   #version locale du paquet de doto
#   dtoto <- sp_dtoto[[key]]
#
#   #recup du nom des esp
#   mix <- strsplit(ls_toto_paquet[1], '_')[[1]][4] #suppose paquet fait par traitement
#   esp <- strsplit(mix, '-')[[1]][1] #'Fix2'
#   esp2 <- strsplit(mix, '-')[[1]][2] #'nonFixSimTest'
#
#   #visu des rendement moyen m2 / a un DOY
#   surfsolref <- NULL
#   nbplt <- NULL
#   nbplt1 <- NULL
#   nbplt2 <- NULL
#
#   #DOYScoupe <- c(165,199,231,271,334)#Avignon
#   #DOYScoupe <- c(187,229,282,334)#Lusignan
#   #DOYdeb <- 60
#   idDOYScoupe <- DOYScoupe - DOYdeb
#   Ytot <- NULL
#   Ycoupe <- NULL
#
#   YEsp1 <- NULL
#   YEsp2 <- NULL
#
#   QNfix <- NULL
#   QNupttot <- NULL
#   QNuptleg <- NULL
#
#   PARi1 <- NULL
#   PARi2 <- NULL
#   Surf1 <- NULL
#   Surf2 <- NULL
#   LRac1 <- NULL
#   LRac2 <- NULL
#   MRac1 <- NULL
#   MRac2 <- NULL
#   MGini1 <- NULL
#   MGini2 <- NULL
#   MAlive1 <- NULL
#   MAlive2 <- NULL
#
#   for (i in 1:length(ls_toto_paquet))#(ls_toto))
#   {
#     name <- ls_toto_paquet[i]
#     damier <- strsplit(name, '_')[[1]][5]
#     dat <- ltoto[[name]]
#     s <- dat[dat$V1=='pattern',3]#m2
#     surfsolref <- cbind(surfsolref, as.numeric(as.character(s)))
#     nb <- length(dat)-2
#     nbplt <- cbind(nbplt, nb)
#
#     #Y Totaux
#     MSaerien <- as.matrix(dat[dat$V1=='MSaerien' & dat$steps %in% DOYScoupe,3:(3+nb-1)], ncol=nb)
#     ProdIaer <- rowSums(MSaerien) / s
#     Ycoupe <- rbind(Ycoupe, ProdIaer)
#     Ytot <- cbind(Ytot, sum(ProdIaer))#cumul des 5 coupes
#
#
#     #N totaux et fixation
#     Qfix <- as.matrix(dat[dat$V1=='Qfix',3:(3+nb-1)], ncol=nb)
#     Qfix <- as.numeric(rowSums(Qfix) / s)
#     Nuptake_sol_tot <- as.matrix(dat[dat$V1=='Nuptake_sol',3:(3+nb-1)], ncol=nb)
#     Nuptake_sol_tot <- as.numeric(rowSums(Nuptake_sol_tot) / s)
#     QNfix <-cbind(QNfix, sum(Qfix))
#     QNupttot <- cbind(QNupttot, sum(Nuptake_sol_tot))
#
#     #YEsp1
#     #esp <- 'Fix2'#'Fix3'#'Fix1'#'Fix' #pourquoi c'est ce nom au lieu de Fix???
#     #esp2 <- 'nonFixSimTest'#'nonFix1'#'nonFix0' #pourquoi c'est ce nom au lieu de Fix???
#
#     nomcol <- names(ltoto[[name]])
#     #if (esp==esp2 & grep('damier', damier)==1)#si deux fois le meme nom d'espece, mais mixture damier
#     #{
#     #  idcols <- as.logical(didcols[didcols$damier==damier,1:66])#idcols lu dans fichier qui leve les ambiguite
#     #} else
#     #{
#     idcols <- grepl(esp, nomcol) & !grepl(esp2, nomcol)#contient esp1 et pas esp2
#     #}
#
#     dat1 <- cbind(ltoto[[name]][,c(1:2)], ltoto[[name]][,idcols])
#     nb1 <- length(dat1)-2
#     nbplt1 <- cbind(nbplt1, nb1)
#     if (nb1>0)
#     {
#       MS1 <- as.matrix(dat1[dat1$V1=='MSaerien' & dat1$steps %in% DOYScoupe,3:(3+nb1-1)], ncol=nb1)
#       ProdIaer1 <- rowSums(MS1) / s
#       Nuptake_sol_leg <- as.matrix(dat1[dat1$V1=='Nuptake_sol',3:(3+nb1-1)], ncol=nb1)
#       Nuptake_sol_leg <- as.numeric(rowSums(Nuptake_sol_leg) / s)
#       jPARi1 <- rowSums(as.matrix(dat1[dat1$V1=='PARiPlante' & dat1$steps %in% DOYScoupe,3:(3+nb1-1)], ncol=nb1)) / s
#       jSurf1 <- rowSums(as.matrix(dat1[dat1$V1=='SurfPlante' & dat1$steps %in% DOYScoupe,3:(3+nb1-1)], ncol=nb1)) / s
#       jLRac1 <- rowSums(as.matrix(dat1[dat1$V1=='RLTot' & dat1$steps %in% DOYScoupe,3:(3+nb1-1)], ncol=nb1)) / s
#       jMRac1 <- rowSums(as.matrix(dat1[dat1$V1=='MS_rac_fine' & dat1$steps %in% DOYScoupe,3:(3+nb1-1)], ncol=nb1)) / s
#       jMPiv1 <- rowSums(as.matrix(dat1[dat1$V1=='MS_pivot' & dat1$steps %in% DOYScoupe,3:(3+nb1-1)], ncol=nb1)) / s
#       #gini par date sur MSA (ttes les plantes)
#       Gini1 <- NULL
#       for (k in 1:length(DOYScoupe))
#       { Gini1 <- cbind(Gini1, ineq(MS1[k,], type="Gini"))}
#
#       #survie
#       matSV1 <- as.matrix(dat1[dat1$V1 == "aliveB" & dat1$steps %in% DOYScoupe,3:(3+nb1-1)], ncol=nb1)
#       matSV1[matSV1>0] <- 1
#       aliveP1 <-  as.numeric(nbplt)-as.matrix(rowSums(matSV1))
#       aliveDens1 <- t(aliveP1 / s)
#       #MortDens1 <-  as.matrix(rowSums(matSV1)) / s
#
#
#     } else
#     {
#       ProdIaer1 <- 0 #pas de plante de l'esp1
#       Nuptake_sol_leg <- 0
#       jPARi1 <- 0
#       jSurf1 <- 0
#       jLRac1 <- 0
#       jMRac1 <- 0
#       jMPiv1 <- 0
#       Gini1 <- c(NA,NA,NA,NA)
#       aliveDens1 <- c(0,0,0,0)
#     }
#     YEsp1 <- cbind(YEsp1, sum(ProdIaer1))#cumul des 5 coupes
#     QNuptleg <- cbind(QNuptleg, sum(Nuptake_sol_leg))
#     PARi1 <- cbind(PARi1, sum(jPARi1))
#     Surf1 <- cbind(Surf1, sum(jSurf1))
#     LRac1 <- cbind(LRac1, max(jLRac1))
#     MRac1 <- cbind(MRac1, max(jMRac1)+max(jMPiv1))
#     MGini1 <- rbind(MGini1, Gini1)
#     MAlive1 <- rbind(MAlive1, aliveDens1)
#
#     #YEsp2
#     #if (esp==esp2 & grep('damier', damier)==1)#si deux fois le meme nom d'espece, mais mixture damier
#     #{
#     #  idcols <- !as.logical(didcols[didcols$damier==damier,1:66])#idcols lu dans fichier qui leve les ambiguite
#     #  idcols[1:2] <- FALSE #remet a faux les deux premieres colonnes
#     #} else
#     #{
#     idcols <- grepl(esp2, nomcol)#contient esp2
#     #}
#
#     dat2 <- cbind(ltoto[[name]][,c(1:2)], ltoto[[name]][,idcols])
#     nb2 <- length(dat2)-2
#     nbplt2 <- cbind(nbplt2, nb2)
#     if (nb2>0)
#     {
#       MS2 <- as.matrix(dat2[dat2$V1=='MSaerien' & dat2$steps %in% DOYScoupe,3:(3+nb2-1)], ncol=nb2)
#       ProdIaer2 <- rowSums(MS2) / s
#       jPARi2 <- rowSums(as.matrix(dat2[dat2$V1=='PARiPlante' & dat2$steps %in% DOYScoupe,3:(3+nb2-1)], ncol=nb2)) / s
#       jSurf2 <- rowSums(as.matrix(dat2[dat2$V1=='SurfPlante' & dat2$steps %in% DOYScoupe,3:(3+nb2-1)], ncol=nb2)) / s
#       jLRac2 <- rowSums(as.matrix(dat2[dat2$V1=='RLTot' & dat2$steps %in% DOYScoupe,3:(3+nb2-1)], ncol=nb2)) / s
#       jMRac2 <- rowSums(as.matrix(dat2[dat2$V1=='MS_rac_fine' & dat2$steps %in% DOYScoupe,3:(3+nb2-1)], ncol=nb2)) / s
#       jMPiv2 <- rowSums(as.matrix(dat2[dat2$V1=='MS_pivot' & dat2$steps %in% DOYScoupe,3:(3+nb2-1)], ncol=nb2)) / s
#       #gini par date sur MSA (ttes les plantes)
#       Gini2 <- NULL
#       for (k in 1:length(DOYScoupe))
#       { Gini2 <- cbind(Gini2, ineq(MS2[k,], type="Gini"))}
#
#       #survie
#       matSV2 <- as.matrix(dat2[dat2$V1 == "aliveB" & dat2$steps %in% DOYScoupe,3:(3+nb1-1)], ncol=nb2)
#       matSV2[matSV2>0] <- 1
#       aliveP2 <-  as.numeric(nbplt)-as.matrix(rowSums(matSV2))
#       aliveDens2 <- t(aliveP2 / s)
#       #MortDens2 <-  as.matrix(rowSums(matSV2)) / s
#     }
#     else
#     {
#       ProdIaer2 <- 0 #pas de plante de l'esp2
#       jPARi2 <- 0
#       jSurf2 <- 0
#       jLRac2 <- 0
#       jMRac2 <- 0
#       jMPiv2 <- 0
#       Gini2 <- c(NA,NA,NA,NA)
#       aliveDens2 <- c(0,0,0,0)
#     }
#     YEsp2 <- cbind(YEsp2, sum(ProdIaer2))#cumul des 5 coupes
#     #YEsp2 <- cbind(YEsp2, sum(ProdIaer2))#cumul des 5 coupes
#     PARi2 <- cbind(PARi2, sum(jPARi2))
#     Surf2 <- cbind(Surf2, sum(jSurf2))
#     LRac2 <- cbind(LRac2, max(jLRac2))
#     MRac2 <- cbind(MRac2, max(jMRac2)+max(jMPiv2))
#     MGini2 <- rbind(MGini2, Gini2)
#     MAlive2 <- rbind(MAlive2, aliveDens2)
#   }
#
#   dtoto$surfsolref <- as.numeric(surfsolref)
#   dtoto$nbplt <- as.numeric(nbplt)
#   dtoto$nbplt1 <- as.numeric(nbplt1)
#   dtoto$nbplt2 <- as.numeric(nbplt2)
#   dtoto$Ytot <- as.numeric(Ytot)
#   dtoto$densite <- dtoto$nbplt/dtoto$surfsolref
#   dtoto$densite1 <- dtoto$nbplt1/dtoto$surfsolref
#   dtoto$YEsp1 <- as.numeric(YEsp1)
#   dtoto$densite2 <- dtoto$nbplt2/dtoto$surfsolref
#   dtoto$YEsp2 <- as.numeric(YEsp2)
#   dtoto$Semprop1 <- dtoto$densite1/dtoto$densite
#   dtoto$Yprop1 <- dtoto$YEsp1 / (dtoto$YEsp1 +dtoto$YEsp2)
#   dtoto$Yprop2 <- dtoto$YEsp2 / (dtoto$YEsp1 +dtoto$YEsp2)
#   dtoto$QNfix <- as.numeric(QNfix)
#   dtoto$QNupttot <- as.numeric(QNupttot)
#   dtoto$QNuptleg <- as.numeric(QNuptleg)
#   dtoto$QNtot <- dtoto$QNfix + dtoto$QNupttot
#
#   #new var
#   dtoto$Pari1 <- as.numeric(PARi1)
#   dtoto$Pari2 <- as.numeric(PARi2)
#   dtoto$Surf1 <- as.numeric(Surf1)
#   dtoto$Surf2 <- as.numeric(Surf2)
#   dtoto$PhiSurf1 <- as.numeric(PARi1) / (as.numeric(Surf1) + 10e-12)#Phi Surf
#   dtoto$PhiSurf2 <- as.numeric(PARi2) / (as.numeric(Surf2) + 10e-12)
#   dtoto$PhiMass1 <- as.numeric(PARi1) / (as.numeric(YEsp1) + 10e-12)#Phi Mass
#   dtoto$PhiMass2 <- as.numeric(PARi2) / (as.numeric(YEsp2) + 10e-12)
#   dtoto$LRac1 <- as.numeric(LRac1)
#   dtoto$LRac2 <- as.numeric(LRac2)
#   dtoto$MRac1 <- as.numeric(MRac1)
#   dtoto$MRac2 <- as.numeric(MRac2)
#   dtoto$UptNLen1 <- (as.numeric(QNupttot) - as.numeric(QNuptleg)) / (as.numeric(LRac1) + 10e-12)#Uptake par Len
#   dtoto$UptNLen2 <- as.numeric(QNuptleg) / (as.numeric(LRac2) + 10e-12)
#   dtoto$UptNMass1 <- (as.numeric(QNupttot) - as.numeric(QNuptleg)) / (as.numeric(MRac1) + 10e-12)#Uptake par Mass root
#   dtoto$UptNMass2 <- as.numeric(QNuptleg) / (as.numeric(MRac2) + 10e-12)
#   dtoto$gini1 <- rowMeans(MGini1) #moyenne des gini de ttes les dates (sans retirer pltes mortes)
#   dtoto$gini2 <- rowMeans(MGini2)
#   dtoto$alive1 <- as.numeric(MAlive1[,dim(MAlive1)[2]]) #survie derniere date coupe
#   dtoto$alive2 <- as.numeric(MAlive2[,dim(MAlive2)[2]]) #survie derniere date coupe
#
#   dtoto
#
# }
# #fonction a generaliser et a bouger ailleurs
# # a reprendre -pour asso binaire
# # build synthetic df des valeur globale par simul
#



#' Build of an average tabmoy from dtoto with several usms and seeds
#'
#' @export
Build_AverageScTable <- function(dtoto, keysc)
{
  sc <- strsplit(keysc," ")[[1]][1]
  mix <- strsplit(keysc," ")[[1]][2]
  mng <- strsplit(keysc," ")[[1]][3]
  sd_ <- strsplit(keysc," ")[[1]][4]
  #recup d'un scenario
  #sc <- '1-1'
  #mix <- 'Fix2-nonFixSimTest'
  #mng <- 'Lusignan30IrrN2'

  res <- dtoto[dtoto$scenario==sc & dtoto$mix==mix & dtoto$Mng==mng & dtoto$sd==sd_, ]

  #calcul des valeurs moyennes
  x <- by(res$YEsp1, as.factor(res$densite1), mean)
  tabmoy <- data.frame(densite1=as.numeric(names(x)), YEsp1=as.numeric(x))
  x <- by(res$YEsp2, as.factor(res$densite1), mean)
  tabmoy$YEsp2 <- as.numeric(x)
  x <- by(res$Ytot, as.factor(res$densite1), mean)
  tabmoy$Ytot <- as.numeric(x)
  x <- by(res$YEsp1, as.factor(res$densite1), sd)
  tabmoy$YEsp1sd <- as.numeric(x)
  x <- by(res$YEsp2, as.factor(res$densite1), sd)
  tabmoy$YEsp2sd <- as.numeric(x)
  x <- by(res$Ytot, as.factor(res$densite1), sd)
  tabmoy$Ytotsd <- as.numeric(x)
  x <- by(res$Semprop1, as.factor(res$densite1), mean)
  tabmoy$Semprop1 <- as.numeric(x)

  x <- by(res$QNtot, as.factor(res$densite1), mean)
  tabmoy$QNtot <- as.numeric(x)
  x <- by(res$QNupttot, as.factor(res$densite1), mean)
  tabmoy$QNupttot  <- as.numeric(x)
  x <- by(res$QNuptleg, as.factor(res$densite1), mean)
  tabmoy$QNuptleg  <- as.numeric(x)
  x <- by(res$QNfix, as.factor(res$densite1), mean)
  tabmoy$QNfix  <- as.numeric(x)

  x <- by(res$Yprop1, as.factor(res$densite1), mean)
  tabmoy$Yprop1  <- as.numeric(x)

  tabmoy$mix <- mix
  tabmoy$sc <- sc
  tabmoy$Mng <- mng
  tabmoy$keysc <- paste(mix, sc, mng)

  tabmoy
}
# to calculate an average tabmoy from dtoto with several usms and seeds









#################################
## fonction analyse/mef diversite intra



#' This function determines to which decile  is a value in a vector of values
#'
#' @export
Which_decile <- function(valparams)
{
  #to find in which decile is a value in a distribution
  qt <- quantile(valparams, probs=seq(0, 1, 0.1))
  qt1 <- as.numeric(valparams<=qt[[2]])*1
  qt2 <- as.numeric(valparams>qt[[2]] & valparams<=qt[[3]])*2
  qt3 <- as.numeric(valparams>qt[[3]] & valparams<=qt[[4]])*3
  qt4 <- as.numeric(valparams>qt[[4]] & valparams<=qt[[5]])*4
  qt5 <- as.numeric(valparams>qt[[5]] & valparams<=qt[[6]])*5
  qt6 <- as.numeric(valparams>qt[[6]] & valparams<=qt[[7]])*6
  qt7 <- as.numeric(valparams>qt[[7]] & valparams<=qt[[8]])*7
  qt8 <- as.numeric(valparams>qt[[8]] & valparams<=qt[[9]])*8
  qt9 <- as.numeric(valparams>qt[[9]] & valparams<=qt[[10]])*9
  qt10 <- as.numeric(valparams>qt[[10]])*10
  qtn <- qt1+qt2+qt3+qt4+qt5+qt6+qt7+qt8+qt9+qt10
  qtn
}


#' This function ...
#'
#' @export
Build_EvolProportions <- function(MStot, sp_tabSD, sp, var="decile")
{
  #consrtuction d'un tableau res des proportion de MStot par decile d'une espece

  # 1 MStot esp au cour du temps
  dynMtotsp <- as.numeric(rowSums(as.matrix(MStot[,sp_tabSD[[sp]]$nump+1])))
  res <- data.frame(dynMtotsp, t=1:dim(MStot)[1])
  # 2 ajout des proportion pour chaque decile
  for (dec in 10:1)
  {
    #dec <-9 #numero de decile
    lsp <- sp_tabSD[[sp]][sp_tabSD[[sp]][,c(var)]==dec, c("nump")]
    #lsp+1

    frac <- as.numeric(rowSums(as.matrix(MStot[,lsp+1])))*100 / dynMtotsp
    res <- cbind(res, frac)
  }
  names(res) <- c("MStot_esp","t", "dec10", "dec9", "dec8", "dec7", "dec6", "dec5", "dec4", "dec3", "dec2", "dec1")

  res
}
#proportion de MStot de l'espece par decile a chaque t


#' This function ...
#'
#' @export
BuildResDecil <- function(MStot, sp_tabSD, param_name="")
{
  #fonction pour calculer les decile /sp et a partir des tabSP (avec decile pour 1 parametre donne)

  #sp_tabSD <- split(resread[["ls_tabSD"]][[1]], resread[["ls_tabSD"]][[1]]$name)
  #MStot <- resread[["ls_MStot"]][[1]]

  sp <- names(sp_tabSD)[1]#"Fix0"
  x1 <- Build_EvolProportions(MStot, sp_tabSD, sp)
  IDlastday <- dim(x1)[1]
  #dec1 <- sum(x1[IDlastday,c(3)])
  dec3_1 <- sum(x1[IDlastday,c(3:5)])
  dec5_1 <- sum(x1[IDlastday,c(3:7)])

  sp <- names(sp_tabSD)[2]#"Fix1"
  x2 <- Build_EvolProportions(MStot, sp_tabSD, sp)
  IDlastday <- dim(x2)[1]
  dec3_2 <- sum(x2[IDlastday,c(3:5)])
  dec5_2 <- sum(x2[IDlastday,c(3:7)])

  res <- data.frame(dec3_1, dec5_1, dec3_2, dec5_2)
  names(res) <- paste(c("dec3", "dec5", "dec3", "dec5"), param_name, c(names(sp_tabSD)[1], names(sp_tabSD)[1], names(sp_tabSD)[2], names(sp_tabSD)[2]),  sep="_")

  res
  #
}
#for last day IDlastday / decil 3 et decile5
# IDlastday par defaut, mais rendre possible de passer un ID en argument?




#' This function ...
#'
#' @export
ls_idvois_ordre1 <- function(n, cote, nblignes)
{
  # pour une plante n, dans un dispocitif regulier arrange en colonnes croissantes de cote indiv
  nbindiv <- cote * nblignes
  ls_defaut <- c(n - (cote + 1), n - cote, n - (cote - 1), n - 1, n + 1, n + (cote - 1), n + cote, n + (cote + 1))

  if (n %% cote == 0)  # bord haut
  {
    ls_defaut[1] <- ls_defaut[1] + cote
    ls_defaut[4] <- ls_defaut[4] + cote
    ls_defaut[6] <- ls_defaut[6] + cote
  }

  if ((n + 1) %% cote == 0)  # bord bas
  {
    ls_defaut[3] <- ls_defaut[3] - cote
    ls_defaut[5] <- ls_defaut[5] - cote
    ls_defaut[8] <- ls_defaut[8] - cote
  }


  for (i in 1:length(ls_defaut))
  {
    if (ls_defaut[i] < 0)  # bord gauche
    {ls_defaut[i] <- ls_defaut[i] + nbindiv}

    if (ls_defaut[i] >= nbindiv)  # bord droit
    {ls_defaut[i] <- ls_defaut[i] - nbindiv}
  }

  ls_defaut
}
#fonction pour determiner l'id des voisins d'ordre 1
#ls_idvois_ordre1(9,6,4)#marche pas pour zero
#pour ordre 2: voisin d'ordre 1 de tous tes voisins!
#!! prevu pour id python commencant a zero


#' This function ...
#'
#' @export
def_indice_vois5050 <- function(cote, nblignes)
{
  #id des voisins pour un damier a 50/50 (1 sur 2 est un Kin/nonKin)

  #cote <- 16
  #nblignes <- 16

  ls_idvois <- vector("list", length=(cote*nblignes))
  names(ls_idvois) <- 1:(cote*nblignes)
  ls_idKin <- vector("list", length=(cote*nblignes))
  names(ls_idKin) <- 1:(cote*nblignes)
  ls_idnonKin <- vector("list", length=(cote*nblignes))
  names(ls_idnonKin) <- 1:(cote*nblignes)

  for (i in 1:(cote*nblignes))
  {
    idvois <- ls_idvois_ordre1(i-1, cote, nblignes) +1 # appel avec i-1 (pour comme nump python) # ajout 1 a sortie pour rang R
    ls_idvois[[i]] <- idvois
    ls_idKin[[i]] <- idvois[c(1,3,6,8)]
    ls_idnonKin[[i]] <- idvois[c(2,4,5,7)]
  }

  list(ls_idvois, ls_idKin, ls_idnonKin)
}
#ls_idv <- def_indice_vois5050(cote=16, nblignes=16)
#ls_idvois <- ls_idv[[1]]
#ls_idKin <- ls_idv[[2]]
#ls_idnonKin <- ls_idv[[3]]
#a generaliser pour autres configuations que 50/50 pour ls_idKin, ls_idnonKin (ou a lire qqs part!)



#' This function ...
#'
#' @export
calc_norm_par <- function(tabpar,lspar, plot_=F, main_="")
{
  # fonction pour Calcul des valeur normalisee (par la moyenne) des parametresSD et la moyenne des valeurs normalisee
  # avec plot_ a True et les coord x,y,  fait un graph de visu

  nbpar <- length(lspar)
  ls_resNorm <- vector("list", length=(nbpar+1))
  names(ls_resNorm) <- c(lspar, "mean_norm_par")

  ls_col_ <- 1:nbpar #a passer en argument eventuellement


  Val_par <- tabpar[,c(lspar[1])]
  #normalise par la moyenne (de ce qui est donne en entree: communaute ou population)
  norm_par <- Val_par/mean(Val_par)
  ls_resNorm[[lspar[1]]] <- norm_par
  mean_norm_par <- norm_par

  if (plot_ == T)
  {plot(tabpar$x, tabpar$y, cex=1.5*norm_par,col="blue", main=main_,xlab="",ylab="")}

  for (i in 2:nbpar)
  {
    Val_par <- tabpar[,c(lspar[i])]
    norm_par <- Val_par/mean(Val_par)
    ls_resNorm[[lspar[i]]] <- norm_par
    mean_norm_par <- mean_norm_par+norm_par

    if (plot_ == T)
    {points(tabpar$x, tabpar$y, cex=1.5*norm_par,col=ls_col_[i])}

  }
  ls_resNorm[["mean_norm_par"]] <- mean_norm_par/nbpar
  names(ls_resNorm)[1:nbpar] <- paste(lspar,"Norm", sep="")
  ls_resNorm <- as.data.frame(ls_resNorm)
  ls_resNorm
}

#ls_resNorm <- calc_norm_par(x ,lspar, plot_=F)
#ParamNorm <- calc_norm_par(temptab[,lspar] ,lspar, plot_=F)$mean_norm_par




#' This function ...
#'
#' @export
calc_neighb_param <- function(tabpar,lspar, ls_idvois, ls_idKin, ls_idnonKin, cote=16, nblignes=16)
{
  #calculate average parameter value of order 1 neighbours, with kin and non kin in a binary mixture
  #can be used for any vector and any list of lspar (not only parameters)

  nbpar <- length(lspar)
  ls_res <- vector("list", length=nbpar)
  names(ls_res) <- c(lspar)

  for (param_name in lspar)
  {

    #param_name <- lspar[1]
    Val_param <- tabpar[,c(param_name)]


    ParaMvois <- NULL
    ParaMKin <- NULL
    ParaMnonKin <- NULL
    for (i in 1:(cote*nblignes))
    {
      #ALL ordre 1
      ParaMvois <- cbind(ParaMvois, mean(Val_param[ls_idvois[[i]]]))
      #Kin/NonKin
      ParaMKin <- cbind(ParaMKin, mean(Val_param[ls_idKin[[i]]]) )
      ParaMnonKin <- cbind(ParaMnonKin, mean(Val_param[ls_idnonKin[[i]]]) )
    }
    ParaMvois <- as.numeric(ParaMvois)
    ParaMKin <- as.numeric(ParaMKin)
    ParaMnonKin <- as.numeric(ParaMnonKin)

    res <- data.frame(ParaMvois, ParaMKin, ParaMnonKin)
    names(res) <- paste(param_name, c("Mvois", "MKin", "MnonKin"), sep="")


    ls_res[[param_name]] <- res

  }

  res <- as.data.frame(ls_res)
  names(res) <- as.character(as.data.frame(t(as.data.frame(strsplit(names(res),"\\."))))$V2)

  res
}
#pourrait prevoir de mettre ls_idvois, ls_idKin, ls_idnonKin dans la table d'entree au prelablable
#lire cote et nblignes autrement??
#calc_neighb_param(x,lspar, ls_idvois, ls_idKin, ls_idnonKin)




#' This function ...
#'
#' @export
Calc_MSindiv_Corr <- function(ltoto, ls_toto_paquet, ls_paramSD, key, ls_idvois, ls_idKin, ls_idnonKin, cote, nblignes, lspar=c("Len","Lfeuille","phyllochron", "Vmax2", "ELmax", "PPtreshh"))
{
  ## fonction pour mettre en forme valeurs de parametres normalise, valeur d'effet de voisinnage, et calculer les correlations entre indices

  #key <- names(sp_dtoto)[260]#[330]#[16]#[3]#[31]#[19]#
  #ls_toto_paquet <- sp_dtoto[[key]]$name
  #ltoto <- read_ltoto(ls_toto_paquet)
  #names(ltoto[[ls_toto_paquet]])
  #ltoto[[ls_toto_paquet]]$V1
  dat <- ltoto[[ls_toto_paquet]]
  nb <- dim(dat)[2]-2

  #lspar <-  c("Len","Lfeuille","phyllochron", "Vmax2", "ELmax", "PPtreshh")
  param_name <- "phyllochron"#"Len"#ls_par[1] #pour exemple, pas utilise
  resread <- read_lsSD_MStot(ltoto, ls_paramSD, param_name)
  sp_tabSD <- split(resread[["ls_tabSD"]][[1]], resread[["ls_tabSD"]][[1]]$name)
  MStot <- resread[["ls_MStot"]][[1]]
  #res <- BuildResDecil(MStot, sp_tabSD)

  #c(187,229,282,334) #dates de coupes fixes
  MStot_ini <- as.numeric(MStot[60,])#30
  MStot_coupe1 <- as.numeric(MStot[127,])#65
  MStot_coupe2 <- as.numeric(MStot[169,])#100
  MStot_coupe3 <- as.numeric(MStot[222,])#150
  MStot_fin <- as.numeric(MStot[dim(MStot)[1],])
  #MStot_coupe4 <- as.numeric(MStot[200,])
  #MStot_coupe5 <- as.numeric(MStot[250,])
  #hist(MStot_fin, main=key)
  #hist(MStot_coupe1, main=key)

  #temptab <- resread[["ls_tabSD"]][[1]][, c("nump","name","x","y","retard","Len","Vmax2","ELmax","Lfeuille","phyllochron","PPtreshh")]
  temptab <- resread[["ls_tabSD"]][[1]][, c("nump","name","retard","Len","Vmax2","ELmax","Lfeuille","phyllochron","PPtreshh")]


  #ordonne dans l'ordre des nump!!
  temptab <- temptab[order(temptab$nump),]


  #Val_param <- temptab[,c(param_name)]#temptab$phyllochron
  #Val_param <- temptab$Len
  #hist(Val_param, main=key)

  #calcul de la valeur normalisee des parametres (multi-trait)
  temptab$phyllochron[temptab$phyllochron<8] <- 8 #pour les valeur <0 mise a 10-10!
  temptab$phyllochron <- 1/(temptab$phyllochron)
  temptab$PPtreshh <- 24-temptab$PPtreshh
  ParamAllNorm <- calc_norm_par(temptab[,lspar] ,lspar, plot_=F)$mean_norm_par
  temptab$ParamAllNorm <- ParamAllNorm

  #agrege par Light / N (specifique papier beatrice)
  lightPar <- c("Len","Lfeuille","phyllochron")
  ParamLightNorm <- calc_norm_par(temptab[,lightPar] ,lightPar, plot_=F)$mean_norm_par
  temptab$ParamLightNorm <- ParamLightNorm
  NPar <- c("Vmax2", "ELmax", "PPtreshh")
  ParamNNorm <- calc_norm_par(temptab[,NPar] ,NPar, plot_=F)$mean_norm_par
  temptab$ParamNNorm <- ParamNNorm



  #calcul des moyenne des voisins
  x <- temptab[,c(lspar,"ParamAllNorm","ParamLightNorm","ParamNNorm")]
  #transforme param phyllochrone et PPtreshh pour avoir effet positif pour valeur croissante


  resN <- calc_neighb_param(x,c(lspar,"ParamAllNorm","ParamLightNorm","ParamNNorm"), ls_idvois, ls_idKin, ls_idnonKin)
  temptab <- cbind(temptab, resN)
  #caluler les difference pour sp1 et sp2
  temptab$diffMvoisNorm <- temptab$ParamAllNormMvois - temptab$ParamAllNorm
  temptab$diffMKinNorm <- temptab$ParamAllNormMKin - temptab$ParamAllNorm
  temptab$diffMnonKinNorm <- temptab$ParamAllNormMnonKin - temptab$ParamAllNorm
  temptab$diffMvoisLightNorm <- temptab$ParamLightNormMvois - temptab$ParamLightNorm
  temptab$diffMvoisNNorm <- temptab$ParamNNormMvois - temptab$ParamNNorm


  #recup PARiPlante et N uptake plante et faire cumul
  PARi <- dat[dat$V1=='PARiPlante',3:(3+nb-1)] #
  for (i in 1:nb) {PARi[,i] <- cumsum(PARi[,i])}
  Nuptake <- dat[dat$V1=='Nuptake_sol',3:(3+nb-1)] #sans fixation!!!
  for (i in 1:nb) {Nuptake[,i] <- cumsum(Nuptake[,i])}
  PARi_fin <- as.numeric(PARi[dim(PARi)[1],])
  Nuptake_fin <- as.numeric(Nuptake[dim(Nuptake)[1],])


  #calcul du cumul de biomasse, note moyenne, uptake des voisins
  MScumvois <- NULL
  MScumKin <- NULL
  MScumnonKin <- NULL
  PARivois <- NULL
  PARiKin <- NULL
  PARinonKin <- NULL
  Nuptakevois <- NULL
  NuptakeKin <- NULL
  NuptakenonKin <- NULL
  for (i in 1:(cote*nblignes))
  {
    #ALL ordre 1
    MSvois <- sum(MStot_fin[ls_idvois[[i]]])
    MScumvois <- cbind(MScumvois, MSvois)
    PARivois <- cbind(PARivois, sum(PARi_fin[ls_idvois[[i]]]) )
    Nuptakevois <- cbind(Nuptakevois, sum(Nuptake_fin[ls_idvois[[i]]]) )

    #Kin/NonKin
    MScumKin <- cbind(MScumKin, sum(MStot_fin[ls_idKin[[i]]]))
    MScumnonKin <- cbind(MScumnonKin, sum(MStot_fin[ls_idnonKin[[i]]]))
    PARiKin <- cbind(PARiKin, sum(PARi_fin[ls_idKin[[i]]]) )
    NuptakeKin <- cbind(NuptakeKin, sum(Nuptake_fin[ls_idKin[[i]]]) )
    PARinonKin <- cbind(PARinonKin, sum(PARi_fin[ls_idnonKin[[i]]]) )
    NuptakenonKin <- cbind(NuptakenonKin, sum(Nuptake_fin[ls_idnonKin[[i]]]) )

  }
  MScumvois <- as.numeric(MScumvois)
  PARivois <- as.numeric(PARivois)
  Nuptakevois <- as.numeric(Nuptakevois)
  MScumKin <- as.numeric(MScumKin)
  MScumnonKin <- as.numeric(MScumnonKin)
  PARiKin <- as.numeric(PARiKin)
  NuptakeKin <- as.numeric(PARiKin)
  PARinonKin <- as.numeric(PARinonKin)
  NuptakenonKin <- as.numeric(PARinonKin)


  dfMS <- data.frame(nump=temptab$nump, MStot_fin, MStot_ini, MStot_coupe1,MStot_coupe2,MStot_coupe3,PARi=PARi_fin, Nuptake=Nuptake_fin, MScumvois, MScumKin, MScumnonKin, PARivois, PARiKin, PARinonKin, Nuptakevois, NuptakeKin, NuptakenonKin)
  #ratio de capture des ressources avec voisins
  dfMS$ratioLight <- PARi_fin/PARivois
  dfMS$ratioNupt <- Nuptake_fin/Nuptakevois
  #EcardPotentiel <-  MStot_fin/mean(MStot_fin) - ParamAllNorm #pas tres logique en multitrait / simple trait


  temptab <- merge(temptab, dfMS, by="nump")


  #correlation MSindiv avec valeur des parametres / valeur des voisins / ecart des voisins / ressources / ressources des voisins
  #subx <- temptab[,5:dim(temptab)[2]]#new: avec x,y
  subx <- temptab[,3:dim(temptab)[2]]#old: sans x,y
  rescor <- as.data.frame(cor(subx))
  valcorAll <- rescor$MStot_fin
  #barplot(valcorAll, names.arg =row.names(rescor), las=3,cex.names=0.6,main=key)

  #faire un data.frame de ca
  res <- data.frame(t(valcorAll))
  names(res) <- paste("Cor_", row.names(rescor),sep="")

  #Corr par espece
  s_temp <- split(temptab, temptab$name)
  sp <- names(s_temp)[1]#"Fix0"
  #subx <- s_temp[[sp]][,5:dim(temptab)[2]]#new: avec x,y
  subx <- s_temp[[sp]][,3:dim(temptab)[2]]#old: sans x,y
  rescor <- as.data.frame(cor(subx))
  valcorSp1 <- rescor$MStot_fin
  res1 <- data.frame(t(valcorSp1))
  names(res1) <- paste(sp,"_Cor_", row.names(rescor),sep="")

  sp <- names(s_temp)[2]#"Fix1"
  #subx <- s_temp[[sp]][,5:dim(temptab)[2]]#new: avec x,y
  subx <- s_temp[[sp]][,3:dim(temptab)[2]]#old: sans x,y
  rescor <- as.data.frame(cor(subx))
  valcorSp2 <- rescor$MStot_fin
  res2 <- data.frame(t(valcorSp2))
  names(res2) <- paste(sp,"_Cor_", row.names(rescor),sep="")
  ##barplot(valcorSp2, names.arg =row.names(rescor), las=3,cex.names=0.6,main=key)

  res <- cbind(res,res1,res2)
  res$key <- key

  res

  #renvoie aussi du tableau des donnees : temptab
  ls_resOK <- list(res, temptab)
  names(ls_resOK) <- c("tabCorMSindiv", "datIndices")
  ls_resOK

}
#distinguer 2 fonctions? voir 3?: mef et calcul des correlations?
#ls_res_cor_i <- Calc_MSindiv_Corr(ltoto, ls_toto_paquet, ls_paramSD, lspar=c("Len","Lfeuille","phyllochron", "Vmax2", "ELmax", "PPtreshh"))
#ls_res_cor_i[["tabCorMSindiv"]]
#ls_res_cor_i[["datIndices"]]



