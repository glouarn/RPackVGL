


#################################
## lecture fichiers toto



#' This function reads the content of the simulation file "toto" of a series of USM and stocks the output into a list
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

  LAI <- moysimval(ltoto, lsusm, var='SurfPlante', esp, optSD)/ surfsolref
  MSA <- moysimval(ltoto,lsusm, var='MSaerien', esp, optSD)/ surfsolref
  MSArec <- moysimval(ltoto,lsusm, var='MSaerienRec', esp, optSD)/ surfsolref
  MSAnonrec <- moysimval(ltoto,lsusm, var='MSaerienNonRec', esp, optSD)/ surfsolref
  MSpiv <- moysimval(ltoto,lsusm, var='MS_pivot', esp, optSD)/ surfsolref
  MSracfine <- moysimval(ltoto,lsusm, var='MS_rac_fine', esp, optSD)/ surfsolref
  MSrac <- MSpiv + MSracfine
  NBI <- moysimval(ltoto,lsusm, var='NBI', esp, optSD)/ nbplt
  NBI <- pmax(0, NBI - 0.75) #correction des simuls pour les comptages decimaux
  #NBIquart <- quantsimval(ltoto,lsusm, var_='NBI',esp=esp)
  NBphyto <- moysimval(ltoto, lsusm, var='NBphyto', esp, optSD)/ surfsolref
  Nbapex <- moysimval(ltoto, lsusm, var='NBapexAct', esp, optSD)/ surfsolref
  NBphyto <- pmax(0,NBphyto - 0.5*Nbapex) #correction simuls pour les comptages decimaux
  NBsh <- moysimval(ltoto, lsusm, var='NBsh', esp, optSD)/ surfsolref

  RDepth <- moysimval(ltoto,lsusm, var='RDepth', esp, optSD)/ nbplt
  Hmax <- moysimval(ltoto,lsusm, var='Hplante', esp, optSD)/ nbplt
  FTSW <- moysimval(ltoto,lsusm, var='FTSW', esp, optSD)/ nbplt
  NNI <- moysimval(ltoto,lsusm, var='NNI', esp, optSD)/ nbplt
  R_DemandC_Root <- moysimval(ltoto,lsusm, var='R_DemandC_Root', esp, optSD)/ nbplt
  cutNB <- moysimval(ltoto,lsusm, var='cutNB', esp, optSD)/ nbplt
  Npc_aer <- moysimval(ltoto,lsusm, var='Npc_aer', esp, optSD)/ nbplt
  Ndfa <- moysimval(ltoto,lsusm, var='Ndfa', esp, optSD)/ nbplt
  Epsi <- moysimval(ltoto,lsusm, var='epsi', esp, optSD)

  simmoy <- data.frame(STEPS, TT, NBI, NBphyto, LAI, MSA, MSArec, MSAnonrec, MSpiv, MSracfine, MSrac, RDepth, Hmax, FTSW, NNI, R_DemandC_Root, cutNB, Npc_aer,Ndfa,Epsi,NBsh)
  simmoy
}#version revue par Lucas tient cmpte du nom de l'espece dans les assos

#simmoy <- build_simmoy(ltoto, lsusm=names(ltoto))
#simmoy <- build_simmoy(ltoto, lsusm=names(ltoto), esp="timbale")
