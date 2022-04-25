
##############
## Regular plot functions
##
##############



#' Plot 2D spatio-temporal change of soil variables from the outHR file
#'
#' @param dat A dataframe with outHR file data
#' @param var_ Name of the soil variable to plot
#' @param epcouche Soil voxel (cm)
#' @param Boundarycols A vector of two color names defining the extremes of the gradient of values
#' @return 2D spatio-temporal plot of soil variables from the outHR file
#' @export
#' @examples
#' dat <- outHR_0
#' Plot2D_soilVar(dat, var_='FTSW')
#' Plot2D_soilVar(dat, var_='HRp')
#' Plot2D_soilVar(dat, var_='m_NO3', Boundarycols =c("brown", "green"))
#' Plot2D_soilVar(dat, var_='m_NN4', Boundarycols =c("brown", "green"))
Plot2D_soilVar <- function(dat, var_='FTSW', epcouche=5., Boundarycols = c("blue", "red"))
{

  #plot spatio-temporel ftsw

  #var_ <- 'FTSW'
  #epcouche = 5.
  lscol <- colorRampPalette(Boundarycols)( 101 ) #palette de couleur
  sdat <- split(dat, dat$var)


  DOYs <- sdat[[var_]]$DOY
  vals <- sdat[[var_]][,c(-1,-2)]
  nbcouches <- dim(vals)[2]
  #normalisation
  if (var_ == 'm_NO3' | var_ == 'm_NN4')
  {
    vals <- vals/max(vals) #normalisation / max
  }

  #plot
  plot(-10,-10,ylim=c(-1*epcouche*nbcouches,-0), xlim=c(min(DOYs),max(DOYs)), main=var_, xlab='DOY', ylab='soil depth')

  #draw a sequence of recatngle
  #jour 1
  j <- 1
  for (j in 1:dim(vals)[1])
  {
    ftswj <- as.numeric(vals[j,])
    cols <- rev(lscol[round((1-ftswj)*100,0)+1])

    xleft <- rep(DOYs[j], nbcouches)
    ybottom <- seq(-1*epcouche*nbcouches, -1*epcouche, epcouche)
    xright <- rep(DOYs[j+1], nbcouches)
    ytop <- seq(-1*epcouche*nbcouches, -1*epcouche, epcouche)+epcouche
    rect(xleft, ybottom, xright, ytop, col=cols, border=cols)
  }

}




#####################
# plot pour coeff compet et Loreau
#
#
#####################

#' Plot single species response to density
#'
#' @param x A dtoto data.frame of the pure species containing Ytot, densite, nbplt and surfsolref vectors
#' @param res The result of a fit with Calc_Beta_coeff function
#' @return A plot of single species response to density with its fit
#' @export
#' @examples
#' dtoto <- tabtoto_compet
#' sp_dtoto <- split(dtoto, dtoto$keysc)
#' key <-"88-1 Fix2-nonFixSimTest Lusignan30IrrN2 -"
#' keypur <- "88-1 nonFixSimTest-nonFixSimTest Lusignan30IrrN2 -"
#' dat <- rbind(sp_dtoto[[key]], sp_dtoto[[keypur]])
#' pur1 <- dat[dat$densite2 == 0., ]
#' res1 <- Calc_Beta_coeff(pur1)
#' Plt_Yresp_densite1(pur1, res1, main="", xlab="densite 1", ylab="Ytot")
Plt_Yresp_densite1 <- function(x, res, ...)
{
  #plot de la reponse a la densite d'une espece pure avec le coeff beta
  plot(x$densite, x$Ytot, ylim=c(0, h= 1/res[["b"]]), ...)
  parameters1 <- summary(res[["model"]])[["parameters"]]
  vals <- Yresp_densite1(a = parameters1[1], b=parameters1[2], densite=seq(0,400,1.))
  lines(seq(0,400,1.), vals, type='l',col=2)
  abline(h= 1/res[["b"]],col=3, lty=2)
  abline(h= 0.5/res[["b"]],col=3, lty=2)
  abline(v= 1/res[["beta"]],col=4, lty=3)
  text(1/res[["beta"]]+30, 50, "1/beta", col=4)
  text(1/res[["beta"]]+30, 0.5/res[["b"]]-50, "b/2", col=3)
  text(200,50,paste ( "beta = b/a = ", round(res[["beta"]], 4)))
  text(200, 200, paste ( "1/a = ", round(1/res[["a"]], 4)))
}


#' Plot two species response to density on the diagonal of density dmax
#'
#' @param x A dtoto data.frame of the mixed species containing Ytot, densite1, densite2, Yesp1, Yesp2, nbplt and surfsolref vectors
#' @param parameters1 The result of a fit with Calc_Beta_coeff function
#' @param parameters2 The result of a fit with Calc_Beta_coeff function
#' @param dmax Density of the diagonal
#' @return A plot of two species response to density with its fit on a diagonal of density
#' @export
#' @examples
#' dtoto <- tabtoto_compet
#' sp_dtoto <- split(dtoto, dtoto$keysc)
#' key <-"88-1 Fix2-nonFixSimTest Lusignan30IrrN2 -"
#' keypur <- "88-1 nonFixSimTest-nonFixSimTest Lusignan30IrrN2 -"
#' dat <- rbind(sp_dtoto[[key]], sp_dtoto[[keypur]])
#' pur1 <- dat[dat$densite2 == 0., ]
#' pur2 <- dat[dat$densite1 == 0., ]
#' diag <- dat[dat$densite == 400., ]
#' iso1 <- pur1[pur1$densite ==min(pur1$densite) , ]
#' iso2 <- pur2[pur2$densite ==min(pur2$densite) , ]
#' res1 <- Calc_Beta_coeff(pur1)
#' res2 <- Calc_Beta_coeff(pur2)
#' resg <- Calc_Gamma_coeffesp12(diag, iso1, iso2, res1, res2)
#' parameters1 <- resg[["parameters1"]]
#' parameters2 <- resg[["parameters2"]]
#' Plot_diag_respFitsd1d2(diag, parameters1, parameters2, dmax=400.)
Plot_diag_respFitsd1d2 <- function(x, parameters1, parameters2, dmax=400., ...)
{
  # Plot des ajustement de gamma sur une diagonale de dispositif de deWit (substitution)
  # x=data.frame de donnee, parameters1 et 2: fits des 2 especes

  plot(x$densite1, x$Ytot, ylim=c(0,2300), ...)
  points(x$densite1, x$YEsp1, col=2)
  points(x$densite1, x$YEsp2, col=4)

  vals <- Yresp_densite2_inv(a = parameters1[1], beta=parameters1[2], gamma=parameters1[3], densite1=seq(0,dmax,1.), densite2=rev(seq(0,dmax,1.)))
  Yesp1 <- (1/vals)*seq(0,dmax,1.)
  lines(seq(0,dmax,1.), Yesp1, type='l',col=2)
  #OK!!

  vals2 <- Yresp_densite2_inv(a = parameters2[1], beta=parameters2[2], gamma=parameters2[3], densite1=rev(seq(0,dmax,1.)), densite2=seq(0,dmax,1.))
  Yesp2 <- (1/vals2)*rev(seq(0,dmax,1.))
  lines(seq(0,dmax,1.), Yesp2, type='l',col=4)

  lines(seq(0,dmax,1.), Yesp1+Yesp2, type='l')
  text(220,1000, paste("gamma1: ",round(parameters1[3], 5)), col=2)
  text(220,900, paste("gamma2: ",round(parameters2[3], 5)), col=4)

}
#Plot_diag_respFitsd1d2(x, parameters1, parameters2, dmax=400.)



#' Plot two species response to density on the one-one diagonal
#'
#' @param x A dtoto data.frame of the mixed species containing Ytot, densite1, densite2, Yesp1, Yesp2, nbplt and surfsolref vectors
#' @param parameters1 The result of a fit with Calc_Beta_coeff function
#' @param parameters2 The result of a fit with Calc_Beta_coeff function
#' @param dmax Density
#' @return A plot of two species response to density with its fit on the one-one diagonal at various densities
#' @export
#' @examples
#' dtoto <- tabtoto_compet
#' sp_dtoto <- split(dtoto, dtoto$keysc)
#' key <-"88-1 Fix2-nonFixSimTest Lusignan30IrrN2 -"
#' keypur <- "88-1 nonFixSimTest-nonFixSimTest Lusignan30IrrN2 -"
#' dat <- rbind(sp_dtoto[[key]], sp_dtoto[[keypur]])
#' pur1 <- dat[dat$densite2 == 0., ]
#' pur2 <- dat[dat$densite1 == 0., ]
#' diag <- dat[dat$densite == 400., ]
#' iso1 <- pur1[pur1$densite ==min(pur1$densite) , ]
#' iso2 <- pur2[pur2$densite ==min(pur2$densite) , ]
#' res1 <- Calc_Beta_coeff(pur1)
#' res2 <- Calc_Beta_coeff(pur2)
#' resg <- Calc_Gamma_coeffesp12(one_one, iso1, iso2, res1, res2)
#' parameters1 <- resg[["parameters1"]]
#' parameters2 <- resg[["parameters2"]]
#' Plot_OneOne_Resp_dtot(one_one, parameters1, parameters2, dmax=400.)
#' Plot_OneOne_prop(one_one, parameters1, parameters2, dmax=400.)
Plot_OneOne_Resp_dtot <- function(x, parameters1, parameters2, dmax=400., ...)
{
  # Plot des ajustement de gamma sur la diagonale 1:1 (50/50 semis) (additif) - pour Ytot
  # x=data.frame de donnee, parameters1 et 2: fits des 2 especes

  plot(x$densite1+x$densite2, x$Ytot, ...)#ylim=c(0,2300), main=titre, xlim=c(0, max(x$densite1+x$densite2)))

  vals <- Yresp_densite2_inv(a = parameters1[1], beta=parameters1[2], gamma=parameters1[3], densite1=seq(0,dmax,1.), densite2=seq(0,dmax,1.))
  Yesp1 <- (1/vals)*seq(0,dmax,1.)
  #lines(seq(0,400,1.)*2, Yesp1, type='l',col=2)

  vals2 <- Yresp_densite2_inv(a = parameters2[1], beta=parameters2[2], gamma=parameters2[3], densite1=seq(0,dmax,1.), densite2=seq(0,dmax,1.))
  Yesp2 <- (1/vals2)*seq(0,dmax,1.)

  Ytot <- Yesp1+Yesp2
  lines(seq(0,dmax,1.)*2, Ytot, type='l')
}
#Plot_OneOne_Resp_dtot(x, parameters1, parameters2, dmax=400.)
#lines(seq(0,400,1.)*2, Ytot-Yesp1, type='l',col=4)



#' Plot two species response to density on the one-one diagonal for species proportions
#'
#' @param x A dtoto data.frame of the mixed species containing Ytot, densite1, densite2, Yesp1, Yesp2, nbplt and surfsolref vectors
#' @param parameters1 The result of a fit with Calc_Beta_coeff function
#' @param parameters2 The result of a fit with Calc_Beta_coeff function
#' @param dmax Density
#' @return A plot of two species response to density with its fit on the one-one diagonal at various densities for species proportions
#' @export
#' @examples
#' dtoto <- tabtoto_compet
#' sp_dtoto <- split(dtoto, dtoto$keysc)
#' key <-"88-1 Fix2-nonFixSimTest Lusignan30IrrN2 -"
#' keypur <- "88-1 nonFixSimTest-nonFixSimTest Lusignan30IrrN2 -"
#' dat <- rbind(sp_dtoto[[key]], sp_dtoto[[keypur]])
#' pur1 <- dat[dat$densite2 == 0., ]
#' pur2 <- dat[dat$densite1 == 0., ]
#' diag <- dat[dat$densite == 400., ]
#' iso1 <- pur1[pur1$densite ==min(pur1$densite) , ]
#' iso2 <- pur2[pur2$densite ==min(pur2$densite) , ]
#' res1 <- Calc_Beta_coeff(pur1)
#' res2 <- Calc_Beta_coeff(pur2)
#' resg <- Calc_Gamma_coeffesp12(one_one, iso1, iso2, res1, res2)
#' parameters1 <- resg[["parameters1"]]
#' parameters2 <- resg[["parameters2"]]
#' Plot_OneOne_Resp_dtot(one_one, parameters1, parameters2, dmax=400.)
#' Plot_OneOne_prop(one_one, parameters1, parameters2, dmax=400.)
Plot_OneOne_prop <- function(x, parameters1, parameters2, dmax=400., ...)
{
  # Plot des ajustement de gamma sur la diagonale 1:1 (50/50 semis) (additif) - pour proportions d'especes
  # x=data.frame de donnee, parameters1 et 2: fits des 2 especes

  plot(x$densite1+x$densite2, x$YEsp1/x$Ytot, col=2, ...)#ylim=c(0,1), xlim=c(0, max(x$densite1+x$densite2)), xlab="",ylab="sp prop", col=2, cex.axis=0.8, ...)
  points(x$densite1+x$densite2, x$YEsp2/x$Ytot, col=4)

  vals <- Yresp_densite2(a = parameters1[1], beta=parameters1[2], gamma=parameters1[3], densite1=seq(0,dmax,1.), densite2=seq(0,dmax,1.))
  Yesp1 <- (1/vals)*seq(0,dmax,1.)
  #lines(seq(0,400,1.)*2, Yesp1, type='l',col=2)

  vals2 <- Yresp_densite2(a = parameters2[1], beta=parameters2[2], gamma=parameters2[3], densite1=seq(0,dmax,1.), densite2=seq(0,dmax,1.))
  Yesp2 <- (1/vals2)*seq(0,dmax,1.)
  Ytot <- Yesp1+Yesp2

  points(seq(0,dmax,1.)*2, Yesp1/Ytot, col=2, type='l')
  points(seq(0,dmax,1.)*2, Yesp2/Ytot, col=4, type='l')
  text(500,0.6, paste("gamma1: ",round(parameters1[3], 5)), col=2, cex=0.75)
  text(500,0.4, paste("gamma2: ",round(parameters2[3], 5)), col=4, cex=0.75)

}
#Plot_OneOne_prop(x, parameters1, parameters2)



#' Plot Loreau's CEi and SEi coefficient for a diagonal
#'
#' @export
Plot_resCE_SE <- function(resCE_SE, ...)
{
  #faire le plot - CE-SE (indices Loreau) pour une diag
  #resCE_SE <- data.frame(Semprop1=x$Semprop1, OYa=x$OYa, CE, SE)

  plot(resCE_SE$Semprop1, resCE_SE$OYa, ...)#ylim=c(-400,400), type='b', ylab="OYa", xlab="Prop1", main=titre)
  #ajouter les surfaces! (un peu transparentes?)

  x <- c(0., resCE_SE$Semprop1, 1.)
  y <- c(0, resCE_SE$SE*0.5,0)#+resCE_SE$SE
  col <- rgb(0,0,1,1/4)#4
  polygon(x,y,col=col)
  x <- c(0., resCE_SE$Semprop1, 1.)
  y <- c(0, resCE_SE$CE,0)
  col <- rgb(1,0,0,1/4)#2
  polygon(x,y,col=col)
  points(resCE_SE$Semprop1, resCE_SE$OYa)
  segments(0,0, 1,0, lty=2)

  text(0.5,-200,paste("mean CE: ", round(mean(resCE_SE$CE),1)), col=2)
  text(0.5,-250,paste("mean SE: ", round(0.5*mean(resCE_SE$SE),1)), col=4)
  text(0.5,-300,paste("mean aOY: ", round(mean(resCE_SE$OYa),1)))
  #pourrait calculer les max!

}
#Plot_resCE_SE(resCE_SE, main="")
#Plot_resCE_SE(resCE_SE, ylim=c(-400,400), type='b', ylab="OYa", xlab="Prop1", main=titre)




#####################
# color palettes
#
#
#####################



#' palette of 100 colors
#'
#' @export
col100 <- function(valrel100, lscols)
{
  #pour gestion des couleur: vecteur 100
  # fonction pour definir un vecteur de couleur a partir de valeur relative et d'une liste de 101 couleur
  #lscols = vecteur de 100 couleurs
  # valrel100 = position dans ce vecteur (% du max)

  #lscols[rdtrel]#pas bon!
  cols_ <- NULL
  for(i in valrel100)
  {
    cols_ <- rbind(cols_, lscols[i+1])
  }
  cols_ <- as.vector(cols_)
  cols_
}




#####################
# plot for simulation reports
#
#
#####################


dynamic_graphs <- function(simmoy, name, obs=NULL, surfsolref=NULL)
{
  #serie de figure pour rapport dynamique d'une simulation Avec ou sans ajouts de points observe
  #pour obs ajoute si dataframe fourni au format obs (teste morpholeg)

  op <- par(mfrow = c(3,1), #lignes, colonnes
            oma = c(5.,2.,3,0) + 0.1, #outer margins c(bottom, left, top, right)
            mar = c(0,4,0,2) + 0.1) #marges internes a chaque compartiment c(bottom, left, top, right)


  #1) Leaf area components
  plot(simmoy$STEPS, simmoy$NBI, type='l', xlab='Time',ylab='Nb phytomere I', labels=F, ylim=c(0,1.5*max(simmoy$NBI)))
  axis(2,labels=T) #remet tick labels y
  title(main=name, outer=T)
  if (!is.null(obs) & 'NBI' %in% names(obs))
  {  points(obs$DOY, obs$NBI, pch=16) }


  plot(simmoy$STEPS, simmoy$NBphyto, type='l', xlab='Time',ylab='Nb phytomere tot', labels=F, ylim=c(0,1.5*max(simmoy$NBphyto)))
  axis(2,labels=T) #remet tick labels y
  if (!is.null(obs) & 'nb_phyto_tot' %in% names(obs))
  { points(obs$DOY, obs$nb_phyto_tot/surfsolref, pch=16) }


  plot(simmoy$STEPS, simmoy$LAI, type='l', xlab='Time',ylab='LAI', labels=F, ylim=c(0,1.5*max(simmoy$LAI)))
  axis(2,labels=T) #remet tick labels y
  axis(1,labels=T) #remet tick labels x
  title(xlab='DOY', outer=T)
  if (!is.null(obs) & 'LAI' %in% names(obs))
  { points(obs$DOY, obs$LAI, pch=16) }
  if (!is.null(obs) & 'surf_tot' %in% names(obs))
  { points(obs$DOY, obs$surf_tot/ (10000*surfsolref), pch=16) } #a reprendre fait 2 courbes actuellement pour eviter bug

  #2)MS et taille
  plot(simmoy$STEPS, -simmoy$RDepth, type='l', xlab='Time',ylab='RDepth', labels=F, ylim=c(-1.5*max(simmoy$RDepth),0))
  axis(2,labels=T) #remet tick labels y
  title(main=name, outer=T)
  if (!is.null(obs) & 'long_pivot' %in% names(obs))
  { points(obs$DOY, -obs$long_pivot, pch=16)}


  plot(simmoy$STEPS, simmoy$Hmax, type='l', xlab='Time',ylab='Hmax', labels=F, ylim=c(0,1.5*max(simmoy$Hmax)))
  axis(2,labels=T) #remet tick labels y
  if (!is.null(obs) & 'Hmax' %in% names(obs))
  { points(obs$DOY, obs$Hmax, pch=16) }

  plot(simmoy$STEPS, simmoy$MSA, type='l', xlab='Time',ylab='MS', labels=F, ylim=c(0,1.5*max(simmoy$MSA)))
  axis(2,labels=T) #remet tick labels y
  axis(1,labels=T) #remet tick labels x
  title(xlab='DOY', outer=T)
  points(simmoy$STEPS, simmoy$MSrac, type='l', lty=2)
  if (!is.null(obs) & 'MSaerien' %in% names(obs) & 'MSroot_tot' %in% names(obs))
  {
    points(obs$DOY, obs$MSaerien/surfsolref, pch=16)
    points(obs$DOY, obs$MSroot_tot/surfsolref)
  }


  #3) fonctions de stress
  plot(simmoy$STEPS, simmoy$FTSW, type='l', xlab='Time',ylab='FTSW', labels=F, ylim=c(0,1.1))
  axis(2,labels=T) #remet tick labels y
  title(main=name, outer=T)

  plot(simmoy$STEPS, simmoy$NNI, type='l', xlab='Time',ylab='NNI', labels=F, ylim=c(0, 1.2*max(simmoy$NNI)))
  axis(2,labels=T) #remet tick labels y

  plot(simmoy$STEPS, simmoy$R_DemandC_Root, type='l', xlab='Time',ylab='R_DemandC_Root', labels=F, ylim=c(0,1.1))
  axis(2,labels=T) #remet tick labels y
  axis(1,labels=T) #remet tick labels x
  title(xlab='DOY', outer=T)
}
#dynamic_graphs(simmoy, onglet, obs, surfsolref)

#sans points d'obsevation
#dynamic_graphs(simmoy, name=names(ltoto)[1])

#avec points d'observation...
#namexl <- "morpholeg14_obs.xls"
#onglet <- "morpholeg14_ISO_timbale"
#obs <- read_excel(paste(pathobs,namexl,sep="\\"), sheet = onglet, col_names = TRUE, na = "")
#dynamic_graphs(simmoy, name=names(ltoto)[1], obs=obs, surfsolref=1)








