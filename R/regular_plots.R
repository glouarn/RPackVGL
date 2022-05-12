
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




#####################
# plot for simulations of de Wit substitution design
#
#
#####################



#' Plot of Overyielding against species proportion
#'
#' @export
YtotvsProp <- function(tabmoy, Ymax=2200, nom="", optProp="sowing",visuplot=T, visutext=T, supindices=T, labx=NA,col2=2,...)
{
  ## calcul des composante de l'overyielding biomasse et fait un plot (visutext=visualisation des valeurs; visuplot=visulaisation des )


  #actual or sowing proportions?
  if (optProp=="sowing")
  {
    xx <- tabmoy$Semprop1
    if (is.na(labx))
    {labx <- 'Sowing proportion (Sp. 1)'}
  }
  if (optProp=="actual")
  {
    xx <- tabmoy$Yprop1
    if (is.na(labx))
    {labx <- 'Actual proportion (Sp. 1)'}
  }

  #esp pures
  moyesp1_pur <- mean(tabmoy[tabmoy$Semprop1==1., c("Ytot")])
  moyesp2_pur <- mean(tabmoy[tabmoy$Semprop1==0., c("Ytot")])

  #calcul des fits des valeurs moyennes
  #modeltot <- smooth.spline(xx, tabmoy$Ytot)
  modeltot <- tryCatch(smooth.spline(xx, tabmoy$Ytot), error=function(e) smooth.spline(xx, tabmoy$Ytot, nknots =5))
  inttot = sum(predict(modeltot, seq(0,1,0.001))$y*0.001) - (moyesp1_pur + moyesp2_pur)/2

  #modelesp1 <- smooth.spline(xx, tabmoy$YEsp1)
  modelesp1 <- tryCatch(smooth.spline(xx, tabmoy$YEsp1), error=function(e) smooth.spline(xx, tabmoy$YEsp1, nknots =5))
  intesp1 = sum(predict(modelesp1, seq(0,1,0.001))$y*0.001) - (moyesp1_pur + 0)/2

  #modelesp2 <- smooth.spline(xx, tabmoy$YEsp2)
  modelesp2 <- tryCatch(smooth.spline(xx, tabmoy$YEsp2), error=function(e) smooth.spline(xx, tabmoy$YEsp2, nknots =5))
  intesp2 = sum(predict(modelesp2, seq(0,1,0.001))$y*0.001) - (0 + moyesp2_pur)/2


  #plot des valeur moyennes Ytot si option activee
  if (visuplot==T)
  {
    plot(xx, tabmoy$Ytot, ylim=c(0,Ymax), xlab=labx, ylab='Shoot biomass (g.m-2)', main=nom, ...)
    #segments(tabmoy$Semprop1, tabmoy$Ytot, tabmoy$Semprop1, tabmoy$Ytot+tabmoy$Ytotsd)
    #segments(tabmoy$Semprop1, tabmoy$Ytot, tabmoy$Semprop1, tabmoy$Ytot-tabmoy$Ytotsd)
    #segments(xx[1], tabmoy$Ytot[1], xx[7], tabmoy$Ytot[7], lty=2)
    segments(xx[1], moyesp2_pur, xx[7], moyesp1_pur, lty=2)
    lines(modeltot)

    points(xx, tabmoy$YEsp1,col=col2)
    #segments(xx[1], tabmoy$YEsp1[1], xx[7], tabmoy$YEsp1[7], lty=2, col=col2)
    segments(xx[1], 0, xx[7], moyesp1_pur, lty=2, col=col2)
    lines(modelesp1, col=col2)

    points(xx, tabmoy$YEsp2,col=4)
    #segments(xx[1], tabmoy$YEsp2[1], xx[7], tabmoy$YEsp2[7], lty=2, col=4)
    segments(xx[1], moyesp2_pur, xx[7], 0, lty=2, col=4)
    lines(modelesp2, col=4)

  }

  if (visutext==T & visuplot==T)
  {
    text(0.15, 0.97*Ymax, paste('overY: ' ,round(inttot,2)))
    text(0.15, 0.93*Ymax, paste('Sp1: ' , round(intesp1,2)),col=2)
    text(0.15,0.89*Ymax, paste('Sp2: ' ,round(intesp2,2)),col=4)
  }



  res <- as.list(c(inttot, intesp1, intesp2))
  names(res) <- c("inttot", "intesp1", "intesp2")

  #calcul et renvoie valeurs calculees supplementaires
  if (supindices==T)
  {
    #cacul des autres indices
    ids <- CalcOpt(modeltot , xx, tabmoy$Ytot)
    propOpt <- ids[1]
    OverMax <- ids[2]
    Ytotmax <- ids[4]
    propYtotmax <- ids[5]
    ids1 <- CalcPropactu50(modelesp1, modelesp2, ids[3])
    propsowing50 <- ids1[2]
    propLegOtp <- ids1[1]

    res <- as.list(c(inttot, intesp1, intesp2, propOpt, OverMax, propsowing50, propLegOtp, Ytotmax, propYtotmax))
    names(res) <- c("inttot", "intesp1", "intesp2", "propOpt", "OverMax", "propsowing50", "propLegOtp", "Ytotmax", "propYtotmax")
  }


  res

}



#' Plot of N-Overyielding against species proportion
#'
#' @export
QNtotvsProp <- function(tabmoy, Ymax=100, nom="", optProp="sowing", visuplot=T, visutext=T, supindices=T, labx=NA,...)
{
  ## calcul des composante de l'overyielding Ntot et fait un plot (visutext=visualisation des valeurs; visuplot=visulaisation des plots)


  #actual or sowing proportions?
  if (optProp=="sowing")
  {
    xx <- tabmoy$Semprop1
    if (is.na(labx))
    {labx <- 'Sowing proportion (Sp. 1)'}
  }
  if (optProp=="actual")
  {
    xx <- tabmoy$Yprop1
    if (is.na(labx))
    {labx <- 'Actual proportion (Sp. 1)'}
  }

  #calcul des fits des valeurs moyennes
  modeltot <- smooth.spline(xx, tabmoy$QNtot)
  intoverN = sum(predict(modeltot, seq(0,1,0.001))$y*0.001) - (tabmoy$QNtot[1]+tabmoy$QNtot[7])/2
  intQNtot = sum(predict(modeltot, seq(0,1,0.001))$y*0.001)

  modelesp1 <- smooth.spline(xx, tabmoy$QNupttot)
  intNupt = sum(predict(modelesp1, seq(0,1,0.001))$y*0.001)
  intFix = intQNtot-intNupt

  modeleg <- smooth.spline(xx, tabmoy$QNuptleg)
  intleg = sum(predict(modeleg, seq(0,1,0.001))$y*0.001) - (tabmoy$QNuptleg[1]+tabmoy$QNuptleg[7])/2


  if (visuplot==T)
  {
    plot(xx, tabmoy$QNtot, ylim=c(0,Ymax), xlab=labx, ylab='Plant N (g N.m-2)', main=nom, ...)
    segments(xx[1], tabmoy$QNtot[1], xx[7], tabmoy$QNtot[7], lty=2)
    lines(modeltot)

    points(xx, tabmoy$QNupttot,col=2)
    #segments(xx[1], tabmoy$QNupttot[1], xx[7], tabmoy$QNupttot[7], lty=2, col=2)
    lines(modelesp1, col=2)

    points(xx, tabmoy$QNuptleg,col=4)
    segments(xx[1], tabmoy$QNuptleg[1], xx[7], tabmoy$QNuptleg[7], lty=2, col=4)
    lines(modeleg, col=4)

  }

  if (visutext==T)
  {
    text(0.15,Ymax, paste(round(intoverN,2), '(over)'))
    text(0.15,0.97*Ymax, paste(round(intFix,2), '(Fix)'),col=1)
    text(0.15,0.94*Ymax, paste(round(intNupt,2), '(Nupt)'),col=2)
    text(0.15,0.91*Ymax, paste(round(intleg,2), '(leg)'),col=4)
  }

  res <- as.list(c(intoverN, intQNtot, intNupt, intFix, intleg))
  names(res) <- c("intoverN", "intQNtot", "intNupt", "intFix", "intleg")


  if (supindices==T)
  {
    #cacul des autres indices
    ids <- CalcOpt(modeltot , xx, tabmoy$QNtot)
    propOptN <- ids[1]
    OverMaxN <- ids[2]
    QNmax <- ids[4]
    propQNmax <- ids[5]
    res <- as.list(c(intoverN, intQNtot, intNupt, intFix, intleg, propOptN, OverMaxN, QNmax, propQNmax))
    names(res) <- c("intoverN", "intQNtot", "intNupt", "intFix", "intleg", "propOptN", "OverMaxN", "QNmax", "propQNmax")

  }

  res

}




OverYvsAll <- function(ls_tabmoys, key, Ymax=300, nom="", optProp="sowing", visuplot=T,labx=NA,laby=NA,...)
{
  #key <- ls_keysc[20]
  #figure de tous les overyielding
  ls_keysc = names(ls_tabmoys)

  if (optProp=="sowing" & is.na(labx))
  { labx <- 'Sowing proportion (Sp. 1)'}
  if (optProp=="actual" & is.na(labx))
  { labx <- 'Actual proportion (Sp. 1)'}
  if (optProp=="sowing" & is.na(laby))
  { laby <- 'Apparent Overyielding (g.m-2)'}
  if (optProp=="actual" & is.na(laby))
  { laby <- 'Overyieding (g.m-2)'}

  if (visuplot==T)
  {
    plot(-100, -100, ylim=c(-Ymax,Ymax), xlim=c(0,1), main=nom, xlab=labx, ylab=laby, ...)
    segments(0, 0, 1, 0, col=1)
  }

  resx <- NULL
  resy <- NULL

  for (keysc in ls_keysc)
  {
    #keysc <- ls_keysc[3]
    tabmoy <- ls_tabmoys[[keysc]]

    #xx <- tabmoy$Semprop1#tabmoy$Yprop1#
    yy <- tabmoy$Ytot
    #actual or sowing proportions?
    if (optProp=="sowing")
    {
      xx <- tabmoy$Semprop1
      labx <- 'Sowing proportion (Esp. 1)'
    }
    if (optProp=="actual")
    {
      xx <- tabmoy$Yprop1
      labx <- 'Actual proportion (Esp. 1)'
    }

    lintot <- lsfit(c(xx[1], xx[7]), c(yy[1], yy[7]))
    ylin <- lintot$coefficients[["Intercept"]] + xx*lintot$coefficients[["X"]]
    overY <- yy - ylin

    if (keysc != key)
    {
      if (visuplot==T)
      { points(xx, overY, pch=16, col='light grey') }
      resx <- cbind(resx,xx)
      resy <- cbind(resy,overY)
    } else
    {
      savexx <- xx
      saveyy <- overY
    }
  }
  if (visuplot==T)
  { points(savexx, saveyy, pch=16, col='blue', type='b')}
  resx <- cbind(resx,savexx)
  resy <- cbind(resy,saveyy)
  data.frame(x=as.numeric(resx), y=as.numeric(resy))
}







#####################
# plot for visualising intra-specific diversity
#
#
#####################



#' Plot Area
#'
#' @export
My_AreaPlot <- function(don, lscol="", ...)
{
  #area plot
  #prends un dataframe don avec x en colonne 1 et les n colonnes de y a mettre en ordre decroissnt (+couleur)

  xmin <- min(don[,1])
  xmax <- max(don[,1])
  cumtot <- as.numeric(rowSums(as.matrix(don[,2:dim(don)[2]])))
  ymax <- max(cumtot)

  plot(-100,-100, ylim=c(0,ymax), xlim=c(xmin,xmax), ...)

  for (i in 2:dim(don)[2])
  {
    #i <- 2
    cumi <- as.numeric(rowSums(as.matrix(don[,i:dim(don)[2]])))
    x <- c(xmin, don[,1], xmax)
    y <- c(0, cumi,0)
    col <- if(lscol != "") lscol[i] else i #genere warnings
    polygon(x,y,col=col)
  }

}




#' Plot Map of values
#'
#' @export
PlotMapVal <- function(tab, Val_param, cexfactor = 4., append = F, reverse=F, norm=NA, ...)
{

  # tab df with x, y and val_param values

  #cexfactor = 4.
  #col_="blue"
  #append = F#T
  #tab <- tabindices

  # normalisation of values
  if (is.na(norm) == T & reverse == F)
  {
    val_norm <- max(tab[,c(Val_param)])
  } else if(is.na(norm) == T & reverse == T)
  {
    val_norm <- max(1/tab[,c(Val_param)])
  } else
  {
    val_norm <- norm #lu
  }

  if (reverse == F)
  {
    Val_paramNorm = tab[,c(Val_param)] / val_norm
  } else
  {
    Val_paramNorm = (1/tab[,c(Val_param)])/ max(1/tab[,c(Val_param)])
  }

  # plot
  if (append == F)
  {
    plot(tab$x, tab$y, cex=cexfactor*Val_paramNorm, xlab="x", ylab="y", ...)
  } else
  {
    points(tab$x, tab$y, cex=cexfactor*Val_paramNorm, ...)
  }

}
# plot map of valeur normalisee
#PlotMapVal(tabindices, Val_param="Len", cexfactor = 4., col="blue")
#PlotMapVal(tabindices, Val_param="Len", cexfactor = 4., col="blue", pch=16)

# visu des parametres
#PlotMapVal(tabindices, Val_param="Len", cexfactor = 4., col="blue", main="Parameter values")
#PlotMapVal(tabindices, Val_param="phyllochron", cexfactor = 4., col="red", append = T, reverse=T )
#PlotMapVal(tabindices, Val_param="Lfeuille", cexfactor = 4., col="green", append = T)
#PlotMapVal(tabindices, Val_param="Vmax2", cexfactor = 4., col="yellow", append = T)




PlotDynMStot <- function(MStot, sp_tabSD, sp, lscol="", titre="", ymax=28, append=F)
{
  # plot dynamique de MStot au cours du temps par plante avec couleur selon decile
  if (append==F)
  {
    plot(-10, -10, xlim=c(1,dim(MStot)[1]), ylim=c(0,ymax), main=titre, xlab="t", ylab="MStot")
  }
  for (i in 1:length(sp_tabSD[[sp]]$nump))
  {
    nump <- sp_tabSD[[sp]]$nump[i]
    col <- if(lscol != "") lscol[sp_tabSD[[sp]]$decile[i]] else i #genere warnings
    points(1:dim(MStot)[1], MStot[,nump+1], col=col, type='l')
  }
}



plotMean_Div <-function (matind, xval, title="", xlab="",ylab="",ylim=c(0,100), lscol=NULL)
{
  # plot de la moyenne versus les valeurs par individu
  nbplt <- dim(matind)[2]
  moy_ <-rowMeans(matind, na.rm=T)
  plot(xval, moy_, col="dark grey", lwd=3, main=title, xlab=xlab, ylab=ylab,ylim=ylim)
  for (idp in 1:nbplt)
  {
    if (is.null(lscol))
    {col<-idp}
    else
    {col <- lscol[idp]}

    points(xval, matind[,idp], col=idp, type="l")
  }

}





plot_ranking_lines <- function(tab, id_refdec=1, decHaut=9, decBas=2, id_refline=NA, titre='', ymax=1, xlab='',ylab='')
{
  # plot de trajectoires d'interaction/ranking entre modalite classees en colonnes dans un tableau (tab)
  # visualise selection de decile ou d'individus particuliers dans une population

  #id_refdec <- 3#1 #colone de ref pour les deciles
  #id_refline <- NA#10#
  #decHaut <- 9
  #decBas <- 2
  #titre <- paste(trait, sp)
  #ymax <- 15000

  nb_dates <- dim(tab)[2]
  plot(-10,-10, xlim=c(0,nb_dates+1), ylim=c(0, ymax),main=titre,xlab=xlab,ylab=ylab)
  for (i in 1:dim(dd)[1])
  {
    points(1:nb_dates, as.numeric(tab[i,]), type="b", col="grey")
  }

  #ajout decile de la date 'id_refdec'
  Dec_ <- quantile(tab[,id_refdec], na.rm=T, probs = seq(0, 1, 0.1))
  D9 <- as.numeric(Dec_[decHaut+1]) #decile 1 haut
  D2 <- as.numeric(Dec_[decBas+1]) #decile 2 bas
  for (i in 1:dim(tab)[1])
  {
    if (! is.na(tab[i,id_refdec]))
    {

      if (tab[i,id_refdec] > D9)
      {
        points(1:nb_dates, as.numeric(tab[i,]), type="b", col="red")
      }

      if (tab[i,id_refdec] < D2)
      {
        points(1:nb_dates, as.numeric(tab[i,]), type="b", col="blue")
      }
    }
  }

  #ajout de la ligne a surligner
  if (! is.na(id_refline))
  {
    points(1:nb_dates, as.numeric(tab[id_refline,]), type="b", col=1, lwd=2)
  }
}

#decile sur base 1ere date
#plot_ranking_lines(dd, id_refdec=1, titre=paste(trait, sp), ymax=15000)
#decile sur base derniere date
#plot_ranking_lines(dd, id_refdec=3, titre=paste(trait, sp), ymax=15000)
#visu plante 50 et pas les deciles
#nbp <- 50
#plot_ranking_lines(dd, id_refdec=1, decHaut=10, decBas=0, id_refline=nbp, titre=paste(trait, sp, nbp), ymax=15000)





#fonction des exemple de pairs
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
#df <- data.frame(retard,Val_param,ParaMvois,PARivois,MScumvois, MStot_ini, MStot_fin, MStot_coupe1, MStot_coupe2, MStot_coupe3, MStot_coupe4, MStot_coupe5)
#pairs(df, lower.panel = panel.smooth, upper.panel = panel.cor,gap=0, row1attop=FALSE, main=key)


