
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



Plot_diag_respFitsd1d2 <- function(x, parameters1, parameters2, dmax=400., ...)
{
  # Plot des ajustement de gamma sur une diagonale de dispositif de deWit (substitution)
  # x=data.frame de donnee, parameters1 et 2: fits des 2 especes

  plot(x$densite1, x$Ytot, ylim=c(0,2300), ...)
  points(x$densite1, x$YEsp1, col=2)
  points(x$densite1, x$YEsp2, col=4)

  vals <- Yresp_densite2(a = parameters1[1], beta=parameters1[2], gamma=parameters1[3], densite1=seq(0,dmax,1.), densite2=rev(seq(0,dmax,1.)))
  Yesp1 <- (1/vals)*seq(0,dmax,1.)
  lines(seq(0,dmax,1.), Yesp1, type='l',col=2)
  #OK!!

  vals2 <- Yresp_densite2(a = parameters2[1], beta=parameters2[2], gamma=parameters2[3], densite1=rev(seq(0,dmax,1.)), densite2=seq(0,dmax,1.))
  Yesp2 <- (1/vals2)*rev(seq(0,dmax,1.))
  lines(seq(0,dmax,1.), Yesp2, type='l',col=4)

  lines(seq(0,dmax,1.), Yesp1+Yesp2, type='l')
  text(220,1000, paste("gamma1: ",round(parameters1[3], 5)), col=2)
  text(220,900, paste("gamma2: ",round(parameters2[3], 5)), col=4)

}
#Plot_diag_respFitsd1d2(x, parameters1, parameters2, dmax=400.)


Plot_OneOne_Resp_dtot <- function(x, parameters1, parameters2, dmax=400., ...)
{
  # Plot des ajustement de gamma sur la diagonale 1:1 (50/50 semis) (additif) - pour Ytot
  # x=data.frame de donnee, parameters1 et 2: fits des 2 especes

  plot(x$densite1+x$densite2, x$Ytot, ...)#ylim=c(0,2300), main=titre, xlim=c(0, max(x$densite1+x$densite2)))

  vals <- Yresp_densite2(a = parameters1[1], beta=parameters1[2], gamma=parameters1[3], densite1=seq(0,dmax,1.), densite2=seq(0,dmax,1.))
  Yesp1 <- (1/vals)*seq(0,dmax,1.)
  #lines(seq(0,400,1.)*2, Yesp1, type='l',col=2)

  vals2 <- Yresp_densite2(a = parameters2[1], beta=parameters2[2], gamma=parameters2[3], densite1=seq(0,dmax,1.), densite2=seq(0,dmax,1.))
  Yesp2 <- (1/vals2)*seq(0,dmax,1.)

  Ytot <- Yesp1+Yesp2
  lines(seq(0,dmax,1.)*2, Ytot, type='l')
}
#Plot_OneOne_Resp_dtot(x, parameters1, parameters2, dmax=400.)

#lines(seq(0,400,1.)*2, Ytot-Yesp1, type='l',col=4)



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







############## palettes


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






