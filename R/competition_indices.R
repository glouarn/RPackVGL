
########### functions competition coefficient indices
# Sackeville Hamilton 2001
############



#' Single species response to density
#'
#' @param a A parameter..
#' @param b A parameter..
#' @param densite plant density..
#' @return Yield at density
#' @export
Yresp_densite1 <- function(a ,b, densite)
{
  # eq 2.3 - sackeville hamilton exprimee en reponse au Ytot
  Ytot = densite/(a+b*densite)
  Ytot
}




#' Calculate Beta competition coefficient for intra-specific competition
#'
#' @param x A dtoto data.frame of the pure species containing Ytot, densite, nbplt and surfsolref vectors
#' @return A model fit result with "a","b" and "beta" values
#' @export
#' @examples
#' dtoto <- tabtoto_compet
#' sp_dtoto <- split(dtoto, dtoto$keysc)
#' key <-"88-1 Fix2-nonFixSimTest Lusignan30IrrN2 -"
#' keypur <- "88-1 nonFixSimTest-nonFixSimTest Lusignan30IrrN2 -"
#' dat <- rbind(sp_dtoto[[key]], sp_dtoto[[keypur]])
#' pur1 <- dat[dat$densite2 == 0., ]
#' Calc_Beta_coeff(pur1)
Calc_Beta_coeff <- function(x)
{
  #x = tableau dtoto des culture pure avec Ytot, densite, nbplt et surfsolref
  # eq 2.3 - sackeville hamilton sous forme non lineaire
  #par fit non lineaire

  startlist <- list(a=0.01, b=0.0004)
  model1 <- nls(Ytot~Yresp_densite1(a ,b, densite), data=x, start=startlist )
  parameters1 <- summary(model1)[["parameters"]]

  a1 <- parameters1[1] #intercept
  b1 <- parameters1[2] #slope
  beta1 <- b1/a1
  res <- list(model1, a1,b1,beta1)
  names(res) <- c("model", "a","b","beta")
  res
}

#Calc_Beta_coeff(pur1)
#Calc_Beta_coeff(pur2)




Calc_Beta_coeff_Jul <- function(x)
{
  #x = tableau dtoto des culture pure avec Ytot, densite, nbplt et surfsolref
  # eq 2.3 - sackeville hamilton
  #par fit lineaire sur l'inverse plant
  MY_plant1 <- x$Ytot/x$nbplt*x$surfsolref
  mod1 <- lm(1/MY_plant1 ~ x$densite)
  a1 <- as.data.frame(summary(mod1)[["coefficients"]])$Estimate[1] #intercept
  b1 <- as.data.frame(summary(mod1)[["coefficients"]])$Estimate[2] #slope
  beta1 <- b1/a1
  res <- list(mod1, a1,b1,beta1)
  names(res) <- c("model", "a","b","beta")
  res
}
#inconvenient: sensible aux point pHD, parfois valeur negative de beta
#avantage: plan simple en 2 points de densite utilisable (isole/dense pur)
#Calc_Beta_coeff_Jul(pur1)
#Calc_Beta_coeff_Jul(pur2)





#' Two species response to density
#'
#' @param a A parameter..
#' @param beta A parameter..
#' @param gamma A parameter..
#' @param densite1 plant density..
#' @param densite2 plant density..
#' @return Yield of Esp1 according to densities of Esp1 and Esp2 and intra- / inter- specific competition coefficients
#' @export
Yresp_densite2 <- function(a ,beta, gamma, densite1, densite2)
{
  # eq 2.4 - sackeville hamilton exprimee en reponse au Yespi a densite de deux espece
  #par modele non lineaire
  Yesp1 = densite1 / (a + a*beta*densite1 + a*gamma*densite2)
  Yesp1
}
#Yresp_densite2(a ,beta, gamma, densite1, densite2)




#' Two species response to density with inverse calculation
#'
#' @param a A parameter..
#' @param beta A parameter..
#' @param gamma A parameter..
#' @param densite1 plant density..
#' @param densite2 plant density..
#' @return inverse of Yield of Esp1 according to densities of Esp1 and Esp2 and intra- / inter- specific competition coefficients
#' @export
Yresp_densite2_inv <- function(a ,beta, gamma, densite1, densite2)
{
  # eq 2.4 - sackeville hamilton exprimee en reponse au Yespi a densite de deux espece
  #par modele lineaire sur inverse rendement
  inv_Yi = a + a*beta*densite1 + a*gamma*densite2
  inv_Yi
}


#' Calculate Gamma competition coefficient for inter-specific competition
#'
#' @param x A dtoto data.frame of the mixed species containing Ytot, densite1, densite2, Yesp1, Yesp2, nbplt and surfsolref vectors
#' @param iso1 A dtoto data.frame of the isolated plants for Esp1 containing Ytot, densite1, densite2, Yesp1, Yesp2, nbplt and surfsolref vectors
#' @param iso2 A dtoto data.frame of the isolated plants for Esp2 containing Ytot, densite1, densite2, Yesp1, Yesp2, nbplt and surfsolref vectors
#' @param res1 A model fit for response to density of Esp1 with "a","b" and "beta" values
#' @param res2 A model fit for response to density of Esp2 with "a","b" and "beta" values
#' @param free An optional parameter to force beta parameters as estimated from pure species
#' @return A model fit result "model1", "model2", "parameters1", "parameters2"
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
#' Calc_Gamma_coeffesp12(diag, iso1, iso2, res1, res2)
#' # sans forcage
#' Calc_Gamma_coeffesp12(diag, iso1, iso2, res1, res2, free=T)
Calc_Gamma_coeffesp12 <- function(x, iso1, iso2, res1, res2, free=F)
{
  # x = tableau dtoto des culture pure et associee avec Ytot, densite1, densite2, Yesp1, Yesp2, nbplt et surfsolref
  # iso1 et iso2: ligne equivalente avec valeur des plantes isolee
  #res1 et res2: resultats des fits des reponses en pur des especes
  # !!: actuellement: force a et beta values lors du fit de Yi~Yresp_densite2
  # fit eq 2.4
  #free = T -> laisse fitter les 3 params
  #avec reponse non lineaire

  #Esp1
  df <- x[,c("YEsp1", "densite1", "densite2")]
  df$inv_Yi <- x$densite1/x$YEsp1
  df <- df[df$densite1!=0,] #retire les purs

  #ajout point isole 50/50
  iso50 <- data.frame(YEsp1 = iso1$YEsp1/(iso1$YEsp1+iso2$YEsp2)*iso1$YEsp1, densite1=iso1$densite1/2, densite2=iso1$densite1/2)
  iso50$inv_Yi <- iso50$densite1/iso50$YEsp1
  df <- rbind(df,iso50)

  #avec nls en forcant beta et a!
  startlist <- list(a=res1[["a"]], beta=res1[["beta"]], gamma=0.002)
  if (free==F)
  {
    #force a et beta
    minis <- c(res1[["a"]],res1[["beta"]],-1.)
    maxis <- c(res1[["a"]],res1[["beta"]],1.)
  } else
  {
    #fit libre borne
    #minis <- c(0.,res1[["beta"]],0.)
    #maxis <- c(1.,res1[["beta"]],1.)
    minis <- c(0.,0.,-1.)
    maxis <- c(10.,1.,1.)
  }

  #model1 <- nls(inv_Yi~Yresp_densite2_inv(a ,beta, gamma, densite1, densite2), data=df, start=startlist ,trace=TRUE,algorithm="port",lower=minis,upper=maxis)
  model1 <- nls(YEsp1~Yresp_densite2(a ,beta, gamma, densite1, densite2), data=df, start=startlist ,trace=TRUE,algorithm="port",lower=minis,upper=maxis)
  parameters1 <- summary(model1)[["parameters"]]
  #yes! rq: forcage de a change pas grand chose

  #Esp2
  df <- x[,c("YEsp2", "densite1", "densite2")]
  df$inv_Yi <- x$densite2/x$YEsp2
  df <- df[df$densite2!=0,] #retire les purs

  #ajout point isole 50/50
  iso50 <- data.frame(YEsp2 = iso2$YEsp2/(iso1$YEsp1+iso2$YEsp2)*iso2$YEsp2, densite1=iso1$densite1/2, densite2=iso1$densite1/2)
  iso50$inv_Yi <- iso50$densite1/iso50$YEsp2
  df <- rbind(df,iso50)

  #inverse les noms pour le fit
  names(df) <- c("YEsp1", "densite2", "densite1", "inv_Yi")

  startlist <- list(a=res2[["a"]], beta=res2[["beta"]], gamma=0.002)
  if (free==F)
  {
    minis <- c(res2[["a"]],res2[["beta"]],-1.)
    maxis <- c(res2[["a"]],res2[["beta"]],1.)
  } else
  {
    #minis <- c(0.,res2[["beta"]],0.)
    #maxis <- c(1.,res2[["beta"]],1.)
    minis <- c(0.,0.,-1.)
    maxis <- c(10.,1.,1.)
  }

  #model2 <- nls(inv_Yi~Yresp_densite2_inv(a ,beta, gamma, densite1, densite2), data=df, start=startlist ,trace=TRUE,algorithm="port",lower=minis,upper=maxis)
  model2 <- nls(YEsp1~Yresp_densite2(a ,beta, gamma, densite1, densite2), data=df, start=startlist ,trace=TRUE,algorithm="port",lower=minis,upper=maxis)
  parameters2 <- summary(model2)[["parameters"]]

  res <- list(model1, model2, parameters1, parameters2)
  names(res) <- c("model1", "model2", "parameters1", "parameters2")
  res
}
#A faire: rendre facultatif les iso1 et 2!
#rendre facultatif res11 et res2
#x <- diag
#resg <- Calc_Gamma_coeffesp12(x, iso1, iso2, res1, res2)
#Calc_Gamma_coeffesp12(all, iso1, iso2, res1, res2)
#Calc_Gamma_coeffesp12(tribas, iso1, iso2, res1, res2)





#Yresp_densite2_diag <- function(a ,beta, gamma, dtot, densite1)
#{
#  # eq 2.4 - sackeville hamilton exprimee en reponse au Yespi a densite de deux espece
#  # une seule variable dans dipositif de DeWit car dtot=cst (a forcer dans l'optimisation)
#  inv_Yi = a + a*beta*densite1 + a*gamma*(dtot-densite1)
#  inv_Yi
#}
#marche pas plus... seule sur diag







#' Calculate Sij coefficients
#'
#' @export
Calc_Sij_coefficients <- function(res1, res2, parameters1, parameters2)
{
  # substitution rates Sij Eq 2.7
  #esp1
  S12 <- parameters1[3] / res1[["beta"]]
  #esp2
  S21 <- parameters2[3] / res2[["beta"]]
  res <- list(S12, S21)
  names(res) <- c("S12", "S21")
  res
}
#Calc_Sij_coefficients(res1, res2, parameters1, parameters2)


#' Calculate Eij coefficients
#'
#' @export
Calc_Eij_coefficients <- function(res1, res2, parameters1, parameters2)
{
  # relative competitive effect  Eij Eq 2.8
  #esp1
  E12 <- 1/(parameters1[3] / res1[["beta"]])
  #esp2
  E21 <- 1/(parameters2[3] / res2[["beta"]])
  res <- list(E12, E21)
  names(res) <- c("E12", "E21")
  res
}
#Calc_Eij_coefficients(res1, res2, parameters1, parameters2)


#' Calculate Rij coefficients
#'
#' @export
Calc_Rij_coefficients <- function(res1, res2, parameters1, parameters2)
{
  # relative competitive effect  Eij Eq 2.8
  #esp1
  S12 <- parameters1[3] / res1[["beta"]]
  #esp2
  S21 <- parameters2[3] / res2[["beta"]]
  res <- 1 / (S12*S21) #le meme pour les deux
  names(res) <- c("R")
  res
}
#Calc_Rij_coefficients(res1, res2, parameters1, parameters2)
# R bien superieur a zero!






#' Calculate Competition Intensity coefficients
#'
#' @export
Calc_CompetitionIntensity<- function(YEsp1, densite1, iso_ind)
{
  #calculate competition intensity relative to isolated plant of sp (with vectors Yesp and density)
  #eq 2.5
  Yi <- YEsp1/densite1
  #iso_ind <- iso$YEsp1/iso$densite
  res <- log10(iso_ind/Yi)
  res
}

#!! prendre en charge le calcul des moyennes d'isole
#Calc_CompetitionIntensity(x$YEsp1, x$densite1, iso1$YEsp1/iso1$densite1)
#Calc_CompetitionIntensity(x$YEsp2, x$densite2, iso2$YEsp2/iso2$densite2)


#' Calculate RCI coefficients
#'
#' @export
Calc_RCI_coeff <- function(YEsp1, densite1, iso_ind)
{
  # Relative compeition index (Maamouri et al2017)
  Yi <- YEsp1/densite1
  #iso_ind <- iso$YEsp1/iso$densite
  res <- (iso_ind-Yi)/iso_ind
  res
}
#Calc_RCI_coeff(x$YEsp1, x$densite1, iso1$YEsp1/iso1$densite1)
#Calc_RCI_coeff(x$YEsp2, x$densite2, iso2$YEsp2/iso2$densite2)








####################
# Loreau indices






#' Calculate Loreau's CEi coefficient
#'
#' @export
Calc_CEi <- function(Nb,M,deltaRY)
{
  #complementarity effect (Hector et Loreau, 2001)
  #Nb: nb sp du melange
  #M: vecteur des rendement en pur par sp du melange
  # deltaRY: vecteur delta de RY par sp du melange
  CEi <- Nb*mean(M)*mean(deltaRY)
  CEi
}


#' Calculate Loreau's SEi coefficient
#'
#' @export
Calc_SEi <- function(Nb,M,deltaRY)
{
  #selection effect (Hector et Loreau, 2001)
  #Nb: nb sp du melange
  #M: vecteur des rendement en pur par sp du melange
  # deltaRY: vecteur delta de RY par sp du melange
  SEi <- Nb*cov(deltaRY, M)
  SEi
}


#' Calculate Loreau's CEi and SEi coefficient for a diagonal
#'
#' @export
Calc_CESE_diag <- function(diag)
{
  #calculate CE and SE coeffcicient (Hector et Loreau 2001)
  # for a tabmoy or diag of a scenario (1 seed), with minimum pure controls ans 1 mixture

  x <- diag

  x$M1 <- x[x$Semprop1==1.,c("YEsp1")] #yield esp pur1 (meme densite)
  x$M2 <- x[x$Semprop1==0.,c("YEsp2")] #yield esp pur2 (meme densite)

  #x$deltaRY1 <- x$Yprop1 - x$Semprop1
  #x$deltaRY2 <- x$Yprop2 - (1-x$Semprop1)
  x$deltaRY1 <- x$YEsp1/x$M1-x$Semprop1
  x$deltaRY2 <- x$YEsp2/x$M2 - (1-x$Semprop1)

  x$Ytheo1 <- x$M1*x$Semprop1
  x$Ytheo2 <- x$M2*(1-x$Semprop1)
  x$Yteo <- x$Ytheo1  + x$Ytheo2
  x$Ytot - x$Yteo
  x$OYa <- x$Ytot - x$Yteo#x$YEsp1 - x$Semprop1*x$M1 + x$YEsp2 - (1-x$Semprop1)*x$M2

  #somme des RYi*Mi obs - somme des RYi/Mi theo
  #(x$M1 * x$YEsp1/x$M1 + x$M2 * x$YEsp2/x$M2) - (x$M1 * x$Semprop1 + x$M2 *(1-x$Semprop1))
  # bon!
  #somme avec les deltaRY
  #x$M1 * (x$YEsp1/x$M1-x$Semprop1) + x$M2*(x$YEsp2/x$M2 - (1-x$Semprop1))
  #x$M1 * x$deltaRY1 + x$M2*x$deltaRY2
  # bon!

  #selection des liste de vecteur des asso (retire purs)
  ls_vdeltaRY <- x[, c("deltaRY1", "deltaRY2")]
  ls_vM <- x[, c("M1", "M2")]

  #calcul par couvert
  CE <- NULL
  SE <- NULL
  for (i in 1:length(ls_vdeltaRY[,1]))
  {
    #i <-2 #numero ligne
    Nb <- length(ls_vdeltaRY[i,])
    M <- as.numeric(ls_vM[i,])
    deltaRY <- as.numeric(ls_vdeltaRY[i,])

    CEi <- Calc_CEi(Nb,M,deltaRY) # Nb*mean(M)*mean(deltaRY)
    SEi <- Calc_SEi(Nb,M,deltaRY)# Nb*cov(deltaRY, M)
    CE <- rbind(CE, CEi)#rbind(CE, CEi/Nb)
    SE <- rbind(SE, SEi)#rbind(SE, SEi/Nb)
    #facteur 2 (Nb qui traine) -> #c'est reference qu'est somme pas moy des cultures pures !
    #?OY = CE+0.5*SE

  }

  x$CE <- CE
  x$SE <- SE
  x
}
