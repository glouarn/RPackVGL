
#library(TeachingDemos)# {for subplot function}

# pour vignette test des indices


dtoto <- tabtoto_compet
sp_dtoto <- split(dtoto, dtoto$keysc)



#########################################
#########################################

#selection des fichiers de dtoto (2 jeux de simul dans tabtoto_compet)

#key <- "55-1 Fix2-nonFixSimTest Lusignan30IrrN2 -"
#keypur <- "55-1 nonFixSimTest-nonFixSimTest Lusignan30IrrN2 -" #traitement pur en plus

key <-"88-1 Fix2-nonFixSimTest Lusignan30IrrN2 -"
keypur <- "88-1 nonFixSimTest-nonFixSimTest Lusignan30IrrN2 -"

dat <- rbind(sp_dtoto[[key]], sp_dtoto[[keypur]])
dat$damier <- as.character(dat$damier)



#########################################
#########################################

#selection des donnees
scenar <- strsplit(key, ' ')[[1]][1]

pur1 <- dat[dat$densite2 == 0., ]
pur2 <- dat[dat$densite1 == 0., ]
iso1 <- pur1[pur1$densite ==min(pur1$densite) , ]
iso2 <- pur2[pur2$densite ==min(pur2$densite) , ]

densi <- 400.
diag <- dat[dat$densite == 400., ]
diag50 <- diag[diag$damier=="damier0" | diag$damier=="damier4" | diag$damier=="damier8",] #point 50/50
tribas <- dat[dat$densite <= 400., ] #triangle bas
trihaut <- dat[dat$densite >= 400., ] #triangle bas
one_one <- dat[dat$densite1 == dat$densite2, ]
all <- dat



# visu du plan de simulation des densite
layout(matrix(1:8,2,4))
plot(all$densite1, all$densite2, xlab="densite1", ylab="densite 2", pch=16, cex=2, main ="all", xlim=c(0,400), ylim=c(0,400))
plot(tribas$densite1, tribas$densite2, xlab="densite1", ylab="densite 2", pch=16, cex=2, main ="tribas", xlim=c(0,400), ylim=c(0,400))
plot(diag$densite1, diag$densite2, xlab="densite1", ylab="densite 2", pch=16, cex=2, main ="diag", xlim=c(0,400), ylim=c(0,400))
plot(diag50$densite1, diag50$densite2, xlab="densite1", ylab="densite 2", pch=16, cex=2, main ="diag50", xlim=c(0,400), ylim=c(0,400))
plot(one_one$densite1, one_one$densite2, xlab="densite1", ylab="densite 2", pch=16, cex=2, main ="one_one", xlim=c(0,400), ylim=c(0,400))
plot(pur1$densite1, pur1$densite2, xlab="densite1", ylab="densite 2", pch=16, cex=2, main ="pur1", xlim=c(0,400), ylim=c(0,400))
plot(pur2$densite1, pur2$densite2, xlab="densite1", ylab="densite 2", pch=16, cex=2, main ="pur2", xlim=c(0,400), ylim=c(0,400))


# Rq: cette version des sorties n'est pas la derniere car points pas reguliers!


#all[,c("densite","densite1","densite2","nbplt1")]




#########################################
#########################################
#reponse densite pur

#fit des reponses pures sur donnees pures (a et beta)
#trace visu des reponses pures
x <- pur1
titre <- paste(strsplit(as.character(x$mix[1]), '-')[[1]][1], scenar)
res1 <- Calc_Beta_coeff(x)
Plt_Yresp_densite1(x, res1, main=titre, xlab="densite 1", ylab="Ytot")


x <- pur2
res2 <- Calc_Beta_coeff(x)
titre <- paste(strsplit(as.character(x$mix[1]), '-')[[1]][2], scenar)
res2 <- Calc_Beta_coeff(x)
Plt_Yresp_densite1(x, res2, main=titre, xlab="densite 1", ylab="Ytot")




#########################################
#########################################
#reponse effet voisin

#fit des reponses au voisin (gamma)


# fit peut etre fait sur differents jeu de donnees
x <- diag#tribas#diag50#all#trihaut#
# fit peut etre fait en forcant ou non les reponses a densite en pur
resg <- Calc_Gamma_coeffesp12(x, iso1, iso2, res1, res2)
#resg <- Calc_Gamma_coeffesp12(x, iso1, iso2, res1, res2,free=T) #sans forcage
parameters1 <- resg[["parameters1"]]
parameters2 <- resg[["parameters2"]]


#trace la visu du fit sur diag
x <- diag
titre <- paste("400 ", scenar)
Plot_diag_respFitsd1d2(x, parameters1, parameters2, dmax=400., main=titre, xlab="densite diag", ylab="Ytot")


#trace la visu du fit surone_one
x <- one_one
titre <- paste("50/50 sowing", scenar)
Plot_OneOne_Resp_dtot(x, parameters1, parameters2, dmax=400., ylim=c(0,2300), main=titre, xlim=c(0, max(x$densite1+x$densite2)), xlab="densite one-one", ylab="Ytot")
#library(TeachingDemos)# {for subplot function}
#subplot(Plot_OneOne_prop(x, parameters1, parameters2, dmax=400.), x = c(300, 750),y=c(200,1200))
Plot_OneOne_prop(x, parameters1, parameters2, dmax=400., main=titre, ylim=c(0,1), xlim=c(0, max(x$densite1+x$densite2)), xlab="densite one-one",ylab="sp prop")




### fit des autres indices
#calcul des autres coeff de competition
Sij <- Calc_Sij_coefficients(res1, res2, parameters1, parameters2)
Eij <- Calc_Eij_coefficients(res1, res2, parameters1, parameters2)
Rij <- Calc_Rij_coefficients(res1, res2, parameters1, parameters2)

Sij
Eij
Rij
#!! le calcul de ces indices se fait seulement pour free=F
#-> modif des fonction pour prendre bon a et beta si free=T?



###############
###############
# test competition intensity
x <- all
I_esp1 <- Calc_CompetitionIntensity(x$YEsp1, x$densite1, iso1$YEsp1/iso1$densite1)
I_esp2 <- Calc_CompetitionIntensity(x$YEsp2, x$densite2, iso2$YEsp2/iso2$densite2)
df = data.frame(x$densite1, x$densite2, I_esp1, I_esp2)

# plot des intensite de competition

I_esp1[is.nan(I_esp1)] <- 0.
I_esp2[is.nan(I_esp2)] <- 0.

# taille cex proportionnelle a intensite
plot(x$densite1, x$densite2, xlab="densite1", ylab="densite 2", pch=16, cex=2*I_esp1, main ="Competition Intensity esp 1", xlim=c(0,400), ylim=c(0,400))
plot(x$densite1, x$densite2, xlab="densite1", ylab="densite 2", pch=16, cex=2*I_esp2, main ="Competition Intensity esp 2", xlim=c(0,400), ylim=c(0,400))



# test RCI
x <- all
RCI1 <- Calc_RCI_coeff(x$YEsp1, x$densite1, iso1$YEsp1/iso1$densite1)
RCI2 <- Calc_RCI_coeff(x$YEsp2, x$densite2, iso2$YEsp2/iso2$densite2)
df = data.frame(x$densite1, x$densite2, RCI1, RCI2)

RCI1[is.nan(RCI1)] <- 0.
RCI2[is.nan(RCI2)] <- 0.


plot(x$densite1, x$densite2, xlab="densite1", ylab="densite 2", pch=16, cex=3*RCI1, main ="RCI esp 1", xlim=c(0,400), ylim=c(0,400))
plot(x$densite1, x$densite2, xlab="densite1", ylab="densite 2", pch=16, cex=3*RCI2, main ="RCI esp 2", xlim=c(0,400), ylim=c(0,400))





#####################
#####################
# test indices loreau


#test indices Loreau pour la diag
# !!valable pour 1 densite donnee!
# somme fait aOY

x <- diag
tabx <- Calc_CESE_diag(x)
tabx$CE
tabx$SE


resCE_SE <- data.frame(Semprop1=x$Semprop1, OYa=tabx$OYa, CE=tabx$CE, SE=tabx$SE)
titre <- paste(as.character(x$mix[1]), "400 ",scenar)
Plot_resCE_SE(resCE_SE, ylim=c(-400,400), type='b', ylab="OYa", xlab="Prop1", main=titre)




#test pour un calcul



#to check!!


