




#fonction graph ggplot2




#' Plotting simulation outputs
#'
#' @param varsim name of the output variable
#' @param simmoy data frame containing the average simulated outputs of all the output variables
#' @param simsd data frame containing the standard deviation of simulated outputs of all the output variables
#' @param name Name...
#' @param col Color...
#' @param colref Color...
#'
#' @return A graph showing the dynamics of the selected output variable
#'
#' @examples
#' ltoto <- ltoto_exemple
#' simmoy <- build_simmoy(ltoto, lsusm=names(ltoto))
#' simsd <- build_simmoy(ltoto, lsusm=names(ltoto), optSD = T)
#' gg_plotsim("LAI", simmoy, simsd, name="test")
gg_plotsim <- function(varsim, simmoy, simsd, name="", col="blue", colref="red")
{
  #fait le line plot avec ecart type a partir des tableau moyen simules
  # for dtoto daily output
  var_ <- varsim #"FTSW"#"NBI"#"MSA"#"NNI"#"LAI"#

  min <- 0
  max <- 1.5*max(simmoy[,var_])

  plot_var <- ggplot(data = simmoy, aes(x = STEPS)) +
    geom_line(aes(y = simmoy[,var_]), color=col)+
    geom_ribbon(aes(ymin=simmoy[,var_]-simsd[,var_],ymax=simmoy[,var_]+simsd[,var_]),fill=col,alpha=0.2)+
    geom_hline(yintercept=0)+
    ylim(min,max)+
    geom_text(x=1.20*min(simmoy$STEPS), y=0.98*max, size=4, label=name)+
    theme(axis.text.x = element_text(size=6),axis.text.y = element_text(size=6))+
    labs(title = "obs",subtitle = "sim",x = "DOY", y = var_)+
    theme(plot.title=element_text(size=10,color = colref),plot.subtitle = element_text(size=10,color = col))

  plot_var
}
#gg_plotsim("LAI", simmoy, simsd, "test")
#titre sera a revoir...




gg_addplotobs <- function(plot_var, var_, obsOK, corresp, colobs="red")
{
  # ajour a un graph simule des points observe pour variable var_
  #obsOK : obs avec tableau meme dimension que les simul (merge)
  #corresp: dataframe de correspondance des nom de variables obs/sim
  nomvarobs <- as.character(corresp[corresp$sim==var_,c("obs")])
  plot_var2 <- plot_var + {if(var_ %in% corresp$sim) geom_point(aes(obsMerge$DOY, obsMerge[,nomvarobs]), fill=colobs,color=colobs , size=2)}

  plot_var2
}
#gg_addplotobs(ls_plt[["MSA"]], "MSA", obsMerge, corresp)




gg_plotObsSim <- function(obssim, var_, name="", colpt="red")
{
  #plot obs-sim avec ggplot

  #var_ <- "NBI"#"MSA"#
  #nomvarobs <- as.character(corresp[corresp$sim==var_,c("obs")])
  #obssim <- na.omit(data.frame(obs=simmoy[,var_], sim=obsMerge[,nomvarobs]))
  #name <- onglet


  min <- 0
  max <- 1.5*max(obssim$sim)
  reg   <- lm(obs ~ sim, data = obssim)
  coeff <- coefficients(reg)
  eq    <- paste0("y = ", round(coeff[2],2), "x + ", round(coeff[1],2))
  RMSE_ <- round(rmse(obssim$obs,obssim$sim), 2)
  rmses_ <- rmsesCoucheney(obssim$obs,obssim$sim)
  rmse_ <- rmseuCoucheney(obssim$obs,obssim$sim)
  pRMSEs_ <- round(pRMSEs(rmse_, rmses_), 2)
  rRMSE <- round(rrmseCoucheney(obssim$obs,obssim$sim), 2)
  EF <- round(efficiencyCoucheney(obssim$obs,obssim$sim), 2)

  plot_ObsSim <- ggplot(obssim, aes(x = obs, y = sim)) +
    ggtitle(name)+
    geom_abline(intercept = 0, slope = 1, color = "black")+
    geom_point(aes(color = "obs"))+
    geom_smooth(method=lm, se = FALSE, color = colpt)+
    ylim(min,max)+
    xlim(min,max)+
    geom_text(x=0.2*max, y=0.95*max, size=3, label=eq)+
    geom_text(x=0.2*max, y=0.9*max, size=3,label=paste("RMSE: ",RMSE_))+
    geom_text(x=0.2*max, y=0.85*max, size=3,label=paste("rRMSE: ",rRMSE))+
    geom_text(x=0.2*max, y=0.8*max, size=3,label=paste("pRMSEs: ",pRMSEs_))+
    geom_text(x=0.2*max, y=0.75*max, size=3,label=paste("EF: ",EF))+
    labs(x = paste("Obs ", var_), y = paste("Sim ", var_))


  plot_ObsSim
}
#plot_ObsSim <- gg_plotObsSim(obssim, "NBI", name=onglet)




Concat_ggplot_layers <- function(ls_pltsim, layertype = "GeomLine")
{
  # function avec une liste de ggplot similaires et ajoute dans le premier
  #les layers de type layertype des autres graphs
  # utilise package gginnards
  #teste ds eval finales ISOP MF

  #layertype = "GeomLine"

  p <- ls_pltsim[[1]] #part du premier plot de la liste
  for (i in 2:length(ls_pltsim))
  {
    nom <- names(ls_pltsim)[i]
    lay_ <- extract_layers(ls_pltsim[[nom]], layertype)
    p <- append_layers(p, lay_, position = "bottom")
    #print(p)
  }

  p
}
#p <- Concat_ggplot_layers(ls_pltsim_ID, layertype = "GeomLine")

