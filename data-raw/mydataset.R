
# data-raw/mydataset.R
# Data import and processing pipeline
# https://grasshoppermouse.github.io/posts/2017-10-18-put-your-data-in-an-r-package/


library(readxl)


# Data cleaning code here...
# (Do NOT put data analysis code here!)

## input files examples

#plant parameter file
path_plante <- system.file("extdata", "Parametres_plante_exemple.xls", package = "RPackVGL")
plt_par <- read_excel(path_plante, sheet="Fix2")

#usm file
path_usm <- system.file("extdata", "liste_usms_exemple.xls", package = "RPackVGL")
USMfile <- read_excel(path_usm, sheet="exemple")

#...


## output file examples
path_zip0 <- system.file("extdata", "toto_0.zip", package = "RPackVGL")
toto_0 <- read.table(unz(path_zip0, "toto_0.csv"), header=T, sep=";")
outHR_0 <- read.table(unz(path_zip0, "outHR_0.csv"), header=T, sep=";")
BilanN_0 <- read.table(unz(path_zip0, "BilanN_0.csv"), header=T, sep=";")
paramsd_0 <- read.table(unz(path_zip0, "paramsd_0.csv"), header=T, sep=";")

## toto file mayssa
toto <- read.table(system.file("extdata", "toto.csv", package = "RPackVGL"), header=T, sep=";")


## exemple files
path_totocompet <- system.file("extdata", "dtoto_ind_compet_exemple.csv", package = "RPackVGL")
tabtoto_compet <- read.table(path_totocompet, header=T, sep=",", dec=".")

#path_totosim <- system.file("extdata", "dtoto_sim_exemple.csv", package = "RPackVGL")
#tabtoto_sim <- read.table(path_totosim, header=T, sep=";", dec=".")
# en fait pas bon dico: pas bien calcule

path_obs <- system.file("extdata", "DigitLuz10_obs.xls", package = "RPackVGL")
obs_exemple <- read_excel(path_obs, sheet="DigitLuz10_LD_Orca")


path_ltoto1 <- system.file("extdata", "toto_6129_l-egume_Orca-Orca_homogeneous0_scenario12-12_DigitLuz10LD_0_DigitLuz10_-_.csv", package = "RPackVGL")
ltoto1 <- read.table(path_ltoto1, header=T, sep=";", dec=".")
path_ltoto2 <- system.file("extdata", "toto_6130_l-egume_Orca-Orca_homogeneous0_scenario12-12_DigitLuz10LD_1_DigitLuz10_-_.csv", package = "RPackVGL")
ltoto2 <- read.table(path_ltoto2, header=T, sep=";", dec=".")
path_ltoto3 <- system.file("extdata", "toto_6131_l-egume_Orca-Orca_homogeneous0_scenario12-12_DigitLuz10LD_2_DigitLuz10_-_.csv", package = "RPackVGL")
ltoto3 <- read.table(path_ltoto3, header=T, sep=";", dec=".")

ltoto_exemple <- list(ltoto1, ltoto2, ltoto3)
names(ltoto_exemple) <- c("toto_6129_l-egume_Orca-Orca_homogeneous0_scenario12-12_DigitLuz10LD_0_DigitLuz10_-_.csv", "toto_6130_l-egume_Orca-Orca_homogeneous0_scenario12-12_DigitLuz10LD_0_DigitLuz10_-_.csv", "toto_6131_l-egume_Orca-Orca_homogeneous0_scenario12-12_DigitLuz10LD_0_DigitLuz10_-_.csv")
#ls_files <- c(path_ltoto1, path_ltoto2, path_ltoto3)


#exemple_sd 64 plantes (N+ 6 params) / simul v ancienne: pas rec/non-rec

path_zipSD <- system.file("extdata", "10100_SD4-4.zip", package = "RPackVGL")
toto_sd <- read.table(unz(path_zipSD, "toto_10100_l-egume_Fix0-Fix1_damidouble4_scenario40-1_Lusignan30IrrNN_0_Lusignan30_SD4-4_.csv"), header=T, sep=";")
outHR_sd <- read.table(unz(path_zipSD, "outHR_10100_l-egume_Fix0-Fix1_damidouble4_scenario40-1_Lusignan30IrrNN_0_Lusignan30_SD4-4_.csv"), header=T, sep=";")
BilanN_sd <- read.table(unz(path_zipSD, "BilanN_10100_l-egume_Fix0-Fix1_damidouble4_scenario40-1_Lusignan30IrrNN_0_Lusignan30_SD4-4_.csv"), header=T, sep=";")
paramsd_sd <- read.table(unz(path_zipSD, "paramSD_10100_l-egume_Fix0-Fix1_damidouble4_scenario40-1_Lusignan30IrrNN_0_Lusignan30__SD4-4_.csv"), header=T, sep=";")

# ajout x, y a paramsd_sd (present dans derniere version du modele mais pas ces fichiers)
xx <- NULL
yy <- NULL
for(i in 1:8)
{
  xx <- c(xx, rep(i,8))
  yy <- c(yy, 8:1)
}
paramsd_sd$x <- xx
paramsd_sd$y <- yy



#data test CE/SE
path_rmixan <- system.file("extdata", "rmixan.csv", package = "RPackVGL")
rmixan <- read.table(path_rmixan, header=T, sep=";", dec=".")


# This should be the last line.
# Note that names are unquoted.
# I like using overwrite = T so everytime I run the script the
# updated objects are saved, but the default is overwrite = F
usethis::use_data(plt_par, toto, toto_0, outHR_0, BilanN_0, paramsd_0, tabtoto_compet, obs_exemple, ltoto_exemple, toto_sd, outHR_sd, BilanN_sd, paramsd_sd, rmixan,USMfile,  overwrite = T)


