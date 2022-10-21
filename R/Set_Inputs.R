


#################################
## prepare input excel files


#' This function ...
#'
#' @export
Tab_unique_1v1 <- function(ls_sp, unic=T)
{
  
  
  #all combination: n x n 
  base <- NULL
  for (i in 1:length(ls_sp))
  {
    for (j in 1:length(ls_sp))
    {
      #pour ordonner tjrs selon premier cite
      if (j>i)
      {
        sp1 <- ls_sp[i]
        sp2 <- ls_sp[j]
        ordre <- 1
      }
      else
      {
        sp1 <- ls_sp[j]
        sp2 <- ls_sp[i]
        ordre <- 2
      }
      
      if (j==i)
      { ordre <- 3}
      
      
      res <- c(sp1, sp2, paste(sp1,sp2,sep="-"), ordre)
      base <- rbind (base, res)
    }
  }
  
  base <- as.data.frame(base)
  
  #si unique
  if (unic==T)
  { base <- base[base$V4 %in% c(1,3), ]}
  
  if (unic=="pur")
  { base <- base[base$V4 %in% c(3), ]}
  
  if (unic=="unic_sanspur")
  { base <- base[base$V4 %in% c(1), ]}
  
  if (unic=="sanspur")
  { base <- base[base$V4 %in% c(1,2), ]}
  
  #noms
  rownames(base) <- 1:dim(base)[1]
  names(base) <- c("Esp1", "Esp2", "trait","order")
  
  #return
  base[,c("Esp1", "Esp2", "trait")]

}
#ls_sp = c("A", "B", "C", "D", "E")
#Tab_unique_1v1(ls_sp, unic=T) #combinaison uniques sans repet
#Tab_unique_1v1(ls_sp, unic=F) #toutes les combinaisons 2a2
#Tab_unique_1v1(ls_sp, unic="pur") #slmt les purs
#Tab_unique_1v1(ls_sp, unic="unic_sanspur") #slmt les purs
#Tab_unique_1v1(ls_sp, unic="sanspur") #slmt les purs



#' This function ...
#'
#' @export
duplicate_seeds <- function(base, nb_seed)
{
  ls_res <- vector("list", length=nb_seed)
  names(ls_res) <- 1:nb_seed
  
  for (i in 1:nb_seed)
  {
    x <- base
    x$seed <- i
    ls_res[[i]] <- x
  }
  
  res <- do.call("rbind", ls_res)
  rownames(res) <- 1:dim(res)[1]
  
  res
  
}

#ls_sp <- c("A", "B", "C", "D", "E")
#base <- Tab_unique_1v1(ls_sp, unic=T)
#nb_seed <- 10
#tab <- duplicate_seeds(base, nb_seed)



#' This function ...
#'
#' @export
duplicate_lsusm <- function(baseUSM, var, ls_vals, offset=0)
{
  # to duplicate a list of usm and change a single column (var) - update ID_usm unique
  
  ls_res <- vector("list", length=length(ls_vals))
  names(ls_res) <- 1:length(ls_vals)
  
  for (i in 1:length(ls_vals))
  {
    x <- baseUSM
    x[,c(var)] <- ls_vals[i]
    ls_res[[i]] <- x
  }
  
  res <- as.data.frame(do.call("rbind", ls_res))
  row.names(res) <- 1:dim(res)[1]
  
  #update IDUSM unique
  res$ID_usm <- (1:dim(res)[1]) + offset
  
  res
}

# test <- duplicate_lsusm(newUSM, "opt_sd", c(0,1))



# ajout fonction duplicate sd 
# fonction gerer scenario?


