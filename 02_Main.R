#FanGraphs Reliability Project
#v 0.0 -- sean setup
#v 0.1 -- first build
#v 0.2 -- adding pitchers and evaluation
#v 0.3 -- cleaning up code

## functions_have_underscores()
## variables.have.periods
## i,j,k are index variables
## a,b,c,d,e,f are test variables.  If they make it in to actual code, these are errors.
##
## local variables in functions use simplest names as possible such as df or vector

#sets working directory
#setwd('~/fg/projects/fg_SSS/GITHUB/FanGraphs---Reliability-data/data') #Sean Mac
setwd('~/projects/fg/fg_SSS/') #Sean Linux

out.dir <- '~/projects/fg/fg_SSS/out/'

######FUNCTION LOAD######

source('FanGraphs---Reliability/01_FunctionDefinitions.R')

######DATA LOAD######

data.ALL <- read.csv('FanGraphs---Reliability-data/data/2002-2015_MLBPlateAppearances.csv')
data.BIP <- read.csv('FanGraphs---Reliability-data/data/2009-2014_BIPvelocity.csv')


#JP SAMPLING
#creates df matrix
#PA.list sets the intervals for measuring alpha
PA.list <- seq(from=10, to=500, by=10)

#creates list to get functions for specific stats
#FUN.list - PA-based
FUN.list <- list(list(K_parse,'K_pct'),list(BB_parse,'BB_pct'),list(OBP_parse,'OBP'),
                 list(HR_parse,'HR'), list(HBP_parse,'HBP'), list(X1B_parse,'X1B'),
                 list(wOBA_parse,'wOBA'))

#AB-based
AB.list <- list(list(AVG_parse,'AVG'), list(SLG_parse,'SLG'), list(ISO_parse,'ISO'))

#BIP-based
BIP.list <- list(list(AVG_parse,'BABIP'))
new.BIP.list <- list(list(HH_parse,'HH_pct'), list(MH_parse,'MH_pct'), list(SH_parse,'SH_pct'))

random = T

ptm <- proc.time() #times
out.df <- NULL #calibrates out data frame
for (i in PA.list){
  
  #df.prep <- data_prep_jp(data.ALL, i)
  #ab.prep <- data_prep_jp_ab(data.ALL, i)
  #bip.prep <- data_prep_jp_bip(data.ALL, i)

  
#   for (j in FUN.list) {
#     
#     player.year.matrix <- matrix_parse(df.prep,i,j[[1]],random) #get rid of the matrix_parse i
#     alpha.list <- FG_alpha(player.year.matrix)
#     out.list <- data.frame(denom=i, type = 'PA', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                            sample_sd = alpha.list$sd)
#     out.df <- rbind(out.df, out.list)
#   }

#   for (j in AB.list) {
#     
#     player.year.matrix <- matrix_parse(ab.prep,i,j[[1]],random) #get rid of the matrix_parse i
#     alpha.list <- FG_alpha(player.year.matrix)
#     out.list <- data.frame(denom=i, type = 'AB', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                            sample_sd = alpha.list$sd)
#     out.df <- rbind(out.df, out.list)
#     
#   }
#  
#   for (j in BIP.list) {
#     
#     player.year.matrix <- matrix_parse(bip.prep,i,AVG_parse,random) #get rid of the matrix_parse i
#     alpha.list <- FG_alpha(player.year.matrix)
#     out.list <- data.frame(denom=i, type = 'BIP', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                            sample_sd = alpha.list$sd)
#     out.df <- rbind(out.df, out.list)
#     
#   }
#     
  
}
proc.time() - ptm #ends and prints timing

#out.df for output
#writes to file
write.csv(out.df, file=paste(out.dir, 'fg_alpha_out_JPRand_Boot_BIP.csv'), row.names=F)


###################
####PC Sampling####
###################

#Alternate sampling method
#PA.list Sets interval for PA/AB/or BIP
#PC.denom.list sets the requirement for the prep function
PA.list <- seq(from=10, to=2000, by=10)
PC.denom.list <- list(PA=2000,AB=2000,BIP=2000,newBIP=500)

#Sets if the data matrix out of the prep function to be random or chronological...T = random, F = chronological
Random = F

df.prep <- data_prep_pc(data.ALL, PC.denom.list$PA)
ab.prep <- data_prep_pc_ab(data.ALL, PC.denom.list$AB)
bip.prep <- data_prep_pc_bip(data.ALL, PC.denom.list$BIP)
new.bip.prep <- data_prep_pc_new_bip(data.BIP, PC.denom.list$newBIP) #used for HH, SH & MH ball speed

new.BIP.list <- list(list(HH_parse,'HH_pct'), list(MH_parse,'MH_pct'), list(SH_parse,'SH_pct'))


ptm <- proc.time()
out.df <- NULL
for (i in PA.list){
  
#   for (j in FUN.list) {
#     
#     player.year.matrix <- matrix_parse(df.prep,i,j[[1]], Random) #get rid of the matrix_parse i
#     alpha.list <- FG_alpha(player.year.matrix)
#     out.list <- data.frame(denom=i, type = 'PA', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                            sample_sd = alpha.list$sd)
#     out.df <- rbind(out.df, out.list)
#   }
#   
#   for (j in AB.list) {
#     
#     player.year.matrix <- matrix_parse(ab.prep,i,j[[1]], Random) #get rid of the matrix_parse i
#     alpha.list <- FG_alpha(player.year.matrix)
#     out.list <- data.frame(denom=i, type = 'AB', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                            sample_sd = alpha.list$sd)
#     out.df <- rbind(out.df, out.list)
#     
#   }
#   
    for (j in BIP.list) {
      
      player.year.matrix <- matrix_parse(bip.prep,i,AVG_parse, Random) #get rid of the matrix_parse i
      alpha.list <- FG_alpha(player.year.matrix)
      out.list <- data.frame(denom=i, type = 'BIP', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
                             sample_sd = alpha.list$sd)
      out.df <- rbind(out.df, out.list)
      
    }
#  
#     for (j in BIP.list) {
#       
#       player.year.matrix <- matrix_parse(new.bip.prep,i,j[[1]], Random) #get rid of the matrix_parse i
#       alpha.list <- FG_alpha(player.year.matrix)
#       out.list <- data.frame(denom=i, type = 'BIP', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                              sample_sd = alpha.list$sd)
#       out.df <- rbind(out.df, out.list)
#       
#     }
#     
  
}

proc.time() - ptm

#df for output
write.csv(out.df, file=paste(out.dir, 'fg_alpha_out_JP_noyear_AB_full.csv'), row.names=F)



#Bootstrap
FUN.list <- list(list(OBP_parse,'OBP'))
FUN.list <- list(list(K_parse,'K_pct'))
PA.list <- seq(from=10, to=600, by=10)
ptm <- proc.time()
out.df <- NULL
for (i in PA.list){
  
  
    for (j in FUN.list) {
      
      sample.all <- NULL
      for (k in 1:30){

        player.year.matrix <- matrix_parse_boot(df.prep,i,j[[1]], Random) #get rid of the matrix_parse i
        alpha.list <- FG_alpha(player.year.matrix)
        
        sample.out <- data.frame(alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
                               sample_sd = alpha.list$sd)
        sample.all <- rbind(sample.all, sample.out)

      }
        
      out.list <- data.frame(denom=i, type = 'PA', stat=j[[2]], alpha=mean(sample.all$alpha), sample_mean = mean(sample.all$sample_mean), 
                             sample_sd = mean(sample.all$sample_sd))
      out.df <- rbind(out.df, out.list)
    }
}
    

#### JP Sampling Bootstrap ####
PA.list <- seq(from=10, to=500, by=100)
random = T

ptm <- proc.time() #times
out.df <- NULL #calibrates out data frame
for (i in PA.list){
  
  #df.prep <- data_prep_jp(data.ALL, i)
  #ab.prep <- data_prep_jp_ab(data.ALL, i)
  bip.prep <- data_prep_jp_bip(data.ALL, i)
  
  
#   for (j in FUN.list) {
#     
#     sample.all <- NULL
#     for (k in 1:30){
#       
#       player.year.matrix <- matrix_parse_boot(df.prep,i,j[[1]], Random) #get rid of the matrix_parse i
#       alpha.list <- FG_alpha(player.year.matrix)
#       
#       sample.out <- data.frame(alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                                sample_sd = alpha.list$sd)
#       sample.all <- rbind(sample.all, sample.out)
#       
#     }
#     
#     out.list <- data.frame(denom=i, type = 'PA', stat=j[[2]], alpha=mean(sample.all$alpha), sample_mean = mean(sample.all$sample_mean), 
#                            sample_sd = mean(sample.all$sample_sd))
#     out.df <- rbind(out.df, out.list)
#   }
#   
#   for (j in AB.list) {
#     
#     sample.all <- NULL
#     for (k in 1:30){
#       
#       player.year.matrix <- matrix_parse_boot(ab.prep,i,j[[1]], Random) #get rid of the matrix_parse i
#       alpha.list <- FG_alpha(player.year.matrix)
#       
#       sample.out <- data.frame(alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                                sample_sd = alpha.list$sd)
#       sample.all <- rbind(sample.all, sample.out)
#       
#     }
#     
#     out.list <- data.frame(denom=i, type = 'AB', stat=j[[2]], alpha=mean(sample.all$alpha), sample_mean = mean(sample.all$sample_mean), 
#                            sample_sd = mean(sample.all$sample_sd))
#     out.df <- rbind(out.df, out.list)
#   }
   
    for (j in BIP.list) {
      
      sample.all <- NULL
      for (k in 1:30){
    
        player.year.matrix <- matrix_parse_boot(bip.prep,i,j[[1]], Random) #get rid of the matrix_parse i
        alpha.list <- FG_alpha(player.year.matrix)
        
        sample.out <- data.frame(alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
                                 sample_sd = alpha.list$sd)
        sample.all <- rbind(sample.all, sample.out)
    
      }
    
      out.list <- data.frame(denom=i, type = 'BIP', stat=j[[2]], alpha=mean(sample.all$alpha), sample_mean = mean(sample.all$sample_mean), 
                             sample_sd = mean(sample.all$sample_sd))
      out.df <- rbind(out.df, out.list)
    }
}
proc.time() - ptm #ends and prints timing







plot(out.df$alpha)


random = T

ptm <- proc.time() #times
out.df <- NULL #calibrates out data frame
for (i in PA.list){
  
  #df.prep <- data_prep_jp(data.ALL, i)
  #ab.prep <- data_prep_jp_ab(data.ALL, i)
  ab.prep <- data_prep_jp_ab_noyear(data.ALL, i)
  #bip.prep <- data_prep_jp_bip(data.ALL, i)
  
  
  #   for (j in FUN.list) {
  #     
  #     player.year.matrix <- matrix_parse(df.prep,i,j[[1]],random) #get rid of the matrix_parse i
  #     alpha.list <- FG_alpha(player.year.matrix)
  #     out.list <- data.frame(denom=i, type = 'PA', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
  #                            sample_sd = alpha.list$sd)
  #     out.df <- rbind(out.df, out.list)
  #   }
  
    for (j in AB.list) {
      
      player.year.matrix <- matrix_parse(ab.prep,i,j[[1]],random) #get rid of the matrix_parse i
      alpha.list <- FG_alpha(player.year.matrix)
      out.list <- data.frame(denom=i, type = 'AB', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
                             sample_sd = alpha.list$sd)
      out.df <- rbind(out.df, out.list)
      
    }
  #  
  #   for (j in BIP.list) {
  #     
  #     player.year.matrix <- matrix_parse(bip.prep,i,AVG_parse,random) #get rid of the matrix_parse i
  #     alpha.list <- FG_alpha(player.year.matrix)
  #     out.list <- data.frame(denom=i, type = 'BIP', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
  #                            sample_sd = alpha.list$sd)
  #     out.df <- rbind(out.df, out.list)
  #     
  #   }
  #     
  
}
proc.time() - ptm


#JP Year-by-Year
ptm <- proc.time() #times
out.df <- NULL #calibrates out data frame
for (i in PA.list){
  
  #df.prep <- data_prep_jp(data.ALL, i)
  df.prep <- data_prep_jp_year(data.ALL, i, 2014)
  #ab.prep <- data_prep_jp_ab(data.ALL, i)
  #ab.prep <- data_prep_jp_ab_noyear(data.ALL, i)
  #bip.prep <- data_prep_jp_bip(data.ALL, i)
  
  
    for (j in FUN.list) {
      
      player.year.matrix <- matrix_parse(df.prep,i,j[[1]],random) #get rid of the matrix_parse i
      alpha.list <- FG_alpha(player.year.matrix)
      out.list <- data.frame(denom=i, type = 'PA', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
                             sample_sd = alpha.list$sd)
      out.df <- rbind(out.df, out.list)
    }
  
#   for (j in AB.list) {
#     
#     player.year.matrix <- matrix_parse(ab.prep,i,j[[1]],random) #get rid of the matrix_parse i
#     alpha.list <- FG_alpha(player.year.matrix)
#     out.list <- data.frame(denom=i, type = 'AB', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                            sample_sd = alpha.list$sd)
#     out.df <- rbind(out.df, out.list)
#     
#   }
  #  
  #   for (j in BIP.list) {
  #     
  #     player.year.matrix <- matrix_parse(bip.prep,i,AVG_parse,random) #get rid of the matrix_parse i
  #     alpha.list <- FG_alpha(player.year.matrix)
  #     out.list <- data.frame(denom=i, type = 'BIP', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
  #                            sample_sd = alpha.list$sd)
  #     out.df <- rbind(out.df, out.list)
  #     
  #   }
  #     
  
}
proc.time() - ptm


write.csv(out.df, file=paste(out.dir, 'fg_alpha_out_JP_2014year_PA_full.csv'), row.names=F)


