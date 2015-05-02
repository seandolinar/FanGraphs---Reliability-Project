#FanGraphs Reliability Project
#v 0.0 -- sean setup
#v 0.1
#v 0.2
#v 0.3

## functions_have_underscores()
## variables.have.periods
## i,j,k are index variables
## a,b,c,d,e,f are test variables.  If they make it in to actual code, these are errors.
##
## local variables in functions use simplest names as possible such as df or vector

#sets working directory
#setwd('~/fg/projects/fg_SSS/GITHUB/FanGraphs---Reliability-data/data') #Sean Mac
setwd('~/projects/fg/fg_SSS/') #Sean Linux

######FUNCTION LOAD######

source('FanGraphs---Reliability/01_FunctionDefinitions.R')

######DATA LOAD######

data.ALL <- read.csv('FanGraphs---Reliability-data/data/2002-2015_MLBPlateAppearances.csv')



#creates df matrix
PA.list <- seq(from=100, to=500, by=100)
FUN.list <- list(list(K_parse,'K_pct'),list(BB_parse,'BB_pct'),list(OBP_parse,'OBP'),
                 list(HR_parse,'HR'), list(HBP_parse,'HBP'), list(X1B_parse,'X1B'),
                 list(wOBA_parse,'wOBA'))

AB.list <- list(list(AVG_parse,'AVG'), list(SLG_parse,'SLG'), list(ISO_parse,'ISO'))

BIP.list <- list(list(AVG_parse,'BABIP'))

FUN.list <- list(list(K_parse,'K_pct'))
AB.list <- list(list(AVG_parse,'AVG'))
ptm <- proc.time()
out.df <- NULL
for (i in PA.list){
  
  #df.prep <- data_prep_jp(data.ALL, i)
  ab.prep <- data_prep_jp_ab(data.ALL, i)
  #bip.prep <- data_prep_jp_bip(data.ALL, i)
#   
#   for (j in FUN.list) {
#     
#     player.year.matrix <- matrix_parse(df.prep,i,j[[1]]) #get rid of the matrix_parse i
#     alpha.list <- FG_alpha(player.year.matrix)
#     out.list <- data.frame(denom=i, type = 'PA', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                            sample_sd = alpha.list$sd)
#     out.df <- rbind(out.df, out.list)
#   }

  for (j in AB.list) {
    
    player.year.matrix <- matrix_parse(ab.prep,i,j[[1]]) #get rid of the matrix_parse i
    alpha.list <- FG_alpha(player.year.matrix)
    out.list <- data.frame(denom=i, type = 'AB', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
                           sample_sd = alpha.list$sd)
    out.df <- rbind(out.df, out.list)
    
  }
 
#   for (j in BIP.list) {
#     
#     player.year.matrix <- matrix_parse(bip.prep,i,AVG_parse) #get rid of the matrix_parse i
#     alpha.list <- FG_alpha(player.year.matrix)
#     out.list <- data.frame(denom=i, type = 'BIP', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                            sample_sd = alpha.list$sd)
#     out.df <- rbind(out.df, out.list)
#     
#   }
    
  
}
#df for output
c <- as.data.frame(out.df)
#colnames(c) <- c('denom', 'type', 'stat', 'alpha', 'sample_mean', 'sample_sd')
proc.time() - ptm

c

#writes to file
write.csv(c, file='fg_alpha_out_test_08.csv', row.names=F)


####PC Sampling####
#Random

Random = T
PC.denom.list <- list(PA=2000,AB=2000,BIP=2000)
ptm <- proc.time()
out.df <- NULL
for (i in PA.list){
  
  #df.prep <- data_prep_pc(data.ALL, PC.denom.list$PA)
  ab.prep <- data_prep_pc_ab(data.ALL, PC.denom.list$AB)
  #bip.prep <- data_prep_pc_bip(data.ALL, PC.denom.list$BIP)
  
  for (j in FUN.list) {
    
    player.year.matrix <- matrix_parse(df.prep,i,j[[1]], Random) #get rid of the matrix_parse i
    alpha.list <- FG_alpha(player.year.matrix)
    out.list <- data.frame(denom=i, type = 'PA', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
                           sample_sd = alpha.list$sd)
    out.df <- rbind(out.df, out.list)
  }
  
#   for (j in AB.list) {
#     
#     player.year.matrix <- matrix_parse(ab.prep,i,j[[1]], Random) #get rid of the matrix_parse i
#     alpha.list <- FG_alpha(player.year.matrix)
#     out.list <- data.frame(denom=i, type = 'AB', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                            sample_sd = alpha.list$sd)
#     out.df <- rbind(out.df, out.list)
#     
#   }
  
  #   for (j in BIP.list) {
  #     
  #     player.year.matrix <- matrix_parse(bip.prep,i,AVG_parse, Random) #get rid of the matrix_parse i
  #     alpha.list <- FG_alpha(player.year.matrix)
  #     out.list <- data.frame(denom=i, type = 'BIP', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
  #                            sample_sd = alpha.list$sd)
  #     out.df <- rbind(out.df, out.list)
  #     
  #   }
  
  
}
#df for output
c <- as.data.frame(out.df)
#colnames(c) <- c('denom', 'type', 'stat', 'alpha', 'sample_mean', 'sample_sd')
proc.time() - ptm




#####NEW





