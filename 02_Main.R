#FanGraphs Reliability Project
#v 0.0 -- sean setup
#v 0.1 -- first build
#v 0.2 -- adding pitchers and evaluation

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
data.BIP <- read.csv('FanGraphs---Reliability-data/data/2009-2014_BIPvelocity.csv')



#creates df matrix
PA.list <- seq(from=10, to=600, by=50)
FUN.list <- list(list(K_parse,'K_pct'),list(BB_parse,'BB_pct'),list(OBP_parse,'OBP'),
                 list(HR_parse,'HR'), list(HBP_parse,'HBP'), list(X1B_parse,'X1B'),
                 list(wOBA_parse,'wOBA'))

AB.list <- list(list(AVG_parse,'AVG'), list(SLG_parse,'SLG'), list(ISO_parse,'ISO'))

BIP.list <- list(list(AVG_parse,'BABIP'))
BIP.list <- list(list(AVG_parse,'BABIP'))


FUN.list <- list(list(K_parse,'K_pct'))
AB.list <- list(list(AVG_parse,'AVG'))

ptm <- proc.time()
out.df <- NULL
for (i in PA.list){
  
  #df.prep <- data_prep_jp(data.ALL, i)
  #ab.prep <- data_prep_jp_ab(data.ALL, i)
   bip.prep <- data_prep_jp_bip(data.ALL, i)

  
#   for (j in FUN.list) {
#     
#     player.year.matrix <- matrix_parse(df.prep,i,j[[1]]) #get rid of the matrix_parse i
#     alpha.list <- FG_alpha(player.year.matrix)
#     out.list <- data.frame(denom=i, type = 'PA', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                            sample_sd = alpha.list$sd)
#     out.df <- rbind(out.df, out.list)
#   }
# 
#   for (j in AB.list) {
#     
#     player.year.matrix <- matrix_parse(ab.prep,i,j[[1]]) #get rid of the matrix_parse i
#     alpha.list <- FG_alpha(player.year.matrix)
#     out.list <- data.frame(denom=i, type = 'AB', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                            sample_sd = alpha.list$sd)
#     out.df <- rbind(out.df, out.list)
#     
#   }
#  
  for (j in BIP.list) {
    
    player.year.matrix <- matrix_parse(bip.prep,i,AVG_parse) #get rid of the matrix_parse i
    alpha.list <- FG_alpha(player.year.matrix)
    out.list <- data.frame(denom=i, type = 'BIP', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
                           sample_sd = alpha.list$sd)
    out.df <- rbind(out.df, out.list)
    
  }
    
  
}

#df for output
c <- as.data.frame(out.df)
proc.time() - ptm

#writes to file
write.csv(c, file='fg_alpha_out_JP_50BIP.csv', row.names=F)


####PC Sampling####
#Random
PA.list <- seq(from=10, to=500, by=10)

Random = T
PC.denom.list <- list(PA=2000,AB=2000,BIP=2000,newBIP=500)

df.prep <- data_prep_pc(data.ALL, PC.denom.list$PA)
ab.prep <- data_prep_pc_ab(data.ALL, PC.denom.list$AB)
bip.prep <- data_prep_pc_bip(data.ALL, PC.denom.list$BIP)

new.bip.prep <- data_prep_pc_new_bip(data.BIP, PC.denom.list$newBIP)

BIP.list <- list(list(HH_parse,'HH_pct'), list(MH_parse,'MH_pct'), list(SH_parse,'SH_pct'))


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
#     for (j in BIP.list) {
#       
#       player.year.matrix <- matrix_parse(bip.prep,i,AVG_parse, Random) #get rid of the matrix_parse i
#       alpha.list <- FG_alpha(player.year.matrix)
#       out.list <- data.frame(denom=i, type = 'BIP', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                              sample_sd = alpha.list$sd)
#       out.df <- rbind(out.df, out.list)
#       
#     }
#   
    for (j in BIP.list) {
      
      player.year.matrix <- matrix_parse(new.bip.prep,i,j[[1]], Random) #get rid of the matrix_parse i
      alpha.list <- FG_alpha(player.year.matrix)
      out.list <- data.frame(denom=i, type = 'BIP', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
                             sample_sd = alpha.list$sd)
      out.df <- rbind(out.df, out.list)
      
    }
    
  
}
#df for output
c <- as.data.frame(out.df)
#colnames(c) <- c('denom', 'type', 'stat', 'alpha', 'sample_mean', 'sample_sd')
proc.time() - ptm


write.csv(c, file='fg_alpha_out_test_500BIP_HH_prelim.csv', row.names=F)

#####NEW
df.prep$key.vector
player.out <- df.prep$df
player.out <- player.out[player.out$key == '2014 8252',]

for (i in 1:nrow(player.out)){
  
  player.out$PA[i] <- i
  player.out$culm_K[i] <- sum(player.out$K[1:i])/length(player.out$K[1:i])  
  
}
write.csv(player.out, file='example_player.csv')
 