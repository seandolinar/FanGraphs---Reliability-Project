#FanGraphs Reliability Project -- Pitchers
#v 0.2 -- adding pitchers and evaluation

## functions_have_underscores()
## variables.have.periods
## i,j,k are index variables
## a,b,c,d,e,f are test variables.  If they make it in to actual code, these are errors.
##
## local variables in functions use simplest names as possible such as df or vector



setwd('~/projects/fg/fg_SSS/') #Sean Linux

######FUNCTION LOAD######

source('FanGraphs---Reliability/01_FunctionDefinitions.R')

######DATA LOAD######

data.BIP <- read.csv('FanGraphs---Reliability-data/data/2009-2014_BIPvelocity.csv')

FUN.list <- list(list(K_parse,'K_pct'),list(BB_parse,'BB_pct'),list(OBP_parse,'OBP'),
                 list(HR_parse,'HR'), list(HBP_parse,'HBP'), list(X1B_parse,'X1B'),
                 list(wOBA_parse,'wOBA'))




####PC Sampling####

PA.list <- seq(from=10, to=1000, by=10)

Random = T
PC.denom.list <- list(PA=2000,AB=2000,BIP=2000,newBIP=1000)

df.prep <- data_prep_pc_p(data.ALL, PC.denom.list$PA) #update these
# ab.prep <- data_prep_pc_ab(data.ALL, PC.denom.list$AB)
# bip.prep <- data_prep_pc_bip(data.ALL, PC.denom.list$BIP)

new.bip.prep <- data_prep_pc_new_bip_p(data.BIP, PC.denom.list$newBIP)

BIP.list <- list(list(HH_parse,'HH_pct'), list(MH_parse,'MH_pct'), list(SH_parse,'SH_pct'))


ptm <- proc.time()
out.df <- NULL
for (i in PA.list){
  
#     for (j in FUN.list) {
#       
#       player.year.matrix <- matrix_parse(df.prep,i,j[[1]], Random) #get rid of the matrix_parse i
#       alpha.list <- FG_alpha(player.year.matrix)
#       out.list <- data.frame(denom=i, type = 'PA', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                              sample_sd = alpha.list$sd)
#       out.df <- rbind(out.df, out.list)
#     }
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


write.csv(c, file='fg_alpha_out_test_pitchertruncated.csv', row.names=F)


