#FanGraphs Reliability Project -- Pitchers
#v 0.2 -- adding pitchers and evaluation

## functions_have_underscores()
## variables.have.periods
## i,j,k are index variables
## a,b,c,d,e,f are test variables.  If they make it in to actual code, these are errors.
##
## local variables in functions use simplest names as possible such as df or vector



setwd('~/projects/fg/fg_SSS/') #Sean Linux
out.dir <- '~/projects/fg/fg_SSS/out/'

######FUNCTION LOAD######

source('FanGraphs---Reliability/01_FunctionDefinitions.R')

######DATA LOAD######

data.ALL <- read.csv('FanGraphs---Reliability-data/data/2002-2015_MLBPlateAppearances.csv')
data.BIP <- read.csv('FanGraphs---Reliability-data/data/2009-2014_BIPvelocity.csv')

FUN.list <- list(list(K_parse,'K_pct'),list(BB_parse,'BB_pct'),list(OBP_parse,'OBP'),
                 list(HR_parse,'HR'), list(HBP_parse,'HBP'), list(X1B_parse,'X1B'),
                 list(wOBA_parse,'wOBA'))




####PC Sampling####

PA.list <- seq(from=10, to=700, by=10)

Random = T
PC.denom.list <- list(PA=2000,AB=700,BIP=2000,newBIP=1000)

p.df.prep <- data_prep_pc_p(data.ALL, PC.denom.list$PA) #update these
p.ab.prep <- data_prep_pc_ab(data.ALL, PC.denom.list$AB)


p.bip.prep <- data_prep_pc_bip_p(data.ALL, PC.denom.list$BIP)
new.bip.prep <- data_prep_pc_new_bip_p(data.BIP, PC.denom.list$newBIP)

BIP.list <- list(list(AVG_parse,'BABIP'))
new.BIP.list <- list(list(HH_parse,'HH_pct'), list(MH_parse,'MH_pct'), list(SH_parse,'SH_pct'))


ptm <- proc.time()
out.df <- NULL
for (i in PA.list){
  
#     for (j in FUN.list) {
#       
#       player.year.matrix <- matrix_parse(p.df.prep,i,j[[1]], Random) #get rid of the matrix_parse i
#       alpha.list <- FG_alpha(player.year.matrix)
#       out.list <- data.frame(denom=i, type = 'PA', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                              sample_sd = alpha.list$sd)
#       out.df <- rbind(out.df, out.list)
#     }
#   
    for (j in AB.list) {
      
      player.year.matrix <- matrix_parse(p.ab.prep,i,j[[1]], Random) #get rid of the matrix_parse i
      alpha.list <- FG_alpha(player.year.matrix)
      out.list <- data.frame(denom=i, type = 'AB', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
                             sample_sd = alpha.list$sd)
      out.df <- rbind(out.df, out.list)
      
    }
    
#       for (j in BIP.list) {
#         
#         player.year.matrix <- matrix_parse(p.bip.prep,i,AVG_parse, Random) #get rid of the matrix_parse i
#         alpha.list <- FG_alpha(player.year.matrix)
#         out.list <- data.frame(denom=i, type = 'BIP', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                                sample_sd = alpha.list$sd)
#         out.df <- rbind(out.df, out.list)
#         
#       }
#     
#   for (j in BIP.list) {
#     
#     player.year.matrix <- matrix_parse(new.bip.prep,i,j[[1]], Random) #get rid of the matrix_parse i
#     alpha.list <- FG_alpha(player.year.matrix)
#     out.list <- data.frame(denom=i, type = 'BIP', stat=j[[2]], alpha=alpha.list$alpha, sample_mean = alpha.list$X_bar, 
#                            sample_sd = alpha.list$sd)
#     out.df <- rbind(out.df, out.list)
#     
#   }
  
  
}
proc.time() - ptm

#df for output
write.csv(out.df, file=paste(out.dir,'fg_alpha_out_test_pitchertruncated.csv'), row.names=F)


