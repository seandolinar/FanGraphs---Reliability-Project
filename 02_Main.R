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
PA.list <- seq(from=300, to=600, by=100)
FUN.list <- list(list(K_parse,'K_pct'),list(BB_parse,'BB_pct'),list(OBP_parse,'OBP'),list(HR_parse,'HR'))


ptm <- proc.time()
out.df <- NULL
for (i in PA.list){
  
  df.prep <- data_prep_jp(data.ALL, i)
  
  
  for (j in FUN.list) {
    
    player.year.matrix <- matrix_parse(df.prep,i,j[[1]]) #get rid of the matrix_parse i
    alpha.list <- FG_alpha(player.year.matrix)
    out.list <- list(i,j[[2]], alpha.list$alpha, alpha.list$X_bar, alpha.list$sd)
    out.df <- rbind(out.df, out.list)
  }

  

}

c <- data.frame(out.df)
colnames(c) <- c('PA', 'stat', 'alpha', 'sample_mean', 'sample_sd')
proc.time() - ptm


