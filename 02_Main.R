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
setwd('~/fg/projects/fg_SSS/GITHUB/FanGraphs - Reliability/data')

######FUNCTION LOAD######



######DATA LOAD######

data.ALL <- read.csv('2002-2015_MLBPlateAppearances.csv')


data_prep_jp <- function(df, K){
  
  year.list <- unique(df$year)
  len.year <- length(year.list)
  
  player.list <- as.numeric(unique(factor(df$PlayerId)))
  len.player <- length(player.list)
  
  out.df <- df[0,] #creates empty output df
  
  for (i in year.list) {
    
    year.df <- df[which(df$year == i),]
    for (j in player.list){
      
      player.year.df <- year.df[which(year.df$PlayerId == j),]
      if (nrow(player.year.df) >= K){
        
        out.df <- rbind(out.df, player.year.df)
      }
    }
    out.player <- as.numeric(unique(factor(out.df$PlayerId)))
    out.list <- list(df = out.df, player.list = out.player)
    return(out.list)
  } 
}

data_parse <- function(df, K){
  
  
  
  
}


