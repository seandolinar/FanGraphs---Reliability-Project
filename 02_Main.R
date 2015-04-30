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
setwd('~/fg/projects/fg_SSS/GITHUB/FanGraphs---Reliability-data/data')

######FUNCTION LOAD######



######DATA LOAD######

data.ALL <- read.csv('2002-2015_MLBPlateAppearances.csv')


data_prep_jp <- function(df, K){
  
  year.list <- unique(df$year)
  len.year <- length(year.list)
  
  player.list <- as.numeric(unique(factor(df$PlayerId)))
  len.player <- length(player.list)
  
  out.df <- df[0,] #creates empty output df
  df.list <- list()
  
  for (i in year.list) {
    
    year.df <- df[which(df$year == i),]
    for (j in player.list){
      
      player.year.df <- year.df[which(year.df$PlayerId == j),]
      if (nrow(player.year.df) >= K){
        
        out.df <- rbind(out.df, player.year.df)
      }
    }

  } 
  out.player <- unique(out.df$PlayerId)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list)
  return(out.list)
  
}

data_parse <- function(df, K, year, player, type = 'jp'){
  
  year.df <- df[which(df$year == year),]
  player.year.df <- year.df[which(year.df$PlayerId == player),]
  return(player.year.df)
  
}


K_parse <- function(df,K){
  
  len.df = nrow(df)
  set.seed(1)
  down.sampled <- sample(1:len.df, size=K)
  
  return(df$K[down.sampled])
}

BB_parse <- function(df,K){
  
  len.df = nrow(df)
  set.seed(1)
  down.sampled <- sample(1:len.df, size=K)
  
  return(df$BB[down.sampled])
}


OBP_parse <- function(df,K){
  
  len.df = nrow(df)
  set.seed(1)
  down.sampled <- sample(1:len.df, size=K)
  
  return(df$OnBase[down.sampled])
}



matrix_parse <- function(df, K, FUN){
  
  stat.matrix <- NULL 
  
  for (i in df$year.list){
    
    for (j in df$player.list){
      
      player.year.df <- data_parse(df$df, 600, i, j)
      if (nrow(player.year.df) > 0){
        
        player.vector <- FUN(player.year.df, K)
        stat.matrix <- rbind(stat.matrix, player.vector)
        
      }
    }
  }
  
  return(stat.matrix)
}


FG_alpha <- function(df, K=ncol(df)){
  
  wide.vector <- c()
  tall.vector <- c()
  
  for (j in 1:ncol(df[,1:K])){
    
    tall.vector <- c(tall.vector, var(df[,j]))
    
  }
  for (i in 1:nrow(df[,1:K])){
    
    wide.vector <- c(wide.vector, sum(data.matrix(df[1:K])[i,]))
    
  }
  
  alpha <- K/(K-1)*(1-sum(tall.vector)/var(wide.vector))
  X_bar <- mean(wide.vector)
  sd <- sqrt(var(wide.vector))
  return(list(alpha = alpha, X_bar = X_bar, K = K, sd = sd))
  
}






b <- matrix_parse(a,600,BB_parse)
b <- matrix_parse(a,600,OBP_parse)
dim(b)
sum(is.na(b))

#creates df matrix
PA.list <- seq(from=600, to=650, by=50)

for (i in PA.list){
  print(data_prep_jp(data.ALL, 600))
  #player.year.matrix <- matrix_parse(,i,K_parse) #get rid of the matrix_parse i
  #print(player.year.matrix)

}




