#FanGraphs Reliability Project -- Verification
#v 0.2 -- adding pitchers and evaluation


#sets working directory
#setwd('~/fg/projects/fg_SSS/GITHUB/FanGraphs---Reliability-data/data') #Sean Mac
setwd('~/projects/fg/fg_SSS/') #Sean Linux

#functions
alpha_join <- function(number){
  return(trunc(number/10-1,1) * 10 + 10)
}

k <- 1




PD_stats <- function(stat.df,alpha.df){
  
  if (nrow(stat.df) == 1){
    
    alpha.row <- alpha.df[which(alpha.df$stat %in% stat.df$stat.name & alpha.df$denom %in% stat.df$denom.value ), ]
    denom <- stat.df$denom
    alpha <- alpha.row$alpha
    mean <- alpha.row$sample_mean
    SD <- alpha.row$sample_sd
    stat.df$alpha <- alpha
    stat.df$regressed_stat <- mean/denom + alpha * (stat.df$stat.value - mean/denom)
    stat.df$sem <- SD/denom * sqrt(1-alpha) * sqrt(alpha)
    stat.df$lower <- stat.df$regressed_stat - stat.df$sem
    stat.df$upper <- stat.df$regressed_stat + stat.df$sem
    return(stat.df)
    
  }
  else{
    
    return('error')
  }
  
  
}






alpha.key <- read.csv('fg_alpha_out_test_ALLPA_PC1000_Random.csv')

df <- df.prep$df
player.vector <- df.prep$player.list[1]


for (i in player.vector[2]){
  
  player.df <- df[df$key == i,]
  for (i in 1:nrow(player.df)){
    
    player.df$PA[i] <- i
    player.df$culm_K[i] <- sum(player.df$K[1:i])/length(player.df$K[1:i])
    
    
  }
  
  
}
apply(1:20, 2, FUN=alpha_join)



out.df <- read.csv('fg_alpha_out_test_700PA_pitcher.csv')
stat.df <- data.frame(denom.value=alpha_join(player.df$PA), denom=player.df$PA,stat.name='K_pct', stat.value=player.df$culm_K)

alpha.df <- out.df
N <- 500

eval.df <- NULL
for (i in 1:N){
  
  eval.df <- rbind(eval.df, PD_stats(stat.df[i,], alpha.df))
  
}
eval.df$stat.2000 <- stat.df[which(stat.df$denom==2000),]$stat.value

sum(eval.df$stat.2000 > eval.df$lower & eval.df$stat.2000 < eval.df$upper)/N


