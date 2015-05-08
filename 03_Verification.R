#FanGraphs Reliability Project -- Verification
#v 0.2 -- adding pitchers and evaluation
#v 0.2.1 -- evaluation is working but needs cleaned.


#sets working directory
#setwd('~/fg/projects/fg_SSS/GITHUB/FanGraphs---Reliability-data/data') #Sean Mac
setwd('~/projects/fg/fg_SSS/') #Sean Linux

#functions
alpha_join <- function(number){
  return(trunc(number/10-1,1) * 10 + 10)
}






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

total <- c()
player.vector <- df.prep$player.list[sample(1:length(df.prep$player.list),size=300)]

player.stat.df <- NULL
for (i in player.vector){
  print(i)
  player.df <- df[df$key == i,]
  for (j in 1:nrow(player.df)){
    
    player.df$denom[j] <- j
    player.df$stat.value[j] <- sum(player.df$K[1:j])/length(player.df$K[1:j]) #should make this loopable
    
  }
  player.df$stat.2000 <- player.df[which(player.df$denom==2000),]$stat.value
  player.stat.df <- rbind(player.stat.df, player.df[1:N,])
  
}
print(eval_stat(player.stat.df, alpha.df))
total <- c(total,eval_stat(player.stat.df, alpha.df))
player.df$stat.2000
mean(total)
hist(total)

out.df <- read.csv('fg_alpha_out_test_700PA_pitcher.csv')
N <- 500
alpha.df <- out.df

eval_stat <- function(player.df, alpha.df){

    stat.df <- data.frame(denom.value=alpha_join(player.df$denom), denom=player.df$denom,stat.name='K_pct', 
                          stat.value=player.df$stat.value, stat.2000=player.df$stat.2000)
    N <- nrow(stat.df)
    print(stat.df)
    eval.df <- NULL
    for (i in 1:N){
      
      eval.df <- rbind(eval.df, PD_stats(stat.df[i,], alpha.df))
      
    }
    print(eval.df)
    return(sum(eval.df$stat.2000 > eval.df$lower & eval.df$stat.2000 < eval.df$upper)/N)

}



