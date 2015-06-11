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
    alpha <- abs(alpha.row$alpha)
    mean <- alpha.row$sample_mean
    SD <- alpha.row$sample_sd
    stat.df$alpha <- alpha
    stat.df$regressed_stat <- mean/denom + alpha * (stat.df$stat.value - mean/denom)
    stat.df$sem <- SD/denom * sqrt(1-alpha) * sqrt(alpha)
    stat.df$lower <- stat.df$regressed_stat - stat.df$sem
    stat.df$upper <- stat.df$regressed_stat + stat.df$sem
    stat.df$se <- sqrt(stat.df$stat.value*(1-stat.df$stat.value)/denom)
    stat.df$se_lower <- stat.df$stat.value - stat.df$se
    stat.df$se_upper <- stat.df$stat.value + stat.df$se
    return(stat.df)
    
  }
  else{
    
    return('error')
  } 
}

eval_stat <- function(player.df, alpha.df,stat.info,SEM=T){
  
  stat.df <- data.frame(denom.value=alpha_join(player.df$denom), denom=player.df$denom,stat.name=stat.info[[1]], 
                        stat.value=player.df$stat.value, stat.2000=player.df$stat.2000)
  N <- nrow(stat.df)
  print(stat.df)
  eval.df <- NULL
  for (i in 1:N){
    
    eval.df <- rbind(eval.df, PD_stats(stat.df[i,], alpha.df))
    
  }
  print(eval.df)
  if (SEM){
    print('sem')
    return(sum(eval.df$stat.2000 > eval.df$lower & eval.df$stat.2000 < eval.df$upper)/N)
  }
  else{
    print('se')
    return(sum(eval.df$stat.2000 > eval.df$se_lower & eval.df$stat.2000 < eval.df$se_upper)/N)
  }
  
  
}


#####DATA LOAD######

alpha.key <- read.csv('fg_alpha_out_evaluation_PA_2000Random.csv')
alpha.key <- read.csv('fg_alpha_out_test_ALLPA.csv')
alpha.key <- read.csv('fg_alpha_out_evaluation_AB_2000Random.csv')
alpha.key <- read.csv('fg_alpha_out_evaluation_BIP_2000Random.csv')
alpha.key <- out.df

pa.prep <- df.prep 

df.prep <-bip.prep
df <- df.prep$df


N <- 500 #set interval of demoninator (PA, AB, BIP, etc)
MAX <- 2000 #set 'true' talent level



stat.list <- list(list('K_pct','K'))
stat.list <- list(list('BB_pct','BB'))
stat.list <- list(list('OBP','OnBase'))
stat.list <- list(list('AVG','hit'))
stat.list <- list(list('SLG','SLG'))
stat.list <- list(list('HR','HR'))
stat.list <- list(list('wOBA','wOBA'))
stat.list <- list(list('HBP','HBP'))
stat.list <- list(list('X1B','S'))
stat.list <- list(list('BABIP','hit'))
stat.list <- list(list('AVG','hit'))



player.vector <- df.prep$player.list[sample(1:length(df.prep$player.list),size=50)]
total <- NULL
for (stat.info in stat.list){
  
  full.stat.df <- NULL
  col <- stat.info[[2]]
  
  for (i in player.vector){
    print(i)
    player.df <- df[df$key == i,]
    #player.df <- player.df[sample(x=1:nrow(player.df),size=nrow(player.df),replace=F),]
    for (j in 1:nrow(player.df)){
      
      player.df$denom[j] <- j
      player.df$stat.value[j] <- sum(player.df[1:j,col])/length(player.df[1:j,col]) #should make this loopable
      
    }
    player.df$stat.2000 <- player.df[which(player.df$denom==MAX),]$stat.value
    full.stat.df <- rbind(full.stat.df, player.df[1:N,])
    
  }
  #print(eval_stat(full.stat.df, alpha.df,stat.info))
  total <- rbind(total, full.stat.df)  
}
set.seed(1)
print(eval_stat(total[sample(x=nrow(total), size=1000),], alpha.key,stat.info, SEM=F))




sample.data <- total[which(total$denom==1),]$stat.2000[sample(x=1:length(which(total$denom==1)),size=50)]



