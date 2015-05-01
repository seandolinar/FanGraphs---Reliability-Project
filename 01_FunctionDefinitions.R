data_prep_jp <- function(df, K){
  
  
  agg.df <- aggregate(GameDate ~ year + PlayerId, data = df, FUN = length)
  agg.df <- agg.df[which(agg.df$GameDate >= K),]
  key.vector <- paste(agg.df$year,agg.df$PlayerId)
  
  out.df <- df[which(paste(df$year,df$PlayerId) %in% key.vector),]
  out.player <- unique(out.df$PlayerId)
  year.list <- unique(out.df$year)
  out.list <- list(df = out.df, player.list = out.player, year.list = year.list, key.vector = key.vector)
  return(out.list)
  
  
}

matrix_parse <- function(df.obj, K, FUN){
  
  stat.matrix <- NULL 
  df <- df.obj$df
  key <- paste(df$year,df$PlayerId)
  for (i in df.obj$key.vector){
    
    #player.year.df <-  subset(df, paste(df$year,df$PlayerId) == i)
    player.year.df <- df[which(key == i),]
    player.vector <- FUN(player.year.df, K)
    stat.matrix <- rbind(stat.matrix, player.vector)
    
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
    
    wide.vector <- c(wide.vector, sum(data.matrix(df[,1:K])[i,]))
    
  }
  
  alpha <- K/(K-1)*(1-sum(tall.vector)/var(wide.vector))
  X_bar <- mean(wide.vector)
  sd <- sqrt(var(wide.vector))
  return(list(alpha = alpha, X_bar = X_bar, K = K, sd = sd))
  
}


####STAT PARSE####

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

HR_parse <- function(df,K){
  
  len.df = nrow(df)
  set.seed(1)
  down.sampled <- sample(1:len.df, size=K)
  
  return(df$HR[down.sampled])
}
 
