top_cor <- function(cor.mat, drop = F, thresh = .95){
  library(reshape)
  
  cor_df <- cor.mat
  cor_df[cor_df == 1] <- NA #drop perfect
  cor_df[abs(cor_df) < 0.5] <- NA # drop less than abs(0.5)
  cor_df <- na.omit(melt(cor_df)) # melt! 
  cor_df <- cor_df[order(-abs(cor_df$value)),] # sort

  if (drop == T) {
    nms <- as.vector(NULL)
    A <- NULL
    B <- NULL

    A <- as.character(cor_df[1,1])
    B <- as.character(cor_df[1,2])

      while (abs(cor_df[1,3]) > thresh) {
      
        A <- as.character(cor_df[1,1])
        B <- as.character(cor_df[1,2])
        
        print(cor_df[1,3])
      
#        browser()
          
      if (mean(cor.mat[A,]) > mean(cor.mat[B,])) {
        
        cor.mat <- cor.mat[!rownames(cor.mat) %in% A,]
        cor.mat <- cor.mat[,!colnames(cor.mat) %in% A]
        cor_df <- cor.mat
        cor_df[cor_df == 1] <- NA #drop perfect
        cor_df[abs(cor_df) < 0.5] <- NA # drop less than abs(0.5)
        cor_df <- na.omit(melt(cor_df)) # melt! 
        cor_df <- cor_df[order(-abs(cor_df$value)),] # sort

      }else{
        
        cor.mat <- cor.mat[!rownames(cor.mat) %in% B,]
        cor.mat <- cor.mat[,!colnames(cor.mat) %in% B]

        cor_df <- cor.mat
        cor_df[cor_df == 1] <- NA #drop perfect
        cor_df[abs(cor_df) < 0.5] <- NA # drop less than abs(0.5)
        cor_df <- na.omit(melt(cor_df)) # melt! 
        cor_df <- cor_df[order(-abs(cor_df$value)),] # sort
        
      }
  

    }
    nms <- unique(c(levels(cor_df$X1), levels(cor_df$X2)))    
  return(nms)
  }else{
    return(cor_df)
  }
  
}




