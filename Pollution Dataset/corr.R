#Correlation between sulphates and phosphates

corr <- function (directory, threshold = 0) {
		t <- setwd(directory)
		index <- 0
		lst <- dir()
		
 		sol <- vector()
		for (fname in lst) {
			
      dfTmp <- read.csv(fname, header = T )
			ans <- sum(complete.cases(dfTmp))
			

			if ( ans <= threshold) next
			index <- index + 1 
			sol[index] <- cor(dfTmp$sulfate, dfTmp$nitrate, use = "complete.obs")
				
		}
    
		setwd(t)
		sol
}