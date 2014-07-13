#Calculates mean of specified pollutant across the ids in the id vector

pollutantmean <- function (directory, pollutant, id = 1:332) {
		setwd(directory);
		pMean <- numeric(332)
		index <- 0
		total <- 0
		tobs <- 0
		df <- data.frame()
		for (i in id) {
			index <- index + 1
			if (i < 1) {
 				print("Invalid id.\n")
				return
			}

			else if ( i < 10) {
			 fname = paste("00",as.character(i), ".csv", sep = "")
				}
				else if ( i < 100) {
			fname = paste("0",as.character(i),".csv",sep = "")
				}
				else if (i < 333) {
				fname = paste(as.character(i), ".csv", sep = "")
				}

			dfTmp <- read.csv(fname, header = T )
			
			df <- rbind(df, dfTmp)		


			} #for loop ends
		
		mean(df[, pollutant],na.rm = T) #calculates mean discarding the NA (missing) values	

}