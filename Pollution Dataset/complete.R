#A table of complete cases for each file in the dirctory

complete <- function (directory, id = 1:332) {
		setwd(directory);
    #declarations
		fid <- character()
		nobs <- integer()
		df <- data.frame(id  = 0, nobs = 0)
 
		for (i in id) {
			
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
			
			    ans <- sum(complete.cases(dfTmp))
			    df <- rbind(df, data.frame(id = i, nobs = ans))		


			} #end for
	
	  df <- subset(df,id != 0)
		df
}