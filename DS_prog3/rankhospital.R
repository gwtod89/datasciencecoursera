## Programming assignment #3 Hospital Quality 
## Part 2. 

## Function "Best" --> takes state name and outcomes the name of a Hospital
## With the lowest 30-day mortality for type of deaths. If there is a tie, 
## alphabetical rank decides. 

## -------------------------------------------- > 

rankhospital <- function(nameOfState = char() , typeOfIll = char(), ranking) {

	## Read in data 
	outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	outcome <- outcome[,c(2,7,11,17,23),]
	names(outcome)[3:5] <- c("heart attack","heart failure","pneumonia"); 


	## Check outcomes are valid 
	if (!is.element(nameOfState,unique(outcome[,2]))){
		## Error function 
		stop('invalid state')
	}

	if (!is.element(typeOfIll,colnames(outcome))){
		## Error function 
		stop('invalid outcome')
	}

	## Refind Data 
	postSelection <- outcome[outcome[,2] == nameOfState,]; 
	postSelection[, c(3:5)] <- sapply(postSelection[, c(3:5)], as.numeric); 


	## Find Best hospital 
	tempVar <- colnames(postSelection)

	for (n in 3:5){
		if(typeOfIll == tempVar[n]){
		postSelection <-  postSelection[ order(postSelection[,n], postSelection[,1]),]
		postSelection <-  postSelection[complete.cases(postSelection[,n]),]
		
		## if ranking is char, turn to numeric 
		if (ranking == "worst"){
			ranking <- nrow(postSelection)
		}
		if (ranking == "best"){
			ranking <- 1; 
		}

		## Out of range 
		if (ranking > nrow(postSelection)){
			return(postSelection[nrow(postSelection)+1,1])
		}


		## follow normally. 
		tieIndex <- is.element(postSelection[,n],postSelection[ranking,n])
		answerVar <- postSelection[tieIndex,]
		## If no ties 
			if(nrow(answerVar) == 1) {
				return(answerVar$Hospital.Name)
			}
			else if (nrow(answerVar) > 1){
				sortedAns <- sort(answerVar[,1])
				return(sortedAns[1])
			}
		}
	}
}