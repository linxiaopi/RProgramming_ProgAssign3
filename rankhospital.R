##
##

rankhospital <- function(state, outcome, num) {
        # read in data
        data <- read.csv("../assign_3_data/outcome-of-care-measures.csv", 
                         na.strings = "Not Available")
        
        # check that "state" and "outcome" args are valid
        ST <- which(data$State == state)
        if (sum(ST) < 1) {
                stop("invalid state")
        }
        if (!(outcome == "heart attack" | outcome == "heart failure" | 
                outcome == "pneumonia")) {
                stop("invalid outcome")
        }
        
        # assign appropriate column numbers to 'outcome'
        if (outcome == "heart attack")
                        outcome <- 11
        if (outcome == "heart failure")
                        outcome <- 17
        if (outcome == "pneumonia")
                        outcome <- 23
        
        # subset to state, remove NAs, sort by outcome and alphabetize data
        sub <- data[data$State == state , ]
        sub <- sub[!is.na(sub[ , outcome]), ]
        sub <- sub[order(sub[ , outcome], sub$Hospital.Name) , ]
        
        # add "rank" to "sub" data frame
        sub$rank <- 1:nrow(sub)
        
        # check that "rank" arg is valid
        if (!(num == "best" | num == "worst" |
              (num > 0 & num < nrow(sub)))) {
                return(NA)
        }
                
        
        # return hospital name in chosen state with specified rank
        if (num == "best") {
                num <- 1
        }
                
        if (num == "worst") {
                num <- max(sub$rank)
        }
                
        as.character(sub[num,2])
}