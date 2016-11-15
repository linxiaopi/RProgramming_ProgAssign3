##
##

rankhospital <- function(state, outcome, num) {
        # read in data
        data <- read.csv("../assign_3_data/outcome-of-care-measures.csv", 
                         na.strings = "Not Available")
        
        # check that state and outcome are valid
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
        
        # find lowest 30-day mortality rate and make a vector of the hospitals
        # with that score
        low <- min(sub[ , outcome])
        hospital <- sub[sub[ , outcome] == low, 2]
        
        # return hospital name in that state with lowest 30-day death rate
        as.character(hospital[1])
}
