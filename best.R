## Shows the best hopital in a state for a given outcome; ties go to the
## hospital that comes first alphabetically

best <- function(state, outcome) {
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
        sub <- sub[!is.na(sub[ , outcome]) , ]
        sub <- sub[order(sub[ , outcome], sub$Hospital.Name) , ]
        
        # return hospital name in that state with lowest 30-day death rate
        as.character(sub[1,2])
}
