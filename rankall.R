## Shows the hopital in each state that has the specified rank
## for the specified outcome;
## ties go to the hospital that comes first alphabetically

rankall <- function(outcome, num = "best") {
        # read in data
        data <- read.csv("../assign_3_data/outcome-of-care-measures.csv", 
                         na.strings = "Not Available")
        
        # check that "outcome" arg is valid
        if (!(outcome == "heart attack" | outcome == "heart failure" | 
              outcome == "pneumonia")) {
                stop("invalid outcome")
        }
        
        
        # check that "rank" arg is valid
        if (!(num == "best" | num == "worst" | num > 0)) {
                stop("invalid num")
        }
        
        
        # assign appropriate column numbers to 'outcome'
        if (outcome == "heart attack")
                outcome <- 11
        if (outcome == "heart failure")
                outcome <- 17
        if (outcome == "pneumonia")
                outcome <- 23
        
        
        # subset to remove NAs, order by state and outcome and alphabetize hospitals
        sub <- data[!is.na(data[ , outcome]), ]
        
        # reverse outcome order for num = "worst"
        if (num == "worst") {
                sub <- sub[order(sub$State, -sub[ , outcome], sub$Hospital.Name) , ]
        }
        else {
                sub <- sub[order(sub$State, sub[ , outcome], sub$Hospital.Name) , ]
        }
                
        
        
        # subset to required columns; rename columns, add split (by state)
        sub <- sub[ , c(2, 7, outcome)]
        names(sub) <- c("hospital","st", "rate")
        by_state <- split(sub, sub$st)
        

        # get list of hospitals with rank of "num" 
        if (num == "best" | num == "worst") num <- 1
        hosp <- lapply(by_state, function(x) as.character( x[num , 1]) )
        
        
        # return data frame with hospital of specified rank in each state
        data.frame(HOSPITAL = unlist(hosp, use.names = FALSE), STATE = names(hosp))

}