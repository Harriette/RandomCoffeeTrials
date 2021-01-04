# Global functions for app

# Load libraries
library(shiny)
library(DT)
library(shinyalert)
library(shinyjs)
library(dplyr)
library(shinyFeedback)
library(data.table)

admin_password <- "Ineedmycoffeenow!"

departments <- c("Select Department", "Claims Technicians", "EMEA1", "EMEA2", "Loss Prevention", "Hellas",
                 "HK P&I", "New Jersey P&I", "San Fran P&I", "Secretarial and Administration",
                 "Senior Management", "Signum", "Singapore", "Underwriting", "You Forgot Mine!")

saveDir <- "data"   #Rather than a database
dataFile <- "Participants.rds" # Name of data file

# Define the fields we want to save from the form
fields <- c("first_name", "last_name", "email", "department")

# Save the data ------------
saveData <- function(input) {
  # put variables in a data frame
  data <- data.frame(matrix(nrow=1,ncol=0))
  for (x in fields) {
    var <- input[[x]]
    if (length(var) > 1 ) {
      # handles lists from checkboxGroup and multiple Select
      data[[x]] <- list(var)
    } else {
      # all other data types
      data[[x]] <- var
    }
  }
  data$submit_time <- date()
  
  # Load existing data 
  oldData <- loadData()
  
  # Confirm email not already used and then save
  if(data$email %in% oldData$email){
    shinyalert("Error", 
               paste0(data$email, " has already signed up!"), 
               type = "error",
               confirmButtonCol = "#001b8e")
    return(FALSE)
  } else {
    # Combine with existing data
    data <- rbind(oldData, data)
    # Write the file to the local system
    saveRDS(object = data, 
            file = file.path(saveDir, dataFile))
    shinyalert("Done", "Thanks for signing up!", 
               type = "success",
               confirmButtonCol = "#001b8e")
    return(TRUE)
  } 
  
}

# Load the data -------------
loadData <- function() {
  
  # Load existing data 
  files <- list.files(saveDir, full.names = TRUE)
  
  if (length(files) > 0) {
    data <- readRDS(file = file.path(saveDir, dataFile))
  } else {
    # create empty data frame with correct columns
    field_list <- c(fields, "submit_time")
    data <- data.frame(matrix(ncol = length(field_list), nrow = 0))
    names(data) <- field_list    
  }
  
  data
}

# Reset the form after use ----
resetForm <- function(session) {
  # reset values
  updateTextInput(session, "first_name", value = "")
  updateTextInput(session, "last_name", value = "")
  updateTextInput(session, "email", value = "")
  updateSelectInput(session, "department", selected = 1)
}


# Function to match people and avoid some (e.g. same department)
# Need to extend to meetings greater than 2 (if we want to do so)
match_people <- function(people, meeting_size=2, avoid_same=NULL) {
  
  require(dplyr)
  
  # Add unique id to people
  people$.id <- 1:nrow(people)
  
  # Check avoid_same is a valid column name of people
  if(!is.null(avoid_same)) {
    if( !(avoid_same %in% names(people)) ) stop("'avoid_same' is not a valid column name of 'people'") 
  }
  
  # Randomise order so each run is different
  people <- people[sample(1:nrow(people)), ]
  
  matches <- data.frame(matrix(ncol = meeting_size, 
                               nrow = ceiling( nrow(people) / meeting_size )
  )
  )
  names(matches) <- c("m1", "m2")
  meetings <- list()
  
  m <- 1 # Meeting number
  n <- 1 # Candidate number
  
  for(m in 1:nrow(matches)) {
    
    # Find matches for the first name on the list
    # Get details of that name and remove it from the list. 
    matches[m, 1] <- people$.id[n]
    meetings[[m]] <- list(people[n,])
    people <- people[-n,]
    
    if (nrow(people) > 0) {
      # If there are people remaining on the list...
      
      # Find a pool of suitable matches - those in different departments
      pool <- people[unlist(people[ , ..avoid_same]) != unlist(meetings[[m]][[1]][ , ..avoid_same]), ] 
      
      if (nrow(pool) > 0) {
        # Pick a random match from the Pool
        p <- pool[sample(1:nrow(pool), 1), ]
      } else {
        # Only those conflicted left (same department), so pick one of them
        p <- people[sample(1:nrow(people), 1), ]
      }
      
      # Store details and delete the name from the list 
      meetings[[m]][[2]] <- p
      matches[m, 2] <- p$.id
      people <- people[-match(p$.id, people$.id),]
      
    }
    
  }
  
  out <- dplyr::bind_rows(
    lapply(meetings, dplyr::bind_rows), 
    .id = "meeting"
  )
  out$meeting <- as.integer(out$meeting)
  
  
  # Last meeting might have too few people
  # If only one, then join another meeting
  last_meeting <- out[out$meeting == max(out$meeting), ]
  if(nrow(last_meeting)==1) {
    # Find a meeting with fewest colleagues
    best_meeting <- tapply(unlist(out[, ..avoid_same]), 
                           out$meeting, 
                           function(x) length(x[x==unlist(last_meeting[ , ..avoid_same])]))
    best_meeting <- as.integer( names(sort(best_meeting)[1]) )
    
    # Apply that meeting to the output
    out$meeting[out$meeting==max(out$meeting)] <- best_meeting
  }
  
  out <- meeting_participants(out)
  
  return(out[order(out$meeting), ])
  
}



meeting_participants <- function(paired_people) {
  
  paired_people <- as.data.table(paired_people)
  
  paired_people[ , id := 1:length(meeting)]
  
  temp <- paired_people[ , 
                         list(id = id,
                              email = email,
                              participants = paste(email, collapse=" ")), 
                         by=meeting]
 
  temp[ ,
        `:=`(participants = gsub(email, '', participants)),
        by = id]
  
  temp2 <- as.data.table(tstrsplit(trimws(temp$participants), split = ' +'))

  temp <- cbind(temp, temp2)
  
  paired_people <- merge(paired_people, 
                         temp[ , .SD, .SDcols = !c('email', 'participants')])
  
  return(paired_people)
  
}

