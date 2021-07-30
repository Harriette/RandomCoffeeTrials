library(data.table)

saveDir <- "data"   #Rather than a database
dataFile <- "Participants.rds" # Name of data file
importedDataFile <- "Data.csv" # Name of imported data file


data <- fread(paste0(saveDir, '/', importedDataFile))

readRDS(paste0(saveDir, '/', dataFile))

saveRDS(object = data, 
        file = file.path(saveDir, dataFile))

match_people(data, avoid_same = "department")

fwrite()