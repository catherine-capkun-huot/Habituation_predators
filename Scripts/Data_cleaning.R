##### Data cleaning ####################

#Script by Catherine Čapkun-Huot
#Date: 05-12-2021

##### Load data ########################

# Load data
data <- read.csv("data_FID.csv", header = TRUE, sep = ";")

# Structure data
data$ID <- as.factor(data$ID)
data$obs_number <- as.numeric(data$obs_number)

##### Removal of observations ##########

# First reason
# the observer’s approach was not the cause of flight (n = 13), 

data <- data[!(data$ID=="J086" & data$obs_number=="6"),]   # Remove observation 6 of chipmunk J086 because it chased R028 during the test
data <- data[!(data$ID=="L003" & data$obs_number=="7"),]   # Remove observation 7 of chipmunk L003 because it chased another chipmunk during the test
data <- data[!(data$ID=="P056" & data$obs_number=="8"),]   # Remove observation 8 of chipmunk P056 because it chased another chipmunk during the test
data <- data[!(data$ID=="Q005" & data$obs_number=="0"),]   # Remove observation 0 of chipmunk Q005 because N124 arrived during the test
data <- data[!(data$ID=="Q007" & data$obs_number=="10"),]  # Remove observation 10 of chipmunk Q007 because it chased another chipmunk during the test
data <- data[!(data$ID=="Q017" & data$obs_number=="5"),]   # Remove observation 5 of chipmunk Q017 because 2 other chipmunks were chasing each other, which caused Q017 to flee
data <- data[!(data$ID=="Q027" & data$obs_number=="11"),]  # Remove observation 11 of chipmunk Q027 because it chased another chipmunk during the test
data <- data[!(data$ID=="Q045" & data$obs_number=="2"),]   # Remove observation 2 of chipmunk Q045 because it chased Q090 during the test
data <- data[!(data$ID=="Q045" & data$obs_number=="9"),]   # Remove observation 9 of chipmunk Q045 because it chased N038 during the test
data <- data[!(data$ID=="Q045" & data$obs_number=="10"),]  # Remove observation 10 of chipmunk Q045 because it chased N038 during the test
data <- data[!(data$ID=="Q088" & data$obs_number=="1"),]   # Remove observation 1 of chipmunk Q088 because it chased another chipmunk during the test
data <- data[!(data$ID=="Q088" & data$obs_number=="14"),]  # Remove observation 14 of chipmunk Q088 because it chased another chipmunk during the test
data <- data[!(data$ID=="Q088" & data$obs_number=="15"),]  # Remove observation 15 of chipmunk Q088 because it chased another chipmunk during the test

# Second reason
# the flight was caused by the walker stumbling upon an object while approaching (n = 4)

data <- data[!(data$ID=="P057" & data$obs_number=="3"),]   # Remove observation 3 of chipmunk P057 because the walker stumbled during the test
data <- data[!(data$ID=="Q011" & data$obs_number=="11"),]  # Remove observation 11 of chipmunk Q011 because a branch cracked during the test
data <- data[!(data$ID=="Q081" & data$obs_number=="2"),]   # Remove observation 2 of chipmunk Q081 because the walker stumbled on a tree during the test
data <- data[!(data$ID=="R003" & data$obs_number=="5"),]   # Remove observation 5 of chipmunk R003 because the walker slipped during the test

# Third reason
# an observation was not flight per se (n = 2) 

data <- data[!(data$ID=="P056" & data$obs_number=="9"),]   # Remove observation 9 of chipmunk P056 because it ran towards the walker 
data <- data[!(data$ID=="Q007" & data$obs_number=="12"),]  # Remove observation 12 of chipmunk Q007 because it ran towards the walker 

# Fourth reason 
# the FID was not correctly measured (NA)

data <- subset(data, !(is.na(FID)))                        # Remove observation 4 of chipmunk P037 because the walker forgot to note the FID 

##### Export final data ################

write.csv(data,"~/Dropbox/Maîtrise/Travaux/Mémoire/texte/article/Submission/open_data/Data/data_FID_analyses.csv", row.names = FALSE)
