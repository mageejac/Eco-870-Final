#read in the data (Level 0) from GitHub
deer_data <- read.csv('https://raw.githubusercontent.com/mageejac/Eco-870-Final/main/DBS_2022_raw.csv')

# Data Cleaning (Level 1 data)

#remove duplicated observations of the same group
deer_data_2 <- deer_data[!duplicated(deer_data$Group.GID), ]

#subset the data for group size, habitat, and location

deer_data_3 <- deer_data_2[,c("habitat_group","total_deer_group","obs.x","obs.y")]

#remove observations with unknown habitat type

deer_data_4 <- deer_data_3[!(deer_data_3$habitat_group=="unk" | deer_data_3$habitat_group=="tilled_unk"),]