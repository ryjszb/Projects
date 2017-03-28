#Want to pull in the for R sheet of the excel file
# This has all Count data for the 100 sites for each of the 4 treatments observations
setwd("C:/Users/barto/Documents/675/Consulting")
mydata <- read.csv("for_R_sheet.csv", header = TRUE)

#Want to collapse to find the number of total attacks at each site independent of treatment
library(doBy)
collapsed <- summaryBy(Attacked ~ Site, FUN = sum, data = mydata)
# keep only the sites that there was an attack
had_attack <- subset(collapsed, collapsed$Attacked.sum != 0)
#convert this data frame to a vector
attacked_sites <- as.numeric(had_attack$Site)


#Want to only keep the data at these sites
#Need a way to find which sites are in the vector

in_vec <- function(vec, test_num) {
  #want to loop through the vector to see if the test_num is in the vector
  n <- length(vec)
  #this is the number of comparisons we need to make
  return_val = FALSE
  #Return false if never told otherwise
  for (ind in c(1:n)) {
    #loop through each element of vector
    if (test_num == vec[ind]) {
      #if test_num matches any number in the vector return true
      return_val = TRUE
    }
  }
  return(return_val)
}

#initialize logical vector
logic_attacked_sites <- vector(mode = "logical", length = 400)

#use function to find entries of dataset in sites with attacks
for (ind in c(1:400)) {
  logic_attacked_sites[ind] <- in_vec(attacked_sites, mydata$Site[ind])
}

#this keeps only the TRUE values from the vector created in the loop
attacked_site_data <- mydata[logic_attacked_sites,]

#write.table(attacked_site_data, "Attacked_Sites.csv") This tried to write to csv but didnt
#     work very well

#This creates binary variable had_attack to replace the count data of sum of attacks
attacked_site_data$had_attack <- ifelse(attacked_site_data$Attacked == 0, 0, 1)

#This then sums the amount of sites that had an attack by each treatment
Table_setup <- summaryBy(had_attack ~ Treatment, FUN = sum, data = attacked_site_data)
#The amount of sites without attacks is 25 - sites with an attack
Table_setup$No_attacks <- 25 - Table_setup$had_attack.sum

#write.table(Table_setup, "4by3Table.csv") This was supposed to export that table


