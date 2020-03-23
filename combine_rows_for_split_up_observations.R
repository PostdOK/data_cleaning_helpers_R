#data_raw is the data frame including information that should be in one row being split up
#into two rows. In this example, a student needed the number of several events per soccer
#game (e.g. shots or passes) but collected them per team in different rows. For sure, a
#problem which can happen with different types of data and in many research areas.

#Now, let’s assume that it would not be able to identify unique observations based on one
#item, which actually was the case for this example. The student had one item called
#“match-up”, but as the data.frame included several seasons, it could occur several times.
#Thus, here an example if you have to identify unique observations using two variables:

#Create the new data frame and the helper (k) for the row. More information about this later
Observations <- data.frame()
k = 0
#Use the variable with less unique values as first criteria and start a loop.
#I call this variable the SR_Variable (for smaller range), the other variable BG_Variable
Cases_SR_Variable <- unique(data_raw$Saison)
end_loop <- length(Cases_SR_Variable)
for (i in 1:end_loop) {
  #Select all the IDs that have the respective value in the SR Variable. Basically, I extract IDs for a subset
  SR_Variable_IDs <-
    as.integer(rownames(subset(
      data_raw, data_raw[, 1] == Cases_SR_Variable[i]
    )))
  #Create a Vector including all unique values of the BG_Variable for this subset IDs
  VectorObservations <- data_raw[SR_Variable_IDs, 2]
  #Now, I create a list with each object being one occurence of an unique observation.
  #Each object again consists of the IDs for this unique observation. In this case of two IDs.
  BR_Variable_ID_list <-
    tapply(seq_along(VectorObservations),
           VectorObservations,
           identity)[unique(VectorObservations)]
  #Now I loop through this list
  for (j in 1:length(BR_Variable_ID_list)) {
    #Extract the ids for the object (unique observation)
    id1 = SR_Variable_IDs[BR_Variable_ID_list[[j]][1]]
    id2 = SR_Variable_IDs[BR_Variable_ID_list[[j]][2]]
    #I will just give two examples of combining data for one observation.
    #Example: Add up the values for the observation and write it down in the Obseravtions data frame. E.g. for columns 3:10
    Observations[j + k, 3:10] = data_raw[id1, 3:10] + data_raw[id2, 3:10]
    #Example: If information is equal for both rows of the observation. E.g. column 11 
    Observations[j + k, 11] = data_raw[id1, 11]
  }
  #Note: Why j+k? j would only be enough if we can identify unique observations based on one variable
  #(and therefore in one loop)
  #This is why, we need k and manipulate it here
  k = k + length(BR_Variable_ID_list)
}