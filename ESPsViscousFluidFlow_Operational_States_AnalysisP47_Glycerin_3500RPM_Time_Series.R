######################################################################################################################
# Define the lenght of the time-windnw
time_window_length<-10

# Define data.frame for the results of time-series
df_results<-data.frame(Window=c(),   mean_n=c(),   sd_n=c(),      operational_state=c(),       diagnosis=c(),      ADF_test=c(),           ADF_pvalue=c(),  stationarity =c(),   Ljung_box=c(),   Ljung_box_pvalue=c(),         white_noise=c())

# First scan the P47, RPM300, Viscosity 128, Glycerin
# Take the table for Q versus efficiency
merge_water_viscous_sub_Q_n<-merge_water_viscous_sub[,c("Q","n")]

# Sort data.frame by Q
merge_water_viscous_sub_Q_n <- merge_water_viscous_sub_Q_n[order(merge_water_viscous_sub_Q_n$Q), ]

# Plot the Flow rate versus Eficiency for check 

# If I have X data points, and the window length is l
# then I have X/l windows
# Perform sliding window analysis
# First, split the vector into chuncs
time_windows<-split(merge_water_viscous_sub_Q_n$n, ceiling(seq_along(merge_water_viscous_sub_Q_n$n)/time_window_length))

# Then, for each time-series
for (time_window in time_windows)
{
  # Calculate the mean
  print(time_window)
}



# For each data.point
for (row_id in rownames(merge_water_viscous_sub))
{
  print(row_id)
  merge_water_viscous_sub[row_id,]
}

######################################################################################################################
# Assert diagnosis and classification equal to normal 
merge_water_viscous_sub<-cbind(merge_water_viscous_sub,Diagnosis="normal")

# If efficiency not stationary, then Diagnosis is fault
merge_water_viscous_sub[which(merge_water_viscous_sub$n_discrete!="High"),"Diagnosis"]<-"Fault"

# Assert diagnosis and classification equal to normal 
simulated_data_all<-cbind( data.frame(simulated_data_all,Diagnosis="normal"))

# Start all results
df_results<-data.frame(Q=c(),   Tm.i=c(),   Tm.o=c(),      P1=c(),       P2=c(),      RPM=c(),           T=c(),  pi=c(),   mi=c(),   mo=c(),         n=c(),      BHP=c(),         H=c(), Time=c(), Series=c(),operational_states=c(),Diagnosis=c())

# For each simulated time-series
for (series in unique(as.numeric(simulated_data_all[,c("Series")])))
{
  # Take the table for the corresponding time-series
  simulated_data_sub<-simulated_data_all[simulated_data_all[,c("Series")]==series,]

  # If efficiency not stationary, then Diagnosis is fault
  simulated_data_sub[which(simulated_data_sub$n_discrete!="High"),"Diagnosis"]<-"Fault"

  # Concatenate data.frame
  df_results<-rbind(df_results,simulated_data_sub)
}
# Replace dataset
simulated_data_all<-df_results
