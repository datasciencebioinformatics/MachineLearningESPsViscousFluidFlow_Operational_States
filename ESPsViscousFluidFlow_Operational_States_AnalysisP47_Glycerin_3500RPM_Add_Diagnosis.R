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

