#####################################################################################################################################################################################################################################
# Assert the operational states on the Monte Verde data.
# The operational state will be defined as the n=state-h=state-bhp=state
# This script will add a collumn to the metafile with the operational state
# The operational state will be defined as the n=state-h=state-bhp=state
merge_water_viscous_sub<-merge_water_viscous[which(merge_water_viscous$equip=="P47" & merge_water_viscous$fluid == "Glycerin" & merge_water_viscous$RPM=="3500" & merge_water_viscous$Inlet.Viscosity == "128"),]

# Take the tertiles
merge_water_viscous_sub_tertiles<-as.data.frame(lapply(merge_water_viscous_sub[,c("n","BHP","H")], tertile))

# Renames collumns
colnames(merge_water_viscous_sub_tertiles)<-c("n_discrete","BHP_discrete","H_discrete")

# Merge tables
merge_water_viscous_sub<-cbind(merge_water_viscous_sub,merge_water_viscous_sub_tertiles)

# add operational states
merge_water_viscous_sub$operational_states<-paste(paste("n=",merge_water_viscous_sub$n_discrete,sep=""),paste("BHP=",merge_water_viscous_sub$BHP_discrete,sep=""),paste("H=",merge_water_viscous_sub$H_discrete,sep=""),sep="|")

# Plot operati0nal states
#####################################################################################################################################################################################################################################
# Assert the operational states on the simulated data.
# Load the simulated data
# simulated_data_all
# Raname collumns
colnames(simulated_data_all)<-c("Q", "Tm.i", "Tm.o", "P1", "P2", "RPM", "T", "pi", "mi", "mo", "n", "BHP", "H", "Time", "Series")

# Remove NA by zero
# Replace all NA values with 0
simulated_data_all[is.na(simulated_data_all)] <- 0

# Start all results
df_results<-data.frame(Q=c(),   Tm.i=c(),   Tm.o=c(),      P1=c(),       P2=c(),      RPM=c(),           T=c(),  pi=c(),   mi=c(),   mo=c(),         n=c(),      BHP=c(),         H=c(), Time=c(), Series=c())

# For each simulated time-series
for (series in unique(as.numeric(simulated_data_all[,c("Series")])))
{
  # Take the table for the corresponding time-series
  simulated_data_sub<-simulated_data_all[simulated_data_all[,c("Series")]==series,]

  n_discrete<-cut(simulated_data_sub[,c("n")], quantile(simulated_data_sub[,c("n")], c(0:3/3)), include.lowest = T, labels = c("Low", "Medium", "High"))
  h_discrete<-cut(simulated_data_sub[,c("H")], quantile(simulated_data_sub[,c("H")], c(0:3/3)), include.lowest = T, labels = c("Low", "Medium", "High"))

  # If all BHP value is zero
  if(sum(simulated_data_sub[,c("BHP")])==0)
  {
      bhp_discrete<-"Low"
  }else
  {
      bhp_discrete<-cut(simulated_data_sub[,c("BHP")], quantile(simulated_data_sub[,c("BHP")], c(0:3/3)), include.lowest = T, labels = c("Low", "Medium", "High"))
  }
  
  # Set the tertiles
  simulated_data_sub_tertiles<-data.frame(n_discrete=n_discrete,h_discrete=h_discrete,bhp_discrete=bhp_discrete)
  
  # Merge tables
  simulated_data_sub<-cbind(simulated_data_sub,simulated_data_sub_tertiles)
  
  # add operational states
  simulated_data_sub$operational_states<-paste(paste("n=",simulated_data_sub$n_discrete,sep=""),paste("BHP=",simulated_data_sub$h_discrete,sep=""),paste("H=",simulated_data_sub$bhp_discrete,sep=""),sep="|")  

  # Concatenate data.frame
  df_results<-rbind(df_results,simulated_data_sub)
}
#####################################################################################################################################################################################################################################
# Replace data.frames
simulated_data_all<-df_results

# Set rownames
rownames(simulated_data_all)<-1:dim(simulated_data_all)[1]

