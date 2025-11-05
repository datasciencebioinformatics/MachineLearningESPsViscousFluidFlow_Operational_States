# TO DO : try all with the 5800 points.
# This script will add a collumn to the metafile with the operational state
# The operational state will be defined as the n=state-h=state-bhp=state
merge_water_viscous_sub<-merge_water_viscous[which(merge_water_viscous$equip=="P47" & merge_water_viscous$fluid == "Glycerin"),]

# Add Time to the variable
merge_water_viscous_sub$Time<-1:dim(merge_water_viscous_sub)[1]

# Sub-set collumns
merge_water_viscous_sub<-merge_water_viscous_sub[,c("Time","Q", "Average.Inlet.Temp.Tm.i", "Average.Outlet.Temp.Tm.o", "Inlet.Pressure.P1", "Outlet.Pressure.P2", "Shaft.Torque", "Inlet.Density.ρi", "Inlet.Viscosity.mi", "Outlet.Viscosity.mo", "RPM", "n", "H", "BHP", "Inlet.Viscosity")]

# Set colnames
colnames(merge_water_viscous_sub)<-c("Time","Q", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo", "RPM", "n", "H", "BHP", "Inlet.Viscosity")

# Convert RPM to numeric
merge_water_viscous_sub$RPM<-as.numeric(merge_water_viscous_sub$RPM)
###############################################################################################################################################################################
# List to store the models
trainned_rf_models<-list()

# Split the dataset in training set and testing set
merge_water_viscous_trainning<-merge_water_viscous_sub[merge_water_viscous_sub$RPM==3000 ,]
merge_water_viscous_testing  <-merge_water_viscous_sub[merge_water_viscous_sub$RPM==3500 ,]

# Simulations of Well Sanding (Pump Plugging).
# First, simulate each variable in function of Q
# Start df with the results
df_predicted_results<-data.frame(Time=c(),Value=c(),variable=c())

# The rows are increasing viscosity values and the collumns the increasing time value
# Convert the P47_viscous_3500_data_sub to time-series for each variable
# For each variable 
for (variable in c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo","RPM"))
{
    # Set formula for predicting the variable in function of Q
    Formula_variable_versus_n<-as.formula(paste(variable," ~ n",sep=""))

    # Set random forest morel
    rf_variable_versus_n   <- train(Formula_variable_versus_n, data = merge_water_viscous_trainning, method = "rf" )         # K-Nearest Neighbors (KNN)                     Ok   

    # Store the trained model
    trainned_rf_models[[variable]]<-rf_variable_versus_n
  
    # Calculate predictions
    rf_variable_versus_prediction<-predict(rf_variable_versus_n , merge_water_viscous_testing)

    # Add results of the variable
    df_predicted_results<-rbind(df_predicted_results,data.frame(Time=merge_water_viscous_testing$Time,value=rf_variable_versus_prediction,variable=variable))
}
####################################################################################################################################################################################
# Mett data.frame
melt_water_viscous_testing<-reshape2::melt(merge_water_viscous_testing[,c("Time","n","Q", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo", "RPM")],id.vars=c("Time"))
####################################################################################################################################################################################
# Add data type
melt_water_viscous_testing$type   <-"experimental"
df_predicted_results$type         <-"simulated"

# Merge datasets
merged_predicted_results<-rbind(df_predicted_results,melt_water_viscous_testing)

# Relevel factors
merged_predicted_results$variable<-factor(merged_predicted_results$variable,levels=c(c("n","Q","RPM", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo")))
####################################################################################################################################################################################
# Add data type
df_predicted_results$type     <-"experimental"
df_predicted_results$type     <-"simulated"

# Merge datasets
merged_predicted_results<-rbind(df_predicted_results,melt_water_viscous_testing)

# Relevel factors
merged_predicted_results$variable<-factor(merged_predicted_results$variable,levels=c(c("n","Q","RPM", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo")))
