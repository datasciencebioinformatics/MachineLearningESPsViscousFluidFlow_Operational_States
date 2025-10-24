# TO DO : try all with the 5800 points.

# This script will add a collumn to the metafile with the operational state
# The operational state will be defined as the n=state-h=state-bhp=state
merge_water_viscous_sub<-merge_water_viscous[which(merge_water_viscous$equip=="P47" & merge_water_viscous$fluid == "Glycerin"),]

# Add Time to the variable
merge_water_viscous_sub$Time<-1:dim(merge_water_viscous_sub)[1]

# Sub-set collumns
merge_water_viscous_sub<-merge_water_viscous_sub[,c("Time","Flow.rate", "Average.Inlet.Temp.Tm.i", "Average.Outlet.Temp.Tm.o", "Inlet.Pressure.P1", "Outlet.Pressure.P2", "Shaft.Torque", "Inlet.Density.ρi", "Inlet.Viscosity.mi", "Outlet.Viscosity.mo", "RPM", "n", "H", "BHP", "Inlet.Viscosity")]

# Set colnames
colnames(merge_water_viscous_sub)<-c("Time","Q", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo", "RPM", "n", "H", "BHP", "Inlet.Viscosity")

# Convert RPM to numeric
merge_water_viscous_sub$RPM<-as.numeric(merge_water_viscous_sub$RPM)

###############################################################################################################################################################################
# Mett data.frame
melt_water_viscous_sub<-reshape2::melt(merge_water_viscous_sub[,c("Time","Q", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo", "RPM")],id.vars=c("Time"))

# Most basic bubble plot
p <- ggplot(melt_water_viscous_sub, aes(x=Time, y=value)) +  geom_line() +   facet_grid(rows = vars(variable),scales="free") + theme_bw()  + ggtitle ("Reference time-series")

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"Reference_time_series.png",sep=""), width = 15, height = 20, res=600, units = "cm")  
  p
dev.off()
####################################################################################################################################################################################
# Split the dataset in training set and testing set
merge_water_viscous_trainning<-merge_water_viscous_sub[merge_water_viscous_sub$RPM!=3500,]
merge_water_viscous_testing<-merge_water_viscous_sub[merge_water_viscous_sub$RPM==3500,]

# Simulations of Well Sanding (Pump Plugging).
# First, simulate each variable in function of Q
# Start df with the results
df_predicted_results<-data.frame(Time=c(),Value=c(),variable=c())

# The rows are increasing viscosity values and the collumns the increasing time value
# Convert the P47_viscous_3500_data_sub to time-series for each variable
# For each variable 
for (variable in c("Tm.i","Tm.o","P1","P2","T","pi","mi","mo","RPM"))
{
    # Set formula for predicting the variable in function of Q
    Formula_variable_versus_Q<-as.formula(paste(variable," ~ Q",sep=""))

    # Set random forest morel
    rf_variable_versus_Q   <- train(Formula_variable_versus_Q, data = merge_water_viscous_trainning, method = "knn", tuneGrid =data.frame(k = seq(1,100,by = 5) ) )         # K-Nearest Neighbors (KNN)                     Ok   

    # Calculate predictions
    rf_variable_versus_prediction<-predict(rf_variable_versus_Q , merge_water_viscous_testing)

    # Add results of the variable
    df_predicted_results<-rbind(df_predicted_results,data.frame(Time=merge_water_viscous_testing$Time,value=rf_variable_versus_prediction,variable=variable))
}
####################################################################################################################################################################################
# Mett data.frame
melt_water_viscous_testing<-reshape2::melt(merge_water_viscous_testing[,c("Time","Q", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo", "RPM")],id.vars=c("Time"))
####################################################################################################################################################################################
# Add data type
melt_water_viscous_testing$type   <-"experimental"
df_predicted_results$type         <-"simulated"

# Merge datasets
merged_predicted_results<-rbind(df_predicted_results,melt_water_viscous_testing)

# Relevel factors
merged_predicted_results$variable<-factor(merged_predicted_results$variable,levels=c(c("Q","RPM", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo")))

# Most basic bubble plot
p2 <- ggplot(merged_predicted_results, aes(x=Time, y=value,group = type, color = type)) +  geom_line() +   facet_grid(rows = vars(variable),scales="free") + theme_bw()  + ggtitle ("Random forest predicted time-series") + scale_colour_brewer(palette = "Set1")

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"Reference_time_series_ranfom_forest.png",sep=""), width = 15, height = 20, res=600, units = "cm")  
  p2
dev.off()
####################################################################################################################################################################################
