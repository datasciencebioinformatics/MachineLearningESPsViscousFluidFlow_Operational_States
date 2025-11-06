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
# First, simulate each variable in function of P1
# Start df with the results
df_predicted_results<-data.frame(Time=c(),Value=c(),variable=c())

# The rows are increasing viscosity values and the collumns the increasing time value
# Convert the P47_viscous_3500_data_sub to time-series for each variable
# For each variable 
for (variable in c("Q","Tm.i","Tm.o","P2","T","pi","mi","mo","RPM"))
{
    # Set formula for predicting the variable in function of Q
    Formula_variable_versus_P1<-as.formula(paste(variable," ~ P1",sep=""))

    # Set random forest morel
    rf_variable_versus_P1   <- train(Formula_variable_versus_P1, data = merge_water_viscous_trainning, method = "rf" )         # K-Nearest Neighbors (KNN)                     Ok   

    # Store the trained model
    trainned_rf_models[[variable]]<-rf_variable_versus_P1
  
    # Calculate predictions
    rf_variable_versus_prediction<-predict(rf_variable_versus_P1 , merge_water_viscous_testing)

    # Add results of the variable
    df_predicted_results<-rbind(df_predicted_results,data.frame(Time=merge_water_viscous_testing$Time,value=rf_variable_versus_prediction,variable=variable))
}
####################################################################################################################################################################################
# Simulate time-series for Broken Shaft
####################################################################################################################################################################################
# Set a seed for reproducibility
set.seed(42)

# Time variable
time <- seq(0, 10 * 3.141593, length.out = 100)

# Generate an oscillating component (sine wave)
oscillation_1 <- sin(time) * 2.0  # 5 is the amplitude
oscillation_2 <- sin(time) * 1.5  # 5 is the amplitude
oscillation_3 <- cos(time) * 2.5  # 5 is the amplitude
oscillation_4 <- cos(time) * 1.5  # 5 is the amplitude

# Add a trend and some random noise
trend <- 2 # seq_along(time) * 0.1
noise <- rnorm(100, mean = 1, sd = 0.5)

# Combine components to create the final time series
oscillating_ts_1 <- ts(oscillation_1 + trend + noise, start = 1, frequency = 1)
oscillating_ts_2 <- ts(oscillation_2 + trend + noise, start = 1, frequency = 1)
oscillating_ts_3 <- ts(oscillation_3 + trend + noise, start = 1, frequency = 1)
oscillating_ts_4 <- ts(oscillation_4 + trend + noise, start = 1, frequency = 1)

# 5. Combine the data into a data frame for easy plotting
sim_data_1 <- data.frame(Time = 1:100, Noisy_Value = oscillating_ts_1,oscilation="osc1")
sim_data_2 <- data.frame(Time = 1:100, Noisy_Value = oscillating_ts_2,oscilation="osc2")
sim_data_3 <- data.frame(Time = 1:100, Noisy_Value = oscillating_ts_3,oscilation="cos1")
sim_data_4 <- data.frame(Time = 1:100, Noisy_Value = oscillating_ts_3,oscilation="cos2")
sim_data_5 <- data.frame(Time = 1:100, Noisy_Value =  merge_water_viscous_testing[1:100,"P1"],oscilation="reference")

# Set data
sim_data<-data.frame(data.matrix(rbind(as.matrix(sim_data_1),as.matrix(sim_data_2),as.matrix(sim_data_3),as.matrix(sim_data_4),as.matrix(sim_data_5))))

# Convert to numeric
sim_data$Time<-as.numeric(sim_data$Time)
sim_data$Noisy_Value<-as.numeric(sim_data$Noisy_Value)

# Repeat each simulated time-series for different inlet viscosity valkues
# 29 points are used for trainning decision tree.
# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"Gas_Locking_Simulated_Oscilation_P1_with_Noise.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  # --- Visualize the results with ggplot2 ---
  ggplot(sim_data, aes(x = Time, group=oscilation)) +
    # Plot the noisy data as points
    geom_point(aes(y = Noisy_Value, group=oscilation, colour=oscilation),alpha = 0.6) +
    # Plot the ideal, noiseless curve as a line
    geom_line(aes(y = Noisy_Value, group=oscilation, colour=oscilation), alpha = 0.6) +
    # Add labels and a title
    labs(
      title = "Simulated oscilation of P1 with Noise",
      x = "Time",
      y = "Value"
    ) +
    theme_minimal()
dev.off()

####################################################################################################################################################################################
# Simulations of Well Sanding (Pump Plugging).
# First, simulate each variable in function of Q
# Start df with the results
df_predicted_results<-data.frame(Time=c(),Value=c(),variable=c(),oscilation=c())

# The rows are increasing viscosity values and the collumns the increasing time value
# Convert the P47_viscous_3500_data_sub to time-series for each variable
# For each variable 
for (oscilation in levels(factor(sim_data$oscilation)))
{   
  # The rows are increasing viscosity values and the collumns the increasing time value
  # Convert the P47_viscous_3500_data_sub to time-series for each variable
  # For each variable 
  for (variable in c("Q","Tm.i","Tm.o","P2","T","pi","mi","mo","RPM"))
  {  
      # Store the trained model
      rf_variable_versus_P1<-trainned_rf_models[[variable]]
    
      # Calculate predictions
      rf_variable_versus_prediction<-predict(rf_variable_versus_P1 , data.frame(P1=sim_data[sim_data$oscilation==oscilation,"Noisy_Value"]))
  
      # Add results of the variable
      df_predicted_results<-rbind(df_predicted_results,data.frame(Time=time_points,value=rf_variable_versus_prediction,variable=variable,oscilation=oscilation))

  }
  # Add results of the variable
  df_predicted_results<-rbind(df_predicted_results,data.frame(Time=time_points,value=sim_data[sim_data$oscilation==oscilation,"Noisy_Value"],variable="P1",oscilation=oscilation))
}
####################################################################################################################################################################################
# Add also simulated data
####################################################################################################################################################################################
# Relevel factors
df_predicted_results$variable<-factor(df_predicted_results$variable,levels=c(c("P1","Q", "Tm.i", "Tm.o", "P2", "T", "pi", "mi", "mo","RPM")))

# Relevel factors
df_predicted_results$oscilation<-factor(df_predicted_results$oscilation)
####################################################################################################################################################################################
# Add also simulated data
####################################################################################################################################################################################
# Relevel factors
df_predicted_results$variable<-factor(df_predicted_results$variable,levels=c("P1","Q","Tm.i", "Tm.o", "P2", "T", "pi", "mi", "mo","RPM" ))

# Relevel factors
df_predicted_results$oscilation<-factor(df_predicted_results$oscilation)

# Most basic bubble plot
p2 <- ggplot(df_predicted_results, aes(x=Time, y=value,group = oscilation, color = oscilation)) +  geom_line() +   facet_grid(rows = vars(variable),scales="free") + theme_bw()  + ggtitle ("Random forest predicted time-series") + scale_colour_brewer(palette = "Set1")

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"Broken_Shaft_Simulated_time_series_ranfom_forest.png",sep=""), width = 15, height = 20, res=600, units = "cm")  
  p2
dev.off()
