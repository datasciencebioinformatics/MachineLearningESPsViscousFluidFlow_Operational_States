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
# List to store the models
trainned_rf_models<-list()

# Split the dataset in training set and testing set
merge_water_viscous_trainning<-merge_water_viscous_sub[merge_water_viscous_sub$RPM!=3500,]
merge_water_viscous_testing  <-merge_water_viscous_sub[merge_water_viscous_sub$RPM==3500,]

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
    rf_variable_versus_Q   <- train(Formula_variable_versus_Q, data = merge_water_viscous_trainning, method = "rf" )         # K-Nearest Neighbors (KNN)                     Ok   

    # Store the trained model
    trainned_rf_models[[variable]]<-rf_variable_versus_Q
  
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
# Simulate time-series for Simulations of Well Sanding (Pump Plugging)
####################################################################################################################################################################################
# Set a seed for reproducibility
set.seed(42)

# --- Define the simulation parameters ---
# Number of data points
n_points <- length(unique(merge_water_viscous_testing$Time))

# Exponential decay parameters
initial_value <- 50
decay_rate_1 <- 0.01
decay_rate_2 <- 0.1
decay_rate_2 <- 0.25

# Noise parameters (normally distributed)
noise_mean_1 <- 0
noise_mean_2 <- 5
noise_mean_3 <- 10
noise_sd_1   <- 2 # Standard deviation of the noise
noise_sd_2   <- 2 # Standard deviation of the noise
noise_sd_3   <- 2 # Standard deviation of the noise

# --- Simulate the data ---
# 1. Create a time vector
time <- 1:n_points

# 2. Generate the ideal (noiseless) exponential decay curve
ideal_decay_1 <- initial_value * exp(-decay_rate_1 * time)
ideal_decay_2 <- initial_value * exp(-decay_rate_2 * time)
ideal_decay_3 <- initial_value * exp(-decay_rate_3 * time)


# 3. Generate random noise
noise_1 <- rnorm(n_points, mean = noise_mean_1, sd = noise_sd_1)
noise_2 <- rnorm(n_points, mean = noise_mean_2, sd = noise_sd_2)
noise_3 <- rnorm(n_points, mean = noise_mean_3, sd = noise_sd_3)

# 4. Create the final noisy data by adding the noise to the ideal curve
noisy_data_1 <- ideal_decay_1 + noise_1
noisy_data_2 <- ideal_decay_2 + noise_2
noisy_data_3 <- ideal_decay_3 + noise_3

# 5. Combine the data into a data frame for easy plotting
sim_data_1 <- data.frame(Time = time, Noisy_Value = noisy_data_1, Ideal_Value = ideal_decay_1,decay_rate="0.01")
sim_data_2 <- data.frame(Time = time, Noisy_Value = noisy_data_2, Ideal_Value = ideal_decay_2,decay_rate="0.1")
sim_data_3 <- data.frame(Time = time, Noisy_Value = noisy_data_3, Ideal_Value = ideal_decay_3,decay_rate="0.25")

# Set data
sim_data<-rbind(sim_data_1,sim_data_2,sim_data_3)


# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"Simulated_Declining_Flow_Rate_Q with_Noise.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  # --- Visualize the results with ggplot2 ---
  ggplot(sim_data, aes(x = Time, group=decay_rate)) +
    # Plot the noisy data as points
    geom_point(aes(y = Noisy_Value, group=decay_rate, colour=decay_rate),alpha = 0.6) +
    # Plot the ideal, noiseless curve as a line
    geom_line(aes(y = Noisy_Value, group=decay_rate, colour=decay_rate), color = "blue", alpha = 0.6) +
    # Add labels and a title
    labs(
      title = "Simulated Declining Flow Rate Q with Noise",
      x = "Time",
      y = "Value"
    ) +
    theme_minimal()
dev.off()
