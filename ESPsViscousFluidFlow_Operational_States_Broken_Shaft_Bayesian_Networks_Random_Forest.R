####################################################################################################################################################################################
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
####################################################################################################################################################################################
# Simulate time-series for Broken Shaft
####################################################################################################################################################################################
# Set a seed for reproducibility
set.seed(42)

# --- Define the simulation parameters ---
# Number of data points
time_points <- 1:101 # Number of time points

# Exponential decay parameters
b_1 <- pracma::sigmoid(50:-50,a=0.1,b=1) + rnorm(length(-50:50), mean = 0, sd = 0.01) 
b_2 <- pracma::sigmoid(50:-50,a=0.15,b=1) + rnorm(length(-50:50), mean = 0, sd = 0.01)
b_3 <- pracma::sigmoid(50:-50,a=0.2,b=1) + rnorm(length(-50:50), mean = 0, sd = 0.01)

# 5. Combine the data into a data frame for easy plotting
sim_data_1 <- data.frame(Time = time_points, Noisy_Value = b_1/10,b="0.25")
sim_data_2 <- data.frame(Time = time_points, Noisy_Value = b_2/10,b="0.15")
sim_data_3 <- data.frame(Time = time_points, Noisy_Value = b_3/10,b="0.1")
sim_data_4 <- data.frame(Time = time_points, Noisy_Value =  merge_water_viscous_testing[time_points,"n"],b="reference")

# Set data
sim_data<-rbind(sim_data_1,sim_data_2,sim_data_3,sim_data_4)

# Repeat each simulated time-series for different inlet viscosity valkues
# 29 points are used for trainning decision tree.
# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"Broken_Shaft_Simulated_Declining_Flow_Rate_n_with_Noise.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  # --- Visualize the results with ggplot2 ---
  ggplot(sim_data, aes(x = Time, group=decay_rate)) +
    # Plot the noisy data as points
    geom_point(aes(y = Noisy_Value, group=b, colour=b),alpha = 0.6) +
    # Plot the ideal, noiseless curve as a line
    geom_line(aes(y = Noisy_Value, group=b, colour=b), color = "blue", alpha = 0.6) +
    # Add labels and a title
    labs(
      title = "Simulated Declining Efficiency n with Noise",
      x = "Time",
      y = "Value"
    ) +
    theme_minimal()
dev.off()
