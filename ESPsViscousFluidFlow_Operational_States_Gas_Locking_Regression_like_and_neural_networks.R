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
# Simulate time-series for Gas Locking
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

# First, simulated values of Q are generate by simulation.
# Second the values of input variables are predicted from the simulated Q.
# Thirds, the values were used to generate decision  trees.
# Only 29 points are used to generate the decision tree.
# More points can be used.
# One way is to increase the range and skip the reference data.
####################################################################################################################################################################################
# Simulations of Gas Locking
# First, simulate each variable in function of P1
# Split the dataset in training set and testing set
merge_water_viscous_trainning<-merge_water_viscous_sub[merge_water_viscous_sub$RPM==3000 ,]
merge_water_viscous_testing  <-merge_water_viscous_sub[merge_water_viscous_sub$RPM==3500 ,]


# Create a scatter plot and add Pearson correlation
p1<-ggplot(sim_data, aes(x = Time, y = Noisy_Value,oscilation)) +  geom_point() +  geom_line() +  stat_cor(method = "pearson", label.x = 0, label.y = 10) + facet_grid(rows = vars(oscilation),scale="free") + theme_bw() + xlab("Time") + ylab("P1")



