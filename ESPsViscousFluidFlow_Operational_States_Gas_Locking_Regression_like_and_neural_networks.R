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
png(filename=paste(project_folder,"Gas_Locking_Simulated_time_series_ranfom_forest.png",sep=""), width = 15, height = 20, res=600, units = "cm")  
  p2
dev.off()



####################################################################################################################################################################################
# Calculate performance, head and BHP for each configurations.
####################################################################################################################################################################################
Q    <- df_predicted_results[which(df_predicted_results$variable=="Q"),"value"]
Tm.i <- df_predicted_results[which(df_predicted_results$variable=="Tm.i"),"value"]
Tm.o <- df_predicted_results[which(df_predicted_results$variable=="Tm.o"),"value"]
P1   <-df_predicted_results[which(df_predicted_results$variable=="P1"),"value"]
P2   <-df_predicted_results[which(df_predicted_results$variable=="P2"),"value"]
T    <-df_predicted_results[which(df_predicted_results$variable=="T"),"value"]
pi   <- df_predicted_results[which(df_predicted_results$variable=="pi"),"value"]
mi   <-  df_predicted_results[which(df_predicted_results$variable=="mi"),"value"]
mo   <- df_predicted_results[which(df_predicted_results$variable=="mo"),"value"]
RPM  <- df_predicted_results[which(df_predicted_results$variable=="RPM"),"value"]
Time <- df_predicted_results[which(df_predicted_results$variable=="RPM"),"Time"]
oscilation<- df_predicted_results[which(df_predicted_results$variable=="RPM"),"oscilation"]

# Compile input variables
df_simulated_input_variables<-data.frame(Time=Time,Q=Q, Tm.i=Tm.i, Tm.o=Tm.o, P1=P1, P2=P2, T=T, pi=pi, mi=mi, mo=mo, RPM=RPM, oscilation=oscilation )

# set  the gravitational constant
# 9.81 meters per second squared (m/s2) is the approximate value of the acceleration due to gravity on Earth's surface. This value is represented by the letter g. 
g=9.81

# Number of stages (constant equanto to 3)
N   <-3 

# Store the useful power Ph
df_simulated_input_variables$P_h<-0

# The pump efficiency (n) is defined as:
# n = efficiency, dimensionless [%]
#df_simulated_input_variables$n<-0

# Store the pump head
# H = head, L, m
df_simulated_input_variables$H<-0

# Store the shaft torque is the mechanical parameter used to calculate the driving power or brake horsepower (BHP)
# BHP = mL^2t^–3, watts
df_simulated_input_variables$BHP<-0

# Compute the delta pressure
df_simulated_input_variables$Delta.Pressure<-df_simulated_input_variables$P2-df_simulated_input_variables$P1

# First, Calculate the velocity
for (measure in rownames(df_simulated_input_variables))
{ 
  # Model
  model=df_simulated_input_variables[measure,"equip"]

  # Impeller Diameter mm
  # Unit checked mm
  D=unique(metada_data[which(metada_data$model == "P47"),2]) * 1000 # m converted to mm
    
  # Store inlet and outlet pressure
  # P = pressure, mL–1t–2, Pa
  # Unit checked pa
  P1  <-as.numeric(df_simulated_input_variables[measure,"P1"])  * 100000 # Bar converted to pa
  P2  <-as.numeric(df_simulated_input_variables[measure,"P2"]) * 100000 # Bar converted to pa
  
  # Density
  # p = density, mL–3, kg/m3
  # Unit checked kg/m3
  p  <-as.numeric(df_simulated_input_variables[measure,"pi"])

  # The Flow Rate, Q 
  # Q = volumetric flow rate, L3, t–1, m3/s
  # Unit checked m3/h
  #df_simulated_input_variables[measure,"Q"]<-as.numeric(df_simulated_input_variables[measure,"Q"])/(p) # kg/h converted to m3/s

  # Head pump
  # H = head, L, m
  df_simulated_input_variables[measure,"H"]<-((P2-P1)/(p*g))*(1/N) 

  # The rotational speed w in rads/s
  # unit checked RPM
  w=as.numeric(df_simulated_input_variables[measure,"RPM"])*0.10472
  
  # Net.Shaft.Torque
  # T = shaft torque, mL2,t–2, N·m
  # unit checked N·m
  T=as.numeric(df_simulated_input_variables[measure,"T"])

  # ESP is the BHP
  # BHP = mL^2t^–3, watts
  df_simulated_input_variables[measure,"BHP"]=(1/N)*(w*T)

  #P_{h}: The power in kilowatts (kW) 
  # Q    : The flow capacity in cubic meters per hour (m3/h) 
  # p    : The density of the fluid in kilograms per cubic meter (kg/m3) 
  # g    : The acceleration due to gravity (9.81m/s2) 
  # H    : The differential head in meters (m)
  
  # useful power Ph 
  # Flow rate in m3/s
  df_simulated_input_variables[measure,"P_h"]<- (p*g*df_simulated_input_variables[measure,"H"]*(df_simulated_input_variables[measure,"Q"]*(1/3600)))

  # The pump efficiency (n) is defined as:
  # n = efficiency, dimensionless [%]
  df_simulated_input_variables[measure,"n"] <- df_simulated_input_variables[measure,"P_h"]/df_simulated_input_variables[measure,"BHP"]
}
#######################################################################################################
# add plot
melt_simulated_input_variables<-reshape2::melt(df_simulated_input_variables,id.vars=c("Time","oscilation")) 

# Most basic bubble plot
p3 <- ggplot(melt_simulated_input_variables[melt_simulated_input_variables$variable %in% c("n","H","BHP"),], aes(x=Time, y=value,group = oscilation, color = oscilation)) +  geom_line() +   facet_grid(rows = vars(variable),scales="free") + theme_bw()  + ggtitle ("Random forest predicted time-series - performance variables") + scale_colour_brewer(palette = "Set1")

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"Gas_Locking_Simulated_performance_variables_ranfom_forest.png",sep=""), width = 15, height = 10, res=600, units = "cm")  
  p3
dev.off()
#######################################################################################################
