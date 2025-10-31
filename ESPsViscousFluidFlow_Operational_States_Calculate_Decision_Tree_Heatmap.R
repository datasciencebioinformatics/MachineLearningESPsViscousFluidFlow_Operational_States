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
# Add data type
df_predicted_results$type     <-"experimental"
df_predicted_results$type     <-"simulated"

# Merge datasets
merged_predicted_results<-rbind(df_predicted_results,melt_water_viscous_testing)

# Relevel factors
merged_predicted_results$variable<-factor(merged_predicted_results$variable,levels=c(c("Q","RPM", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo")))
####################################################################################################################################################################################
# Simulate time-series for Simulations of Well Sanding (Pump Plugging)
####################################################################################################################################################################################
# Set a seed for reproducibility
set.seed(42)

# --- Define the simulation parameters ---
# Number of data points
n_points <- 100 # length(unique(merge_water_viscous_testing$Time))

# Exponential decay parameters
initial_value <- 50
decay_rate_1 <- 0.01
decay_rate_2 <- 0.1
decay_rate_3 <- 0.25

# Noise parameters (normally distributed)
noise_mean_1 <- 0
noise_mean_2 <- 0
noise_mean_3 <- 0
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
#sim_data_1 <- data.frame(Time = unique(merge_water_viscous_testing$Time), Noisy_Value = noisy_data_1, Ideal_Value = ideal_decay_1,decay_rate="0.01")
#sim_data_2 <- data.frame(Time = unique(merge_water_viscous_testing$Time), Noisy_Value = noisy_data_2, Ideal_Value = ideal_decay_2,decay_rate="0.1")
#sim_data_3 <- data.frame(Time = unique(merge_water_viscous_testing$Time), Noisy_Value = noisy_data_3, Ideal_Value = ideal_decay_3,decay_rate="0.25")

# 5. Combine the data into a data frame for easy plotting
sim_data_1 <- data.frame(Time = 1:n_points, Noisy_Value = noisy_data_1, Ideal_Value = ideal_decay_1,decay_rate="0.01")
sim_data_2 <- data.frame(Time = 1:n_points, Noisy_Value = noisy_data_2, Ideal_Value = ideal_decay_2,decay_rate="0.1")
sim_data_3 <- data.frame(Time = 1:n_points, Noisy_Value = noisy_data_3, Ideal_Value = ideal_decay_3,decay_rate="0.25")
sim_data_4 <- data.frame(Time = 1:n_points, Noisy_Value =  merge_water_viscous_testing[1:n_points,"Q"], Ideal_Value =  merge_water_viscous_testing[1:n_points,"Q"],decay_rate="reference")

# Set data
sim_data<-rbind(sim_data_1,sim_data_2,sim_data_3,sim_data_4)

# Repeat each simulated time-series for different inlet viscosity valkues
# 29 points are used for trainning decision tree.

# First, simulated values of Q are generate by simulation.
# Second the values of input variables are predicted from the simulated Q.
# Thirds, the values were used to generate decision  trees.
# Only 29 points are used to generate the decision tree.
# More points can be used.
# One way is to increase the range and skip the reference data.
####################################################################################################################################################################################
# Simulations of Well Sanding (Pump Plugging).
# First, simulate each variable in function of Q
# Start df with the results
df_predicted_results<-data.frame(Time=c(),Value=c(),variable=c(),decay=c())

# The rows are increasing viscosity values and the collumns the increasing time value
# Convert the P47_viscous_3500_data_sub to time-series for each variable
# For each variable 
for (decay_rate in levels(factor(sim_data$decay_rate)))
{   
  # The rows are increasing viscosity values and the collumns the increasing time value
  # Convert the P47_viscous_3500_data_sub to time-series for each variable
  # For each variable 
  for (variable in c("Tm.i","Tm.o","P1","P2","T","pi","mi","mo","RPM"))
  {  
      # Store the trained model
      rf_variable_versus_Q<-trainned_rf_models[[variable]]
    
      # Calculate predictions
      rf_variable_versus_prediction<-predict(rf_variable_versus_Q , data.frame(Q=sim_data[sim_data$decay_rate==decay_rate,"Noisy_Value"]))
  
      # Add results of the variable
      df_predicted_results<-rbind(df_predicted_results,data.frame(Time=1:n_points,value=rf_variable_versus_prediction,variable=variable,decay=decay_rate))

  }
  # Add results of the variable
  df_predicted_results<-rbind(df_predicted_results,data.frame(Time=1:n_points,value=sim_data[sim_data$decay_rate==decay_rate,"Noisy_Value"],variable="Q",decay=decay_rate))
}
####################################################################################################################################################################################
# Add also simulated data
####################################################################################################################################################################################
# Relevel factors
df_predicted_results$variable<-factor(df_predicted_results$variable,levels=c(c("Q","RPM", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo")))

# Relevel factors
df_predicted_results$decay<-factor(df_predicted_results$decay)
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
decay<- df_predicted_results[which(df_predicted_results$variable=="RPM"),"decay"]

# Compile input variables
df_simulated_input_variables<-data.frame(Time=Time, Q=Q, Tm.i=Tm.i, Tm.o=Tm.o, P1=P1, P2=P2, T=T, pi=pi, mi=mi, mo=mo, RPM=RPM, decay=decay )

# set  the gravitational constant
# 9.81 meters per second squared (m/s2) is the approximate value of the acceleration due to gravity on Earth's surface. This value is represented by the letter g. 
g=9.81

# Number of stages (constant equanto to 3)
N   <-3 

# Store the useful power Ph
df_simulated_input_variables$P_h<-0

# The pump efficiency (n) is defined as:
# n = efficiency, dimensionless [%]
df_simulated_input_variables$n<-0

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
################################################################
# Start a data.frame
df_simulated_input_variables_bck<- data.frame(c(Time=c(), Q=c(),Tm.i=c(), Tm.o=c(), P1=c(), P2=c(),T=c(),pi=c(),mi=c(),mo=c(), RPM=c(),decay=c(), P_h=c(), n=c(), H=c(), BHP=c(), Delta.Pressure=c()))


# Plot the heatmap - all
for (decay in unique(df_simulated_input_variables$decay))
{
    # Take dec
    decay_data<-df_simulated_input_variables[which(df_simulated_input_variables$decay==decay),]
    
    # Take time data
    decay_data$Time<-paste("Time_",decay_data$Time,sep="")
    
    # Set rownames
    rownames(decay_data)<-decay_data$Time

    # Take the tertiles
    decay_data_tertiles<-as.data.frame(lapply(decay_data[,c("n","BHP","H")], tertile))
    
    # Renames collumns
    colnames(decay_data_tertiles)<-c("n_discrete","BHP_discrete","H_discrete")
    
    # Merge tables
    decay_data<-cbind(decay_data,decay_data_tertiles)    

    # add operational states
    decay_data$operational_states<-paste(paste("n=",decay_data$n_discrete,sep=""),paste("BHP=",decay_data$BHP_discrete,sep=""),paste("H=",decay_data$H_discrete,sep=""),sep="|")

    # Set Diagnosis
    decay_data$Diagnosis <-"Normal"

    # If efficiency not stationary, then Diagnosis is fault
    decay_data[which(decay_data$n_discrete!="High"),"Diagnosis"]<-"Fault"

    # add data.frame with operational states and diagnosis
    df_simulated_input_variables_bck<-rbind(df_simulated_input_variables_bck,decay_data)
}


# Add operational states
# Add doiagnosis
# Add tertiles
################################################################
# The decision tree can be fitted using alll viscosity groups. #
################################################################
# Plot the heatmap - all
for (decay in unique(df_simulated_input_variables$decay))
{
    # Take dec
    decay_data<-df_simulated_input_variables[which(df_simulated_input_variables$decay==decay),]

    # Take time data
    decay_data$Time<-paste("Time_",decay_data$Time,sep="")
    
    # Set rownames
    rownames(decay_data)<-decay_data$Time

    # Remove row lines
    annotation_row_exp=decay_data[,c("n","BHP","H","operational_states","Diagnosis")]

    # Re-set colnmaes
    colnames(annotation_row_exp)<-c("n","BHP","H","operational_states","Diagnosis")

    # Specify colors
    ann_colors = list(n = c(Low="lightgrey", Medium="darkgrey",High="black"), BHP = c(Low="lightgrey", Medium="darkgrey",High="black"), H = c(Low="lightgrey", Medium="darkgrey",High="black") )

    # Normalized values for variables
    decay_data_discrete <- as.data.frame(lapply(decay_data[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo"),], tertile))

    # Add decay_data_discrete
    decay_data_discrete<-cbind(decay_data_discrete,annotation_row_exp)
  
    # Calculate tree
    Diagnosis_rpart_operational_states<-rpart(formula=operational_states ~ Q + Tm.i + Tm.o + P1 + P2 + T + pi + mi + mo, data=decay_data_discrete,method = "class")
    Diagnosis_rpart_Diagnosis         <-rpart(formula=Diagnosis ~ Q + Tm.i + Tm.o + P1 + P2 + T + pi + mi + mo, data=decay_data_discrete,method = "class")
   
    # bwplot               
    png(filename=paste(output_dir,paste("rpart_Operational_state_decay_",decay,".png",sep="")), width = 15, height = 15, res=600, units = "cm")  
      # Plot the bayesian network graph
      fancyRpartPlot(Diagnosis_rpart_operational_states, caption = NULL, sub=NULL)  
    dev.off()
 
    # bwplot               
    png(filename=paste(output_dir,paste("rpart_Diagnosis_decay_",decay,".png",sep="")), width = 15, height = 15, res=600, units = "cm")  
      # Plot the bayesian network graph
      fancyRpartPlot(Diagnosis_rpart_Diagnosis, caption = NULL, sub=NULL)  
    dev.off()
}
