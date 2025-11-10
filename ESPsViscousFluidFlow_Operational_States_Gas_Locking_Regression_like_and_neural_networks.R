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
noise <- rnorm(100, mean = 2, sd = 0.5)

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
      df_predicted_results<-rbind(df_predicted_results,data.frame(Time=1:100,value=rf_variable_versus_prediction,variable=variable,oscilation=oscilation))

  }
  # Add results of the variable
  df_predicted_results<-rbind(df_predicted_results,data.frame(Time=1:100,value=sim_data[sim_data$oscilation==oscilation,"Noisy_Value"],variable="P1",oscilation=oscilation))
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

#######################################################################################################
# Fig. 7—ESP P47 performance pumping viscous fluid at 3,500 rev/min.
ESP_P47_water_plot_Q_H <- ggplot(df_simulated_input_variables[,c("Q","n","H","BHP","oscilation")], aes(x = Q, y = H,colour=oscilation))     + geom_point() + geom_line()  + theme_bw()   + ggtitle ("Flow rate Q vs. Head H")    + ylab("Head H [m]")                   + labs(x = expression("Flow rate Q [" * m^3/h * "]")) + theme(legend.position = "bottom")  
ESP_P47_water_plot_BHP <- ggplot(df_simulated_input_variables[,c("Q","n","H","BHP","oscilation")], aes(x = Q, y = BHP,colour=oscilation))   + geom_point() + geom_line() + theme_bw()   + ggtitle ("Flow rate Q vs. Power BHP") + ylab("Power BHP [W]")                + labs(x = expression("Flow rate Q [" * m^3/h * "]")) + theme(legend.position = "none")    
ESP_P47_water_plot_n   <- ggplot(df_simulated_input_variables[,c("Q","n","H","BHP","oscilation")], aes(x = Q, y = n*100,colour=oscilation)) + geom_point() + geom_line() + theme_bw()   + ggtitle ("Flow rate Q vs. Efficiency n") + ylab("Efficiency n [%]")          + labs(x = expression("Flow rate Q [" * m^3/h * "]"))   + theme(legend.position = "bottom")      

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"Gas_Locking_ESP_P47_dilluted_glucerin_Operational_states.png",sep=""), width = 20, height = 25, res=600, units = "cm")  
  ggarrange(ESP_P47_water_plot_Q_H,ESP_P47_water_plot_BHP,ESP_P47_water_plot_n, nrow =3,common.legend = TRUE,legend="bottom")
dev.off()
#######################################################################################################
# Start a data.frame
df_simulated_input_variables_bck<- data.frame(c(Time=c(), Q=c(),Tm.i=c(), Tm.o=c(), P1=c(), P2=c(),T=c(),pi=c(),mi=c(),mo=c(), RPM=c(),decay=c(), P_h=c(), n=c(), H=c(), BHP=c(), Delta.Pressure=c()))

# Plot the heatmap - all
for (oscilation in unique(df_simulated_input_variables$oscilation))
{
    # Take dec
    oscilation_data<-df_simulated_input_variables[which(df_simulated_input_variables$oscilation==oscilation),]
    
    # Take time data
    #oscilation_data$Time<-paste("Time_",oscilation_data$Time,sep="")
    
    # Set rownames
    #rownames(oscilation_data)<-oscilation_data$Time
    n=oscilation_data[,c("n")]
    Tm.i=oscilation_data[,c("Tm.i")]
    Tm.o=oscilation_data[,c("Tm.o")]
    P1=oscilation_data[,c("P1")]
    n_discrete<-cut(n, quantile(n, c(0:3/3)), include.lowest = T, labels = c("Low", "Medium", "High"))
    P1_discrete<-cut(P1, quantile(P1, c(0:3/3)), include.lowest = T, labels = c("Low", "Medium", "High"))

    # Take the median of the temperature
    median_Tm.i<-median(Tm.i)
    median_Tm.o<-median(Tm.o)

    # Take discrete values
    df_Tm<-data.frame(Tm.i=rep("Low",length(Tm.i)),Tm.o=rep("Low",length(Tm.o)))

    # Take the high values
    df_Tm[which(Tm.i>median_Tm.i),"Tm.i"]<-"High"
    df_Tm[which(Tm.o>median_Tm.o),"Tm.o"]<-"High"

    # Set the colnames
    colnames(df_Tm)<-c("Tm.i_discrete","Tm.o_discrete")
    
    # Renames collumns
    oscilation_data_tertiles<-data.frame(n_discrete=n_discrete,P1_discrete=P1_discrete,df_Tm)
    
    # Merge tables
    oscilation_data<-cbind(oscilation_data,oscilation_data_tertiles)    

    # add operational states
    oscilation_data$operational_states<-paste(paste("n=",oscilation_data$n_discrete,sep=""),paste("P1=",oscilation_data$P1_discrete,sep=""),paste("Tm.i=",oscilation_data$Tm.i_discrete,sep=""),paste("Tm.o=",oscilation_data$Tm.o_discrete,sep=""),sep="|")

    # Set Diagnosis
    oscilation_data$Gas_Locking <-"Normal"

    # If efficiency not stationary, then Diagnosis is fault
    oscilation_data[which(oscilation_data$n_discrete!="Low" & oscilation_data$Tm.i_discrete!="High" ),"Gas_Locking"]<-"Fault"

    # add data.frame with operational states and diagnosis
    df_simulated_input_variables_bck<-rbind(df_simulated_input_variables_bck,oscilation_data)
}

# Start data.frame with operational states
df_results_pheatmaps=data.frame(Q=c(),Tm.i=c(),Tm.o=c(),P1=c(),P2=c(),T=c(),pi=c(),mi=c(),mo=c(),decay=c(),n=c(),BHP=c(),H=c(),operational_states=c(),Gas_Locking=c())

# Plot the heatmap - all
for (decay in  unique(df_simulated_input_variables_bck$oscilation))
{
    # Take dec
    decay_data<-df_simulated_input_variables_bck[which(df_simulated_input_variables_bck$oscilation==oscilation),]

    # Remove row lines
    annotation_row_exp=decay_data[,c("n_discrete","P1_discrete","Tm.i_discrete","Gas_Locking")]

    # Re-set colnmaes
    colnames(annotation_row_exp)<-c("n","P1","Tm.i","Gas_Locking")

    # Specify colors
    ann_colors = list(n = c(Low="lightgrey", Medium="darkgrey",High="black"), P1 =  c(Low="lightgrey", Medium="darkgrey",High="black"), Tm.i = c(Low="lightgrey", High="black"),Gas_Locking=c(Normal="lightgrey", Fault="black") )

    # Normalized values for variables
    decay_data_normlized <- as.data.frame(lapply(decay_data[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo"),], normalize))

    # Normalized values for variables
    decay_data_tertile <- as.data.frame(lapply(decay_data[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo"),], tertile))

    # Set rownames
    rownames(decay_data_normlized)<-rownames(decay_data)
  
    # Melt tabele
    # Plot_raw_vibration_data.png                                                                                                            
    png(filename=paste(project_folder,"Gas_Locking_ESPsViscousFluidFlow_Pheatmap_simulated_",decay,".png",sep=""), width = 30, height = 30, res=600, units = "cm")  
      # Add annotation : bhp, head, efficiency
      pheatmap(decay_data_normlized , show_rownames = F,annotation_row = annotation_row_exp,annotation_colors=ann_colors,cluster_rows = FALSE, main=paste("decay",decay,sep=" = "))
    dev.off() 

    # add pheatmaps
    df_results_pheatmaps<-rbind(df_results_pheatmaps,cbind(decay_data_tertile,annotation_row_exp,decay=decay))
}
#####################################################################################################################




###############################################################################
# Load requioment
# Take the categories
df_reference_data<-df_simulated_input_variables[df_simulated_input_variables$oscilation=="reference",]

# Melt by Time and viscosity
melt_reference_data<-reshape2::melt(df_reference_data,id.vars=c("Time"))

# Conver to numeric
melt_reference_data$value<-as.numeric(melt_reference_data$value)

# Data.frame to store results
df_results<-data.frame(Var=c(),Type=c(),Time=c(),Values=c())

# The rows are increasing viscosity values and the collumns the increasing time value
# Convert the P47_viscous_3500_data_sub to time-series for each variable
# For each variable 
for (variable in c("n","P2"))
{
    # Convert the time series to a data frame
    # It is altready on a data.frame format
    # Check what frequency means in the ts means
    df_reference_data_ts <- ts(as.vector(df_reference_data[,c(variable)]))


    # Train an ARIMA model
    # The Autoregressive integrated moving average.
    # Autoregressive model : the observation j+1 is calculated baed on the observation j.
    # In other words, : a regression on past values to predict future values.
    # turns best ARIMA model according to either AIC, AICc or BIC value.
    arima_model      <- forecast::auto.arima(df_reference_data_ts,test="adf")

    # Forecast 12 months ahead
    # Number of periods for forecasting.
    forecast_result      <- forecast::forecast(arima_model, h = 50) 

}
