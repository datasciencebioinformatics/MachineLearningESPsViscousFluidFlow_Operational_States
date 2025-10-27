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
      df_predicted_results<-rbind(df_predicted_results,data.frame(Time=merge_water_viscous_testing$Time,value=rf_variable_versus_prediction,variable=variable,decay=decay_rate))

      # Add results of the variable
      df_predicted_results<-rbind(df_predicted_results,data.frame(Time=merge_water_viscous_testing$Time,value=sim_data[sim_data$decay_rate==decay_rate,"Noisy_Value"],variable="Q",decay=decay_rate))
  }
}
####################################################################################################################################################################################
# Add also simulated data
####################################################################################################################################################################################
# Relevel factors
df_predicted_results$variable<-factor(df_predicted_results$variable,levels=c(c("Q","RPM", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo")))

# Relevel factors
df_predicted_results$decay<-factor(df_predicted_results$decay)

# Most basic bubble plot
p2 <- ggplot(df_predicted_results, aes(x=Time, y=value,group = decay, color = decay)) +  geom_line() +   facet_grid(rows = vars(variable),scales="free") + theme_bw()  + ggtitle ("Random forest predicted time-series") + scale_colour_brewer(palette = "Set1")

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"Simulated_time_series_ranfom_forest.png",sep=""), width = 15, height = 20, res=600, units = "cm")  
  p2
dev.off()


####################################################################################################################################################################################
# Calculate performance, head and BHP for each configurations.
####################################################################################################################################################################################
Q    <- df_predicted_results[which(df_predicted_results$variable=="Q"),"value"]
Tm.i <- df_predicted_results[which(df_predicted_results$variable=="Tm.i"),"value"]
Tm.i <- df_predicted_results[which(df_predicted_results$variable=="Tm.o"),"value"]
P1   <-df_predicted_results[which(df_predicted_results$variable=="P1"),"value"]
P2   <-df_predicted_results[which(df_predicted_results$variable=="P2"),"value"]
T    <-df_predicted_results[which(df_predicted_results$variable=="T"),"value"]
pi   <- df_predicted_results[which(df_predicted_results$variable=="pi"),"value"]
mi   <-  df_predicted_results[which(df_predicted_results$variable=="mi"),"value"]
mo   <- df_predicted_results[which(df_predicted_results$variable=="mo"),"value"]
RPM  <- df_predicted_results[which(df_predicted_results$variable=="mo"),"RPM"]
Time <- unique(df_predicted_results$Time)

data.frame(Time=Time=)

# set  the gravitational constant
# 9.81 meters per second squared (m/s2) is the approximate value of the acceleration due to gravity on Earth's surface. This value is represented by the letter g. 
g=9.81

# Number of stages (constant equanto to 3)
N   <-3 

# Store the useful power Ph
df_predicted_results$P_h<-0

# The pump efficiency (n) is defined as:
# n = efficiency, dimensionless [%]
df_predicted_results$n<-0

# Store the pump head
# H = head, L, m
df_predicted_results$H<-0

# Store the shaft torque is the mechanical parameter used to calculate the driving power or brake horsepower (BHP)
# BHP = mL^2t^–3, watts
df_predicted_results$BHP<-0

# Store the flow rate in m3s1
# Q = volumetric flow rate, L3 t–1 , m3/h
df_predicted_results$Q<-0

# Compute the delta pressure
df_predicted_results$Delta.Pressure<-df_predicted_results$P2-df_predicted_results$Inlet.Pressure.P1






ESPsViscousFluidFlow_Operational_States_Calculate_Performance_From_Simluations
