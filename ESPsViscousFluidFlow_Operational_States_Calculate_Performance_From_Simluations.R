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

#######################################################################################################
# add plot
melt_simulated_input_variables<-reshape2::melt(df_simulated_input_variables,id.vars=c("Time","decay")) 

# Most basic bubble plot
p3 <- ggplot(melt_simulated_input_variables[melt_simulated_input_variables$variable %in% c("n","H","BHP"),], aes(x=Time, y=value,group = decay, color = decay)) +  geom_line() +   facet_grid(rows = vars(variable),scales="free") + theme_bw()  + ggtitle ("Random forest predicted time-series - performance variables") + scale_colour_brewer(palette = "Set1")

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"Simulated_performance_variables_ranfom_forest.png",sep=""), width = 15, height = 10, res=600, units = "cm")  
  p3
dev.off()
#######################################################################################################


################################################################################################################
# Fig. 7—ESP P47 performance pumping viscous fluid at 3,500 rev/min.
ESP_P47_water_plot_Q_H <- ggplot(df_simulated_input_variables[,c("Q","n","H","BHP","decay")], aes(x = Q, y = H,colour=decay))     + geom_point() + geom_line()  + theme_bw()   + ggtitle ("Flow rate Q vs. Head H")    + ylab("Head H [m]")                   + labs(x = expression("Flow rate Q [" * m^3/h * "]")) + theme(legend.position = "bottom")  
ESP_P47_water_plot_BHP <- ggplot(df_simulated_input_variables[,c("Q","n","H","BHP","decay")], aes(x = Q, y = BHP,colour=decay))   + geom_point() + geom_line() + theme_bw()   + ggtitle ("Flow rate Q vs. Power BHP") + ylab("Power BHP [W]")                + labs(x = expression("Flow rate Q [" * m^3/h * "]")) + theme(legend.position = "none")    
ESP_P47_water_plot_n   <- ggplot(df_simulated_input_variables[,c("Q","n","H","BHP","decay")], aes(x = Q, y = n*100,colour=decay)) + geom_point() + geom_line() + theme_bw()   + ggtitle ("Flow rate Q vs. Efficiency n") + ylab("Efficiency n [%]")          + labs(x = expression("Flow rate Q [" * m^3/h * "]"))   + theme(legend.position = "bottom")      

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"ESP_P47_dilluted_glucerin_Operational_states.png",sep=""), width = 20, height = 25, res=600, units = "cm")  
  ggarrange(ESP_P47_water_plot_Q_H,ESP_P47_water_plot_BHP,ESP_P47_water_plot_n, nrow =3,common.legend = TRUE,legend="bottom")
dev.off()
#######################################################################################################
# Simulations of Well Sanding (Pump Plugging).
# First, simulate each variable in function of Q
# Start df with the results
df_results<-data.frame(Time=c(), Q=c(), Tm.i=c(), Tm.o=c(), P1=c(), P2=c(), T=c(), pi=c(), mi=c(), mo=c(), RPM=c(), decay=c(), P_h=c(), n=c(), H=c(), BHP=c(), Delta.Pressure=c())

for (decay in unique(df_simulated_input_variables$decay))
{
  # Take dec
  decay_data<-df_simulated_input_variables[which(df_simulated_input_variables$decay==decay),]

  # Take the tertiles
  decay_data_tertiles<-as.data.frame(lapply(decay_data[,c("n","BHP","H")], tertile))

  # 
  colnames(decay_data_tertiles)<-c("n_discrete","BHP_discrete","H_discrete")
  
  # Merge tables
  decay_data<-cbind(decay_data,decay_data_tertiles)  
   
  # add operational states
  decay_data$operational_states<-paste(paste("n=",decay_data$n_discrete,sep=""),paste("BHP=",decay_data$BHP_discrete,sep=""),paste("H=",decay_data$H_discrete,sep=""),sep="|")

  # Assert diagnosis and classification equal to normal 
  decay_data<-cbind(decay_data,Diagnosis="Normal")
  
  # If efficiency not stationary, then Diagnosis is fault
  decay_data[which(decay_data$n_discrete!="High"),"Diagnosis"]<-"Fault"

  # bind results
  df_results<-rbind(df_results,decay_data)
}
##############################################################################################################################################################################################
# Plot the heatmap - only the reference
normalized_merge_water_viscous<-df_results[which(df_results$decay=="reference"),]


# Subset the 
normalized_merge_water_viscous<-normalized_merge_water_viscous[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo","n_discrete","BHP_discrete","H_discrete","operational_states", "Diagnosis")]

# Force rownames
rownames(normalized_merge_water_viscous)<-paste0("Signal_", seq(nrow(normalized_merge_water_viscous)))

# Remove row lines
annotation_row_exp=cbind(normalized_merge_water_viscous[,c("n_discrete","BHP_discrete","H_discrete")],normalized_merge_water_viscous[,c("operational_states","Diagnosis")])

# Set colnames
colnames(annotation_row_exp)<-c("n_discrete","BHP_discrete","H_discrete","operational_states","Diagnosis")
######################################################################################################################
# Specify colors
ann_colors = list(n = c(Low="lightgrey", Medium="darkgrey",High="black"), BHP = c(Low="lightgrey", Medium="darkgrey",High="black"), H = c(Low="lightgrey", Medium="darkgrey",High="black") )

# Normalized values for variables
normalized_merge_water_viscous<-cbind(as.data.frame(lapply(normalized_merge_water_viscous[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo")], normalize)),normalized_merge_water_viscous[,c("n_discrete","BHP_discrete","H_discrete","operational_states", "Diagnosis")])
######################################################################################################################
# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"ESPsViscousFluidFlow_Pheatmap.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  # Add annotation : bhp, head, efficiency
  pheatmap(normalized_merge_water_viscous , show_rownames = T,annotation_row = annotation_row_exp,annotation_colors=ann_colors,cluster_rows = FALSE, cluster_cows = FALSE.,main=paste("P47 3500RPM Glycerin Viscosity 128"))
dev.off()
#######################################################################################################
# Plot the heatmap - all
for (decay in df_simulated_input_variables$decay)
{
    # Take dec
    decay_data<-df_simulated_input_variables[which(df_simulated_input_variables$decay==decay),]

    # Take only performance variables
    decay_data_performance<-decay_data[,c("n","H","BHP")]

    # Take the tertiles
    decay_data_performance<-as.data.frame(lapply(decay_data_performance[,c("n","BHP","H")], tertile))  

    # Renames collumns
    colnames(decay_data_performance)<-c("n_discrete","BHP_discrete","H_discrete")

    # Take time data
    decay_data$Time<-paste("Time_",decay_data$Time,sep="")
    
    # Merge tables
    decay_data<-cbind(decay_data[,c("Time","Q", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo", "RPM")],decay_data_performance)

    # add operational states
    decay_data$operational_states<-paste(paste("n=",decay_data$n_discrete,sep=""),paste("BHP=",decay_data$BHP_discrete,sep=""),paste("H=",decay_data$H_discrete,sep=""),sep="|")

    # Assert diagnosis and classification equal to normal 
    decay_data<-cbind(decay_data,Diagnosis="normal")
    
    # If efficiency not stationary, then Diagnosis is fault
    decay_data[which(decay_data$n_discrete!="High"),"Diagnosis"]<-"fault"

    # Set rownames
    rownames(decay_data)<-decay_data$Time

    # Remove row lines
    annotation_row_exp=decay_data[,c("n_discrete","BHP_discrete","H_discrete","operational_states","Diagnosis")]

    # Re-set colnmaes
    colnames(annotation_row_exp)<-c("n","BHP","H","operational_states","Diagnosis")

    # Specify colors
    ann_colors = list(n = c(Low="lightgrey", Medium="darkgrey",High="black"), BHP = c(Low="lightgrey", Medium="darkgrey",High="black"), H = c(Low="lightgrey", Medium="darkgrey",High="black") )

    paste("decay",decay,sep="")
    # Melt tabele
    # Plot_raw_vibration_data.png                                                                                                            
    png(filename=paste(project_folder,"ESPsViscousFluidFlow_Pheatmap_simulated_",decay,".png",sep=""), width = 30, height = 30, res=600, units = "cm")  
      # Add annotation : bhp, head, efficiency
      pheatmap(decay_data[,c("Q", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo", "RPM")] , show_rownames = F,annotation_row = annotation_row_exp,annotation_colors=ann_colors,cluster_rows = FALSE, main=paste("decay",decay,sep=" = "),scale ="column")
    dev.off()
}

