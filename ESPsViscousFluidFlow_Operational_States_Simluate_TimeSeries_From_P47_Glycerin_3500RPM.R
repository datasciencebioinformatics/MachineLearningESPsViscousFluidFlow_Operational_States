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

# Mett data.frame
melt_water_viscous_sub<-reshape2::melt(merge_water_viscous_sub[,c("Time","Q", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo", "RPM", "n", "H", "BHP")],id.vars=c("Time"))

# Most basic bubble plot
p <- ggplot(melt_water_viscous_sub, aes(x=Time, y=value)) +  geom_line() +   facet_grid(rows = vars(variable),scales="free") + theme_bw()  + ggtitle ("Reference time-series")

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"Reference_time_series.png",sep=""), width = 20, height = 30, res=600, units = "cm")  
  p
dev.off()
####################################################################################################################################################################################
# The rows are increasing viscosity values and the collumns the increasing time value
# Convert the P47_viscous_3500_data_sub to time-series for each variable
# For each variable 
for (variable in c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo"))
{
    # Save data.frame as ts
    P47_viscous_3500_data_ts<-merge_water_viscous_sub[,c("Time",variable)]

    # Set colnames
    colnames(P47_viscous_3500_data_ts)<-c("Time","value")

    # Convert the time series to a data frame
    # It is altready on a data.frame format
    # Check what frequency means in the ts means
    P47_viscous_3500_data_ts <- ts(P47_viscous_3500_data_ts$value)

    ##########################################################################################

    # turns best ARIMA model according to either AIC, AICc or BIC value.
    arima_model_sub <- forecast::auto.arima(P47_viscous_3500_data_sel)
  }
  ####################################################################################################################################################################################
