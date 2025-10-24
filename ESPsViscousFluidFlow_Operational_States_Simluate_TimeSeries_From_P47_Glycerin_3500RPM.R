# TO DO : try all with the 5800 points.

# This script will add a collumn to the metafile with the operational state
# The operational state will be defined as the n=state-h=state-bhp=state
merge_water_viscous_sub<-merge_water_viscous[which(merge_water_viscous$equip=="P47" & merge_water_viscous$fluid == "Glycerin" & merge_water_viscous$RPM=="3500" & merge_water_viscous$Inlet.Viscosity == "128"),]

# Add Time to the variable
####################################################################################################################################################################################
# The rows are increasing viscosity values and the collumns the increasing time value
# Convert the P47_viscous_3500_data_sub to time-series for each variable
# For each variable 
for (variable in c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo"))
{
    # Save data.frame as ts
    P47_viscous_3500_data_ts<-merge_water_viscous_sub[which(merge_water_viscous_sub$variable==variable),c("Time","value","Viscosity")]

    # Convert the time series to a data frame
    # It is altready on a data.frame format
    # Check what frequency means in the ts means
    P47_viscous_3500_data_ts <- ts(P47_viscous_3500_data_ts$value)

    ##########################################################################################
    # Convert the time series to a data frame
    # It is altready on a data.frame format
    # Check what frequency means in the ts means
    P47_viscous_3500_data_sel <- ts(as.vector(P47_viscous_3500_data_ts))

    # turns best ARIMA model according to either AIC, AICc or BIC value.
    arima_model_sub <- forecast::auto.arima(P47_viscous_3500_data_sel)
  }
  ####################################################################################################################################################################################
