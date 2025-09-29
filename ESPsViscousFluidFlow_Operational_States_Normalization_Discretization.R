#########################################################################################################
# Function to normalize the data
normalize <- function(x) 
{
	return ((x - min(x)) / (max(x) - min(x)))
}
#########################################################################################################
# Function to calculate tertile
tertile <- function(x)
{  
  return (cut(x, quantile(x, c(0:3/3)), include.lowest = T, labels = c("Low", "Medium", "High")))
}
#########################################################################################################
# Store nome of analyzed variables
# Flow rate	Inlet Temperature T1 	Inlet Temperature T2 			Inlet Pressure P1	Outlet Pressure P2	Shaft Torque
# TO DO : 
variables<-c("Q","Average.Inlet.Temp.Tm.i","Average.Outlet.Temp.Tm.o","Inlet.Pressure.P1","Outlet.Pressure.P2","RPM","Shaft.Torque","Inlet.Density.ρi","Inlet.Viscosity.mi","Outlet.Viscosity.mo","n","BHP","H")

# Sub-select collumns
subselect_merge_water_viscous<-na.omit(merge_water_viscous[,variables])

# Convert RPM to numeric
subselect_merge_water_viscous$RPM<-as.numeric(subselect_merge_water_viscous$RPM)

# Normalized values for variables
normalized_merge_water_viscous <- as.data.frame(lapply(subselect_merge_water_viscous, normalize))
#########################################################################################################
# Normalized values for variables
discrete_merge_water_viscous <- as.data.frame(lapply(subselect_merge_water_viscous, tertile))

# Set the values of RPM as the original values of RPM
discrete_merge_water_viscous$RPM<-subselect_merge_water_viscous$RPM
#########################################################################################################
# Last table, already filtered values
# Sub-select collumns
subselect_merge_water_viscous<-na.omit(merge_water_viscous[,variables])
#########################################################################################################
subselect_merge_water_viscous<-as.data.frame(sapply(subselect_merge_water_viscous, as.numeric))
#########################################################################################################
