######################################################################################################################
# Set the parameters
# Define the lenght of the time-windnw
window_length<-20

# Define the lenght of the time-windnw
stride        <-5
######################################################################################################################
# First scan the P47, RPM300, Viscosity 128, Glycerin
# Take the table for Q versus efficiency
merge_water_viscous_sub_Q_n<-merge_water_viscous_sub[,c("Q","n","operational_states")]

# Rename collumns
colnames(merge_water_viscous_sub_Q_n)<-c("Q","n","operational_states_points")

# Add collumn 
merge_water_viscous_sub_Q_n<-cbind(merge_water_viscous_sub_Q_n,Window=-1)

# Add time collumns
merge_water_viscous_sub_Q_n<-cbind(merge_water_viscous_sub_Q_n,Time=1:dim(merge_water_viscous_sub_Q_n)[1])
######################################################################################################################

rollapply(x, width = window_length, by = stride, mean)
