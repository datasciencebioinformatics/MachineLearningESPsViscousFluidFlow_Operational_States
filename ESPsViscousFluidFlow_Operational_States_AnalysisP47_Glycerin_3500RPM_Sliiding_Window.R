######################################################################################################################
# Set the parameters
# Define the lenght of the time-windnw
window_length<-10

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
mean_efficiency_values              <-rollapply(merge_water_viscous_sub_Q_n$n, width = window_length, by = stride, mean)
sd_efficiency_values                <-rollapply(merge_water_viscous_sub_Q_n$n, width = window_length, by = stride, sd)
ADF_pvalue_efficiency_values        <-as.vector(unlist(rollapply(merge_water_viscous_sub_Q_n$n, width = window_length, by = stride, adf.test)[,4]))
ADF_Dickey_Fuller_efficiency_values <-as.vector(unlist(rollapply(merge_water_viscous_sub_Q_n$n, width = window_length, by = stride, adf.test)[,1]))
ADF_Dickey_DF_values                <-as.vector(unlist(rollapply(merge_water_viscous_sub_Q_n$n, width = window_length, by = stride, adf.test)[,2]))


