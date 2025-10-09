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
# Take the number of slliding windows
number_slidding_windows<-length(rollapply(merge_water_viscous_sub_Q_n$n, width = window_length, by = stride, mean))

mean_efficiency_values              <-rollapply(merge_water_viscous_sub_Q_n$n, width = window_length, by = stride, mean)
sd_efficiency_values                <-rollapply(merge_water_viscous_sub_Q_n$n, width = window_length, by = stride, sd)
ADF_pvalue_efficiency_values        <-as.vector(unlist(rollapply(merge_water_viscous_sub_Q_n$n, width = window_length, by = stride, adf.test)[,4]))
ADF_Dickey_Fuller_efficiency_values <-as.vector(unlist(rollapply(merge_water_viscous_sub_Q_n$n, width = window_length, by = stride, adf.test)[,1]))
ADF_Dickey_DF_values                <-as.vector(unlist(rollapply(merge_water_viscous_sub_Q_n$n, width = window_length, by = stride, adf.test)[,2]))
ADF_stationairty                    <-rep(FALSE, each=number_slidding_windows)

# Concatenate title
Ljung_Box_Xsquared      <-as.vector(unlist(rollapply(merge_water_viscous_sub_Q_n$n, width = window_length, by = stride, LjungBox)[,1]))
Ljung_Box_df            <-as.vector(unlist(rollapply(merge_water_viscous_sub_Q_n$n, width = window_length, by = stride, LjungBox)[,2]))
Ljung_Box_pvalue        <-as.vector(unlist(rollapply(merge_water_viscous_sub_Q_n$n, width = window_length, by = stride, LjungBox)[,3]))
Ljung_Box_whitenoise    <-rep(FALSE, each=number_slidding_windows)

# Test for white noise
Ljung_Box_whitenoise[which(Ljung_Box_pvalue>0.05)]<-TRUE

# Test for stationarity
ADF_stationairty[which(ADF_pvalue_efficiency_values<=0.05)]<-TRUE

# Take the number of slidding windows
slidding_windows<-1:number_slidding_windows
######################################################################################################################
# Compile results table
df_slidding_windows<-data.frame(sliddingWindows=slidding_windows, mean_n=mean_efficiency_values, sd_n=sd_efficiency_values, ADF_pvalue=ADF_pvalue_efficiency_values, ADF_Dickey_Fuller=ADF_Dickey_Fuller_efficiency_values, ADF_Dickey_DF=ADF_Dickey_DF_values,ADF_stationairty=ADF_stationairty,Ljung_Box_Xsquared=Ljung_Box_Xsquared,Ljung_Box_df=Ljung_Box_df,Ljung_Box_pvalue=Ljung_Box_pvalue,Ljung_Box_whitenoise=Ljung_Box_whitenoise)

