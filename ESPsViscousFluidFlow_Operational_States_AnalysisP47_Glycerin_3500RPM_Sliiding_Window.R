######################################################################################################################
# Set the parameters
# Define the lenght of the time-windnw
window_length<-10

# Define the lenght of the time-windnw
stride        <-5
######################################################################################################################
# First scan the P47, RPM300, Viscosity 128, Glycerin
# Take the table for Q versus efficiency
merge_water_viscous_sub_Q_n<-merge_water_viscous_sub[,c("Q","n","operational_states","Diagnosis")]

# Add time collumns
merge_water_viscous_sub_Q_n<-cbind(merge_water_viscous_sub_Q_n,datapoint=1:dim(merge_water_viscous_sub_Q_n)[1])
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
######################################################################################################################
# Copile table with the slidding_windows of every datapoint position
starting_positition<- 1
ending_positition  <- dim(merge_water_viscous_sub_Q_n)[1]-window_length

# Stridfes move
slidding_window<-1

# Startt data.frame
df_slidding_position<-data.frame(sliddingWindows=c(),datapoint=c())

# Start starting_positition and move by step of stride
for (index in seq(starting_positition,ending_positition,by=stride))
{
  # Add to the data.frame
  df_slidding_position<-rbind(df_slidding_position,data.frame(sliddingWindows=slidding_window,datapoint=seq(index,index+window_length,by=1)))

  # Increment the counter
  slidding_window<-slidding_window+1  
}
# Merge the data.frame
merged_slidding_window<-merge(df_slidding_position,df_slidding_windows,by="sliddingWindows")

# Merge also with the time points
merged_slidding_window<-merge(merged_slidding_window,merge_water_viscous_sub_Q_n,by="datapoint")

# Take the numeric values
merged_slidding_window$stationairty<-as.numeric(merged_slidding_window$ADF_stationairty)
merged_slidding_window$Ljung_Box_whitenoise<-as.numeric(merged_slidding_window$Ljung_Box_whitenoise)
merged_slidding_window$operationalstates<-as.numeric(as.factor(merged_slidding_window$operational_states))
merged_slidding_window$diagnosis<-as.numeric(as.factor(merged_slidding_window$Diagnosis))
#####################################################################################################################
# Generate the melt table
melt_slidding_window<-reshape2::melt(merged_slidding_window,id.vars=c("datapoint"))
######################################################################################################################
# Plot time-series with mean and average
# Plot also panel wtih statistics
# Save the plot with mean
p1<-ggplot(merged_slidding_window, aes(x = datapoint)) +
      geom_line(aes(y = n), color = "black", alpha = 0.6) +
      geom_line(aes(y = mean_n), color = "blue", size = 1) +
      geom_ribbon(aes(ymin = mean_n - sd_n, ymax = mean_n + sd_n), fill = "red", alpha = 0.2) + theme_bw() + ggtitle ("Rollapply(mean) with=10 stride=5, mean±sd")


# Process the stationarity variables
melt_adf_results<-reshape2::melt(merged_slidding_window[,c("ADF_Dickey_Fuller","ADF_pvalue","ADF_Dickey_DF","stationairty","datapoint")],id.vars=c("datapoint","stationairty"))

# Plot the stationairy values
p2<-ggplot(melt_adf_results, aes(datapoint, value)) + geom_line() + geom_point(aes(color=factor(stationairty))) + facet_wrap(vars(variable)) +  theme_bw() + ggtitle("ADF test") + theme(legend.position="bottom") 

# Process the Ljung-box variables
melt_ljung_results<-reshape2::melt(merged_slidding_window[,c("Ljung_Box_Xsquared", "Ljung_Box_df", "Ljung_Box_pvalue", "Ljung_Box_whitenoise","datapoint")],id.vars=c("datapoint","Ljung_Box_whitenoise"))

# Plot the stationairy values
p3<-ggplot(melt_ljung_results, aes(datapoint, value)) + geom_line() + geom_point(aes(color=factor(Ljung_Box_whitenoise))) + facet_wrap(vars(variable)) +  theme_bw() + ggtitle("Ljung-box test") + theme(legend.position="bottom") 



# Plot the stationairy values               
png(filename=paste(output_dir,"Efficiency_rollapply_mean_sd.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  # Plot the bayesian network graph
  p1
dev.off()

# Process the Ljung-box variables       
png(filename=paste(output_dir,"Efficiency_rollapply_ADF.png",sep=""), width = 30, height = 15, res=600, units = "cm")  
  # Plot the bayesian network graph
  p2
dev.off()

# Process the Ljung-box variables       
png(filename=paste(output_dir,"Efficiency_rollapply_Ljung.png",sep=""), width = 30, height = 15, res=600, units = "cm")  
  # Plot the bayesian network graph
  p3
dev.off()







      



