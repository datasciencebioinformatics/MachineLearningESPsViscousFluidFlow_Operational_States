######################################################################################################################
# Set the parameters
# Define the lenght of the time-windnw
window_length<-10

# Define the lenght of the time-windnw
stride        <-5
######################################################################################################################
# First scan the P47, RPM300, Viscosity 128, Glycerin
# Take the table for Q versus efficiency
merge_water_viscous_sub_Q_n<-merge_water_viscous_sub_bck[,c("Q","n","operational_states","Diagnosis")]

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


# Make copyy
merged_slidding_window_experimental<-merged_slidding_window
#######################################################################################################################
# Take only the time-series 11
# Take the table for the corresponding time-series
simulated_data_sub<-simulated_data_all[simulated_data_all[,c("Series")]==10,]

######################################################################################################################
# First scan the P47, RPM300, Viscosity 128, Glycerin
# Take the table for Q versus efficiency
merge_water_viscous_sub_Q_n<-simulated_data_sub[,c("Q","n","operational_states","diagnosis")]

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
merged_slidding_window$diagnosis<-as.numeric(as.factor(merged_slidding_window$diagnosis))
#####################################################################################################################
# Make copyy
merged_slidding_window_series_11<-merged_slidding_window
#####################################################################################################################
# Set the colnames from experimental from simulated time series
vars_A<-c("Q","Average.Inlet.Temp.Tm.i","Average.Outlet.Temp.Tm.o","Inlet.Pressure.P1","Outlet.Pressure.P2","Shaft.Torque","Inlet.Density.ρi","Inlet.Viscosity.mi","Outlet.Viscosity.mo","n","H","BHP","operational_states","Diagnosis","Time","Series")
vars_B<-c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo","n","H","BHP","operational_states","Diagnosis","Time","Series")
#####################################################################################################################
# Experimental and simulated time-series
merged_slidding_window_experimental
merged_slidding_window_series_11

# Copy table
merge_water_viscous_sub<-merge_water_viscous_sub_bck

# Add Time collumns
merge_water_viscous_sub$Time<-1:dim(merge_water_viscous_sub)[1]

# Add Series collumns
merge_water_viscous_sub$Series<-0

# Merge water viscous sub
merge_water_viscous_sub<-merge_water_viscous_sub[,vars_A]

# Subset collumns
simulated_data_sub<-simulated_data_sub[,vars_B]

# Adjust colnames
colnames(merge_water_viscous_sub)<-colnames(simulated_data_sub)

#####################################################################################################################
# Take only series 11
selected_simulates_series<-11

# Subseect series
simulated_data_sub<-simulated_data_sub[which(simulated_data_sub$Series==selected_simulates_series),]
#####################################################################################################################
# Normalized values for variables
normalized_merge_water_viscous_sub <- as.data.frame(lapply(merge_water_viscous_sub[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo","n","BHP","H"),], normalize))

# Normalized values for variables
normalized_simulated_data_sub      <- simulated_data_sub[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo","n","BHP","H"),]
#####################################################################################################################
# Add discrete values of n, BHP, H - Experimental data
normalized_merge_water_viscous_sub$n<-cut(normalized_merge_water_viscous_sub$n, quantile(normalized_merge_water_viscous_sub$n, c(0:3/3)), include.lowest = T, labels = c("Low", "Medium", "High"))
normalized_merge_water_viscous_sub$BHP<-cut(normalized_merge_water_viscous_sub$BHP, quantile(normalized_merge_water_viscous_sub$BHP, c(0:3/3)), include.lowest = T, labels = c("Low", "Medium", "High"))
normalized_merge_water_viscous_sub$H<-cut(normalized_merge_water_viscous_sub$H, quantile(normalized_merge_water_viscous_sub$H, c(0:3/3)), include.lowest = T, labels = c("Low", "Medium", "High"))
#####################################################################################################################
# Add discrete values of n, BHP, H - Simulated data
normalized_simulated_data_sub$n<-cut(normalized_simulated_data_sub$n, quantile(normalized_simulated_data_sub$n, c(0:3/3)), include.lowest = T, labels = c("Low", "Medium", "High"))
normalized_simulated_data_sub$BHP<-cut(normalized_simulated_data_sub$BHP, quantile(normalized_simulated_data_sub$BHP, c(0:3/3)), include.lowest = T, labels = c("Low", "Medium", "High"))
normalized_simulated_data_sub$H<-cut(normalized_simulated_data_sub$H, quantile(normalized_simulated_data_sub$H, c(0:3/3)), include.lowest = T, labels = c("Low", "Medium", "High"))
####################################################################################################################
# Specifying clustering from distance matrix - Experimental
normalized_dist_viscous_exp = dist(normalized_merge_water_viscous_sub[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo")])
normalized_dcols_viscous_exp = dist(t(normalized_merge_water_viscous_sub[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo")]))

# Specifying clustering from distance matrix - Simulated
normalized_dist_viscous_sim = dist(normalized_simulated_data_sub[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo")])
normalized_dcols_viscous_sim = dist(t(normalized_simulated_data_sub[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo")]))
######################################################################################################################
# kmeans
kmeans_clusters_exp<-c(kmeans(normalized_merge_water_viscous_sub[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo")], centers=6, iter.max = 10, nstart = 1, trace = FALSE)$cluster)
kmeans_clusters_sim<-c(kmeans(normalized_simulated_data_sub[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo")], centers=6, iter.max = 10, nstart = 1, trace = FALSE)$cluster)
######################################################################################################################
# Subset the 
df_normalized_merge_exp<-normalized_merge_water_viscous_sub[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo","n","BHP","H")]
df_normalized_merge_sim<-normalized_simulated_data_sub[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo","n","BHP","H")]

# Force rownames
rownames(df_normalized_merge_exp)<-paste0("Time_", seq(nrow(df_normalized_merge_exp)))
rownames(df_normalized_merge_sim)<-paste0("Time_", seq(nrow(df_normalized_merge_sim)))

# Remove row lines
# Add k-means
annotation_row_exp=df_normalized_merge_exp[,c("n","BHP","H")]
annotation_row_sim=df_normalized_merge_sim[,c("n","BHP","H")]

# Set the k-means clusters
annotation_row_exp$Kmeans<-as.factor(kmeans_clusters_exp)
annotation_row_sim$Kmeans<-as.factor(kmeans_clusters_sim)
######################################################################################################################
# Specify colors
ann_colors = list(n = c(Low="lightgrey", Medium="darkgrey",High="black"), BHP = c(Low="lightgrey", Medium="darkgrey",High="black"), H = c(Low="lightgrey", Medium="darkgrey",High="black"),Kmeans = c("#DF536B","#61D04F","#2297E6", "#28E2E5","#CD0BBC", "#F5C710" ) )

# Set the name for the k-means
names(ann_colors$Kmeans)<-c("1","2","3","4","5","6")

order_rows_exp<-rownames(normalized_merge_water_viscous_sub[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo")])[order(kmeans_clusters_exp)]
order_rows_sim<-rownames(normalized_simulated_data_sub[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo")])[order(kmeans_clusters_sim)]

df_normalized_merge_exp<-df_normalized_merge_exp[as.integer(order_rows_exp),]
df_normalized_merge_sim<-df_normalized_merge_sim[as.integer(order_rows_sim),]
######################################################################################################################
# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"ESPsViscousFluidFlow_Pheatmap.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  # Add annotation : bhp, head, efficiency
  pheatmap(df_normalized_merge_exp[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo")] , clustering_distance_cols = normalized_dist_viscous_exp,show_rownames = F,annotation_row = annotation_row_exp,annotation_colors=ann_colors,cluster_rows = FALSE)
dev.off()

