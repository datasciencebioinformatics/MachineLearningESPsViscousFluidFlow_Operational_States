######################################################################################################################
# Define the lenght of the time-windnw
time_window_length<-10

# Define data.frame for the results of time-series
df_results_P47_3500RPM_Viscosity_128_Glycerin<-data.frame(Window=c(),   mean_n=c(),   sd_n=c(),      operational_states=c(),     diagnosis=c(), adf_Dickey_Fuller=c(), adf_df=c(), adf_pvalue=c() , adf_stationarity=c(), Ljung_Box_Xsquared=c(), Ljung_Box_df=c(), Ljung_Box_pvalue=c(), Ljung_Box_whitenoise=c(),series=c())

# First scan the P47, RPM300, Viscosity 128, Glycerin
# Take the table for Q versus efficiency
merge_water_viscous_sub_Q_n<-merge_water_viscous_sub[,c("Q","n","operational_states")]

# Rename collumns
colnames(merge_water_viscous_sub_Q_n)<-c("Q","n","operational_states_points")

# Sort data.frame by Q
merge_water_viscous_sub_Q_n <- merge_water_viscous_sub_Q_n[order(merge_water_viscous_sub_Q_n$Q), ]

# Add collumn 
merge_water_viscous_sub_Q_n<-cbind(merge_water_viscous_sub_Q_n,Window=-1)

# If I have X data points, and the window length is l
# then I have X/l windows
# Perform sliding window analysis
# First, split the vector into chuncs
time_windows<-split(merge_water_viscous_sub_Q_n$n, ceiling(seq_along(merge_water_viscous_sub_Q_n$n)/time_window_length))

# Then, for each time-series
for (time_window in names(time_windows))
{
    # Take only the plots of this series
    data_points<-time_windows[[time_window]]

    # Set the time window in the merge_water_viscous_sub_Q_n tabe
    merge_water_viscous_sub_Q_n[which(merge_water_viscous_sub_Q_n$n %in% data_points),"Window"]<-time_window
    
    # Calculate the mean and sd of Efficiency 
    mean_n=mean(data_points)
  
    # Calculate the mean and sd of Efficiency 
    sd_n=sd(data_points)
  
    # Store also the operational states in a single variable
    operational_states<-paste(unique(merge_water_viscous_sub[which(merge_water_viscous_sub$n %in% data_points),"operational_states"]),collapse = ",")
  
    # Store also the diagnosis in a single variable
    diagnosis<-paste(unique(merge_water_viscous_sub[which(merge_water_viscous_sub$n %in% data_points),"Diagnosis"]),collapse = ",")

    # Concatenate title
    Ljung_Box_Xsquared      <-NA
    Ljung_Box_df            <-NA
    Ljung_Box_pvalue        <-NA
    Ljung_Box_whitenoise    <-NA
      
    # Perform Ljung-Box test
    box.test<-Box.test(data_points, lag = 1, type = "Ljung-Box")

    # If pvalue is not nan
    if(!is.na(box.test$p.value))
    {
        # Concatenate title
        Ljung_Box_Xsquared      <-round(box.test$statistic,3)
        Ljung_Box_df            <-round(box.test$parameter,3)
        Ljung_Box_pvalue        <-round(box.test$p.value,3)
        Ljung_Box_whitenoise    <-FALSE

        # If p-value smaller than 0.05 than set 
        if (Ljung_Box_pvalue > 0.05)
        {
            # Set stationarity to TRUE
            Ljung_Box_whitenoise <-TRUE
        }
    }    
    
    # Concatenate title
    adf_Dickey_Fuller       <-NA
    adf_df                  <-NA
    adf_pvalue              <-NA
    adf_stationarity        <-FALSE
    
    # If at least two data.points
    if (length(data_points)>1)
    {  
        # Store results of adf test
        adf_test<-adf.test(data_points)
        
        # Concatenate title
        adf_Dickey_Fuller       <-adf_test[[1]]
        adf_df                  <-adf_test[[2]]      
        adf_pvalue              <-adf_test[[4]]
        
        # If pvalue is not nan
        if(!is.nan(adf_pvalue))
        {
            # If p-value smaller than 0.05 than set 
            if (adf_pvalue <= 0.05)
            {
            # Set stationarity to TRUE
            adf_stationarity <-TRUE
            }
        }
    
      # If p-value smaller than 0.05 than set 
      if (adf_pvalue <= 0.05)
      {
          # Set stationarity to TRUE
          adf_stationarity <-TRUE
      }
    }
    # df_results
    df_results<-data.frame(Window=time_window,   mean_n=mean_n,   sd_n=sd_n,      operational_states=operational_states,      diagnosis=diagnosis, adf_Dickey_Fuller=adf_Dickey_Fuller, adf_df=adf_df, adf_pvalue=adf_pvalue , adf_stationarity=adf_stationarity, Ljung_Box_Xsquared=Ljung_Box_Xsquared, Ljung_Box_df=Ljung_Box_df, Ljung_Box_pvalue=Ljung_Box_pvalue, Ljung_Box_whitenoise=Ljung_Box_whitenoise,series=0)
    
    # Add results tables
    df_results_P47_3500RPM_Viscosity_128_Glycerin<-rbind(df_results_P47_3500RPM_Viscosity_128_Glycerin,df_results)
}
######################################################################################################################
#  First, plot the results for the equipment P47, 3500RPM, Viscosity 128, Glycering
# The window ID is added to make the correspondence of each data point table to the results table. 
# merge_water_viscous_sub_Q_n
# df_results_P47_3500RPM_Viscosity_128_Glycerin
merge_water_viscous_sub_Q_n<-unique(merge(merge_water_viscous_sub_Q_n,df_results_P47_3500RPM_Viscosity_128_Glycerin,by="Window"))

# Subselect collumns for the plot
merge_water_viscous_sub_Q_n<-merge_water_viscous_sub_Q_n[,c("Q","n","operational_states_points","diagnosis","adf_stationarity","Ljung_Box_whitenoise")]

# Rename collumns
colnames(merge_water_viscous_sub_Q_n)<-c("Q","n","operational_states","diagnosis","adf_stationarity","Ljung_Box_whitenoise")

# Add Time collumn
merge_water_viscous_sub_Q_n$Time<-1:dim(merge_water_viscous_sub_Q_n)[1]
######################################################################################################################
# List to store the simulated_data_sub_Q_n with results data
simulated_data_sub_Q_n_list<-list()

# Fix data.frame
simulated_data_all<-data.frame(simulated_data_all)

 # Set colnames
colnames(simulated_data_all)<-c("Q","Tm.i", "Tm.o", "P1", "P2", "RPM", "T", "pi", "mi","mo","n","H","BHP","Time","Series","n_discrete","h_discrete","bhp_discrete","operational_states")

# Define data.frame for the results of time-series
df_results_simulated<-data.frame(Window=c(),   mean_n=c(),   sd_n=c(),      operational_states=c(),       diagnosis=c(), adf_Dickey_Fuller=c(), adf_df=c(), adf_pvalue=c() , adf_stationarity=c(), Ljung_Box_Xsquared=c(), Ljung_Box_df=c(), Ljung_Box_pvalue=c(), Ljung_Box_whitenoise=c(),series=c())

# For each simulated time-series
for (series in unique(as.numeric(simulated_data_all[,c("Series")])))
{
    # Take the table for the corresponding time-series
    simulated_data_sub<-data.frame(simulated_data_all[simulated_data_all[,c("Series")]==series,])

    # Subset Q and n from the table    
    simulated_data_sub_Q_n<-data.frame(simulated_data_sub[,c("Q","n","operational_states")])

    # Rename collumns
    colnames(simulated_data_sub_Q_n)<-c("Q","n","operational_states_points")

    # Rename collumns
    colnames(simulated_data_sub_Q_n)<-c("Q","n")
    
    # Add collumn 
    simulated_data_sub_Q_n<-cbind(simulated_data_sub_Q_n,Window=-1)

    # Sort data.frame by Q
    simulated_data_sub_Q_n <- simulated_data_sub_Q_n[order(simulated_data_sub_Q_n$Q), ]
    
    # If I have X data points, and the window length is l
    # then I have X/l windows
    # Perform sliding window analysis
    # First, split the vector into chuncs
    time_windows<-split(simulated_data_sub_Q_n$n, ceiling(seq_along(simulated_data_sub_Q_n$n)/time_window_length))
    
    # Then, for each time-series
    for (time_window in names(time_windows))
    {
        # Take only the plots of this series
        data_points<-time_windows[[time_window]]

         # Set the time window in the merge_water_viscous_sub_Q_n tabe
        simulated_data_sub_Q_n[which(simulated_data_sub_Q_n$n %in% data_points),"Window"]<-time_window
        
        # Calculate the mean and sd of Efficiency 
        mean_n=mean(data_points)
      
        # Calculate the mean and sd of Efficiency 
        sd_n=sd(data_points)
      
        # Store also the operational states in a single variable
        operational_states<-paste(unique(simulated_data_sub[which(simulated_data_sub$n %in% data_points),"operational_states"]),collapse = ",")
      
        # Store also the diagnosis in a single variable
        diagnosis<-paste(unique(simulated_data_sub[which(simulated_data_sub$n %in% data_points),"Diagnosis"]),collapse = ",")
        
        # Concatenate title
        Ljung_Box_Xsquared      <-NA
        Ljung_Box_df            <-NA
        Ljung_Box_pvalue        <-NA
        Ljung_Box_whitenoise    <-NA
          
        # Perform Ljung-Box test
        box.test<-Box.test(data_points, lag = 1, type = "Ljung-Box")
    
        # If pvalue is not nan
        if(!is.na(box.test$p.value))
        {
            # Concatenate title
            Ljung_Box_Xsquared      <-round(box.test$statistic,3)
            Ljung_Box_df            <-round(box.test$parameter,3)
            Ljung_Box_pvalue        <-round(box.test$p.value,3)
            Ljung_Box_whitenoise    <-FALSE
    
            # If p-value smaller than 0.05 than set 
            if (Ljung_Box_pvalue > 0.05)
            {
                # Set stationarity to TRUE
                Ljung_Box_whitenoise <-TRUE
            }
        }
        # Add results to the list
        simulated_data_sub_Q_n_list[[series]]<-simulated_data_sub_Q_n
    
        # Concatenate title
        adf_Dickey_Fuller       <-NA
        adf_df                  <-NA
        adf_pvalue              <-NA
        adf_stationarity        <-FALSE
        
        # If at least two data.points
        if (length(data_points)>1)
        {  
            # Store results of adf test
            adf_test<-adf.test(data_points)
            
            # Concatenate title
            adf_Dickey_Fuller       <-adf_test[[1]]
            adf_df                  <-adf_test[[2]]
            adf_pvalue              <-adf_test[[4]]

            # If pvalue is not nan
            if(!is.nan(adf_pvalue))
            {
                # If p-value smaller than 0.05 than set 
                if (adf_pvalue <= 0.05)
                {
                    # Set stationarity to TRUE
                    adf_stationarity <-TRUE
                }
            }
        }
            
        # df_results
        df_results<-data.frame(Window=time_window,   mean_n=mean_n,   sd_n=sd_n,      operational_states=operational_states,       diagnosis=diagnosis, adf_Dickey_Fuller=adf_Dickey_Fuller, adf_df=adf_df, adf_pvalue=adf_pvalue , adf_stationarity=adf_stationarity, Ljung_Box_Xsquared=Ljung_Box_Xsquared, Ljung_Box_df=Ljung_Box_df, Ljung_Box_pvalue=Ljung_Box_pvalue, Ljung_Box_whitenoise=Ljung_Box_whitenoise,series=series)
        
        # Add results tables
        df_results_simulated<-rbind(df_results_simulated,df_results)
    }
}
#############################################################################################################################
# Remove 
# First plot for the operational states
p1<-ggplot(merge_water_viscous_sub_Q_n[,c("Q","n","operational_states")], aes(Q, n, colour = factor(operational_states))) + geom_point() + scale_color_viridis_d() + theme_bw() + theme(legend.position="bottom") + ggtitle("operational_states") +  theme(legend.title = element_blank()) +  theme(legend.text=element_text(size=6)) 
p2<-ggplot(merge_water_viscous_sub_Q_n[,c("Q","n","diagnosis")], aes(Q, n, colour = factor(diagnosis))) + geom_point() + scale_color_viridis_d() + theme_bw() + theme(legend.position="bottom") +scale_fill_viridis_d(option = "plasma") + ggtitle("diagnosis") +  theme(legend.title = element_blank()) +  theme(legend.text=element_text(size=6)) 
p3<-ggplot(merge_water_viscous_sub_Q_n[,c("Q","n","adf_stationarity")], aes(Q, n, colour = factor(adf_stationarity))) + geom_point() + theme_bw() + theme(legend.position="bottom")  + ggtitle("adf_stationarity") + theme(legend.title = element_blank()) +  theme(legend.text=element_text(size=6)) 
p4<-ggplot(merge_water_viscous_sub_Q_n[,c("Q","n","Ljung_Box_whitenoise")], aes(Q, n, colour = factor(Ljung_Box_whitenoise))) + geom_point()  + theme_bw() + theme(legend.position="bottom")+ ggtitle("Ljung_Box_whitenoise") + theme(legend.title = element_blank()) +  theme(legend.text=element_text(size=6)) 


# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"Performance_curves_Time_Series_Analysis.png",sep=""), width = 30, height = 20, res=600, units = "cm")  
  ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = FALSE)
dev.off()
