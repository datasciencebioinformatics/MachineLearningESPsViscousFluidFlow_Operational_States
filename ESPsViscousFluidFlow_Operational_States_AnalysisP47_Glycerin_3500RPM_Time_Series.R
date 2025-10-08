######################################################################################################################
# Define the lenght of the time-windnw
time_window_length<-10

# Define data.frame for the results of time-series
df_results_P47_3500RPM_Viscosity_128_Glycerin<-data.frame(Window=c(),   mean_n=c(),   sd_n=c(),      operational_states=c(),       diagnosis=c(), adf_Dickey_Fuller=c(), adf_df=c(), adf_pvalue=c() , adf_stationarity=c(), Ljung_Box_Xsquared=c(), Ljung_Box_df=c(), Ljung_Box_pvalue=c(), Ljung_Box_whitenoise=c(),series=c())

# First scan the P47, RPM300, Viscosity 128, Glycerin
# Take the table for Q versus efficiency
merge_water_viscous_sub_Q_n<-merge_water_viscous_sub[,c("Q","n")]

# Sort data.frame by Q
merge_water_viscous_sub_Q_n <- merge_water_viscous_sub_Q_n[order(merge_water_viscous_sub_Q_n$Q), ]

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
    df_results<-data.frame(Window=time_window,   mean_n=mean_n,   sd_n=sd_n,      operational_states=operational_states,       diagnosis=diagnosis, adf_Dickey_Fuller=adf_Dickey_Fuller, adf_df=adf_df, adf_pvalue=adf_pvalue , adf_stationarity=adf_stationarity, Ljung_Box_Xsquared=Ljung_Box_Xsquared, Ljung_Box_df=Ljung_Box_df, Ljung_Box_pvalue=Ljung_Box_pvalue, Ljung_Box_whitenoise=Ljung_Box_whitenoise,series=0)
    
    # Add results tables
    df_results_P47_3500RPM_Viscosity_128_Glycerin<-rbind(df_results_P47_3500RPM_Viscosity_128_Glycerin,df_results)
}
######################################################################################################################
# Fix data.frame
simulated_data_all<-data.frame(simulated_data_all)

 # Set colnames
colnames(simulated_data_all)<-c("Q","Tm.i", "Tm.o", "P1", "P2", "RPM", "T", "pi", "mi","mo","n","H","BHP","Time","Series")

# Define data.frame for the results of time-series
df_results_simulated<-data.frame(Window=c(),   mean_n=c(),   sd_n=c(),      operational_states=c(),       diagnosis=c(), adf_Dickey_Fuller=c(), adf_df=c(), adf_pvalue=c() , adf_stationarity=c(), Ljung_Box_Xsquared=c(), Ljung_Box_df=c(), Ljung_Box_pvalue=c(), Ljung_Box_whitenoise=c(),series=c())

# For each simulated time-series
for (series in unique(as.numeric(simulated_data_all[,c("Series")])))
{
    # Take the table for the corresponding time-series
    simulated_data_sub<-data.frame(simulated_data_all[simulated_data_all[,c("Series")]==series,])

    simulated_data_sub_Q_n<-data.frame(simulated_data_sub[,c("Q","n")])
    
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
#############################################################################################################################3
#  First, plot the results for the equipment P47, 3500RPM, Viscosity 128, Glycering
# The window ID is added to make the correspondence of each data point table to the results table. 


# Second, plot the results for the simulated data
