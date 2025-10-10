########################################################################################################
# All the excel files in the project folder are loaded in this                                          #
#########################################################################################################
# For each sample, the data will be loaded by all rotational speed 
# First, the P100 in water samples
simulated_data_2<-data.frame(read_excel(paste(project_folder,"dados_simulados_2.xlsx",sep=""),col_names = FALSE))
simulated_data_3<-data.frame(read_excel(paste(project_folder,"dados_simulados_3.xlsx",sep=""),col_names = FALSE))
simulated_data_4<-data.frame(read_excel(paste(project_folder,"dados_simulados_4.xlsx",sep=""),col_names = FALSE))
simulated_data_5<-data.frame(read_excel(paste(project_folder,"dados_simulados_5.xlsx",sep=""),col_names = FALSE))
simulated_data_6<-data.frame(read_excel(paste(project_folder,"dados_simulados_6.xlsx",sep=""),col_names = FALSE))
simulated_data_7<-data.frame(read_excel(paste(project_folder,"dados_simulados_7.xlsx",sep=""),col_names = FALSE))
simulated_data_8<-data.frame(read_excel(paste(project_folder,"dados_simulados_8.xlsx",sep=""),col_names = FALSE))
simulated_data_9<-data.frame(read_excel(paste(project_folder,"dados_simulados_9.xlsx",sep=""),col_names = FALSE))
simulated_data_10<-data.frame(read_excel(paste(project_folder,"dados_simulados_10.xlsx",sep=""),col_names = FALSE))
simulated_data_11<-data.frame(read_excel(paste(project_folder,"dados_simulados_11.xlsx",sep=""),col_names = FALSE))
simulated_data_HC10000_1800<-data.frame(read_excel(paste(project_folder,"dados_simulados_HC10000_1800.xlsx",sep=""),col_names = FALSE))
#########################################################################################################
simulated_data_2<-as.data.frame(sapply(simulated_data_2, as.numeric))
simulated_data_3<-as.data.frame(sapply(simulated_data_3, as.numeric))
simulated_data_4<-as.data.frame(sapply(simulated_data_4, as.numeric))
simulated_data_5<-as.data.frame(sapply(simulated_data_5, as.numeric))
simulated_data_6<-as.data.frame(sapply(simulated_data_6, as.numeric))
simulated_data_7<-as.data.frame(sapply(simulated_data_7, as.numeric))
simulated_data_8<-as.data.frame(sapply(simulated_data_8, as.numeric))
simulated_data_9<-as.data.frame(sapply(simulated_data_9, as.numeric))
simulated_data_10<-as.data.frame(sapply(simulated_data_10, as.numeric))
simulated_data_11<-as.data.frame(sapply(simulated_data_11, as.numeric))
simulated_data_11<-as.data.frame(sapply(simulated_data_11, as.numeric))
simulated_data_HC10000_1800<-as.data.frame(sapply(simulated_data_HC10000_1800, as.numeric))
#################################################################################################################
# Set rownames()
rownames(simulated_data_2)<-c("Q", "Tm.i", "Tm.o", "Pi", "Po", "RPM", "tau", "rho", "mu.i", "mu.o", "eta", "H", "BHP")
rownames(simulated_data_3)<-c("Q", "Tm.i", "Tm.o", "Pi", "Po", "RPM", "tau", "rho", "mu.i", "mu.o", "eta", "H", "BHP")
rownames(simulated_data_4)<-c("Q", "Tm.i", "Tm.o", "Pi", "Po", "RPM", "tau", "rho", "mu.i", "mu.o", "eta", "H", "BHP")
rownames(simulated_data_5)<-c("Q", "Tm.i", "Tm.o", "Pi", "Po", "RPM", "tau", "rho", "mu.i", "mu.o", "eta", "H", "BHP")
rownames(simulated_data_6)<-c("Q", "Tm.i", "Tm.o", "Pi", "Po", "RPM", "tau", "rho", "mu.i", "mu.o", "eta", "H", "BHP")
rownames(simulated_data_7)<-c("Q", "Tm.i", "Tm.o", "Pi", "Po", "RPM", "tau", "rho", "mu.i", "mu.o", "eta", "H", "BHP")
rownames(simulated_data_9)<-c("Q", "Tm.i", "Tm.o", "Pi", "Po", "RPM", "tau", "rho", "mu.i", "mu.o", "eta", "H", "BHP")
rownames(simulated_data_10)<-c("Q", "Tm.i", "Tm.o", "Pi", "Po", "RPM", "tau", "rho", "mu.i", "mu.o", "eta", "H", "BHP")
rownames(simulated_data_11)<-c("Q", "Tm.i", "Tm.o", "Pi", "Po", "RPM", "tau", "rho", "mu.i", "mu.o", "eta", "H", "BHP")
rownames(simulated_data_HC10000_1800)<-c("q","P1","P2","Tm.i","Tm.o","mi","mo","pi","T")

# Set colnames()
colnames(simulated_data_2)<-1:dim(simulated_data_2)[2]
colnames(simulated_data_3)<-1:dim(simulated_data_3)[2]
colnames(simulated_data_4)<-1:dim(simulated_data_4)[2]
colnames(simulated_data_5)<-1:dim(simulated_data_5)[2]
colnames(simulated_data_6)<-1:dim(simulated_data_6)[2]
colnames(simulated_data_7)<-1:dim(simulated_data_7)[2]
colnames(simulated_data_8)<-1:dim(simulated_data_8)[2]
colnames(simulated_data_9)<-1:dim(simulated_data_9)[2]
colnames(simulated_data_10)<-1:dim(simulated_data_10)[2]
colnames(simulated_data_11)<-1:dim(simulated_data_11)[2]
colnames(simulated_data_HC10000_1800)<-1:dim(simulated_data_HC10000_1800)[2]
#############################################################################################################################
# Take the transpose of the data.frame
simulated_data_2<-t(simulated_data_2)[-1,]
simulated_data_3<-t(simulated_data_3)[-1,]
simulated_data_4<-t(simulated_data_4)[-1,]
simulated_data_5<-t(simulated_data_5)[-1,]
simulated_data_6<-t(simulated_data_6)[-1,]
simulated_data_7<-t(simulated_data_7)[-1,]
simulated_data_8<-t(simulated_data_8)[-1,]
simulated_data_9<-t(simulated_data_9)[-1,]
simulated_data_10<-t(simulated_data_10)[-1,]
simulated_data_11<-t(simulated_data_11)[-1,]
simulated_data_HC10000_1800<-t(simulated_data_HC10000_1800)[-1,]


# Add time to simmulated time-series
simulated_data_2<-cbind(simulated_data_2,Time=1:dim(simulated_data_2)[1])
simulated_data_3<-cbind(simulated_data_3,Time=1:dim(simulated_data_3)[1])
simulated_data_4<-cbind(simulated_data_4,Time=1:dim(simulated_data_4)[1])
simulated_data_5<-cbind(simulated_data_5,Time=1:dim(simulated_data_5)[1])
simulated_data_6<-cbind(simulated_data_6,Time=1:dim(simulated_data_6)[1])
simulated_data_7<-cbind(simulated_data_7,Time=1:dim(simulated_data_7)[1])
simulated_data_8<-cbind(simulated_data_8,Time=1:dim(simulated_data_8)[1])
simulated_data_9<-cbind(simulated_data_9,Time=1:dim(simulated_data_9)[1])
simulated_data_10<-cbind(simulated_data_10,Time=1:dim(simulated_data_10)[1])
simulated_data_11<-cbind(simulated_data_11,Time=1:dim(simulated_data_11)[1])
simulated_data_HC10000_1800<-cbind(simulated_data_HC10000_1800,Time=1:dim(simulated_data_HC10000_1800)[1])
##################################################################################################################################################################################################################
list_forecast_panels<-list()
list_p5_plots<-list()

# Melt by Time and viscosity
simulated_data_list<-list()

rownames(simulated_data_2)<-simulated_data_2[,"Time"]
rownames(simulated_data_3)<-simulated_data_3[,"Time"]
rownames(simulated_data_4)<-simulated_data_4[,"Time"]
rownames(simulated_data_5)<-simulated_data_5[,"Time"]
rownames(simulated_data_6)<-simulated_data_6[,"Time"]
rownames(simulated_data_7)<-simulated_data_7[,"Time"]
rownames(simulated_data_8)<-simulated_data_8[,"Time"]
rownames(simulated_data_9)<-simulated_data_9[,"Time"]
rownames(simulated_data_10)<-simulated_data_10[,"Time"]
rownames(simulated_data_11)<-simulated_data_11[,"Time"]

simulated_data_list[[2]]<-reshape2::melt(simulated_data_2,id.vars=c("Time"))
simulated_data_list[[3]]<-reshape2::melt(simulated_data_3,id.vars=c("Time"))
simulated_data_list[[4]]<-reshape2::melt(simulated_data_4,id.vars=c("Time"))
simulated_data_list[[5]]<-reshape2::melt(simulated_data_5,id.vars=c("Time"))
simulated_data_list[[6]]<-reshape2::melt(simulated_data_6,id.vars=c("Time"))
simulated_data_list[[7]]<-reshape2::melt(simulated_data_7,id.vars=c("Time"))
simulated_data_list[[8]]<-reshape2::melt(simulated_data_8,id.vars=c("Time"))
simulated_data_list[[9]]<-reshape2::melt(simulated_data_9,id.vars=c("Time"))
simulated_data_list[[10]]<-reshape2::melt(simulated_data_10,id.vars=c("Time"))
simulated_data_list[[11]]<-reshape2::melt(simulated_data_11,id.vars=c("Time"))


colnames(simulated_data_7)<-colnames(simulated_data_2)

i=0

# The rows are increasing viscosity values and the collumns the increasing time value
# Convert the P47_viscous_3500_data_sub to time-series for each variable
# For each variable 
for (number in 2:(length(simulated_data_list)))
{
    variable <- "eta"

    # Take simulated time series
    P47_viscous_3500_data_sub<-simulated_data_list[[number]]

    # Save data.frame as ts
    P47_viscous_3500_data_ts<-P47_viscous_3500_data_sub[which(P47_viscous_3500_data_sub$Var2==variable),c("Var1","value")]

    # Set colnames
    colnames(P47_viscous_3500_data_ts)<-c("Time","value")

    # Convert the time series to a data frame
    # It is altready on a data.frame format
    # Check what frequency means in the ts means
    P47_viscous_3500_data_ts <- ts(P47_viscous_3500_data_ts$value)

    # Store results of adf test
    P47_viscous_3500_data_adf<-adf.test(P47_viscous_3500_data_ts)    
    ########################################################################################
    # Train an ARIMA model
    # The Autoregressive integrated moving average.
    # Autoregressive model : the observation j+1 is calculated baed on the observation j.
    # In other words, : a regression on past values to predict future values.
    # turns best ARIMA model according to either AIC, AICc or BIC value.
    arima_model <- forecast::auto.arima(P47_viscous_3500_data_ts,test="adf")
    ##########################################################################################
    # Add title for adf test
    # If pvalue lower than the significance level of 0.05, can indicate that the alternative hypothesis of stationary time series should be accepted.
    adf_title<-paste(paste(P47_viscous_3500_data_adf[[5]],": ",round(P47_viscous_3500_data_adf[[1]],3)), paste("p-value",": ",round(P47_viscous_3500_data_adf[[4]],3)),sep=" / ")

    # Set arima results as data.frame
    df_results_arima<-data.frame(Value=arima_model$x)

    # Set the time
    df_results_arima$Time<-as.numeric(rownames(df_results_arima))

    # Set the fitted line
    df_results_fitted<-data.frame(Value=as.vector(fitted(arima_model)))

    # Set the time
    df_results_fitted$Time<-as.numeric(rownames(df_results_fitted))

    # Add labels to data.frames
    df_results_arima$Label<-"data"
    df_results_fitted$Label<-"fitted"

    # Merge the data
    df_results_merged<-rbind(df_results_fitted,df_results_arima)

    # Generate first plot 1
    p1<-ggplot(data=df_results_merged, aes(x=Time, y=Value,colour=Label)) + geom_line() + theme_bw()  + ggtitle(paste("Simulated time Series",number,variable,sep=" "), subtitle = adf_title) + theme(legend.position='bottom') + ylab(variable) + xlab("")
    ##########################################################################################
    # Ljung-Box test. 
    residuals <- residuals(arima_model)
    
    # Perform Ljung-Box test
    box.test<-Box.test(residuals, lag = 1, type = "Ljung-Box")

    # Concatenate title
    text_Ljung_Box=paste("Ljung-Box ",paste("X-squared =",round(box.test$statistic,3)), paste("df = ",box.test$parameter), paste("p-value = ",round(box.test$p.value,3)),sep=", ") 
    ##########################################################################################
    #  First-Order Differencing
    diff_log_data <- diff(log(as.vector(P47_viscous_3500_data_ts)))

    # Set First-Order Differencing as data.frame
    df_diff_log_data<-data.frame(Value=diff_log_data)

    # Set the time
    df_diff_log_data$Time<-as.numeric(rownames(df_diff_log_data))

    # Rollmeans defual k=3
    rollmeans<-rollmean(diff_log_data, 3)
    
    # Set results as data.frame
    df_rollmeans<-data.frame(Value=rollmeans)
    
    # Set the time
    df_rollmeans$Time<-as.numeric(rownames(df_rollmeans))

    # Set label
    df_diff_log_data$Label<-"transformed time series"
    df_rollmeans$Label    <-"roll means"

    # Merge data.frames
    df_diff_log_merg<-rbind(df_diff_log_data,df_rollmeans)

    # Generate first plot 2
    p2<-ggplot(data=df_diff_log_merg, aes(x=Time, y=Value,colour=Label)) + geom_line() + theme_bw()  + ggtitle(paste("Transformed simulated time Series",number,variable,sep=" "),subtitle=text_Ljung_Box) + theme(legend.position='bottom') + ylab(variable) + xlab("")
    ##########################################################################################
    # Acf and pacf function
    p3<-autoplot(acf(P47_viscous_3500_data_ts)) + theme_bw()  + ggtitle(paste("Autocorrelation Function (ACF) for ",variable,sep=" "))
    p4<-autoplot(pacf(P47_viscous_3500_data_ts)) + theme_bw() + ggtitle(paste("Partial Autocorrelation Function (PACF) for ",variable,sep=" "))
    ##########################################################################################
    # SPlit between trainning and testing set 
    training_timepoints<-seq(1,length(P47_viscous_3500_data_ts)-10,by=1)

    # Set the testing points
    testing_timepoints <-seq(length(P47_viscous_3500_data_ts)-9,length(P47_viscous_3500_data_ts),by=1)

    # Add to a data.frame
    trainning_set<-data.frame(Time=training_timepoints,Set="Trainning")
        testing_set  <-data.frame(Time=testing_timepoints,Set="Testing")

    # Merge data.frames
    training_testing_set<-rbind(trainning_set,testing_set)

    # Add values
    training_testing_set$values<-as.vector(P47_viscous_3500_data_ts)

    # Convert the time series to a data frame
    # It is altready on a data.frame format
    # Check what frequency means in the ts means
    P47_viscous_3500_data_sel <- ts(as.vector(P47_viscous_3500_data_ts)[training_timepoints])

    # turns best ARIMA model according to either AIC, AICc or BIC value.
    arima_model_sub <- forecast::auto.arima(P47_viscous_3500_data_sel)

    # Forecast 12 months ahead
    # Number of periods for forecasting.
    forecast_result_sub <- forecast::forecast(arima_model_sub, h = length(testing_timepoints))

    # Set title
    title<-paste(paste("Precitions MAE : ",round(mae(forecast_result_sub$mean,as.vector(P47_viscous_3500_data_ts)[testing_timepoints]),3),sep=""),
    paste("RMSE : ",round(rmse(forecast_result_sub$mean,as.vector(P47_viscous_3500_data_ts)[testing_timepoints]),3),sep=""),
    paste("COR : ",round(cor(forecast_result_sub$mean,as.vector(P47_viscous_3500_data_ts)[testing_timepoints]),3),sep=""),sep=" / ")
    
    # Add plot for forecast    
    p5<- autoplot(forecast_result_sub, xlab = "time", ylab = variable, main = variable)+ theme_bw() + ggtitle(paste("Forecast ARIMA simulated time Series",number,variable,sep=" "),subtitle=title)+ geom_point(data=training_testing_set, aes(x=Time, y=values,colour=Set)) + theme(legend.position='bottom') + ylab(variable)

    ##########################################################################################
    
    print(i)
    i<-i+1
    list_p5_plots[[i]]<-p1  + theme(legend.position="none")
    i<-i+1
    list_p5_plots[[i]]<-p2  + theme(legend.position="none")

}
##########################################################################################    
# bwplot               
png(filename=paste(output_dir,paste("Temporal_analysis_eta_simulated_timeseries_.png",sep="")), width = 35, height = 50, res=600, units = "cm")  
# Plot the forecast
    grid.arrange(list_p5_plots[[1]], list_p5_plots[[2]], list_p5_plots[[3]],list_p5_plots[[4]], list_p5_plots[[5]], list_p5_plots[[6]],list_p5_plots[[7]], list_p5_plots[[8]], list_p5_plots[[9]], list_p5_plots[[10]], list_p5_plots[[11]], list_p5_plots[[12]],list_p5_plots[[13]], list_p5_plots[[14]], list_p5_plots[[15]],list_p5_plots[[16]], list_p5_plots[[17]], list_p5_plots[[18]], list_p5_plots[[19]], list_p5_plots[[20]], list_p5_plots[[21]],list_p5_plots[[22]], list_p5_plots[[23]], list_p5_plots[[24]],list_p5_plots[[25]], list_p5_plots[[26]], list_p5_plots[[27]],list_p5_plots[[28]], list_p5_plots[[29]], list_p5_plots[[30]], ncol=3, nrow =10)
dev.off()
