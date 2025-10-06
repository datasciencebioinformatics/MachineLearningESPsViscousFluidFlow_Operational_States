################################################################################################################
# To Do:
# reproduce Fig. 7—ESP P47 performance pumping viscous fluid at 3,500 rev/min.
# This script will add a collumn to the metafile with the operational state
# The operational state will be defined as the n=state-h=state-bhp=state
merge_water_viscous_sub<-merge_water_viscous[which(merge_water_viscous$equip=="P47" & merge_water_viscous$fluid == "Glycerin" & merge_water_viscous$RPM=="3500" & merge_water_viscous$Inlet.Viscosity == "128"),]

# Remove units from bhp, head and efficiency and put them in a single plot
merge_water_viscous_normalized<-as.data.frame(lapply(merge_water_viscous_sub[,c("n","BHP","H","Q")], normalize))

# Renames collumns
colnames(merge_water_viscous_normalized)<-c("n_normalized","BHP_normalized","H_normalized","Q_normalized")

# Merge tables
merge_water_viscous_sub<-cbind(merge_water_viscous_sub,merge_water_viscous_normalized)

# Add stepp
merge_water_viscous_sub$step<-1:dim(merge_water_viscous_sub)[1]

# Take collumns for the plot
merge_water_viscous_plot<-merge_water_viscous_sub[,c("n_normalized","BHP_normalized","H_normalized","Q_normalized","step")]

# Rename collumns
colnames(merge_water_viscous_plot)<-c("n","BHP","H","Q","step")

# Melt table
merge_water_viscous_sub_melt<-reshape2::melt(merge_water_viscous_plot,id.vars=c("step","Q"))

# Generate plot
p1<-ggplot(merge_water_viscous_sub_melt, aes(x=Q, y=value, shape=variable)) +   geom_point(aes(color=variable)) +  theme_bw() + theme(legend.position = "bottom") + ggtitle("P47, 3550RPM, Glycerin, inlet viscosituy 128") +  geom_line(aes(color=variable)) + ylab("Normalized values (H, BHP, n)") +  xlab("Normalized Q") 

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"ESP_P47_dilluted_glucerin_RPM3500_Viscosity_128_Normalized_Performance_curves.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  p1 + scale_color_hue(l=40, c=35) + scale_color_hue(l=40, c=35)
dev.off()
################################################################################################################
# To do :
# 1) Define the three classes for efficiency (low, medium, high)
# 2) Organize the ranges for low, medium, high

# Take the tertiles
merge_water_viscous_sub_tertiles<-as.data.frame(lapply(merge_water_viscous_sub[,c("n","BHP","H","Q")], tertile))

# Renames collumns
colnames(merge_water_viscous_sub_tertiles)<-c("n_discrete","BHP_discrete","H_discrete","Q_discrete")
################################################################################################################
# Fig. 7—ESP P47 performance pumping viscous fluid at 3,500 rev/min.
ESP_P47_water_plot_Q_H <- ggplot(merge_water_viscous_sub, aes(x = Q, y = H))     + geom_point(aes(color=n_discrete)) + theme_bw()   + ggtitle ("Flow rate Q vs. Head H")    + ylab("Head H [m]")                   + labs(x = expression("Flow rate Q [" * m^3/h * "]")) + theme(legend.position = "bottom")  
ESP_P47_water_plot_BHP <- ggplot(merge_water_viscous_sub, aes(x = Q, y = BHP))   + geom_point(aes(color=n_discrete)) + theme_bw()   + ggtitle ("Flow rate Q vs. Power BHP") + ylab("Power BHP [W]")                + labs(x = expression("Flow rate Q [" * m^3/h * "]")) + theme(legend.position = "none")    
ESP_P47_water_plot_n   <- ggplot(merge_water_viscous_sub, aes(x = Q, y = n*100)) + geom_point(aes(color=n_discrete)) + theme_bw()   + ggtitle ("Flow rate Q vs. Efficiency n") + ylab("Efficiency n [%]")          + labs(x = expression("Flow rate Q [" * m^3/h * "]"))   + theme(legend.position = "bottom")      

################################################################################################################
# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"ESP_P47_dilluted_glucerin_RPM3500_Viscosity_128_Performance_curves.png",sep=""), width = 20, height = 25, res=600, units = "cm")  
  ggarrange(ESP_P47_water_plot_Q_H,ESP_P47_water_plot_BHP,ESP_P47_water_plot_n, nrow =3,common.legend = TRUE,legend="bottom")
dev.off()

# Merge tables
low_efficiency<-c(min(merge_water_viscous_sub[which(merge_water_viscous_sub$n_discrete=="Low"),"n"]),max(merge_water_viscous_sub[which(merge_water_viscous_sub$n_discrete=="Low"),"n"]))
medium_efficiency<-c(min(merge_water_viscous_sub[which(merge_water_viscous_sub$n_discrete=="Medium"),"n"]),max(merge_water_viscous_sub[which(merge_water_viscous_sub$n_discrete=="Medium"),"n"]))
high_efficiency<-c(min(merge_water_viscous_sub[which(merge_water_viscous_sub$n_discrete=="High"),"n"]),max(merge_water_viscous_sub[which(merge_water_viscous_sub$n_discrete=="High"),"n"]))

# Define the ranges
df_ranges<-data.frame(Star=c(low_efficiency[1],medium_efficiency[1],high_efficiency[1]),End=c(low_efficiency[2],medium_efficiency[2],high_efficiency[2]))

# Set tht ecolnames
rownames(df_ranges)<-c("Low","Medium","High")

# Save table with efficiency ranges
write.table(df_ranges,   paste(output_dir,"Efficiency_ranges_ranges.txt",sep="/"), na = "NA", append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t", quote =   FALSE)  
################################################################################################################
simulated_performance_curves<-list()

# Load the simulated data
# simulated_data_all
# Raname collumns
colnames(simulated_data_all)<-c("Q","Tm.i","Tm.o","P1","P2","RPM","T","pi","mi","mo","n","BHP","H","Time","Series")

# For each simulated time-series
for (series in unique(as.numeric(simulated_data_all[,c("Series")])))
{
  # Take the table for the corresponding time-series
  simulated_data_sub<-simulated_data_all[simulated_data_all[,c("Series")]==series,]

  # Remove units from bhp, head and efficiency and put them in a single plot
  simulated_data_sub[,c("BHP")]<-normalize(simulated_data_sub[,c("BHP")])

  # Rename collumns
  simulated_data<-simulated_data_sub[,c("n","BHP","H","Q")]
  
  # Melt table
  Q_values<-simulated_data[,c("Q")]
  n_values<-simulated_data[,c("n")]
  H_values<-simulated_data[,c("H")]
  BHP_values<-simulated_data[,c("BHP")]
  
  df_values<-rbind(data.frame(Q=Q_values,value=n_values, var="n"),
  data.frame(Q=Q_values,value=H_values, var="H"),
  data.frame(Q=Q_values,value=BHP_values, var="BHP"))
  
  # Generate plot
  p1<-ggplot(df_values, aes(x=Q, y=value, shape=var)) +   geom_point(aes(color=var)) +  theme_bw() + theme(legend.position = "bottom") + ggtitle(paste("Simulated time-series",series)) +  geom_line(aes(color=var)) + ylab("H, BHP, n") 

  # Add panel 
  simulated_performance_curves[[series]]<-p1

}

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"Simulated_Performance_curves.png",sep=""), width = 20, height = 25, res=600, units = "cm")  
  ggarrange(simulated_performance_curves[[2]], simulated_performance_curves[[3]], simulated_performance_curves[[4]], simulated_performance_curves[[5]],simulated_performance_curves[[6]],
          simulated_performance_curves[[7]], simulated_performance_curves[[8]], simulated_performance_curves[[9]], simulated_performance_curves[[10]],simulated_performance_curves[[11]], ncol=2, nrow=5, common.legend = TRUE, legend="bottom")
dev.off()

