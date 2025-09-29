# Plot the barplot with the viscusoty distrituion
png(filename=paste(project_folder,"Plot_viscosity_distribution.png",sep=""), width = 30, height = 15, res=600, units = "cm")  
 ggplot(merge_water_viscous, aes(x=as.numeric(Inlet.Viscosity.mi))) + geom_histogram(color="darkblue", fill="lightblue",bins=200) +scale_x_continuous(breaks = seq(0, 1300, by = 50))  + theme_bw() + ggtitle("Inlet.Viscosity.mi")
dev.off()


# Start inlet viscosity variable to determine the value category
merge_water_viscous$Inlet.Viscosity<-0

# Set inlet viscosity
merge_water_viscous[which(as.numeric(merge_water_viscous$Inlet.Viscosity.mi) < 10),"Inlet.Viscosity"]<-0
merge_water_viscous[which(as.numeric(merge_water_viscous$Inlet.Viscosity.mi) > 10 & as.numeric(merge_water_viscous$Inlet.Viscosity.mi) < 30),"Inlet.Viscosity"]  <-25
merge_water_viscous[which(as.numeric(merge_water_viscous$Inlet.Viscosity.mi) > 31 & as.numeric(merge_water_viscous$Inlet.Viscosity.mi) < 55),"Inlet.Viscosity"]  <-50
merge_water_viscous[which(as.numeric(merge_water_viscous$Inlet.Viscosity.mi) > 56 & as.numeric(merge_water_viscous$Inlet.Viscosity.mi) < 110),"Inlet.Viscosity"] <-88
merge_water_viscous[which(as.numeric(merge_water_viscous$Inlet.Viscosity.mi) > 111 & as.numeric(merge_water_viscous$Inlet.Viscosity.mi) < 200),"Inlet.Viscosity"] <-128                          
merge_water_viscous[which(as.numeric(merge_water_viscous$Inlet.Viscosity.mi) > 201 & as.numeric(merge_water_viscous$Inlet.Viscosity.mi) < 275),"Inlet.Viscosity"] <-226
merge_water_viscous[which(as.numeric(merge_water_viscous$Inlet.Viscosity.mi) > 276 & as.numeric(merge_water_viscous$Inlet.Viscosity.mi) < 400),"Inlet.Viscosity"] <-330                          
merge_water_viscous[which(as.numeric(merge_water_viscous$Inlet.Viscosity.mi) > 401 & as.numeric(merge_water_viscous$Inlet.Viscosity.mi) < 600),"Inlet.Viscosity"]<- 474
merge_water_viscous[which(as.numeric(merge_water_viscous$Inlet.Viscosity.mi) > 601 & as.numeric(merge_water_viscous$Inlet.Viscosity.mi) < 725),"Inlet.Viscosity"]<- 655
merge_water_viscous[which(as.numeric(merge_water_viscous$Inlet.Viscosity.mi) > 726 & as.numeric(merge_water_viscous$Inlet.Viscosity.mi) < 950),"Inlet.Viscosity"]<- 847
merge_water_viscous[which(as.numeric(merge_water_viscous$Inlet.Viscosity.mi) > 951 & as.numeric(merge_water_viscous$Inlet.Viscosity.mi) < 1150),"Inlet.Viscosity"]<-1036
merge_water_viscous[which(as.numeric(merge_water_viscous$Inlet.Viscosity.mi) > 1151 & as.numeric(merge_water_viscous$Inlet.Viscosity.mi) < 1500),"Inlet.Viscosity"]<-1273
