################################################################################################################
# To Do:
# reproduce Fig. 7—ESP P47 performance pumping viscous fluid at 3,500 rev/min.
# Subset ESP_P47 
ESP_p47_glycerin_3500<-merge_water_viscous[which(merge_water_viscous$RPM==3500 & merge_water_viscous$equip =="P47" ),]

ESP_p47_glycerin_3500$Viscosity<-as.factor(ESP_p47_glycerin_3500$Inlet.Viscosity)


################################################################################################################
# Fig. 7—ESP P47 performance pumping viscous fluid at 3,500 rev/min.
ESP_P47_water_plot_Q_H <- ggplot(ESP_p47_glycerin_3500, aes(x = Q, y = H,colour=Viscosity, shape=fluid))     + geom_point() + theme_bw()   + ggtitle ("Flow rate Q vs. Head H")    + ylab("Head H [m]")                   + labs(x = expression("Flow rate Q [" * m^3/h * "]")) + theme(legend.position = "bottom")  
ESP_P47_water_plot_BHP <- ggplot(ESP_p47_glycerin_3500, aes(x = Q, y = BHP,colour=Viscosity, shape=fluid))   + geom_point() + theme_bw()   + ggtitle ("Flow rate Q vs. Power BHP") + ylab("Power BHP [W]")                + labs(x = expression("Flow rate Q [" * m^3/h * "]")) + theme(legend.position = "none")    
ESP_P47_water_plot_n   <- ggplot(ESP_p47_glycerin_3500, aes(x = Q, y = n*100,colour=Viscosity, shape=fluid)) + geom_point() + theme_bw()   + ggtitle ("Flow rate Q vs. Efficiency n") + ylab("Efficiency n [%]")          + labs(x = expression("Flow rate Q [" * m^3/h * "]"))   + theme(legend.position = "bottom")      

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"ESP_P47_dilluted_glucerin_RPM3500.png",sep=""), width = 20, height = 25, res=600, units = "cm")  
  ggarrange(ESP_P47_water_plot_Q_H,ESP_P47_water_plot_BHP,ESP_P47_water_plot_n, nrow =3,common.legend = TRUE,legend="bottom")
dev.off()



################################################################################################################
# Calculate dimensionless n, h, bhp and combine all in the same plot.
# For this to work, select only the Viscosity  = 128
# Subset ESP_P47 
ESP_p47_glycerin_3500_128<-ESP_p47_glycerin_3500[which(ESP_p47_glycerin_3500$Viscosity == 128),]

# The rotational speed w in rads/s
# unit checked RPM
w=as.numeric(3500)*0.10472

# Impeller Diameter mm
# Unit checked mm
D=unique(metada_data[which(metada_data$model == "P47"),2]) #* 1000 # m converted to mm
D2<-D^2
D3<-D^3
D5<-D^5
w2<-w^2
w3<-w^3
p  <-ESP_p47_glycerin_3500_128$Inlet.Density.ρi
Q  <-ESP_p47_glycerin_3500_128$Q/3600/ESP_p47_glycerin_3500_128$Shaft.Torque

# Calculate dimensionless
dimensionless_H<-(g*ESP_p47_glycerin_3500_128$H)/( w2* D2 )
dimensionless_Q<-(Q)/( w* D3 )
dimensionless_P<-ESP_p47_glycerin_3500_128$BHP/(p*w3*D5)
n_percentage<-ESP_p47_glycerin_3500_128$n

df_dimentioneless<-data.frame(dimensionless_Q=dimensionless_Q,dimensionless_H=dimensionless_H,dimensionless_P=dimensionless_P,n_percentage=n_percentage)

# Melt data
ESP_p47_glycerin_3500_melt<-reshape2::melt(df_dimentioneless,id.vars="dimensionless_Q")

# Fig. 7—ESP P47 performance pumping viscous fluid at 3,500 rev/min.
ESP_P47_water_plot_Q_H <- ggplot(df_dimentioneless, aes(x = dimensionless_Q, y = dimensionless_H))     + geom_point() + theme_bw()   + ggtitle ("dimensionless Q vs. dimensionless H")    + ylab("dimensionless H ")                   + labs(x = expression("dimensionless Q")) + theme(legend.position = "bottom")    + scale_color_brewer(palette="Dark2")     
ESP_P47_water_plot_BHP <- ggplot(df_dimentioneless, aes(x = dimensionless_Q, y = dimensionless_P))   + geom_point() + theme_bw()   + ggtitle ("dimensionless Q vs. dimensionless BHP") + ylab("dimensionless W")                + labs(x = expression("dimensionless Q")) + theme(legend.position = "none")    + scale_color_brewer(palette="Dark2")
ESP_P47_water_plot_n   <- ggplot(df_dimentioneless, aes(x = dimensionless_Q, y = n_percentage*100)) + geom_point() + theme_bw()   + ggtitle ("dimensionless Q vs. dimensionless n") + ylab("n [%]")          + labs(x = expression("dimensionless Q"))   + theme(legend.position = "bottom")      + scale_color_brewer(palette="Dark2")

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"ESP_P47_dilluted_glucerin_RPM3500_dimentsionless.png",sep=""), width = 20, height = 25, res=600, units = "cm")  
  ggarrange(ESP_P47_water_plot_Q_H,ESP_P47_water_plot_BHP,ESP_P47_water_plot_n, nrow =3,common.legend = TRUE,legend="bottom")
dev.off()














################################################################################################################
# To Do:
# reproduce Fig. 7—ESP P47 performance pumping viscous fluid at 3,500 rev/min.
# Subset ESP_P47 
ESP_p47_all_3500<-merge_water_viscous[which(merge_water_viscous$RPM==3500 & merge_water_viscous$equip =="P47" ),]

#ESP_P47_water 
ESP_p47_all_3500$Viscosity<-as.factor(ESP_p47_all_3500$Inlet.Viscosity)
################################################################################################################
# Fig. 7—ESP P47 performance pumping viscous fluid at 3,500 rev/min.
ESP_P47_water_plot_Q_H <- ggplot(ESP_p47_all_3500, aes(x = Q, y = H,colour=Viscosity,shape=fluid))     + geom_point() + theme_bw()   + ggtitle ("Flow rate Q vs. Head H")    + ylab("Head H [m]")                   + labs(x = expression("Flow rate Q [" * m^3/h * "]")) + theme(legend.position = "none")    + scale_color_brewer(palette="Dark2")           + scale_x_continuous(breaks = seq(0,max(ESP_p47_all_3500$Q)+5,by=5))        
ESP_P47_water_plot_BHP <- ggplot(ESP_p47_all_3500, aes(x = Q, y = BHP,colour=Viscosity,shape=fluid))   + geom_point() + theme_bw()   + ggtitle ("Flow rate Q vs. Power BHP") + ylab("Power BHP [W]")                + labs(x = expression("Flow rate Q [" * m^3/h * "]")) + theme(legend.position = "none")    + scale_color_brewer(palette="Dark2")           + scale_x_continuous(breaks = seq(0,max(ESP_p47_all_3500$Q)+5,by=5)) + scale_y_continuous(breaks = seq(round(min(ESP_p47_all_3500$BHP)-10,0),round(max(ESP_p47_all_3500$BHP),0)+10,by=500))
ESP_P47_water_plot_n   <- ggplot(ESP_p47_all_3500, aes(x = Q, y = n*100,colour=Viscosity,shape=fluid)) + geom_point() + theme_bw()   + ggtitle ("Flow rate Q vs. Efficiency n") + ylab("Efficiency n [%]")          + labs(x = expression("Flow rate Q [" * m^3/h * "]"))   + theme(legend.position = "bottom")      + scale_color_brewer(palette="Dark2")     + scale_x_continuous(breaks = seq(0,max(ESP_p47_all_3500$Q)+5,by=5)) + scale_y_continuous(breaks = seq(0,100,by=10))

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"ESP_P47_all_RPM3500.png",sep=""), width = 20, height = 25, res=600, units = "cm")  
  ggarrange(ESP_P47_water_plot_Q_H,ESP_P47_water_plot_BHP,ESP_P47_water_plot_n, nrow =3,common.legend = TRUE,legend="bottom")
dev.off()
################################################################################################################






################################################################################################################
# To Do:
# reproduce Fig. 7—ESP P47 performance pumping viscous fluid at 3,500 rev/min.
# Subset ESP_P47 
ESP_P47_Diluted_Glycerin<-merge_water_viscous[merge_water_viscous$RPM=="3500" & merge_water_viscous$equip=="P47" & merge_water_viscous$fluid!="Glycerin" ,]

#ESP_P47_water 
#############################################################################################################
# To Do:
# reproduce Fig. 7—ESP P47 performance pumping viscous fluid at 3,500 rev/min.
# Subset ESP_P47 
ESP_P47_Glycerin<-merge_water_viscous

#ESP_P47_water 
ESP_P47_Glycerin$Viscosity<-as.factor(ESP_P47_Glycerin$Inlet.Viscosity)
################################################################################################################
# Fig. 7—ESP P47 performance pumping viscous fluid at 3,500 rev/min.
# Melt tabele
ESP_P47_water_plot_Q_H <- ggplot(ESP_P47_Glycerin, aes(x = Q, y = H,colour=Viscosity))     + geom_point() + theme_bw()   + ggtitle ("Flow rate Q vs. Head H")    + ylab("Head H [m]")                   + labs(x = expression("Flow rate Q [" * m^3/h * "]"))  + theme(legend.position = "bottom")    + scale_color_brewer(palette="Dark2")     + facet_grid(rows = vars(equip),cols=vars(RPM))
ESP_P47_water_plot_BHP <- ggplot(ESP_P47_Glycerin, aes(x = Q, y = BHP,colour=Viscosity))   + geom_point() + theme_bw()   + ggtitle ("Flow rate Q vs. Power BHP") + ylab("Power BHP [W]")                + labs(x = expression("Flow rate Q [" * m^3/h * "]"))  + scale_color_brewer(palette="Dark2")         + facet_grid(rows = vars(equip),cols=vars(RPM))
ESP_P47_water_plot_n   <- ggplot(ESP_P47_Glycerin, aes(x = Q, y = n*100,colour=Viscosity))   + geom_point() + theme_bw() + ggtitle ("Flow rate Q vs. Efficiency n") + ylab("Efficiency n [%]")          + labs(x = expression("Flow rate Q [" * m^3/h * "]"))  + theme(legend.position = "bottom")  + scale_color_brewer(palette="Dark2")         + facet_grid(rows = vars(equip),cols=vars(RPM))

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"Plot_Q_H_ESP_P47_dilluted_glucerin_RPM3500.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  ESP_P47_water_plot_Q_H
dev.off()

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"Plot_Q_BHP_ESP_P47_dilluted_glucerin_RPM3500.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  ESP_P47_water_plot_BHP
dev.off()

# Melt tabele
# Plot_raw_vibration_data.png                                                                                                            
png(filename=paste(project_folder,"Plot_Q_n_ESP_P47_dilluted_glucerin_RPM3500.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  ESP_P47_water_plot_n
dev.off()
