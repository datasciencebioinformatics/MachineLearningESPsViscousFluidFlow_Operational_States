#########################################################################################################
# The Reynolds number (Re) is a dimensionless quantity used in fluid dynamics to predict whether a flow will be laminar or turbulent. It's calculated as the ratio of inertial forces to viscous forces acting on the fluid. The formula for Reynolds number is: 
merge_water_viscous$Re <-0

# First, Calculate the velocity
for (measure in rownames(merge_water_viscous))
{
  # Save the equipment 
  equip=merge_water_viscous[measure,"equip"]

  # The rotational speed w in rads/s
  w=as.numeric(merge_water_viscous[measure,"RPM"])*0.10472
  
  # Density
  # [kg/m³]
  p  <-as.numeric(merge_water_viscous[measure,"Inlet.Density.ρi"])

  # Store viscosity
  # Pa.s
  vis<-as.numeric(merge_water_viscous[measure,"Outlet.Viscosity.mo"])*0.001
  
  # Inner diameter of pipe, di in m
  # [m]
  L  <-as.numeric(metada_data[metada_data$model==equip & metada_data$Metric=="rads","Impeller.diameter"])

  # Store Reynollds number
  merge_water_viscous[measure ,"Re"]= (p * w * (L^2)) / vis
}
#####################################################################################
png(filename=paste(project_folder,"Boxplot_Reynolds_number_viscous.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  ggplot(na.omit(merge_water_viscous), aes(x=fluid, y=Re, fill=equip)) +  geom_boxplot()+ theme(legend.position = "bottom") + theme_bw() 
dev.off()
#####################################################################################
# Data.frame with average values of avg.Re
df_average_re<-expand.grid(equip=levels(factor(merge_water_viscous$equip)), fluid=levels(factor(merge_water_viscous$fluid)),avg.Re=0)

for (condition in rownames(df_average_re))
{
  # Compute the average value for avg.Re 
  df_average_re[condition,"avg.Re"]<-mean(merge_water_viscous[which(merge_water_viscous$equip %in% df_average_re[condition,"equip"] & merge_water_viscous$fluid %in% df_average_re[condition,"fluid"]),"Re"])
}
#####################################################################################
