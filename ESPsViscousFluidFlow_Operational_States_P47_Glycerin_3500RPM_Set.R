# This script will add a collumn to the metafile with the operational state
# The operational state will be defined as the n=state-h=state-bhp=state
merge_water_viscous_sub<-merge_water_viscous[which(merge_water_viscous$equip=="P47" & merge_water_viscous$fluid == "Glycerin" & merge_water_viscous$RPM=="3500" & merge_water_viscous$Inlet.Viscosity == "128"),]

# Take the tertiles
merge_water_viscous_sub_tertiles<-as.data.frame(lapply(merge_water_viscous_sub[,c("n","BHP","H")], tertile))

# Renames collumns
colnames(merge_water_viscous_sub_tertiles)<-c("n_discrete","BHP_discrete","H_discrete")

# Merge tables
merge_water_viscous_sub<-cbind(merge_water_viscous_sub,merge_water_viscous_sub_tertiles)

# add operational states
merge_water_viscous_sub$operational_states<-paste(paste("n=",merge_water_viscous_sub$n_discrete,sep=""),paste("BHP=",merge_water_viscous_sub$BHP_discrete,sep=""),paste("H=",merge_water_viscous_sub$H_discrete,sep=""),sep="|")
##################################################################################################################################################################
# Remove units from bhp, head and efficiency and put them in a single plot
merge_water_viscous_normalized<-as.data.frame(lapply(merge_water_viscous_sub[,c("n","BHP","H")], normalize))

# Renames collumns
colnames(merge_water_viscous_normalized)<-c("n_normalized","BHP_normalized","H_normalized")

# Merge tables
merge_water_viscous_sub<-cbind(merge_water_viscous_sub,merge_water_viscous_normalized)

# Add stepp
merge_water_viscous_sub$step<-1:dim(merge_water_viscous_sub)[1]

# Take collumns for the plot
merge_water_viscous_plot<-merge_water_viscous_sub[,c("n_normalized","BHP_normalized","H_normalized","step")]

# Rename collumns
colnames(merge_water_viscous_plot)<-c("n","BHP","H","step")

# Melt table
merge_water_viscous_sub_melt<-reshape2::melt(merge_water_viscous_plot,id.vars="step")

# Generate plot
p1<-ggplot(merge_water_viscous_sub_melt, aes(x=step, y=value, group=variable)) +   geom_line(aes(color=variable))+   geom_point(aes(color=variable)) +  theme_bw() + theme(legend.position = "bottom") + ggtitle("P47, 3550RPM, Glycerin, inlet viscosituy 128") + scale_x_continuous(breaks=seq(1,dim(merge_water_viscous_sub)[1],3)) 
##################################################################################################################################################################
# Take the discrete values
merge_water_viscous_sub_tertiles$step<-1:dim(merge_water_viscous_sub_tertiles)[1]

# Rename collumns
colnames(merge_water_viscous_sub_tertiles)<-c("n","BHP","H","step")

# Melt table
merge_water_viscous_sub_tertiles<-reshape2::melt(merge_water_viscous_sub_tertiles,id.vars="step")

@ Generate second
p2<-ggplot(merge_water_viscous_sub_tertiles, aes(x = step, y = variable, fill = value)) +   geom_tile() +    theme_bw() + theme(legend.position = "bottom") + scale_fill_grey()  + theme(axis.text.y = element_text(size=16),face="bold") + scale_x_continuous(breaks=seq(1,dim(merge_water_viscous_sub)[1],3)) 

##################################################################################################################################################################
# Relevel factors
merge_water_viscous_sub$operational_states<-factor(merge_water_viscous_sub$operational_states,levels=c("n=Low|BHP=Low|H=High", "n=Low|BHP=High|H=Low", "n=Medium|BHP=Low|H=High", "n=Medium|BHP=Medium|H=Medium", "n=Medium|BHP=High|H=Low", "n=High|BHP=Medium|H=Medium", "n=High|BHP=High|H=Low"))

# add collumn for the operational state n,H, BHO
# Generate second
p3<-ggplot(merge_water_viscous_sub, aes(x = step, y = operational_states)) +   geom_tile() +    theme_bw() + theme(legend.position = "bottom")  + theme(axis.text.y = element_text(size=16),face="bold") + scale_x_continuous(breaks=seq(1,dim(merge_water_viscous_sub)[1],3)) 


# bwplot               
png(filename=paste(output_dir,"Normalized_Discrete_Performance_P47_3500RPM_Glycerin_Viscosity_128.png",sep=""), width = 15, height =45, res=600, units = "cm")  
  # Plot the bayesian network graph
  ggarrange(p1, p2, p3, labels = c("A", "B","C"),    ncol = , nrow = 3)
dev.off()
#################################################################
#1  n=Low|BHP=Low|H=High
#2 n=Low|BHP=High|H=Low
#3 n=Medium|BHP=Low|H=High
#4 n=Medium|BHP=Medium|H=Medium
#5 n=Medium|BHP=High|H=Low
#6 n=High|BHP=Medium|H=Medium
#7 n=High|BHP=High|H=Low
# Set the discrete values of Q, Tm.i, Tm.o, P1, P2, Ⲅ, ρ, μi, μo

# Take the tertiles
input_variables_tertiles<-as.data.frame(lapply(merge_water_viscous_sub[,c("Flow.rate", "Average.Inlet.Temp.Tm.i", "Average.Outlet.Temp.Tm.o", "Inlet.Pressure.P1", "Outlet.Pressure.P2", "Shaft.Torque", "Inlet.Density.ρi", "Inlet.Viscosity.mi", "Outlet.Viscosity.mo")], tertile))

# Reset-colnames
colnames(input_variables_tertiles)<-c("Q_a", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo")

# Merge data.franmes
merge_water_viscous_sub<-cbind(merge_water_viscous_sub,input_variables_tertiles)

# Take the discrete valuers
merge_water_viscous_sub[which(merge_water_viscous_sub$operational_states=="n=Low|BHP=Low|H=High"),c("Q_a", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo")]





#################################################################
OS1<-unique(merge_water_viscous_sub[which(merge_water_viscous_sub$operational_states=="n=Low|BHP=Low|H=High"),c("Q_a", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo")])
OS2<-unique(merge_water_viscous_sub[which(merge_water_viscous_sub$operational_states=="n=Low|BHP=High|H=Low"),c("Q_a", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo")])
OS3<-unique(merge_water_viscous_sub[which(merge_water_viscous_sub$operational_states=="n=Medium|BHP=Low|H=High"),c("Q_a", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo")])
OS4<-unique(merge_water_viscous_sub[which(merge_water_viscous_sub$operational_states=="n=Medium|BHP=Medium|H=Medium"),c("Q_a", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo")])
OS5<-unique(merge_water_viscous_sub[which(merge_water_viscous_sub$operational_states=="n=Medium|BHP=High|H=Low"),c("Q_a", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo")])
OS6<-unique(merge_water_viscous_sub[which(merge_water_viscous_sub$operational_states=="n=High|BHP=Medium|H=Medium"),c("Q_a", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo")])
OS7<-unique(merge_water_viscous_sub[which(merge_water_viscous_sub$operational_states=="n=High|BHP=High|H=Low"),c("Q_a", "Tm.i", "Tm.o", "P1", "P2", "T", "pi", "mi", "mo")])
#################################################################
OS1$Operational_State<-"n=Low|BHP=Low|H=High"
OS2$Operational_State<-"n=Low|BHP=High|H=Low"
OS3$Operational_State<-"n=Medium|BHP=Low|H=High"
OS4$Operational_State<-"n=Medium|BHP=Medium|H=Medium"
OS5$Operational_State<-"n=Medium|BHP=High|H=Low"
OS6$Operational_State<-"n=High|BHP=Medium|H=Medium"
OS7$Operational_State<-"n=High|BHP=High|H=Low"
#################################################################
# Save results trable                                                  # 
write.table(data.frame(rbind(OS1,OS2,OS3,OS4,OS5,OS6,OS7)),   paste(output_dir,"ESPsViscousFluidFlow_Operational_state.tsv",sep="/"), na = "NA", append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t", quote =   FALSE)  








