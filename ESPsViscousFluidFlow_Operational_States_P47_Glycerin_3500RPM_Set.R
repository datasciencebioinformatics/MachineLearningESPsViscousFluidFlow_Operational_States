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
# add collumn for the operational state n,H, BHO
@ Generate second
p3<-ggplot(merge_water_viscous_sub, aes(x = step, y = operational_states)) +   geom_tile() +    theme_bw() + theme(legend.position = "bottom")  + theme(axis.text.y = element_text(size=16),face="bold") + scale_x_continuous(breaks=seq(1,dim(merge_water_viscous_sub)[1],3)) 


# bwplot               
png(filename=paste(output_dir,"Normalized_Discrete_Performance_P47_3500RPM_Glycerin_Viscosity_128.png",sep=""), width = 15, height =45, res=600, units = "cm")  
  # Plot the bayesian network graph
  ggarrange(p1, p2, p3, labels = c("A", "B","C"),    ncol = , nrow = 3)
dev.off()











