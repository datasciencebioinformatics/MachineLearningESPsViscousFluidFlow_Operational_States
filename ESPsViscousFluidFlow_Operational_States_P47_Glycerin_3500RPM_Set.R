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











