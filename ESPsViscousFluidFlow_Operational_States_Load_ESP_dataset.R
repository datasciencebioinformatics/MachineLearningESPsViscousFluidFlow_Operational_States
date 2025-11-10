#########################################################################################################
# All the excel files in the project folder are loaded in this                                          #
#########################################################################################################
# For each sample, the data will be loaded by all rotational speed 
# First, the P100 in water samples
P100_viscous_2400_data <- cbind(data.frame(read_excel(paste(project_folder,"P100_viscous.xlsx",sep=""), sheet = "2400 rpm",skip = 9)),RPM="2400")
P100_viscous_3000_data <- cbind(data.frame(read_excel(paste(project_folder,"P100_viscous.xlsx",sep=""), sheet = "3000 rpm",skip = 9)),RPM="3000")
P100_viscous_3500_data <- cbind(data.frame(read_excel(paste(project_folder,"P100_viscous.xlsx",sep=""), sheet = "3500 rpm",skip = 9)),RPM="3500")

# Second, the P62 in water samples
P62_viscous_2400_data <- cbind(data.frame(read_excel(paste(project_folder,"P62_viscous.xlsx",sep=""), sheet = "2400 rpm",skip = 9)),RPM="2400")
P62_viscous_3000_data <- cbind(data.frame(read_excel(paste(project_folder,"P62_viscous.xlsx",sep=""), sheet = "3000 rpm",skip = 9)),RPM="3000")
P62_viscous_3500_data <- cbind(data.frame(read_excel(paste(project_folder,"P62_viscous.xlsx",sep=""), sheet = "3500 rpm",skip = 9)),RPM="3500")

# Third, the P47 in water samples
P47_viscous_2400_data <- cbind(data.frame(read_excel(paste(project_folder,"P47_viscous.xlsx",sep=""), sheet = "2400 rpm",skip = 9)),RPM="2400")
P47_viscous_3000_data <- cbind(data.frame(read_excel(paste(project_folder,"P47_viscous.xlsx",sep=""), sheet = "3000 rpm",skip = 9)),RPM="3000")
P47_viscous_3500_data <- cbind(data.frame(read_excel(paste(project_folder,"P47_viscous.xlsx",sep=""), sheet = "3500 rpm",skip = 9)),RPM="3500")

# Fourh, the P37 in water samples
P37_viscous_2400_data <- cbind(data.frame(read_excel(paste(project_folder,"P37_viscous.xlsx",sep=""), sheet = "2400 rpm",skip = 9)),RPM="2400")
P37_viscous_3000_data <- cbind(data.frame(read_excel(paste(project_folder,"P37_viscous.xlsx",sep=""), sheet = "3000 rpm",skip = 9)),RPM="3000")
P37_viscous_3500_data <- cbind(data.frame(read_excel(paste(project_folder,"P37_viscous.xlsx",sep=""), sheet = "3500 rpm",skip = 9)),RPM="3500")

# Fifth, the P37 in water samples
HC10000_viscous_1800_data <- cbind(data.frame(read_excel(paste(project_folder,"HC10000_viscous.xlsx",sep=""), sheet = "1800 rpm",skip = 9)),RPM="1800")
HC10000_viscous_2400_data <- cbind(data.frame(read_excel(paste(project_folder,"HC10000_viscous.xlsx",sep=""), sheet = "2400 rpm",skip = 9)),RPM="2400")
HC10000_viscous_3000_data <- cbind(data.frame(read_excel(paste(project_folder,"HC10000_viscous.xlsx",sep=""), sheet = "3000 rpm",skip = 9)),RPM="3000")

# Sixthh, the P37 in water samples
HC12500_viscous_1800_data <- cbind(data.frame(read_excel(paste(project_folder,"HC12500_viscous.xlsx",sep=""), sheet = "1800 rpm",skip = 9)),RPM="1800")
HC12500_viscous_2400_data <- cbind(data.frame(read_excel(paste(project_folder,"HC12500_viscous.xlsx",sep=""), sheet = "2400 rpm",skip = 9)),RPM="2400")
HC12500_viscous_3000_data <- cbind(data.frame(read_excel(paste(project_folder,"HC12500_viscous.xlsx",sep=""), sheet = "3000 rpm",skip = 9)),RPM="3000")

# Bing all sheets for P100_viscous
P100_viscous<-cbind(rbind(P100_viscous_2400_data[-1,],P100_viscous_3000_data[-1,],P100_viscous_3500_data[-1,]),equip="P100")
P62_viscous <-cbind(rbind(P62_viscous_2400_data[-1,],P62_viscous_3000_data[-1,],P62_viscous_3500_data[-1,]),equip="P62")
P47_viscous <-cbind(rbind(P47_viscous_2400_data[-1,],P47_viscous_3000_data[-1,],P47_viscous_3500_data[-1,]),equip="P47")
P37_viscous <-cbind(rbind(P37_viscous_2400_data[-1,],P37_viscous_3000_data[-1,],P37_viscous_3500_data[-1,]),equip="P37")
HC10000_viscous <-cbind(rbind(HC10000_viscous_1800_data[-1,],HC10000_viscous_2400_data[-1,],HC10000_viscous_3000_data[-1,]),equip="HC10000")
HC12500_viscous <-cbind(rbind(HC12500_viscous_1800_data[-1,],HC12500_viscous_2400_data[-1,],HC12500_viscous_3000_data[-1,]),equip="HC12500")

# Merge all viscous
All_viscous<-rbind(P100_viscous,P62_viscous,P47_viscous,P37_viscous,HC10000_viscous,HC12500_viscous)
#########################################################################################################
# First, the P100 in water samples
#P100_water_1200_data <-cbind(data.frame(read_excel(paste(project_folder,"P100_water.xlsx",sep=""), sheet = "1200 rpm",skip = 7)),RPM="1200")
P100_water_1800_data <- cbind(data.frame(read_excel(paste(project_folder,"P100_water.xlsx",sep=""), sheet = "1800 rpm",skip = 7)),RPM="1800")
P100_water_2400_data <- cbind(data.frame(read_excel(paste(project_folder,"P100_water.xlsx",sep=""), sheet = "2400 rpm",skip = 7)),RPM="2400")
P100_water_3000_data <- cbind(data.frame(read_excel(paste(project_folder,"P100_water.xlsx",sep=""), sheet = "3000 rpm",skip = 7)),RPM="3000")
P100_water_3500_data <- cbind(data.frame(read_excel(paste(project_folder,"P100_water.xlsx",sep=""), sheet = "3500 rpm",skip = 7)),RPM="3500")

#P62_water_1200_data <- cbind(data.frame(read_excel(paste(project_folder,"P62_water.xlsx",sep=""), sheet = "1200 rpm",skip = 7)),RPM="1200")
P62_water_1800_data <- cbind(data.frame(read_excel(paste(project_folder,"P62_water.xlsx",sep=""), sheet = "1800 rpm",skip = 7)),RPM="1800")
P62_water_2400_data <- cbind(data.frame(read_excel(paste(project_folder,"P62_water.xlsx",sep=""), sheet = "2400 rpm",skip = 7)),RPM="2400")
P62_water_3000_data <- cbind(data.frame(read_excel(paste(project_folder,"P62_water.xlsx",sep=""), sheet = "3000 rpm",skip = 7)),RPM="3000")
P62_water_3500_data <- cbind(data.frame(read_excel(paste(project_folder,"P62_water.xlsx",sep=""), sheet = "3500 rpm",skip = 7)),RPM="3500")

#P47_water_1200_data <- cbind(data.frame(read_excel(paste(project_folder,"P47_water.xlsx",sep=""), sheet = "1200 rpm",skip = 7)),RPM="1200")
P47_water_1800_data <- cbind(data.frame(read_excel(paste(project_folder,"P47_water.xlsx",sep=""), sheet = "1800 rpm",skip = 7)),RPM="1800")
P47_water_2400_data <- cbind(data.frame(read_excel(paste(project_folder,"P47_water.xlsx",sep=""), sheet = "2400 rpm",skip = 7)),RPM="2400")
P47_water_3000_data <- cbind(data.frame(read_excel(paste(project_folder,"P47_water.xlsx",sep=""), sheet = "3000 rpm",skip = 7)),RPM="3000")
P47_water_3500_data <- cbind(data.frame(read_excel(paste(project_folder,"P47_water.xlsx",sep=""), sheet = "3500 rpm",skip = 7)),RPM="3500")

#P37_water_1200_data <- cbind(data.frame(read_excel(paste(project_folder,"P37_water.xlsx",sep=""), sheet = "1200 rpm",skip = 7)),RPM="1200")
P37_water_1800_data <-  cbind(data.frame(read_excel(paste(project_folder,"P37_water.xlsx",sep=""), sheet = "1800 rpm",skip = 7)),RPM="1800")
P37_water_2400_data <- cbind(data.frame(read_excel(paste(project_folder,"P37_water.xlsx",sep=""), sheet = "2400 rpm",skip = 7)),RPM="2400")
P37_water_3000_data <- cbind(data.frame(read_excel(paste(project_folder,"P37_water.xlsx",sep=""), sheet = "3000 rpm",skip = 7)),RPM="3000")
P37_water_3500_data <- cbind(data.frame(read_excel(paste(project_folder,"P37_water.xlsx",sep=""), sheet = "3500 rpm",skip = 7)),RPM="3500")

#HC10000_water_1200_data <- cbind(data.frame(read_excel(paste(project_folder,"HC10000_water.xlsx",sep=""), sheet = "1200 rpm",skip = 9)),RPM="1200")
HC10000_water_1800_data <- cbind(data.frame(read_excel(paste(project_folder,"HC10000_water.xlsx",sep=""), sheet = "1800 rpm",skip = 7)),RPM="1800")
HC10000_water_2400_data <- cbind(data.frame(read_excel(paste(project_folder,"HC10000_water.xlsx",sep=""), sheet = "2400 rpm",skip = 7)),RPM="2400")
HC10000_water_3000_data <- cbind(data.frame(read_excel(paste(project_folder,"HC10000_water.xlsx",sep=""), sheet = "3000 rpm",skip = 7)),RPM="3000")
HC10000_water_3500_data <- cbind(data.frame(read_excel(paste(project_folder,"HC10000_water.xlsx",sep=""), sheet = "3500 rpm",skip = 7)),RPM="3500")

#HC12500_water_1200_data <- cbind(data.frame(read_excel(paste(project_folder,"HC10000_water.xlsx",sep=""), sheet = "1200 rpm",skip = 7)),RPM="1200")
HC12500_water_1800_data <- cbind(data.frame(read_excel(paste(project_folder,"HC10000_water.xlsx",sep=""), sheet = "1800 rpm",skip = 7)),RPM="1800")
HC12500_water_2400_data <- cbind(data.frame(read_excel(paste(project_folder,"HC10000_water.xlsx",sep=""), sheet = "2400 rpm",skip = 7)),RPM="2400")
HC12500_water_3000_data <- cbind(data.frame(read_excel(paste(project_folder,"HC10000_water.xlsx",sep=""), sheet = "3000 rpm",skip = 7)),RPM="3000")
HC12500_water_3500_data <- cbind(data.frame(read_excel(paste(project_folder,"HC10000_water.xlsx",sep=""), sheet = "3500 rpm",skip = 7)),RPM="3500")

# Bing all sheets for P100_viscous
P100_water<-cbind(rbind(P100_water_1800_data[-1,],P100_water_2400_data[-1,],P100_water_3000_data[-1,],P100_water_3500_data[-1,]),equip="P100")
P62_water<-cbind(rbind(P62_water_1800_data[-1,],P62_water_2400_data[-1,],P62_water_3000_data[-1,],P62_water_3500_data[-1,]),equip="P62")
P47_water<-cbind(rbind(P47_water_1800_data[-1,],P47_water_2400_data[-1,],P47_water_3000_data[-1,],P47_water_3500_data[-1,]),equip="P47")
P37_water<-cbind(rbind(P37_water_1800_data[-1,],P37_water_2400_data[-1,],P37_water_3000_data[-1,],P37_water_3500_data[-1,]),equip="P37")
HC10000_water<-cbind(rbind(HC10000_water_1800_data[-1,],HC10000_water_2400_data[-1,],HC10000_water_3000_data[-1,],HC10000_water_3500_data[-1,]),equip="HC10000")
HC12500_water<-cbind(rbind(HC12500_water_1800_data[-1,],HC12500_water_2400_data[-1,],HC12500_water_3000_data[-1,],HC12500_water_3500_data[-1,]),equip="HC12500")


# Merge all water
All_water<-rbind(P100_water,P47_water,P37_water,P62_water,HC10000_water,HC12500_water)
#########################################################################################################
# Path to metadata file
metada_data_file="eps_metada.xlsx"

# Load the metada file
metada_data<-data.frame(read_excel(paste(project_folder,metada_data_file,sep=""),skip=1))

# Split data.frame into RPM and rads
metada_data_rpm <-cbind(metada_data[,c(1,2,3,4,5,6)],Metric="RPM")
metada_data_rads<-cbind(metada_data[,c(1,2,7,8,9,10)],Metric="rads")

# Split data.frame into RPM and rads
colnames(metada_data_rpm)<-c("model","Impeller.diameter","1800","2400","3000","3500","Metric")
colnames(metada_data_rads)<-c("model","Impeller.diameter","1800","2400","3000","3500","Metric")

# Compile metada data
metada_data<-rbind(metada_data_rpm,metada_data_rads)
#######################################################################1.01325 bar##################################
# Complete water sample data to be compatible with the viscuous sample data
# Rename Shaft.Torque
colnames(All_viscous)[9]<-"Shaft.Torque"

# Calculate water density at inlet temperature
#p  = water("rho", T = convert(as.numeric(All_water$Inlet.Temperature.T1), "K"), P = as.numeric(All_water$Inlet.Pressure.P1))

# Implement equation to calculate viscosity
#mi = water("visc", T = convert(as.numeric(All_water$Inlet.Temperature.T1), "K"), P = as.numeric(All_water$Inlet.Pressure.P1))

# Implement equation to calculate viscosity    
#mo = water("visck", T = convert(as.numeric(All_water$Inlet.Temperature.T1), "K"), P = as.numeric(All_water$Inlet.Pressure.P1)) * 0.1


# Add collumns for viscosities in water. 
# Star values as -1
#All_water$Inlet.Viscosity.mi  <- mi$visc
#All_water$Outlet.Viscosity.mo <- mo$visck

# Inlet.Density.ρi
#All_water$Inlet.Density.ρi<-p$rho


# Set Average tempaerature to NA
#All_water$Average.Inlet.Temp.Tm.i   <- -1
#All_water$Average.Outlet.Temp.Tm.o  <- -1

# Colnames
#common_varibles<-colnames(All_viscous)[which(colnames(All_viscous) %in% colnames(All_water))]

#########################################################################################################
# Merge tables
#merge_water_viscous<-rbind(All_water[,common_varibles],All_viscous[,common_varibles])
########################################################################################################
# Remove water samples from Water samples with necessary
merge_water_viscous<-All_viscous
########################################################################################################
merge_water_viscous$id<-as.numeric(merge_water_viscous$id)                       
merge_water_viscous$Flow.rate<-as.numeric(merge_water_viscous$Flow.rate)
merge_water_viscous$Inlet.Temperature.T1<-as.numeric(merge_water_viscous$Inlet.Temperature.T1)
merge_water_viscous$Inlet.Temperature.T2<-as.numeric(merge_water_viscous$Inlet.Temperature.T2)
merge_water_viscous$Outlet.Temperature.T3<-as.numeric(merge_water_viscous$Outlet.Temperature.T3)  
merge_water_viscous$Outlet.Temperature.T4<-as.numeric(merge_water_viscous$Outlet.Temperature.T4)
merge_water_viscous$Inlet.Pressure.P1<-as.numeric(merge_water_viscous$Inlet.Pressure.P1)
merge_water_viscous$Outlet.Pressure.P2<-as.numeric(merge_water_viscous$Outlet.Pressure.P2)
merge_water_viscous$Shaft.Torque<-as.numeric(merge_water_viscous$Shaft.Torque)
merge_water_viscous$Average.Inlet.Temp.Tm.i<-as.numeric(merge_water_viscous$Average.Inlet.Temp.Tm.i)
merge_water_viscous$Average.Outlet.Temp.Tm.o<-as.numeric(merge_water_viscous$Average.Outlet.Temp.Tm.o)
merge_water_viscous$Inlet.Density.ρi<-as.numeric(merge_water_viscous$Inlet.Density.ρi)
merge_water_viscous$Inlet.Viscosity.mi<-as.numeric(merge_water_viscous$Inlet.Viscosity.mi)
merge_water_viscous$Outlet.Viscosity.mo<-as.numeric(merge_water_viscous$Outlet.Viscosity.mo)
merge_water_viscous$replicate<-as.numeric(merge_water_viscous$replicate)
########################################################################################################
# Caclulate statistical summary
numeric_variables<-merge_water_viscous[,c("Flow.rate","Inlet.Temperature.T1","Inlet.Temperature.T2","Outlet.Temperature.T3","Outlet.Temperature.T4","Inlet.Pressure.P1","Outlet.Pressure.P2","Shaft.Torque","Average.Inlet.Temp.Tm.i","Average.Outlet.Temp.Tm.o","Inlet.Density.ρi","Inlet.Viscosity.mi","Outlet.Viscosity.mo")]
########################################################################################################
