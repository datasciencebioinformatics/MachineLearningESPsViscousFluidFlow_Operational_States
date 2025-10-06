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
#################################################################################################################################################################################################################
# Add collumn name
simulated_data_2<-cbind(simulated_data_2,Series=2)
simulated_data_3<-cbind(simulated_data_3,Series=3)
simulated_data_4<-cbind(simulated_data_4,Series=4)
simulated_data_5<-cbind(simulated_data_5,Series=5)
simulated_data_6<-cbind(simulated_data_6,Series=6)
simulated_data_7<-cbind(simulated_data_7,Series=7)
simulated_data_8<-cbind(simulated_data_8,Series=8)
simulated_data_9<-cbind(simulated_data_9,Series=9)
simulated_data_10<-cbind(simulated_data_10,Series=10)
simulated_data_11<-cbind(simulated_data_11,Series=11)
#################################################################################################################################################################################################################
simulated_data_all<-rbind(simulated_data_2,simulated_data_3,simulated_data_4,simulated_data_5,simulated_data_6,simulated_data_7,simulated_data_8,simulated_data_9,simulated_data_10,simulated_data_11)
#################################################################################################################################################################################################################
