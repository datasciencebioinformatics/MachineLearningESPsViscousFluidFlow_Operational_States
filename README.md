## Data points analyis
### 1) Set the project folder either on Linux or in Window
##### a) If in Linux
project_folder="/home/felipe/Documents/MachineLearningESPsViscousFluidFlow_Operational_States/"

##### b) Or in Windows
project_folder="C:/Users/User/Documents/GitHub/MachineLearningESPsViscousFluidFlow_Operational_States/"

##### c) set outputfolder
output_dir=project_folder

##### 2) Load all packages
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_Load_All_R_Packages.R",sep=""))

##### 3) Load ESP dataset from files
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_Load_ESP_dataset.R",sep=""))

##### 4) Calculate the efficiency parameters (Efficiency, Head, Power)
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_Parametrization.R",sep=""))

##### 5) Calculate the efficiency parameters (Efficiency, Head, Power)
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_ReynoldsNumber.R",sep=""))

##### 6) Add the test matrix (RPM, equipment, visosity)
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_TestMatrix_ESP.R",sep=""))

##### 7) Load ESP dataset from files
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_Exploratory_ESP_dataset.R",sep=""))

##### 8) Load ESP Normalization and Discretization
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_Normalization_Discretization.R",sep=""))
1
#### 9) Generate plots
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_P47_Glycerin_3500RPM_Set.R",sep=""))

#### 10) Load simulated data
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_Load_Simulated_Data.R",sep=""))

#### 11) Analysis of flow rate (Q) versus performance (n, H, BHP)
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_P47_Glycerin_3500RPM_Performance_curves.R",sep=""))

#### 12) ESPsViscousFluidFlow_Operational_States_AnalysisP47_Glycerin_3500RPM_Add_Operational_states
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_AnalysisP47_Glycerin_3500RPM_Add_Operational_states.R",sep=""))

#### 13) ESPsViscousFluidFlow_Operational_States_AnalysisP47_Glycerin_3500RPM_Add_Diagnosis
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_AnalysisP47_Glycerin_3500RPM_Add_Diagnosis.R",sep=""))

#### 14) ESPsViscousFluidFlow_Operational_States_AnalysisP47_Glycerin_3500RPM_Time_Series
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_AnalysisP47_Glycerin_3500RPM_Time_Series.R",sep=""))

#### 15) ESPsViscousFluidFlow_Operational_States_AnalysisP47_Glycerin_3500RPM_Sliiding_Window
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_AnalysisP47_Glycerin_3500RPM_Sliding_Window.R",sep=""))

#### 16) ESPsViscousFluidFlow_Operational_States_AnalysisP47_Glycerin_3500RPM_Sliiding_Window
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_AnalysisP47_Glycerin_3500RPM_Sliding_Window_Heatmap.R",sep=""))
