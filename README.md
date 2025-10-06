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

#### 9) Generate plots
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_P47_Glycerin_3500RPM_Set.R",sep=""))

### 10) Analysis of flow rate (Q) versus performance (n, H, BHP)
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_P47_Glycerin_3500RPM_Performance_curves.R",sep=""))

### 11) Analysis of flow rate (Q) versus performance (n, H, BHP)
source(paste(project_folder,"ESPsViscousFluidFlow_Operational_States_Simulated_Performance_curves.R",sep=""))

