#####################################################
##                    Set up code                  ##
## This is source on launch of the Rstudio project ##
#####################################################

#===================================
# File paths

# Check for OS
sys_path <- ifelse(Sys.info()["sysname"]=="Windows", "G:/","/Volumes/GoogleDrive/")

# Path to emLab data folder
emLab_data_path <- file.path(sys_path,"Shared drives/emlab/data")

# Path to this project's folder
project_path <- file.path(sys_path,"Shared drives/emlab/projects/current-projects/china_hs_closure")
## Path to this project's data folder
project_data_path <- file.path(sys_path,"Shared drives/emlab/projects/current-projects/china_hs_closure/data")

#===================================
# Source functions