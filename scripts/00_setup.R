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
# Define a plotting theme

# Set a global theme -----------------------------------------------------------
ggplot2::theme_set(
  ggplot2::theme_bw()
)
ggplot2::theme_update(
  panel.grid.major.x = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  panel.grid.major.y = ggplot2::element_line(color = "gray",
                                              linewidth = 0.176389),
  line = ggplot2::element_line(color = "black",
                               linewidth = 0.176389),
  legend.background = ggplot2::element_blank(),
  legend.key = ggplot2::element_blank(),
  strip.background = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(hjust = 0),
  text = ggplot2::element_text(size = 8),
)


eez_col <- "#9ECEEB"
land_col <- "#8996A0"
polygon_pal <- c(Atlantic = "#C13832", Pacific = "#D28E00")
closure_pal <- c(Closed = "gray50", Open = "gray90")