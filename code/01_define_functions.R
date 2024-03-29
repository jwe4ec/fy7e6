# ---------------------------------------------------------------------------- #
# Define Functions
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Define version_control() ----
# ---------------------------------------------------------------------------- #

# Define function to check R version, load groundhog package, and return groundhog_day

version_control <- function() {
  # Ensure you are using the same version of R used at the time the script was 
  # written. To install a previous version, go to 
  # https://cran.r-project.org/bin/windows/base/old/
  
  script_R_version <- "R version 4.1.2 (2021-11-01)"
  current_R_version <- R.Version()$version.string
  
  if(current_R_version != script_R_version) {
    warning(paste0("This script is based on ", script_R_version,
                   ". You are running ", current_R_version, "."))
  }
  
  # Load packages using "groundhog", which installs and loads the most recent
  # versions of packages available on the specified date ("groundhog_day"). This 
  # is important for reproducibility so that everyone running the script is using
  # the same versions of packages used at the time the script was written.
  
  # Note that packages may take longer to load the first time you load them with
  # "groundhog.library". This is because you may not have the correct versions of 
  # the packages installed based on the "groundhog_day". After "groundhog.library"
  # automatically installs the correct versions alongside other versions you may 
  # have installed, it will load the packages more quickly.
  
  # If in the process of loading packages with "groundhog.library" for the first 
  # time the console states that you first need to install "Rtools", follow steps 
  # here (https://cran.r-project.org/bin/windows/Rtools/) for installing "Rtools" 
  # and putting "Rtools" on the PATH. Then try loading the packages again.
  
  library(groundhog)
  meta.groundhog("2022-01-01")
  groundhog_day <- "2022-01-01"
  
  return(groundhog_day)
}

# ---------------------------------------------------------------------------- #
# Define convert_POSIXct() ----
# ---------------------------------------------------------------------------- #

# Define function to convert system-generated timestamps to POSIXct data types 
# (with "tz = 'UTC'" for user-provided "return_date_as_POSIXct" of "return_intention" 
# table and "tz = 'EST'" for all system-generated timestamps)

convert_POSIXct <- function(dat) {
  for (i in 1:length(dat)) {
    POSIXct_colnames <- c(names(dat[[i]])[grep("as_POSIXct", names(dat[[i]]))],
                          "system_date_time_earliest",
                          "system_date_time_latest")
    
    for (j in 1:length(POSIXct_colnames)) {
      # Strip timezone from character vector
      
      dat[[i]][, POSIXct_colnames[j]] <- sub(" UTC| EST", "", 
                                             dat[[i]][, POSIXct_colnames[j]])
      
      # Convert character vector to POSIXct, specifying timezone
      
      if (names(dat[i]) == "return_intention" & 
          POSIXct_colnames[j] == "return_date_as_POSIXct") {
        dat[[i]][, POSIXct_colnames[j]] <- as.POSIXct(dat[[i]][, POSIXct_colnames[j]],
                                                      format = "%Y-%m-%d %H:%M:%S",
                                                      tz = "UTC")
      } else {
        dat[[i]][, POSIXct_colnames[j]] <- as.POSIXct(dat[[i]][, POSIXct_colnames[j]],
                                                      format = "%Y-%m-%d %H:%M:%S",
                                                      tz = "EST")
      }
    }
  }
  
  return(dat)
}