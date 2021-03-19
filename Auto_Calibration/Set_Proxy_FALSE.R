# Initialisation ####
rm(list = ls())
# Setting Directory and sourcing files
Possible.Dir <- c("S:\\Box Sync\\AirSensEUR\\Fieldtests\\Shiny",
                  "C:\\Bureau\\DIffusion\\Box Sync\\AirSensEUR\\Fieldtests\\Shiny",
                  "/media/sf_Box_Sync/AirSensEUR/Fieldtests/Shiny",
                  "/home/shinyadmin/App")
for (Dir in Possible.Dir) if (dir.exists(Dir)) {setwd(Dir); break()}
rm(Possible.Dir)
Project     <- "Federico"
DIR_Config  <- "Configuration"

# Load packages
librarian::shelf(data.table) 
librarian::shelf(futile.logger) 

futile.logger::flog.info("[Global] List of installed packages")
print(search(), quote = FALSE)
cat("\n")

# Keeping tracks of current List.ASE
if (exists("List.ASE")) List.ASE.Old <- List.ASE

# Setting PROXY to False
List.ASE <- c("424A56","424A57","425D01","425D0A","425D0B","4278FD")
for (ASE in List.ASE) {
    cat("===============================\n")
    futile.logger::flog.info(paste0("ASE: ",ASE))
    
    # load ASE-Server.cfg and set PROXY to FALSE if needed
    File_Server_cfg <- file.path(Dir, Project, ASE, DIR_Config, paste0(ASE,"_Servers.cfg"))
    if (file.exists(File_Server_cfg)) {
        futile.logger::flog.info(paste0("The config file ", File_Server_cfg, " for the configuration of servers exists"))
        # reading the Server configuration files
        cfg <- data.table::transpose(fread(file = File_Server_cfg, header = FALSE, na.strings=c("","NA", "<NA>")), fill = NA, make.names = 1)
        if (cfg$PROXY == "TRUE") {
            # Set Proxy to FALSE
            set(cfg, j = "PROXY", value = "FALSE")
            # save modified ASE_Server.sfg
            fwrite(setDT(as.data.frame(t(cfg)), keep.rownames = "PROXY")[],
                   file = File_Server_cfg, row.names = FALSE, col.names = FALSE)
            futile.logger::flog.info(paste0("Modified ", ASE,"_Servers.cfg saved in directory Configuration."))
        } else futile.logger::flog.info(paste0("PROXY already set to FALSE"))
    } else futile.logger::flog.warn(paste0("No server config file for the AirSensEUR box."))}

# Resuming List.ASE
if (exists("List.ASE.Old")) List.ASE <- List.ASE.Old
