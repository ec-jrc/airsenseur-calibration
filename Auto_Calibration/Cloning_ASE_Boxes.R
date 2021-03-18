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

# Load packages
# Installing package librarian to install and load packages from CRAN, github and bio conductor in 1 line
# https://towardsdatascience.com/an-efficient-way-to-install-and-load-r-packages-bc53247f058d
if (!"remotes"   %in% utils::installed.packages()) install.packages("remotes")
if (!"librarian" %in% utils::installed.packages()) remotes::install_github("DesiQuintans/librarian")
source("global.R")
if (exists("list.Packages") && list.Packages != "") {
    librarian::shelf(list.Packages)
    rm(list.Packages, envir = .GlobalEnv)}
if (exists("list.packages.github") && list.packages.github != "") {
    librarian::shelf(list.packages.github)
    rm(list.packages.github, envir = .GlobalEnv)}
librarian::shelf(gplots) # heatmap2

futile.logger::flog.info("[Global] List of installed packages")
print(search(), quote = FALSE)
cat("\n")

# Cloning ASE Boxes ####
# Antwerp ====
# Creating ASE boxes ----
Cloning.path <- file.path(Dir, Project, "424A56")
List.ASE <- c("424A57","425D01","425D0A","425D0B","4278FD")
for (i in List.ASE) Create_ASE(DirShiny = Dir, Project = Project, New.ASE.Name = i, Cloning.path = Cloning.path)
