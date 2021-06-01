# This script is the master code that produces all results and figures for the manuscript --> it does take some time to run, with several of the programs monitoring progress along the way.

model_files <- list.files('model-process',full.names = TRUE)


for (i in seq_along(model_files)) {
  source(model_files[[i]])
}



figure_files <-list.files('figures-process',full.names = TRUE)

for (i in seq_along(figure_files)) {
  source(figure_files[[i]])
}
