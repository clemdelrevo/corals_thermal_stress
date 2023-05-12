# Create R project structure

#renv::init()
#renv::install("usethis")
#renv::snapshot() ; renv::status()
#usethis::use_description(check_name = FALSE)
#usethis::edit_r_environ()

#dir.create("data")
#dir.create("analyses")
#dir.create("outputs")
#dir.create("R")

#renv

#renv::install() ; renv::snapshot(prompt = FALSE) ; renv::status()

#dev

#devtools::load_all() 

#targets
#dir.create("outputs/pipeline")
# targets::tar_config_set(
#  store = "outputs/pipeline/",
#  script = "analyses/pipeline.R"
# )


#targets::tar_visnetwork(targets_only = T)
targets::tar_make()
