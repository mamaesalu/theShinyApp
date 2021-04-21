# Set options here
options(golem.app.prod = TRUE) # TRUE = production mode, FALSE = development mode
options(shiny.maxRequestSize=100*1024^2)

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app()
