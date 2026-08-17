# deploy.R — re-render the report and stage it for GitHub Pages
# The report renders next to its .qmd; Pages serves from docs/.
quarto::quarto_render("public/bh_school_sim_open.qmd")
file.copy("public/bh_school_sim_open.html", "docs/index.html", overwrite = TRUE)
message("docs/index.html updated - commit and push to publish")
