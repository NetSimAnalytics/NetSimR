# Exports the package's five Shiny tools as shinylive apps, which run in the browser with
# webR: no R or server needed. Each app installs NetSimR from the website's WebAssembly
# repository (built by r-wasm/actions/build-rwasm in .github/workflows/pkgdown.yaml) and its
# dependencies from webR's, then starts its tool.
#
#   Rscript pkgdown/shinylive-apps.R <output directory> [<WebAssembly repository URL>]
#
# The output directory gets one folder per tool and the shared shinylive assets.
args <- commandArgs(TRUE)
out <- if (length(args) >= 1) args[1] else "docs/apps"
repo <- if (length(args) >= 2) args[2] else "https://netsimanalytics.github.io/NetSimR/wasm"

tools <- c(
  simulator = "run_shiny_simulator",
  distribution = "run_shiny_distribution_fitting_tool",
  glm = "run_shiny_glm_fitting_tool",
  reserving = "run_shiny_reserving_tool",
  reinsurance = "run_shiny_reinsurance_pricing_tool"
)
titles <- c(
  simulator = "NetSim claims simulator",
  distribution = "NetSim distribution fitting tool",
  glm = "NetSim GLM fitting tool",
  reserving = "NetSim reserving tool",
  reinsurance = "NetSim reinsurance pricing tool"
)

source_dir <- file.path(tempdir(), "netsimr-shinylive")
for (tool in names(tools)) {
  dir <- file.path(source_dir, tool)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  # the tool is looked up by name, so that shinylive does not take NetSimR for a package
  # to install from webR's repository, which has an old version
  writeLines(c(
    "# written by pkgdown/shinylive-apps.R",
    "library(shiny)",
    sprintf("webr::install(\"NetSimR\", repos = c(\"%s\", \"https://repo.r-wasm.org\"))", repo),
    sprintf("getExportedValue(\"NetSimR\", \"%s\")()", tools[[tool]])
  ), file.path(dir, "app.R"))
  # the packages are downloaded when an app starts, not bundled with it: NetSimR is not on
  # webR's repository in this version, which bundling would look for
  shinylive::export(dir, out, subdir = tool, quiet = TRUE, wasm_packages = FALSE,
                    template_params = list(title = titles[[tool]]))
}
cat("Exported", length(tools), "tools to", out, "\n")
