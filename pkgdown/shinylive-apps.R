# Exports the package's six Shiny tools as shinylive apps, which run in the browser with
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
  reinsurance = "run_shiny_reinsurance_pricing_tool",
  portfolio = "run_shiny_portfolio_tool"
)
titles <- c(
  simulator = "NetSimulator \u2013 Claims & Reinsurance Simulator",
  distribution = "NetDisFit \u2013 Distribution Fitting Tool",
  glm = "NetGLM \u2013 GLM Fitting Tool",
  reserving = "NetReserve \u2013 Claims Reserving Tool",
  reinsurance = "NetTreaty \u2013 Reinsurance Pricing Tool",
  portfolio = "NetCapital \u2013 Portfolio Capital Tool"
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
# each tool's page counts its visits with GoatCounter, as the rest of the website does
# (_pkgdown.yml): without cookies or personal data, and never the data uploaded to a tool,
# which stays in the browser. The installed package counts nothing.
goatcounter <- paste0("<script data-goatcounter=\"https://netsimanalytics.goatcounter.com/count\" ",
                      "async src=\"https://gc.zgo.at/count.js\"></script>")
for (tool in names(tools)) {
  page <- file.path(out, tool, "index.html")
  html <- readLines(page, warn = FALSE)
  head_end <- grep("</head>", html, fixed = TRUE)[1]
  if (is.na(head_end)) stop("no </head> in ", page)
  html <- append(html, goatcounter, after = head_end - 1)
  writeLines(html, page)
}

cat("Exported", length(tools), "tools to", out, "\n")
