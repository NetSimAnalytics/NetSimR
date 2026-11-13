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

# what a link to a tool shows when it is shared (LinkedIn, Teams, Slack, search results):
# a description and the tool's screenshot from pkgdown/assets/screenshots
descriptions <- c(
  simulator = paste("Simulate claims with Pareto tails, caps and reinsurance layers, with return periods",
                    "and a report. Free, in your browser: your data stays on your computer."),
  distribution = paste("Fit claim counts and sizes, with reporting thresholds and policy limits, and compare",
                       "the fits. Free, in your browser: your data stays on your computer."),
  glm = paste("Fit GLMs with splines, stepwise selection or the lasso, and download the rate table.",
              "Free, in your browser: your data stays on your computer."),
  reserving = paste("Chain ladder, Mack, Bornhuetter-Ferguson, bootstrap, IFRS 17 and Solvency II reserves.",
                    "Free, in your browser: your data stays on your computer."),
  reinsurance = paste("Price excess of loss layers and compare reinsurance programmes by cost and capital.",
                      "Free, in your browser: your data stays on your computer."),
  portfolio = paste("Join lines of business with a copula: VaR, TVaR, diversification and the Solvency II",
                    "standard formula. Free, in your browser: your data stays on your computer.")
)
screenshots <- c(simulator = "simulator", distribution = "distribution", glm = "glm-rates",
                 reserving = "reserving", reinsurance = "reinsurance", portfolio = "portfolio")
site <- sub("wasm/?$", "", repo)
html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}
page_head <- function(tool) {
  meta <- function(attr, name, value) sprintf("<meta %s=\"%s\" content=\"%s\">", attr, name, html_escape(value))
  c(meta("name", "description", descriptions[[tool]]),
    meta("property", "og:title", titles[[tool]]),
    meta("property", "og:description", descriptions[[tool]]),
    meta("property", "og:url", paste0(site, "apps/", tool, "/")),
    meta("property", "og:image", paste0(site, "screenshots/", screenshots[[tool]], ".png")),
    meta("property", "og:image:alt", paste("A screenshot of", titles[[tool]])),
    meta("name", "twitter:card", "summary_large_image"),
    # the website's icons, two folders up
    "<link rel=\"icon\" type=\"image/png\" sizes=\"96x96\" href=\"../../favicon-96x96.png\">",
    "<link rel=\"icon\" type=\"image/svg+xml\" href=\"../../favicon.svg\">",
    "<link rel=\"icon\" sizes=\"any\" href=\"../../favicon.ico\">",
    "<link rel=\"apple-touch-icon\" sizes=\"180x180\" href=\"../../apple-touch-icon.png\">")
}

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
  html <- append(html, c(page_head(tool), goatcounter), after = head_end - 1)
  writeLines(html, page)
}

cat("Exported", length(tools), "tools to", out, "\n")
