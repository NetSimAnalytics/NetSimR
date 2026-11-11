##############
#The fonts of the tools
##############

# The tools are set in Inter, with JetBrains Mono for code in the GLM tool. Both come with
# the package (inst/fonts), so that a tool asks no other server for them: not when it runs
# from R, where it needs no internet connection, and not on the website. The files are the
# variable fonts of the Fontsource packages @fontsource-variable/inter and
# @fontsource-variable/jetbrains-mono, version 5.3.0, split by script as Google Fonts splits
# them; a browser downloads a file only when the page has a character of its range. Both
# fonts are under the SIL Open Font License 1.1 (inst/fonts/LICENSE-*.txt).

# the scripts bundled, each with the characters its file holds
netsimr_font_ranges <- c(
  cyrillic = "U+0301,U+0400-045F,U+0490-0491,U+04B0-04B1,U+2116",
  greek = "U+0370-0377,U+037A-037F,U+0384-038A,U+038C,U+038E-03A1,U+03A3-03FF",
  `latin-ext` = paste0("U+0100-02BA,U+02BD-02C5,U+02C7-02CC,U+02CE-02D7,U+02DD-02FF,U+0304,U+0308,U+0329,U+1D00-1DBF,",
                       "U+1E00-1E9F,U+1EF2-1EFF,U+2020,U+20A0-20AB,U+20AD-20C0,U+2113,U+2C60-2C7F,U+A720-A7FF"),
  latin = paste0("U+0000-00FF,U+0131,U+0152-0153,U+02BB-02BC,U+02C6,U+02DA,U+02DC,U+0304,U+0308,U+0329,U+2000-206F,",
                 "U+20AC,U+2122,U+2191,U+2193,U+2212,U+2215,U+FEFF,U+FFFD")
)

# the families: the name the tools' styles use, the files' prefix and the weights of the variable font
netsimr_font_families <- list(
  list(family = "Inter", file = "inter", weight = "100 900"),
  list(family = "JetBrains Mono", file = "jetbrains-mono", weight = "100 800")
)

# the path the fonts are served under, relative to the page of a tool
netsimr_fonts_path <- "netsimr-fonts"

# The @font-face rules of the bundled fonts, with the code font when `code` is TRUE.
netsimr_font_css <- function(code = FALSE) {
  families <- if (code) netsimr_font_families else netsimr_font_families[1]
  rules <- lapply(families, function(f) {
    vapply(names(netsimr_font_ranges), function(script) {
      paste0("@font-face {\n  font-family: '", f$family, "';\n  font-style: normal;\n  font-display: swap;\n  font-weight: ",
             f$weight, ";\n  src: url('", netsimr_fonts_path, "/", f$file, "-", script,
             "-wght-normal.woff2') format('woff2');\n  unicode-range: ", netsimr_font_ranges[[script]], ";\n}")
    }, character(1))
  })
  paste(unlist(rules), collapse = "\n")
}

# For the head of a tool's page: serves the bundled fonts and declares them. Without the
# files (they are part of the installed package) the page uses the fonts of the computer.
netsimr_fonts_head <- function(code = FALSE) {
  dir <- system.file("fonts", package = "NetSimR")
  if (!nzchar(dir)) return(NULL)
  shiny::addResourcePath(netsimr_fonts_path, dir)
  tags$style(HTML(netsimr_font_css(code)))
}
