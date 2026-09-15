#the GLM fitting tool: what a downloaded model carries, failed imports, comparing stored models,
#the column choices after an import and the chart device, driven through shiny::testServer()

#insurance-like data: an ID, rating factors, an exposure, a weight, a count and an amount
glm_claims_data <- function(n = 3000) {
  set.seed(21)
  d <- data.frame(policy_id = sprintf("P%05d", seq_len(n)), age = sample(18:80, n, TRUE),
                  region = sample(c("North", "South", "East", "West"), n, TRUE),
                  exposure = round(runif(n, 0.2, 1), 3), weight = sample(1:4, n, TRUE), stringsAsFactors = FALSE)
  d$claim_count <- rpois(n, d$exposure * exp(-2 + 0.01 * d$age))
  d$claim_amount <- ifelse(d$claim_count > 0, round(rgamma(n, 2, 0.001), 2), NA)
  d$age[5] <- NA
  d
}

glm_write_csv <- function(d) {
  path <- tempfile(fileext = ".csv")
  write.csv(d, path, row.names = FALSE)
  path
}

#the bytes of an RDS file once decompressed
rds_bytes <- function(path) {
  con <- gzfile(path, "rb")
  on.exit(close(con))
  bytes <- raw(0)
  repeat {
    chunk <- readBin(con, "raw", 1e6)
    if (length(chunk) == 0) break
    bytes <- c(bytes, chunk)
  }
  bytes
}

test_that("GLM fitting tool downloads a model without the Shiny session or the password", {
  d <- glm_claims_data()
  path <- glm_write_csv(d)
  password <- "S3cretPW-glm-test"
  #the same model fitted with glm(), with nothing but the model in its file (a formula made here
  #would keep this test's environment)
  reference <- glm(stats::as.formula("claim_count ~ offset(log(exposure)) + age + region", env = globalenv()),
                   family = poisson, data = d, weights = weight, na.action = na.exclude)
  plain <- tempfile(fileext = ".rds")
  saveRDS(reference, plain)
  shiny::testServer(GLMFittingToolServer, {
    #a password typed on the Data tab before a CSV file is imported
    session$setInputs(data_source = "Database", db_type = "MySQL", db_user = "someone", db_password = password)
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = path, name = "claims.csv"), submit = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "log",
                      offset = "exposure", offset_log = TRUE, weights = "weight", formula = "age + region", fit_model = 1)
    model <- fitted_model()
    expect_s3_class(model, "glm")
    #the file of the Model (RDS) download
    file <- output$download_model
    bytes <- rds_bytes(file)
    expect_length(grepRaw(password, bytes, fixed = TRUE, all = TRUE), 0)
    expect_length(grepRaw("ShinySession", bytes, fixed = TRUE, all = TRUE), 0)
    #nor the server's own variables, which would be saved with its environment
    expect_length(grepRaw("fit_result", bytes, fixed = TRUE, all = TRUE), 0)
    expect_length(grepRaw("selected_data", bytes, fixed = TRUE, all = TRUE), 0)
    #about the size of the same model fitted with glm()
    expect_lt(file.size(file), 5 * file.size(plain))
    expect_lt(length(serialize(model$family, NULL)), 1e5)
    #the saved model predicts as glm() does, keeps a row for each row of the data (NA where
    #age is missing) and shows the real call in its summary
    saved <- readRDS(file)
    newdata <- d[1:50, ]
    expect_equal(predict(saved, newdata, type = "response"), predict(reference, newdata, type = "response"))
    expect_equal(coef(saved), coef(reference))
    expect_length(fitted(saved), nrow(d))
    expect_true(is.na(fitted(saved)[5]))
    call_text <- paste(deparse(saved$call), collapse = "")
    expect_match(call_text, "poisson(link = \"log\")", fixed = TRUE)
    expect_match(call_text, "weights = weight", fixed = TRUE)
    expect_match(paste(capture.output(summary(saved)), collapse = "\n"), "offset(log(exposure))", fixed = TRUE)
    #update() refits it given the data, as data = or as model_data
    expect_equal(coef(update(saved, . ~ . - region, data = d)), coef(update(reference, . ~ . - region)))
    model_data <- d
    expect_equal(coef(update(saved, . ~ . - region)), coef(update(reference, . ~ . - region)))
    #every family: nothing of the session in the model
    responses <- c(gaussian = "age", binomial = "region", Gamma = "claim_amount", inverse.gaussian = "claim_amount")
    for (family in names(responses)) {
      session$setInputs(response_variable = responses[[family]], glm_distribution = family,
                        link_function = glm_family_links[[family]][1], offset = "None", weights = "None",
                        formula = if (family == "binomial") "age" else "region", fit_model = 1 + match(family, names(responses)))
      model <- fitted_model()
      expect_s3_class(model, "glm")
      bytes <- serialize(model, NULL)
      expect_length(grepRaw("ShinySession", bytes, fixed = TRUE, all = TRUE), 0)
      expect_length(grepRaw(password, bytes, fixed = TRUE, all = TRUE), 0)
    }
  })
})

test_that("GLM fitting tool keeps the data it has when an import fails", {
  d <- glm_claims_data(200)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "claims.csv"), submit = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "log",
                      offset = "None", weights = "None", formula = "age", fit_model = 1)
    model <- fitted_model()
    #a database import that fails
    session$setInputs(data_source = "Database", db_type = "SQLite", db_name = file.path(tempdir(), "no_such_database.sqlite"),
                      sql_query = "SELECT 1", submit = 2)
    expect_equal(nrow(selected_data()), 200)
    expect_equal(names(selected_data()), names(d))
    expect_match(output$selected_input_data_table$html, "P00001", fixed = TRUE)
    expect_match(output$data_overview$html, "The last import failed", fixed = TRUE)
    expect_match(output$data_overview$html, "claims.csv", fixed = TRUE)
    #the model and its data still go together, and fitting again works
    expect_identical(fitted_model(), model)
    session$setInputs(formula = "age + region", fit_model = 2)
    expect_equal(stats::nobs(fitted_model()), sum(!is.na(d$age)))
    #an import that works replaces the data and the note
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d[1:50, ]), name = "small.csv"), submit = 3)
    expect_equal(nrow(selected_data()), 50)
    expect_no_match(output$data_overview$html, "The last import failed", fixed = TRUE)
  })
  #a failed first import still leaves no data
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", submit = 1)
    expect_null(selected_data())
  })
})

test_that("GLM fitting tool compares the AIC of stored models only with the same weights", {
  d <- glm_claims_data(500)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "claims.csv"), submit = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "log",
                      offset = "None", weights = "None", formula = "age", fit_model = 1, save_formula_1 = 1)
    session$setInputs(weights = "weight", fit_model = 2, save_formula_2 = 1)
    both <- paste(output$aic_output_1$html, output$aic_output_2$html)
    expect_no_match(both, "lower", fixed = TRUE)
    expect_match(output$aic_output_1$html, "Not comparable", fixed = TRUE)
    expect_match(output$aic_output_2$html, "different response, observations or weights", fixed = TRUE)
    #the same weights: comparable, and the lower AIC is marked
    session$setInputs(formula = "age + region", fit_model = 3, save_formula_1 = 2)
    both <- paste(output$aic_output_1$html, output$aic_output_2$html)
    expect_match(both, "lower", fixed = TRUE)
    expect_no_match(both, "Not comparable", fixed = TRUE)
  })
})

test_that("GLM fitting tool chooses a response and a chart variable that suit the data", {
  d <- glm_claims_data(300)
  #the counts for the default families, the amounts for the Gamma family; never the ID or the age
  expect_equal(glm_default_response(d, "gaussian"), "claim_count")
  expect_equal(glm_default_response(d, "poisson"), "claim_count")
  expect_equal(glm_default_response(d, "Gamma"), "claim_amount")
  choices <- glm_column_choices(d, "poisson")
  expect_equal(choices, list(response_variable = "claim_count", offset = "None", weights = "None", visualize_variable = "age"))
  #a numeric ID, in any order, is not the chart variable either
  d_numeric <- d
  d_numeric$policy_id <- sample(100001:100300)
  expect_equal(glm_column_choices(d_numeric, "poisson")$visualize_variable, "age")
  #the words of the name, whatever the case and separators; not a word that only contains one
  expect_equal(glm_default_response(data.frame(account_type = 1:3 %% 2, ClaimCount = c(0, 1, 0), x = c(2.5, 1, 3)), "poisson"), "ClaimCount")
  expect_equal(glm_default_response(data.frame(a = rnorm(5), Y = rnorm(5), b = rnorm(5)), "gaussian"), "Y")
  #no name suggests a response: the last numeric column that is not an ID
  plain <- data.frame(id = 1:40, grp = rep(c("a", "b"), 20), x = rnorm(40), z = rnorm(40), row = 40:1)
  expect_equal(glm_default_response(plain, "gaussian"), "z")
  expect_equal(glm_default_variable(plain, "z"), "grp")
  #text with too many values to draw is skipped, and a column already chosen is not the variable
  many <- data.frame(postcode = rep(sprintf("PC%03d", 1:150), 4), weight = 1, age = sample(18:80, 600, TRUE), claims = rpois(600, 1))
  expect_equal(glm_column_choices(many, "poisson", list(weights = "weight"))$visualize_variable, "age")
  #the choices already made are kept when the data has the column
  kept <- glm_column_choices(d, "poisson", list(response_variable = "age", offset = "exposure", visualize_variable = "region"))
  expect_equal(kept, list(response_variable = "age", offset = "exposure", weights = "None", visualize_variable = "region"))
  expect_equal(glm_column_choices(d, "poisson", list(response_variable = "not_a_column"))$response_variable, "claim_count")
  #a text response only for the binomial family
  yes_no <- data.frame(claim = sample(c("yes", "no"), 30, TRUE), age = rnorm(30))
  expect_equal(glm_default_response(yes_no, "binomial"), "claim")
  expect_equal(glm_default_response(yes_no, "poisson"), "age")
  #what counts as an ID
  expect_true(glm_id_like(sprintf("P%03d", 1:100)))
  expect_true(glm_id_like(sample(1:100)))
  expect_false(glm_id_like(sample(18:80, 100, TRUE)))
  expect_false(glm_id_like(round(rnorm(100, 5000, 2000), 2)))
  expect_false(glm_id_like(sample(1e6, 100)))
  expect_false(glm_id_like(rep(c("a", "b"), 50)))
  expect_false(glm_id_like(c(1, Inf, 3)))
  #the server applies them after an import without failing
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "claims.csv"), submit = 1)
    expect_equal(nrow(selected_data()), 300)
  })
})

test_that("GLM fitting tool draws its chart with the tools' plot device and redraws it on resize", {
  expect_false(any(grepl("(^|[^_[:alnum:]])renderPlot\\(", deparse(GLMFittingToolServer))))
  d <- glm_claims_data(300)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = glm_write_csv(d), name = "claims.csv"), submit = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "log",
                      offset = "None", weights = "None", formula = "age", fit_model = 1)
    session$setInputs(visualize_variable = "region", number_of_bands_input = 10, execute_visualization = 1,
                      .clientdata_output_fitness_plot_width = 900, .clientdata_output_fitness_plot_height = 520)
    wide <- output$fitness_plot
    expect_match(wide$src, "^data:image/png;base64,")
    expect_match(wide$alt, "region")
    session$setInputs(app_theme = "dark", .clientdata_output_fitness_plot_width = 400)
    narrow <- output$fitness_plot
    expect_match(narrow$src, "^data:image/png;base64,")
    expect_false(identical(narrow$src, wide$src))
  })
})
