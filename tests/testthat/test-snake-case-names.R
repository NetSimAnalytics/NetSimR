#the snake_case names of the older CamelCase functions are the same functions

test_that("each snake_case name is its CamelCase function, exported", {
  pairs <- c(
    upper_incomplete_gamma = "IGamma", gamma_capped_mean = "GammaCappedMean", exposure_curve_gamma = "ExposureCurveGamma",
    ilf_gamma = "ILFGamma", pure_ibnr_gamma = "PureIBNRGamma", lnorm_capped_mean = "LNormCappedMean",
    exposure_curve_lnorm = "ExposureCurveLNorm", ilf_lnorm = "ILFLNorm", pure_ibnr_lnorm = "PureIBNRLNorm",
    pareto_capped_mean_calc = "ParetoCappedMeanCalc", pareto_capped_mean = "ParetoCappedMean",
    exposure_curve_pareto = "ExposureCurvePareto", ilf_pareto = "ILFPareto",
    sliced_gamma_pareto_mean = "SlicedGammaParetoMean", sliced_gamma_pareto_capped_mean = "SlicedGammaParetoCappedMean",
    exposure_curve_sliced_gamma_pareto = "ExposureCurveSlicedGammaPareto", ilf_sliced_gamma_pareto = "ILFSlicedGammaPareto",
    psliced_gamma_pareto = "pSlicedGammaPareto", qsliced_gamma_pareto = "qSlicedGammaPareto",
    dsliced_gamma_pareto = "dSlicedGammaPareto", sliced_lnorm_pareto_mean = "SlicedLNormParetoMean",
    sliced_lnorm_pareto_capped_mean = "SlicedLNormParetoCappedMean",
    exposure_curve_sliced_lnorm_pareto = "ExposureCurveSlicedLNormPareto", ilf_sliced_lnorm_pareto = "ILFSlicedLNormPareto",
    psliced_lnorm_pareto = "pSlicedLNormPareto", qsliced_lnorm_pareto = "qSlicedLNormPareto",
    dsliced_lnorm_pareto = "dSlicedLNormPareto"
  )
  exported <- getNamespaceExports("NetSimR")
  for (new in names(pairs)) {
    expect_identical(get(new, envir = asNamespace("NetSimR")), get(pairs[[new]], envir = asNamespace("NetSimR")), label = new)
    expect_true(new %in% exported, label = new)
  }
  expect_equal(lnorm_capped_mean(1000, 6, 1.5), LNormCappedMean(1000, 6, 1.5))
})
