#' Server function for the Shiny Simulator application
#'
#' @param input Input for the server function.
#' @param output Output for the server function.
#' @param session Session for the server function.
#' @return Returns server rendering for the shiny application.
#' @import rmarkdown
#' @import shiny
#' @import future.apply
#' @import data.table
#' @importFrom future plan
#' @importFrom future sequential
#' @importFrom future multisession
#' @import rmarkdown
#' @import methods
#' @import stats
#' @import scales
#' @import utils
#' @import reactable
shiny_simulator_server = function(input, output, session) {
  #set data.table threads once per session, leave one core free for the main process
  data.table::setDTthreads(max(1, parallel::detectCores() - 1))

  #ensure any future workers are cleaned up when the session ends
  session$onSessionEnded(function() {
    future::plan(future::sequential)
  })

  #seed input
  output$seed_value <- renderUI({
    if (input$seedSetBinary) {
      sliderInput('seedValue', 'Seed value', min=1, max=100, value=1, step=1)
    }
  })

  #help function to render freq/sev parameters
  render_UI_param <- function(param_id, object){
    return(if(!is.na(object@paramIDs[param_id])){numericInput(
      inputId = object@paramIDs[param_id]
      ,label = object@param_labels[param_id]
      ,min = object@param_min_values[param_id]
      ,max = object@param_max_values[param_id]
      ,step = if (isTRUE(object@param_whole_numbers[param_id])) 1 else NA
      ,value = NULL
    )
    })}

  #render frequency parameters
  lapply(freq_dist_parameter_placeholders$param_number, function(i) {
    output[[paste0("freq_param_", i)]] <- renderUI({
      render_UI_param(param_id=i, object=freq_dist_options[[input$freqDistr]])
    })
  })

  #render severity parameters
  lapply(sev_dist_parameter_placeholders$param_number, function(i) {
    output[[paste0("sev_param_", i)]] <- renderUI({
      render_UI_param(param_id=i, object=sev_dist_options[[input$sevDistr]])
    })
  })

  #render Pareto slice parameters
  output$pareto_slice_times <- renderUI({
    if (input$paretoSlice) {
      selectInput("pareto_slice_times","Number of Pareto Slices",1:max_number_of_pareto_slices)
    }
  })

  lapply(1:(2*max_number_of_pareto_slices), function(i) {
    output[[paste0("slice_pareto_param_", i)]] <- renderUI({
      req(input$pareto_slice_times)
      if(input$paretoSlice){if(as.numeric(input$pareto_slice_times)*2>=i){
        numericInput(
          inputId = paste0("slice_pareto_param_", i)
          ,label = ifelse(i %% 2, paste("Sliced alpha", (1+i)/2), paste('Sliced x_m', i/2))
          ,value = NULL
          ,min = 0
        )
      }}
    })
  })

  #render severity cap amount
  output$sev_cap_amount <- renderUI({
    if (input$sevCapBinary) {
      numericInput('sev_cap_amount', 'Severity Cap Amount', value = NULL, min = 0)
    }
  })

  #reinsurance structure EEL inputs
  output$reinsuranceStructureDeductibleEEL <- renderUI({
    if (input$reinsuranceStructureEEL %in% c('Unlimited Layer', 'Exclude Layer', 'Limited Layer')) {
      numericInput('reinsurance_structure_eel_dedctible_amount', 'EEL Deductible Amount', value = NULL, min = 0)
    }
  })

  output$reinsuranceStructureLimitEEL <- renderUI({
    if (input$reinsuranceStructureEEL %in% c('Limited Layer', 'Exclude Layer')) {
      numericInput('reinsurance_structure_eel_limit_amount', 'EEL Limit Amount', value = NULL, min = 0)
    }
  })

  output$reinsuranceStructureLimitedReinstatements <- renderUI({
    if (input$reinsuranceStructureEEL %in% c('Limited Layer')) {
      checkboxInput('reinsuranceStructureLimitedReinstatements', 'Limited Reinstatments', value = F)
    }
  })

  output$reinsuranceStructureReinstatementLimit <- renderUI({
    req(input$reinsuranceStructureLimitedReinstatements)
    if (input$reinsuranceStructureEEL %in% c('Limited Layer')) {
      if(input$reinsuranceStructureLimitedReinstatements) {
        numericInput('reinsuranceStructureReinstatementLimit', 'Number of Reinstatements', value = NULL, min = 0)
      }
    }
  })

  #reinsurance structure AL inputs
  output$reinsuranceStructureDeductibleAL <- renderUI({
    if (input$reinsuranceStructureAL %in% c('Unlimited Layer', 'Exclude Layer', 'Limited Layer')) {
      numericInput('reinsurance_structure_al_dedctible_amount', 'AL Deductible Amount', value = NULL, min = 0)
    }
  })

  output$reinsuranceStructureLimitAL <- renderUI({
    if (input$reinsuranceStructureAL %in% c('Limited Layer', 'Exclude Layer')) {
      numericInput('reinsurance_structure_al_limit_amount', 'AL Limit Amount', value = NULL, min = 0)
    }
  })

  #create simulation data dataFrame reactive to enable download buttons & simulation settings list
  simulated_data <- reactiveValues(data=NULL)
  simulation_settings <- list()

  #run simulation button
  #the browser disables the button and shows "Running..." on click; this message re-enables it
  observeEvent(input$RunSimulations,{
    on.exit(session$sendCustomMessage("netsimr-run-finished", TRUE), add = TRUE)

    #collect the settings for this run
    pareto_slice_count <- if (is.null(input$pareto_slice_times)) 0 else as.numeric(input$pareto_slice_times)
    new_settings <- list(
      freq_params = unname(sapply(
        freq_dist_options[[input$freqDistr]]@paramIDs
        ,function(x) input[[x]]
      ))
      ,sev_params = unname(sapply(
        sev_dist_options[[input$sevDistr]]@paramIDs
        ,function(y) input[[y]]
      ))
      ,numOfSimulations = input$numberOfSimulations
      ,seedSetBinary = input$seedSetBinary
      ,seedValue = input$seedValue
      ,freqDistr = input$freqDistr
      ,sevDistr = input$sevDistr
      ,paretoSlice = input$paretoSlice
      ,pareto_slice_times = as.numeric(input$pareto_slice_times)
      ,slice_pareto_alphas = if(input$paretoSlice){unname(sapply(
        seq_len(pareto_slice_count)
        ,function(y) input[[paste0("slice_pareto_param_", y*2-1)]]
      ))}
      ,slice_pareto_x_ms = if(input$paretoSlice){unname(sapply(
        seq_len(pareto_slice_count)
        ,function(y) input[[paste0("slice_pareto_param_", y*2)]]
      ))}
      ,sevCapBinary = input$sevCapBinary
      ,sev_cap_amount = input$sev_cap_amount
      ,reinsuranceStructureEEL = input$reinsuranceStructureEEL
      ,reinsurance_structure_eel_dedctible_amount = input$reinsurance_structure_eel_dedctible_amount
      ,reinsurance_structure_eel_limit_amount = input$reinsurance_structure_eel_limit_amount
      ,reinsuranceStructureAL = input$reinsuranceStructureAL
      ,reinsurance_structure_al_dedctible_amount = input$reinsurance_structure_al_dedctible_amount
      ,reinsurance_structure_al_limit_amount = input$reinsurance_structure_al_limit_amount
      ,multiprocessing = input$multiprocessingBinary
      ,reinsuranceStructureLimitedReinstatements = input$reinsuranceStructureLimitedReinstatements
      ,reinsuranceStructureReinstatementLimit = input$reinsuranceStructureReinstatementLimit
    )

    #stop before running and tell the user which fields are empty
    missing_settings <- find_missing_simulation_settings(new_settings)
    if (length(missing_settings) > 0) {
      showNotification(
        tags$div(
          tags$strong("Please fix these settings before running:"),
          tags$ul(lapply(missing_settings, tags$li))
        ),
        type = "error",
        duration = 10,
        id = "missing_settings_notice"
      )
      return(NULL)
    }
    removeNotification("missing_settings_notice")

    simulation_settings <<- new_settings

    on.exit(gc(), add = TRUE)

    #run simmulations
    simulated_data$data <- tryCatch(
      {
        do.call(simulate_function, simulation_settings)
      }, error = function(cond) {
        future::plan(future::sequential)
        showNotification(
          paste("Error:", conditionMessage(cond)),
          type = "error",
          duration = NULL
        )
        print(cond)
        NULL
      }
    )
  })

  #the browser shows "Preparing report..." on click; the report handler re-enables the button
  output$downloadReportButton <- renderUI({
    req(simulated_data$data)
    downloadButton('downloadReportHandler', 'Report', icon = icon("file-lines"), class = "btn-outline-primary")
  })

  #download data button
  output$DownloadDataHandler <- downloadHandler(
    filename = function() { paste0('Simulation_data', '.csv') },
    content = function(file) {
      showNotification(
        "Preparing data file, save dialog will appear shortly...",
        type = "message",
        duration = 2
      )
      write.csv(simulated_data$data, file, row.names = FALSE)
    }
  )

  output$downloadDataButton <- renderUI({
    req(simulated_data$data)
    downloadButton('DownloadDataHandler', 'CSV data', icon = icon("file-csv"), class = "btn-outline-primary")
  })

  #download report button
  output$downloadReportHandler <- downloadHandler(
    filename = "simulation_report.html",
    content = function(file) {
      on.exit(session$sendCustomMessage("netsimr-report-finished", TRUE), add = TRUE)

      showNotification(
        "Preparing report, save dialog will appear shortly...",
        type = "message",
        duration = 3
      )

      shiny::withProgress(message = "Preparing report", value = 0, {
        incProgress(0.2, detail = "Copying template")
        tempReport <- normalizePath(file.path(tempdir(), "ShinySimulatorReport.Rmd"), mustWork = FALSE)
        file.copy(
          normalizePath(system.file("rmd", "ShinySimulatorReport.Rmd", package = "NetSimR")),
          tempReport,
          overwrite = TRUE
        )

        incProgress(0.5, detail = "Rendering report")
        report_params <- append(simulation_settings, list(
          total_claims_data = simulated_data$data$total_claims
          #readable names so the report can show e.g. "Negative Binomial: r = 2, beta = 1"
          ,freq_distr_label = freq_dist_options[[simulation_settings$freqDistr]]@distr_label
          ,sev_distr_label = sev_dist_options[[simulation_settings$sevDistr]]@distr_label
          ,freq_param_labels = freq_dist_options[[simulation_settings$freqDistr]]@param_labels
          ,sev_param_labels = sev_dist_options[[simulation_settings$sevDistr]]@param_labels
        ))
        #pass only the fields the template declares, so an older copy of the template still renders
        declared_params <- names(rmarkdown::yaml_front_matter(tempReport)$params)
        report_params <- report_params[names(report_params) %in% declared_params]

        tryCatch(
          rmarkdown::render(
            tempReport,
            output_file = file,
            quiet = TRUE,
            params = report_params,
            envir = new.env(parent = globalenv())
          ),
          error = function(cond) {
            #without this the download just fails, with the reason only in the R console
            showNotification(
              paste("The report could not be created:", conditionMessage(cond)),
              type = "error",
              duration = NULL
            )
            stop(cond)
          }
        )

        incProgress(1, detail = "Done")
      })
    }
  )

  #keep dynamic outputs rendering even while their empty containers are collapsed by the UI styles
  dynamic_outputs <- c(
    freq_dist_parameter_placeholders$param_id
    ,sev_dist_parameter_placeholders$param_id
    ,paste0("slice_pareto_param_", 1:(2*max_number_of_pareto_slices))
    ,"reinsuranceStructureDeductibleEEL", "reinsuranceStructureLimitEEL"
    ,"reinsuranceStructureDeductibleAL", "reinsuranceStructureLimitAL"
    ,"downloadDataButton", "downloadReportButton"
  )
  for (output_name in dynamic_outputs) {
    outputOptions(output, output_name, suspendWhenHidden = FALSE)
  }
}
