#' R Shiny App to Design Plate formats
#'
#' @export
design_plate <- function(){

  require(shiny)

  ui <- shiny::fluidPage(
    plate_designer_ui("Setup"),
  )

  server <- function(input, output, session) {

    r <- plate_designer_server("Setup",
                              assignToDest = reactive(input$assignToDest),
                              rxn.Count = reactive(input$rxn.Count),
                              replicatecount = reactive(input$replicatecount),
                              destplate = reactive(input$destplate),
                              deststartwell = reactive(input$deststartwell),
                              wellspacing = reactive(input$wellspacing),
                              replicatestyle = reactive(input$replicatestyle),
                              fillwise = reactive(input$fillwise),
                              platereplicates = reactive(input$platereplicates)
    )

  }

  shinyApp(ui, server)
}


#' @export
#' Plate Design UI Module
plate_designer_ui <- function(id, label = "Setup") {
  ns <- shiny::NS(id)
  tabPanel(
    title = "Destination Plate Setup",
    br(),
    fluidRow(
      column(
        3,
        numericInput(
          inputId = ns("rxn.Count"),
          label = "Number of reactions",
          value = 1,
          step = 1,
          min = 1
        )
      ),
      column(
        3,
        numericInput(
          inputId = ns("platereplicates"),
          label = "Number of plate replicates",
          value = 1,
          step = 1,
          min = 1
        )
      ),
      column(
        3,
        numericInput(
          inputId = ns("replicatecount"),
          label = "Number of Replicates",
          value = 3,
          min = 1
        )
      ),
    ),
    br(),
    fluidRow(
      column(
        3,
        selectInput(
          inputId = ns("destplate"),
          label = "Destination Plate Type",
          choices = c("384" , "96", "48", "24", "12")
        )
      ),
      column(
        3,
        selectInput(
          inputId = ns("deststartwell"),
          label = "Starting Well",
          choices = c(
            mtpR::convert_well(
              input = 1:384,
              direction = "to_well",
              wise = "col",
              plate = 384
            )
          )
        )
      ),
      column(
        3,
        selectInput(
          inputId = ns("fillwise"),
          label = "Work by Rows or Columns?",
          choices = c(Rows = "row",
                      Columns = "col")
        )
      ),
    ),
    br(),
    fluidRow(
      column(
        3,
        selectInput(
          inputId = ns("replicatestyle"),
          label = "Prioritize Reactions or Replicates",
          choices = c("reaction", "replicate")

        )
      ),
      column(
        3,
        numericInput(
          inputId = ns("interwell"),
          label = "Spacing across Wells",
          value = 1,
          min = 1,
          step = 1
        )
      ),
      column(
        3,
        numericInput(
          inputId = ns("intrawell"),
          label = "Spacing between Wells",
          value = 1,
          min = 1,
          step = 1
        )
      ),
    ),
    fluidRow(
      column(
        3,
        actionButton(ns("assignToDest"),
                   label = "Generate Destination Plate Map"),
        br(),
        br(),
        radioButtons(
          inputId = ns("toggleDestOutput"),
          label = "Select Destination Plate Output Type",
          choices = c("Plot", "Table")
        ),
    ),
    ),
    br(),
    plotOutput(outputId = ns('destPlatePlot')),
    DT::dataTableOutput(outputId = ns('destPlateMap'))
  )
}

#' @export
#'
plate_designer_server <- function(id,
                                 assignToDest,
                                 rxn.Count,
                                 replicatecount,
                                 destplate,
                                 deststartwell,
                                 wellspacing,
                                 replicatestyle,
                                 fillwise,
                                 platereplicates
                                 ) {
  moduleServer(
    id,
    # Below is the module function
    function(input, output, session) {

      myReactives <- reactiveValues(Reactions.Dest = NULL)

      observeEvent(input$assignToDest, {
        req(input$rxn.Count)
        myReactives$Reactions.Dest <- data.frame(reaction_count = seq_len(input$rxn.Count)) |>
          replicate_reactions(reaction_col_name = "reaction_count",
                              num_replicates = input$replicatecount,
                              priority = input$replicatestyle,
                              inter_spacing = input$interwell,
                              intra_spacing = input$intrawell,
                              start_position = mtpR::convert_well(input = input$deststartwell,
                                                                  direction = "to_num",
                                                                  wise = input$fillwise,
                                                                  plate = 384)) |>
          add_plates(well_index_column = "position",
                     plate_size = as.numeric(input$destplate)) |>
          mutate(Well.Position = mtpR::convert_well(Well.Index,
                                                    direction = "to_well",
                                                    wise = input$fillwise,
                                                    plate = as.numeric(input$destplate)
                                                    )
                 ) |>
          add_plate_replicates(plate_replicate_number_total = input$platereplicates) |>
          dplyr::mutate(`Plate Name` = stringr::str_glue("DestPlate_{Plate.Index}_{PlateReplicate.Number}"))

      })

      output$destPlatePlot <- shiny::renderPlot({
        req(myReactives$Reactions.Dest)
        if(input$toggleDestOutput == "Plot"){
          myReactives$Reactions.Dest |>
            plot_plate(fill = "reaction_count",
                        well_id = "Well.Position",
                        facet_rows = "Plate.Index",
                        facet_cols = "PlateReplicate.Number",
                        plate = as.numeric((input$destplate))) +
            ggplot2::labs(fill = "Reaction Number")
        }
      })

      output$destPlateMap <- DT::renderDataTable({
        req(myReactives$Reactions.Dest)
        if(input$toggleDestOutput == "Table"){
          myReactives$Reactions.Dest |>
            DT::datatable(
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                pageLength = -1
              )
            )
        }
      })
      # return(myReactives$Reactions.Dest)
    }
  )
}
