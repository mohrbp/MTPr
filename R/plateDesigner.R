#' R Shiny App to Design Plate formats
#'
#' @export
designPlate <- function(){

  require(shiny)

  ui <- shiny::fluidPage(
    plateDesigner_UI("Setup"),
  )

  server <- function(input, output, session) {

    r <- plateDesigner_Server("Setup",
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
#'
plateDesigner_UI <- function(id, label = "Setup") {
  ns <- shiny::NS(id)
  tabPanel(title = "Destination Plate Setup",
           fluidRow(
             column(3,
                    numericInput(inputId = ns("rxn.Count"),
                                 label = "Number of reactions",
                                 value = 1,
                                 step = 1)
             ),
           ),
           br(),
           fluidRow(
             column(3,
                    selectInput(inputId = ns("destplate"),
                                label = "Destination Plate Type",
                                choices = c("384" , "96", "48", "24", "12")
                    )
             ),
             column(3,
                    selectInput(inputId = ns("deststartwell"),
                                label = "Starting Well",
                                choices = c(mtpR::to.well(1:384, wise = "col", plate = 384))
                    )
             ),
             column(3,
                    numericInput(inputId = ns("wellspacing"),
                                 label = "Spacing between Wells",
                                 value = 1,
                                 min = 1,
                                 max = 5,
                                 step = 1)
             ),
             column(3,
                    selectInput(inputId = ns("fillwise"),
                                label = "Work by Rows or Columns?",
                                choices = c(Rows = "row",
                                            Columns = "col")
                    )
             ),
           ),
           br(),
           fluidRow(
             column(3,
                    numericInput(inputId = ns("replicatecount"),
                                 label = "Number of Replicates",
                                 value = 3,
                                 min = 1)
             ),
             column(3,
                    selectInput(inputId = ns("replicatestyle"),
                                label = "Replicate Organization",
                                choices = c("Together", "EveryOther")

                    )
             ),
             column(3,
                    numericInput(inputId = ns("platereplicates"),
                                 label = "Number of plate replicates",
                                 value = 1,
                                 step = 1)
             )
           ),
           actionButton(ns("assignToDest"),
                        label = "Generate Destination Plate Map"),
           br(),
           radioButtons(inputId = ns("toggleDestOutput"),
                        label = "Select Destination Plate Output Type",
                        choices = c("Plot", "Table")),
           br(),
           plotOutput(outputId = ns('destPlatePlot')),
           DT::dataTableOutput(outputId = ns('destPlateMap'))
  )
}

#' @export
#'
plateDesigner_Server <- function(id,
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
        # Generate df of Replicates tied to Rxn.Numbers from the Expanded Reactions
        myReactives$Reactions.Dest <-
          data.frame (Rxn.Index = (seq(length.out = input$rxn.Count))) %>%
          AddReplicates(Replicate.Number.Total = input$replicatecount) %>%
          WellAssigner(Rxn.Count = input$rxn.Count,
                       DestPlate.Size = as.numeric(input$destplate),
                       Well.Position.Start = input$deststartwell,
                       Well.Spacing = input$wellspacing,
                       Replicate.Number.Total = input$replicatecount,
                       Replicate.Style = input$replicatestyle,
                       Fill.Wise = input$fillwise)  %>%
          dplyr::mutate(Rxn.Number = Rxn.Index) %>%
          dplyr::select(-(Rxn.Index)) %>%
          AddPlateReplicates(PlateReplicate.Number.Total = input$platereplicates) %>%
          dplyr::mutate(`Plate Name` = stringr::str_glue("DestPlate_{Plate.Index}_{PlateReplicate.Number}"))

      })

      output$destPlatePlot <- shiny::renderPlot({
        req(myReactives$Reactions.Dest)
        if(input$toggleDestOutput == "Plot"){
          myReactives$Reactions.Dest |>
            plotPlate_facet(fill = ("as.factor(Rxn.Number)"),
                            wellID = Well.Position,
                            facetrows = Plate.Index,
                            facetcols = PlateReplicate.Number,
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


WellAssigner <- function(Reactions.Replicates,
                         Rxn.Count,
                         DestPlate.Size = DestPlate.Size,
                         Well.Position.Start = Well.Position.Start,
                         Well.Spacing = Well.Spacing,
                         Replicate.Number.Total = Replicate.Number.Total,
                         Replicate.Style = Replicate.Style,
                         Fill.Wise = Fill.Wise) {

  ## Identify the starting position

  mtpR::to.num(Well.Position.Start,
               plate = DestPlate.Size,
               wise = Fill.Wise) -> Well.Index.Start

  ## Identify the total number of wells to be used


  if (Replicate.Style == "Together"){

    Well.Locations <- data.frame(Well.Index = seq(from = Well.Index.Start,
                                                  length.out = (Rxn.Count),
                                                  by = Well.Spacing + Replicate.Number.Total - 1) %>%
                                   sequence(nvec = rep(Replicate.Number.Total, length(.)),
                                            from = .)) %>%
      add_plates(Well.Index.ColumnID = Well.Index,
                 Plate.Size = DestPlate.Size) %>%
      dplyr::mutate(Well.Position = mtpR::to.well(num = Well.Index,
                                                  wise = Fill.Wise,
                                                  plate = DestPlate.Size)
      )

    Reactions.Wells <- Reactions.Replicates %>%
      dplyr::arrange(Rxn.Index, Replicate.Number) %>%
      cbind("Well.Position" = Well.Locations$Well.Position,
            "Plate.Index" = Well.Locations$Plate.Index)
  }
  else {

    Well.Locations <- data.frame(Well.Index = seq(from = Well.Index.Start,
                                                  length.out = (Rxn.Count * Replicate.Number.Total),
                                                  by = Well.Spacing)) %>%
      mtpR::add_plates(Well.Index.ColumnID = Well.Index,
                       Plate.Size = DestPlate.Size) %>%
      dplyr::mutate(Well.Position = to.well(num = Well.Index,
                                            wise = Fill.Wise,
                                            plate = DestPlate.Size)
      )


    Reactions.Wells <- Reactions.Replicates %>%
      dplyr::arrange(Replicate.Number, Rxn.Index) %>%
      cbind("Well.Position" = Well.Locations$Well.Position,
            "Plate.Index" = Well.Locations$Plate.Index)
  }

  return(Reactions.Wells)

}

add_plates <- function(dat,
                       Well.Index.ColumnID,
                       Plate.Size
                       ) {

  dat.new <- dat %>%
    dplyr::mutate(Plate.Index = ceiling({{Well.Index.ColumnID}} / Plate.Size),
           Well.Index.New = {{Well.Index.ColumnID}} - Plate.Size * (Plate.Index - 1),
           Well.Index = Well.Index.New) %>%
    dplyr::select(-c(Well.Index.New))

  return(dat.new)

}

AddReplicates <- function(Reactions,
                          Replicate.Number.Total) {

  Reactions.Replicates <- NULL
  for (Replicate.Number in 1:Replicate.Number.Total){
    Reactions %>%
      dplyr::bind_cols(Replicate.Number = Replicate.Number) %>%
      dplyr::bind_rows(Reactions.Replicates) -> Reactions.Replicates
  }

  return(Reactions.Replicates)
}

AddPlateReplicates <- function(Reactions,
                               PlateReplicate.Number.Total) {

  Reactions.PlateReplicates <- Reactions %>%
    tidyr::uncount(weights = PlateReplicate.Number.Total,
            .remove = FALSE,
            .id = "PlateReplicate.Number")

  return(Reactions.PlateReplicates)
}

