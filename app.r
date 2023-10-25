ui <- function(request) {

  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Creel Survey App"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "tabs",
        shinydashboard::menuItem(
          "Create Survey Schedule",
          tabName = "new_cal"
        )
      ),
      collapsed = TRUE
    ),
    shinydashboard::dashboardBody(
      shinyjs::useShinyjs(),
      req_input_css_tag(),
      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(
          width = 8,
          shinyglide::glide(
            screen1_ui,
            screen2_ui,
            screen3_ui,
            screen4_ui,
            screen5_ui,
            screen6_ui
          )
        ),
        shiny::column(width = 2)
      )
    )
  )

}

server <- function(input, output, session) {
  .data <- rlang::.data

  ##############################################################################
  #
  # reactive values
  #
  ##############################################################################
  rct <- shiny::reactiveValues(
    # survey coordinates
    lat = 43.0760,
    lng = -107.2903,
    # survey date data
    dates = NULL,
    # survey time data
    times = NULL,
    # current active date in dates calendar
    def_date = NULL
  )

  rtz <- shiny::reactive({
    lutz::tz_lookup_coords(rct$lat, rct$lng, "accurate")
  })

  ##############################################################################
  #
  # validation functions
  #
  ##############################################################################
  # check that all inputs needed to generate survey dates are provided
  date_check <- shiny::reactive({

    date_inputs <- list(
      input$sd,
      input$int,
      input$int_units,
      input$n_int
    )

    all(sapply(date_inputs, shiny::isTruthy))

  })

  # check that all inputs needed to generate survey times are provided
  time_check <- shiny::reactive({

    time_inputs <- list(
      rct$dates,
      rct$lat,
      rct$lng,
      input$es,
      input$ls,
      input$spd,
      input$wd,
      input$we
    )

    all(sapply(time_inputs, shiny::isTruthy))

  })

  ##############################################################################
  #
  # input processing/data generation functions
  #
  ##############################################################################
  # generate survey dates based on input values
  shiny::observe({
    shiny::req(date_check())

    rct$dates <- gen_survey_dates(
      input$sd,
      input$int,
      input$int_units,
      input$n_int
    )

  })

  # generate survey times based on input values
  shiny::observe({
    shiny::req(time_check())

    rct$times <- gen_survey_times(
      rct$dates,
      rct$lat,
      rct$lng,
      input$es,
      input$ls,
      input$spd,
      input$wd,
      input$we
    )

  })

  ##############################################################################
  #
  # screen 1 reactivity
  #
  ##############################################################################
  # survey location picker map
  output$ll_map <- leaflet::renderLeaflet({

    leaflet::leaflet(wy_bbox) |>
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopo) |>
      leaflet::addPolygons(group = "wy") |>
      leaflet::addMarkers(
        lng = -107.2903,
        lat = 43.0760,
        group = "creel_marker"
      ) |>
      leaflet::hideGroup("wy")

  })

  # update reactive values on map click
  shiny::observeEvent(input$ll_map_click, {

    rct$lat <- input$ll_map_click$lat
    rct$lng <- input$ll_map_click$lng

  })

  # update marker location in leaflet
  shiny::observe({

    leaflet::leafletProxy("ll_map") |>
      leaflet::clearGroup("creel_marker") |>
      leaflet::addMarkers(
        lng = rct$lng,
        lat = rct$lat,
        group = "creel_marker"
      )

  })

  # display click coordinates
  output$ll <- shiny::renderUI({

    shiny::div(
      shiny::span(
        shiny::tags$strong("Survey coordinates: "),
        paste(rct$lat, rct$lng, sep = ", ")
      ),
      style = "margin-top: 12px;"
    )

  })

  # show/hide asterisk when survey name changes
  shiny::observeEvent(input$sname, {

    shinyjs::toggleClass(
      "sname-req",
      "required-input",
      !(length(input$sname) > 0 && nchar(input$sname) > 0)
    )

  })

  ##############################################################################
  #
  # screen 2 reactivity
  #
  ##############################################################################

  # show/hide asterisk when start date changes
  shiny::observeEvent(input$sd, {

    rct$def_date <- input$sd

    shinyjs::toggleClass(
      "sd-req",
      "required-input",
      !(length(input$sd) > 0 && nchar(input$sd) > 0)
    )

  })

  # show/hide asterisk when stratum length changes
  shiny::observeEvent(input$int, {

    shinyjs::toggleClass(
      "int-req",
      "required-input",
      !(length(input$int) > 0 && input$int > 0)
    )

  })

  # show/hide asterisk when length unit changes and update sliders
  shiny::observeEvent(input$int_units, {

    shinyjs::toggleClass(
      "int_units-req",
      "required-input",
      !(length(input$int_units) > 0 && nchar(input$int_units) > 0)
    )

    shiny::req(input$int_units)

    # update number of strata slider
    sl_value <- dplyr::case_when(
      input$int_units == "Days" ~ 6L,
      input$int_units == "Weeks" ~ 6L,
      input$int_units == "Months" ~ 3L
    )

    sl_max <- dplyr::case_when(
      input$int_units == "Days" ~ 30L,
      input$int_units == "Weeks" ~ 26L,
      input$int_units == "Months" ~ 12L
    )

    shiny::updateSliderInput(
      session = session,
      "n_int",
      value = sl_value,
      max = sl_max
    )

    # update number of weekdays per strata slider
    sl_value <- dplyr::case_when(
      input$int_units == "Days" ~ 2L,
      input$int_units == "Weeks" ~ 2L,
      input$int_units == "Months" ~ 4L
    )

    sl_max <- dplyr::case_when(
      input$int_units == "Days" ~ 10L,
      input$int_units == "Weeks" ~ 15L,
      input$int_units == "Months" ~ 20L
    )

    shiny::updateSliderInput(
      session = session,
      "wd",
      value = sl_value,
      max = sl_max
    )

    # update number of weekend days per strata slider
    shiny::updateSliderInput(
      session = session,
      "we",
      value = sl_value,
      max = sl_max
    )

  })

  # no asterisk for sliders, can't be set to NULL/""/etc

  ##############################################################################
  #
  # screen 3 reactivity
  #
  ##############################################################################
  # survey date calendar
  output$survey_dates_cal <- toastui::renderCalendar({
    shiny::req(date_check())

    rct$dates |>
      survey_dates_to_tui() |>
      toastui::calendar(
        defaultDate = rct$def_date,
        navigation = TRUE,
        useDetailPopup = FALSE,
        navOpts = toastui::navigation_options(
          today_label = "Today",
          prev_label = "Previous Month",
          next_label = "Next Month",
          fmt_date = "MMM DD, YYYY"
        )
      ) |>
      toastui::cal_props(as.data.frame(tui_calendars)) |>
      toastui::cal_month_options(isAlways6Week = FALSE) |>
      toastui::cal_timezone(rtz()) |>
      toastui::cal_events(
        clickSchedule = htmlwidgets::JS(
          "function(event) {",
            "Shiny.setInputValue(
              'survey_dates_cal_click',
              {
                id: event.schedule.id,
                calendarId: event.schedule.calendarId,
                start: event.schedule.start._date
              }
            );",
          "}"
        )
      )

  })

  # modal for changing category
  shiny::observeEvent(input$survey_dates_cal_click, {
    shiny::req(input$survey_dates_cal_click$calendarId < 5)

    rct$def_date <- as.Date(input$survey_dates_cal_click$start)

    shiny::showModal(shiny::modalDialog(
      title = "Edit Category",
      shiny::selectInput(
        "date_cat",
        "New Category:",
        choices = tui_calendars |>
          dplyr::filter(.data$id < 5) |>
          dplyr::mutate(weekend = .data$id - 1) |>
          dplyr::pull(.data$weekend, name = .data$name),
        selected = 1
      ),
      shinyjs::hidden(shiny::actionButton(
        "save_date_cat",
        "Save Changes",
        width = "100%"
      ))
    ))

  })

  # show/hide save button in modal
  shiny::observe({
    shiny::req(input$survey_dates_cal_click$start)

    ex_value <- rct$dates |>
      dplyr::filter(date == as.Date(input$survey_dates_cal_click$start)) |>
      dplyr::pull(weekend)

    shinyjs::toggle(
      "save_date_cat",
      condition = ex_value != as.integer(input$date_cat)
    )

  })

  # apply changes from modal
  shiny::observeEvent(input$save_date_cat, {
    shiny::req(input$date_cat)

    new_row <- rct$dates |>
      dplyr::filter(date == as.Date(input$survey_dates_cal_click$start)) |>
      dplyr::mutate(weekend = as.integer(input$date_cat))

    rct$dates <- rct$dates |>
      dplyr::rows_update(new_row, by = "date")

    shiny::removeModal()

  })

  ##############################################################################
  #
  # screen 5 reactivity
  #
  ##############################################################################
  # survey times calendar
  output$survey_times_cal <- toastui::renderCalendar({
    shiny::req(time_check())

    rct$times |>
      survey_times_to_tui(rtz()) |>
      toastui::calendar(
        defaultDate = input$sd,
        navigation = TRUE,
        useDetailPopup = TRUE,
        navOpts = toastui::navigation_options(
          today_label = "Today",
          prev_label = "Previous Month",
          next_label = "Next Month",
          fmt_date = "MMM DD, YYYY"
        )
      ) |>
      toastui::cal_props(as.data.frame(tui_calendars)) |>
      toastui::cal_month_options(isAlways6Week = FALSE) |>
      toastui::cal_timezone(rtz())

  })

  ##############################################################################
  #
  # screen 6 reactivity
  #
  ##############################################################################
  # xlsx download
  output$dl_xl <- shiny::downloadHandler(
    filename = function() {
      paste0(gsub("\\W", "_", input$sname), ".xlsx")
    },
    content = function(file) {
      times_to_xl(rct$times, input$sname, rtz(), file)
    }
  )

  # pdf download
  output$dl_pdf <- shiny::downloadHandler(
    filename = function() {
      paste0(gsub("\\W", "_", input$sname), ".pdf")
    },
    content = function(file) {
      pdf(file)
      lapply(calr_plots(rct$dates, rct$times, rtz()), plot)
      dev.off()
    }
  )

  # ics download
  output$dl_ics <- shiny::downloadHandler(
    filename = function() {
      paste0(gsub("\\W", "_", input$sname), ".ics")
    },
    content = function(file) {
      times_to_ics(rct$times, input$sname, rtz(), out_file = file)
    }
  )

}

shiny::shinyApp(ui, server)
