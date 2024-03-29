screen1_ui <- shinyglide::screen(
  shiny::column(
    width = 12,
    shiny::h2("Survey Setup - Basic Info"),
    shiny::helpText(
      "Enter a name for the survey below, and click the map to indicate",
      "the approximate location for the survey. You can zoom into the map",
      "using the buttons at the top left or the mouse wheel."
    )
  ),
  shiny::column(
    width = 12,
    shiny::div(
      shiny::textInput(
        "sname",
        "Survey Name:",
        placeholder = "e.g. Lake Desmet 2024"
      ),
      class = "required-input",
      id = "sname-req"
    )
  ),
  shiny::column(
    width = 12,
    shiny::tags$label(
      class = "control-label",
      "Location:"
    ),
    leaflet::leafletOutput("ll_map")
  ),
  shiny::column(
    width = 12,
    shiny::uiOutput("ll")
  ),
  next_condition = "input.sname.length > 0"
)

screen2_ui <- shinyglide::screen(
  shiny::column(
    width = 12,
    shiny::h2("Survey Setup - Survey Duration"),
    shiny::helpText(
      "Enter the start date, stratum length in months, weeks, or days, and",
      "number of strata for your survey below."
    )
  ),
  shiny::column(
    width = 4,
    shiny::div(
      shiny::dateInput("sd", "Start Date:", width = "100%"),
      id = "sd-req"
    )
  ),
  shiny::column(
    width = 2,
    shiny::div(
      shiny::numericInput(
        "int",
        "Stratum Length:",
        value = 1,
        min = 1,
        width = "100%"
      ),
      id = "int-req"
    )
  ),
  shiny::column(
    width = 2,
    shiny::div(
      shiny::selectInput(
        "int_units",
        "Units:",
        choices = c("Days", "Weeks", "Months"),
        selected = "Months",
        width = "100%"
      ),
      id = "int_units-req"
    )
  ),
  shiny::column(
    width = 4,
    shiny::sliderInput(
      "n_int",
      "Number of strata:",
      min = 1,
      max = 12,
      value = 3,
      step = 1,
      width = "100%"
    )
  ),
  next_condition = paste(
    "input.sd != null &",
    "input.int > 0 & ",
    "input.int_units.length > 0"
  )
)

screen3_ui <- shinyglide::screen(
  shiny::column(
    width = 12,
    shiny::h2("Survey Setup - Available Dates"),
    shiny::helpText(
      "Use the calendar below to modify how dates are categorized",
      "for this survey if needed. You can also mark certain dates",
      "totally unavailable for sampling (never selected) or mandatory",
      "(always selected)."
    )
  ),
  shiny::column(
    width = 12,
    shiny::div(
      toastui::calendarOutput("survey_dates_cal"),
      style = "height: 650px;"
    )
  )
)

screen4_ui <- shinyglide::screen(
  shiny::column(
    width = 12,
    shiny::h2("Survey Setup - Survey Times"),
    shiny::helpText(
      "Use the inputs below to specify how many counts take place per selected",
      "sampling day, how many days are selected in each category per stratum,",
      "and what times are available for counts based on sunrise/sunset."
    )
  ),
  shiny::column(
    width = 4,
    shiny::sliderInput(
      "spd",
      "Counts per day:",
      min = 1,
      max = 5,
      value = 3,
      step = 1,
      width = "100%"
    )
  ),
  shiny::column(
    width = 4,
    shiny::sliderInput(
      "wd",
      "Weekdays to survey (per stratum):",
      min = 1,
      max = 10,
      value = 4,
      step = 1,
      width = "100%"
    ),
    shiny::sliderInput(
      "we",
      "Weekend days to survey (per stratum):",
      min = 1,
      max = 10,
      value = 4,
      step = 1,
      width = "100%"
    )
  ),
  shiny::column(
    width = 4,
    shiny::sliderInput(
      "es",
      "Earliest Potential Count (in minutes, relative to sunrise):",
      min = -120,
      max = 120,
      value = 0,
      step = 5,
      width = "100%"
    ),
    shiny::sliderInput(
      "ls",
      "Latest Potential Count (in minutes, relative to sunset):",
      min = -120,
      max = 120,
      value = 0,
      step = 5,
      width = "100%"
    )
  )
)

screen5_ui <- shinyglide::screen(
  shiny::column(
    width = 12,
    shiny::h2("Survey Setup - Survey Times"),
    shiny::helpText(
      "The calendar below shows randomly generated survey times.",
      "Times cannot be edited, but you can go back to previous screens to",
      "change the settings used to generate them."
    )
  ),
  shiny::column(
    width = 12,
    shiny::div(
      toastui::calendarOutput("survey_times_cal"),
      style = "height: 700px;"
    )
  )
)

screen6_ui <- shinyglide::screen(
  shiny::column(
    width = 12,
    shiny::h2("Survey Setup - Starting Location"),
    shiny::helpText(
      "Use the inputs below to define the available starting locations for",
      "the survey. When more than 1 starting location is available, you can",
      "set the name and selection probability for each location using the",
      "inputs below the slider."
    )
  ),
  shiny::column(
    width = 12,
    shiny::sliderInput(
      "n_loc",
      "Number of starting locations:",
      min = 1,
      max = 5,
      value = 1,
      step = 1,
      width = "100%"
    )
  ),
  shiny::column(
    width = 12,
    shiny::uiOutput("loc")
  )
)

screen7_ui <- shinyglide::screen(
  shiny::column(
    width = 12,
    shiny::h2("Export Survey"),
    shiny::helpText(
      "Now that your creel survey has been created, you can download the",
      "survey details in a variety of formats below."
    )
  ),
  shiny::column(
    width = 4,
    shiny::div(
      shiny::downloadButton("dl_xl", "Spreadsheet", style = "width: 100%")
    )
  ),
  shiny::column(
    width = 4,
    shiny::div(
      shiny::downloadButton("dl_pdf", "PDF", style = "width: 100%")
    )
  ),
  shiny::column(
    width = 4,
    shiny::div(
      shiny::downloadButton("dl_ics", "Calendar File", style = "width: 100%")
    )
  ),
  shiny::column(
    width = 12,
    shiny::div(style = "margin-bottom: 12px;"),
    shiny::helpText(
      "To import a calendar (.ics) file into google calendar,",
      "follow these steps:",
      shiny::tags$ol(
        shiny::tags$li(
          "Browse to google calendar in chrome and log in if needed"
        ),
        shiny::tags$li(
          "In the 'Other calendars' section click the plus icon and ",
          "select 'Create new calendar'"
        ),
        shiny::tags$li(
          "Fill in the name and description for the new calendar and click ",
          "the button to create it"
        ),
        shiny::tags$li(
          "Still in the settings page, click 'Import & export' in the menu ",
          "to the left"
        ),
        shiny::tags$li(
          "Click 'Select file from your computer' and browse to the .ics ",
          "you downloaded from the app"
        ),
        shiny::tags$li(
          "Under 'Add to calendar' select the new calendar you created in ",
          "step 3"
        ),
        shiny::tags$li(
          "Click 'Import' to add the events in the .ics to the new calendar"
        ),
        shiny::tags$li(
          "Now you can share the new creel survey calendar with survey",
          "personnel, set up reminders and notifications, etc."
        )
      )
    )
  )
)
