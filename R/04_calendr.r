calr_month_plot <- function(year, month, cal_data) {

  cal_cats <- cal_data$category |>
    as.factor() |>
    levels()

  cal_colors <- c(
    Weekday = "lightblue",
    `Weekend/Holiday` = "lightgreen",
    Unavailable = "lightpink",
    Mandatory = "lightgray"
  )[cal_cats]

  cal_days <- rep(NA, lubridate::days_in_month(cal_data$date[1]))
  cal_days[lubridate::mday(cal_data$date)] <- cal_data$category

  offset <- min(lubridate::mday(cal_data$date)) - 1

  cal_text <- cal_data$st[!is.na(cal_data$st)]
  cal_text_pos <- which(!is.na(cal_data$st)) + offset

  calendR::calendR(
    year = year,
    month = month,
    special.days = cal_days,
    special.col = cal_colors,
    text = cal_text,
    text.pos = cal_text_pos,
    text.size = 3
  )

}

calr_plots <- function(dates, times, tz) {
  .data <- rlang::.data

  survey_days <- times |>
    dplyr::group_by(.data$date) |>
    dplyr::mutate(survey_time = lubridate::with_tz(.data$survey_time, tz)) |>
    dplyr::summarize(
      st = paste(format(.data$survey_time, "%I:%M %p"), collapse = "\n"),
      .groups = "drop"
    )

  calr_data <- dates |>
    dplyr::left_join(survey_days, by = "date") |>
    dplyr::mutate(category = tui_calendars$name[.data$weekend + 1])

  calr_data |>
    dplyr::mutate(
      year = lubridate::year(.data$date),
      month = lubridate::month(.data$date)
    ) |>
    dplyr::nest_by(.data$year, .data$month, .key = "cal_data") |>
    purrr::pmap(calr_month_plot)

}
