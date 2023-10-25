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
    text.pos = cal_text_pos
  )

}

calr_plots <- function(dates, times) {
  .data <- rlang::.data

  survey_days <- times |>
    dplyr::group_by(.data$date) |>
    dplyr::summarize(
      st = paste(format(.data$survey_time, "%R"), collapse = "\n"),
      .groups = "drop"
    )

  calr_data <- dates |>
    dplyr::left_join(survey_days, by = "date") |>
    dplyr::mutate(category = tui_calendars$name[.data$weekend + 1])

  calr_data |>
    dplyr::mutate(
      year = lubridate::year(date),
      month = lubridate::month(date)
    ) |>
    dplyr::nest_by(year, month, .key = "cal_data") |>
    purrr::pmap(calr_month_plot)

}
