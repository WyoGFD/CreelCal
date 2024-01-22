times_to_xl <- function(times, survey_name, tz, out_file) {
  .data <- rlang::.data

  times |>
    dplyr::group_by(.data$date) |>
    dplyr::mutate(
      id = .data$weekend + 1,
      survey = survey_name,
      `survey number` = dplyr::row_number(),
      survey_time = lubridate::with_tz(.data$survey_time, tz),
      `survey time` = format(.data$survey_time, "%I:%M %p")
    ) |>
    dplyr::ungroup() |>
    dplyr::inner_join(tui_calendars, by = "id") |>
    dplyr::select(
      dplyr::all_of(
        c(
          "survey", "date", "stratum", category = "name",
          "survey number", "survey time", "location"
        )
      )
    ) |>
    writexl::write_xlsx(out_file)

}
