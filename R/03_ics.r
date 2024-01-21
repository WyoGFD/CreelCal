# convert date to ics format (YYYYMMDD)
date_to_ics <- function(d) {

  strftime(d, "%Y%m%d")

}

# convert posix to ics UTC format (YYYYMMDDTHHMMSSZ)
posix_to_ics <- function(dt) {

  strftime(
    lubridate::with_tz(dt, "UTC"),
    "%Y%m%dT%H%M%SZ",
    tz = "UTC"
  )

}

# convert a survey date to ics
cti_date_sub <- function(start, uid, summary, descr = NULL) {

  out <- c(
    "BEGIN:VEVENT",
    glue::glue("DTSTART;VALUE=DATE:{date_to_ics(start)}"),
    glue::glue("DTEND;VALUE=DATE:{date_to_ics(start)}"),
    glue::glue("DTSTAMP:{posix_to_ics(Sys.time())}"),
    glue::glue("UID:{uid}"),
    glue::glue("SUMMARY:{summary}")
  )

  if (!is.null(descr)) {
    out <- c(
      out,
      glue::glue("DESCRIPTION:{descr}")
    )
  }

  out <- c(out, "END:VEVENT")

}

# convert a survey time to ics
cti_time_sub <- function(start, uid, summary, descr = NULL) {

  out <- c(
    "BEGIN:VEVENT",
    glue::glue("DTSTART:{posix_to_ics(start)}"),
    glue::glue("DTEND:{posix_to_ics(start + lubridate::minutes(15))}"),
    glue::glue("DTSTAMP:{posix_to_ics(Sys.time())}"),
    glue::glue("UID:{uid}"),
    glue::glue("SUMMARY:{summary}")
  )

  if (!is.null(descr)) {
    out <- c(
      out,
      glue::glue("DESCRIPTION:{descr}")
    )
  }

  out <- c(out, "END:VEVENT")

}

# convert survey times to ics
times_to_ics <- function(times,
                         survey_name,
                         tz,
                         incl_dates = TRUE,
                         out_file = NULL) {
  .data <- rlang::.data

  out <- c(
    "BEGIN:VCALENDAR",
    "VERSION:2.0",
    "PRODID:-//ericn.shinyapps.io//EN",
    "CALSCALE:GREGORIAN",
    "METHOD:PUBLISH",
    glue::glue("X-WR-CALNAME:{survey_name}"),
    "X-WR-CALDESC:WGFD Creel Surveys"
  )

  times_ics <- times |>
    dplyr::transmute(
      start = .data$survey_time,
      uid = paste(
        gsub("\\W", "", survey_name),
        dplyr::row_number(),
        "wyo.gov",
        sep = "-"
      ),
      summary = sprintf(
        "Creel Survey: %s%s",
        format(lubridate::with_tz(.data$survey_time, tz), "%I:%M %p"),
        dplyr::if_else(is.na(.data$location), "", paste0(" ", .data$location))
      ),
      descr = sprintf(
        "Stratum %d<br />%s<br />Randomized Survey Time",
        .data$stratum,
        tui_calendars$name[.data$weekend + 1]
      )
    ) |>
    purrr::pmap(cti_time_sub) |>
    unlist()

  out <- c(out, times_ics)

  if (incl_dates) {

    dates_ics <- times |>
      dplyr::group_by(.data$date, .data$stratum, .data$weekend) |>
      dplyr::summarize(
        st = paste(
          format(lubridate::with_tz(.data$survey_time, tz), "%I:%M %p"),
          collapse = ", "
        ),
        .groups = "drop"
      ) |>
      dplyr::transmute(
        start = .data$date,
        uid = paste(
          gsub("\\W", "", survey_name),
          dplyr::row_number() + nrow(times),
          "wyo.gov",
          sep = "-"
        ),
        summary = survey_name,
        descr = sprintf(
          "Stratum %d<br />%s<br />Survey Times: %s",
          .data$stratum,
          tui_calendars$name[.data$weekend + 1],
          .data$st
        )
      ) |>
      purrr::pmap(cti_date_sub) |>
      unlist()

    out <- c(out, dates_ics)

  }

  out <- c(out, "END:VCALENDAR")

  if (!is.null(out_file)) {
    writeLines(out, out_file)
  }

  out

}
