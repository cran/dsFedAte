#' @keywords internal
#' @noRd
#' @importFrom dsBase listDisclosureSettingsDS
#' @importFrom cli cli_abort
.checkDisclosureRisk <- function(data) {
  thr            <- dsBase::listDisclosureSettingsDS()
  nfilter.subset <- as.numeric(thr$nfilter.subset)
  if (nrow(data) < nfilter.subset) {
    cli::cli_abort(
      c(
        "x" = "Disclosure risk: dataset has too few rows to proceed.",
        "i" = "Minimum required: {nfilter.subset}, provided: {nrow(data)}"
      )
    )
  }
}

#' @keywords internal
#' @noRd
#' @importFrom cli cli_abort
.parse_numeric_scalar <- function(x_str, arg_name) {
  val <- suppressWarnings(as.numeric(x_str))
  if (is.na(val)) {
    cli::cli_abort(c(
      "x" = "`{arg_name}` could not be parsed as a numeric value.",
      "i" = "Received: {x_str}"
    ))
  }
  val
}

#' @keywords internal
#' @noRd
#' @importFrom cli cli_abort
.parse_numeric_vector <- function(x_str, arg_name) {
  vals <- suppressWarnings(
    as.numeric(unlist(strsplit(x_str, split = ",", fixed = TRUE)))
  )
  if (any(is.na(vals))) {
    cli::cli_abort(c(
      "x" = "`{arg_name}` could not be fully parsed as a numeric vector.",
      "i" = "Received: {x_str}"
    ))
  }
  vals
}
