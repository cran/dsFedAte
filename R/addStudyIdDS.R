#' Add study membership dummy columns to a server-side data frame
#'
#' Server-side assign function called once per server before the
#' study-effects arm-specific regression models are fitted via
#' \code{ds.glm()}.  Called automatically when
#' \code{study_effects = TRUE} is passed to \code{dsFedAteClient::ds.EstimateAte()}.
#' Appends K-1 explicit 0/1 integer columns named \code{study_id2},
#' \ldots, \code{study_idK} to the server-side data frame.
#'
#' These are dummy columns: column \code{study_idk} equals
#' \code{1} on the server whose study index is \eqn{k} and \code{0} on every
#' other server.  Study 1 is the reference level (all dummies zero).
#'
#' Because the columns are plain integers, \code{ds.glm()} includes them in the
#' model formula without invoking R's factor-contrast machinery.  This avoids
#' the \emph{"contrasts can be applied only to factors with 2 or more levels"}
#' error that arises when a factor column carries only one level locally (every
#' server holds observations from exactly one study).
#'
#' The column values are entirely determined by the study index supplied by the
#' client — they are constants, not individual-level data — so appending them
#' carries no disclosure risk beyond the existing \code{nfilter.subset} check
#' on the dataset size.
#'
#' @param base_name A character string naming the server-side data frame.
#' @param study_index A character string giving this server's 1-based study
#'   index (its position in the client's \code{datasources} list).
#' @param K_studies A character string giving the total number of studies K.
#'
#' @return The augmented data frame with K-1 integer dummy columns
#'   (\code{study_id2}, \ldots, \code{study_idK}) appended.  The return
#'   value is assigned to a server-side symbol by \code{DSI::datashield.assign()}
#'   on the client.
#' @section Privacy control level:
#'   \itemize{
#'     \item Permitted: \code{permissive}, \code{banana}.
#'     \item Blocked: \code{carrot}, \code{avocado}, \code{non-permissive}.
#'   }
#' @seealso \code{\link{localAteDS}}, \code{dsFedAteClient::ds.EstimateAte()}.
#' @references
#' Khellaf, R., Bellet, A., and Josse, J. (2025).
#' Federated Causal Inference: Multi-Study ATE Estimation beyond Meta-Analysis.
#' \emph{Proceedings of the 28th International Conference on Artificial
#' Intelligence and Statistics (AISTATS 2025)}.
#' @importFrom dsBase checkPermissivePrivacyControlLevel listDisclosureSettingsDS
#' @importFrom cli cli_abort
#' @examples
#' \dontrun{
#' # Called automatically by dsFedAteClient::ds.EstimateAte(..., study_effects = TRUE).
#' # Direct invocation (per server, from the client):
#' cally <- call("addStudyIdDS", "study_data", "2", "3")
#' DSI::datashield.assign(conns["site2"], "study_data_adj", cally)
#' }
#' @export
addStudyIdDS <- function(base_name, study_index, K_studies) {

  # 1. Privacy control level check
  dsBase::checkPermissivePrivacyControlLevel(c("permissive", "banana"))

  # 2. Resolve server-side object
  base <- eval(parse(text = base_name), envir = parent.frame())

  # 3. Disclosure check
  .checkDisclosureRisk(base)

  # 4. Parse and validate arguments
  study_index <- as.integer(study_index)
  K_studies   <- as.integer(K_studies)

  if (is.na(study_index) || study_index < 1L || study_index > K_studies) {
    cli::cli_abort(c(
      "x" = "`study_index` must be an integer between 1 and {K_studies}.",
      "i" = "Received: {study_index}"
    ))
  }

  # 5. Append K-1 treatment-contrast dummy columns.
  #    Column study_idk = 1 iff this server is study k, 0 otherwise.
  #    Study 1 is the reference level: all its dummies are zero.
  for (k in seq(2L, K_studies)) {
    col_name         <- paste0("study_id", k)
    base[[col_name]] <- as.integer(study_index == k)
  }

  return(base)
}
