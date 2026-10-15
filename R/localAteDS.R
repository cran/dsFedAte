#' Compute local statistics for the federated average treatment effect (ATE)
#' and (optionally) its variance
#'
#' Server-side aggregate function called after the federated GLM has converged.
#' Always returns the quantities needed for the ATE point estimate.  When
#' \code{compute_variance = TRUE} (the default) it also returns the statistics
#' required for the delta-method variance.  Set \code{compute_variance = FALSE}
#' to limit disclosure to the minimum needed for the point estimate only.
#'
#' @param base_name A character string naming the server-side data frame.
#' @param outcome_name A character string naming the outcome variable inside
#'   \code{base_name}.
#' @param covariate_names_str A character string of comma-separated covariate
#'   names.
#' @param treatment_name A character string naming the binary treatment
#'   indicator inside \code{base_name}.
#' @param alpha0_str Character. Intercept of the control-arm model.
#' @param beta0_str Character. Comma-separated slope coefficients of the
#'   control-arm model.
#' @param alpha1_str Character. Intercept of the treated-arm model.
#' @param beta1_str Character. Comma-separated slope coefficients of the
#'   treated-arm model.
#' @param compute_variance Logical. If \code{TRUE} (default), also return the
#'   within-site sum of squared deviations of the individual treatment-effect
#'   projections from their site mean, and — when \code{heterogeneous = TRUE}
#'   — the column sums of the augmented design matrix.  Set to \code{FALSE} to
#'   limit disclosure to the three scalars strictly needed for the point
#'   estimate.
#' @param heterogeneous Logical. Controls which variance statistics are
#'   returned.  \code{TRUE} (default) makes no assumption about the
#'   participating RCTs: each site may have a different covariate distribution
#'   and a different treatment probability.  The column sums of the augmented
#'   design matrix are returned so the client can assemble the global covariate
#'   mean needed for the leverage-corrected variance.  \code{FALSE} assumes all
#'   RCTs share the same covariate distribution; these column sums are then
#'   unnecessary and are not returned, limiting disclosure.  Has no effect when
#'   \code{compute_variance = FALSE}.
#'
#' @return A named list. The following three elements are always present:
#'   \describe{
#'     \item{n_k}{Total sample size on this server.}
#'     \item{n_k_1}{Number of treated observations on this server.}
#'     \item{tau_k}{Local average treatment effect estimate \eqn{\hat\tau_k}.}
#'   }
#'   When \code{compute_variance = TRUE}, one further element is added:
#'   \describe{
#'     \item{s_k}{Within-site sum of squared deviations of the individual
#'       treatment-effect projections from their site mean:
#'       \eqn{\sum_{i \in k}(X_i^\top\hat\gamma - \bar{X}_k^\top\hat\gamma)^2},
#'       where \eqn{\hat\gamma = \hat\beta_1 - \hat\beta_0} is the difference
#'       of the arm-specific slope vectors.}
#'   }
#'   When \code{compute_variance = TRUE} and \code{heterogeneous = TRUE}, two
#'   additional elements:
#'   \describe{
#'     \item{g_raw_k}{Column sums of the augmented design matrix
#'       \eqn{(1, X)}, i.e. the vector
#'       \deqn{(n_k,\,\sum\nolimits_i X_{i1},\,\ldots,\,\sum\nolimits_i X_{id}).}
#'       Accumulated across servers by the client to obtain the global
#'       covariate mean used for both arm gradients.}
#'   }
#' @section Privacy control level:
#'   \itemize{
#'     \item Permitted: \code{permissive}, \code{banana}.
#'     \item Blocked: \code{carrot}, \code{avocado}, \code{non-permissive}.
#'   }
#' @seealso \code{\link{addStudyIdDS}}, \code{dsFedAteClient::ds.EstimateAte()}.
#' @references
#' Khellaf, R., Bellet, A., and Josse, J. (2025).
#' Federated Causal Inference: Multi-Study ATE Estimation beyond Meta-Analysis.
#' \emph{Proceedings of the 28th International Conference on Artificial
#' Intelligence and Statistics (AISTATS 2025)}.
#' @importFrom dsBase checkPermissivePrivacyControlLevel
#' @importFrom cli cli_abort
#' @importFrom stats model.matrix as.formula
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   X1 = rnorm(30), X2 = rnorm(30),
#'   W  = rbinom(30, 1, 0.5)
#' )
#' df$Y <- 1 + 0.5 * df$W + 0.3 * df$X1 + rnorm(30, sd = 0.5)
#' options(datashield.privacyControlLevel = "banana", nfilter.subset = 3)
#' localAteDS("df", "Y", "X1,X2", "W", "1.0", "0.3,0.0", "1.5", "0.3,0.0")
#' }
#' @export
localAteDS <- function(base_name, outcome_name, covariate_names_str, treatment_name,
                       alpha0_str, beta0_str, alpha1_str, beta1_str,
                       compute_variance = TRUE, heterogeneous = TRUE) {

  # 1. Privacy control level check
  dsBase::checkPermissivePrivacyControlLevel(c("permissive", "banana"))

  # 2. Parse arguments
  covariate_names <- scan(
    text = covariate_names_str, what = "", sep = ",",
    strip.white = TRUE, quiet = TRUE
  )
  alpha0 <- .parse_numeric_scalar(alpha0_str, "alpha0_str")
  alpha1 <- .parse_numeric_scalar(alpha1_str, "alpha1_str")
  beta0  <- .parse_numeric_vector(beta0_str,  "beta0_str")
  beta1  <- .parse_numeric_vector(beta1_str,  "beta1_str")

  base <- eval(parse(text = base_name), envir = parent.frame())

  # 3. Disclosure check
  .checkDisclosureRisk(base)

  # 4. Extract data components
  W     <- base[[treatment_name]]
  Y     <- base[[outcome_name]]
  X_mat <- model.matrix(
    as.formula(paste("~", paste(covariate_names, collapse = "+"))),
    data = base[, covariate_names, drop = FALSE]
  )[, -1L, drop = FALSE]

  n_k   <- nrow(base)
  idx_1 <- W == 1L
  n_k_1 <- sum(idx_1)

  # 5. ATE point estimate
  delta_alpha <- alpha1 - alpha0
  gamma       <- beta1  - beta0
  proj_i      <- as.numeric(X_mat %*% gamma)
  proj_bar_k  <- mean(proj_i)
  tau_k       <- as.numeric(delta_alpha + proj_bar_k)

  # 6. Early exit when only the point estimate is needed
  if (!compute_variance) {
    return(list(n_k = n_k, n_k_1 = n_k_1, tau_k = tau_k))
  }

  # 7. Variance statistics

  # 7a. Local scatter s_k
  s_k <- sum((proj_i - proj_bar_k)^2)

  result <- list(n_k = n_k, n_k_1 = n_k_1, tau_k = tau_k, s_k = s_k)

  # 7b. Covariate sums (heterogeneous = TRUE only)
  if (heterogeneous) {
    result$g_raw_k <- colSums(cbind(1, X_mat))
  }
  return(result)
}

