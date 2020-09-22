#' Predict hemispheric dominance
#'
#' Predict hemispheric dominance based on observed laterality measures, using the methods
#' described in \insertCite{Sorensen2020;textual}{BayesianLaterality}.
#'
#' @param data Data frame with the following columns:
#' \itemize{
#' \item \code{listening}: Score between -100 and 100.
#' \item \code{handedness}: \code{"left"} for adextral (non-right-handed) and
#'       \code{"right"} for dextral (right-handed)
#'       }
#'  In addition, an optional column named \code{ID}
#'   can be provided, giving the subject ID. If a subject has multiple
#'   measurements, the posterior based on all measurements is provided. If the
#'   \code{ID} column is missing, each row is assumed to be measured on a
#'   separate subject.
#'
#' @param parameters Data frame in which the first two columns specify combinations
#' of hemispheric dominance and handedness and the last three columns specify
#' the corresponding parameter values. In particular, the columns are defined as follows:
#' \itemize{
#' \item \code{dominance}: character specifying hemispheric dominance.
#' \item \code{handedness}: character specifying handedness.
#' \item \code{mean_li}: mean dichotic listening score.
#' \item \code{sd_li}: standard deviation of dichotic listening score.
#' \item \code{prob_dominance}: probability of hemispheric dominance given handedness.
#' }
#' @param truncation Numeric vector with two elements specifying the lower and upper
#' bounds for truncation of the normal distribution for dichotic listening scores.
#' @param icc Intraclass correlation for repeated measurements on the same individual.
#' Defaults to 0.
#'
#' @return The probability of left or right hemispheric dominance in additional
#'   columns of \code{data}.
#' @export
#' @examples
#' # The package comes with two example datasets.
#' # The first contains single measurements on three subjects.
#' # We can first take a look at the data
#' example_data1
#' # Next, compute predictions.
#' # Since there is no ID column, predict_dominance() will print a message telling
#' # the user that the rows are assumed to contain observations from different subjects.
#' predict_dominance(example_data1)
#'
#' # The next example dataset contains repeated measurements
#' example_data2
#'
#' # We compute the predictions as before:
#' predict_dominance(example_data2)
#'
#' @references
#' \insertAllCited{}
#'
#' @importFrom rlang .data
#' @importFrom Rdpack reprompt
predict_dominance <- function(data,
                              parameters = dplyr::tibble(
                                dominance = rep(c("left", "right", "none"), each = 2),
                                handedness = rep(c("left", "right"), 3),
                                mean_li = c(10, 12, -24, -24, 0, 0),
                                sd_li = c(24.9, 17.0, 24.9, 17.0, 22, 22),
                                prob_dominance = c(.65, .87, .35, .13, 0, 0)
                              ),
                              truncation = c(-100, 100),
                              icc = 0
                              ){

  stopifnot(icc >= -1 && icc <= 1)

  # Check if data contains an ID column
  if(!"ID" %in% colnames(data)) {
    message("No ID column in data, assuming one subject per row.")
    data$ID = as.character(seq(1, nrow(data), by = 1))
  }

  dat1 <- dplyr::select_at(data, dplyr::vars("ID", "listening", "handedness"))
  dat1 <- dplyr::inner_join(dat1, parameters, by = "handedness")
  dat1 <- dplyr::select_at(dat1, dplyr::vars("ID", "handedness", "dominance",
                                             "prob_dominance", "mean_li", "sd_li", "listening"))
  dat2 <- tidyr::nest(dat1, df = c("listening", "mean_li", "sd_li"))
  dat3 <- dplyr::mutate(dat2,
                log_prob_listening = purrr::map_dbl(.data$df, function(x) {
                  tmvtnorm::dtmvnorm(x$listening,
                                     mean = x$mean_li,
                                     sigma = x$sd_li^2 * (diag(nrow(x)) * (1 - icc) + icc),
                                     lower = rep(truncation[[1]], nrow(x)),
                                     upper = rep(truncation[[2]], nrow(x)),
                                     log = TRUE
                                     )
                }),
                log_posterior = log(.data$prob_dominance) + .data$log_prob_listening,
                probability = exp(.data$log_posterior)
                )
  dat4 <- dplyr::group_by_at(dat3, dplyr::vars("ID"))
  dat5 <- dplyr::mutate_at(dat4, dplyr::vars("probability"), ~ . / sum(.))

  dplyr::select_at(dplyr::ungroup(dat5), dplyr::vars("ID", "handedness",
                                                     "dominance", "probability"))
}

