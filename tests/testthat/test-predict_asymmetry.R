
data <- dplyr::tibble(
  listening = c(-20),
  handedness = "left",
  stringsAsFactors = FALSE
)

df <- predict_dominance(data)

expect_equal(
  df$probability,
  c(0.476309298919027, 0.523690701080972, 0)
)

