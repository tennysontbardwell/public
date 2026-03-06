# Usage:
# tlib <- new.env(parent = baseenv())
# source("~/.config/R/tennyson-common.R", local = tlib)

cmd_in_tmux = function(cmd, dir = NULL) {
  if (is.null(dir)) {
    dir <- getwd()
  }
  paste0(
    "tmux new-session -A -s main; tmux new-window -c ",
    shQuote(dir),
    " -t main ",
    shQuote(cmd),
    "; osascript -e 'tell application \"ghostty\" to activate'"
  ) |>
    system()
}

vd = function(df) {
  utils::write.csv(df, "/tmp/from-r.csv")
  cmd_in_tmux("vd /tmp/from-r.csv")
}

format_si <- function(x, sig_figs = 3) {
  si_prefixes <- data.frame(
    power = c(-24, -21, -18, -15, -12, -9, -6, -3, 0, 3, 6, 9, 12, 15, 18, 21, 24),
    symbol = c("y", "z", "a", "f", "p", "n", "µ", "m", "", "k", "M", "G", "T", "P", "E", "Z", "Y")
  )

  vapply(x, FUN.VALUE = character(1), FUN = function(n) {
    if (!is.finite(n)) return(as.character(n))
    if (n == 0) return("0")

    ## Calculate the exponent and find the appropriate SI prefix group
    exponent         <- floor(log10(abs(n)))
    si_power_level   <- floor(exponent / 3) * 3
    prefix_symbol    <- si_prefixes$symbol[si_prefixes$power == si_power_level]

    ## If no prefix is found (number is too large/small), use scientific notation
    if (length(prefix_symbol) == 0) {
      return(format(n, scientific = TRUE, digits = sig_figs))
    }
    ## Scale the number by dividing it by its SI power of 10
    scaled_number    <- n / (10^si_power_level)
    ## Round the SCALED number to the desired significant figures
    formatted_number <- format(signif(scaled_number, digits = sig_figs))
    paste0(formatted_number, prefix_symbol)
  })
}
si = format_si

scale_x_si = function() { ggplot2::scale_x_continuous(labels = format_si) }
scale_y_si = function() { ggplot2::scale_y_continuous(labels = format_si) }

tee = function(x) {
  print(x)
  x
}
rrev = function(df) {
  df |> dplyr::arrange(dplyr::desc(dplyr::row_number()))
}
bigtext = function() {
  ggplot2::theme(text = ggplot2::element_text(size = 18))
}
rotxlab = function() {
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.5))
}

## options(crayon.enabled = FALSE)
## options(max.print=300)
## options(browser = "open")
