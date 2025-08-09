library(dplyr)
library(ggplot2)
library(scales)
library(rlang)
library(tidyr)

#' Plot Two Numerical Variables Over Time with Dual Y-Axes and Matching Colors
#'
#' Creates a time series line plot comparing two numerical variables that share the same
#' date column (`file_date`). The plot uses dual y-axes with independent scales to better
#' visualize differences in magnitude. The left and right y-axis labels and ticks are
#' colored to match their corresponding lines.
#'
#' The function optionally adds dashed horizontal lines representing the mean of each
#' variable and can highlight a date range with a translucent orange rectangle.
#'
#' @param data A data frame containing at least a `file_date` column and two numerical columns.
#' @param col1 Bare or quoted name of the first numerical variable to plot (left y-axis).
#' @param col2 Bare or quoted name of the second numerical variable to plot (right y-axis).
#' @param label1 Character. Label for the first variable and left y-axis. Defaults to the name of `col1`.
#' @param label2 Character. Label for the second variable and right y-axis. Defaults to the name of `col2`.
#' @param title Character. Plot title. Defaults to "Comparison of {label1} and {label2}".
#' @param highlight_start Date or character. Start date of the highlight range (optional).
#' @param highlight_end Date or character. End date of the highlight range (optional).
#' @param show_means Logical. Whether to show dashed horizontal lines at the mean values of each variable. Defaults to TRUE.
#'
#' @return A `ggplot` object showing the time series comparison with dual y-axes.
#'
#' @examples
#' \dontrun{
#' plot_dual_axis(
#'   data = combined,
#'   col1 = rmssd_detail,
#'   col2 = resting_hr,
#'   label1 = "HRV (RMSSD)",
#'   label2 = "Resting Heart Rate (bpm)",
#'   title = "HRV vs Resting Heart Rate Over Time",
#'   highlight_start = "2025-07-01",
#'   highlight_end = "2025-07-10",
#'   show_means = TRUE
#' )
#' }
plot_dual_axis <- function(data,
                           col1,
                           col2,
                           label1 = NULL,
                           label2 = NULL,
                           title = NULL,
                           highlight_start = NULL,
                           highlight_end = NULL,
                           show_means = TRUE) {
  
  col1 <- enquo(col1)
  col2 <- enquo(col2)
  
  # Summarize by date
  plot_df <- data %>%
    group_by(file_date) %>%
    summarize(
      var1 = mean(!!col1, na.rm = TRUE),
      var2 = mean(!!col2, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Observed ranges for scaling
  min2 <- min(plot_df$var2, na.rm = TRUE)
  max2 <- max(plot_df$var2, na.rm = TRUE)
  
  scale_factor <- (max(plot_df$var1, na.rm = TRUE) -
                     min(plot_df$var1, na.rm = TRUE)) /
    (max2 - min2)
  
  mean1 <- mean(plot_df$var1, na.rm = TRUE)
  mean2 <- mean(plot_df$var2, na.rm = TRUE)
  
  # Default labels & title
  if (is.null(label1)) label1 <- as_name(col1)
  if (is.null(label2)) label2 <- as_name(col2)
  if (is.null(title))  title  <- paste("Comparison of", label1, "and", label2)
  
  # Prepare long-format data with scaled var2 for plotting
  long_df <- bind_rows(
    plot_df %>% mutate(Value = var1, Measure = label1),
    plot_df %>% mutate(
      Value = (var2 - min2) * scale_factor + min(plot_df$var1, na.rm = TRUE),
      Measure = label2
    )
  )
  
  # Correct mapping of colors to actual Measure values
  plot_colors <- hue_pal()(2)
  measure_colors <- setNames(plot_colors, c(label1, label2))
  
  p <- ggplot(long_df, aes(x = file_date, y = Value, color = Measure)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = measure_colors) +
    scale_y_continuous(
      name = label1,
      sec.axis = sec_axis(
        ~ (. - min(plot_df$var1, na.rm = TRUE)) / scale_factor + min2,
        name = label2
      )
    ) +
    labs(
      title = title,
      x = "Date",
      color = "Measure"
    ) +
    theme_bw() +
    theme(
      axis.title.y = element_text(color = measure_colors[label1]),
      axis.title.y.right = element_text(color = measure_colors[label2]),
      axis.text.y = element_text(color = measure_colors[label1]),
      axis.text.y.right = element_text(color = measure_colors[label2])
    )
  
  # Optional mean lines
  if (show_means) {
    p <- p +
      geom_hline(yintercept = mean1, linetype = "dashed", color = measure_colors[label1]) +
      geom_hline(
        yintercept = (mean2 - min2) * scale_factor + min(plot_df$var1, na.rm = TRUE),
        linetype = "dashed", color = measure_colors[label2]
      )
  }
  
  # Optional highlight region
  if (!is.null(highlight_start) && !is.null(highlight_end)) {
    p <- p +
      annotate(
        "rect",
        xmin = as.Date(highlight_start),
        xmax = as.Date(highlight_end),
        ymin = -Inf,
        ymax = Inf,
        alpha = 0.2,
        fill = "orange"
      )
  }
  
  return(p)
}
