library(dplyr)
library(ggplot2)
library(scales)
library(rlang)

plot_dual_axis <- function(data,
                           col1,
                           col2,
                           label1 = NULL,
                           label2 = NULL,
                           title = NULL,
                           highlight_start = NULL,
                           highlight_end = NULL) {
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(rlang)
  library(tidyr)
  
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
    # Mean lines (on scaled y for plotting)
    geom_hline(yintercept = mean1, linetype = "dashed", color = measure_colors[label1]) +
    geom_hline(
      yintercept = (mean2 - min2) * scale_factor + min(plot_df$var1, na.rm = TRUE),
      linetype = "dashed", color = measure_colors[label2]
    ) +
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
    theme_bw()
  
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

