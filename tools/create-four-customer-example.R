# Generate four static illustrations, without rendering slides.
# These fixed illustrative customers are not sampled from the course CSV.
# Income is in $1,000s; spending is in $100s for simple SSE arithmetic.
# Run from the lecture repository root with the ggplot2 package available.

library(ggplot2)
library(grid)

income <- c(40, 60, 80, 100)
train_spend <- c(5, 11, 7, 13)
new_spend <- c(7, 5, 13, 11)
train <- data.frame(income, spending = train_spend)

# Estimate both models once, from training customers only.
straight_fit <- lm(spending ~ income, data = train)
straight <- function(x) as.numeric(predict(straight_fit, data.frame(income = x)))
flexible <- splinefun(income, train_spend, method = "fmm")
curve_income <- seq(min(income), max(income), length.out = 700)

draw_example <- function(filename, model, actual, sample, color, expected,
                         model_label, positions) {
  predicted <- model(income)
  squared <- (actual - predicted)^2
  stopifnot(all(abs(squared - round(squared)) < 1e-9))
  squared <- as.integer(round(squared))
  stopifnot(sum(squared) == expected)
  point_color <- if (sample == "Training customers") "#253847" else "#087e83"
  data <- data.frame(income, actual, predicted, squared,
                     midpoint = (actual + predicted) / 2,
                     label_x = positions$x, label_y = positions$y,
                     label_hjust = positions$hjust,
                     error_label = paste("Squared error:", squared))
  curve <- data.frame(income = curve_income, prediction = model(curve_income))
  errors <- data[data$squared > 0, ]
  palette <- setNames(c(color, point_color), c(model_label, sample))

  p <- ggplot() +
    geom_line(data = curve, aes(x = income, y = prediction,
                               color = model_label), linewidth = 1.3) +
    geom_segment(data = errors,
                 aes(x = income, xend = income, y = actual, yend = predicted),
                 color = "#7a8792", linetype = "dashed", linewidth = .8) +
    geom_segment(data = errors,
                 aes(x = income - .65, xend = income + .65,
                     y = predicted, yend = predicted),
                 color = color, linewidth = .8) +
    geom_point(data = data, aes(x = income, y = actual, color = sample),
               size = 4.5) +
    geom_segment(data = data,
                 aes(x = income, xend = label_x, y = midpoint, yend = label_y),
                 color = "#7a8792", linewidth = .45) +
    geom_label(data = data,
               aes(x = label_x, y = label_y, label = error_label,
                   hjust = label_hjust),
               size = 5.2, color = "#253847", fill = "white",
               label.padding = unit(.25, "lines"), linewidth = .25) +
    scale_color_manual(values = palette, breaks = c(model_label, sample),
                       name = NULL) +
    scale_x_continuous(breaks = income) +
    scale_y_continuous(breaks = c(4, 6, 8, 10, 12, 14)) +
    coord_cartesian(xlim = c(32, 108), ylim = c(3.5, 15.6), expand = FALSE) +
    labs(x = "Annual household income ($1,000s)",
         y = "Annual retailer spending ($100s)") +
    theme_minimal(base_size = 17) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#e8edf1", linewidth = .3),
          axis.text = element_text(color = "#253847", size = 15),
          axis.title = element_text(color = "#253847", size = 17),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          legend.position = "inside",
          legend.position.inside = c(.025, .98),
          legend.justification = c(0, 1),
          legend.direction = "horizontal",
          legend.text = element_text(size = 15),
          legend.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(10, 24, 8, 12))

  png(file.path("Figures", filename), width = 1920, height = 880, res = 160,
      bg = "white")
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(c(.87, .13), "null"))))
  print(p, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
  grid.roundrect(x = .53, y = .69, width = .92, height = .54,
                 r = unit(.10, "snpc"), gp = gpar(fill = "#f2f5f8", col = NA))
  total <- paste0("Total sum of squared errors: ",
                  paste(squared, collapse = " + "), " = ", sum(squared))
  grid.text(total, x = .53, y = .69,
             gp = gpar(fontsize = 21, fontface = "bold", col = color))
  grid.text("Squared-error units: ($100)\u00b2", x = .53, y = .20,
             gp = gpar(fontsize = 13, col = "#546777"))
  popViewport(2)
  dev.off()
  data.frame(model = model_label, sample, squared_errors = paste(squared, collapse = ","),
             total = sum(squared))
}

simple_positions <- function(actual, model) data.frame(
  x = c(44, 64, 84, 96), y = (actual + model(income)) / 2,
  hjust = c(0, 0, 0, 1))
training_curve_positions <- data.frame(x = c(44, 64, 84, 96),
                                        y = c(5.8, 11.6, 7.6, 14),
                                        hjust = c(0, 0, 0, 1))
new_line_positions <- data.frame(x = c(44, 64, 78, 96),
                                  y = c(6.5, 6.5, 14, 11.5),
                                  hjust = c(0, 0, 1, 1))

results <- rbind(
  draw_example("four-customers-train-straight.png", straight, train_spend,
                "Training customers", "#2a6fbb", 20, "Straight line",
                simple_positions(train_spend, straight)),
  draw_example("four-customers-train-flexible.png", flexible, train_spend,
                "Training customers", "#c46b19", 0, "Flexible curve",
                training_curve_positions),
  draw_example("four-customers-new-straight.png", straight, new_spend,
                "New customers", "#2a6fbb", 20, "Straight line",
                new_line_positions),
  draw_example("four-customers-new-flexible.png", flexible, new_spend,
                "New customers", "#c46b19", 80, "Flexible curve",
                simple_positions(new_spend, flexible))
)
stopifnot(max(abs(flexible(income) - train_spend)) < 1e-9,
          max(abs(straight(income) - c(6, 8, 10, 12))) < 1e-9)
print(results, row.names = FALSE)
