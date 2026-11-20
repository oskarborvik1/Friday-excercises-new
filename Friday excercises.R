# Argument 1 is the image width in pixels. Height is 70% of the width.
# Exact command: Rscript analysis.R 1500

# Read and check the command-line argument
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1 || !grepl("^[0-9]+$", args[1])) {
  stop("Supply one whole-number width. Example: Rscript analysis.R 1500",
       call. = FALSE)
}

image_size <- as.numeric(args[1])

if (image_size < 900 || image_size > 3000) {
  stop("Choose a width between 900 and 3000 pixels.", call. = FALSE)
}

# Load the data and use the first alcohol band as the reference
esoph_data <- datasets::esoph
esoph_data$alcgp <- factor(esoph_data$alcgp, ordered = FALSE)

# Fit models before and after accounting for age and tobacco
unadjusted <- glm(cbind(ncases, ncontrols) ~ alcgp,
                  data = esoph_data, family = binomial())

adjusted <- glm(cbind(ncases, ncontrols) ~ alcgp + agegp + tobgp,
                data = esoph_data, family = binomial())

# Get the alcohol groups and their matching coefficient names
alcohol_groups <- levels(esoph_data$alcgp)[-1]
alcohol_terms <- paste0("alcgp", alcohol_groups)

# Convert log odds ratios and their 95% confidence limits to odds ratios
get_odds_ratios <- function(model) {
  estimates <- summary(model)$coefficients[alcohol_terms, ]
  log_odds_ratio <- estimates[, "Estimate"]
  margin <- qnorm(0.975) * estimates[, "Std. Error"]
  exp(cbind(estimate = log_odds_ratio,
            lower = log_odds_ratio - margin,
            upper = log_odds_ratio + margin))
}

before <- get_odds_ratios(unadjusted)
after <- get_odds_ratios(adjusted)

model_colours <- c(Unadjusted = "#24678B", Adjusted = "#A64523")
ink <- "#172A35"
muted <- "#455B68"

# Open the PNG
output_file <- "esoph-infographic.png"
png_type <- if (capabilities("aqua")) "quartz" else "cairo"

png(filename = output_file,
    width = image_size, height = round(0.7 * image_size),
    res = round(image_size / 10), type = png_type, bg = "#F6F4EF")

# Make a blank canvas with coordinates from 0 to 100
par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
plot(0, 0, type = "n", xlim = c(0, 100), ylim = c(0, 100),
     axes = FALSE, xlab = "", ylab = "")

# Add the title, question and highlighted result
text(5, 89, "Alcohol and oesophageal cancer",
     adj = 0, cex = 2.85, font = 2, col = ink)
text(5, 82, "How alcohol impacts the odds ratio of oesophageal cancer.",
     adj = 0, cex = 1.35, col = muted)

rect(5, 68.5, 95, 77, col = "#F0E4D9", border = NA)
finding <- sprintf(
  "At 120+ g/day, the odds ratio rose from %.1f to %.1f.",
  before[3, "estimate"], after[3, "estimate"]
)
text(7, 72.8, finding, adj = 0, font = 2, cex = 1.25, col = ink)

# Add the row heading and legend
text(5, 63.5, "ALCOHOL (g/day)", adj = 0, font = 2, col = muted)

legend(32, 63.5,
       legend = c("Unadjusted", "Adjusted for age and tobacco"),
       col = model_colours, pch = c(16, 18), pt.cex = 1.5,
       horiz = TRUE, bty = "n", yjust = 0.5)

# Draw the chart background, grid and row labels
axis_x <- function(value) 26 + 28 * log10(value)
ticks <- c(1, 2, 5, 10, 20, 50, 100)
row_y <- c(52, 40, 28)

rect(5, 22, 95, 58, col = "#FFFEFB", border = NA)
segments(axis_x(ticks), 22, axis_x(ticks), 58, col = "#E0E5E5")
segments(axis_x(1), 22, axis_x(1), 58, col = muted, lty = 2)
text(7, row_y, alcohol_groups, adj = 0, font = 2, col = ink)

# Draw a confidence interval, point and value for each estimate
draw_estimates <- function(results, y, colour, shape) {
  segments(axis_x(results[, "lower"]), y,
           axis_x(results[, "upper"]), y, col = colour, lwd = 2)
  points(axis_x(results[, "estimate"]), y, col = colour, pch = shape, cex = 1.5)
  text(93, y, sprintf("%.1f", results[, "estimate"]),
       adj = 1, font = 2, col = colour)
}

draw_estimates(before, row_y + 1.8, model_colours[1], 16)
draw_estimates(after, row_y - 1.8, model_colours[2], 18)

# Add the axis labels and source
text(axis_x(ticks), 19.5, ticks, col = muted)
text(54, 15.5, "Odds ratio", col = ink)

dev.off() # Close the plot and finish writing the PNG
