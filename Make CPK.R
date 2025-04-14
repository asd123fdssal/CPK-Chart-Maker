library(ggplot2)
library(gridExtra)
library(grid)
library(extrafont)
library(tidyr)
library(tools)

theme_set(theme_bw(base_family = "Noto Sans"))

file_path <- file.choose()
data <- read.csv(file_path, stringsAsFactors = FALSE)
data_file_name <- file_path_sans_ext(basename(file_path))
data$측정스텝 <- ifelse(trimws(data$측정스텝) == "", NA, data$측정스텝)
data <- fill(data, 측정스텝)
data_filtered <- data[trimws(data$측정항목) != "", ]

plot_measurement_grob <- function(row) {
  title_text <- paste0(row["측정스텝"], "_", row["측정항목"])
  spec <- as.character(row[["측정스펙"]])
  spec_parts <- unlist(strsplit(spec, "~"))
  if (length(spec_parts) != 2) return(NULL)
  LSL <- as.numeric(spec_parts[1])
  USL <- as.numeric(spec_parts[2])
  lastValueCol <- ncol(row) - 1
  measurement_values <- as.numeric(unlist(row[4:lastValueCol]))
  measurement_values <- measurement_values[!is.na(measurement_values)]
  n_val <- length(measurement_values)
  if (n_val < 2) return(NULL)
  
  mu <- mean(measurement_values)
  overall_sd <- sd(measurement_values)
  c4n_Val <- calculate_c4(n_val)
  within_sd <- overall_sd / c4n_Val
  
  # 지표 계산
  Pp  <- (USL - LSL) / (6 * overall_sd)
  Ppk <- min((USL - mu) / (3 * overall_sd), (mu - LSL) / (3 * overall_sd))
  PPL <- (mu - LSL) / (3 * overall_sd)
  PPU <- (USL - mu) / (3 * overall_sd)
  
  Cp  <- (USL - LSL) / (6 * within_sd)
  Cpk <- min((USL - mu) / (3 * within_sd), (mu - LSL) / (3 * within_sd))
  CPL <- (mu - LSL) / (3 * within_sd)
  CPU <- (USL - mu) / (3 * within_sd)
  
  if (!is.finite(Cpk)) {
    message("Cpk가 유효하지 않음: ", title_text)
    return(NULL)
  }
  
  x_lower <- LSL
  x_upper <- USL
  margin <- 0.1 * (x_upper - x_lower)
  x_min <- x_lower - margin
  x_max <- x_upper + margin
  
  # 정규분포 그래프
  xVals <- seq(mu - 4 * overall_sd, mu + 4 * overall_sd, length.out = 400)
  dist_df <- data.frame(
    x = xVals,
    y = dnorm(xVals, mean = mu, sd = within_sd)
  )
  
  bin_count <- 100
  bin_width <- (USL - LSL) / bin_count
  
  p <- ggplot(mapping = aes(x = measurement_values)) +
    geom_histogram(aes(y = ..density..), binwidth = bin_width, fill = "lightblue", color = "black") +
    geom_line(data = dist_df, aes(x = x, y = y), color = "red", size = 1) +
    geom_vline(xintercept = c(LSL, USL), color = "blue", linetype = "dashed", size = 1) +
    annotate("text", x = LSL, y = 0, label = "LSL", color = "blue", angle = 90, vjust = 1.2, size = 3) +
    annotate("text", x = USL, y = 0, label = "USL", color = "blue", angle = 90, vjust = 1.2, size = 3) +
    ggtitle(title_text) +
    scale_x_continuous(limits = c(x_min, x_max)) +
    scale_y_continuous(expand = expansion(mult = c(0.15, 0.1))) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank()
    )
  
  caption_text <- sprintf(
    "LSL: %.3f\nUSL: %.3f\nSample Mean: %.5f\nSample N: %d\nstDev(Overall): %.6f\nstDev(Within): %.6f\n\nPp: %.2f\nPPL: %.2f\nPPU: %.2f\nPpk: %.2f\n\nCp: %.2f\nCPL: %.2f\nCPU: %.2f\nCpk: %.2f",
    LSL, USL, mu, n_val, overall_sd, within_sd, Pp, PPL, PPU, Ppk, Cp, CPL, CPU, Cpk
  )
  
  caption_grob <- textGrob(caption_text,
                           x = unit(0, "npc"), just = "left",
                           gp = gpar(fontsize = 11, fontfamily = "Noto Sans"))
  
  grob <- arrangeGrob(p, caption_grob, ncol = 2, widths = c(4, 1))
  return(grob)
}

calculate_c4 <- function(n) {
  sqrt(2 / (n - 1)) * gamma(n / 2) / gamma((n - 1) / 2)
}

plot_list <- lapply(1:nrow(data_filtered), function(i) {
  plot_measurement_grob(data_filtered[i, ])
})
plot_list <- Filter(Negate(is.null), plot_list)

folder_name <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", data_file_name)
if (!dir.exists(folder_name)) dir.create(folder_name)
message("저장 폴더: ", folder_name)

log_file <- file.path(folder_name, "log.txt")
if (file.exists(log_file)) file.remove(log_file)

for(i in 1:nrow(data_filtered)) {
  title_text <- paste0(data_filtered[i, "측정스텝"], "_", data_filtered[i, "측정항목"])
  sanitized_title <- gsub("[^[:alnum:]_#]", "_", title_text)
  fileName <- file.path(folder_name, paste0(sanitized_title, ".png"))
  
  grob <- plot_measurement_grob(data_filtered[i, ])
  if (is.null(grob)) {
    log_message <- paste("Skipped:", title_text)
    message(log_message)
    cat(log_message, "\n", file = log_file, append = TRUE)
    next
  }
  
  png(fileName, width = 800, height = 600)
  grid.newpage()
  grid.draw(grob)
  dev.off()
  
  log_message <- paste("Saved:", fileName)
  message(log_message)
  cat(log_message, "\n", file = log_file, append = TRUE)
}
