library(ggplot2)
## We take this function from the see package
# (but implement it this way to minimise dependencies)
theme_modern <- function(base_size = 11,
                         base_family = "",
                         plot.title.size = 15,
                         plot.title.face = "plain",
                         plot.title.space = 20,
                         plot.title.position = "plot",
                         legend.position = "right",
                         axis.title.space = 20,
                         legend.title.size = 13,
                         legend.text.size = 12,
                         axis.title.size = 13,
                         axis.title.face = "plain",
                         axis.text.size = 12,
                         axis.text.angle = NULL,
                         tags.size = 15,
                         tags.face = "bold") {
  # Remove legend title if necessary
  if (is.null(plot.title.size)) {
    plot.title.size <-
      element_text(
        size = plot.title.size,
        face = plot.title.face,
        margin = margin(0, 0, plot.title.space, 0)
      )
  } else if (plot.title.size == "none") {
    plot.title.size <- element_blank()
  } else {
    plot.title.size <-
      element_text(
        size = plot.title.size,
        face = plot.title.face,
        margin = margin(0, 0, plot.title.space, 0)
      )
  }
  
  # Remove legend title if necessary
  if (is.null(legend.title.size)) {
    legend.title.size <- element_text(size = legend.title.size)
  } else if (legend.title.size == "none") {
    legend.title.size <- element_blank()
  } else {
    legend.title.size <- element_text(size = legend.title.size)
  }
  
  # Remove axis title if necessary
  if (is.null(axis.title.size)) {
    axis.title.size <- element_text(size = axis.title.size, face = axis.title.face)
  } else if (axis.title.size == "none") {
    axis.title.size <- element_blank()
  } else {
    axis.title.size <- element_text(size = axis.title.size, face = axis.title.face)
  }
  
  # Remove axis text if necessary
  if (is.null(axis.text.size)) {
    axis.text.size <- element_text(size = axis.text.size)
  } else if (axis.text.size == "none") {
    axis.text.size <- element_blank()
  } else {
    axis.text.size <- element_text(size = axis.text.size)
  }
  
  # Rotate
  if (!is.null(axis.text.angle)) {
    hjust <- 1
  } else {
    hjust <- NULL
  }
  
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = plot.title.size,
      plot.title.position = plot.title.position,
      legend.position = legend.position,
      legend.text = element_text(size = legend.text.size),
      legend.title = legend.title.size,
      legend.key = element_blank(),
      legend.spacing.x = unit(2, "pt"),
      axis.title.y = element_text(margin = margin(t = 0, r = axis.title.space, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = axis.title.space, r = 0, b = 0, l = 0)),
      axis.title = axis.title.size,
      axis.text.x = element_text(angle = axis.text.angle, hjust = hjust),
      axis.text = axis.text.size,
      axis.ticks = element_blank(),
      plot.tag = element_text(size = tags.size, face = tags.face),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    )
}
w <- 8.268 
theme_set(
  theme_modern(11) + 
    theme(
      strip.text = element_text(size = 13, face="bold"),
      axis.title = element_text(size = 13, face="bold"),
      plot.title = element_text(size = 18, face="bold"),
      legend.title = element_text(size = 13, face="bold"),
      legend.background = element_rect(fill = NA, colour = "black"),
      legend.position = "bottom",
    )
)
