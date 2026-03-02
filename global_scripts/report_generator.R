# report_generator.R
# Functions to generate PDF reports

library(plotly)

#' Save a plotly chart as a static PNG image
#' @param plot_obj A plotly object
#' @param filepath Where to save the PNG
#' @param width Width in pixels
#' @param height Height in pixels
save_plotly_as_png <- function(plot_obj, filepath, width = 1000, height = 500) {
  tryCatch({
    # Use orca or kaleido to export
    plotly::save_image(plot_obj, file = filepath, width = width, height = height)
    return(TRUE)
  }, error = function(e) {
    # Fallback: try using webshot
    tryCatch({
      tmphtml <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(plot_obj, tmphtml, selfcontained = TRUE)
      webshot2::webshot(tmphtml, file = filepath, vwidth = width, vheight = height, delay = 2)
      file.remove(tmphtml)
      return(TRUE)
    }, error = function(e2) {
      warning(paste("Could not save plot:", e2$message))
      return(FALSE)
    })
  })
}

#' Generate a brand overview PDF report
#' @param brand_name Character
#' @param ai_summary Character, the AI-generated analysis text
#' @param rankings_data Data frame with brand scores
#' @param trend_plot Plotly object for the trend chart
#' @param spider_plot Plotly object for the spider chart
#' @param output_path Where to save the PDF
generate_brand_report <- function(brand_name,
                                  ai_summary = "",
                                  rankings_data = NULL,
                                  trend_plot = NULL,
                                  spider_plot = NULL,
                                  output_path = NULL) {
  
  if (is.null(output_path)) {
    output_path <- tempfile(fileext = ".pdf")
  }
  
  # Create temp directory for plot images
  tmp_dir <- tempdir()
  trend_path <- file.path(tmp_dir, "trend_plot.png")
  spider_path <- file.path(tmp_dir, "spider_plot.png")
  
  # Save plots as images
  if (!is.null(trend_plot)) {
    save_plotly_as_png(trend_plot, trend_path, width = 1000, height = 400)
  } else {
    trend_path <- ""
  }
  
  if (!is.null(spider_plot)) {
    save_plotly_as_png(spider_plot, spider_path, width = 600, height = 500)
  } else {
    spider_path <- ""
  }
  
  # Prepare rankings data
  if (!is.null(rankings_data)) {
    rankings_data$is_main <- rankings_data$main_brand_flag
  }
  
  # Render the report
  rmarkdown::render(
    input = "report_template.Rmd",
    output_file = output_path,
    params = list(
      brand_name = brand_name,
      report_type = "brand",
      prompt_text = "",
      ai_summary = ai_summary %||% "",
      rankings_data = rankings_data,
      trend_plot_path = trend_path,
      spider_plot_path = spider_path,
      generated_at = format(Sys.time(), "%B %d, %Y at %I:%M %p")
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  
  return(output_path)
}

#' Generate a prompt overview PDF report
generate_prompt_report <- function(brand_name,
                                   prompt_text = "",
                                   ai_summary = "",
                                   rankings_data = NULL,
                                   trend_plot = NULL,
                                   spider_plot = NULL,
                                   output_path = NULL) {
  
  if (is.null(output_path)) {
    output_path <- tempfile(fileext = ".pdf")
  }
  
  tmp_dir <- tempdir()
  trend_path <- file.path(tmp_dir, "prompt_trend_plot.png")
  spider_path <- file.path(tmp_dir, "prompt_spider_plot.png")
  
  if (!is.null(trend_plot)) {
    save_plotly_as_png(trend_plot, trend_path, width = 1000, height = 400)
  } else {
    trend_path <- ""
  }
  
  if (!is.null(spider_plot)) {
    save_plotly_as_png(spider_plot, spider_path, width = 600, height = 500)
  } else {
    spider_path <- ""
  }
  
  if (!is.null(rankings_data)) {
    rankings_data$is_main <- rankings_data$main_brand_flag
  }
  
  rmarkdown::render(
    input = "report_template.Rmd",
    output_file = output_path,
    params = list(
      brand_name = brand_name,
      report_type = "prompt",
      prompt_text = prompt_text,
      ai_summary = ai_summary %||% "",
      rankings_data = rankings_data,
      trend_plot_path = trend_path,
      spider_plot_path = spider_path,
      generated_at = format(Sys.time(), "%B %d, %Y at %I:%M %p")
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  
  return(output_path)
}