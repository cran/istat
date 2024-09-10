#' Plot dataset interactively
#'
#' @description
#' Build different types of exploratory graphs (scatter plot, bar plot, pie chart). You can interactively choose the plot that you want create. Once you have chosen the plot, you can interactively choose the variables from the data set for which you want to build the plot.
#'
#' @usage plot_interactive(df)
#'
#' @param df data set as data.frame
#'
#' @return It returns the chosen plot.
#' @export
#'
#' @note plot_interactive allows you to have an idea about the general trend of your data, and it's intended to be used with exploratory purpose.
#' @examples 
#' if(interactive()) {
#'   plot_interactive(iris)
#' }
plot_interactive <- function(df) {
  if (!is.data.frame(df)) {
    stop("Inserted dataset is not a data frame")
  }

  cat("Available plots:\n1: Scatter Plot\n2: Bar plot\n3: Pie chart\n")
  chart_type <- as.numeric(readline(prompt = "Choose type of plot (1, 2 or 3): "))

  cat("Available variables:\n")
  columns <- colnames(df)
  print(columns)

  x_var <- NULL
  y_var <- NULL
  group_var <- NULL
  pie_var <- NULL

  if (chart_type == 1) { # Scatter Plot
    x_var <- readline(prompt = "Variable X (insert name): ")
    y_var <- readline(prompt = "Variable Y (insert name): ")
    group_var <- readline(prompt = "Grouping variable (optional, press enter if not necessary): ")

    if (!(x_var %in% columns) || !(y_var %in% columns)) {
      stop("One or each selected variables are not on dataframe.")
    }
    if (group_var != "" && !(group_var %in% columns)) {
      stop("Selected grouping variable is not on dataframe.")
    }

  } else if (chart_type == 2) { # Bar plot
    x_var <- readline(prompt = "Variable X (insert name): ")
    y_var <- readline(prompt = "Variable Y (insert name): ")
    group_var <- readline(prompt = "Grouping variable (optional, press enter if not necessary): ")

    if (!(x_var %in% columns) || !(y_var %in% columns)) {
      stop("One or each selected variables are not on dataframe.")
    }
    if (group_var != "" && !(group_var %in% columns)) {
      stop("Selected grouping variable is not on dataframe.")
    }

  } else if (chart_type == 3) { # Pie chart
    pie_var <- readline(prompt = "Choose the variable for pie chart: ")

    if (!(pie_var %in% columns)) {
      stop("Selected variable for pie chart is not on dataframe.")
    }

  } else {
    stop("Plot type not available. Choose between 1, 2 o 3.")
  }

  
  p <- NULL
  if (chart_type == 1) {
    p <- ggplot(df, aes_string(x = x_var, y = y_var, color = if (!is.null(group_var) && group_var != "") group_var else NULL)) +
      geom_point() +
      theme_minimal() +
      labs(title = paste("Scatter plot of", x_var, "and", y_var))

  } else if (chart_type == 2) {
    p <- ggplot(df, aes_string(x = x_var, y = y_var, fill = if (!is.null(group_var) && group_var != "") group_var else NULL)) +
      geom_bar(stat = "identity", position = if (!is.null(group_var) && group_var != "") "dodge" else "stack") +
      theme_minimal() +
      labs(title = paste("Bar plot of", x_var, "and", y_var))

  } else if (chart_type == 3) {
    df_pie <- df %>%
      group_by(across(all_of(pie_var))) %>%
      summarise(total_value = n(), .groups = 'drop')

    p <- ggplot(df_pie, aes_string(x = "''", y = "total_value", fill = pie_var)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(title = paste("Pie chart of", pie_var))
  }

  print(p)
}

