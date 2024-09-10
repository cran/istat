#' Filter data set interactively
#'
#' @description
#' An interactive and more intuitive version of filter_istat function. It filters data set by column(s). Takes as input a data.frame (not only ISTAT ones) and allows you to select for which column(s) value(s) to filter the data set interactively.
#'
#' @usage filter_istat_interactive(dataset, lang = "ita")
#'
#' @param dataset as data.frame
#' @param lang language parameter for labels ("ita" for Italian, "eng" for English)
#'
#' @return It returns the filtered data set.
#' @importFrom rsdmx readSDMX
#' @importFrom utils write.csv
#' @importFrom writexl write_xlsx
#' @export
#'
#' @note
#' In this first version, language parameter works only with data sets downloaded with get_i_stat (provider I.Stat).
#'
#' @examples \donttest{filter_istat_interactive(iris)}
filter_istat_interactive<- function(dataset, lang = "ita") {
  if (lang == "ita") {
    columns <- colnames(dataset)
    cat("Colonne disponibili:\n")
    print(columns)

    colonna_scelta <- readline(prompt = "Inserisci la colonna o le colonne (separate da virgola): ")
    colonna_scelta <- strsplit(colonna_scelta, ",")[[1]]
    colonna_scelta <- trimws(colonna_scelta)

    if (all(colonna_scelta %in% columns)) {
      subsetdf <- dataset
      for (col in colonna_scelta) {
        dato <- unique(dataset[[col]])
        cat("Valori disponibili per la colonna", col, ":\n")
        print(dato)

        valore_scelto <- readline(prompt = paste("Inserisci i valori scelti per la colonna", col, "(separati da virgola): "))
        valore_scelto <- strsplit(valore_scelto, ",")[[1]]
        valore_scelto <- trimws(valore_scelto)

        if (all(valore_scelto %in% dato)) {
          subsetdf <- subsetdf[subsetdf[[col]] %in% valore_scelto, ]
        } else {
          stop("Uno o piu' valori specificati non esistono nella colonna", col, ":(. Per favore, seleziona solo valori disponibili.")
        }
      }
      return(as.data.frame(subsetdf))
    } else {
      stop("Una o piu' colonne specificate non esistono nel dataset :(. Per favore, seleziona solo colonne disponibili.")
    }

  } else if (lang == "eng") {
    columns <- colnames(dataset)
    cat("Available columns:\n")
    print(columns)

    colonna_scelta <- readline(prompt = "Enter the column(s) (separated by comma): ")
    colonna_scelta <- strsplit(colonna_scelta, ",")[[1]]
    colonna_scelta <- trimws(colonna_scelta)

    if (all(colonna_scelta %in% columns)) {
      subsetdf <- dataset
      for (col in colonna_scelta) {
        dato <- unique(dataset[[col]])
        cat("Available values for column", col, ":\n")
        print(dato)

        valore_scelto <- readline(prompt = paste("Enter the chosen values for column", col, "(separated by comma): "))
        valore_scelto <- strsplit(valore_scelto, ",")[[1]]
        valore_scelto <- trimws(valore_scelto)

        if (all(valore_scelto %in% dato)) {
          subsetdf <- subsetdf[subsetdf[[col]] %in% valore_scelto, ]
        } else {
          stop("One or more specified values do not exist in column", col, ":(. Please select only available values.")
        }
      }
      return(as.data.frame(subsetdf))
    } else {
      stop("One or more specified columns do not exist in the dataset :(. Please select only available columns.")
    }

  } else {
    stop("Wrong language parameter. Select 'ita' for Italian or 'eng' for English.")
  }
}


