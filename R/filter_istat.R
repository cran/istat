#' Filter data sets
#'
#'@description
#' Filter data set by column(s). Takes as input a data.frame (not only ISTAT ones) and allows you to select for which column(s) value(s) to filter the dataset. Alternatively, use filter_istat_interactive for an interactive version.
#'
#' @usage filter_istat(dataset, columns, datatype, lang = "ita")
#'
#' @param dataset as data.frame
#' @param columns data set column(s) for which you want to filter the data. If you want to filter for more than one column, write c("column1","column2", ...) as argument.
#' @param datatype column(s) value(s) for which you want to filter the data. Write as many datatype as many columns that you selected in "columns" (as in examples).
#' @param lang language parameter for labels ("ita" for Italian, "eng" for English)
#'
#' @return It returns the filtered data set.
#' @export
#'
#' @examples \donttest{filter_istat(iris, columns = "Species", datatype = "setosa") #Here,
#' #the function filters the data set 'iris' for the value 'setosa' of the column 'Species'.}
#' \donttest{filter_istat(iris, columns = c("Species","Petal.Width"),
#' datatype = list(c("virginica","setosa"), c("0.1","1.9")))
#' # Here, the function filters the data set 'iris' for the values 'virginica'
#' #and 'setosa' of the column 'Species' and for the values '0.1' and '1.9' of
#' #the column 'Petal.Width'.}
filter_istat <- function(dataset, columns, datatype, lang = "ita") {

  if (!is.list(columns)) columns <- as.list(columns)
  if (!is.list(datatype)) datatype <- as.list(datatype)

  subsetdf <- dataset

  for (i in seq_along(columns)) {
    column <- columns[[i]]
    data_type <- datatype[[i]]

    dato <- unique(dataset[[column]])

    if (lang == "ita") {
      label <- unique(dataset$TIPO_DATO_label.it) # label works only with old provider (I.Stat)
    } else if (lang == "eng") {
      label <- unique(dataset$TIPO_DATO_label.en) # #label works only with old provider (I.Stat)
    } else {
      stop("Wrong language parameter. Select 'ita' for Italian or 'eng' for English.")
    }

    valori <- cbind(dato, label)

    if (all(data_type %in% dato)) {
      subsetdf <- subsetdf[subsetdf[[column]] %in% data_type, ]
    } else {
      if (lang == "ita") {
        stop("Una o piu' colonne specificate non esistono nel dataset :(. Per favore, seleziona solo valori disponibili.")
      } else {
        stop("One or more specified columns values are not contained in the dataset :( . Please select only available values.")
      }
    }
  }

  return(as.data.frame(subsetdf))
}

