#' Obtain the complete list of available data sets (source: I.Stat)
#'
#' @usage list_i_stat(lang = "ita")
#'
#' @param lang language parameter for labels ("ita" for Italian, "eng" for English)
#'
#' @return It returns the complete list of available data sets from I.Stat with their id and name.
#' @export
#'
#' @examples \donttest{list_i_stat()}
list_i_stat <- function(lang = "ita") {

  istat.fl <- rsdmx::readSDMX(providerId = "ISTAT", resource = "dataflow")
  istat.fl <- as.data.frame(istat.fl)

  #istat.fl <- istat.fl %>% 
  #  filter(!id %in% "14_121")
   
  ID <- istat.fl$id

  if (lang == "ita") {
    if ("Name.it" %in% colnames(istat.fl)) {
      Name <- istat.fl$Name.it
    } else {
      stop("La colonna Name.it non esiste nel dataframe.")
    }
  } else if (lang == "eng") {
    if ("Name.en" %in% colnames(istat.fl)) {
      Name <- istat.fl$Name.en
    } else {
      stop("La colonna Name.en non esiste nel dataframe.")
    }
  } else {
    stop("Wrong language parameter. Select 'ita' for Italian or 'eng' for English.")
  }

  Data <- data.frame(ID, Name)

  return(Data)
}
