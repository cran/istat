#' Download data sets by id (source: I.Stat)
#'
#' @description
#' Download data sets from I.Stat (old ISTAT provider). Alternatively, use get_istatdata to download data sets from IstatData (new ISTAT provider).
#' Note that in this first version of the package, only get_i_stat allows you to download variable labels, while get_istatdata has not this functionality yet.
#' The parameters "id_dataset" to download data sets can be found using list_i_stat function or search_i_stat function.
#'
#' @usage get_i_stat(id_dataset,
#'            start_period=NULL,
#'            end_period=NULL,
#'            recent=FALSE,
#'            csv=FALSE,
#'            xlsx=FALSE,
#'            lang="both")
#'
#' @param id_dataset data set id
#' @param start_period Time value for the start (NULL by default)
#' @param end_period Time value for the end (NULL bu default)
#' @param recent False by default, if TRUE, the function retrieves data from last 10 years
#' @param csv False by default, if TRUE, the function saves the data set to directory as .csv
#' @param xlsx False by default, if TRUE, the function saves the data set to directory as .xlsx
#' @param lang Language parameter for labels ("ita" for Italian, "eng" for English)
#'
#' @return It returns the data set as data.frame. It can be saved to environment or as .csv/.xlsx.
#' @importFrom rsdmx readSDMX
#' @importFrom utils write.csv
#' @importFrom writexl write_xlsx
#' @export
#'
#' @note
#' Downloading may take some time. Future versions will speed up the process.
#'
#' @examples \donttest{get_i_stat("12_60")}
#'\donttest{get_i_stat("12_60", start_period=2015, end_period=2018)}
#'\donttest{get_i_stat("12_60", start_period=2015, end_period=2015)}
#'\donttest{get_i_stat("12_60", recent=TRUE, lang="eng")}
get_i_stat = function(id_dataset,
                      start_period=NULL,
                      end_period=NULL,
                      recent=FALSE,
                      csv=FALSE,
                      xlsx=FALSE,
                      lang="both") {

  if(!is.null(start_period) &!is.null(end_period) &isTRUE(recent)){
    stop("Use recent = TRUE for data from last 10 years.")
  }

  if(is.null(start_period) & is.null(end_period) & isFALSE(recent)){
    dati = readSDMX(providerId = "ISTAT", resource = "data",
                           flowRef    = id_dataset,
                           dsd        = TRUE)
    dati=as.data.frame(dati,labels=T)

  }else if(!is.null(start_period) & !is.null(end_period)){
    dati = readSDMX(providerId = "ISTAT", resource = "data",
                           flowRef  = id_dataset,
                           dsd = TRUE, start=start_period, end=end_period)
    dati=as.data.frame(dati,labels=T)
  } else if(is.null(start_period) & is.null(end_period) & isTRUE(recent)){
    dati = readSDMX(providerId = "ISTAT", resource = "data",
                           flowRef       = id_dataset,
                           dsd   = TRUE,
                           start = as.numeric(format(Sys.Date(), "%Y"))-10,
                           end   = as.numeric(format(Sys.Date(), "%Y")))
    dati=as.data.frame(dati,labels=T)
  }

  if (lang == "ita") {
    dati <- dati[, !grepl("\\.en$", names(dati))]
  } else if (lang == "eng") {
    dati <- dati[, !grepl("\\.it$", names(dati))]
  }

  if (isTRUE(csv) & isTRUE(xlsx)){
    stop("Choose csv OR xlsx")
  }else if (isTRUE(xlsx)){
    write_xlsx(dati, "Dataset.xlsx")
  }else if (isTRUE(csv)){
    write.csv(dati, "Dataset.csv")
  }
    return(dati)

}
