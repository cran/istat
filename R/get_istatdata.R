#' Download data set by agencyId, id and version (source: IstatData)
#'
#' @description
#' Download data sets from IstatData (new ISTAT provider). Alternatively, use get_i_stat to download data sets from I.Stat (old ISTAT provider).
#' Note that in this first version of the package, only get_i_stat allows you to download variable labels, while get_istatdata has not this functionality yet.
#' The parameters "agencyId","dataset_id" and "version" to download the data sets can be found using list_istatdata function or search_istatdata function.
#'
#' @usage get_istatdata(agencyId,
#'              dataset_id,
#'              version,
#'              start=NULL,
#'              end=NULL,
#'              recent=FALSE,
#'              csv=FALSE,
#'              xlsx=FALSE)
#'
#' @param agencyId data set agencyId
#' @param dataset_id data set id
#' @param version data set version
#' @param start Time value for the start (NULL by default)
#' @param end Time value for the end (NULL bu default)
#' @param recent False by default, if TRUE, the function retrieves data from last 10 years
#' @param csv False by default, if TRUE, the function saves the data set to directory as .csv
#' @param xlsx False by default, if TRUE, the function saves the data set to directory as .xlsx
#'
#' @return It returns data set as data.frame. It can be saved to environment or as .csv/.xlsx.
#' @importFrom rsdmx readSDMX
#' @export
#'
#' @note
#' Downloading may take some time. Future versions will speed up the process.
#' @examples \donttest{get_istatdata("IT1", "12_60_DF_DCCV_CONSACQUA_2" , "1.0",recent = TRUE)}
#' \donttest{get_istatdata("IT1", "12_60_DF_DCCV_CONSACQUA_2" , "1.0", start = 2015, end = 2018)}
get_istatdata <- function(agencyId,
                             dataset_id,
                             version,
                             start=NULL,
                             end=NULL,
                             recent=FALSE,
                             csv=FALSE,
                             xlsx=FALSE) {

  base_url <- "https://esploradati.istat.it/SDMXWS/rest"

  if (!is.null(start) & !is.null(end) & recent == FALSE){
    url <- paste0(base_url, "/data/", agencyId, ",", dataset_id, ",", version, "/ALL/?detail=full&startPeriod=", start, "-01-01&endPeriod=", end, "-12-31&dimensionAtObservation=TIME_PERIOD")
  } else if (is.null(start) & is.null(end) & recent == TRUE){
    start <- as.numeric(format(Sys.Date(), "%Y"))-10
    end <- as.numeric(format(Sys.Date(), "%Y"))
    url <- paste0(base_url, "/data/", agencyId, ",", dataset_id, ",", version, "/ALL/?detail=full&startPeriod=", start, "-01-01&endPeriod=", end, "-12-31&dimensionAtObservation=TIME_PERIOD")
  } else if (is.null(start) & is.null(end) & recent == FALSE){
    url <- paste0(base_url, "/data/",agencyId, ",", dataset_id, ",", version, "/ALL/?detail=full&dimensionAtObservation=TIME_PERIOD")
  } else {
    stop("Wrong parameters. Use start and end to define a period or recent = TRUE to obtain data from last 10 years.")
  }

  message(paste("Requesting URL:", url))
  sdmx_data <- rsdmx::readSDMX(url)
  data <- as.data.frame(sdmx_data)

  if (isTRUE(csv) & isTRUE(xlsx)){
    stop("Choose csv OR xlsx")
  }else if (isTRUE(xlsx)){
    write_xlsx(data, "Dataset.xlsx")
    return(data)
  }else if (isTRUE(csv)){
    write.csv(data, "Dataset.csv")
    return(data)
  }
    return(data)

}
