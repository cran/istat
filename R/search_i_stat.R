#' Search data sets by keywords (source: I.Stat)
#' @description
#' Search I.Stat data sets by keywords. To download data sets, use "get_i_stat" function and insert data set id.
#'
#' @usage search_i_stat(keywords,
#'               lang = "ita")
#'
#' @param keywords keyword(s) to search data sets
#' @param lang language parameter for labels ("ita" for Italian, "eng" for English)
#'
#' @return It returns a list of data sets containing the keyword(s) with their id and name.
#' @importFrom rsdmx readSDMX
#' @export
#' @note
#'Searching may take some time. Future versions will speed up the process.
#'
#' @examples \donttest{search_i_stat(c( "incidenti", "stradali"))}
#'\donttest{search_i_stat("population", lang="eng")}
search_i_stat <- function(keywords, lang = "ita") {

  istat.fl <- readSDMX(providerId = "ISTAT", resource = "dataflow")
  istat.fl <- as.data.frame(istat.fl)
  
  #istat.fl <- istat.fl %>% 
  #  filter(!id %in% "14_121")

  if (lang == "ita") {
    name_column <- "Name.it"
  } else if (lang == "eng") {
    name_column <- "Name.en"
  } else {
    stop("Wrong language parameter. Use 'ita' for Italian and 'eng' for English.")
  }

  search_keyword <- function(keyword) {
    grepl(keyword, istat.fl[[name_column]], ignore.case = TRUE)
  }

  all_matches <- list()

  if (length(keywords) == 1) {
    id <- istat.fl$id[grep(keywords, istat.fl[[name_column]], ignore.case = TRUE)]
    name <- istat.fl[[name_column]][grep(keywords, istat.fl[[name_column]], ignore.case = TRUE)]
    match <- cbind(id, name)

    if (nrow(match) == 0 && lang=="ita") {
      message("Non ci sono dataset contenenti la parola chiave inserita.")
    } else if (nrow(match) == 0 && lang=="eng") {
      message("There are no dataset including that keyword.")
    }
    else {
      match
    }

  } else if (length(keywords) > 1) {
    index_list <- lapply(keywords, function(k) grep(k, istat.fl[[name_column]], ignore.case = TRUE))
    index <- Reduce(intersect, index_list)
    id <- istat.fl$id[index]
    name <- istat.fl[[name_column]][index]
    match2 <- cbind(id, name)

    for (kw in keywords) {
      matches <- search_keyword(kw)
      id <- istat.fl$id[matches]
      name <- istat.fl[[name_column]][matches]
      match <- cbind(id, name)
      all_matches[[kw]] <- match
    }

    if (sum(sapply(all_matches, nrow)) == 0 && lang == "ita") {
      message("Non ci sono dataset contenenti le parole chiave inserite.")
    } else if (sum(sapply(all_matches, nrow)) == 0 && lang == "eng") {
      message("There are no datasets including those keywords.")
    } else {

      list(all_matches, match2)
    }
  }
}

