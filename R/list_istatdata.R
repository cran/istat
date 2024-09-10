#' Obtain the complete list of available data sets (source: IstatData)
#'
#' @usage list_istatdata(lang = "ita")
#'
#' @param lang language parameter for labels ("ita" for Italian, "eng" for English)
#'
#' @return It returns the complete list of available data sets from IstatData with their agencyId, id, version and name.
#' @export
#'
#' @examples \donttest{list_istatdata()}
list_istatdata <- function(lang = "ita") {
  link <- "https://esploradati.istat.it/SDMXWS/rest/dataflow"
  istat.fl <- rsdmx::readSDMX(link)
  istat.fl <- as.data.frame(istat.fl)
  
  # removal of unavailable datasets
  istat.fl <- istat.fl %>% 
     filter(!id %in% c("152"
    #                   "111_263_DF_DCSC_FIDIMPRMAN_11"
    #                   ,"111_263_DF_DCSC_FIDIMPRMAN_12"
    #                   ,"111_263_DF_DCSC_FIDIMPRMAN_13"
    #                   ,"111_263_DF_DCSC_FIDIMPRMAN_14"
    #                   ,"111_263_DF_DCSC_FIDIMPRMAN_15"
    #                   ,"111_263_DF_DCSC_FIDIMPRMAN_16"
    #                   ,"111_263_DF_DCSC_FIDIMPRMAN_9"
    #                   ,"111_40_DF_DCSC_FIDIMPRCOSTR_4" #perchè giallo
    #                   ,"111_40_DF_DCSC_FIDIMPRCOSTR_5" #perchè bianco
    #                   ,"111_40_DF_DCSC_FIDIMPRCOSTR_6" #perchè bianco
    #                   ,"117_262_DF_DCSC_FIDIMPRSERV_5"
    #                   ,"117_262_DF_DCSC_FIDIMPRSERV_6"
    #                   ,"117_262_DF_DCSC_FIDIMPRSERV_7"
    #                   ,"117_266_DF_DCSC_FIDCOMM_3"
    #                   ,"117_266_DF_DCSC_FIDCOMM_4" #perchè giallo
    #                   ,"14_121_DF_DCCV_TRASPUB_1"
    #                   ,"14_166_DF_DCCV_INTBONRUM_1"
    #                   ,"14_167_DF_DCCV_MONINQACS_1"
    #                   ,"172_926"
    #                   ,"172_926_DF_DCCV_COMPL1_1"
    #                   ,"172_926_DF_DCCV_COMPL1_12"
    #                   ,"172_926_DF_DCCV_COMPL1_13"
    #                   ,"172_926_DF_DCCV_COMPL1_14"
    #                   ,"172_926_DF_DCCV_COMPL1_15"
    #                   ,"172_926_DF_DCCV_COMPL1_16"
    #                   ,"172_926_DF_DCCV_COMPL1_17"
    #                   ,"172_926_DF_DCCV_COMPL1_18"
    #                   ,"172_926_DF_DCCV_COMPL1_19"
    #                   ,"172_926_DF_DCCV_COMPL1_2"
    #                   ,"172_926_DF_DCCV_COMPL1_20"
    #                   ,"172_926_DF_DCCV_COMPL1_21"
    #                   ,"172_926_DF_DCCV_COMPL1_22"
    #                   ,"172_926_DF_DCCV_COMPL1_23"
    #                   ,"172_926_DF_DCCV_COMPL1_24"
    #                   ,"172_926_DF_DCCV_COMPL1_25"
    #                   ,"172_926_DF_DCCV_COMPL1_26"
    #                   ,"172_926_DF_DCCV_COMPL1_3"
    #                   ,"172_926_DF_DCCV_COMPL1_4"
    #                   ,"172_926_DF_DCCV_COMPL1_5"
    #                   ,"25_74_DF_DCIS_NATI1_10"
    #                   ,"25_74_DF_DCIS_NATI1_11"
    #                   ,"25_74_DF_DCIS_NATI1_12"
    #                   ,"25_74_DF_DCIS_NATI1_13"
    #                   ,"25_74_DF_DCIS_NATI1_14"
    #                   ,"25_74_DF_DCIS_NATI1_15"
    #                   ,"25_74_DF_DCIS_NATI1_16"
    #                   ,"25_74_DF_DCIS_NATI1_17"
    #                   ,"25_74_DF_DCIS_NATI1_7"
    #                   ,"25_74_DF_DCIS_NATI1_8"
    #                   ,"25_74_DF_DCIS_NATI1_9"
    #                   ,"60_195_DF_DCIS_MUSVIS_5"
    #                   ,"610_119_DF_DCCV_TRASPRIV_1"
    #                   ,"68_698_DF_DCCV_VIAGGI_CHARACT_CAPI_4"
    #                   ,"68_698_DF_DCCV_VIAGGI_CHARACT_CAPI_5"
    #                   ,"68_698_DF_DCCV_VIAGGI_CHARACT_CAPI_6"
    #                   ,"68_698_DF_DCCV_VIAGGI_CHARACT_CAPI_7"
    #                   ,"68_698_DF_DCCV_VIAGGI_CHARACT_CAPI_8"
    #                   ,"73_1071_DF_DCCV_IMPUTATI_A_10"
    #                   ,"73_1071_DF_DCCV_IMPUTATI_A_11"
    #                   ,"73_1071_DF_DCCV_IMPUTATI_A_12"
    #                   ,"73_1071_DF_DCCV_IMPUTATI_A_13"
    #                   ,"73_1072_DF_DCCV_IMPUTATI_M_10"
    #                   ,"73_1072_DF_DCCV_IMPUTATI_M_11"
    #                   ,"73_1072_DF_DCCV_IMPUTATI_M_12"
    #                   ,"73_1072_DF_DCCV_IMPUTATI_M_13"
    #                   ,"73_230_DF_DCCV_AUTVITTPS_5"
    #                   ,"73_230_DF_DCCV_AUTVITTPS_6"
    #                   ,"73_230_DF_DCCV_AUTVITTPS_7"
    #                   ,"73_230_DF_DCCV_AUTVITTPS_8"
    #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_13"
    #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_14"
    #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_15"
    #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_16"
    #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_17"
    #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_18"
    #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_19"
    #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_20"
    #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_21"
    #                   ,"73_436_DF_DCCV_PROCEEDCRIME_M_22"
    #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_14"
    #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_15"
    #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_16"
    #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_17"
    #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_18"
    #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_19"
    #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_20"
    #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_21"
    #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_22"
    #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_23"
    #                   ,"73_440_DF_DCCV_PROCEEDCRIME_A_24"
    #                   ,"73_58_DF_DCCV_CONDCRIM1_14"
    #                   ,"73_58_DF_DCCV_CONDCRIM1_15"
    #                   ,"73_58_DF_DCCV_CONDCRIM1_16"
    #                   ,"73_58_DF_DCCV_CONDCRIM1_17"
    #                   ,"73_58_DF_DCCV_CONDCRIM1_18"
    #                   ,"73_58_DF_DCCV_CONDCRIM1_19"
    #                   ,"73_58_DF_DCCV_CONDCRIM1_20"
    #                   ,"73_58_DF_DCCV_CONDCRIM1_21"
    #                   ,"73_58_DF_DCCV_CONDCRIM1_22"
    #                   ,"73_59_DF_DCCV_CONDGEO1_15"
    #                   ,"73_59_DF_DCCV_CONDGEO1_16"
    #                   ,"73_59_DF_DCCV_CONDGEO1_17"
    #                   ,"73_59_DF_DCCV_CONDGEO1_18"
    #                   ,"73_59_DF_DCCV_CONDGEO1_19"
    #                   ,"73_59_DF_DCCV_CONDGEO1_20"
    #                   ,"73_59_DF_DCCV_CONDGEO1_21"
    #                   ,"73_59_DF_DCCV_CONDGEO1_22"
    #                   ,"73_59_DF_DCCV_CONDGEO1_23"
    #                   ,"83_63_DF_DCCV_AVQ_PERSONE_125"
    #                   ,"92_504_NOTE_TIME"
                      ,"22_289_DF_DCIS_POPRES1_3"
                      ,"22_315_DF_DCIS_POPORESBIL1_10"
                      ,"22_315_DF_DCIS_POPORESBIL1_11"
                      ,"22_315_DF_DCIS_POPORESBIL1_12"
                      ,"22_315_DF_DCIS_POPORESBIL1_13"
                      ,"22_315_DF_DCIS_POPORESBIL1_14"
                      ,"22_315_DF_DCIS_POPORESBIL1_15"
                      ,"22_315_DF_DCIS_POPORESBIL1_16"
                      ,"22_315_DF_DCIS_POPORESBIL1_17"
                      ,"22_315_DF_DCIS_POPORESBIL1_18"
                      ,"22_315_DF_DCIS_POPORESBIL1_19"
                      ,"22_315_DF_DCIS_POPORESBIL1_20"
                      ,"22_315_DF_DCIS_POPORESBIL1_21"
                      ,"22_315_DF_DCIS_POPORESBIL1_22"
                      ,"22_315_DF_DCIS_POPORESBIL1_23"
                      ,"22_315_DF_DCIS_POPORESBIL1_3"
                      ,"22_315_DF_DCIS_POPORESBIL1_4"
                      ,"22_315_DF_DCIS_POPORESBIL1_5"
                      ,"22_315_DF_DCIS_POPORESBIL1_6"
                      ,"22_315_DF_DCIS_POPORESBIL1_7"
                      ,"22_315_DF_DCIS_POPORESBIL1_8"
                      ,"22_315_DF_DCIS_POPORESBIL1_9"
                      ,"DF_BULK_DCCN_FPA"
                      ,"DF_BULK_DCSC_CAPACOFTUR"
                      ,"DF_BULK_DCSC_OCCUPCOLLE"
                      ,"DF_BULK_DCSC_TURISAREA"
                      ,"DF_BULK_DCSP_FOI1B2010_TB4"
                      ,"DF_BULK_DCSP_FOI1B2015_NUTS3"
                      ,"DF_BULK_DCSP_FOI1B2015_TB4"
                      ,"DF_BULK_DCSP_NIC1B2015_NUTS1"
                      ,"DF_BULK_DCSP_NIC1B2015_NUTS2"
                      ,"DF_BULK_DCSP_NIC1B2015_NUTS3"
                      ,"DF_BULK_DCSP_NIC1B2015_RECONNIC"
                      ,"DF_BULK_DCSP_NIC1B2015_SPLICECO"
                      ,"DF_BULK_DCSP_NIC1B2015_SPLICI"
                      ,"DF_BULK_DCSP_NIC1B2015_TB2"
                      ,"DF_BULK_DCSP_NIC1B2015_TB3"
                      ,"DF_BULK_DCSP_NICDUEB2010_CPI"
                      ,"DF_BULK_DCSP_NICDUEB2010_TB2"
                      ,"DF_BULK_DCSP_NICDUEB2010_TB3"
                      ,"DF_BULK_DCSP_NICUNOBB2010_CPI"
                      ,"DF_BULK_DCSP_NICUNOBB2010_NIC"
                      ,"DF_BULK_DCSP_NICUNOBB2010_TB2"
                      ,"DF_BULK_DCSP_NICUNOBB2010_TB3"
                      ,"DF_BULK_DIVORCESSE"
                      ,"DF_BULK_ENTERPRISE"
                      ,"DF_BULK_FARMSTRUCT"
                      ,"DF_BULK_FISHERYBRO"
                      ,"DF_BULK_FLOWERSBRO"
                      ,"DF_BULK_FORESTRYBR"
                      ,"DF_BULK_HIGHSCHOOL"
                      ,"DF_BULK_HOURLYWAGE"
                      ,"DF_BULK_HUNTINGBRO"
                      ,"DF_BULK_LOCALUNITS"
                      ,"DF_BULK_PERMANENTC"
                      ,"DF_BULK_POPOLAZION"
                      ,"DF_BULK_PREVIOUSBA"
                      ,"DF_BULK_PROCEEDING"
                      ,"DF_BULK_PROTECTION"
                      ,"DF_BULK_PROTESTSSE"
                      ,"DF_BULK_PUBLISHEDB"
                      ,"DF_BULK_REPORTSOFP"
                      ,"DF_BULK_SCHOOLSSER"
                      ,"DF_BULK_SEEDSBROKE"
                      ,"DF_BULK_SEPARATION"
                      ,"DF_BULK_STATEARCHI"
                      ,"DF_BULK_STATEOWNED"
                      ,"DF_BULK_STIMEPERGR"
                      ,"DF_BULK_SUICIDESAN"
                      ,"DF_BULK_SUICIDI"
                      ,"DF_BULK_SURVEYCALE"
                      ,"DF_BULK_UNIVERSITY"
                      ,"DF_BULK_UNIVERSITY_DEGREE"
                      ,"DF_DCAT_CENSAGRIC2020_AGE_MAR"
                      ,"DF_DIFF_REPORT"
                      ,"DF_TEST_DCIS_ABORTISPONTL"))

  agencyId <- istat.fl$agencyID
  ID <- istat.fl$id
  version <- istat.fl$version

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

  Data <- data.frame(agencyId, ID, version, Name)

  return(Data)
}

