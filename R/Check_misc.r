#I. Check Last Name ####

CheckLast <- function(x){
  assertthat::assert_that(is.character(x))

  last <- c("BEN", "DA", "DAL", "DE", "DEL", "DEN", "DER", "DI", "DU", "LA", "LE", "MC", "SAN", "ST", "STE", "VAN", "VANDER", "VEL", "VON", "MC", "LOS", "MAC", "LAS", "LES")

  ifelse(toupper(x) %in% last, TRUE, FALSE)
}

#II. Check if Middle Name has Length of 1 ####

CheckMI <- function(x) {
  assertthat::assert_that(is.character(x))

  ifelse(nchar(x) == 1, TRUE, FALSE)
}

#III. Check if Word is a Prefix ####

CheckPrefix <- function(x) {
  assertthat::assert_that(is.character(x))

  prefixes <- c("MR", "MRS", "MS", "MISS", "DR", "REV", "FR", "PR", "ATTY", "PROF", "HON", "PRES", "GOV", "OFC", "SIR", "MISTER", "MISTRESS", "REVEREND", "PASTOR", "FATHER", "DOCTOR", "ATTORNEY", "PROFESSOR", "HONORABLE", "PRESIDENT", "GOVERNOR", "OFFICER")
  prefixes_expanded <- c("MSGR", "MONSIGNOR", "SR", "SISTER", "BR", "BROTHER", "SUPT", "SUPERINTENDENT", "REP", "REPRESENTATIVE", "SEN", "SENATOR", "AMB", "AMBASSADOR", "TREAS", "TREASURER", "SEC", "SECRETARY")
  prefixes_military <- c("PVT", "PRIVATE", "CPL", "CORPORAL", "SGT", "SARGENT", "ADM", "ADMINISTRATIVE", "MAJ", "MAJOR", "CAPT", "CAPTAIN", "CMDR", "COMMANDER", "LT", "LIEUTENANT", "COL", "COLONEL", "GEN", "GENERAL")

  ifelse(toupper(x) %in% c(prefixes, prefixes_military, prefixes_expanded), TRUE, FALSE)
}

#IV. Check if Word is a Suffix ####

CheckSuffix <- function(x) {
  assertthat::assert_that(is.character(x))

  suffixes <- c("SR", "JR", "SENIOR", "JUNIOR", "I", "II", "III", "IV", "1ST", "2ND", "3RD", "4TH")
  suffixes_pro <- c("MD", "DC", "DDS", "DMD", "DO", "DVM", "EDD", "ESQ", "JD", "LLD", "OD", "PHD", "RET", "RN", "RNC", "CPA", "CLU", "CFRE")
  suffixes_military <- c("USA", "USAF", "USAFR", "USAR", "USCG", "USMC", "USMCR", "USN", "USNR", "PC")
  suffixes_religious <- c("DD", "CSC", "CSJ", "OSB", "PE", "RGS", "SHCJ", "SJ", "SNJM", "SSMO")
  suffixes_business <- c("VP", "SVP", "CEO", "CFO", "COO", "CAO", "CAE", "CBO", "CBDO", "CCO", "CDO", "CEM", "CXO", "CHRO", "CIO", "CISO", "CKO", "CIPO", "CLO", "CMO", "CNO", "CPO", "CQO", "CTO", "CXO")

  ifelse(toupper(x) %in% c(suffixes, suffixes_pro, suffixes_military, suffixes_religious, suffixes_business), TRUE, FALSE)
}

#V. Removes Bad and/or Unwanted Characters from Names
#' Removes Bad and Unwated Characters and Wrods from Names
#' @description Can remove a predetermined set of bad words and/or a custom list of words or
#'  regular expresions
#' @importFrom magrittr "%>%"
#' @export

clean_name <- function(data, args_list, default_list = TRUE)	{
  to_return <- toupper(data)

  if(!missing(args_list)) {
    for(i in args_list) {
      to_return <- to_return %>% stringr::str_replace_all(toupper(i), "")
    }
  }

	if(default_list) {
    .list <- c("[^ -[:alnum:]]", "STORE", "AP", "NA", "ACCOUNTS", "ACCOUNTING", "DEPT",
               "PAYABLES", "ACCTS", "RSMEANS", "MEANS", "PAYABLE", "ACCOUNT", "CUSTOMER", "NOEMAIL")
    for(j in .list) {
      to_return <- to_return %>% stringr::str_replace_all(toupper(j), "")
    }

	}
	to_return <- to_return %>% gsub("^ *|(?<= ) | *$", "", perl=T, .)
  to_return
}

#VI. Count the Number of Words in a Character Variable

count_words <- function(var) {
  stringi::stri_count(var, regex = "\\S+")
}
