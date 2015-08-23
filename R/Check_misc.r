#I. Check Last Name ####

CheckLast <- function(x){
  last <- c("BEN", "DA", "DAL", "DE", "DEL", "DEN", "DER", "DI", "DU", "E", "LA", "LE", "MC", "SAN", "ST", "STE", "VAN", "VANDER", "VEL", "VON", "MC", "LOS", "MAC", "LAS", "LES")

  ifelse(x %in% last, TRUE, FALSE)
}

#II. Check if Middle Name has Length of 1 ####

CheckMI <- function(x) {
  ifelse(nchar(x) == 1, TRUE, FALSE)
}

#III. Check if Word is a Prefix ####

CheckPrefix <- function(x) {
  prefixes <- c("MR", "MRS", "MS", "MISS", "DR", "REV", "FR", "PR", "ATTY", "PROF", "HON", "PRES", "GOV", "OFC", "SIR", "MISTER", "MISTRESS", "REVEREND", "PASTOR", "FATHER", "DOCTOR", "ATTORNEY", "PROFESSOR", "HONORABLE", "PRESIDENT", "GOVERNOR", "OFFICER")
  prefixes_expanded <- c("MSGR", "MONSIGNOR", "SR", "SISTER", "BR", "BROTHER", "SUPT", "SUPERINTENDENT", "REP", "REPRESENTATIVE", "SEN", "sENATOR", "AMB", "AMBASSADOR", "TREAS", "TREASURER", "SEC", "SECRETARY")
  prefixes_military <- c("PVT", "PRIVATE", "CPL", "CORPORAL", "SGT", "SARGENT", "ADM", "ADMINISTRATIVE", "MAJ", "MAJOR", "CAPT", "CAPTAIN", "CMDR", "COMMANDER", "LT", "LIEUTENANT", "COL", "COLONEL", "GEN", "GENERAL")

  ifelse(x %in% c(prefixes, prefixes_military, prefixes_expanded), TRUE, FALSE)
}

#IV. Check if Word is a Suffix ####

CheckSuffix <- function(x) {
  suffixes <- c("SR", "JR", "SENIOR", "JUNIOR", "I", "II", "III", "IV", "1ST", "2ND", "3RD", "4TH")
  suffixes_pro <- c("MD", "DC", "DDS", "DMD", "DO", "DVM", "EDD", "ESQ", "JD", "LLD", "OD", "PHD", "RET", "RN", "RNC", "CPA", "CLU", "CFRE")
  suffixes_military <- c("USA", "USAF", "USAFR", "USAR", "USCG", "USMC", "USMCR", "USN", "USNR", "PC")
  suffixes_religious <- c("DD", "CSC", "CSJ", "OSB", "PE", "RGS", "SHCJ", "SJ", "SNJM", "SSMO")
  suffixes_business <- c("VP", "SVP", "CEO", "CFO", "COO", "CAO", "CAE", "CBO", "CBDO", "CCO", "CDO", "CEM", "CXO", "CHRO", "CIO", "CISO", "CKO", "CIPO", "CLO", "CMO", "CNO", "CPO", "CQO", "CTO", "CXO")

  ifelse(x %in% c(suffixes, suffixes_pro, suffixes_military, suffixes_religious), TRUE, FALSE)
}

#V. Removes Bad and/or Unwanted Characters from Names
#' @importFrom magrittr "%>%"

CleanName <- function(data)	{
  data %>%
	stringr::str_replace_all("[^-[:alnum:]]", " ") %>%
	stringr::str_replace_all("STORE", " ") %>%
	stringr::str_replace_all("A/P", " ") %>%
	stringr::str_replace_all("N/A", " ") %>%
	stringr::str_replace_all("ACCOUNTS", " ") %>%
	stringr::str_replace_all("ACCOUNTING", " ") %>%
	stringr::str_replace_all("DEPT", " ") %>%
	stringr::str_replace_all("PAYABLES", " ") %>%
	stringr::str_replace_all("ACCTS", " ") %>%
	stringr::str_replace_all("RSMEANS", " ") %>%
	stringr::str_replace_all("MEANS", " ") %>%
	stringr::str_replace_all("PAYABLE", " ") %>%
	stringr::str_replace_all("ACCOUNT", " ") %>%
	stringr::str_replace_all("CUSTOMER", " ") %>%
	stringr::str_replace_all("NOEMAIL", " ") %>%
	gsub("^ *|(?<= ) | *$", "", perl=T, .) %>%
	gsub("\\d", "", perl = T, .)
}

#VI. Count the Number of Words in a Character Variable

CountWords <- function(var) {
  stringi::stri_count(var, regex = "\\S+")
}
