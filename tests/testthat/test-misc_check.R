context("Misc check functions")

test_that("Name Checks return TRUE", {
  .last <- c("BEN", "DA", "DAL", "DE", "DEL", "DEN", "DER", "DI", "DU", "LA", "LE", "MC", "SAN", "ST", "STE", "VAN", "VANDER", "VEL", "VON", "MC", "LOS", "MAC", "LAS", "LES")
  expect_equal(CheckLast(.last), c(rep(TRUE, 24)))
  expect_false(CheckLast("Matthew"))
  expect_true(CheckMI("M"))
  expect_false(CheckMI("MA"))
  .prefixes <- c("MR", "MRS", "MS", "MISS", "DR", "REV", "FR", "PR", "ATTY", "PROF", "HON", "PRES", "GOV", "OFC", "SIR", "MISTER", "MISTRESS", "REVEREND", "PASTOR", "FATHER", "DOCTOR", "ATTORNEY", "PROFESSOR", "HONORABLE", "PRESIDENT", "GOVERNOR", "OFFICER")
  expect_equal(CheckPrefix(.prefixes), c(rep(TRUE, 27)))
  .prefixes_expanded <- c("MSGR", "MONSIGNOR", "SR", "SISTER", "BR", "BROTHER", "SUPT", "SUPERINTENDENT", "REP", "REPRESENTATIVE", "SEN", "SENATOR", "AMB", "AMBASSADOR", "TREAS", "TREASURER", "SEC", "SECRETARY")
  expect_equal(CheckPrefix(.prefixes_expanded), c(rep(TRUE, 18)))
  .prefixes_military <- c("PVT", "PRIVATE", "CPL", "CORPORAL", "SGT", "SARGENT", "ADM", "ADMINISTRATIVE", "MAJ", "MAJOR", "CAPT", "CAPTAIN", "CMDR", "COMMANDER", "LT", "LIEUTENANT", "COL", "COLONEL", "GEN", "GENERAL")
  expect_equal(CheckPrefix(.prefixes_military), c(rep(TRUE, 20)))
  expect_false(CheckPrefix("III"))
  .suffixes <- c("SR", "JR", "SENIOR", "JUNIOR", "I", "II", "III", "IV", "1ST", "2ND", "3RD", "4TH")
  expect_equal(CheckSuffix(.suffixes), c(rep(TRUE, 12)))
  .suffixes_pro <- c("MD", "DC", "DDS", "DMD", "DO", "DVM", "EDD", "ESQ", "JD", "LLD", "OD", "PHD", "RET", "RN", "RNC", "CPA", "CLU", "CFRE")
  expect_equal(CheckSuffix(.suffixes_pro), c(rep(TRUE, 18)))
  .suffixes_military <- c("USA", "USAF", "USAFR", "USAR", "USCG", "USMC", "USMCR", "USN", "USNR", "PC")
  expect_equal(CheckSuffix(.suffixes_military), c(rep(TRUE, 10)))
  .suffixes_religious <- c("DD", "CSC", "CSJ", "OSB", "PE", "RGS", "SHCJ", "SJ", "SNJM", "SSMO")
  expect_equal(CheckSuffix(.suffixes_religious), c(rep(TRUE, 10)))
  .suffixes_business <- c("VP", "SVP", "CEO", "CFO", "COO", "CAO", "CAE", "CBO", "CBDO", "CCO", "CDO", "CEM", "CXO", "CHRO", "CIO", "CISO", "CKO", "CIPO", "CLO", "CMO", "CNO", "CPO", "CQO", "CTO", "CXO")
  expect_equal(CheckSuffix(.suffixes_business), c(rep(TRUE, 25)))
})

test_that("clean_name removes defaults", {
  .default <- "STORE A/P N/A ACCOUNTS DEPT PAYABLES ACCTS RSMEANS MEANS PAYABLE ACCOUNT CUSTOMER NOEMAIL !@#$%^&*()"
  test_text <- "This is a test"
  test_name <- "Matthew Kelliher-Gibson"
  expect_equal(clean_name(.default), "")
  expect_equal(clean_name(test_text), toupper(test_text))
  expect_equal(clean_name(test_name), toupper(test_name))
})

test_that("clean_name removes strings", {
  test_name <- "Mark Fake Name Roberts"
  test_text <- "Fake Name"
  expect_equal(clean_name(test_name, test_text, FALSE), "MARK ROBERTS")
  expect_equal(clean_name("Alternate Contact John Smith", "Alternate Contact", FALSE), "JOHN SMITH")
})

test_that("clean_name removes regular expressions", {
  test_name <- "<[Red Forman]>"
  test_text <- "[^ [:alpha:]]"
  expect_equal(clean_name(test_name, test_text, FALSE), "RED FORMAN")
  expect_equal(clean_name(c("1. George Washington", "2. John Adams"), "^[0-9].", FALSE), c("GEORGE WASHINGTON", "JOHN ADAMS"))
})
