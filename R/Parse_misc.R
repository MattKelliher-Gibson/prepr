#I. Parse Prefix

ParsePrefix <- function(.data) {
  .data[WordCount <= 1 | WordCount > 5 | is.na(WordCount), Final_Prefix := ""]

  .data[WordCount > 1, Final_Prefix := ifelse(CheckPrefix(word1), word1, "")]
}

#II. Parse First Name

ParseFirst <- function(.data) {
  .data[WordCount <= 1 | WordCount > 5 | is.na(WordCount), Final_First := ""]

  .data[WordCount == 2, Final_First := ifelse(Final_Prefix == "", word1, "")]

  .data[WordCount == 3 & is.na(Final_First), Final_First := ifelse(Final_Prefix == "", word1, word2)]

  .data[WordCount == 4 & is.na(Final_First),
        Final_First := ifelse(!(Final_Prefix == ""), word2,
                              ifelse(CheckMI(word3), paste(word1, word2), word1))]

  .data[WordCount == 5 & is.na(Final_First),
        Final_First := ifelse(!(Final_Prefix == ""),
                              ifelse(CheckMI(word4) & Final_SUffix == "", paste(word2, word3), word2),
                              ifelse(CheckMI(word3), paste(word1, word2),
                                     ifelse(!(Final_Suffix) == "", word1,
                                            ifelse(CheckMI(word4), paste(word1, word2, word3),
                                                   ifelse(CheckMI(word2) | CheckLast(word2) | CheckLast(word3), word1, paste(word1, word2))))))]
}

#III. Parse Middle Name

PareseMiddle <- function(.data) {
  .data[WordCount <= 2 | WordCount > 5 | is.na(WordCount), Final_Middle := ""]

  .data[WordCount == 3, Final_Middle := ifelse(Final_First == word1 & Final_Last == word3, word2, "")]

  .data[WordCount == 4 & is.na(Final_Middle),
        Final_Middle := ifelse(Final_Prefix == word1 & Final_Last == word4, word3,
                               ifelse(Final_First == word1,
                                      ifelse(Final_Last == word3, word2,
                                             ifelse(CountWords(Final_Last) == 2, word2, "")), word3))]

  .data[WordCount == 5 & is.na(Final_Middle),
        Final_Middle := ifelse(!(Final_Prefix == ""),
                               ifelse(CountWords(Final_First) == 2, word4,
                                      ifelse(CountWords(Final_Last) == 1, word3,
                                             ifelse(Final_Suffix == "" & CountWords(Final_Last) == 2, word3))),
                               ifelse(!(Final_Suffix == ""),
                                      ifelse(CountWords(Final_Last) == 2, word2,
                                             ifelse(CountWords(Final_Last) == 1, word3, "")),
                                      ifelse(CountWords(Final_Last) == 2, word3,
                                             ifelse(CountWords(Final_Last) == 3, word2,
                                                    ifelse(CountWords(Final_Last) == 1, word4, "")))))]
}

#IV. Parse Last Name

ParseLast <- function(.data) {
  .data[WordCount < 1 | is.na(WordCount), Final_Last := ""]

  .data[WordCount == 1, Final_Last := word1]

  .data[WordCount > 5, Final_Last := Final_Name]

  .data[WordCount == 2, Final_Last := word2]

  .data[WordCount == 3 & is.na(Final_Last),
        Final_Last := ifelse(!(Final_Prefix == ""), word3,
                             ifelse(!(Final_Suffix == ""), word2,
                                    ifelse(CheckMI(word2), word3,
                                           ifelse(CheckLast(word2), paste(word2, word3), word3))))]

  .data[WordCount == 4 & is.na(Final_Last),
        Final_Last := ifelse(!(Final_Prefix == ""),
                             ifelse( !(Final_Suffix == ""), word3,
                                     ifelse(CheckLast(word3), paste(word3, word4), word4)),
                             ifelse(!(Final_Suffix == ""),
                                    ifelse(CountWords(Final_Suffix) == 2, word2, word3),
                                    ifelse(CountWords(Final_First) == 2, word4,
                                           ifelse(CheckMI(word2), paste(word3, word4),
                                                  ifelse(CheckLast(word2), paste(word2, word3, word4), paste(word3, word4))))))]

  .data[WordCount == 5 & is.na(Final_Last),
        Final_Last := ifelse(!(Final_Prefix == ""),
                             ifelse(!(Final_Suffix == ""),
                                    ifelse(CheckLast(word3), paste(word3, word4), word4),
                                    ifelse(CheckMI(word3), paste(word4, word5),
                                           ifelse(CheckMI(word4), word5,
                                                  ifelse(CheckLast(word3), paste(word3, word4, word5), paste(word4, word5))))),
                             ifelse(!(Final_Suffix == ""),
                                    ifelse(CountWords(Final_First) == 2, word3,
                                           ifelse(CheckLast(word2), paste(word2, word3, word4), paste(word3, word4))),
                                    ifelse(CountWords(Final_First) == 1,
                                           ifelse(CheckLast(word2), paste(word2, word3, word4), paste(word3, word4, word5)),
                                           ifelse(CountWords(Final_First) == 2, paste(word4, word5), word5))))]
}

#V. Parse Suffix

ParseSuffix <- function(.data) {
  .data[WordCount < 3 | WordCount > 5 | is.na(WordCount), Final_Suffix := ""]

  .data[WordCount == 3 & is.na(Final_Suffix), Final_Suffix := ifelse(CheckSuffix(word3), word3, "")]

  .data[WordCount == 4 & is.na(Final_Suffix),
            Final_Suffix := ifelse(CheckSuffix(word4),
                                   ifelse(Final_Prefix == "" & CheckSuffix(word3), paste(word3, word4), word4), "")]

  .data[WordCount == 5 & is.na(Final_Suffix), Final_Suffix := ifelse(CheckSuffix(word5), word5, "")]
}
