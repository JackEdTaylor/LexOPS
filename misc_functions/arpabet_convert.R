arpabet_convert <- function (string = "@dhisIvz", to = "two") {
  if (class(string) != "character") {
    stop("Expected a class of character for string.")
  }
  arpabet <- tibble(
    twoletter = c("AA", "AE", "AH", "AO", "AW", "AY", "B", "CH", "D", "DH", "EH", "ER", "EY", "F", "G", "HH", "IH", "IY", "JH", "K", "L", "M", "N", "NG", "OW", "OY", "P", "R", "S", "SH", "T", "TH", "UH", "UW", "V", "W", "Y", "Z", "ZH"),
    oneletter = c("a", "@", "A", "c", "W", "Y", "b", "C", "d", "D", "E", "R", "e", "f", "g", "h", "I", "i", "J", "k", "l", "m", "n", "G", "o", "O", "p", "r", "s", "S", "t", "T", "U", "u", "v", "w", "y", "z", "Z")
  )
  regex.oneletter <- sprintf("[%s]", paste(arpabet$oneletter, collapse=""))
  regex.twoletter <- sprintf("[%s|_]", paste(arpabet$twoletter, collapse="|"))
  ph_nr <- 0
  if(to=="one") {
    if (!grepl("_", string) & !(string %in% arpabet$twoletter)) {
      stop("Check string. Already in one-letter phoneme representation?")
    }
    if (nchar(gsub(regex.twoletter, "", string))!=0) {
      stop(sprintf('Unexpected phoneme representation: "%s".
                   Acceptable representations for two-to-one letter representations are:
                   %s', gsub(regex.twoletter, "", string), paste(arpabet$twoletter, collapse=" ")))
    }
    strvec <- strsplit(string, "_")[[1]]
    strvec <- strvec[strvec!=""]  # remove any empty entries
    if (lengths(regmatches(string, gregexpr("_", string))) != length(strvec)-1) {
      stop(sprintf('Incorrect number of "_" seperators. Expected %i but found %i?',
                   length(strvec)-1, lengths(regmatches(string, gregexpr("_", string)))))
    }
    for (ph in strvec) {
      ph_nr <- ph_nr + 1
      nextph <- arpabet$oneletter[arpabet$twoletter==ph]
      if (to=="two") {
        nextph <- arpabet$twoletter[arpabet$oneletter==ph]
      }
      if (ph_nr == 1) {
        out <- nextph
      } else {
        out <- sprintf("%s%s", out, nextph)
      }
    }
  } else if (to=="two") {
    if (grepl("_", string)) {
      stop("Check string. Already in two-letter phoneme representation?")
    }
    if (nchar(gsub(regex.oneletter, "", string))!=0) {
      stop(sprintf('Unexpected character: "%s".
                   Acceptable characters for one-to-two letter representations are:
                   %s', gsub(regex.oneletter, "", string), paste(arpabet$oneletter, collapse=" ")))
    }
    strvec <- str_split(string, boundary("character"))[[1]]
    for (ph in strvec) {
      ph_nr <- ph_nr + 1
      nextph <- arpabet$twoletter[arpabet$oneletter==ph]
      if (ph_nr == 1) {
        out <- nextph
      } else {
        out <- sprintf("%s%s", out, nextph)
      }
      if (ph_nr != length(strvec)) {out <- sprintf("%s_", out)}
    }
  }
  out
}
