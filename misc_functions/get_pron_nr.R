get_pron_nr <- function(inputsource, str, df, sep='-') {
  pron_summ <- get_pronunciations(str, df) %>%
    unname() %>%
    lapply(arpabet_convert, to="two", sep=sep) %>%
    unlist(use.names = F)
  match(inputsource, pron_summ)
}
