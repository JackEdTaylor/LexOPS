get_pronunciations <- function(wordstring  = "example", df) {
  df %>%
    filter(string==wordstring) %>%
    select('cmu.pr1_pronun_1letter', 'cmu.pr2_pronun_1letter', 'cmu.pr3_pronun_1letter', 'cmu.pr4_pronun_1letter') %>%
    select_if(~sum(!is.na(.)) > 0)
}
