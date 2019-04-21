get_pronunciations <- function(wordstring  = "example", df) {
  df %>%
    filter(string==wordstring) %>%
    select("CMU.pr1_1letter", "CMU.pr2_1letter", "CMU.pr3_1letter", "CMU.pr4_1letter") %>%
    select_if(~sum(!is.na(.)) > 0)
}
