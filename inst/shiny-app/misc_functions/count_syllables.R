# Requires phoneme representation to be in two-letter format
# Assumes each vowel phoneme co=occurs with a syllable

count_syllables <- function(pronun_vec = c("AH", "D",  "ER", "L",  "IY")) {
  cmu_vowels <- c("AA", "AE", "AH", "AO", "AW", "AX", "AXR", "AY", "EH", "ER", "EY", "IH", "IX", "IY", "OW", "OY", "UH", "UW", "UX")
  vowel_vec <- pronun_vec[pronun_vec %in% cmu_vowels]
  as.integer(length(vowel_vec))
}