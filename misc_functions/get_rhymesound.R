# Requires phoneme representation to be in two-letter format

get_rhymesound <- function(pronun_vec = c("AH", "D",  "ER", "L",  "IY"), with.stress = F) {
  prev_sound <- ""
  vowel_sound <- ""
  vowel_vec <- if (with.stress) {cmu_vowel_phons_stress} else {cmu_vowel_phons}
  for (sound in rev(pronun_vec)) {
    if (sound %in% vowel_vec) {
      vowel_sound <- sound
      break
    } else {
      if (prev_sound != ""){
        prev_sound <- sprintf("%s_%s", sound, prev_sound)
      } else {
        prev_sound <- sprintf("%s", sound)
      }
    }
  }
  if (vowel_sound != ""){
    sep <- if(prev_sound=="") {""} else {"_"}
    sprintf("%s%s%s", vowel_sound, sep, prev_sound)
  } else {
    NA
  }
}