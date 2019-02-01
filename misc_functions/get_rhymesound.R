# Requires phoneme representation to be in two-letter format

get_rhymesound <- function(pronun_vec = c("AH", "D",  "ER", "L",  "IY"), with.stress = F) {
  vowel_vec <- if (with.stress) {cmu_vowel_phons_stress} else {cmu_vowel_phons}
  firstsound <- T
  rhyme_sound <- ""
  vowel_detected <- F
  for (sound in rev(pronun_vec)) {
    if (firstsound) {
      rhyme_sound <- sound
      firstsound <- F
    } else {
      if (!(sound %in% vowel_vec)) {
        if (vowel_detected) {
          break
        }
        rhyme_sound <- sprintf("%s_%s", sound, rhyme_sound)
      }
      if (sound %in% vowel_vec) {
        rhyme_sound <- sprintf("%s_%s", sound, rhyme_sound)
      }
    }
    if (sound %in% vowel_vec) {vowel_detected <- T}
  }
  rhyme_sound
}
