# Visualilsation vector categories
vis.cats <- c('Word Frequency', 'Part of Speech', 'Length', 'Bigram Probability', 'Orthographic Neighbourhood', 'Syllables', 'Phonemes', 'Rhyme', 'Phonological Neighbourhood', 'Familiarity', 'Age of Acquisition', 'Concreteness', 'Arousal', 'Valence', 'Dominance', 'Imageability', 'Semantic Size', 'Semantic Gender', 'Humour', 'Lexical Decision Response Time', 'Lexical Decision Accuracy')

visualise.opts <- names(lexops)[!(names(lexops) %in% c('string'))]

vis.opt.2.source <- function(x){switch(x,
                                       '(None)' = '',
                                       'Target Match Word' = '',
                                       'Suggested Matches' = '',
                                       'Words Uploaded to Fetch Tab' = '',
                                       'Part of Speech' = visualise.opts[grepl("PoS",visualise.opts)],
                                       'Length' = 'Length',
                                       'Syllables' = visualise.opts[grepl("Syllables",visualise.opts)],
                                       'Word Frequency' = visualise.opts[grepl("Zipf",visualise.opts) | grepl("fpmw",visualise.opts)],
                                       'Bigram Probability' = visualise.opts[grepl("BG",visualise.opts)],
                                       'Orthographic Neighbourhood' = visualise.opts[grepl("ON",visualise.opts)],
                                       'Phonemes' = visualise.opts[grepl("Phonemes",visualise.opts)],
                                       'Rhyme' = visualise.opts[grepl("Rhyme",visualise.opts)],
                                       'Phonological Neighbourhood' = visualise.opts[grepl("PLD20",visualise.opts) | grepl("PhonColtheartN",visualise.opts)],
                                       'Familiarity' = visualise.opts[grepl("FAM",visualise.opts)],
                                       'Age of Acquisition' = visualise.opts[grepl("AoA",visualise.opts)],
                                       'Concreteness' = visualise.opts[grepl("CNC",visualise.opts)],
                                       'Arousal' = visualise.opts[grepl("AROU",visualise.opts)],
                                       'Valence' = visualise.opts[grepl("VAL",visualise.opts)],
                                       'Dominance' = visualise.opts[grepl("DOM",visualise.opts)],
                                       'Imageability' = visualise.opts[grepl("IMAG",visualise.opts)],
                                       'Semantic Size' = visualise.opts[grepl("SIZE",visualise.opts)],
                                       'Semantic Gender' = visualise.opts[grepl("GEND",visualise.opts)],
                                       'Humour' = visualise.opts[grepl("HUM",visualise.opts)],
                                       'Lexical Decision Response Time' = visualise.opts[grepl("RT",visualise.opts)],
                                       'Lexical Decision Accuracy' = visualise.opts[grepl("Accuracy",visualise.opts)]
)}
