# Regex literals and classes naming an NFC-expanding codepoint

A lone codepoint whose NFC spelling is several codepoints (U+2ADC, a composition
exclusion) is held by the subject string as one grapheme. A regex literal
(`/\x[2adc]/`) or character class (`<[\x[2adc]]>`) naming it now matches that
grapheme, as Rakudo does. Found with the P5quotemeta ecosystem distribution, whose
5756-assertion suite now passes in full.
