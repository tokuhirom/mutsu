use Test;

# Regex assertions on value-typed Unicode properties that the Rust regex crate
# does not support (Line_Break, Word_Break, Bidi_Class, Joining_Type,
# East_Asian_Width, ...). mutsu previously only matched General_Category and
# Script, returning False for the rest.

plan 20;

# Line_Break (both <:Prop("Val")> and <:Prop<Val>> forms)
ok "漢" ~~ /<:Line_Break("ID")>/, 'Line_Break ID matches ideograph (quoted)';
ok "漢" ~~ /<:Line_Break<ID>>/, 'Line_Break ID matches ideograph (angle)';
ok "A" ~~ /<:Line_Break("AL")>/, 'Line_Break AL matches Latin';
nok "漢" ~~ /<:Line_Break("AL")>/, 'Line_Break AL does not match ideograph';

# Word_Break
ok "あ" ~~ /<:Word_Break("Other")>/, 'Word_Break Other matches Hiragana';
ok "A" ~~ /<:Word_Break("ALetter")>/, 'Word_Break ALetter matches Latin';

# Bidi_Class
ok "ب" ~~ /<:Bidi_Class("AL")>/, 'Bidi_Class AL matches Arabic';
ok "5" ~~ /<:Bidi_Class("EN")>/, 'Bidi_Class EN matches digit';
ok "א" ~~ /<:Bidi_Class("R")>/, 'Bidi_Class R matches Hebrew';

# Joining_Type
ok "ب" ~~ /<:Joining_Type("D")>/, 'Joining_Type D matches Arabic beh';
ok "ا" ~~ /<:Joining_Type("R")>/, 'Joining_Type R matches Arabic alef';

# East_Asian_Width (short property-value aliases)
ok "漢" ~~ /<:East_Asian_Width("W")>/, 'East_Asian_Width W matches ideograph';
ok "A" ~~ /<:East_Asian_Width("Na")>/, 'East_Asian_Width Na matches Latin';

# Grapheme_Cluster_Break / Sentence_Break / Numeric_Type
ok "\xAC00" ~~ /<:Grapheme_Cluster_Break("LV")>/, 'GCB LV matches Hangul syllable';
ok "あ" ~~ /<:Sentence_Break("OLetter")>/, 'Sentence_Break OLetter matches Hiragana';
ok "5" ~~ /<:Numeric_Type("Decimal")>/, 'Numeric_Type Decimal matches digit';

# Still works: General_Category and Script and bare binary properties
ok "A" ~~ /<:Script("Latin")>/, 'Script Latin still matches';
ok "A" ~~ /<:Lu>/, 'General_Category Lu still matches';
ok "A" ~~ /<:Letter>/, 'binary Letter still matches';

# Negation and quantification
ok "café" ~~ /^<:Line_Break("AL")>+$/, 'quantified Line_Break AL matches all-Latin word';
