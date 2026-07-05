use Test;

# The character-class combining forms <+:Prop("Value")> and <-:Prop("Value")>
# embed the property value in the class-item name. mutsu only handled the
# angle-bracket <Value> form there, so value-typed properties (Line_Break,
# Bidi_Class, East_Asian_Width, ...) failed inside <+...>/<-...>.

plan 14;

# <+:Prop("Value")> inclusion
ok "漢" ~~ /<+:Line_Break("ID")>/, 'plus Line_Break ID matches ideograph';
nok "A" ~~ /<+:Line_Break("ID")>/, 'plus Line_Break ID does not match Latin';
ok "ب" ~~ /<+:Bidi_Class("AL")>/, 'plus Bidi_Class AL matches Arabic';
ok "5" ~~ /<+:Bidi_Class("EN")>/, 'plus Bidi_Class EN matches digit';
ok "漢" ~~ /<+:East_Asian_Width("W")>/, 'plus East_Asian_Width W matches ideograph';

# quantified
ok "ب漢" ~~ /^<+:Bidi_Class("AL")>+/, 'quantified plus form matches leading Arabic';

# <-:Prop("Value")> exclusion
ok "A" ~~ /<-:Line_Break("ID")>/, 'minus Line_Break ID matches non-ideograph';
nok "漢" ~~ /<-:Line_Break("ID")>/, 'minus Line_Break ID excludes ideograph';

# combined with other class items
ok "5" ~~ /<[a..z]+:Bidi_Class("EN")>/, 'combined range + property matches digit';
ok "x" ~~ /<[a..z]+:Bidi_Class("EN")>/, 'combined range + property matches letter';

# existing forms still work
ok "漢" ~~ /<:Line_Break("ID")>/, 'assertion form still works';
ok "A" ~~ /<+alpha>/, 'built-in class still works';
ok "A" ~~ /<+:Lu>/, 'plus General_Category still works';
ok "A" ~~ /<+:Script("Latin")>/, 'plus Script still works';
