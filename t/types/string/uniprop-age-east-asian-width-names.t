use Test;

# From the App::Uni distribution's `uni -v`: Age comes from the Unicode age
# table, East_Asian_Width is the short alias, and property names are matched
# without regard to case or underscores. Expected values are rakudo's.

plan 13;

is "🚮".uniprop('Age'), '6.0', 'Age of U+1F6AE';
is "A".uniprop('Age'), '1.1', 'Age of A';
is "€".uniprop('Age'), '2.1', 'Age of the euro sign';
is 0x1F600.chr.uniprop('Age'), '6.1', 'Age of U+1F600';
is 0x378.chr.uniprop('Age'), 'Unassigned', 'an unassigned codepoint has no age';

is "🚮".uniprop('East_Asian_Width'), 'W', 'wide';
is "a".uniprop('East_Asian_Width'), 'Na', 'narrow';
is "a".uniprop('ea'), 'Na', 'short property name';

is "🚮".uniprop('white_space'), False, 'lowercase property name';
is " ".uniprop('whitespace'), True, 'underscores are ignored';
is-deeply "🚮".uniprops('white_space'), (False,), 'uniprops with a folded name';
is "🚮".uniprop('Script'), 'Common', 'exact names are unaffected';
is "A".uniprop('gc'), 'Lu', 'short names are unaffected';
