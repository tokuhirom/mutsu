use Test;

# Found via App::DuckETL (TUI::Render): its display-width helper asks
# `$c.uniprops('W')`. rakudo reads a bare property VALUE name as the property
# the value belongs to, so 'W' answers the East_Asian_Width; mutsu answered the
# General_Category ("Lo") and every CJK character counted as one column.

plan 14;

is '中'.uniprop('W'),  'W',  "'W' is East_Asian_Width: wide";
is 'a'.uniprop('Na'),  'Na', "'Na': narrow";
is 'ａ'.uniprop('F'),  'F',  "'F': full-width";
is 'ｱ'.uniprop('H'),   'H',  "'H': half-width";
is-deeply '中'.uniprops('W').list, ('W',), 'uniprops with the value name';
is-deeply '中a'.uniprops('W').list, ('W', 'Na'), 'one answer per character';

is 'a'.uniprop('Latin'),  'Latin',  "a Script value name answers the Script";
is '中'.uniprop('Han'),   'Han',    'Han';
is ' '.uniprop('Common'), 'Common', 'Common';
is 'a'.uniprop('Greek'),  'Latin',  'the answer is the character\'s script, not the name asked for';

is 'a'.uniprop('Ll'),  'Ll', 'a General_Category value answers the category';
is 'A'.uniprop('Lu'),  'Lu', 'uppercase letter category';
is ' '.uniprop('Zs'),  'Zs', 'Zs';
is '中'.uniprop('East_Asian_Width'), 'W', 'the long property name still works';
