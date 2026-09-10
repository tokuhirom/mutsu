use Test;
plan 4;

is Unicode.^name, 'Unicode',
    'Unicode resolves to its built-in type object';
is Unicode.^mro.map(*.^name).join(','), 'Unicode,Any,Mu',
    'Unicode has the core type-object MRO';
is Unicode.version.raku, 'v17.0',
    'Unicode.version reports the version of mutsu normalization data';
ok Unicode.NFG,
    'Unicode.NFG reports grapheme normalization support';
