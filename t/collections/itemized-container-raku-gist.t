use Test;

# `.raku` shows the `$` itemization sigil for an itemized container
# (`$[...]`, `$(...)`, `${...}`); `.gist` never shows it at any nesting level.
# Itemizing an already-scalar value (`$(1)`) is a no-op.

plan 22;

# --- itemized hash `.raku` carries the `$` sigil ---
is ${a => 1}.raku,          '${:a(1)}',          'itemized hash .raku has $';
is ${a => 1, b => 2}.raku,  '${:a(1), :b(2)}',   'multi-key itemized hash .raku has $';
is (my %x = a => 1).item.raku, '${:a(1)}',       '.item on a hash itemizes for .raku';

# --- itemized array / list `.raku` carries the `$` sigil ---
is $[1, 2, 3].raku,         '$[1, 2, 3]',        'itemized array .raku has $';
is $(1, 2).raku,            '$(1, 2)',           'itemized list .raku has $';
is $(1,).raku,              '$(1,)',             'one-element itemized list .raku';

# --- itemizing a plain scalar is a no-op in `.raku` ---
is $(1).raku,               '1',                 'itemized Int .raku is unwrapped';
is $("x").raku,             '"x"',               'itemized Str .raku is unwrapped';
is $(1..3).raku,            '1..3',              'itemized Range .raku is unwrapped';

# --- `.gist` never shows the `$` sigil ---
is ${a => 1}.gist,          '{a => 1}',          'itemized hash .gist drops $';
is $[1, 2, 3].gist,         '[1 2 3]',           'itemized array .gist drops $';
is $(1, 2).gist,            '(1 2)',             'itemized list .gist drops $';

# --- nested itemized containers: .gist drops $ at every level ---
is [$[1, 2], $[3, 4]].gist, '[[1 2] [3 4]]',     'nested itemized arrays gist without $';
is ($[1, 2], 3).gist,       '([1 2] 3)',         'itemized array nested in list gist';
is [${a => 1}].gist,        '[{a => 1}]',        'itemized hash nested in array gist';
is (${a => 1}, ${b => 2}).gist, '({a => 1} {b => 2})', 'itemized hashes nested in list gist';
is $[$[1, 2]].gist,         '[[1 2]]',           'doubly-itemized array gist';

# --- nested itemized containers: .raku keeps $ at every level ---
is ($[1, 2], 3).raku,       '($[1, 2], 3)',      'itemized array nested in list raku keeps $';
is (${a => 1}, ${b => 2}).raku, '(${:a(1)}, ${:b(2)})', 'itemized hashes nested in list raku keeps $';

# --- non-itemized counterparts are unchanged ---
is {a => 1}.raku,           '{:a(1)}',           'plain hash .raku has no $';
is [1, 2, 3].raku,          '[1, 2, 3]',         'plain array .raku has no $';
is {a => 1}.gist,           '{a => 1}',          'plain hash .gist unchanged';
