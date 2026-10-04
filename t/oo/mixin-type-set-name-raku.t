use Test;

# A mixin type object renamed with `.^set_name` renders by that name in
# `.raku` and `.gist`, as `.^name` reports it. Upstream NativeCall names
# `Pointer[void]` this way in its `^parameterize`.

plan 4;

role R { }
my $t := Int.^mixin(R);
$t.^set_name('Fancy');
is $t.^name, 'Fancy', '.^name is the new name';
is $t.raku, 'Fancy', '.raku renders it';
is $t.gist, '(Fancy)', '.gist renders it';

my $u := Str.^mixin(R);
is $u.raku, 'Str+{R}', 'an unrenamed mixin type keeps its composed name';
