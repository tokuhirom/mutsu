use Test;

# From Path::Finder: a token's code block holds `make /<-[/]>*?/`. The word
# `make` takes a term, so its `/` opens a regex literal; read as a division,
# the literal's closing `/` opened another one and the token lost its source
# tree (the RakuAST round-trip then refused the whole module).
plan 4;

grammar Globbing {
    token TOP { <star> }
    token star { '*' { make /<-[/]>*?/ } }
}

my $m = Globbing.parse('*');
ok $m, 'the grammar with a regex literal after `make` parses';
isa-ok $m<star>.made, Regex, 'make stored the regex literal';
my $r = $m<star>.made;
is ('ab' ~~ $r).Str, '', 'the stored lazy regex matches the empty prefix';
is ('a/b' ~~ /^ <-[/]>+ $/).defined, False, 'a class with a slash still refuses it';

# vim: expandtab shiftwidth=4
