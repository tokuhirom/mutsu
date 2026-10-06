use Test;

# A routine the program declares itself wins over the builtin of the same name,
# because the parser sees the declaration. A tree read back from source has to
# make the same choice: `say`, `put`, `print`, `note`, `die`, `fail` and `take`
# are statements of their own once lowered, which a declared `sub` takes back.

plan 9;

sub run($src) { EVAL($src.AST) }

for <say put print note die fail take> -> $name {
    my $program = 'sub NAME($n) { "mine " ~ $n }; NAME(5)'.subst('NAME', $name, :g);
    is run($program), 'mine 5', "a declared sub $name is called, not the builtin";
}

# A lexical `&name` shadows the builtin too.
is run(Q[my &take = -> $n { "mine $n" }; take(6)]), 'mine 6', 'a lexical &take shadows the builtin';
# Without a declaration the builtin is still the statement.
is run(Q[my @a = gather { take 1; take 2 }; @a.join(',')]), '1,2', 'the builtin take still gathers';
