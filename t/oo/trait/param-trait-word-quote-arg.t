use Test;

# A parameter trait may carry its argument as a word quote written directly
# after the trait name -- `is option<!>` -- not only as `is encoded('utf8')`.
# App::Prove6 declares its whole `sub MAIN` that way (via `Trait::Option`), so
# the unparsed `<!>` used to fail the signature at its closing paren and cost
# the distribution its only module.

plan 6;

multi sub trait_mod:<is>(Parameter:D $p, :$option!) { }

lives-ok { EVAL 'sub m1(Bool :$timer is option<!>) { }' },
    'is trait<arg> on a named parameter parses';

lives-ok { EVAL 'sub m2($x is option<a b c>) { }' },
    'a multi-word trait argument parses';

lives-ok { EVAL 'sub m3($x is option«a b») { }' },
    'a French-quoted trait argument parses';

# The trait argument is skipped, so the parameter still behaves normally.
sub taker(Int $n is option<!>) { $n * 2 }
is taker(21), 42, 'a parameter carrying a word-quote trait argument still binds';

# The opener has to follow the trait name directly, so a `where` clause whose
# expression merely starts with `<` is untouched.
sub bounded($x is copy where * < 3) { $x }
is bounded(1), 1, 'is copy followed by a where clause with < still parses';

dies-ok { EVAL 'sub m4($x is copy where * < 3) { }; m4(9)' },
    'and that where clause is still enforced';
