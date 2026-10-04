use Test;

# A sub's custom `is` traits in RakuAST, measured on rakudo 2026.09: each is a
# `Trait::Is(name, argument => Circumfix::Parentheses(…))`, in source order
# beside the `returns` trait. EVAL of the tree applies them as the parsed
# program does.

plan 10;

my $defs = Q:to/END/;
    multi trait_mod:<is>(Routine $r, :$noted!) { $r.wrap(-> |c { "noted(" ~ callsame() ~ ")" }) }
    multi trait_mod:<is>(Routine $r, :$tagged!) { $r.wrap(-> |c { $tagged.join('+') ~ "<" ~ callsame() ~ ">" }) }
    END

# rakudo checks a trait exists while building the tree, so the source
# declares the two traits first.
my @t = ($defs ~ Q[sub b() returns Str is tagged(1, 2) is noted { "b" }]).AST.statements[2].expression.traits;
is @t.elems, 3, 'the returns trait and both custom traits';
isa-ok @t[0], RakuAST::Trait::Returns, 'in source order: `returns` first';
isa-ok @t[1], RakuAST::Trait::Is, 'then `is tagged(…)`';
is @t[1].name.canonicalize, 'tagged', 'named as written';
isa-ok @t[1].argument, RakuAST::Circumfix::Parentheses, 'its argument is the parenthesised list';
nok @t[2].argument.defined, 'a bare trait has no argument';

is EVAL(($defs ~ Q[sub a is noted { 1 }; a()]).AST), 'noted(1)', 'a bare custom trait applies';
is EVAL(($defs ~ Q[sub b() returns Str is tagged(1, 2) { "b" }; b()]).AST), '1+2<b>',
    'a list argument reaches the trait as the list';
is EVAL(($defs ~ Q[sub c is tagged("x") is noted { "c" }; c()]).AST), 'noted(x<c>)',
    'several custom traits apply in source order';

use NativeCall;
is EVAL(Q[use NativeCall; sub strlen(Str --> size_t) is native(Str) { * }; strlen("hello")].AST),
    5, '`is native` survives the round trip';
