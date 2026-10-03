use Test;

# A Match destructures through its `.Capture`: the positional captures are its
# positional part and the named captures its named part. mutsu bound a
# sub-signature's named parameters from the Match object's attributes, so
# `-> ( :$specifier ) { ... }` as an `.subst` replacement saw nothing.
# Reduced from Lumberjack's `format-message`.

plan 5;

my $m = "%P" ~~ / '%' $<s>=[P] /;
-> (:$s) { is ~$s, 'P', 'named capture binds a named sub-param' }($m);
sub named-only((*%h)) { %h.keys.List }
is-deeply named-only($m), ('s',), 'named slurpy gets only the named captures';

my $p = "ab" ~~ / (a) (b) /;
-> ($x, $y) { is "$x$y", 'ab', 'positional captures bind positional sub-params' }($p);

my regex fe { '%' $<specifier>=[P|D] }
is "[%P] %D".subst(&fe, -> ( :$specifier ) { "<$specifier>" }, :g), '[<P>] <D>',
    'subst replacement block destructuring the Match';
is "x%Dy".subst(/ '%' $<k>=[\w] /, -> (:$k) { $k.lc }), 'xdy', 'non-global subst';
