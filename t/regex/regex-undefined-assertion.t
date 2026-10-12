use Test;

# `<$var>` holding an undefined value dies: rakudo's INTERPOLATE_ASSERTION has
# no candidate for a `:U` argument (#13042). Expected values verified against
# rakudo.

plan 6;

{
    my $n;
    throws-like { "a" ~~ /<$n> a/ }, X::Multi::NoMatch,
        message => /'INTERPOLATE_ASSERTION(Match:D: Any:U,'/,
        'an outer Any';
}
{
    my Str $s;
    throws-like { "a" ~~ /<$s> a/ }, X::Multi::NoMatch,
        message => /'INTERPOLATE_ASSERTION(Match:D: Str:U,'/,
        'a typed outer variable reports its type';
}
throws-like { "a" ~~ / :my $n; <$n> a / }, X::Multi::NoMatch,
    'an in-regex :my left unassigned';
throws-like { "a" ~~ / :my $n = Nil; <$n> / }, X::Multi::NoMatch,
    'an in-regex :my assigned Nil';

{
    my $n = "a";
    ok "a" ~~ /<$n>/, 'a defined Str still interpolates';
}
ok "a" ~~ / :my $n; { $n = "a" } <$n> /, 'a :my assigned before the assertion';
