use Test;
plan 7;

# Perl 5 special variables are unsupported in Raku and must throw
# X::Syntax::Perl5Var (not a generic parse error).
for '$& = 1', '$` = 1', '$| = 1', '$? = 1' -> $code {
    throws-like $code, X::Syntax::Perl5Var, "$code throws X::Syntax::Perl5Var";
}

# Valid Raku constructs that look superficially similar must keep working:
# - $&foo is a code-object deref
# - $?FILE is a compile-time variable (?-twigil)
# - an anonymous $ scalar parameter followed by punctuation
sub foo { 42 }
my $c = $&foo;
is $c(), 42, '$&foo is a code-object deref, not the $& match variable';

ok $?FILE.defined, '$?FILE compile-time variable still works';

sub takes-anon($, $x) { $x }
is takes-anon(1, 2), 2, 'anonymous $ scalar parameter still works';
