use Test;
use experimental :rakuast;

plan 13;

# The `=` of `s[...] = EXPR` is an item assignment exactly when the
# enclosing expression's `$*LEFTSIGIL` is `$` there, as rakudo decides for
# `infix:<=>`: a bare `$` variable before it in the same expression, with no
# postfix on it and no `=` in between (#18446).

sub infix-of(Str $code) {
    my $found;
    sub walk($x) {
        return unless $x ~~ RakuAST::Node;
        if $x ~~ RakuAST::Substitution { $found = $x.infix.raku; return }
        $x.visit-children(&walk);
    }
    walk($code.AST);
    $found
}

my @item = Q|my $s; $s ~~ s[\d] = 5|, Q|$_ ~~ s[\d] = 5|, Q|my $s; $s + 1 ~~ s[\d] = 5|,
    Q|my $s; -$s ~~ s[\d] = 5|, Q|my $s; $s and s[\d] = 5|, Q|my $s; my @r = $s ~~ s[\d] = 5|,
    Q|my $s; $s ~~ S[\d] = 5|;
my @list = Q|s[\d] = 5|, Q|my @s; @s ~~ s[\d] = 5|, Q|my $s; $s.so ~~ s[\d] = 5|,
    Q|my $s; ($s) ~~ s[\d] = 5|, Q|my $s; $s[0] ~~ s[\d] = 5|, Q|my $s; my $r = s[\d] = 5|;

is infix-of($_), 'RakuAST::Assignment.new(:item)', $_ for @item;
is infix-of($_), 'RakuAST::Assignment.new', $_ for @list;
