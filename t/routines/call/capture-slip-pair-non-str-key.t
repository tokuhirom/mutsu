use Test;

# `|$pair` is a named argument whatever the key's type: Rakudo's
# `Pair.Capture` names it by the key's string form (CSS::Grammar::AST builds
# `CSSObject::StyleSheet => …` and CSS::Stylesheet dispatches `|.ast` to
# `multi method load(:stylesheet!)`).

plan 6;

enum E (:StyleSheet<stylesheet>);
enum I (:A(5));
sub f(*@pos, *%named) { %named }

is-deeply f(|(E::StyleSheet => 1)), %(stylesheet => 1), 'a Str-valued enum key';
is-deeply f(|(I::A => 1)), %(A => 1), 'an Int-valued enum key';
is-deeply f(|(1 => 2)), %('1' => 2), 'an Int key';
is-deeply f(|(1.5 => 2)), %('1.5' => 2), 'a Rat key';

my %h{Int} = 1 => 2;
is-deeply f(|%h), %('1' => 2), 'an object hash slips its entries as named';

class C {
    multi method load(:stylesheet($)!) { 'sheet' }
    multi method load($) is default { 'default' }
}
my $p = E::StyleSheet => [1];
is C.load(|$p), 'sheet', 'multi dispatch sees the named argument';
