use Test;

# BEGIN-time evaluation of an extended identifier's adverb value
# (`Language/syntax.rakudoc`, "Extended identifiers").
#
# `«...»` interpolates like `qqw`, `(...)`/`[...]` hold an expression list, and
# `<...>` deliberately does neither. All of them are evaluated *before* the
# value becomes part of the variable's name, and the evaluation is BEGIN-time:
# only a `constant` is visible there.

plan 26;

# --- `«...»` interpolates a constant ----------------------------------------
{
    constant $c = 42;
    my $a:foo<42> = "answer";
    is $a:foo«$c», "answer", '«$c» names the same variable as <42>';
    is $a:foo<42>, "answer", 'the literal spelling still works';
}

{
    constant $i = 42;
    my $foo:bar«$i» = 'meow';
    is $foo:bar«$i», 'meow', 'a declaration may interpolate too';
    is $foo:bar<42>, 'meow', 'and the literal spelling finds it';
}

{
    constant $c = 42;
    my $a:foo<42 7> = "two";
    is $a:foo«$c 7», "two", 'interpolation mixes with literal words';
}

{
    constant $c = "a b";
    my $a:foo<a b> = "split";
    is $a:foo«$c», "split", 'the interpolated value is word-split, like qqw';
}

# `<...>` mimics single quotes: it never interpolates.
{
    constant $c = 42;
    my $a:foo<$c> = "literal";
    is $a:foo<$c>, "literal", '<$c> is the literal name, not an interpolation';
}

# A `«...»` without a sigil is plain `qw`.
{
    constant C = 7;
    my $a:foo<C> = "word";
    is $a:foo«C», "word", '«C» is a literal word, not a constant read';
}

# --- `(...)` / `[...]` evaluate an expression -------------------------------
{
    my $foo:bar<2> = 5;
    is $foo:bar(1+1), 5, '(1+1) names <2>';
    is $foo:bar[1+1], 5, '[1+1] names <2> as well';
}

{
    my $a:foo<ab> = "cat";
    is $a:foo("a" ~ "b"), "cat", 'a constant string expression folds';
}

{
    constant C = 7;
    my $a:foo<7> = "seven";
    is $a:foo(C), "seven", 'a sigilless constant resolves inside (...)';
    is $a:foo(C), $a:foo<7>, 'and names exactly the literal spelling';
}

{
    constant $c = 3;
    my $a:foo<9> = "nine";
    is $a:foo($c * 3), "nine", 'a sigilled constant resolves inside (...)';
}

{
    my $a:foo<3.5> = "rat";
    is $a:foo(7/2), "rat", 'a Rat stringifies the way the literal does';
}

{
    my $a:foo<1 2> = "list";
    is $a:foo(1,2), "list", 'a comma list becomes space-separated words';
    is $a:foo[1,2], "list", 'and so does the [] spelling';
}

# --- canonicalization -------------------------------------------------------
{
    my $a:foo<a b> = "canon";
    is $a:foo< a  b >, "canon", 'extra whitespace in <> is not part of the name';
    is $a:foo«a  b», "canon", 'nor in «»';
    is $a:foo['a','b'], "canon", 'nor between [] items';
    is $a:foo('a','b'), "canon", 'nor between () items';
}

# --- multiple adverbs and other sigils --------------------------------------
{
    constant $c = 1;
    my $a:foo<1>:bar<2> = "both";
    is $a:foo«$c»:bar(1+1), "both", 'every adverb of a name is evaluated';

    my @arr:foo<1> = 1, 2, 3;
    is-deeply @arr:foo«$c».List, (1, 2, 3), 'an @ variable interpolates too';

    my %h:foo<1> = a => 1;
    is %h:foo«$c»<a>, 1, 'and a % variable';
}

# --- BEGIN time means BEGIN time --------------------------------------------
# An ordinary lexical has no compile-time value, so raku reports the resulting
# name as undeclared. mutsu names the offending variable directly.
{
    my $x = "abc";
    my $a:foo<abc> = "runtime";
    dies-ok { EVAL 'my $x = "abc"; my $a:foo<abc> = 1; $a:foo«$x»' },
        'a runtime lexical cannot be interpolated into a name';
}

{
    dies-ok { EVAL '$a:foo«$nope»' },
        'an undeclared variable in an adverb value is an error';
}
