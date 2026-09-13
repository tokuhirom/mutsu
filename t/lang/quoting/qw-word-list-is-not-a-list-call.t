use Test;

# A word-quoting construct is a LITERAL, not a call the program can intercept.
# mutsu lowered every one of them to a plain `list(...)` call, and `list` is an
# ordinary routine name a Raku program may declare -- so declaring `sub list`
# made every word list in that compilation unit resolve to the user's routine
# instead of building a List.
#
# Found in `Crane::List`, which declares a whole `multi sub list` set: its own
# word lists then bound against its own candidates, which is where
# `Unknown function: list` and
# `Cannot resolve caller list(Str:D, Str:D, Str:D)` came from, aborting both
# `t/list.rakutest` and `t/flatten.rakutest`.

plan 17;

# --- a user `sub list` does not capture a word list -------------------------

{
    my $captured = 0;
    my &saw := sub { $captured++ };

    sub list($a, $b) { saw(); 'user-list' }

    is-deeply qw<zero one two>, ("zero", "one", "two"),
        'qw<> builds a List, not a call to the user routine';
    is-deeply <zero one two>, ("zero", "one", "two"),
        '<> builds a List';
    is-deeply qw/zero one two/, ("zero", "one", "two"),
        'qw// builds a List';
    is-deeply qqw<zero one two>, ("zero", "one", "two"),
        'qqw<> builds a List';
    is-deeply q:w/zero one two/, ("zero", "one", "two"),
        'q:w// builds a List';
    is $captured, 0, 'and none of them called the user routine';

    is list(1, 2), 'user-list', 'the user routine is still callable by name';
    is $captured, 1, 'and that call did reach it';
}

# --- a user `multi sub list` set, the shape Crane::List declares ------------

{
    multi sub list(Positional:D $c, :@path) { "pos:" ~ $c.elems }
    multi sub list($c, :path(@))            { "any" }
    multi sub list('do', $c, :@carry = ())  { "do-any" }

    my $a = qw<zero one two>;
    is $a.elems, 3, 'the word list is a 3-element List';
    is list($a), 'pos:3', 'and the user multi still dispatches on it';
    is list('do', $a), 'do-any', 'including the literal-parameter candidate';
}

# --- the ordinary behaviour of a word list is unchanged ---------------------

{
    is-deeply qw<a b c>, ("a", "b", "c"), 'plain qw';
    my $x = qw<a>;
    is-deeply $x, "a", 'a one-word list is that word, not a List';
    my @a = qw<a b>;
    is-deeply @a, ["a", "b"], 'assigning a word list to an array flattens it';
    is-deeply (qw<a b>, qw<c>), (("a", "b"), "c"),
        'a word list nested in a list stays one element';
    my $n = 2;
    is-deeply qqw<a $n>, ("a", "2"), 'qqw interpolates';
    is-deeply <1 2 3>.map(* + 1), (2, 3, 4), 'angle words are usable as a List';
}
