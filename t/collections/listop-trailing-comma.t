use Test;

# A trailing comma after the last argument of a builtin listop call belongs to
# the argument list; it must not wrap the call's result in a one-element list.
# Found in Selkie (`join '', "a", "b", ;` in its terminal-cleanup sequence).

plan 7;

sub t1(--> Str) {
    join '',
        "a",     # one
        "b",
        "c", # trailing comma, then the statement end
    ;
}
is t1(), "abc", 'join with trailing comma before ; returns a Str';

sub t2 { join "-", "a", "b", ; }
is t2().raku, '"a-b"', 'trailing comma then ; on one line';

sub t3 { join "-", "a", "b", }
is t3().raku, '"a-b"', 'trailing comma then }';

sub t4 { my $x = join "-", "a", "b",; $x }
is t4().raku, '"a-b"', 'trailing comma in an assignment';

is (join "-", 1, 2,).raku, '"1-2"', 'parenthesised';

my @c = 1;
my $r = do { join "-", 1, 2, if @c };
is $r, "1-2", 'trailing comma before a statement modifier';

sub foo(*@a) { @a.join("|") }
sub t7 { foo "a", "b", ; }
is t7(), "a|b", 'user sub listop with trailing comma (unchanged)';
