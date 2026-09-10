use Test;

# `say`, `print`, `put`, and `note` are statement-form listops, but their
# direct colonpairs still bind into the named lane rather than becoming output.
# A grouped Pair remains a positional value, which distinguishes call-site
# syntax from Pair data (ADR-0021).

plan 7;

{
    my $out = '';
    my $err = '';
    {
        my $*OUT = class { method print(*@a) { $out ~= @a.join } };
        my $*ERR = class { method print(*@a) { $err ~= @a.join } };
        say :a;
        say :d, 'x';
        print 'p', :a;
        put :a, 'u';
        note :a, 'n';
        say :!d:r, 'z';
        say (a => 1), 'q';
    }
    is $out, "\nx\npu\nz\na => 1q\n", 'IO listops omit direct named pairs but print grouped Pairs';
    is $err, "n\n", 'note omits its direct named pair';
}

# A direct Pair interpolation is named by the `|` call-site syntax too.
{
    my $out = '';
    {
        my $*OUT = class { method print(*@a) { $out ~= @a.join } };
        say |(:a), 'x';
    }
    is $out, "x\n", 'slipped direct Pair is named and omitted from say output';
}

sub collect(*@p, *%n) { @p.elems ~ '/' ~ %n.elems }
is (collect :a, 'x'), '1/1', 'ordinary call named binding remains unchanged';
is (a => 1).gist, 'a => 1', 'Pair data rendering remains unchanged';
is (a => 1).key, 'a', 'grouped Pair remains a Pair value';
is (a => 1).value, 1, 'grouped Pair value remains intact';
