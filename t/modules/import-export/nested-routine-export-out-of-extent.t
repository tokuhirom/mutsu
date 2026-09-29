use v6;
use Test;

# An exported routine nested in another routine's body reads its free
# variables from the declaring routine, never from its caller (mutsu#10114).
# Called outside the declaring routine's dynamic extent -- before it ever ran,
# or after it returned -- it must not touch the caller's same-named lexical.

plan 14;

my $lib = $*TMPDIR.add("mutsu-nested-export-extent-{$*PID}");
$lib.mkdir;
LEAVE { try { .unlink for $lib.dir; $lib.rmdir } }

$lib.add('NestedExtent.rakumod').spurt(q:to/END/);
    unit module NestedExtent;
    multi sub set(Callable $c) is export {
        my @t;
        my multi sub test(Str $d, Callable $s) is export { @t.push($d) }
        my multi sub test(Callable $s) is export { test("anon", $s) }
        $c();
        @t.join(",")
    }
    sub counter(&body) is export {
        my $n = 0;
        my sub bump($by = 1) is export { $n += $by }
        body();
        $n
    }
    sub tally(&body) is export {
        my %h;
        my sub mark($k) is export { %h{$k}++ }
        my sub marked() is export { %h.keys.sort.join(",") }
        body();
        %h.keys.sort.join(",")
    }
    END

$lib.add('FileScope.rakumod').spurt(q:to/END/);
    unit module FileScope;
    my %seen;
    sub see($k) is export { %seen{$k}++ }
    sub seen() is export { %seen.keys.sort.join(",") }
    END

sub run-code($code) {
    my $proc = run($*EXECUTABLE, '-I', $lib.Str, '-e', $code, :out, :err);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($out, $err)
}

{
    my ($out, $err) = run-code(q:to/CODE/);
        use NestedExtent;
        my @t = <mine>;
        test("outside", sub {});
        say @t;
        say set(sub { my @t = <shadow>; test("x", sub {}); say @t });
        CODE
    is $out.lines[0], '[mine]', 'a call before any activation leaves the caller\'s @t alone';
    is $out.lines[1], '[shadow]', 'a shadowing @t in the callback is left alone';
    ok $out.lines[2].ends-with('x'), 'the in-extent call writes the declaring frame\'s @t';
    is $err, '', 'no error';
}

{
    my ($out, $err) = run-code(q:to/CODE/);
        use NestedExtent;
        sub foo { my @t = <foo>; test("y", sub {}); @t.join(",") }
        say set(sub { say foo() });
        my @t = <mine>;
        test("after", sub {});
        say @t;
        CODE
    is $out, "foo\ny\n[mine]\n",
        'an in-extent call through a sub with its own @t, and a call after the routine returned';
    is $err, '', 'no error';
}

{
    my ($out, $err) = run-code(q:to/CODE/);
        use NestedExtent;
        my $n = 100;
        bump(5);
        say $n;
        say counter({ my $n = 7; bump(2); bump });
        CODE
    is $out, "100\n3\n", 'a scalar free variable: the caller\'s $n is untouched';
    is $err, '', 'no error';
}

{
    my ($out, $err) = run-code(q:to/CODE/);
        use NestedExtent;
        my %h = a => 1;
        mark("z");
        say %h.keys.sort.join(",");
        say marked();
        say tally({ my %h; mark("q"); mark("r"); say marked() });
        CODE
    is $out.lines[0], 'a', 'a hash free variable: the caller\'s %h is untouched';
    is $out.lines[1], 'z', 'sibling nested subs share the routine\'s static %h';
    is $out.lines[2], $out.lines[3], 'in extent, siblings share the activation\'s %h';
    is $err, '', 'no error';
}

{
    my ($out, $err) = run-code(q:to/CODE/);
        use FileScope;
        my %seen = a => 1;
        see("x");
        say %seen.keys.sort.join(",");
        say seen();
        CODE
    is $out, "a\nx\n", 'an element increment of a module file-scope %h lands in the module\'s';
    is $err, '', 'no error';
}
