use Test;

# The generated MAIN usage message follows Rakudo's `default-generate-usage`
# (#9796): named options before positionals, `[=Type]` for an option whose
# type accepts `True`, aliases as `-l|--length`, one dash for a one-letter
# name, the routine's `#|` doc after `--`, and a table of the `#=`-documented
# parameters with their defaults.

plan 9;

sub usage(Str $code, *@args) {
    my $proc = run $*EXECUTABLE, '-e', $code, |@args, :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($out, $err, $proc.exitcode)
}

{
    my ($out, $err, $code) = usage(q:to/END/, '--bogus');
        sub MAIN(Str $file = 'f.dat',   #= a file
                 Int :l(:$length) = 24, #= length
                 Bool :$verbose, :$c) {}
        END
    is $err, join("\n",
        "Usage:",
        "  -e '...' [-l|--length[=Int]] [--verbose] [-c[=Any]] [<file>]",
        "  ",
        "    [<file>]             a file [default: 'f.dat']",
        "    -l|--length[=Int]    length [default: 24]",
        ""), 'named first, aliases, [=Type] and the WHY table';
    is $code, 2, 'a failed dispatch exits with 2';
}

{
    my ($out, $err) = usage(q:to/END/);
        sub new-main($name, *@stuff) { }
        RUN-MAIN(&new-main, Nil);
        END
    is $err, "Usage:\n  -e '...' <name> [<stuff> ...]\n",
        'RUN-MAIN describes the callable it was given';
}

{
    my ($out) = usage(q:to/END/, 'x', 'y');
        sub new-main($name, $other) { print "new-main $name $other" }
        RUN-MAIN(&new-main, Nil);
        END
    is $out, 'new-main x y', 'RUN-MAIN dispatches to the callable it was given';
}

{
    my ($out, $err) = usage(q:to/END/, '--bogus');
        #| the main thing
        multi sub MAIN(Int $x, Str :$name!, :@list, Rat :$r = 1.5, *@rest) {}
        #| second
        multi sub MAIN('add', $y, Bool :v(:$verbose), *%h) {}
        multi sub MAIN(:$help, :x(:y(:$zed))) {}
        END
    is $err, qq:to/END/, 'multi candidates in declaration order with their docs';
        Usage:
          -e '...' --name=<Str> [--list=<Any> ...] [-r=<Rat>] <x> [<rest> ...] -- the main thing
          -e '...' [-v|--verbose] [--<h>=...] add <y> -- second
          -e '...' [--help[=Any]] [-x|-y|--zed[=Any]]
        END
}

{
    my ($out, $err) = usage(q:to/END/, 'b', '1', '2');
        multi MAIN("a", $x) {}
        multi MAIN("b", $y) {}
        multi MAIN($z) {}
        END
    is $err, "Usage:\n  -e '...' b <y>\n",
        'a sub-command argument narrows the usage to its candidates';
}

{
    my ($out, $err) = usage(q:to/END/, 'S');
        subset S where / 'ok' /;
        enum Color <red green blue>;
        sub MAIN(S $s-pos, S :$s-named = "ok", Color :$c,
                 Int :$big = 12345678901234567890123, #= big
                 Str :$long = 'abcdefghijklmnopqrstuvwxyz', #= long
                 ) {}
        END
    is $err, join("\n",
        "Usage:",
        "  -e '...' [--s-named[=S]] [-c=<Color> (blue green red)] [--big[=Int]] [--long=<Str>] <s-pos>",
        "  ",
        "    --big[=Int]     big [default: 123456789…4567890123]",
        "    --long=<Str>    long [default: 'abcdefghijklmnopqrs…']",
        ""), 'subsets, enums and long defaults';
}

{
    my ($out, $err) = usage(q:to/END/, '1');
        sub MAIN($pos, :x(:y(:$zed))) { print $*USAGE.Str }
        END
    is $out, "Usage:\n  -e '...' [-x|-y|--zed[=Any]] <pos>",
        '$*USAGE holds the whole message';
}

{
    my ($out, $err) = usage(q:to/END/, '--y=5');
        sub MAIN(:x(:y(:$zed))) { print $zed }
        END
    is $out, '5', 'every link of a nested alias chain is accepted';
}
