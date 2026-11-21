use Test;
use lib 'roast/packages/Test-Helpers';
use Test::Util;

# `--` ends option parsing for MAIN even with `:named-anywhere`: every word
# after it is positional, including ones that look like options (#18479;
# Red's `red migration apply --test -- $*EXECUTABLE -e 'exit 0'`).

plan 4;

my $code = ｢
    my %*SUB-MAIN-OPTS = :named-anywhere;
    multi MAIN('migration', 'apply', Str :$config = 'x') { print "plain" }
    multi MAIN('migration', 'apply', Bool :$test! where so *, Str :$config = 'c',
               *@command) { print "test[$config]: @command.join('|')" }
｣;

is_run $code, :args[<migration apply --test --config=y -- /bin/x -e --config=z -->],
    { :out('test[y]: /bin/x|-e|--config=z|--'), :err(''), :0status },
    'words after -- are positional under named-anywhere';
is_run $code, :args[<migration --test apply --config=y>],
    { :out('test[y]: '), :err(''), :0status },
    'named-anywhere still takes options between positionals';
is_run $code, :args[<migration apply --config=y>],
    { :out('plain'), :err(''), :0status },
    'without --test the plain candidate is chosen';
is_run ｢ sub MAIN(*@a, :$x) { print "@a.join('|') x={$x // 'none'}" } ｣,
    :args[<a -- --x=1 b>],
    { :out('a|--x=1|b x=none'), :err(''), :0status },
    'without named-anywhere -- ends options too';
