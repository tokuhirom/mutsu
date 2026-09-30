use Test;

# Promise callbacks must resolve a lexical `&name` before a same-named core
# routine. Cover callbacks invoked inline and after a Promise settles.
plan 6;

my &run = -> $n { "lex $n" };
my &uc = -> $n { "lex $n" };

is (await Promise.kept(1).then({ run(2) })), 'lex 2',
    '.then on a kept Promise calls lexical &run';
is (await Promise.kept(1).andthen({ uc(3) })), 'lex 3',
    '.andthen on a kept Promise calls lexical &uc';

my $pending-then = Promise.new;
my $then = $pending-then.then({ run(4) });
$pending-then.keep(1);
is (await $then), 'lex 4', '.then on a later-kept Promise calls lexical &run';

my $pending-andthen = Promise.new;
my $andthen = $pending-andthen.andthen({ uc(5) });
$pending-andthen.keep(1);
is (await $andthen), 'lex 5',
    '.andthen on a later-kept Promise calls lexical &uc';

my $pending-orelse = Promise.new;
my $orelse = $pending-orelse.orelse({ run(6) });
$pending-orelse.break('reason');
is (await $orelse), 'lex 6',
    '.orelse on a later-broken Promise calls lexical &run';

is (await start { run(7) }), 'lex 7',
    'another threaded callback still calls lexical &run';
