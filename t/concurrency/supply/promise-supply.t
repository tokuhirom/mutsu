use Test;

# `Promise.Supply` emits the result then is done, or quits with the cause.

plan 8;

my @got;
my $settled = Promise.new;
Promise.kept(42).Supply.tap(-> $v { @got.push("value $v") },
    done => { @got.push('done'); $settled.keep });
await $settled;
is @got.join(';'), 'value 42;done', 'a kept promise emits its result, then is done';

@got = ();
$settled = Promise.new;
Promise.broken(X::AdHoc.new(:payload<boom>)).Supply.tap(-> $v { @got.push('value') },
    quit => -> $e { @got.push("quit {$e.message}"); $settled.keep });
await $settled;
is @got.join(';'), 'quit boom', 'a broken promise quits with its cause';

my $later = Promise.new;
my $done = Promise.new;
$later.Supply.tap(-> $v { @got.push("later $v") }, done => { $done.keep });
$later.keep(7);
await $done;
ok @got.grep('later 7'), 'a promise kept after the tap emits then';

my $broken-later = Promise.new;
my $quit = Promise.new;
$broken-later.Supply.tap(-> $ { }, quit => -> $e { $quit.keep($e.message) });
$broken-later.break('bad');
is (await $quit), 'bad', 'a promise broken after the tap quits';

is (await Promise.kept(3).Supply), 3, 'await on the Supply';
is Promise.kept(5).Supply.list, (5,), '.list of the Supply';

my $p = Promise.new;
start { sleep 0.05; $p.keep(9) };
my @react;
react { whenever $p.Supply { @react.push($_) } }
is @react, [9], 'a react whenever on a planned promise\'s Supply';

my ($kind, $value);
react {
    whenever Promise.broken('nope').Supply {
        QUIT { default { ($kind, $value) = 'quit', .message } }
    }
}
is "$kind $value", 'quit nope', 'a react whenever sees the quit';
