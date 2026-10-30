use Test;

plan 4;

# A closure created in a routine that captures one of the routine's own
# readonly parameters sees THIS invocation's value, not a shared cell of the
# same name that the thread running the routine inherited from its spawner
# (#15625: MCP::Client's Correlator `!claim($id)` inside `$!lock.protect`).

class Claims {
    has Lock $!lock .= new;
    has %!seen;
    method !claim($id) {
        $!lock.protect: { %!seen{$id} = $id; $id }
    }
    method claim($id) { self!claim($id) }
    method seen { %!seen.keys.sort.List }
}

# A caller-side `$id` captured and mutated by a closure becomes a shared cell.
my $id = 1;
my &bump = { $id++ };
bump();

my $c = Claims.new;
my @got;
my $done = Promise.new;
my $proc = Proc::Async.new(:w, 'cat');
$proc.stdout.lines.tap(-> $line { @got.push($c.claim($line.Int)) }, done => { $done.keep(True) });
$proc.start;
$proc.print("10\n20\n30\n");
$proc.close-stdin;
await Promise.anyof($done, Promise.in(10));

is-deeply @got, [10, 20, 30], 'a tap thread claims each id it is given';
is-deeply $c.seen, ("10", "20", "30"), 'and records each one under its own key';

my @from-threads = await (1..3).map(-> $n { start { $c.claim($n * 100) } });
is-deeply @from-threads, [100, 200, 300], 'a start block claims each id it is given';
is $id, 2, 'the caller-side cell is untouched';
