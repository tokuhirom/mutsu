use Test;

# From the Commands ecosystem distribution: a class that has its own `print`
# but no `say`/`put` acts as an output handle (Mu.say(\x) / Mu.say(|) fallback).

plan 4;

class Catcher {
    has @!seen;
    method print(*@parts --> True) { @!seen.push: @parts.join }
    method seen() { @!seen.splice }
}
my $c = Catcher.new;
$c.say("hi");
is-deeply $c.seen, ["hi\n"], 'say(x) gists and appends nl-out';
$c.say(1, "b");
is-deeply $c.seen, ["1b\n"], 'say(|) joins the gists';
$c.put("x", 2);
is-deeply $c.seen, ["x2\n"], 'put(|) stringifies and appends nl-out';
$c.say([1, 2]);
is-deeply $c.seen, ["[1 2]\n"], 'say uses gist';
