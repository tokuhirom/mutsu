use Test;

# From the Config::TOML distribution: an action that dies when its rule reduces
# must leave `.parse`, even though the match goes on to succeed and a later
# action runs code (a `~~`) that used to clear the recorded error.

plan 6;

grammar G {
    token TOP { <doc> }
    token doc { <item>+ % \n }
    token item { <k> '=' <v> }
    token k { \w+ }
    token v { \d+ }
}

class Dup {
    method item($/) { die "duplicate key" }
    method doc($/) { make so $/ ~~ /a/ }
}
class DupTyped {
    method item($/) { die X::AdHoc.new(:payload("typed")) }
    method doc($/) { make so $/ ~~ /a/ }
}
class Fine {
    method doc($/) { make so $/ ~~ /a/ }
}

throws-like { G.parse("a=1\nb=2", :actions(Dup.new)) }, X::AdHoc, message => /duplicate/,
    'the action die leaves .parse after a later action ran a smartmatch';
throws-like { G.parse("a=1", :actions(DupTyped.new)) }, X::AdHoc, payload => 'typed',
    'a typed exception arrives as itself';

my $ok = G.parse("a=1\nb=2", :actions(Fine.new));
ok $ok, 'a parse whose actions do not die still succeeds';
ok $ok<doc>.made, 'and its actions ran';

# the error belongs to the failed parse alone
lives-ok { G.parse("a=1", :actions(Fine.new)) }, 'the next parse is not poisoned';
nok G.parse("???", :actions(Fine.new)), 'a failing match is still a failed match';
