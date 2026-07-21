use v6;
use Test;

# A named attributive callable parameter with a twigil (`:&!writer`, `:&.hook`)
# must accept an optional `?` marker and/or a default value. The bare-anonymous
# `&?` / `&!` shorthand ate the `&`, mis-reading the twigil `!`/`?` as the
# required/optional marker of an anonymous callable and stranding the name.
# Regression surfaced by Chart::Gnuplot (`:&!writer? = -> $msg { ... }`).

plan 8;

# A `:&!writer` param binds a named argument straight into a private callable
# attribute. Exercise it as the sole positional-less BUILD parameter (the
# Chart::Gnuplot shape) and combined with other named params.

class C {
    has &!writer;
    has $.log = '';
    submethod BUILD(:&!writer? = -> $m { "default:$m" }) {
        $!log = &!writer('hi');
    }
    method run { $!log }
}
is C.new.run, 'default:hi', "attributive callable named-twigil param uses its pointy default";
is C.new(writer => -> $m { "custom:$m" }).run, 'custom:hi',
    "a passed callable overrides the default";

# optional marker alone (no default)
class D {
    has &!cb;
    method has-cb { defined &!cb }
    submethod BUILD(:&!cb?) { }
}
nok D.new.has-cb, "optional named-twigil callable may be omitted";
ok  D.new(cb => -> {}).has-cb, "optional named-twigil callable binds when supplied";

# A `:&!writer` param alongside other params, matching Chart::Gnuplot's BUILD.
class E {
    has &!writer;
    has $.persist;
    has $.out = '';
    submethod BUILD(:$terminal!, :$!persist = True, :&!writer? = -> $m { "d:$m" }) {
        $!out = &!writer($terminal);
    }
    method run { $!out }
}
is E.new(terminal => 'png').run, 'd:png',
    "attributive callable default coexists with other named params";
is E.new(terminal => 'x', writer => -> $m { "w:$m" }).run, 'w:x',
    "attributive callable override coexists with a required named param";

# The bare anonymous `&?` / `&!` shorthand still works (no name follows).
sub opt(&?)  { 'opt-ok' }
sub req(&!)  { 'req-ok' }
is opt(),        'opt-ok', "bare anonymous &? (optional) still parses";
is req(-> {}),   'req-ok', "bare anonymous &! (required) still parses";
