use Test;
plan 8;

# `Parameter.raku` spells a parameter the way it was declared.
sub a(*%o, *@r, **@d, :$n, :$m!) {}
is-deeply &a.signature.params.map(*.raku).list,
    ("*\%o", "*\@r", "**\@d", ":\$n", ":\$m!"), "slurpies and named parameters";

sub b(|c) {}
is &b.signature.params[0].raku, "|c", "a named capture";

sub c(|) {}
is &c.signature.params[0].raku, "|", "an anonymous capture";

sub d(\x) {}
is &d.signature.params[0].raku, '\x', "a sigilless parameter";

sub e(:z($w), :$y!) {}
is-deeply &e.signature.params.map(*.raku).list, (":z(\$w)", ":\$y!"), "a renamed named parameter";

is &a.signature.raku, ':(*%o, *@r, **@d, :$n, :$m!)', "the signature spelling";
is &b.signature.raku, ':(|c)', "capture signature";
is &e.signature.raku, ':(:z($w), :$y!)', "renamed named signature";
