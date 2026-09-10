use Test;

# `$.name()` in string interpolation is `self.name()` — the accessor called with
# explicit parens. mutsu used to interpolate `$.name` but leave the trailing `()`
# as a literal fragment (`Jane()`), diverging from raku. (Language/syntax.rakudoc.)

plan 8;

class Person {
    has Str $.name = "Anon";
    has @.items = (1, 2, 3);
    method greet($who) { "hi $who" }

    method m-bare      { "[$.name]" }
    method m-parens    { "[$.name()]" }
    method m-parens-x  { "[$.name()!]" }
    method m-chain     { "[$.name.chars()]" }
    method m-args      { "[$.greet('you')]" }
    method m-elems     { "[$.items.elems()]" }
    method m-no-parens { "[$.name.uc]" }   # no trailing parens => `.uc` stays literal
}

my $p = Person.new(:name<Jane>);

is $p.m-bare,      "[Jane]",   'bare $.name interpolates';
is $p.m-parens,    "[Jane]",   '$.name() interpolates (parens consumed)';
is $p.m-parens-x,  "[Jane!]",  '$.name() followed by literal text';
is $p.m-chain,     "[4]",      '$.name.chars() chained call';
is $p.m-args,      "[hi you]", '$.method(args) with arguments';
is $p.m-elems,     "[3]",      '$.items.elems() on an array attribute';
is $p.m-no-parens, "[Jane.uc]",'trailing .uc without parens stays literal (raku parity)';

# Unicode attribute value round-trips through the rebuilt method call.
class Uni { has $.n = "Zoë"; method g { "<$.n()>" } }
is Uni.new.g, "<Zoë>", 'unicode value via method-call parens';
