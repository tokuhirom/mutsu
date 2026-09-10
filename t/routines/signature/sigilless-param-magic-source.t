use Test;

plan 12;

# A sigilless parameter (`\x`) re-reads its argument from the callee's env by the
# argument's SOURCE name, so that a later `x := …` can write through. That reread
# must not apply to the per-routine magic names: this frame has already reset `$_`
# to Any and `$!` to Nil, and rebound `@_`/`%_` to its own arguments, before
# binding runs — so `f($_)` bound the freshly-blanked topic instead of the value.

class K {
    method smiley    (K:D: Int \ch = 1) { "smiley={ch}" }
    method smiley-nd (K:D: Int \ch)     { "smiley-nd={ch}" }
    method plain     (K:   Int \ch)     { "plain={ch}" }
    method noinv     (Int \ch)          { "noinv={ch}" }
    method sigiled   (K:D: Int $ch)     { "sigiled=$ch" }
    method untyped   (K:D: \ch)         { "untyped={ch}" }
}

my $k = K.new;
$_ = 7;

is $k.smiley($_),    'smiley=7',    'a sigilless param takes the topic through a :D invocant';
is $k.smiley-nd($_), 'smiley-nd=7', 'and with no default';
is $k.plain($_),     'plain=7',     'and through a plain invocant';
is $k.noinv($_),     'noinv=7',     'and with no explicit invocant';
is $k.sigiled($_),   'sigiled=7',   'a sigiled param was already right';
is $k.untyped($_),   'untyped=7',   'an untyped sigilless param takes the topic, not Any';

# The topic from a `for` loop and from `given`, which is how it shows up in real
# code (String::Rotate's `for ^$str.chars { $str.rotate($_) }`).
for 7..7   { is $k.smiley($_), 'smiley=7', 'a for-loop topic binds'; }
given 7    { is $k.smiley($_), 'smiley=7', 'a given topic binds'; }

# `$!` is reset to Nil on entry, so it was the same hazard.
class E { method take (E:D: \ex) { ex.defined ?? ex.message !! 'Nil' } }
try { die "boom" };
is E.new.take($!), 'boom', 'a sigilless param takes $! by value, not the callee-reset Nil';

# A sigilless param must still alias a normal lexical (the reread's actual job).
sub writes (\x) { x = 99 }
my $target = 1;
writes($target);
is $target, 99, 'a sigilless param still aliases an ordinary lexical for write-through';

# The whole shape from String::Rotate: a role method with a :D invocant and a
# sigilless param, composed into an augmented builtin class.
role Rotate {
    multi method rotate-str (Str:D: Int \ch = 1 --> Str) {
        my \shft = abs(ch % self.chars);
        self.substr(shft) ~ self.substr(0, shft)
    }
}
use MONKEY-TYPING;
augment class Str does Rotate { }

is 'Rakudo'.rotate-str(3), 'udoRak', 'the augment+role shape works with a literal';
for 3..3 { is 'Rakudo'.rotate-str($_), 'udoRak', 'and with the loop topic'; }
