use v6;
use Test;

# A type-only invocant (`method m(K:D: ...)`) is anonymous: the source never
# names it. mutsu stores it under the env key `self` (tagged with the parser's
# `implicit-invocant` trait) so the binder can find it, but introspection has
# to show what the source wrote -- an empty `Parameter.name` and `$:` in `.raku`.
# The user-written `$self:` / `$a:` forms keep their names.

plan 21;

class K {
    method m(K:D: Int $c --> Int) { }
    method u(K:U: $w) { }
    method c(::?CLASS:D: $y) { }
    method p(K: $x) { }
    method s($self: $z) { }
    method r(Str:D $a:) { }
    method multi-d(K:D: $v) { 'defined' }
    method multi-u(K:U: $v) { 'type' }
}

my $m = K.^find_method('m');
is $m.signature.raku, ':(K:D $:: Int $c, *%_ --> Int)',
    'a type-only :D invocant renders as `K:D $::`, return type kept';
is $m.signature.params[0].raku, 'K:D $:', 'the invocant parameter\'s .raku is `K:D $:`';
is $m.signature.params.map(*.name).join('|'), '|$c|%_',
    'the invocant has no name; the others keep theirs';
is $m.signature.params[0].name, '', 'the anonymous invocant\'s .name is the empty string';
ok $m.signature.params[0].invocant, 'it is still the invocant';
is $m.signature.params[0].type.^name, 'K', 'and still constrained to K';

is K.^find_method('u').signature.raku, ':(K:U $:: $w, *%_)', 'a type-only :U invocant';
is K.^find_method('c').signature.raku, ':(K:D $:: $y, *%_)', 'a ::?CLASS:D invocant';
is K.^find_method('c').signature.params[0].name, '', '... is anonymous too';

# What was already anonymous stays so.
is K.^find_method('p').signature.raku, ':(K $:: $x, *%_)', 'a smiley-less type-only invocant';
is K.^find_method('p').signature.params[0].name, '', '... is anonymous';

# Named invocants keep their names.
is K.^find_method('s').signature.raku, ':($self:: $z, *%_)', 'an explicit `$self:` keeps its name';
is K.^find_method('s').signature.params[0].name, '$self', '... in .name too';
is K.^find_method('r').signature.raku, ':(Str:D $a:: *%_)', 'a typed named invocant keeps its name';
is K.^find_method('r').signature.params[0].name, '$a', '... in .name too';

# Binding is untouched: the anonymous invocant still constrains dispatch.
is K.new.multi-d(1), 'defined', 'a :D invocant accepts an instance';
is K.multi-u(1), 'type', 'a :U invocant accepts the type object';
dies-ok { K.multi-d(1) }, 'a :D invocant rejects the type object';
dies-ok { K.new.multi-u(1) }, 'a :U invocant rejects an instance';

# A type-only invocant declares no `$self` lexical (the explicit one does).
class W {
    method who(W:D: --> Str) { self.^name }
    method who2($self: --> Str) { $self.^name }
}
is W.new.who, 'W', '`self` is still available inside a method with a type-only invocant';
is W.new.who2, 'W', 'and `$self` inside one that names it';
