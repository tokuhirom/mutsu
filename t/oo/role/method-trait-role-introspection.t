use Test;

# A role a method trait composes onto the method (`$m does Tag`) belongs to the
# method, so the method objects the metaclass hands out carry it:
# `C.^methods.grep(Tag)` finds exactly the tagged methods. Tinky's
# `is before-apply-workflow` / `is before-apply-transition` callbacks are
# found and run this way.

plan 6;

role Tag { }
multi sub trait_mod:<is>(Method $m, :$tagged!) { $m does Tag }

class D {
    has $.seen;
    method m($x) is tagged { $!seen = $x }
    method n() { }
    method run-tagged($x) {
        for self.^methods.grep(Tag) -> $method { self.$method($x) }
    }
}

is D.^methods.grep(Tag).map(*.name), ('m',), '^methods carries the trait role';
ok D.^find_method('m') ~~ Tag, '^find_method too';
nok D.^find_method('n') ~~ Tag, 'an untagged method does not';
my $d = D.new;
$d.run-tagged('hi');
is $d.seen, 'hi', 'the tagged method is callable from the introspected list';

role Extra { method extra { } }
my $e = D.new;
$e.^mixin(Extra);
is $e.^methods.grep(Tag).elems, 1, 'the tag survives a mixin on the object';

{
    my class Lex {
        method m() is tagged { }
    }
    is Lex.^methods.grep(Tag).elems, 1, 'a lexical class keeps the tag too';
}
