use Test;

# A planned method call takes the owner's role-ness, its role composition
# bindings, the package its body runs in and its `?ROLE` from the plan
# instead of asking per call (#18267 D1a). Each shape below is called several
# times, so later calls replay the plan, and must keep rakudo's answers;
# `augment` and a run-time `^add_method` drop the plans.
plan 9;

role Greeter {
    method greet(Str $who) { "hello $who from " ~ $?ROLE.^name }
}
class Host does Greeter { }
my $h = Host.new;
is (^3).map({ $h.greet("x$_") }).join('|'),
    'hello x0 from Greeter|hello x1 from Greeter|hello x2 from Greeter',
    'a composed role method keeps $?ROLE';

role Typed[::T] {
    method make($v) { $v ~~ T }
    method kind() { T.^name }
}
class IntBox does Typed[Int] { }
class StrBox does Typed[Str] { }
is (^2).map({ IntBox.new.make($_) ~ StrBox.new.make($_) }).join(','), 'TrueFalse,TrueFalse',
    'role parameter bindings on every call';
is (^2).map({ StrBox.new.kind ~ IntBox.new.kind }).join(','), 'StrInt,StrInt',
    'two compositions of one parametric role keep apart';

class WithSubs {
    sub helper($x) { $x * 10 }
    method use(Int $x) { helper($x) + 1 }
}
my $w = WithSubs.new;
is (^3).map({ $w.use($_) }).join(','), '1,11,21', 'a class-scoped sub resolves through the body package';

module Outer {
    our sub scaled($x) { $x * 100 }
    class Inner {
        method go(Int $x) { scaled($x) }
    }
}
is (^2).map({ Outer::Inner.new.go($_ + 1) }).join(','), '100,200', 'a class declared in a module';

class Late {
    method tag() { 'old' }
    method call-tag() { self.tag }
}
my $l = Late.new;
my @before = (^2).map({ $l.call-tag });
use MONKEY-TYPING;
augment class Late { method extra() { 'extra' } }
is (@before, $l.call-tag, $l.extra).flat.join(','), 'old,old,old,extra', 'augment drops the plans';

class Added { method m() { 'm' } }
my $a = Added.new;
my @m = (^2).map({ $a.m });
Added.^add_method('n', method () { 'n' });
is (@m, $a.m, $a.n).flat.join(','), 'm,m,m,n', 'a run-time ^add_method drops the plans';

role R2 { method who() { self.^name } }
my $mixed = Host.new but R2;
is (^2).map({ $mixed.who }).join(','), (^2).map({ $mixed.^name }).join(','),
    'a mixin receiver answers its own name';
is (^2).map({ $mixed.greet('m') }).join('|'), 'hello m from Greeter|hello m from Greeter',
    'a role method through a mixin receiver';
