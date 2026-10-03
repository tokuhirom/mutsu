use Test;

# A block a method or package sub hands to `.map`/`.grep` reads the routine's
# own `my`, not a same-named `my` of the enclosing class/package body (#10651,
# the URI::Template `my &enc` shape). Expected values are rakudo's.

plan 8;

class V {
    my $enc = "outer";
    my &f = sub { "outerf" };
    my $cnt = 0;
    method scalar() { my $enc = "inner"; (1,).map({ $enc }).join }
    method code-var() { my &f = sub { "innerf" }; (1,).map({ f() }).join }
    method amp-call() { my &f = sub { "innerf" }; (1,).map({ &f() }).join }
    method in-grep() { my $cnt = 7; (1, 2, 3).grep({ $_ == $cnt - 5 }).join }
    method static() { (1,).map({ f() ~ $enc }).join }
    method bump() { (1, 2).map({ $cnt++ }).sink; $cnt }
}

is V.new.scalar, 'inner', 'a method-local scalar shadows the class-body one';
is V.new.code-var, 'innerf', 'a method-local `my &f` shadows the class-body one (bare call)';
is V.new.amp-call, 'innerf', '... and through `&f()`';
is V.new.in-grep, '2', 'a `.grep` block too';
is V.new.static, 'outerfouter', 'an unshadowed class-body lexical is still reached';
is V.new.bump, 2, 'a write to a class-body lexical from a block persists';

package P {
    my $enc = "outer";
    our sub run() { my $enc = "inner"; (1,).map({ $enc }).join }
}
is P::run(), 'inner', 'a package sub\'s own `my` shadows the package body\'s';

class URIish {
    my &enc = sub ($m) { "%" ~ $m.Str.ord.base(16) };
    my sub encode(Str:D $text) { $text.subst(/<[/]>/, &enc, :g) }
    method expand(%value) {
        my &enc = &encode;
        %value.map({ enc(.key) ~ "=" ~ enc(.value) }).join(",")
    }
}
is URIish.new.expand({ "a b" => "c/d" }), 'a b=c%2Fd', 'the URI::Template shape';
