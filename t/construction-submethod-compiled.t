use Test;
plan 9;

# §B Slice 3: BUILD/TWEAK submethods run as compiled bytecode.
{
    class P1 { has $.a; has $.b; submethod BUILD(:$x = 1) { $!a = $x; $!b = $x * 2 } }
    is P1.new(x => 5).a, 5, 'BUILD binds named arg';
    is P1.new(x => 5).b, 10, 'BUILD derives another attribute';
    is P1.new.a, 1, 'BUILD parameter default';
}

# TWEAK runs after BUILD/default init.
{
    class P2 { has $.n = 10; submethod TWEAK { $!n = $!n + 5 } }
    is P2.new.n, 15, 'TWEAK adjusts an attribute';
}

# fail inside BUILD yields a Failure, not a thrown error.
{
    class F { has $.x; submethod BUILD(:$x) { fail "bad" if $x < 0; $!x = $x } }
    ok (try F.new(x => 5)).defined, 'BUILD succeeds for valid value';
    my $r = F.new(x => -1);
    ok $r ~~ Failure, 'fail inside BUILD returns a Failure';
}

# Inherited BUILD chain runs each class's BUILD (base-first).
{
    class Base3 { has @.order is rw; submethod BUILD { self.order.push("base") } }
    class Sub3 is Base3 { submethod BUILD { self.order.push("sub") } }
    is Sub3.new.order.join(","), "base,sub", 'BUILD runs base-first across inheritance';
}

# A captured-outer write in BUILD still works (kept on the tree-walk path).
{
    my $count = 0;
    class C4 { submethod BUILD { $count++ } }
    C4.new; C4.new;
    is $count, 2, 'BUILD captured-outer write propagates';
}

# Attribute not touched by BUILD keeps its default.
{
    class DK { has $.a; has $.b = "kept"; submethod BUILD(:$a) { $!a = $a } }
    is DK.new(a => 1).b, "kept", 'untouched attribute keeps default';
}
