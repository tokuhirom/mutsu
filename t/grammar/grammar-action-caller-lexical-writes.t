use Test;

# An action that runs at its rule's reduction (ADR-7576) can rebind the
# lexicals of the code that called `.parse`; every write must be visible there
# once `.parse` returns, while the action's own `my` variables stay its own.

plan 12;

grammar G { token TOP { <w>+ % ' ' }; token w { (\w)+ } }

{
    my $count = 0;
    my @seen;
    my %h;
    my $last;
    my class A {
        method w($/) { $count++; @seen.push(~$/); %h{~$/} = $0.elems; $last = ~$/; make ~$/ }
        method TOP($/) { make $<w>».made.join('|') }
    }
    my $m = G.parse('ab cde f', :actions(A));
    is $count, 3, 'a scalar counter';
    is-deeply @seen, ['ab', 'cde', 'f'], 'an array push';
    is-deeply %h.sort.List, (ab => 2, cde => 3, f => 1), 'a hash store';
    is $last, 'f', 'a scalar assignment';
    is $m.made, 'ab|cde|f', 'the parent sees the children\'s made';
}

sub in-sub {
    my $c = 0;
    my class B { method w($/) { $c += 10 } }
    G.parse('a b', :actions(B));
    $c
}
is in-sub(), 20, 'a lexical of the sub that called .parse';

{
    my $total = 0;
    my &cb = { $total += $_ };
    my class C { method w($/) { cb($/.chars) } }
    G.parse('aa bbb', :actions(C));
    is $total, 5, 'a write through a closure the action calls';
}

{
    my $*dyn = 0;
    my class D { method w($/) { $*dyn++ } }
    G.parse('x y z', :actions(D));
    is $*dyn, 3, 'a dynamic variable';
}

{
    my $x = 1;
    my $count = 3;
    my class E { method w($/) { my $x = 5; my $count = 99; } }
    G.parse('q', :actions(E));
    is "$x $count", '1 3', 'the action\'s own lexicals do not leak out';
}

{
    my $count = 0;
    my class F { method w($m) { $count = 1000 + $m.chars } }
    G.parse('qq', :actions(F));
    is $count, 1002, 'a signature without $/';
}

{
    my class H { has $.n = 0; method w($/) { $!n++ } }
    my $h = H.new;
    my $m = G.parse('a b c d', :actions($h));
    is $h.n, 4, 'an attribute of the actions instance';
    is $m.actions.n, 4, '.actions is the mutated instance';
}
