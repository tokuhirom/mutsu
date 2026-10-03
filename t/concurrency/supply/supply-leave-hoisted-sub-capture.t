use Test;

plan 3;

# A supply body with a phaser (LEAVE/ENTER) and a sub that captures one of its
# lexicals: the `whenever` callback runs after the body has left, and must
# still see the lexical's value, not a block-exit Nil.
my $in = supply { emit "x" };

my @seen;
my $a = supply {
    my $g = 42;
    sub f { $g }
    whenever $in -> $l { @seen.push: $g; $g = 7; @seen.push: f() }
    LEAVE { }
};
$a.tap({;});
is-deeply @seen, [42, 7], 'whenever reads the lexical a hoisted sub captured (LEAVE)';

@seen = ();
my $b = supply {
    my $g = 43;
    sub f { $g }
    whenever $in -> $l { @seen.push: f() }
    ENTER { }
};
$b.tap({;});
is-deeply @seen, [43], 'the sub called from whenever sees the value (ENTER)';

my &cb;
sub mk { my $g = 44; sub f { $g }; &cb = -> { $g }; LEAVE { } }
mk;
is cb(), 44, 'a closure escaping a sub with LEAVE keeps the captured value';
