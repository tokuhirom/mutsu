use Test;

plan 7;

class T {
    has $.a;

    method l1 is rw {
        return-rw $!a;
    }

    method l2 is rw {
        $!a;
    }
}

my $o = T.new(:a<z>);
is $o.l1, "z", "rw method with return-rw works as rvalue";

lives-ok { $o.l1 = 5 }, "can assign through rw method";
is $o.a, 5, "assignment through rw method updates attribute";

my $seen = Nil;
lives-ok { temp $o.l2 = 9; $seen = $o.a }, "temp assignment through rw method lives";
is $seen, 9, "temp assignment value is visible in scope";
is $o.a, 5, "temp assignment restores after scope";
is $o.l2, 5, "rw method returns restored value";
