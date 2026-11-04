use Test;

plan 8;

# A user subclass of Metamodel::SubsetHOW whose `new_type` delegates with
# `callwith` / `nextwith` / the qualified native call mints a real subset.
class H1 is Metamodel::SubsetHOW {
    method new_type(:$name = "<anon>") {
        callwith :$name, :refinee(Any), :refinement{ $_ > 2 }
    }
}
my $t1 = H1.new_type(:name<V1>);
ok !(1 ~~ $t1), 'callwith: refinement is kept (1 fails)';
ok 5 ~~ $t1, 'callwith: refinement is kept (5 passes)';
is $t1.^refinee.raku, 'Any', 'callwith: .^refinee answers';

class H2 is Metamodel::SubsetHOW {
    method new_type(:$name = "<anon>") {
        nextwith :$name, :refinee(Any), :refinement{ $_ > 2 }
    }
}
my $t2 = H2.new_type(:name<V2>);
ok !(1 ~~ $t2) && (5 ~~ $t2), 'nextwith: refinement is kept';

class H3 is Metamodel::SubsetHOW {
    method new_type(:$name = "<anon>") {
        self.Metamodel::SubsetHOW::new_type(:$name, :refinee(Any), :refinement{ $_ > 2 })
    }
}
my $t3 = H3.new_type(:name<V3>);
ok !(1 ~~ $t3), 'qualified: does not recurse, refinement kept (1 fails)';
ok 5 ~~ $t3, 'qualified: refinement kept (5 passes)';
is $t3.^refinee.raku, 'Any', 'qualified: .^refinee answers';
is $t3.^name, 'V3', 'qualified: name';
