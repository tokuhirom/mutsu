use Test;

plan 8;

{
    my $called = 0;
    role R {
        has Str:D $.attr is required;

        multi method new(Int:D $n) {
            $called++;
            self.new(attr => $n.Str)
        }
    }

    my $r = R.new(42);
    is $called, 1, 'a punned role runs its user-defined new';
    is $r.attr, '42', 'a punned role new may delegate to the default constructor';
    ok $r ~~ R, 'the constructed object still does the role';
}

{
    role R {
        has $.attr;

        method new(Int:D $n) {
            self.bless(attr => "value $n")
        }
    }

    my $r = R.new(7);
    is $r.attr, 'value 7', 'self.bless works in a punned role new';
    ok $r ~~ R, 'a self.bless result retains the punned role';
}

{
    role R {
        has $.first;
        has $.second;
    }

    throws-like { R.new(10, 20) }, X::Constructor::Positional,
        'a punned role default constructor rejects positional arguments';
    is R.new(first => 10, second => 20).first, 10,
        'a punned role default constructor accepts named attributes';
    is R.new(first => 10, second => 20).second, 20,
        'all named attributes reach the punned class constructor';
}
