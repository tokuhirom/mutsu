use Test;
plan 3;

# §B: a role method reached via a `does`/`but` mixin now runs through the shared
# compiled-or-treewalk helper (was run_instance_method_resolved directly).
{
    role Greet { method hi { "hi" } }
    my $x = 42 but Greet;
    is $x.hi, "hi", 'mixed-in role method runs (compiled helper)';
}

# Role method that reads the object's value.
{
    role Describe { method describe { "val=" ~ self.Int } }
    my $n = 7 but Describe;
    is $n.describe, "val=7", 'mixed-in role method reads invocant';
}

# `does` mixin with a role method that reads an attribute.
{
    class Box { has $.v }
    role Tag { method tagged { "tag:" ~ self.v } }
    my $b = Box.new(v => 3);
    $b does Tag;
    is $b.tagged, "tag:3", 'does-mixin role method reads attribute';
}
