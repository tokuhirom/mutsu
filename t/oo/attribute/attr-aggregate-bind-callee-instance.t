use Test;

# `my @p := @!p` (and `%`) binds the running invocant's attribute, even while a
# caller frame -- a method of the same type on ANOTHER instance -- holds its
# own `my @p := @!p` binding. The source is read through the attribute slot
# (ADR-18356 D2); a by-name probe of the env chain answered the caller's
# binding instead, so the callee pushed into the caller's attribute (the
# Commands distribution's `extended-help-from-hash`, called from a command
# handler, built an empty help object). Each case is checked against rakudo.

plan 8;

# A punned role: the shape the Commands distribution has.
{
    my role R {
        has @.p;
        has %.h;
        method add($x) { my @p := @!p; @p.push($x); my %h := %!h; %h{$x} = 1 }
        method run(&cb) { my @p := @!p; my %h := %!h; cb() }
    }
    my $a = R.new;
    my $b = R.new;
    $a.run({ $b.add(1) });
    is-deeply $a.p, [], 'role: the caller keeps its own array';
    is-deeply $b.p, [1], 'role: the callee pushed into its own array';
    is-deeply $a.h, {}, 'role: the caller keeps its own hash';
    is-deeply $b.h, {1 => 1}, 'role: the callee stored into its own hash';
}

# A class.
{
    my class C {
        has @.p;
        method add($x) { my @p := @!p; @p.push($x) }
        method run(&cb) { my @p := @!p; cb() }
    }
    my $a = C.new;
    my $b = C.new;
    $a.run({ $b.add(2) });
    is-deeply ($a.p, $b.p), ([], [2]), 'class: each instance keeps its own array';
}

# The callee is constructed inside the caller's method, its TWEAK binding
# its attributes (the Commands constructor shape).
{
    my role Cmds {
        has @.names is built(False);
        method TWEAK(:$names!) { self.add($_) for $names<> }
        method add($n) { my @names := @!names; @names.push($n) unless $n (elem) @names }
        method help() { Cmds.new(names => @!names.map(*.uc).List) }
        method run(&cb) { my @names := @!names; cb() }
    }
    my $c = Cmds.new(names => <quit exit>);
    my $help;
    $c.run({ $help = $c.help });
    is-deeply $help.names, [<QUIT EXIT>], 'an object built in a callee binds its own attribute';
    is-deeply $c.names, [<quit exit>], 'and the caller instance is left alone';
}

# The binding still aliases the attribute.
{
    my class D {
        has @.p;
        method grow() { my @p := @!p; @p.push(1); @p.push(2) }
    }
    my $d = D.new;
    $d.grow;
    is-deeply $d.p, [1, 2], 'pushes through the bound name reach the attribute';
}
