use Test;

# Registering a `class`/`role`/`grammar` must NOT write the type object into
# `$_`. It used to: `exec_register_class_op` / `exec_register_role_op` published
# the type through the topic, which doubles as the block-value channel. The
# topic that a declaration happens to sit inside was collateral damage —
# `for ^3 { class C { }; say $_ }` printed the type object three times instead
# of 0, 1, 2, plus an "uninitialized value ... in string context" warning per
# iteration.
#
# The block value still has to be the type object where Raku says it is, so both
# halves are checked here.

plan 12;

# --- the topic survives a declaration ---------------------------------------

{
    my @seen;
    for ^3 { class C1 { }; @seen.push($_) }
    is @seen.join(','), '0,1,2', 'a class declaration leaves the loop topic alone';
}

{
    my @seen;
    for ^3 { role R1 { }; @seen.push($_) }
    is @seen.join(','), '0,1,2', 'a role declaration leaves the loop topic alone';
}

{
    my @seen;
    for ^3 { grammar G1 { token TOP { \d+ } }; @seen.push($_) }
    is @seen.join(','), '0,1,2', 'a grammar declaration leaves the loop topic alone';
}

{
    my @seen;
    for ^3 { my $o = 42 but role :: { method z { 1 } }; @seen.push("$_/{$o.z}") }
    is @seen.join(','), '0/1,1/1,2/1',
        'an anonymous-role `but` mixin leaves the loop topic alone';
}

{
    $_ = 'kept';
    class C2 { }
    is $_, 'kept', 'a top-level class declaration leaves $_ alone';
    role R2 { }
    is $_, 'kept', 'a top-level role declaration leaves $_ alone';
}

{
    my @seen;
    given 'topic' { class C3 { }; @seen.push($_) }
    is @seen.join(','), 'topic', 'a declaration inside `given` leaves $_ alone';
}

# --- the declaration still yields the type object where it should ------------

{
    my $r = role :: { method z { 1 } };
    ok $r.^name.starts-with('<anon'), 'an anonymous role expression yields the role';
}

{
    my $c = class :: { method z { 5 } };
    is $c.new.z, 5, 'an anonymous class expression yields the class';
}

# A postfix on the same line as the closing brace applies to the type object,
# even inside a routine body where the declaration parses as a statement.
{
    sub make-it { class It { method p { 7 } }.new }
    is make-it().p, 7, 'class Name { ... }.new in a routine body builds an instance';
}

{
    my $t = EVAL 'unit class UCT is export; has $.x = 42;';
    is $t.new.x, 42, 'EVAL of a unit class compilation unit returns the class';
}

# A `.method` on the NEXT line is a new statement on the topic, not a postfix.
{
    my @seen;
    for ^2 {
        class C4 { }
        @seen.push($_);
    }
    is @seen.join(','), '0,1', 'a newline before the next statement keeps the topic';
}
