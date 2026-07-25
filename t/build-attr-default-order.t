use Test;

plan 15;

# Raku runs a user `submethod BUILD` FIRST and only then applies each
# `has $.x = <default>` initializer, and only to attributes BUILD did not set.

# --- ordering: BUILD before the initializers -------------------------------
{
    my @log;
    class Ordered {
        has $.a = { @log.push('default-a'); 1 }();
        has $.b = { @log.push('default-b'); 2 }();
        submethod BUILD(:$!a = 9) { @log.push('build') }
    }
    my $o = Ordered.new;
    is @log.join(','), 'build,default-b',
        'BUILD runs before the initializers, and the set attribute skips its own';
    is $o.a, 9, 'BUILD owns the attribute it set';
    is $o.b, 2, 'the untouched attribute still gets its initializer';
}

# --- an initializer reading a sibling sees BUILD's value -------------------
{
    class Sibling {
        has $.x = 100;
        has $!y = $!x;
        submethod BUILD(:$!x = 200) { }
        method y { $!y }
    }
    my $s = Sibling.new;
    is $s.x, 200, 'BUILD set $!x';
    is $s.y, 200, 'the $!y initializer read the post-BUILD $!x';
    is Sibling.new(x => 5).y, 5,
        'a passed named arg reaches BUILD, and the initializer sees that';
}

# --- an explicit undefined write still counts as "BUILD set it" ------------
{
    class WroteAny {
        has $.a = 5;
        submethod BUILD { $!a = Any }
    }
    is WroteAny.new.a.^name, 'Any',
        'assigning Any in BUILD suppresses the initializer';
}

# --- an attributive parameter that was never passed also counts ------------
{
    class Bound {
        has $.a = 5;
        submethod BUILD(:$!a) { }
    }
    is Bound.new.a.^name, 'Any', ':$!a binds even unpassed, so no initializer';
    is Bound.new(a => 9).a, 9, ':$!a binds the passed value';
}

# --- an in-place container mutation counts too -----------------------------
{
    class Pushed {
        has @.xs = 1, 2, 3;
        submethod BUILD { @!xs.push(9) }
    }
    is Pushed.new.xs.join(','), '9',
        'vivifying @!xs in BUILD suppresses its initializer';
}

# --- without a BUILD nothing changes ---------------------------------------
{
    class Plain {
        has $.a = 5;
        has $.b = $!a * 2;
    }
    my $p = Plain.new;
    is $p.b, 10, 'without BUILD the initializers still chain';
    is Plain.new(a => 7).a, 7, 'without BUILD a named arg still lands';
}

# --- the same ordering through an explicit bless ---------------------------
{
    class Blessed {
        has $.x = 100;
        has $!y = $!x;
        submethod BUILD(:$!x = 200) { }
        method y { $!y }
        method new(|c) { self.bless(|c) }
    }
    is Blessed.new.y, 200, 'bless applies the initializers after BUILD too';
}

# --- and with a role-composed BUILD ----------------------------------------
{
    role Timed { has $.t; submethod BUILD(:$!t = 7) { } }
    class Event does Timed {
        has $.u = $!t * 3;
    }
    is Event.new.t, 7, 'the role BUILD set $!t';
    is Event.new.u, 21, 'the initializer saw the role BUILD value';
}
