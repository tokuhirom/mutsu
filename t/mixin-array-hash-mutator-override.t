use v6;
use Test;

# A `does`-mixed-in role method overriding a builtin Array/Hash mutator
# (push/append/unshift/pop/shift/splice on Array, push/append on Hash) must
# win over the native fast path, on both a static receiver (`@a.push(...)`)
# and a dynamic-name receiver (`@a."$name"(...)`). ADR-0019 E6c: the shared
# mut slow path (`call_method_mut_with_values`) special-cased these methods
# purely by sigil (`@`/`%`) with no check that the value behind the sigil is
# actually a plain Array/Hash and not a `does`-mixed Mixin — so a role's own
# `push` was silently shadowed by the native array/hash mutator. The `.push`
# ArrayPush fast opcode already had this guard (`is_simple_array`, ADR-0019
# E6d); this pins the same guard on the shared slow path every other mut
# entry point (`CallMethodMut`'s non-opcode methods, `CallMethodDynamicMut`)
# falls through to.

role Loud {
    method push($x)      { say "ROLE-PUSH: $x"; self }
    method unshift($x)   { say "ROLE-UNSHIFT: $x"; self }
    method append(*@x)   { say "ROLE-APPEND"; self }
    method prepend(*@x)  { say "ROLE-PREPEND"; self }
    method pop()         { say "ROLE-POP"; self }
    method shift()       { say "ROLE-SHIFT"; self }
}

role LoudHash {
    method push(*@x)   { say "ROLE-HPUSH"; self }
    method append(*@x) { say "ROLE-HAPPEND"; self }
}

plan 8;

my @a = (1, 2, 3);
@a does Loud;
@a.push(9);
is @a, (1, 2, 3), 'role .push wins over native Array.push (static mut)';

my @b = (1, 2, 3);
@b does Loud;
@b.unshift(9);
is @b, (1, 2, 3), 'role .unshift wins over native Array.unshift (static mut)';

my @c = (1, 2, 3);
@c does Loud;
@c.append(9);
is @c, (1, 2, 3), 'role .append wins over native Array.append (static mut)';

my @d = (1, 2, 3);
@d does Loud;
my $name = "push";
@d."$name"(9);
is @d, (1, 2, 3), 'role .push wins over native Array.push (dynamic-name mut)';

my @e = (1, 2, 3);
@e does Loud;
my $name2 = "unshift";
@e."$name2"(9);
is @e, (1, 2, 3), 'role .unshift wins over native Array.unshift (dynamic-name mut)';

my %h = (a => 1);
%h does LoudHash;
%h.push((b => 2));
is %h, { a => 1 }, 'role .push wins over native Hash.push (static mut)';

my %h2 = (a => 1);
%h2 does LoudHash;
my $name3 = "push";
%h2."$name3"((b => 2));
is %h2, { a => 1 }, 'role .push wins over native Hash.push (dynamic-name mut)';

# Plain (non-mixed) arrays/hashes still use the native fast path.
my @plain = (1, 2, 3);
@plain.push(4);
is @plain, (1, 2, 3, 4), 'plain Array.push still runs natively (no mixin)';

done-testing;
