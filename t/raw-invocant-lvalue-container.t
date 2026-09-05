use MONKEY-TYPING;
use Test;

# ADR-0067 slice 3a: a routine hands back the container it was *given*, and the
# invocant is parameter zero. `$a.m = 5` writes through `$a`'s own container
# when — and only when — the callee declares BOTH halves:
#
#   * the invocant parameter is raw (`\S:`, `$s is raw:`, `$s is rw:`), and
#   * the routine is rw-capable (`is rw` / `is raw` / spells `return-rw`).
#
# Dropping either half must keep refusing, which is what the regression
# controls below pin. Verified byte-identical under `mutsu` and `raku`.

plan 21;

my @mutsu-raw-seen;

augment class Any {
    # rw-capable AND raw invocant, in all three rw-capable spellings.
    method mutsuRawInv(\S:) is raw { S }
    method mutsuRwInv(\S:) is rw { S }
    method mutsuRetRwInv(\S:) { return-rw S }
    # a raw invocant spelled with a sigil rather than sigil-less.
    method mutsuSigilRaw($s is raw:) is raw { $s }
    method mutsuSigilRw($s is rw:) is raw { $s }
    # regression controls.
    method mutsuNotRwCapable(\S:) { S }
    method mutsuNotRawInvocant(Any:D $s:) is raw { $s }
    # a raw invocant that hands back something that is not a location.
    method mutsuRawButConst(\S:) is raw { 99 }
    # a raw-invocant routine that also READS its invocant before handing it back.
    method mutsuPeek(\S:) is raw { @mutsu-raw-seen.push(S); S }
}

# --- the three rw-capable spellings, over a scalar --------------------------

{
    my $a = 42;
    $a.mutsuRawInv = 5;
    is $a, 5, 'is raw + raw invocant writes through the scalar';
}
{
    my $a = 42;
    $a.mutsuRwInv = 5;
    is $a, 5, 'is rw + raw invocant writes through the scalar';
}
{
    my $a = 42;
    $a.mutsuRetRwInv = 5;
    is $a, 5, 'return-rw + raw invocant writes through the scalar';
}

# --- the sigiled raw-invocant spellings -------------------------------------

{
    my $a = 42;
    $a.mutsuSigilRaw = 5;
    is $a, 5, '$s is raw: invocant writes through the scalar';
}
{
    my $a = 42;
    $a.mutsuSigilRw = 5;
    is $a, 5, '$s is rw: invocant writes through the scalar';
}

# --- both halves are required -----------------------------------------------

{
    my $a = 42;
    dies-ok { $a.mutsuNotRwCapable = 5 },
        'a raw invocant without is raw/is rw/return-rw still refuses';
    is $a, 42, 'and the refused assignment left the variable alone';
}
{
    my $a = 42;
    dies-ok { $a.mutsuNotRawInvocant = 5 },
        'is raw without a raw invocant still refuses';
    is $a, 42, 'and the refused assignment left the variable alone (2)';
}
{
    my $a = 42;
    dies-ok { $a.mutsuRawButConst = 5 },
        'a raw-invocant routine that returns a value, not a location, refuses';
    is $a, 42, 'and the refused assignment left the variable alone (3)';
}

# --- the element invocant spellings -----------------------------------------
#
# `@a[0].m = 9` compiles to a copy-in/copy-out protocol through a compiler
# temp, so boxing the TEMP is what makes the write reach the element.

{
    my @a = 1, 2;
    @a[0].mutsuRawInv = 9;
    is-deeply @a, [9, 2], 'an array-element invocant writes back into the element';
}
{
    my %h = a => 1, b => 2;
    %h<a>.mutsuRawInv = 9;
    is %h<a>, 9, 'a hash-element invocant writes back into the entry';
    is %h<b>, 2, 'and leaves its neighbour alone';
}

# --- an instance-valued scalar ----------------------------------------------
#
# The raw invocant is the VARIABLE's container, so the assignment replaces its
# whole contents rather than touching an attribute.

{
    class MutsuRawC { method m(\S:) is raw { S } }
    my $c = MutsuRawC.new;
    $c.m = 5;
    is $c, 5, 'an instance invocant: the write replaces the whole variable';
}

# --- the routine still receives the invocant as a readable value ------------

{
    my $d = 41;
    $d.mutsuPeek = 8;
    is @mutsu-raw-seen[0], 41, 'the routine observed the pre-assignment invocant';
    is $d, 8, 'and the assignment still landed';
}

# --- an ordinary (non-lvalue) call is unaffected ----------------------------

{
    my $a = 42;
    is $a.mutsuRawInv, 42, 'an rvalue call still just returns the invocant';
    is $a, 42, 'and does not disturb the variable';
    ok $a.mutsuRawInv =:= $a, 'the raw invocant is the same container';
}

# --- a non-raw-invocant method on a class is untouched ----------------------

{
    class MutsuRawE { has $.v is rw }
    my $e = MutsuRawE.new(v => 1);
    $e.v = 7;
    is $e.v, 7, 'an ordinary is rw attribute accessor still assigns normally';
}

done-testing;
