use Test;

plan 14;

# A container tie declared with `is <Type>` must survive a *whole-value*
# assignment (`%!C = ...`) and must work when the type named is a **role**,
# which raku puns. Two gaps this pins:
#
#   * an attribute's local slot is only seeded by a *read*, so `%!C = ...` with
#     no prior read of `%!C` used to see an empty slot and clobber the tie with
#     a plain Hash — inserting any read of `%!C` first made it disappear;
#   * a punned role is a `Mixin`, and both the tie gate and the `STORE`
#     re-assignment path matched `Instance` only, so every role-typed tie was
#     skipped.
#
# This is the shape `DBDish::mysql::Connection` uses:
# `has %.Converter is DBDish::TypeConverter`, populated by a bare `%!Converter =`
# in BUILD and later read back as `my %C := $conn.Converter; %C.convert-function(...)`.

role TinyAssoc does Associative {
    has %!store;
    has $.stores = 0;
    method AT-KEY($k)         is raw { %!store.AT-KEY($k) }
    method ASSIGN-KEY($k, \v) is raw { %!store.ASSIGN-KEY($k, v) }
    method keys()                    { %!store.keys }
    method tag()                     { 'tiny' }
    method STORE(*@pairs) {
        $!stores++;
        for @pairs -> $p { self.ASSIGN-KEY($p.key, $p.value) }
        self
    }
}
class TiedHash does TinyAssoc { }

# --- 1. a lexical tied by a bare role (raku puns it) -------------------------

my %r is TinyAssoc;
is %r.^name, 'TinyAssoc', 'my %h is <Role> puns the role and ties the variable';
%r = (a => 1, b => 2);
is %r.^name, 'TinyAssoc', 'assigning to a role-tied lexical keeps the punned role';
is %r.keys.sort.join(','), 'a,b', 'the assignment went through the role STORE';
is %r.tag, 'tiny', 'a role method still dispatches after the assignment';

# --- 2. a class-typed attribute assigned whole, with no prior read -----------

class HolderC {
    has %.C is TiedHash;
    submethod BUILD() { %!C = (a => 1, b => 2) }
    method poke()     { %!C = (p => 9) }
}
my $hc = HolderC.new;
is $hc.C.^name, 'TiedHash', 'a whole-value assign in BUILD keeps the tied class';
is $hc.C.keys.sort.join(','), 'a,b', 'BUILD assignment stored through STORE';
$hc.poke;
is $hc.C.^name, 'TiedHash', 'a later whole-value assign in a method keeps the tie';
is $hc.C<p>, 9, 'the method assignment stored through STORE';

# --- 3. a role-typed attribute (the DBIish shape) ---------------------------

class HolderR {
    has %.C is TinyAssoc;
    submethod BUILD() { %!C = (x => 1) }
}
my $hr = HolderR.new;
is $hr.C.^name, 'TinyAssoc', 'a role-typed attribute keeps the punned role across assignment';
is $hr.C.tag, 'tiny', 'a role method dispatches on the attribute';
is $hr.C<x>, 1, 'the role STORE populated the attribute';

# Bound to a `%` variable and used through a role method, as DBDish does.
my %bound := $hr.C;
is %bound.^name, 'TinyAssoc', 'binding the attribute to a % variable keeps the role';
is %bound.tag, 'tiny', 'the role method is reachable through the bound variable';

# --- 4. an untied attribute is untouched ------------------------------------

class Plain {
    has %.h;
    submethod BUILD() { %!h = (k => 1) }
}
is Plain.new.h.^name, 'Hash', 'an ordinary % attribute is still a plain Hash';
