use Test;

plan 14;

# A `my %h is CustomClass` where CustomClass composes Associative is backed by
# a blessed instance ("tied hash"), so `.^name`, subscripting, and coercion
# dispatch to the class's (possibly role-provided) methods.

role TinyAssoc does Associative {
    method AT-KEY($k)          is raw { %!store.AT-KEY($k) }
    method ASSIGN-KEY($k, \v)  is raw { %!store.ASSIGN-KEY($k, v) }
    method EXISTS-KEY($k)             { %!store.EXISTS-KEY($k) }
    method keys()                     { %!store.keys }
    method STORE(*@pairs) {
        for @pairs -> $p { self.ASSIGN-KEY($p.key, $p.value) }
        self
    }
    has %!store;
}

class TiedHash does TinyAssoc { }

# --- `.^name` reflects the tied class, not Hash --------------------------------
my %h is TiedHash;
is %h.^name, 'TiedHash', 'my %h is TiedHash backs the variable with the class';

# --- element assignment dispatches to ASSIGN-KEY / AT-KEY ----------------------
%h<x> = 42;
is %h<x>, 42, 'element assign+read round-trips through AT-KEY/ASSIGN-KEY';
%h<y> = 7;
is %h<y>, 7, 'a second element';
is %h.keys.sort.join(','), 'x,y', '.keys sees both keys';

# --- initializer flows through STORE, keeping the tied identity ---------------
my %g is TiedHash = (a => 1, b => 2, c => 3);
is %g.^name, 'TiedHash', 'initializer keeps the tied class';
is %g<b>, 2, 'initializer element present';
is %g.keys.elems, 3, 'initializer stored all pairs';

# --- element writes after the initializer keep dispatching to the class -------
%g<d> = 4;
is %g<d>, 4, 'post-initializer element write round-trips';

# --- `is raw` container-accessor method lvalue (`self.AT-KEY($k) = v`) --------
class Direct {
    has %!hash;
    method AT-KEY($key) is raw { %!hash.AT-KEY($key) }
    method put($k, $v) { self.AT-KEY($k) = $v }   # raw accessor lvalue
    method get($k)     { %!hash{$k} }
}
my $d = Direct.new;
$d.put('a', 100);
$d.put('b', 200);
is $d.get('a'), 100, 'raw-accessor lvalue writes through to the backing hash';
is $d.get('b'), 200, 'a second raw-accessor lvalue write';

# --- `%!attr.AT-KEY($k) = v` writes back to the instance attribute ------------
class InnerAttr {
    has %!m;
    method set($k, $v) { %!m.AT-KEY($k) = $v }
    method fetch($k)   { %!m{$k} }
}
my $ia = InnerAttr.new;
$ia.set('k', 5);
is $ia.fetch('k'), 5, 'attribute-hash AT-KEY lvalue persists';

# --- AT-KEY lvalue on a plain lexical hash still works ------------------------
my %lex;
%lex.AT-KEY('z') = 99;
is %lex<z>, 99, 'AT-KEY lvalue on a lexical hash';

# --- AT-POS lvalue on an attribute array -------------------------------------
class ArrAttr {
    has @!a;
    method set($i, $v) { @!a.AT-POS($i) = $v }
    method fetch($i)   { @!a[$i] }
}
my $aa = ArrAttr.new;
$aa.set(2, 'two');
is $aa.fetch(2), 'two', 'AT-POS lvalue on an attribute array persists';

is %h.keys.elems, 2, 'tied hash reflects stored element count';
