use Test;

# An `is rw` method whose body is a bare `$!attr` IS that attribute's accessor,
# so `$obj.method = v` must behave exactly like `$obj.attr = v`: the same `Nil`
# -> default reset, and the same type check. The two stores used to disagree —
# the method store applied only the `is default(...)` half and no type check at
# all, and the accessor store forgot that an *untyped* scalar attribute resets
# to `Any`.

plan 18;

class A { }

class U {
    has A   $.typed    is rw;
    has     $.untyped  is rw;
    has A   $.defaults is rw is default(A);
    has Int $.int      is rw;
    has A   $!private;
    method typed-m    is rw { $!typed }
    method untyped-m  is rw { $!untyped }
    method defaults-m is rw { $!defaults }
    method int-m      is rw { $!int }
    method private-m  is rw { $!private }
}

my $u = U.new;

# --- Nil restores the declared type object --------------------------------
$u.typed = A.new;
$u.typed = Nil;
is $u.typed.^name, 'A', 'accessor: Nil restores the declared type object';

$u.typed-m = A.new;
$u.typed-m = Nil;
is $u.typed.^name, 'A', 'rw method: Nil restores the declared type object';

# --- an untyped scalar attribute resets to Any ----------------------------
$u.untyped = 5;
$u.untyped = Nil;
is $u.untyped.^name, 'Any', 'accessor: Nil on an untyped attribute gives Any';

$u.untyped-m = 5;
$u.untyped-m = Nil;
is $u.untyped.^name, 'Any', 'rw method: Nil on an untyped attribute gives Any';

# --- `is default(...)` wins over the type object --------------------------
$u.defaults = A.new;
$u.defaults = Nil;
is $u.defaults.^name, 'A', 'accessor: is default(...) is honoured';
$u.defaults-m = A.new;
$u.defaults-m = Nil;
is $u.defaults.^name, 'A', 'rw method: is default(...) is honoured';

# --- a native-ish typed attribute -----------------------------------------
$u.int = 7;
$u.int = Nil;
is $u.int.^name, 'Int', 'accessor: Nil on Int gives the Int type object';
$u.int-m = 7;
$u.int-m = Nil;
is $u.int.^name, 'Int', 'rw method: Nil on Int gives the Int type object';

# --- a private attribute exposed by an rw method --------------------------
$u.private-m = A.new;
is $u.private-m.^name, 'A', 'rw method over a private attribute stores';
$u.private-m = Nil;
is $u.private-m.^name, 'A', 'rw method over a private attribute resets to A';

# --- the type check applies through both spellings ------------------------
dies-ok { $u.typed   = Any },   'accessor rejects a value of the wrong type';
dies-ok { $u.typed-m = Any },   'rw method rejects a value of the wrong type';
dies-ok { $u.int     = "str" }, 'accessor rejects a Str for an Int attribute';
dies-ok { $u.int-m   = "str" }, 'rw method rejects a Str for an Int attribute';
is $u.int.^name, 'Int', 'the rejected stores left the attribute alone';

# --- a matching value still stores through both ---------------------------
$u.int = 3;
is $u.int, 3, 'accessor stores a matching value';
$u.int-m = 4;
is $u.int, 4, 'rw method stores a matching value';

# --- an @-sigil attribute exposed by an rw method -------------------------
class Q { has @.list is rw; method list-m is rw { @!list } }
my $q = Q.new(list => [1, 2, 3]);
$q.list-m = (4, 5);
is-deeply $q.list, [4, 5], 'rw method over an @ attribute list-assigns';

# vim: ft=raku
