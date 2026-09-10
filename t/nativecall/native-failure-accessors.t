use v6;
use Test;

# `.exception` (the stored exception object) and `.handled` (read of the global
# handled flag) on a `Failure` are in the interpreter's no-explode "safe" list,
# so they dispatch natively via `native_method_0arg` instead of routing through
# the interpreter. Crucially, a *non*-safe method on an unhandled Failure (e.g.
# `.succ`/`.Int`) still returns `None` from the native layer and reaches the
# interpreter, which explodes the Failure — so the native accessors do not
# weaken Failure's defining behavior. Byte-identical to the interpreter.

plan 10;

# --- .handled read (unhandled -> False; does not explode) ---
my $f = Failure.new("oops");
is $f.handled, False, '.handled on an unhandled Failure is False (no explosion)';

# --- .exception read returns the stored exception (does not explode) ---
is $f.exception.message, 'oops', '.exception returns the stored exception';
is $f.exception.WHAT.^name, 'X::AdHoc', '.exception is an X::AdHoc';

# --- mark handled, then .handled reads True ---
$f.handled = True;
is $f.handled, True, '.handled reflects the global handled flag after setting';
is $f.exception.message, 'oops', '.exception still works after handling';

# --- a non-safe method on an unhandled Failure still explodes ---
my $g = Failure.new("boom");
my $lived = (try { $g.Int; True }) // False;
is $lived, False, 'a non-safe method (.Int) on an unhandled Failure explodes';

my $h = Failure.new("bang");
my $lived2 = (try { $h.succ; True }) // False;
is $lived2, False, 'a non-safe method (.succ) on an unhandled Failure explodes';

# --- a handled Failure does NOT explode on a non-safe method ---
my $k = Failure.new("ok");
$k.handled = True;
my $lived3 = (try { $k.defined; True }) // False;
is $lived3, True, 'a handled Failure does not explode';

# --- .exception/.handled on a handled Failure ---
is $k.handled, True, '.handled True on a handled Failure';
is $k.exception.message, 'ok', '.exception on a handled Failure';
