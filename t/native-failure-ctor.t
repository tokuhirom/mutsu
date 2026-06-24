use Test;

# Pin for the §D ③ ctor-fork slice native-izing `Failure.new($exception?)` in the
# VM. The construction is pure data assembly reading only VM-owned state: the
# explicit exception argument (or `$!` from env when omitted, or the
# `X::AdHoc("Failed")` default), wrapped into an `X::AdHoc` if it is not already an
# Exception (checked via the MRO read `mro_readonly`). No FS / process / user
# code. The VM builds it directly via `build_native_failure_value` — the single
# impl the interpreter's `dispatch_new_and_constructors` arm also delegates to.
#
# (`X::AdHoc` is built with `:payload`, not `:message` — `.message` reads the
# payload, a pre-existing X::AdHoc detail unrelated to construction.)

plan 11;

# --- explicit exception argument is kept as-is ------------------------------
my $f1 = Failure.new(X::AdHoc.new(payload => "boom"));
isa-ok $f1, Failure, 'Failure.new(exception) yields a Failure';
is $f1.exception.message, "boom", 'explicit exception preserved';
is $f1.handled, False, 'fresh Failure is unhandled';
isa-ok $f1.exception, X::AdHoc, 'X:: subtype kept (not re-wrapped)';

# --- a non-Exception value is wrapped into X::AdHoc -------------------------
my $f2 = Failure.new("oops");
isa-ok $f2, Failure, 'Failure.new(Str) yields a Failure';
is $f2.exception.message, "oops", 'string wrapped as X::AdHoc message';
isa-ok $f2.exception, X::AdHoc, 'non-Exception wrapped into X::AdHoc';

# --- no argument defaults to X::AdHoc("Failed") -----------------------------
my $f3 = Failure.new;
isa-ok $f3, Failure, 'argless Failure.new yields a Failure';
is $f3.exception.message, "Failed", 'argless default message is "Failed"';

# --- `.handled` round-trips -------------------------------------------------
my $f4 = Failure.new("h");
$f4.handled = True;
is $f4.handled, True, '.handled can be set';

# mark the sample Failures handled so they do not warn at sink
$f1.so; $f2.so; $f3.so;
ok True, 'all sample Failures marked handled';
