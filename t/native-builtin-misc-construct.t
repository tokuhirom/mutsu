use v6;
use Test;

# VM-native construction extended to the remaining pure-data built-in `.new`
# constructors: `Version`, `Duration` (the one fallible builder — a bad string
# is X::Str::Numeric), `StrDistance`, `Stash`, and the empty-instance handles
# (`Tap`, `Cancellation`, `ThreadPoolScheduler`, `CurrentThreadScheduler`).
# All route through the shared `try_native_builtin_construct` entry; the
# interpreter's `dispatch_new` arms call the same helpers, keeping them
# byte-identical.

plan 16;

# --- Version ---
is Version.new("1.2.3").Str, '1.2.3', 'Version.new from a string';
is Version.new(v1.2).Str, '1.2', 'Version.new from a version literal';
is Version.new("1.2.3").WHAT.^name, 'Version', 'Version.new produces a Version';

# --- Duration ---
is Duration.new(3.5).Int, 3, 'Duration.new from a Rat-ish number';
is Duration.new(10).Numeric, 10, 'Duration.new from an Int';
is Duration.new("2.5").Numeric, 2.5, 'Duration.new from a numeric string';
dies-ok { Duration.new("notanum") }, 'Duration.new from a bad string dies';
throws-like { Duration.new("xyz") }, X::Str::Numeric,
    'Duration.new bad string is X::Str::Numeric';

# --- StrDistance ---
my $sd = StrDistance.new(before => "cat", after => "cot");
is $sd.before, 'cat', 'StrDistance before';
is $sd.after, 'cot', 'StrDistance after';

# --- Stash ---
is Stash.new.^name, 'Stash', 'Stash.new produces a Stash';

# --- empty-instance handles ---
is Tap.new.^name, 'Tap', 'Tap.new';
is Cancellation.new.^name, 'Cancellation', 'Cancellation.new';
is ThreadPoolScheduler.new.^name, 'ThreadPoolScheduler', 'ThreadPoolScheduler.new';
is CurrentThreadScheduler.new.^name, 'CurrentThreadScheduler',
    'CurrentThreadScheduler.new';

# --- Version round-trips through a comparison still works ---
ok Version.new("1.2.0") eqv v1.2.0, 'Version.new is eqv to a literal';
