use Test;
plan 2;

# `Supply.do($cb)`'s callback used to run only for values an on-demand
# (`supply { ... }`) source's body emits *synchronously*. A value the same
# source delivers *asynchronously* — through a nested `whenever` chain
# registering a live tap on the body's emitter, the shape of essentially
# every non-trivial `supply` block — reached the final subscriber untouched,
# `$cb` never having run: the value-loop that fires `do_callbacks`
# (`native_supply_mut_methods.rs`) only ever saw the synchronous
# `plain_values`, while the async delivery path registered the real
# subscriber's callback directly on the emitter, bypassing `do_callbacks`
# entirely.
#
# This is what still left the vendored Cro suite's `http-auth-basic.rakutest`
# at 3/5 passing after the on-demand dead-end fix (see
# `t/supply-do-on-demand-source.t`): `Cro::HTTP::Auth::Basic.process-responses`
# adds the `WWW-Authenticate` header inside a `.do($cb)` callback, but the
# response it needs to tag is always delivered through Cro's `whenever`-driven
# pipeline, never a synchronous `emit` — so the header never made it onto the
# 401 response. `http-auth-basic.rakutest` is 5/5 with this fix.

my $inner = supply { whenever Promise.in(0).then({ 1 }) -> $v { emit $v } };
my @seen;
my @tapped;
$inner.do({ @seen.push($_) }).tap({ @tapped.push($_) });
sleep 0.3;
is @seen, (1,), 'do callback fires for a value delivered asynchronously through a nested whenever';
is @tapped, (1,), 'the derived Supply still delivers the async value to its own tap';
