# Connection-manager per-connection pipelines treat live supplies as cold (Cro::Core composer.rakutest 87-88 + trailing hang)

With #5605/#5608/#5611 and the closure-self/parent-tier fix in, Cro::Core's
`t/composer.rakutest` reaches 86/88, failing only the connection-manager
message-flow tests and hanging after the last one (exit 124).

Scenario (composer.rakutest ~line 470-520): `Cro.compose($conn-source,
TestUppercaseTransform)` produces a service whose connection manager builds a
per-connection pipeline: `connection.incoming` (a live `Supplier`-backed
supply) → transform (`supply { whenever $input { emit ... } }`) → the
connection's replier sink (`whenever $input { $!replier.emit(.body);
LAST $!replier.emit('(closed)') }`).

Observed: `$response-channel-a.receive` gets `'(closed)'` instead of `'BBQ'`
— the replier sink's `whenever` saw ZERO messages and its LAST phaser fired
immediately. The message is emitted into the connection's `$!send` Supplier
*after* the pipeline is assembled, so the pipeline must stay LIVE; instead
the per-connection pipeline assembly (which happens inside the connection
manager's own `whenever` body, i.e. inside a running supply callback) takes
a cold/finite replay path: 0 buffered values → done → LAST.

The trailing process hang after test 86 (before these fixes: after 88) is
likely the same machinery: a per-connection driver waiting on something that
never completes.

Where to look: the supply-block tap path's marker handling in
`native_supply_mut_methods.rs` (live supplier-backed sources ARE handled when
the tap happens at "top level", so the difference is assembling/tapping from
within a whenever callback — `supply_emit_buffer` frames / `react_active`
state at that moment), and `run_whenever_with_value`'s react-mode vs
non-react-mode branches.

Repro: run the vendored suite `tmp`-extract of Cro::Core:
`target/debug/mutsu -I lib t/composer.rakutest` (tests 87-88).
