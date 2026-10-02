# A `whenever` on a `supply { }` block fires LAST when the supply is done

In `react { whenever $s { ...; LAST { ... } } }` where `$s` is
`supply { whenever $live { emit ... } }`, the top-level on-demand branch of
`build_react_subscriptions` fired the outer LAST phasers as soon as the supply
body returned. The body only registers the inner live `whenever`, so LAST
ran at subscription time, before any value arrived. A `LAST done` then ended
the react with nothing received. Rakudo fires LAST after the inner source
completes.

The branch now does what the nested-stage path
(`register_nested_on_demand_source`) already did. The inner live subscription
is tagged with the supply's emitter, so its completion resolves the supply's
done promise, and the outer LAST phasers move to the shadow subscription that
polls that promise.

This came up while looking for needlessly slow `t/` files.
`t/concurrency/supply/whenever-user-supply-coercion.t` took 30 s because each
of its three reacts waited out a 10 s `Promise.in` safety timer. With
`LAST done` on the whenever (which the bug used to break) it takes 0.7 s.
