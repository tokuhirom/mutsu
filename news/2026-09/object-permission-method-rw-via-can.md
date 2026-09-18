# A Method reached via `.^can` on an attribute accessor now answers `.rw`

`.^can('name')` on an auto-generated attribute accessor (`has $.x is rw`)
built a bare `Routine` handle carrying no `is_rw` info, unlike
`.^find_method`/`.^lookup`, which build a full `Method` `Instance` with an
`rw` attribute. `.rw` on that handle died with "No such method 'rw' for
invocant of type 'Method'" even though `.WHAT` correctly reported
`(Method)`.

Found via the `ecosystem-dist-roulette` sweep (locked on
[#7884](https://github.com/tokuhirom/mutsu/issues/7884)) on the
`Object::Permission` distribution (v0.0.7): its custom `is
authorised-by(...)` trait fetches the target method with
`$package.^can($name)[0]` and immediately calls `.rw` on it to decide which
wrapper closure to install. Fixed by adding `"rw"`/`"readonly"` handling to
`dispatch_routine_method`, looking the accessor's own `is rw` up from the
class attribute table. Pinned with
`t/oo/attribute/classhow-can-attribute-accessor-rw.t`.

The `Object::Permission` ledger record moves from 2/6 to 2/6 parity files
(t/030-simple.t moves from `regression` to `partial`, its "definition with
traits compiles" assertion now passing) — `mutsu_assertions` rises from 3 to
4 of 31. It stays `partial` overall: the remaining four files are blocked on
two separate, unrelated architectural gaps filed as their own issues rather
than fixed here:

- [#8682](https://github.com/tokuhirom/mutsu/issues/8682) — `PROCESS::<$name> := ...`
  written inside a nested block/module/sub body does not survive scope exit
  (blocks t/020-basic.t, t/040-method.t, t/050-attribute.t, all of which
  install/read `$*AUTH-USER` this way).
- [#8683](https://github.com/tokuhirom/mutsu/issues/8683) — a class declared
  inside a sub body is not found by `::('Name')` from outside the sub
  (blocks t/030-simple.t's second assertion, via `isa-ok`'s internal
  `nqp::istype($var, $type.WHAT)` check).
