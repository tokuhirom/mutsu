# An attribute initializer runs in the package that wrote it, not the one being constructed

A typed attribute declaration carries the bare type name as its default
expression: `has ScopeHandle $!handle;` compiles to `default: BareWord("ScopeHandle")`,
because an uninitialized typed attribute reads back as its own type object.
mutsu evaluated that expression anchored on the class being **constructed** —
`current_package`, `constructing_class` and `current_unit` were all set from the
constructor's `class_key`. That is right only when the class being constructed
is also the one that wrote the `has`.

It usually is, so the gap stayed hidden. It opens as soon as the declaration is
inherited or composed and the constructing class lives in a different compunit:

```raku
# Base.rakumod
use Types;                 # exports `class ScopeHandle`
unit class Base;
has ScopeHandle $!handle;

# Child.rakumod -- never imports Types
use Base;
unit class Child is Base;
```

`Child.new` resolved `ScopeHandle` against `Child`'s package, found nothing, and
fell through bareword resolution's last resort to the plain string
`"ScopeHandle"` — which the attribute's own type check then rejected with
`Type check failed in assignment to $!handle; expected Types::ScopeHandle but
got Str ("ScopeHandle")`. Rakudo compiles the initializer in the declaring
package's lexical scope and has no such problem.

## Half one: carry the declaring package with the declaration

`ClassAttributeDef` now records `declaring_package`, stamped at registration by
the class-body, role-body and `augment` attribute paths. Role composition and
MRO collection both clone the whole def, so the scope rides along to every class
that inherits or composes the declaration for free.

`eval_attr_default_expr` takes that package instead of `class_key` for all three
anchors it sets, and falls back to `class_declaring_units[declaring_package]`
for the compunit. The existing `captured_env`/`captured_unit` pair, which only
ever covered role attributes with an explicit initializer, joins it in a single
`AttrDeclScope` argument shared by the four construction paths (`dispatch_new`,
`dispatch_bless`, the native default-constructor fast path, and the post-BUILD
deferred pass).

Both other readers of `constructing_class` wanted the declaring class all
along — `lexical_closure_package_sym` stamps it onto a closure a default builds,
and `resolve_suppressed_type` resolves a nested type name "within its owning
class". Neither is the constructed subclass.

## Half two: a role body's `use` has to land under the role

Anchoring correctly is not enough when the anchor's alias table is empty, which
is what [#8842](https://github.com/tokuhirom/mutsu/issues/8842) recorded as the
reason a first attempt at half one measured the right package and still
resolved nothing.

A role's body (and an `augment` body) is recorded at declaration time and re-run
at composition, from whatever compunit composes. Both runners already re-point
`current_package` at the declaring package around a `use` statement, for exactly
this reason — but neither of the two things a `use` writes actually reads
`current_package`:

* the importer package a module load keys `package_type_aliases` by comes from
  `unit_module_loading_stack`, which still names the composing compunit. A new
  `Interpreter::import_target_package` states the target explicitly, and both
  importer-package computations (first load and the already-loaded re-import
  copy) consult it first;
* `module_scope_lexicals` / `module_imported_lexical_names` are folded in once,
  when the importing compunit finishes loading. A role body's `use` runs *after*
  its own compunit finished, so its names were never folded in at all — which is
  why `has ScopeMode $!mode = ScopeOpaque;` could not see an imported enum value
  even with the package right. `record_deferred_body_imports` folds the names a
  deferred body imports into the declaring package's scope, additively, so the
  composing compunit still gets its own copy.

## Result

`Selkie::UI` 0.0.4's `t/04-data-visualization.rakutest` — the last non-parity
baseline file of that distribution — now passes 9/9 under mutsu, matching
rakudo. Its `Selkie::Widget` role declares `has NcplaneHandle $!plane;` and
`has AlphaMode $!applied-fg-alpha = AlphaOpaque;` and is composed into
`Selkie::Widget::BarChart`, inherited by `Selkie::Widget::Histogram`, and
constructed from `Selkie::UI::HistogramBuilder`'s own `.= new` attribute
default — four compunits deep, and exactly one of them imports the types.

Pinned by `t/modules/attr-default-declaring-package-scope.t`, which reproduces
the whole shape on eight small fixtures, including the plain-inheritance half
that needs no role at all.
