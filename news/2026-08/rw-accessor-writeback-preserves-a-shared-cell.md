# An rw-accessor writeback no longer drops a shared container cell

`Cro::HTTP::RequestParser` parsed two pipelined requests but only ever delivered
the first: the parser's `$request` lexical, reassigned by its nested
`sub fresh-message`, was back to the *previous* object on the next `whenever`
invocation, so the second request's headers were appended to the first request's
object. Two `Content-Type` headers then made `$request.content-type` parse
`'text/plain,text/plain'`, which dies with `X::Cro::MediaType::Invalid` — that
exception escaped `emit $request`, so the second request never reached the tap.
(`Cro::MediaType.parse('text/plain,text/plain')` fails identically under real
`raku`, so that step was correct behaviour reached from a wrong state.)

## Root cause

A `supply` block's own `my` lexicals are promoted to shared `ContainerRef` cells
(`share_supply_block_lexicals`, added with the emitter-stamp work) so every
callback the block registers reads and writes ONE binding rather than a
per-callback snapshot.

`$request.method = @parts[0]` in the `whenever` body is an rw-accessor
assignment. It lowers to the `__mutsu_assign_method_lvalue` writeback, which
mutates the instance and then persists the result with `Env::insert` — and
`Env::insert` **replaces the binding**. That is right for `:=` and for a fresh
`my`, but wrong here: the name already denoted a shared cell, and replacing it
with a bare value silently un-shared every alias.

From then on the callback's env held a plain `$request`, so `fresh-message`'s
`$request = Cro::HTTP::Request.new` wrote a cell nobody read any more and the
next invocation saw the stale object. The instance mutation itself was never
lost — `write_back_sharing` commits into the instance's own shared attribute
cell — only the *variable rebinding* was destructive.

## Fix

`Env` gained `insert_through` / `insert_through_sym`: assign a **value** to a
name, writing *through* a `ContainerRef` cell when the name is bound to one, and
falling back to a plain `insert` otherwise. Every writeback in
`runtime/methods_mut_method_lvalue.rs` — the rw-accessor path plus its siblings
(`.substr-rw`, `.AT-KEY`/`.first` lvalues, pair `.value`, mixin rebuilds,
`handles` delegates) — now goes through it, since all of them assign to the
container a name already denotes rather than rebinding the name.

Pinned by `t/supply-block-lexical-survives-rw-accessor-write.t`, which covers
the nested-sub reassignment, an rw write from a nested sub read back in the
body, and a sibling `whenever` observing the write.

## Effect

Cro's `t/http-request-parser.rakutest` goes from 334 pass / 7 fail to
**340 pass / 4 fail** — the three "Two separate packages are parsed" tests and
their split-packet variants — and a mutsu Cro server can serve pipelined
requests on one connection.

## Reduced repro

The synthetic route had been declared exhausted in the original ticket; a shadow
bisect of the real `RequestParser.rakumod` narrowed the trigger to the single
line `$request.method = @parts[0]`, which reduces to twenty lines with no Cro at
all:

```raku
class Obj { has $.id; has $.tag is rw; my $c = 0; submethod TWEAK { $!id = ++$c } }
my $src = Supplier.new;
my @got;
supply {
    my $obj;
    my sub fresh() { $obj = Obj.new }
    fresh;
    whenever $src -> $v {
        $obj.tag = "t$v";        # <-- dropped the shared cell
        emit "v=$v obj={$obj.id}";
        fresh();
    }
}.tap(-> $x { @got.push($x) });
$src.emit(1);
$src.emit(2);
say @got.raku;   # raku: ["v=1 obj=1", "v=2 obj=2"];  mutsu was ["v=1 obj=1", "v=2 obj=1"]
```

## Still open

The escaping exception was reported as `X::AdHoc: X::Cro::MediaType::Invalid()`
— a typed exception flattened into `X::AdHoc` with the *type object's gist* as
its message. Caught directly the same exception keeps its class and message, so
the mangling is in the supply/quit propagation path, not in `die`. That defect is
untouched by this fix.
