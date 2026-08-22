use v6;
use Test;

# ADR-0039 §8 (the cross-thread-store axis of by-name container resolution).
#
# `@`/`%` have no per-binding closure cell, so the bare-name-keyed cross-thread
# store IS their sharing mechanism. Every spawn used to publish EVERY live
# container into it, including ones the spawned block could not possibly touch.
# The entry then outlived the frame that owned it, and any later frame with a
# same-named container resolved to it instead of to its own binding — a silent
# wrong value, deterministic, repeating on every call.
#
# The first block below is deliberately the FIRST thing in the file: the
# callee's `my @items` runs before the process's first spawn, so no
# `thread_redeclared_vars` mask exists at that moment. Nothing but "the spawn
# declines to publish a container its block never names" keeps it frame-local
# (ADR-0039 §8.4: no keying discipline on the store can fix this shape).
#
# The second half pins the sharing that must keep working: a container the
# block DOES name stays on the lane, with its atomicity intact.

plan 21;

# --- a callee's own `my @items` must not escape through the thread lane ------

sub work($tag) {
    my @items = ($tag,);
    await start { 1 };          # arms the lane; names no container
    @items.push("$tag-2");
    @items.join(",")
}

my @items = <x y z>;
is work('A'), "A,A-2", "callee's own \@items got both of its values";
is @items.join(","), "x,y,z", "caller's \@items is untouched by the callee's spawn";
@items.push('MINE');
is @items.join(","), "x,y,z,MINE", "caller's \@items still takes its own push";
is work('B'), "B,B-2", "a second call starts from a fresh \@items";
is @items.join(","), "x,y,z,MINE", "caller's \@items survives the second call";

sub hwork($tag) {
    my %items = (k => $tag);
    await start { 1 };
    %items{"$tag-2"} = 1;
    %items.keys.sort.join(",")
}

my %items = (x => 1);
is hwork('A'), "A-2,k", "callee's own %items got both keys";
is %items.keys.sort.join(","), "x", "caller's %items is untouched";
is hwork('B'), "B-2,k", "a second call starts from a fresh %items";
is %items.keys.sort.join(","), "x", "caller's %items survives the second call";

# --- a non-slurpy `@`/`%` PARAMETER must not escape its call -----------------
# `mask_thread_redeclared_params` deliberately never masks these (ADR-0039
# §8.3); they stay frame-local because the spawn does not publish them.

sub takes(@list is copy) { await start { 1 }; @list.push('R'); @list.join(",") }
my @list = <x y z>;
is takes(<p q>), "p,q,R", "the parameter's own copy got the push";
is @list.join(","), "x,y,z", "caller's \@list is untouched by the callee's parameter";

sub htakes(%h is copy) { await start { 1 }; %h<R> = 1; %h.keys.sort.join(",") }
my %h = (x => 1);
is htakes({p => 1}), "R,p", "the parameter's own copy got the key";
is %h.keys.sort.join(","), "x", "caller's %h is untouched by the callee's parameter";

# --- genuinely shared containers still work ----------------------------------

my @a;
await start { @a.push(1) };
is @a.join(","), "1", "a container the block NAMES is still shared with the parent";

my %sh;
await start { %sh<k> = 'v' };
is %sh<k>, 'v', "a hash the block NAMES is still shared with the parent";

my @shared;
await Promise.allof( (^4).map: -> $i { start { @shared.push($i) for ^25 } } );
is @shared.elems, 100, "concurrent pushes to a named container all land";

my @slots = 0 xx 4;
await Promise.allof( (^4).map: -> $i { start { @slots[$i] = $i * 2 } } );
is @slots.join(","), "0,2,4,6", "concurrent element assignment still writes through";

my @deep;
await start { await start { @deep.push('d') } };
is @deep.join(","), "d", "a nested spawn's named container reaches the outermost parent";

# Sibling spawns each declaring the same name stay isolated (ADR-0010).
my @res = await (^3).map: -> $n { start { my @w = ($n,); @w.push($n * 10); @w.join(",") } };
is @res.join("|"), "0,0|1,10|2,20", "sibling workers' same-named containers stay isolated";

# --- a container reached INDIRECTLY (never named by the block) still works ---
# The block names `&inner`, not `@acc`: the container is shared by container
# identity / the nested-named-sub cell, not by the name lane.

sub outer() {
    my @acc;
    sub inner($v) { @acc.push($v) }
    await start { inner('x') };
    @acc.join(",")
}
is outer(), "x", "a nested named sub's push from a worker still reaches its owner";

my @top;
sub topper($v) { @top.push($v) }
await start { topper('t') };
is @top.join(","), "t", "a mainline named sub's push from a worker still reaches \@top";

done-testing;
