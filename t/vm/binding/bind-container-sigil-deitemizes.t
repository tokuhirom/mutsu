use Test;

plan 18;

# Binding a container-sigil name -- an `@`/`%` signature parameter, or a `:=`
# target -- hands it the Positional/Associative ITSELF, never an itemized
# holder of one. A real Array/Hash stores each element in its own container, so
# a nested aggregate reads back as `$[...]` / `${...}`; rakudo's binder
# decontainerizes, mutsu's did not for `%` (and for `@` only by accident, when
# the parameter name happened not to resolve to a local slot).
#
# The visible consequence: an itemized `%v` is ONE item in list context, so
# `for %v { }` iterated the whole hash as a single element and `$_.key` died
# with "No such method 'key' for invocant of type 'Hash'" --
# https://github.com/tokuhirom/mutsu/issues/9006, found in Graph's
# `vertex-component`.

my %adj = a => { b => 1, c => 2 }, b => { a => 1 };

# The element itself is itemized: that part is correct and must stay.
is %adj<a>.raku, '${:b(1), :c(2)}', 'a stored hash element is itemized';

# --- `for LIST.kv -> $k, %v`: the shape the issue was filed from -------------

my @seen;
for %adj.kv -> $k, %v {
    is %v.raku.substr(0, 1), '{', 'multi-param %v binds a plain Hash';
    for %v {
        @seen.push("$k -> {$_.key}");
    }
}
is @seen.sort.join(','), 'a -> b,a -> c,b -> a',
    'nested `for %v` iterates the bound hash pairwise, not as one item';

# --- every other route to a `%`-sigil binding --------------------------------

sub takes-hash(%h) { %h.raku }
is takes-hash(%adj<b>), '{:a(1)}', 'a positional %-parameter de-itemizes';

sub takes-named(:%o) { %o.raku }
is takes-named(o => %adj<b>), '{:a(1)}', 'a named %-parameter de-itemizes';

sub takes-copy(%h is copy) { %h.raku }
is takes-copy(%adj<b>), '{:a(1)}', 'an `is copy` %-parameter de-itemizes';

my $placeholder = { %^o.raku };
is $placeholder(%adj<b>), '{:a(1)}', 'a %^o placeholder de-itemizes';

class WithMethod { method m(%h) { %h.raku } }
is WithMethod.m(%adj<b>), '{:a(1)}', 'a %-parameter of a method de-itemizes';

my @hashes = { p => 1 }, { q => 2 };
my @single;
for @hashes -> %v { @single.push(%v.raku) }
is @single.join(','), '{:p(1)},{:q(2)}', 'a single %-parameter de-itemizes';

my $itemized-hash = $(my %plain = x => 1);
is $itemized-hash.raku, '${:x(1)}', '$(%h) is itemized to begin with';
my %bound := $itemized-hash;
is %bound.raku, '{:x(1)}', '`my %b := $(%h)` binds the Hash, not the wrapper';

# --- the `@`-sigil twin ------------------------------------------------------

my @outer = $(1, 2), $('x',);
my @shapes;
for @outer -> @v { @shapes.push(@v.raku) }
is @shapes.join(','), '(1, 2),("x",)',
    'a single @-parameter de-itemizes, keeping the List shape';

# The `@` de-itemization only used to happen when the parameter name did not
# already resolve to a local slot, so a same-named earlier binding masked it.
my %arrs = a => [1, 2];
for %arrs.kv -> $k, @v { }
my @after;
for %arrs.values -> @v { @after.push(@v.raku) }
is @after.join(','), '[1, 2]',
    'an @-parameter de-itemizes even when the name is already a local';

# --- what must NOT change ----------------------------------------------------

# De-itemization is identity-preserving: the SAME backing store is bound, so a
# write through the parameter still reaches the caller's hash.
my %target = a => { b => 1 };
for %target.kv -> $k, %v { %v<c> = 2 }
is %target<a>.raku, '${:b(1), :c(2)}', 'writing through the binding reaches the source';

sub mutate(%h) { %h<d> = 3 }
mutate(%target<a>);
is %target<a><d>, 3, 'writing through a %-parameter reaches the source';

# A QuantHash has no itemization to strip: it must keep its own type.
my @quant = set(1, 2), bag(3, 3);
my @types;
for @quant -> %q { @types.push(%q.^name) }
is @types.join(','), 'Set,Bag', 'a Set/Bag bound to %v keeps its type';

# An immutable Map binds unchanged too.
is takes-hash(Map.new((a => 1))), 'Map.new((:a(1)))', 'a Map binds as a Map';
