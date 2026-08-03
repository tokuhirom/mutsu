use Test;

plan 4;

# A `whenever` nested inside another `whenever`'s body must keep the enclosing
# supply block's emitter. The emitter name is unique per parse site but SHARED by
# every runtime instance of that site, so with two live instances of one
# `supply { whenever … { whenever … { emit } } }` (Cro's route sets: an outer one
# delegating to an inner one) the nested `emit` used to resolve to whichever
# sibling instance happened to be dispatching it, and the two ping-ponged the
# value forever.

class RouteSet {
    has $.name;
    has $.inner;
    method transformer(Supply:D $requests) {
        supply {
            whenever $requests -> $request {
                if $!inner {
                    my $delegated = $!inner.transformer(supply { emit "$request/via-$!name" });
                    whenever $delegated -> $response {
                        emit $response;
                    }
                }
                else {
                    emit "handled($request) by $!name";
                }
            }
        }
    }
}

my $inner = RouteSet.new(name => 'INNER');
my $outer = RouteSet.new(name => 'OUTER', inner => $inner);

my $src = Supplier.new;
my @got;
$outer.transformer($src.Supply).tap(-> $v { @got.push($v) });
$src.emit("REQ");

is @got.elems, 1, 'the delegated response is emitted exactly once';
is @got[0], 'handled(REQ/via-OUTER) by INNER', 'and it is the inner route set-s response';

# The same shape, but with the emit written as a sub-expression rather than a
# statement: `$x ~~ T ?? emit($x) !! die` is what Cro's middleware role uses, and
# an unrewritten `emit` there fell back to the dynamic emitter stack — in a
# pipeline that is a NEIGHBOURING stage's emitter, so the value skipped the rest
# of the pipeline.
role Checked {
    method transformer(Supply $pipeline --> Supply) {
        supply whenever self.process($pipeline) -> $item {
            $item ~~ Str
                ?? emit($item)
                !! die "not a Str";
        }
    }
    method process(Supply $items --> Supply) { ... }
}

class Upper does Checked {
    method process(Supply $items --> Supply) {
        supply whenever $items -> $item { emit $item.uc }
    }
}

class Collector {
    method transformer(Supply $items --> Supply) {
        supply {
            whenever $items -> $item { emit "[$item]" }
        }
    }
}

my $src2 = Supplier.new;
my @got2;
Collector.new.transformer(Upper.new.transformer($src2.Supply)).tap(-> $v { @got2.push($v) });
$src2.emit("hi");

is @got2.elems, 1, 'a ternary emit reaches its own supply exactly once';
is @got2[0], '[HI]', 'and it goes downstream, not to a neighbouring stage';
