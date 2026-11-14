use Test;

# A type `Metamodel::ClassHOW.new_type` mints on one worker thread, under a
# Lock, after its sibling threads already started, is reachable from those
# siblings through the shared type object (#17697, ADR-17697).

plan 11;

role Greets[::T] { method greet { "hi " ~ T.^name } }
class Base { has $.n = 7; method base-m { "base" } }
class Model {}
class Other {}

class Cache {
    has $!rs = Any;
    has $.name;
    method get {
        return $!rs unless $!rs === Any;
        state Lock $lock .= new;
        $lock.protect({
            if $!rs === Any {
                my $x := Metamodel::ClassHOW.new_type: :name($!name);
                $x.^add_parent: Base;
                $x.^add_role: Greets[Model];
                $x.^add_method: 'own', method { "own " ~ self.n };
                $x.^compose;
                $!rs := $x;
            }
            $!rs
        })
    }
}

# The ADR's repro: only `.new` on the cached type.
{
    my $c = Cache.new(:name<Shared::ResultSeq>);
    my @r = await (^16).map(-> $ { start { $c.get.new; 'ok' } });
    is @r.unique.join(","), 'ok', 'siblings can .new a qualified type minted on another worker';
}

for <Shared::Composed PlainComposed> -> $name {
    my $c = Cache.new(:$name);
    my @r = await (^16).map(-> $ { start {
        my $o = $c.get.new;
        join "|", $o.^name, $o.greet, $o.base-m, $o.own, ($o ~~ Base), ($o ~~ Greets),
            $o.^mro.map(*.^name).join(",")
    } });
    is @r.unique.elems, 1, "$name: every sibling sees the same type";
    is @r.head, "$name|hi Model|base|own 7|True|True|$name,Base,Any,Mu",
        "$name: parent, role, added method and MRO reach the siblings";
}

# The type minted on a worker, then queried on the main thread.
{
    my $c = Cache.new(:name<Main::Seen>);
    my @types = await (^4).map(-> $ { start { $c.get.new.WHAT } });
    my $t = @types.head;
    is $t.^name, 'Main::Seen', 'main thread sees the name';
    ok $t ~~ Greets[Model], 'main thread: composes the parameterized role';
    nok $t ~~ Greets[Other], 'main thread: not another parameterization';
    ok $t ~~ Greets, 'main thread: does the role';
    is $t.new.greet, 'hi Model', 'main thread: role method runs with its type argument';
    is $t.^roles.map(*.^name).join(","), 'Greets[Model]', 'main thread: .^roles';
}
