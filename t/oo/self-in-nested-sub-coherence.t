use Test;

# `self` inside a nested sub is resolved from the dynamic env. A method/action
# call on ANOTHER object binds `self` to that object; if the dispatch path does
# not restore the caller's `self`, a later `self` read in the enclosing nested
# sub sees the wrong invocant. These cover the dispatch paths that used to leak.

plan 4;

# 1. String interpolation of an object (StringConcat -> Str) must not leak self.
{
    class OtherS { method Str() { "O" } }
    class Ca {
        method who() { "C" }
        method run($obj) {
            sub probe() { self.who() }
            my $a = probe();
            my $junk = "$obj";        # stringify Other via .Str
            my $b = probe();          # self must still be C
            "$a$b";
        }
    }
    is Ca.new.run(OtherS.new), "CC", 'string interpolation does not leak self';
}

# 2. Dynamic method call (`$obj."$name"()`) must not leak self.
{
    class OtherN { method name() { "n" } }
    class Cb {
        method who() { "C" }
        method run($obj) {
            sub probe() { self.who() }
            my $a = probe();
            my $meth = "name";
            my $junk = $obj."$meth"();   # dynamic dispatch on Other
            my $b = probe();             # self must still be C
            "$a$b";
        }
    }
    is Cb.new.run(OtherN.new), "CC", 'dynamic method call does not leak self';
}

# 3. Grammar action dispatch (Grammar.parse :actions) must not leak self.
{
    grammar G { token TOP { \w+ } }
    class Acts { method TOP($/) { make ~$/ } }
    class Cc {
        method who() { "C" }
        method run() {
            sub probe() { self.who() }
            my $a = probe();
            G.parse("hello", :actions(Acts.new));
            my $b = probe();             # self must still be C, not Acts
            "$a$b";
        }
    }
    is Cc.new.run(), "CC", 'grammar action dispatch does not leak self';
}

# 4. Combination: dotted-name + object stringification (Template::Mustache shape).
{
    class Obj { method Str() { "father" }; method label() { "L" } }
    class Host {
        method tag() { "T" }
        method render(%ctx) {
            sub emit($key) {
                # read self, call a method on a context object, read self again
                my $before = self.tag();
                my $v = %ctx{$key};
                my $s = "$v";              # stringify the object
                my $after = self.tag();
                "$before/$s/$after";
            }
            emit("o");
        }
    }
    is Host.new.render({ o => Obj.new }), "T/father/T",
        'self stable across object stringify in nested sub';
}
