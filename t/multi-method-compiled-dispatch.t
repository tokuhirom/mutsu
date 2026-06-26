use Test;
plan 6;

# Multi-method dispatch + samewith re-dispatch run as compiled bytecode (§B):
# `call_method_with_values` -> run_instance_method now executes the resolved
# candidate via call_compiled_method instead of recompile-each-call tree-walk.
{
    class C1 {
        has $.log is rw;
        multi method m(Int $x) { self.log ~= "int;"; samewith $x.Str }
        multi method m(Str $x) { self.log ~= "str:$x;" }
    }
    my $o = C1.new(log => "");
    $o.m(42);
    is $o.log, "int;str:42;", 'multi-method samewith re-dispatch chains correctly';
}

# Repeated multi-method dispatch in a loop (the defer-next hot-path shape).
{
    class C2 {
        multi method m(Int $x) { samewith $x.Str }
        multi method m(Str) { 7 }
        multi method call() { self.m(1) }
    }
    my $sum = 0;
    $sum += C2.new.call for ^200;
    is $sum, 1400, 'repeated multi-method/samewith loop accumulates correctly';
}

# Multi-method dispatched by argument type.
{
    class C3 {
        multi method describe(Int) { "int" }
        multi method describe(Str) { "str" }
        multi method describe($)   { "other" }
    }
    my $o = C3.new;
    is $o.describe(5), "int", 'multi-method Int candidate';
    is $o.describe("x"), "str", 'multi-method Str candidate';
    is $o.describe(3.14), "other", 'multi-method fallback candidate';
}

# Attribute mutation across a samewith chain is preserved.
{
    class C4 {
        has @.calls is rw;
        multi method step(Int $n) { @!calls.push("i$n"); samewith "s$n" }
        multi method step(Str $s) { @!calls.push($s) }
    }
    my $o = C4.new(calls => []);
    $o.step(9);
    is $o.calls.join(","), "i9,s9", 'attribute mutation across samewith preserved';
}
