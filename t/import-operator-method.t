use Test;

# `method prefix:<~> is export` / `method infix:<as> is export` declares an
# operator method that `import ClassName` exposes as a sub form: `~$obj` and
# `$obj as $x` then dispatch to the class's operator overload.

plan 8;

{
    class OtherClass {
        has $.x is rw;
    }

    class MyClass {
        method prefix:<~> is export { "hi" }
        method prefix:<+> is export { 42 }
        method infix:<as>($self: OtherClass $to) is export {
            my $obj = $to.new;
            $obj.x = 23;
            return $obj;
        }
    }
    import MyClass;

    my $obj;
    lives-ok { $obj = MyClass.new }, "instantiation works";
    lives-ok { ~$obj }, "object can be stringified";
    is ~$obj, "hi", "prefix:<~> dispatches to the method";
    is +$obj, 42, "prefix:<+> dispatches to the method";
    is ($obj as OtherClass).x, 23, "infix:<as> dispatches to the method";
    # The new operator must also be visible inside EVAL (runtime re-parse).
    is EVAL('($obj as OtherClass).x'), 23, "infix:<as> visible in EVAL";

    # The overload only applies to the declaring class; builtins still work.
    is ~5, "5", "builtin prefix:<~> still works for other types";
    is +"7", 7, "builtin prefix:<+> still works for other types";
}
