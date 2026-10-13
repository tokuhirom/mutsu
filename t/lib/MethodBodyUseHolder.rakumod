# Holds a `use` that appears only inside a method body. Raku loads it when this
# compunit is compiled, so the module's classes exist before any method runs.
class MethodBodyUseHolder {
    method late {
        use MethodBodyUseLate;
        MethodBodyUseLate::Marker.new.tag
    }
}
