unit module RequiredDriver::Native;

# A type the driver below imports for its OWN lexical scope. It is exported, so
# `use RequiredDriver::Native` puts the short name `Widget` in the importing
# module's scope -- which is exactly the alias a `require` run inside a method
# frame used to lose (see t/require-in-method-keeps-module-type-alias.t).
class Widget is export {
    method label(--> Str) { 'widget' }
}

constant WIDGET-TAG is export = 'tagged';

# File-scope sigiled lexicals PRIVATE to this module, read by its own exported
# subs — the shape of MoarVM::Guts::REPRs' `my %known-bodies` consulted from
# `BODY_OF`. The sub runs with no frame of this module's classes on the stack,
# so the `%h{...}` / `@a[...]` reads must reach the module-scope fallback.
my %known-kinds = ptr => 'Pointer', arr => 'VMArray';
my @known-order = <ptr arr>;

sub kind-of(Str $k) is export {
    %known-kinds{$k} // 'MISS';
}

sub kind-count() is export {
    %known-kinds.elems;
}

sub kind-at(Int $i) is export {
    @known-order[$i];
}
