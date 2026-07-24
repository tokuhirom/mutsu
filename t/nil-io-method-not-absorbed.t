use v6;
use Test;

# The I/O routines `say` / `note` / `put` / `print` print rather than being
# Nil-absorbed: `Nil.say` emits the gist "Nil" (like the `say Nil` sub form)
# instead of silently returning Nil. Previously mutsu's Nil-method fast path
# swallowed these, so `sub a { }; a().say` printed nothing.

# --- Nil.say / Nil.note emit the gist and return True ---
{
    my $out = $*OUT;
    is (Nil.say).raku, "Bool::True", "Nil.say returns True";
    is (Nil.note).raku, "Bool::True", "Nil.note returns True";
}

# `.note` (on any invocant) returns True, matching the sub form.
is ("hello".note).raku, "Bool::True", ".note returns True (general)";

# --- The Type/Nil doc examples: empty subs / blocks return Nil, and .say
#     prints "Nil" ---
{
    sub a { };
    is a().gist, "Nil", "empty sub returns Nil";
    sub b { return };
    is b().gist, "Nil", "return with no value yields Nil";
    is ({ ; }()).gist, "Nil", "empty block returns Nil";
    sub c( --> Int:D ) { return Nil };
    is c().gist, "Nil", "return Nil ignores the :D return constraint";
}

# --- Regression: a genuinely Nil-absorbing (unknown) method still returns Nil ---
is Nil.foo.gist, "Nil", "unknown method on Nil is absorbed to Nil";

# --- Regression: the sub forms are unchanged ---
is (say Nil).raku, "Bool::True", "say Nil sub form returns True";

done-testing;
