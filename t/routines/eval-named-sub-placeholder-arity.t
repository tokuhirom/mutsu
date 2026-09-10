use Test;

# A named sub built via EVAL (`sub t { $^a + $^b }`), stored in a lexical and
# called as a value, is a runtime call -- not compile-time-diagnosable -- so
# an arity mismatch must surface raku's plain runtime message, not the
# "Calling ... will never work with declared signature ..." wrapper (which is
# for statically-resolved bare calls). Found via Template::Mojo, whose
# generated template sub is exactly this shape.
# (todo/tickets/template-mojo-residual-failures.md)
#
# Only the too-few case is covered here. Raku also rejects too-many
# positionals for this shape, but mutsu deliberately does not enforce that:
# a `^`-twigil placeholder sub whose body also references bare `@_`/`%_`
# legitimately accepts extra positionals in Raku (they flow into `@_`), and
# nothing in mutsu's raw `params` list distinguishes that shape from one that
# should reject extras -- see the comment above `required_positional_count`
# in `src/runtime/types/binding_signature.rs` for why a targeted fix was
# tried and reverted.

use MONKEY-SEE-NO-EVAL;

plan 2;

sub build() {
    EVAL 'sub t { $^a + $^b }';
}

sub call-with(*@args) {
    my &f = build();
    my $err = '';
    {
        f(|@args);
        CATCH { default { $err = .Str } }
    }
    $err;
}

my $too-few = call-with(23);
ok $too-few ~~ /'Too few positionals passed'/, 'too few positionals: message shape';
ok $too-few ~~ /'expected 2'/ && $too-few ~~ /'got 1'/, 'too few positionals: counts';
