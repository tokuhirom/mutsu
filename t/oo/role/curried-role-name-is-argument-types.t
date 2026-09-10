use Test;

# Rakudo names a CURRIED parametric role after the TYPES of its arguments, not
# their values: `R["x"].^name` is `R[Str]`, not `R[x]`. mutsu stringified each
# stored argument, so every rendering of a curried role (`.^name`, `.raku`,
# `.gist`, `.WHAT`, `.HOW.name`) showed the value — and it propagated into
# anything that embeds the name, such as a `^parameterize` hook that builds one
# from `roles.map(*.^name)`.

plan 14;

# --- a value argument renders as its type --------------------------------
{
    my role R[Str:D $n] { }
    is R["x"].^name, 'R[Str]', 'a Str argument renders as Str';
    is R["x"].raku, 'R[Str]', '.raku agrees';
    is R["x"].gist, '(R[Str])', '.gist wraps the same name in type-object parens';
    is R["x"].WHAT.^name, 'R[Str]', '.WHAT carries the same name';
    is R["x"].HOW.name(R["x"]), 'R[Str]', 'and so does the CurriedRoleHOW';
}
{
    my role U[Int $i] { }
    is U[42].^name, 'U[Int]', 'an Int argument renders as Int';
}
{
    my role V[Str $a, Int $b] { }
    is V["p", 3].^name, 'V[Str,Int]', 'several arguments each render as their type';
}

# --- a TYPE OBJECT argument keeps its own name ---------------------------
# "render the argument's type" reduces to the argument itself here, which is
# the case that already worked and must keep working.
{
    my role S[::T] { }
    is S[Int].^name, 'S[Int]', 'a type-object argument keeps its own name';
    is S[Str].^name, 'S[Str]', 'for any type';
}

# --- a curried role AS an argument nests -------------------------------
{
    my role Inner[Str $s] { }
    my role Outer[::T] { }
    is Outer[Inner['x']].^name, 'Outer[Inner[Str]]',
        'a curried-role argument renders with its own arguments typed';
}

# --- the ^parameterize hook from Language/mop.rakudoc --------------------
{
    my class Foo {
        method ^parameterize(::?CLASS:U $this is raw, +roles) {
            my Str:D $name   = self.name: $this;
            my Mu    $mixin := $this.^mixin: |roles;
            $mixin.^set_name: [~] $name, '[', roles.map(*.^name).join(','), ']';
            $mixin
        }
    }
    my role P[Str:D $n] { }
    is Foo[P['x']].^name, 'Foo[P[Str]]',
        'a name built from roles.map(*.^name) picks up the typed spelling';
}

# --- non-regression: currying still works as a type ---------------------
{
    my role W[Str $a] { }
    my class C does W['q'] { }
    ok C ~~ W, 'the composing class still does the role';
    ok W['q'] ~~ W, 'and the curried role still smartmatches its group';
    my role X[Str $a] { method tag { $a } }
    my class D does X['tagged'] { }
    is D.new.tag, 'tagged', 'the argument VALUE still reaches the role body';
}
