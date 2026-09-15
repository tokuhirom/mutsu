use Test;

# `my Type $var.method;` (no `=`, no `.=`) calls `.method` on the
# just-declared `$var` itself (an undefined instance of the declared type)
# and discards the result -- confirmed against `raku` directly. mutsu
# previously left the declaration's `rest` untouched, so the trailing
# `.method` was independently parsed as a *leading-dot call on the topic*
# (`$_.method`) by the general statement parser -- the classic "singleton
# accessor" idiom (`my Foo::Bar $x.instance;`) then dispatched on whatever
# `$_` happened to hold instead of on the freshly declared variable.
#
# A second, related bug lived in class-body registration: each class-body
# statement is compiled and run as its own one-statement chunk, and
# `compile_unit`'s "last statement becomes the topic" tail treatment (meant
# for a real routine/mainline implicit-return path) fired for that lone
# statement every time, writing the class body's *own* `my` declarations
# into the *caller's* `$_` -- so a class with two or more `my`-declared
# lexicals silently clobbered the enclosing scope's topic. This is what
# made the leading-dot-on-`$_` misparse above look like it worked for a
# one-`my`-var class (`$_` happened to end up holding a compatible value)
# and then broke for a two-`my`-var class (`$_` ended up holding the
# *second* declaration's value instead), as in `Data::Generators`'s
# `ResourceAccess.instance`.

plan 13;

class Singleton {
    my Singleton $instance;
    method instance() {
        $instance //= Singleton.new;
        $instance;
    }
    method greet() { "hello" }
}

{
    my Singleton $s.instance;
    is $s.^name, 'Singleton',
        'my Type $var.method; leaves $var typed as the declared class';
    nok $s.defined,
        '...but $var itself stays undefined -- .instance\'s return is discarded';
    is $s.greet, 'hello',
        '$var is a real usable invocant of the declared type afterwards';
}

{
    $_ = 'ambient-topic';
    my Singleton $s2.instance;
    is $_, 'ambient-topic',
        'my Type $var.method; does not retopicalize the ambient $_';
}

# Untyped scalar: `.method` still targets the declared variable, not $_.
{
    $_ = 'ambient-topic';
    my $x.say;
    is $_, 'ambient-topic', 'untyped my $x.method; does not touch $_ either';
}

# A submethod called with no parens (the real-world `Data::Generators`
# shape) behaves the same as one called with `()`.
class SingletonNoParens {
    my SingletonNoParens $instance;
    submethod instance {
        $instance //= SingletonNoParens.bless;
        $instance;
    }
}
{
    my SingletonNoParens $s3.instance;
    is $s3.^name, 'SingletonNoParens',
        'no-parens submethod form also targets $var, not $_';
}

# The class-body $_-leak on its own, independent of the bare-dot idiom: a
# class whose body declares two (or more) `my` lexicals must not leave the
# *second* one sitting in the caller's $_ after the class registers.
{
    $_ = 'ambient-topic';
    class TwoLexicals {
        my $a = "first";
        my Int $b = 42;
    }
    is $_, 'ambient-topic',
        'a class body with 2+ my-decls does not clobber the caller\'s $_';
}
{
    $_ = 'ambient-topic';
    role TwoLexicalsRole {
        my $a = "first";
        my Int $b = 42;
    }
    is $_, 'ambient-topic',
        'a role body with 2+ my-decls does not clobber the caller\'s $_';
}

# A class body that legitimately reads/writes an *outer* lexical still
# works -- only the topic write is suppressed, not real lexical effects.
{
    my $tracker = 0;
    class Bumps {
        $tracker = 99;
    }
    is $tracker, 99, 'a real outer-lexical write from a class body still lands';
}

# Sanity: plain declarations (no trailing dot) are unaffected.
{
    my $x = 5;
    is $x, 5, 'plain scalar decl unaffected';
}
{
    my Int $y = 7;
    is $y, 7, 'plain typed scalar decl unaffected';
}
{
    class Plain { my $z = 1; }
    my Plain $p;
    is $p.^name, 'Plain', 'plain typed decl with no trailing dot unaffected';
}

# `.=` (method-call-assign) is a different construct and must still assign
# back into the declared variable, not merely call-and-discard.
{
    my Singleton $s4 .= instance;
    ok $s4.defined, 'my Type $var .= method; still assigns back (unlike bare .method)';
}
