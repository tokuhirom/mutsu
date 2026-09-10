use Test;

# A slurpy hash (`*%o`) collects *named* arguments. It is neither a positional
# parameter nor a positional variadic, so it must not make its candidate accept
# an unbounded number of positionals.

plan 6;

{
    sub s(Str $a, Str $b = '', *%o) { "a=$a b=$b o={%o.keys.sort.join(',')}" }

    is s('x'), 'a=x b= o=', 'the optional positional stays optional';
    is s('x', 'y'), 'a=x b=y o=', 'both positionals bind';
    is s('x', 'y', :k(1), :j(2)), 'a=x b=y o=j,k', 'the slurpy collects the nameds';
    dies-ok { s('x', 'y', 'z') }, 'a third positional does not fit';
}

{
    # Dispatch must reject the named-slurpy candidate on positional arity, not
    # treat it as a catch-all.
    proto sub p(|) {*}
    multi sub p(Str $a, *%o) { 'one' }
    multi sub p(Str $a, Str $b, Str $c) { 'three' }

    is p('x'), 'one', 'the named-slurpy candidate takes its own arity';
    is p('x', 'y', 'z'), 'three', 'it does not swallow a wider call';
}
