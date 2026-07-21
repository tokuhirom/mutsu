use Test;

# A private/accessor attribute array or hash with a method call must
# interpolate inside a double-quoted string, exactly like a lexical `@var` /
# `%hash`. Previously `"@!attr.method(...)"` was not recognized as an
# interpolation: the twigilled sigil (`@!`, `@.`, `%!`, `%.`) fell through the
# interpolation scanner, so the value was left literal — and when the method
# had a nested double-quoted argument (`.join(".")`), the inner quote closed the
# outer string and crashed the parse. (Version::Semver.Str builds its output
# with `"@!pre-release.join(".")"`.)

plan 8;

class V {
    has @.pre = <a b>;
    has @.build = <x y>;
    has %.opts = (k => 1);

    method with-double-quote(V:D:)  { "-@!pre.join(".")" }
    method with-single-quote(V:D:)  { "-@!pre.join('.')" }
    method no-nested-quote(V:D:)    { "n=@!pre.elems()" }
    method accessor-twigil(V:D:)    { "-@.build.join("-")" }
    method hash-private(V:D:)       { "k=%!opts.keys.join(",")" }
    method combined(V:D:) {
        "@!pre.join(".")" ~ ("+@!build.join(".")" if @!build)
    }
}

my $v = V.new;
is $v.with-double-quote, "-a.b", 'private array attr, nested double quotes';
is $v.with-single-quote, "-a.b", 'private array attr, nested single quotes';
is $v.no-nested-quote,   "n=2", 'private array attr method, no nested quote';
is $v.accessor-twigil,   "-x-y", 'accessor-twigil array attr interpolates';
is $v.hash-private,      "k=k", 'private hash attr interpolates';
is $v.combined,          "a.b+x.y", 'concat with paren-modified attr interp';

# Lexical arrays/hashes still interpolate as before (no regression).
{
    my @a = <p q>;
    is "@a.join("/")", "p/q", 'lexical array method interp still works';
    my %h = (z => 9);
    is "%h.keys.join(",")", "z", 'lexical hash method interp still works';
}
