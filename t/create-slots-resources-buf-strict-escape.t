use v6;
use Test;

plan 15;

# --- 1. CREATE allocates attribute slots so `$!attr = ...` in a private
#        builder (`self.CREATE!SET-SELF`) persists. ---
{
    class Builder {
        has %.exts;
        has $.name;
        method !setup {
            %!exts = (a => 1, b => 2);
            $!name = "built";
            self
        }
        method build { self.CREATE!setup }
    }
    my $b = Builder.build;
    is $b.name, "built", "CREATE: scalar attr write in private builder persists";
    is $b.exts.keys.sort.join(","), "a,b", "CREATE: hash attr write persists";
}

# CREATE does NOT run default-value expressions (slots are type-default empty).
{
    class WithDefault { has $.x = "default-expr"; }
    my $w = WithDefault.CREATE;
    nok $w.x.defined, "CREATE does not evaluate has-default expressions";
}

# --- 2. `.Buf` / `.Blob` coercion on an encoded byte-string. ---
{
    my $u = "hi".encode;          # utf8 (does Blob)
    my $buf = $u.Buf;
    is $buf.^name, "Buf", ".Buf returns a Buf";
    is $buf.elems, 2, ".Buf preserves bytes";
    is "ab".encode.Blob.^name, "Blob", ".Blob returns a Blob";
    is "\r\n".encode.Buf.list.join(","), "13,10", ".Buf bytes are correct";
}

# --- 3. `\e` (escape, 0x1B) in strings and regexes. ---
{
    is "\e".ord, 27, '\e in a string is ESC (0x1B)';
    is "x\e[0my".subst(/\e\[ <[0..9;]>+ m/, "", :g), "xy", '\e in a regex matches ESC';
}

# --- 4. `use strict` must not flag attribute twigils or compiler temporaries
#        as undeclared (regression: transitively-loaded class methods that
#        write `%!attr := ...` under an outer module's `use strict`). ---
{
    use strict;
    class StrictAttr {
        has %.h;
        method fill { %!h = (k => 1); %!h }
    }
    my $s = StrictAttr.new;
    $s.fill;
    is $s.h<k>, 1, "strict: writing %!attr in a method is allowed";
}

# --- regression guards ---
{
    # CREATE then a public mutating method returning self
    class C { has $.v; method set($x) { $!v = $x; self } }
    my $c = C.CREATE;
    is $c.set(42).v, 42, "CREATE + public mutating method writeback";
}
{
    # bless/new path still works
    class N { has $.n = 7; }
    is N.new.n, 7, "new still evaluates has-default expressions";
    is N.new(n => 9).n, 9, "new with named arg";
}
{
    is "tab\there".subst(/\t/, " ").words.elems, 2, '\t still works in regex';
}

# --- `__`-prefixed compiler temporaries are exempt from `use strict`
#     (with/without topic temps, for-loop sources, etc.). ---
{
    use strict;
    my %h = a => 1;
    my $seen = '';
    with %h<a> { $seen = "with:$_" }      # desugars to a `__with_tmp_*` temp
    without %h<missing> { $seen ~= "-skipped" }
    with %h<missing> { $seen ~= "!" }
    is $seen, "with:1-skipped", "with/without under strict (internal temps exempt)";
}
