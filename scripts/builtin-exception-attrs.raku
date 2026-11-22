#!/usr/bin/env raku
# Regenerates src/runtime/builtin_exception_attrs.tsv: the attributes each of
# rakudo's built-in exception classes (`Exception`, `X::*`, `CX::*`) declares
# itself (`.^attributes(:local)`), in declaration order.
#
#   raku scripts/builtin-exception-attrs.raku > src/runtime/builtin_exception_attrs.tsv
#
# One row per attribute that construction binds (an accessor or `is built`):
# `Class<TAB>sigil<TAB>name<TAB>public<TAB>default<TAB>build`, where `public`
# is 1 when the attribute has an accessor, `default` is the type object an
# unset scalar holds (`Any`, `Mu`, `Bool`, `Nil`), empty for an `@`/`%`
# attribute, and `build` is a literal initializer (`has $.what = 'symbol'`)
# as `Str:<text>` or `Bool:True`/`Bool:False`, empty when there is none or it
# is computed (a block).
# Private bookkeeping attributes (`Exception`'s `$!ex`/`$!bt`, a cached
# `$!message`) are not listed: no named argument reaches them, and `.raku`
# does not show them.
use nqp;

my %seen;
sub walk($stash, Str $prefix) {
    for $stash.keys.sort -> $key {
        my $name = $prefix ~ $key;
        my \type = $stash{$key};
        next if %seen{$name}++;
        emit(type) if type.HOW ~~ Metamodel::ClassHOW && type.^name eq $name;
        walk(type.WHO, $name ~ '::') if type.WHO.keys;
    }
}
sub emit(\type) {
    for type.^attributes(:local) -> $attr {
        next unless $attr.has_accessor || $attr.is_built;
        my $sigil = $attr.name.substr(0, 1);
        my $name  = $attr.name.substr(2);
        my \container = $attr.container;
        my $default = $sigil eq '$' && nqp::iscont(container)
          ?? container.VAR.default.^name
          !! '';
        my $b = $attr.build;
        my $build = do given $b {
            when Code { '' }
            when Bool:D { 'Bool:' ~ $b.Str }
            when Str:D {
                die "unencodable initializer {$b.raku}" if $b ~~ /<[\t\n\\]>/;
                'Str:' ~ $b
            }
            default { '' }
        };
        say (type.^name, $sigil, $name, +$attr.has_accessor, $default, $build).join("\t");
    }
}

emit(Exception);
walk(X::, 'X::');
walk(CX::, 'CX::');
