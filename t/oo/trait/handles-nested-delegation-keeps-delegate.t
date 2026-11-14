use Test;

# A delegated call made inside the block another delegated method runs
# (`self.n` inside `$.protect: { ... }`, both through `handles`) leaves the
# outer delegate in place: the second `$.protect` still finds the Lock.
# Expected values from rakudo. Reduced from Font::FreeType's
# `Face.index-to-unicode` (PDF::Font::Loader::CSS), which died with
# "No such method 'protect'" on its second call.

plan 4;

class Raw { has $.n = 3 }
class Face {
    has Raw $.raw handles <n> = Raw.new;
    has Lock $!lock handles<protect> .= new;
    has @!to;
    method idx {
        $.protect: {
            unless @!to {
                @!to[self.n] = 0;
            }
        }
        @!to.elems;
    }
    method twice { $.protect: { self.n } }
}

my $f = Face.new;
is $f.idx, 4, 'first delegated protect';
is $f.idx, 4, 'second delegated protect still finds the Lock';
is $f.twice, 3, 'a nested delegation returns its value';
is $f.twice, 3, '...and again';
