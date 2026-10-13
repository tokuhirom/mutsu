use Test;

plan 6;

# `IO::Path.path` is the path text; a user subclass inherits it, whether or
# not it declares attributes or overrides `Str` (App::RaCoCo, #13126).
class Plain is IO::Path { }
class WithAttr is IO::Path {
    has $.modified is rw;
    method Str() { self.path }
}

my $p = Plain.new("/x/y");
is $p.path, "/x/y", 'path on an attribute-less IO::Path subclass';
isa-ok $p.path, Str, 'path is a Str';

my $w = WithAttr.new("/a/b");
is $w.path, "/a/b", 'path on a subclass with a declared attribute';
is $w.Str, "/a/b", 'overridden Str calling self.path';
is "$w", "/a/b", 'interpolation uses the overridden Str';
is "/a/b".IO.path, "/a/b", 'plain IO::Path still answers path';
