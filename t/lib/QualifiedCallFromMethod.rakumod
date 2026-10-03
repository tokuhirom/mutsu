unit module QualifiedCallFromMethod;

our package Inner {
    our sub plain(Str $s) { "plain:$s" }
    our proto multi-f(|) {*}
    multi multi-f(UInt $n, Bool :$flag = False) { "uint:$n:$flag" }
    multi multi-f(Str $s, Bool :$flag = False) { "str:$s:$flag" }
}

our class C {
    method plain { Inner::plain "c" }
    method multi { Inner::multi-f 7, :flag }
    method in-closure { my $f = { Inner::plain "closure" }; $f() }
    method via-sub { helper() }
    sub helper { Inner::plain "lexical-sub" }
}

our role R {
    method from-role(:$flag = False) { Inner::multi-f self, :$flag }
}

our sub from-sub { Inner::multi-f "sub" }
