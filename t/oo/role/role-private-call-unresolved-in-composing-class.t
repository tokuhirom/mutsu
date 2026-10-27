use Test;

# From Selkie (t/105-screen-subscription-lifecycle.rakutest): a role method that
# calls `self!helper` on a path that is not taken must not make composing the
# role into a class fail when the class has no `!helper`; rakudo checks private
# calls only in the class's own methods.
plan 3;

role R {
    has Bool $.flag = False;
    method !maybe(--> Mu) {
        return Nil unless $.flag;
        self!missing;
    }
    method go { self!maybe }
}
class F does R { }
ok F.new.go === Nil, 'the untaken private call is not judged at composition';
dies-ok { F.new(:flag).go }, 'taking the path fails at run time';

throws-like 'class G { method m { self!nope } }', X::Method::NotFound,
    "a class's own method still gets the compile-time check";
