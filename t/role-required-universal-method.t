use v6;
use Test;

# A role requirement is satisfied by NAME, and every object inherits a set of
# methods from Mu/Any (`new`, `bless`, `clone`, `gist`, `defined`, ...). A role
# stub `method new {...}` is therefore satisfied by the inherited `Mu.new` even
# when the composing class does not redefine it — mutsu used to reject this with
# "Method 'new' must be implemented by C because it is required by roles: R.".
# Regression pin for Tree::Binary::PrettyTree (only defines `submethod BUILD`).

plan 6;

{
    role R {
        method new(:$x) {...}
        method go() {...}
    }
    class C does R {
        method go() { 'hi' }
    }
    lives-ok { C.new }, 'class composing a role that requires `new` builds';
    is C.new.go, 'hi', 'the class works after composition';
}

{
    # `gist` required by a role is satisfied by the universal Mu.gist.
    role Showable {
        method gist() {...}
        method label() {...}
    }
    class Widget does Showable {
        method label() { 'widget' }
    }
    lives-ok { Widget.new }, 'role requiring `gist` is satisfied by Mu.gist';
    is Widget.new.label, 'widget', 'Widget.label works';
}

{
    # A genuinely-unimplemented (non-universal) required method still errors.
    my $err;
    {
        role Needs {
            method frobnicate() {...}
        }
        # Compose at runtime via EVAL so the composition error is catchable.
        $err = (try EVAL 'role N2 { method frobnicate() {...} }; class Bad does N2 { }; Bad') // $!;
    }
    ok $err.defined && "$err".contains('frobnicate'),
        'a non-universal required method is still enforced';
}

{
    # The BUILD-only shape from Tree::Binary::PrettyTree: role requires `new`,
    # class provides only `submethod BUILD`.
    role Renderer {
        method new(:$tree) {...}
        method render() {...}
    }
    class Pretty does Renderer {
        has $.tree;
        submethod BUILD(:$!tree = 0) {}
        method render() { "tree=$!tree" }
    }
    is Pretty.new(tree => 5).render, 'tree=5',
        'BUILD-only class composes a role requiring `new`';
}
