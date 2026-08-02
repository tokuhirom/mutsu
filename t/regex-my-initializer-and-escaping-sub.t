use v6;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 5;

# A later `:my` initializer sees an earlier `:my` lexical (and may call
# methods on it — the initializer runs with full dispatch).
{
    my class Req {
        has $.path = "/greet/world";
        method segs() { $!path.substr(1).split("/").list }
    }
    my $*REQ = Req.new;
    my $rx = EVAL 'regex { ^ :my $req = $*REQ; :my @segs = $req.segs; greet { make @segs.join(",") } $ }';
    "greet" ~~ $rx;
    is $/.ast, "greet,world", 'later :my initializer reads an earlier :my lexical via a method call';
}

# A code-bearing regex literal is a closure: created in one frame (here via
# EVAL inside a method), stored, and matched from another frame, its embedded
# code still sees the defining scope's lexicals.
{
    my class RouteSet {
        has $!matcher;
        method generate() {
            my @handlers = "aa", "bb";
            use MONKEY-SEE-NO-EVAL;
            $!matcher = EVAL 'regex { ^ :my $cap; greet { $cap = 99 } <?{ my $han = @handlers[1]; $han.chars == 2 }> { make (1, $cap) } $ }';
        }
        method route(Str $path) {
            with $path ~~ $!matcher {
                my ($idx, $args) = .ast;
                "idx=$idx args=$args"
            }
            else { "NO MATCH" }
        }
    }
    my $rs = RouteSet.new;
    $rs.generate();
    is $rs.route("greet"), "idx=1 args=99",
        'stored EVAL\'d regex still reads the defining method\'s lexical array';
}

# A sub declared lexically in a block stays callable from a closure that
# escapes the block (Cro: `my sub fresh-message` in a supply block, called
# from a whenever).
{
    my $c;
    {
        my sub f() { "escaped-ok" }
        $c = -> { f() };
    }
    is $c(), "escaped-ok", 'block-lexical my sub callable from escaped closure';
}

{
    my @got;
    my $s = supply {
        my $n = 0;
        my sub f() { "sup-" ~ ++$n }
        emit f();
        whenever Supply.from-list(1, 2) -> $x {
            emit f();
        }
    };
    react {
        whenever $s -> $v { @got.push($v) }
    }
    is @got.join(","), "sup-1,sup-2,sup-3",
        'supply-block my sub callable from whenever closures';
}

# Signature.ACCEPTS as a method call (Cro's route bind check).
{
    my $sig = (-> "greet", $name { }).signature;
    ok $sig.ACCEPTS(\("greet", "world")) && !$sig.ACCEPTS(\("nope", "x")),
        'Signature.ACCEPTS(Capture) works as a method call';
}
