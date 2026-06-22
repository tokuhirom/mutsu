use v6;
use Test;

plan 4;

# Grammar action methods must fire for named rules that are nested inside a
# numbered capture group `( ... )`, and the propagated captures must keep their
# own nested sub-captures so deeper action methods also fire.

grammar G {
    token TOP { ( <.request-line> ) .* }
    token request-line { <method> <.SP> <request-target> }
    token method { \w+ }
    token request-target { <path> [ '?' <query> ]? }
    token path  { <-[?#\ ]>* }
    token query { <-[#\ ]>* }
    token SP { ' ' }
}

class Actions {
    has %.env;
    method method($/)         { %!env<METHOD>      = ~$/; }
    method request-target($/) { %!env<URI>         = ~$/; %!env<QUERY> //= ''; }
    method path($/)           { %!env<PATH>        = ~$/; }
    method query($/)          { %!env<QUERY>       = ~$/; }
}

# Actions for rules nested in a `( )` group fire (regression: a positional
# capture group used to be a boundary the action walk never crossed).
{
    my $a = Actions.new;
    G.parse("GET /foo HTTP", :actions($a));
    is $a.env<METHOD>, 'GET', 'action for a rule inside a ( ) group fires (method)';
    is $a.env<URI>, '/foo', 'captured subrule action fires (request-target)';
    # request-target is captured AND nested under the non-captured request-line;
    # its own nested <path> must survive so the path action fires too.
    is $a.env<PATH>, '/foo', 'grandchild action survives propagation (path)';
}

# Action methods for rules nested (any depth) under a silent `<.subrule>` fire,
# because the propagated captures keep their own nested sub-captures.
{
    grammar H {
        token TOP { <.outer> }
        token outer { <inner> }
        token inner { <leaf> }
        token leaf { \w+ }
    }
    class HActions {
        has @.seen;
        method leaf($/)  { @!seen.push('leaf:'  ~ ~$/); }
        method inner($/) { @!seen.push('inner:' ~ ~$/); }
    }
    my $a = HActions.new;
    H.parse("hello", :actions($a));
    is $a.seen, ['leaf:hello', 'inner:hello'],
        'deeply nested action methods fire through a silent subrule';
}
