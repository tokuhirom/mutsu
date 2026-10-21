use Test;

plan 6;

# &?ROUTINE inside a grammar rule is the rule itself (a proto candidate for
# `b:sym<t>`), resolved at declaration time as Rakudo does (#13246).
my @seen;
grammar G {
    #| outer doc
    rule outer { <inner> }
    #| inner doc
    rule inner { 'x' { @seen.push: &?ROUTINE.name; @seen.push: &?ROUTINE.WHY.leading } }
    proto rule b {*}
    #| the sym doc
    rule b:sym<t> { 't' { @seen.push: &?ROUTINE.name; @seen.push: &?ROUTINE.^name; @seen.push: &?ROUTINE.WHY.leading } }
    rule c { 'c' <note(&?ROUTINE.WHY.leading)> }
    token note($s) { <?{ @seen.push: "arg=$s"; True }> }
    #| c doc
    rule d { 'c' <note(&?ROUTINE.WHY.leading)> }
}

G.subparse('x', :rule<outer>);
is-deeply @seen, ['inner', 'inner doc'], 'a rule sees itself, not the entry rule';
@seen = ();
G.subparse('t', :rule<b>);
is-deeply @seen, ['b:sym<t>', 'Regex', 'the sym doc'], 'a proto candidate sees the candidate';
@seen = ();
G.subparse('c', :rule<d>);
is-deeply @seen, ['arg=c doc'], 'a call argument reads the rule';
@seen = ();
G.subparse('x', :rule<inner>);
is @seen[0], 'inner', 'entering through the rule itself';
ok G.^lookup('inner').WHY.defined, 'lookup still finds the documented rule';
is G.^lookup('b:sym<t>').^name, 'Regex', 'candidate lookup is a Regex';
