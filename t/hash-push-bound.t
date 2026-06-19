use Test;

# `%h.push` / `%h.append` through a `:=`-bound hash must propagate to the
# shared bind source (regression: it detached into the receiver's own slot, so
# the bind source never saw the new pair).

plan 9;

{
    my %g = (a => 1);
    my %h := %g;
    %h.push((b => 2));
    is %h.sort.gist, '(a => 1 b => 2)', 'push visible through bound alias';
    is %g.sort.gist, '(a => 1 b => 2)', 'push propagates to bind source';
}

{
    my %g = (a => 1);
    my %h := %g;
    %h.push((a => 2));
    is %g.gist, '{a => [1 2]}', 'existing-key push promotes to list on bind source';
}

{
    my %g = (a => 1);
    my %h := %g;
    %h.append((b => 5));
    is %g.sort.gist, '(a => 1 b => 5)', 'append propagates to bind source';
}

{
    my %g = (a => 1);
    my %h := %g;
    %h.push((b => 2), (c => 3));
    is %g.sort.gist, '(a => 1 b => 2 c => 3)', 'multi-pair push propagates';
}

{
    my %g = (a => 1);
    my %m := %g;
    my %h := %m;
    %h.push((z => 9));
    is %g.sort.gist, '(a => 1 z => 9)', 'push reaches chained bind root';
}

# plain (non-bound) hash push/append must stay correct
{
    my %h = (a => 1);
    %h.push((b => 2));
    is %h.sort.gist, '(a => 1 b => 2)', 'plain hash push';

    my %k = (a => 1);
    %k.push((a => 2));
    is %k.gist, '{a => [1 2]}', 'plain hash existing-key push promotes to list';

    my %j = (a => 1);
    %j.append((b => 5));
    is %j.sort.gist, '(a => 1 b => 5)', 'plain hash append';
}
