use v6;
use Test;

# A `.WHICH`-derived (composite) object-hash key must select the SAME entry for
# a read-modify-write (`++`, `--`, `+=`, `~=`) as it does for a plain store.
# It used to key `++` by the raw list node's own identity while `=` keyed it by
# the canonical `Scalar`-wrapped subscript, so the increment landed in a second
# bucket and never accumulated (GH #7538).

plan 23;

class IncKey { has $.n }

# --- List key -----------------------------------------------------------
{
    my %h{Any};
    my $k = (3, 4);
    %h{$k} = 1;
    %h{$k}++;
    is %h{$k}, 2, 'List key: ++ accumulates';
    is %h.elems, 1, 'List key: ++ does not add a second entry';
    is %h.keys[0].raku, '(3, 4)', 'List key: the recorded key stays de-itemized';
}

{
    my %h{Any};
    my $k = (3, 4);
    %h{$k} = 1;
    %h{$k}--;
    is %h{$k}, 0, 'List key: -- accumulates';
    is %h.elems, 1, 'List key: -- does not add a second entry';
}

{
    my %h{Any};
    my $k = (3, 4);
    %h{$k} = 1;
    ++%h{$k};
    is %h{$k}, 2, 'List key: prefix ++ accumulates';
    is %h.elems, 1, 'List key: prefix ++ does not add a second entry';
}

{
    my %h{Any};
    my $k = (3, 4);
    %h{$k} = 1;
    %h{$k} += 5;
    is %h{$k}, 6, 'List key: += accumulates';
    is %h.elems, 1, 'List key: += does not add a second entry';
}

{
    my %h{Any};
    my $k = (3, 4);
    %h{$k} = "a";
    %h{$k} ~= "b";
    is %h{$k}, "ab", 'List key: ~= accumulates';
    is %h.elems, 1, 'List key: ~= does not add a second entry';
}

# --- itemized List key --------------------------------------------------
{
    my %h{Any};
    my $k = $(3, 4);
    %h{$k} = 1;
    %h{$k}++;
    is %h{$k}, 2, 'itemized List key: ++ accumulates';
    is %h.elems, 1, 'itemized List key: ++ does not add a second entry';
    is %h.keys[0].raku, '(3, 4)', 'itemized List key: the recorded key stays de-itemized';
}

# --- Array key ----------------------------------------------------------
{
    my %h{Any};
    my $k = [3, 4];
    %h{$k} = 1;
    %h{$k}++;
    is %h{$k}, 2, 'Array key: ++ accumulates';
    is %h.elems, 1, 'Array key: ++ does not add a second entry';
    is %h.keys[0].raku, '[3, 4]', 'Array key: the recorded key keeps its Array shape';
}

# --- Hash key -----------------------------------------------------------
{
    my %h{Any};
    my $k = { a => 1 };
    %h{$k} = 1;
    %h{$k}++;
    is %h{$k}, 2, 'Hash key: ++ accumulates';
    is %h.elems, 1, 'Hash key: ++ does not add a second entry';
}

# --- instance key (already worked; pinned so it stays that way) ---------
{
    my %h{Any};
    my $k = IncKey.new(n => 1);
    %h{$k} = 1;
    %h{$k}++;
    is %h{$k}, 2, 'instance key: ++ accumulates';
    is %h.elems, 1, 'instance key: ++ does not add a second entry';
}

# --- scalar key (already worked; pinned so it stays that way) ----------
{
    my %h{Any};
    my $k = 5;
    %h{$k} = 1;
    %h{$k}++;
    is %h{$k}, 2, 'scalar key: ++ accumulates';
}

# --- autovivifying ++ on an absent composite key -----------------------
{
    my %h{Any};
    my $k = (3, 4);
    %h{$k}++;
    %h{$k}++;
    is %h{$k}, 2, 'List key: ++ autovivifies then accumulates';
}
