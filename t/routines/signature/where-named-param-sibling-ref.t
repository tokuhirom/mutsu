use Test;

# A `where` constraint on a named parameter must be able to reference the OTHER
# named parameters of the same signature, e.g. `:$line!, :$col! where $line == *`.
# Positional params already saw each other; named params did not, so a candidate
# whose `where` referenced a sibling named param never matched and multi dispatch
# failed with "Cannot resolve caller" / "No matching candidates".

plan 8;

# --- Whatever on the right, referencing a sibling named param ---
{
    multi f(:$line!, :$col! where $line == *) { 'eq' }
    multi f(:$line!, :$col! where $line >  *) { 'gt' }
    is f(:line(2), :col(2)), 'eq', 'where $line == * (sibling ref, Whatever right)';
    is f(:line(2), :col(1)), 'gt', 'where $line >  * (sibling ref, Whatever right)';
}

# --- Whatever on the left, referencing a sibling named param ---
{
    multi g(:$line!, :$col! where * == $line) { 'eq' }
    multi g(:$line!, :$col! where * <  $line) { 'lt' }
    is g(:line(5), :col(5)), 'eq', 'where * == $line (sibling ref, Whatever left)';
    is g(:line(5), :col(3)), 'lt', 'where * <  $line (sibling ref, Whatever left)';
}

# --- explicit block form referencing both params ---
{
    multi h(:$line!, :$col! where { $col <= $line }) { 'ok' }
    is h(:line(4), :col(2)), 'ok', 'where { $col <= $line } (explicit block, sibling ref)';
}

# --- full three-way dispatch as used by Math::PascalTriangle ---
{
    class Tri {
        proto method get(UInt:D() :$line!, UInt:D() :$col!) {*}
        multi method get(:$line!, :$col! where * == 0)     { 1 }
        multi method get(:$line!, :$col! where $line == *) { 1 }
        multi method get(:$line!, :$col! where $line > *)  {
            self.get(:line($line - 1), :$col) + self.get(:line($line - 1), :col($col - 1))
        }
    }
    is Tri.get(:line(2), :col(1)), 2,   'binomial C(2,1) via sibling-ref where dispatch';
    is Tri.get(:line(4), :col(2)), 6,   'binomial C(4,2)';
    is Tri.get(:line(9), :col(4)), 126, 'binomial C(9,4)';
}
