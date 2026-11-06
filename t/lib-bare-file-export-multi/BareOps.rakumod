class BareOps { has $.v }
multi infix:<*>(BareOps $c, Real $r) is export { BareOps.new(v => $c.v * $r) }
