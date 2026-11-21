# Exports user candidates of core comparison operators for a type of its own,
# as Red does for its AST nodes (#18478).
unit module JunctionOpCandidates;
role JOCNode { }
multi infix:<eq>(JOCNode $a, Str() $b) is export { 'node-eq' }
multi infix:<eq>(Str() $a, JOCNode $b) is export { 'node-eq' }
multi infix:<ne>(JOCNode $a, $b) is export { 'node-ne' }
multi infix:<lt>(JOCNode $a, $b) is export { 'node-lt' }
multi infix:<==>(JOCNode $a, Numeric() $b) is export { 'node-==' }
multi infix:<!=>(JOCNode $a, $b) is export { 'node-!=' }
multi infix:«<»(JOCNode $a, $b) is export { 'node-<' }
multi infix:<~>(JOCNode $a, $b) is export { 'node-~' }
