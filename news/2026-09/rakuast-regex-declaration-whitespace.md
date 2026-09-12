# Regex declaration trees preserve source whitespace boundaries

Regex and grammar declarations now retain the source-level whitespace markers
that RakuAST exposes, including adjacency around groups and the implicit final
rule term. Reconstructed rules lower their sigspace policy from that tree, so a
source-adjacent group is not changed into a whitespace-separated execution
pattern.
