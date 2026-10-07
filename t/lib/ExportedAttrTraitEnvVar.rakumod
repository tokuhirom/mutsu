unit module ExportedAttrTraitEnvVar;

multi sub trait_mod:<is>(Variable $v, :%env) is export { $v.var = 'hash' }
multi sub trait_mod:<is>(Variable $v, :$env) is export { $v.var = 'scalar' }
