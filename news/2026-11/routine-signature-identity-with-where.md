# A routine with a `where` clause keeps one Signature object again

#18590 gave a `:(...)` signature literal whose parameters carry code a
signature per closure value, so its `ACCEPTS` binds through the literal's own
scope. It recognised that literal as "a routine with an empty body", but an
ordinary compiled routine has an empty AST body too (its body lives only in
its bytecode), so every routine with a `where` clause or computed default
lost the per-declaration signature cache: each `.signature` read returned a
new object. The `static` distribution keys parameters by identity
(`RakuAST::Parameter.meta-object` against `$candidate.signature.params`) and
reported every constrained multi candidate as unverifiable (#18642). The
literal is now recognised by also having no compiled routine body, and
`t/routines/signature/signature-identity-routine-with-where.t` pins the
identity.
