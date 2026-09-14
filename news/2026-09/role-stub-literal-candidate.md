# Literal multi candidates stay distinct from role stubs

mutsu now keeps literal-constrained multi candidates such as `prepare("")`
distinct from general type candidates when satisfying role requirements and
dispatching methods. This allows `Red::Driver::Cache::Memory` from Red 0.2.5
to load past its role-composition conflict.
