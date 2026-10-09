# Mu.clone and Mu.new are method-table rows

The base candidates a user `clone` or `new` override reaches through `callsame`/`nextsame`/`nextwith` are rows on `Mu`, so
`native_mu_base_next_candidate` only asks the table (ADR-11276 §9.48, part of #12423).
