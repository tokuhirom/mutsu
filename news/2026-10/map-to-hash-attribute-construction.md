# `%` attributes initialized from Map hold a Hash

Passing a `Map` to a `%`-sigiled attribute through `.new` or the default constructor now copies its entries into a plain `Hash`, matching `Hash.STORE` (#13886).
