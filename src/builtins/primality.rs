use num_bigint::BigInt;
use num_traits::{One, Signed, ToPrimitive, Zero};

/// Check if an i64 is prime using trial division.
pub(crate) fn is_prime_i64(n: i64) -> bool {
    if n < 2 {
        return false;
    }
    if n < 4 {
        return true;
    }
    if n % 2 == 0 || n % 3 == 0 {
        return false;
    }
    let mut i = 5i64;
    while i.saturating_mul(i) <= n {
        if n % i == 0 || n % (i + 2) == 0 {
            return false;
        }
        i += 6;
    }
    true
}

/// Check if a BigInt is prime using Miller-Rabin probabilistic test.
pub(crate) fn is_prime_bigint(n: &BigInt) -> bool {
    let n = n.abs();
    if n < BigInt::from(2) {
        return false;
    }
    // Try to fit in i64 for small numbers
    if let Some(small) = n.to_i64() {
        return is_prime_i64(small);
    }

    // Check small prime divisors first
    let small_primes: &[u64] = &[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47];
    for &p in small_primes {
        let bp = BigInt::from(p);
        if n == bp {
            return true;
        }
        if (&n % &bp).is_zero() {
            return false;
        }
    }

    // Miller-Rabin primality test
    // Write n-1 as 2^r * d
    let one = BigInt::one();
    let two = BigInt::from(2);
    let n_minus_1 = &n - &one;
    let mut d = n_minus_1.clone();
    let mut r = 0u64;
    while (&d % &two).is_zero() {
        d /= &two;
        r += 1;
    }

    // Deterministic witnesses sufficient for very large numbers
    let witnesses: &[u64] = &[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37];

    'witness: for &a in witnesses {
        let a = BigInt::from(a);
        if a >= n {
            continue;
        }

        let mut x = a.modpow(&d, &n);

        if x == one || x == n_minus_1 {
            continue;
        }

        for _ in 0..r - 1 {
            x = x.modpow(&two, &n);
            if x == n_minus_1 {
                continue 'witness;
            }
        }
        return false;
    }
    true
}
