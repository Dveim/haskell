import Math.NumberTheory.Prime -- stop re-invent the wheel
perfects = [2^(p-1)*(2^p - 1) | p <- primes, isPrime (2^p - 1)]