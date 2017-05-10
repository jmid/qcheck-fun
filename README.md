QCheck-fun: Fun examples for exercising QCheck's function generator
===================================================================

This is a collection of examples from
 - JSVerify https://github.com/jsverify/jsverify
 - Koen Claessen's Haskell'12 pearl "Shrinking and Showing Functions"
 - Ulf Norell's talk "Generating random functions"

Note: latest commit tests QCheck 6448cc4cc1bdf5b0ee169143fadef3004557638b

Some observations:
 - shrinking is much more effective
 - shrinking may occasionally take very long
 - some things could potentially be shrunk even further, e.g., the 7414 here:

    Test map filter failed (40 shrink steps):

    ({_ -> 0}, {7414 -> false; _ -> true}, [7414])


Old observations:
 - shrinking does not work as well over functions
     (counterexamples are even greater with int generator)
 - the printed functions are partial 
     (Claessen in Haskell and Norell's eqc_fun in Erlang prints them with a final _ catch all case) 
 - can one express Norell's prop_losing as a recursive invocation of QCheck_runner.run_tests?
