QCheck-fun: Fun examples for exercising QCheck's function generator
===================================================================

This is a collection of examples from
 - JSVerify https://github.com/jsverify/jsverify
 - Koen Claessen's Haskell'12 pearl "Shrinking and Showing Functions"
 - Ulf Norell's talk "Generating random functions"


Some observations:
 - shrinking does not work as well over functions
     (counterexamples are even greater with int generator)
 - the printed functions are partial 
     (Claessen in Haskell and Norell's eqc_fun in Erlang prints them with a final _ catch all case) 
 - can one express Norell's prop_losing as a recursive invocation of QCheck_runner.run_tests?
