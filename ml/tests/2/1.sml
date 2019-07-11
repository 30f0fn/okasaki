
use "../../src/2/1.sml";

val test_data = [
    ([],[[]]),
    ([1],[[1],[]]),
    ([1,2],[[1,2],[2],[]]),
    ([1,2,3], [[1,2,3],[2,3],[3],[]])
    ]

val easy =  suffixes [1,2];

fun run_tests f [] = (print "passed all tests!\n";
                     [])
  | run_tests f (tests as ((input, expected) :: remaining_tests)) = 
    let
        val actual = f input
    in
        if actual = expected then run_tests f remaining_tests
        else (print "failed a test!\n";
              tests)
    end

val main = run_tests suffixes test_data
