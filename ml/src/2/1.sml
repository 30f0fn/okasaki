(* 
write a function suffixes of type 'a list -> 'a list list which takes a list xs and returns a list of all suffixes of xs in decreasing order of length.  show that the resulting list can be generated in O(n) time and represented in O(n) space. 
*)

fun suffixes [] = [[]]
  | suffixes (h :: t) = (h :: t) :: (suffixes t)

