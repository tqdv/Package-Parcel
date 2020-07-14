unit module Tilwa::WithVal;

#|[ Used to pass named arguments only if they are defined. It is tighter than a comma, so you can omit the parentheses
Usage: `|with-val` followed by a pair
  f($a, |with-val b => $value, $c)
  g $α, |with-val :$β
Rationale: if you omit this operator, you may pass type objects (::U) instead of nothing
]
sub prefix:<with-val> (Pair:D $pair) is equiv(&prefix:<so>) is export {
	$pair with $pair.value orelse Empty
}

#|[ Same as with-val, but for positional arguments
Usage:
  f $a, |if-def $b, $c
]
sub prefix:<if-def> ($v) is equiv(&prefix:<so>) is export {
	$v with $v orelse Empty
}