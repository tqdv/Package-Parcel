unit module Tilwa::ListInsert;

#|[ Insert elements @elts into the array $a starting at position $pos.
The newly inserted elements push the others to the right.
This is equivalent to splicing at a position without deleting any elements, but clearer. ]
multi sub insert (Array:D $a, Int $pos, **@elts) is export {
	unless @elts { return }
	$a.splice: $pos, 0, @elts
}
multi sub insert (Array:D $a, &pos, **@elts) is export {
	unless @elts { return }
	$a.splice: pos($a.elems), 0, @elts
}
multi sub insert (Array:D $a, Whatever $, **@elts) is export {
	unless @elts { return }
	$a.splice: $a.elems, 0, @elts
}
