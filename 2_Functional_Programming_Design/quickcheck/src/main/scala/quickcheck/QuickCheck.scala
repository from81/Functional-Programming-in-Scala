package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

/* You are given multiple implementations of IntHeaps in file src/main/scala/quickcheck/Heap.scala.
Only one of these is correct, while the other ones have bugs.
Your goal is to write some properties that will be automatically checked.
All the properties you write should be satisfiable by the correct implementation,
while at least one of them should fail in each incorrect implementation, thus revealing it's buggy*/
abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    heap <- oneOf(genHeap, Gen.const(empty))
  } yield insert(i, heap)
  given Arbitrary[H] = Arbitrary(genHeap)

  // for any heap, adding the minimal element, and then finding it, should return the element in question
  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // adding a single element to an empty heap, and then removing this element, should yield the element in question
  property("min1") = forAll { (a: Int, h: H) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("insert 2 distinct values and get minimum should return the lower value") = forAll { (x: Int, y: Int) =>
    val h = insert(x, insert(y, empty))
    if x >= y then findMin(h) == y else findMin(h) == x
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("insert one element into an empty map, delete the min, should result in empty heap") =
    forAll { (x: Int) =>
      deleteMin(insert(x, empty)) == empty
    }

  def popMin(h: H): List[Int] = if !isEmpty(h) then findMin(h) :: popMin(deleteMin(h)) else Nil

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("finding and deleting minimum should yield sorted sequence of elements") = forAll {
    (x: Int, y: Int, z: Int) =>
      popMin(insert(x, insert(y, insert(z, empty)))) == List(x, y, z).sorted
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other") = forAll {
    (h1: H, h2: H, x: Int, y: Int) =>
      if isEmpty(h1) && isEmpty(h2) then findMin(meld(insert(x, h1), insert(y, h2))) == List(x, y).min
      else if isEmpty(h1) then findMin(meld(insert(x, h1), h2)) == List(findMin(h1), findMin(h2), x).min
      else if isEmpty(h2) then findMin(meld(h1, insert(x, h2))) == List(findMin(h1), findMin(h2), x).min
      else findMin(meld(h1, h2)) == List(findMin(h1), findMin(h2)).min
  }
