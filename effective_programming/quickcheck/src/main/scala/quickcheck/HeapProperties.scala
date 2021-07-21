package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.*

class HeapProperties(heapInterface: HeapInterface) extends Properties("Heap"):
  
  // Import all the operations of the `HeapInterface` (e.g., `empty`
  // `insert`, etc.)
  import heapInterface.*


  // Examples of properties
  property("inserting the minimal element and then finding it should return the same minimal element") =
    forAll { (heap: List[Node]) =>
      val min = if isEmpty(heap) then 0 else findMin(heap)
      findMin(insert(min, heap)) == min
    }

  property("the minimum of a heap of two elements should be the smallest of the two elements") =
    forAll { (x1: Int, x2: Int) =>
      val heap = insert(x2, insert(x1, empty))
      val min: Int = Math.min(x1, x2)
      findMin(heap) == min
    }

  property("delete minumum of heap of one element should return an empty heap") =
    forAll { (x: Int) =>
      // create a heap with exactly one element, `x`
      val heap1: List[Node] = insert(x, empty)
      // delete the minimal element from it
      val heap0: List[Node] = deleteMin(heap1)
      // check that heap0 is empty
      isEmpty(heap0)
    }

  property("continually finding and deleting the minimal element of a heap should return a sorted sequence") =
    // recursively traverse the heap
    def check(heap: List[Node]): Boolean =
      // if the heap is empty, or if it has just one element, we have
      // successfully finished our checks
      if isEmpty(heap) || isEmpty(deleteMin(heap)) then
        true
      else
        // find the minimal element
        val x1: Int = findMin(heap)
        // delete the minimal element of `heap`
        val heap2: List[Node] = deleteMin(heap)
        // find the minimal element in `heap2`
        val x2: Int = findMin(heap2)
        // check that the deleted element is smaller than the minimal element
        // of the remaining heap, and that the remaining heap verifies the
        // same property (by recursively calling `check`)
        val checked: Boolean = x1 <= x2
        checked
    // check arbitrary heaps
    forAll { (heap: List[Node]) =>
      check(heap)
    }

  // for any heap, adding the minimal element, and then finding it, should return the element in question
  property("gen1") = forAll { (heap: List[Node]) =>
    val m = if isEmpty(heap) then 0 else findMin(heap)
    findMin(insert(m, heap)) == m
  }

  // adding a single element to an empty heap, and then removing this element, should yield the element in question
  property("min1") = forAll { (a: Int, heap: List[Node]) =>
    val heap = insert(a, empty)
    findMin(heap) == a
  }

  // If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("insert 2 distinct values and get minimum should return the lower value") = forAll { (x: Int, y: Int) =>
    val heap = insert(x, insert(y, empty))
    if x >= y then findMin(heap) == y else findMin(heap) == x
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("insert one element into an empty map, delete the min, should result in empty heap") =
    forAll { (x: Int) =>
      deleteMin(insert(x, empty)) == empty
    }


  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("finding and deleting minimum should yield sorted sequence of elements") = forAll {
    (x: Int, y: Int, z: Int) =>
      def popMin(heap: List[Node]): List[Int] =
        if !isEmpty(heap) then findMin(heap) :: popMin(deleteMin(heap))
        else Nil
      popMin(insert(x, insert(y, insert(z, empty)))) == List(x, y, z).sorted
  }
  

  // random heap generator --- DO NOT MODIFY
  private lazy val genHeap: Gen[List[Node]] = oneOf(const(empty),
    for
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    yield insert(v, h)
  )

  private given Arbitrary[List[Node]] = Arbitrary(genHeap)
  
end HeapProperties
