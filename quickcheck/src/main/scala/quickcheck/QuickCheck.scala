package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("minimum of two elements") = forAll { (elem1: Int, elem2: Int) =>
    val x = insert(elem2, insert(elem1, empty))
    val smallOne = if (elem1 < elem2) elem1 else elem2
    findMin(x) == smallOne
  }

  //If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("deleting the minimum after inserted") = forAll{ (elem: Int) =>
    val x = insert(elem,empty)
    val h = deleteMin(x)
    h == empty
  }

  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  // (Hint: recursion and helper functions are your friends.)

  property("sorted sequence after deleting minima") = forAll { x: H =>
      def remMin(xs: H, ys: List[Int]): List[Int] = {
        if (isEmpty(xs)) ys
        else findMin(xs) :: remMin(deleteMin(xs), ys)
      }
      val xs = remMin(x, Nil)
      xs == xs.sorted
  }

  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("minimum of the melding") = forAll { (heap1: H, heap2: H) =>
    val min1 = findMin(heap1)
    val min2 = findMin(heap2)
    val m = meld(heap1, heap2)
    val minMeld = findMin(m)
    minMeld == min1 || minMeld == min2
  }

  // Take two arbitrary heaps, meld together. Then remove min from 1 and insert into 2, meld the results.
  // Compare two melds by comparing sequences of ranks.
  property("min from 1 and insert into 2") = forAll { (heap1: H, heap2: H) =>
    def remMin(ts: H, as: List[Int]): List[Int] = {
      if (isEmpty(ts)) as
      else findMin(ts) :: remMin(deleteMin(ts), as)
    }
    val meld1 = meld(heap1, heap2)
    val min1 = findMin(heap1)
    val meld2 = meld(deleteMin(heap1), insert(min1, heap2))
    val xs1 = remMin(meld1, Nil)
    val xs2 = remMin(meld2, Nil)
    xs1 == xs2
  }

}
