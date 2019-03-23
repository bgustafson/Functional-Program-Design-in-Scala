package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  /*
   1) If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
   2) If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
   3) Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
   4) Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
   */

  lazy val genHeap: Gen[H] = for {
    k <- Gen.frequency((1, arbitrary[Int]))
    h <- oneOf(const(empty), genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("1 - find the smallest element") = forAll { a: Int =>
    val heap = insert(a, empty)
    findMin(heap) == a
  }

  property("2 - insert and then remove the elements to get an empty heap") = forAll { i: Int =>
    val heap = empty
    val heap2 = insert(i, heap)
    val heap3 = deleteMin(heap2)
    isEmpty(heap3)
  }

  property("3- sort the heap") = forAll { (heap: H, _: Int) =>
    sort(heap, List())
  }

  def sort(heap: H, list: List[Int]): Boolean = {
    if (isEmpty(heap)) list match {
      case Nil => true
      case x :: xs => xs.forall(x >= _)
    }
    else {
      val min = findMin(heap)
      val heap2 = deleteMin(heap)
      sort(heap2, min :: list)
    }
  }

  property("4 - melding sort of the heap") = forAll { (heap1: H, heap2: H) =>
    sort(meld(heap1, heap2), List())
  }

  property("5- melding find the smallest element") = forAll { (heap1: H, heap2: H) =>
    val min1 = findMin(heap1)
    val min2 = findMin(heap2)
    val melded = meld(heap1, heap2)
    val min3 = findMin(melded)
    min3 == min1 || min3 == min2
  }

  property("6 - does the heap contain the element") = forAll { (heap1: H, i: Int) =>
    val heap2 = insert(i, heap1)
    contains(heap2, i)
  }

  def contains(heap: H, x: Int): Boolean = {
    if (isEmpty(heap)) false
    else {
      val min = findMin(heap)
      if (min == x) true
      else contains(deleteMin(heap), x)
    }
  }
}
