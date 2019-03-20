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

  property("1 - minimum") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("2 - delete") = forAll { i: Int =>
    val h = empty
    val h2 = insert(i, h)
    val h3 = deleteMin(h2)
    isEmpty(h3)
  }

  property("3- sort") = forAll { (h: H, _: Int) =>
    sort(h, List())
  }

  def sort(h: H, list: List[Int]): Boolean = {
    if (isEmpty(h)) list match {
      case Nil => true
      case x :: xs => xs.forall(x >= _)
    }
    else {
      val m = findMin(h)
      val h2 = deleteMin(h)
      sort(h2, m :: list)
    }
  }

  property("4 - melding sort") = forAll { (h1: H, h2: H) =>
    sort(meld(h1, h2), List())
  }

  property("5- melding minimum") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val melded = meld(h1, h2)
    val m3 = findMin(melded)
    m3 == m1 || m3 == m2
  }

  property("6 - contains") = forAll { (h1: H, i: Int) =>
    val h2 = insert(i, h1)
    contains(h2, i)
  }

  def contains(h: H, x: Int): Boolean = {
    if (isEmpty(h)) false
    else {
      val m = findMin(h)
      if (m == x) true
      else contains(deleteMin(h), x)
    }
  }
}
