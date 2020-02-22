package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(empty: Gen[H], genHeap)
  } yield insert(a, h)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (a: A, b: A) =>
    findMin(insert(b, insert(a, empty))) == (if (a < b) a else b)
  }

  property("gen3") = forAll { (a: A) =>
    deleteMin(insert(a, empty)) == empty
  }

  
  property("gen4") = forAll { (h: H) =>

    def issorted(h1: A, t: H): Boolean = {
      if (t == Nil) true else {
        val o = findMin(t)
        if (o < h1) false else {
          issorted(o, deleteMin(t))
        }
      }
    }

    if (h == Nil) true else
      issorted(findMin(h), deleteMin(h))

  }

  property("gen5") = forAll { (a: H, b: H) =>
    val x = findMin(meld(a, b))
    x == findMin(a) || x == findMin(b)
  }


}
