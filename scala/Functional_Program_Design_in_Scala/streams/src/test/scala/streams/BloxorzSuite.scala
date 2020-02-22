package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
      * This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves
      * is a valid solution, i.e. leads to the goal.
      */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/


    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 0") {

    new Level1 {

      override val level =
        """oooo
          |oooo
          |oSoo
          |oooo""".stripMargin
      assert(startPos == Pos(2, 1))
    }
  }

  test("terrain function level 1") {

    new Level1 {

      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal T
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(!terrain(Pos(5, 0)), "5,9")
      assert(terrain(Pos(5, 6)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
    }
  }


  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }

  test("findChar goal") {
    new Level1 {
      assert(goal == Pos(4, 7))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      val l=solve(solution)
      assert(l == Block(goal, goal))
    }
  }


  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("neighborsWithHistory") {
    new Level1 {
      val k = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up))
      assert(k.toSet == Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ))
    }
  }

  test("newNeighborsOnly") {
    new Level1 {
      //  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])], explored: Set[Block]): Stream[(Block, List[Move])] = ???
      val l = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        ).toStream,

        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))
      )

      assert(l.toSet ==
        Set((Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))
      )
    }
  }

  test("solution2") {

    new Level1 {

      override val level =
        """oST
          |ooo
          |ooo""".stripMargin

      assert(startPos == Pos(0, 1))
      assert(goal == Pos(0, 2))

      val res = pathsToGoal.toSet

//      res.toList.foreach(z => println(s"res $z"))
      assert(res
        === Stream(
        (Block(Pos(0, 2), Pos(0, 2)), List(Up, Right, Down))

      ).toSet
      )

    }
  }
  test("solution") {
    new Level1 {

      val res = pathsToGoal.toSet
//      res.toList.foreach(z => println(s"res $z"))
      assert(res
        === Stream(
        (Block(Pos(4, 7), Pos(4, 7)), List(Down, Right, Right, Right, Down, Right, Right))
        , (Block(Pos(4, 7), Pos(4, 7)), List(Right, Down, Down, Right, Right, Down, Right))
        , (Block(Pos(4, 7), Pos(4, 7)), List(Right, Down, Right, Right, Down, Down, Right))
        , (Block(Pos(4, 7), Pos(4, 7)), List(Down, Right, Right, Right, Right, Right, Right, Down))

      ).toSet
      )

    }
  }

  test("pathsToGoal") {

    new Level1 {

      override val level =
        """oST
          |ooo
          |ooo""".stripMargin

      assert(startPos == Pos(0, 1))
      assert(goal == Pos(0, 2))

      val res = pathsToGoal.toSet

//      res.toList.foreach(z => println(s"res $z"))
      assert(res
        === Stream(
        (Block(Pos(0, 2), Pos(0, 2)), List(Up, Right, Down))

      ).toSet
      )

    }
  }

  test("pathsFromStart") {

    new Level1 {

      override val level =
        """oSo
          |ooo
          |ooo""".stripMargin

      assert(startPos == Pos(0, 1))

      val res = pathsFromStart.toSet

//      res.toList.foreach(z => println(s"res $z"))
      assert(res
        === Stream(
        (Block(Pos(0, 1), Pos(0, 1)), List()),
        (Block(Pos(1, 1), Pos(2, 1)), List(Down)),
        (Block(Pos(1, 0), Pos(2, 0)), List(Left, Down)),
        (Block(Pos(1, 2), Pos(2, 2)), List(Right, Down)),
        (Block(Pos(0, 0), Pos(0, 0)), List(Up, Left, Down)),
        (Block(Pos(0, 2), Pos(0, 2)), List(Up, Right, Down)),
        (Block(Pos(0, 1), Pos(0, 2)), List(Right, Up, Left, Down)),
        (Block(Pos(0, 0), Pos(0, 1)), List(Left, Up, Right, Down)),
        (Block(Pos(1, 1), Pos(1, 2)), List(Down, Right, Up, Left, Down)),
        (Block(Pos(1, 0), Pos(1, 1)), List(Down, Left, Up, Right, Down)),
        (Block(Pos(1, 0), Pos(1, 0)), List(Left, Down, Right, Up, Left, Down)),
        (Block(Pos(2, 1), Pos(2, 2)), List(Down, Down, Right, Up, Left, Down)),
        (Block(Pos(1, 2), Pos(1, 2)), List(Right, Down, Left, Up, Right, Down)),
        (Block(Pos(2, 0), Pos(2, 1)), List(Down, Down, Left, Up, Right, Down)),
        (Block(Pos(2, 0), Pos(2, 0)), List(Left, Down, Down, Right, Up, Left, Down)),
        (Block(Pos(2, 2), Pos(2, 2)), List(Right, Down, Down, Left, Up, Right, Down)),
        (Block(Pos(0, 0), Pos(1, 0)), List(Up, Left, Down, Down, Right, Up, Left, Down)),
        (Block(Pos(0, 2), Pos(1, 2)), List(Up, Right, Down, Down, Left, Up, Right, Down)),
        (Block(Pos(0, 1), Pos(1, 1)), List(Right, Up, Left, Down, Down, Right, Up, Left, Down)),
        (Block(Pos(0, 1), Pos(1, 1)), List(Left, Up, Right, Down, Down, Left, Up, Right, Down)),
        (Block(Pos(2, 1), Pos(2, 1)), List(Down, Right, Up, Left, Down, Down, Right, Up, Left, Down)),
        (Block(Pos(2, 1), Pos(2, 1)), List(Down, Left, Up, Right, Down, Down, Left, Up, Right, Down))


      ).toSet
      )

    }
  }

  test("from2") {

    new Level1 {

      override val level =
        """oSo
          |ooo
          |ooo""".stripMargin

      val res = from(
        Stream((Block(Pos(0, 1), Pos(0, 1)), List())),
        Set())

      val res2=res.sortBy(a=>a.toString().length)

//      res.toList.foreach(z => println(s"res $z"))
      assert(res.toSet
        === Stream(
        (Block(Pos(0, 1), Pos(0, 1)), List()),
        (Block(Pos(1, 1), Pos(2, 1)), List(Down)),
        (Block(Pos(1, 0), Pos(2, 0)), List(Left, Down)),
        (Block(Pos(1, 2), Pos(2, 2)), List(Right, Down)),
        (Block(Pos(0, 0), Pos(0, 0)), List(Up, Left, Down)),
        (Block(Pos(0, 2), Pos(0, 2)), List(Up, Right, Down)),
        (Block(Pos(0, 1), Pos(0, 2)), List(Right, Up, Left, Down)),
        (Block(Pos(0, 0), Pos(0, 1)), List(Left, Up, Right, Down)),
        (Block(Pos(1, 1), Pos(1, 2)), List(Down, Right, Up, Left, Down)),
        (Block(Pos(1, 0), Pos(1, 1)), List(Down, Left, Up, Right, Down)),
        (Block(Pos(1, 0), Pos(1, 0)), List(Left, Down, Right, Up, Left, Down)),
        (Block(Pos(2, 1), Pos(2, 2)), List(Down, Down, Right, Up, Left, Down)),
        (Block(Pos(1, 2), Pos(1, 2)), List(Right, Down, Left, Up, Right, Down)),
        (Block(Pos(2, 0), Pos(2, 1)), List(Down, Down, Left, Up, Right, Down)),
        (Block(Pos(2, 0), Pos(2, 0)), List(Left, Down, Down, Right, Up, Left, Down)),
        (Block(Pos(2, 2), Pos(2, 2)), List(Right, Down, Down, Left, Up, Right, Down)),
        (Block(Pos(0, 0), Pos(1, 0)), List(Up, Left, Down, Down, Right, Up, Left, Down)),
        (Block(Pos(0, 2), Pos(1, 2)), List(Up, Right, Down, Down, Left, Up, Right, Down)),
        (Block(Pos(0, 1), Pos(1, 1)), List(Right, Up, Left, Down, Down, Right, Up, Left, Down)),
        (Block(Pos(0, 1), Pos(1, 1)), List(Left, Up, Right, Down, Down, Left, Up, Right, Down)),
        (Block(Pos(2, 1), Pos(2, 1)), List(Down, Right, Up, Left, Down, Down, Right, Up, Left, Down)),
        (Block(Pos(2, 1), Pos(2, 1)), List(Down, Left, Up, Right, Down, Down, Left, Up, Right, Down))


      ).toSet
      )

    }
  }

}
