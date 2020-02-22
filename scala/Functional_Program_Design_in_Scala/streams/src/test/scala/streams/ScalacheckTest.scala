package streams

import org.scalacheck.Gen.choose
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}

object ScalacheckTest extends Properties("ScalacheckTest") with GameDef {

  val T = 3
  lazy val genConsumer: Gen[(Block, Block)] = for {
    row1 <- choose(-T, T)
    col1 <- choose(-T, T)

    row2 <- choose(row1, row1 + T)
    col2 <- choose(col1, col1 + T)

    row3 <- choose(-T, T)
    col3 <- choose(-T, T)

    row4 <- choose(row3, row3 + T)
    col4 <- choose(col3, col3 + T)

  } yield (Block(Pos(row1, col1), Pos(row2, col2)), Block(Pos(row3, col3), Pos(row4, col4)))

  implicit lazy val arbConsumer: Arbitrary[(ScalacheckTest.Block, ScalacheckTest.Block)] = Arbitrary(genConsumer)

  property("equals prop") = Prop.forAll { a: (Block, Block) =>
    //    println(a)
    val (block1, block2) = a

    (block1 == block2 &&
      block1.b1.row == block2.b1.row && block1.b1.col == block2.b1.col &&
      block1.b2.row == block2.b2.row && block1.b2.col == block2.b2.col) || (block1 != block2 && (
      block1.b1.row != block2.b1.row || block1.b1.col != block2.b1.col ||
        block1.b2.row != block2.b2.row || block1.b2.col != block2.b2.col))
  }

  override val startPos = Pos(1, 1)

  override val goal = Pos(1, 1)

  override val terrain: Terrain = (pos: Pos) => true
}



