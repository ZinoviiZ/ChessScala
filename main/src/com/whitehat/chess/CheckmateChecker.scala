package com.whitehat.chess

import scala.util.{Failure, Success}

/** Trait for implementing checking checkmate on the board*/
trait CheckmateChecker {
  def check(color: Color, board: ChessBoard): Boolean
}

/** Simple implementation of CheckmateChecker, which try to simulate every possible move and
  * check if check can be handled*/
object SimpleCheckmateChecker extends CheckmateChecker {

  def check(color: Color, board: ChessBoard): Boolean = {
    var isStillCheck = board.kingIsUnderAttack(color)
    for {
      figure <- board.color2Figures(color) if isStillCheck
      possibleMove <- figure.findAttackingRegion(board) if isStillCheck
    } yield {
      figure.move(possibleMove, board) match {
        case Success(boardAfterMove) => isStillCheck = isStillCheck && boardAfterMove.kingIsUnderAttack(color)
        case Failure(th) => ()
      }
    }
    isStillCheck
  }
}
