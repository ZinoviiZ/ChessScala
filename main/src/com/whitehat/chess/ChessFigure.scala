package com.whitehat.chess

import scala.util.{Failure, Success, Try}

/** Representing board square on the chess board*/
sealed abstract class BoardSquare(label: Char) {
  override def toString: String = label.toString
}

/** Board square which doesn't have chess figure*/
object EmptySquare extends BoardSquare(' ')

/** Representing chess figure on the chess board*/
sealed abstract class ChessFigure(val coordinate: Coordinate, val color: Color, initialLabel: Char) extends BoardSquare((initialLabel + color.asciiDiff).toChar) {

  /**
    * Check and rebuild new chess board with move
    * @param to Coordinate to move
    * @param board Chess board before the move
    * @return New chess board if move is valid otherwise exception
    */
  def move(to: Coordinate, board: ChessBoard): Try[ChessBoard] = {
    for {
      _ <- canMoveAt(to, board)
      boardAfterMove = rebuildBoardWithMove(to, board)
      _ <- checkIfBoardStatusChanged(board, boardAfterMove)
    } yield boardAfterMove
  }

  /**
    * Compare board before and after move and indentify if it has fixed Chess check (if it was)
    * @param prevBoard Board before the move
    * @param boardAfterMove Board after move
    * @return
    */
  def checkIfBoardStatusChanged(prevBoard: ChessBoard, boardAfterMove: ChessBoard): Try[Unit] = {
    if(prevBoard.kingIsUnderAttack.getOrElse(color, false) && boardAfterMove.kingIsUnderAttack.getOrElse(color, false)) {
      Failure(new RuntimeException("Turn is not fixed check"))
    } else if(!prevBoard.kingIsUnderAttack.getOrElse(color, false) && boardAfterMove.kingIsUnderAttack.getOrElse(color, false)) {
      Failure(new RuntimeException("After this turn alien King is under attack"))
    } else {
      Success()
    }
  }

  /**
    * Rebuild new board with move
    * @param to Coordinate to move
    * @param board Chess board before move
    * @return Chess board with move. It's new object.
    */
  def rebuildBoardWithMove(to: Coordinate, board: ChessBoard): ChessBoard = {
    val boardAfterMove = board.squares.map(_.clone)
    boardAfterMove(coordinate.y)(coordinate.x) = EmptySquare
    boardAfterMove(to.y)(to.x) = buildFigureWithNewCoordinate(to)
    ChessBoard(boardAfterMove, color)
  }

  /** Build possible attacking region for current figure on the chess board and check if move within this region */
  def canMoveAt(to: Coordinate, board: ChessBoard): Try[Unit] = {
    if(findAttackingRegion(board).contains(to)) {
      Success()
    } else {
      Failure(new RuntimeException(s"Figure $this $coordinate is not able to reach coordinate [$to]"))
    }
  }

  /** abstract method for recreating this figure with new coordinate*/
  def buildFigureWithNewCoordinate(newCoordinate: Coordinate): ChessFigure

  /** Find all coordinates within attacking region*/
  def findAttackingRegion(board: ChessBoard): Set[Coordinate] = {
    val attackingCoordinates = attackingRegion(board)
    filterNotAlienCoordinates(attackingCoordinates, board)
  }

  protected def attackingRegion(board: ChessBoard): Set[Coordinate]

  /**
    * Gathering all possible coordinates to move, from starting coordinate
    * @param start Starting coordinate
    * @param board Chess board
    * @param nextCoordinateFunc Function for recreating next coordinate from previous one
    * @return Set of coordinates of attacking region
    */
  def exploreAttackingRegion(start: Coordinate, board: ChessBoard)(nextCoordinateFunc: Coordinate => Coordinate): Set[Coordinate] = {
    val region = scala.collection.mutable.Set.empty[Coordinate]
    var stop = false
    var cur = nextCoordinateFunc(start)
    do {
      board.atCoordinate(cur) match {
        case None => stop = true
        case Some(figure: ChessFigure) =>
          if(figure.color != color) region.add(cur)
          stop = true
        case _ => region.add(cur)
      }
      cur = nextCoordinateFunc(cur)
    } while(!stop)
    region.toSet
  }

  /** Filtering Alien coordinates*/
  def filterNotAlienCoordinates(coordinates: Set[Coordinate], board: ChessBoard): Set[Coordinate] = {
    coordinates.filter { c =>
      board.atCoordinate(c) match {
        case None => false
        case Some(figure: ChessFigure) if figure.color ==  color => false
        case _ => true
      }
    }
  }
}

/** Representing Chess Pawn with its moving logic*/
case class Pawn(override val coordinate: Coordinate, override val color: Color) extends ChessFigure(coordinate, color, 'p') {
  override def buildFigureWithNewCoordinate(newCoordinate: Coordinate): ChessFigure = Pawn(newCoordinate, color)

  override def attackingRegion(board: ChessBoard): Set[Coordinate] = {

    val possibleMoves = if(color == White) {
      Set(coordinate.up) ++ {
        if(board.isEnemyOrEmptyAt(coordinate.leftUp, color)) Set(coordinate.leftUp) else Set.empty[Coordinate]
      } ++ {
        if(board.isEnemyOrEmptyAt(coordinate.rightUp, color)) Set(coordinate.rightUp) else Set.empty[Coordinate]
      } ++ {
        if(isOnStartPosition) Set(coordinate.up.up) else Set.empty[Coordinate]
      }
    } else {
      Set(coordinate.down) ++ {
        if(board.isEnemyOrEmptyAt(coordinate.leftDown, color)) Set(coordinate.leftDown) else Set.empty[Coordinate]
      } ++ {
        if(board.isEnemyOrEmptyAt(coordinate.rightDown, color)) Set(coordinate.rightDown) else Set.empty[Coordinate]
      } ++ {
        if(isOnStartPosition) Set(coordinate.down.down) else Set.empty[Coordinate]
      }
    }

    filterNotAlienCoordinates(possibleMoves, board)
  }

  def isOnStartPosition: Boolean = if(color == White) coordinate.y == 1 else coordinate.y == 6
}

/** Representing Chess Rook with its moving logic*/
case class Rook(override val coordinate: Coordinate, override val color: Color) extends ChessFigure(coordinate, color, 'r') {

  override def buildFigureWithNewCoordinate(newCoordinate: Coordinate): ChessFigure = Rook(newCoordinate, color)

  override def attackingRegion(board: ChessBoard): Set[Coordinate] = {
    val leftRegion = exploreAttackingRegion(coordinate, board)(c => c.left)
    val rightRegion = exploreAttackingRegion(coordinate, board)(c => c.right)
    val upRegion = exploreAttackingRegion(coordinate, board)(c => c.up)
    val downRegion = exploreAttackingRegion(coordinate, board)(c => c.down)
    leftRegion ++ rightRegion ++ upRegion ++ downRegion
  }
}

/** Representing Chess Knight with its moving logic*/
case class Knight(override val coordinate: Coordinate, override val color: Color) extends ChessFigure(coordinate, color, 'n') {
  override def buildFigureWithNewCoordinate(newCoordinate: Coordinate): ChessFigure = Knight(newCoordinate, color)

  override def attackingRegion(board: ChessBoard): Set[Coordinate] = {
    val possibleMoves = Set(
      coordinate.up.up.left, coordinate.up.up.right, coordinate.left.left.up, coordinate.left.left.down,
      coordinate.right.right.up, coordinate.right.right.down, coordinate.down.down.left, coordinate.down.down.right
    )
    filterNotAlienCoordinates(possibleMoves, board)
  }
}

/** Representing Chess Bishop with its moving logic*/
case class Bishop(override val coordinate: Coordinate, override val color: Color) extends ChessFigure(coordinate, color, 'b') {
  override def buildFigureWithNewCoordinate(newCoordinate: Coordinate): ChessFigure = Bishop(newCoordinate, color)

  override def attackingRegion(board: ChessBoard): Set[Coordinate] = {
    val leftUpRegion = exploreAttackingRegion(coordinate, board)(c => c.leftUp)
    val rightUpRegion = exploreAttackingRegion(coordinate, board)(c => c.rightUp)
    val rightDownRegion = exploreAttackingRegion(coordinate, board)(c => c.rightDown)
    val leftDownRegion = exploreAttackingRegion(coordinate, board)(c => c.leftDown)
    leftUpRegion ++ rightUpRegion ++ rightDownRegion ++ leftDownRegion
  }
}

/** Representing Chess Queen with its moving logic*/
case class Queen(override val coordinate: Coordinate, override val color: Color) extends ChessFigure(coordinate, color, 'q') {
  override def buildFigureWithNewCoordinate(newCoordinate: Coordinate): ChessFigure = Queen(newCoordinate, color)

  override def attackingRegion(board: ChessBoard): Set[Coordinate] = {
    val leftRegion = exploreAttackingRegion(coordinate, board)(c => c.left)
    val rightRegion = exploreAttackingRegion(coordinate, board)(c => c.right)
    val upRegion = exploreAttackingRegion(coordinate, board)(c => c.up)
    val downRegion = exploreAttackingRegion(coordinate, board)(c => c.down)
    val leftUpRegion = exploreAttackingRegion(coordinate, board)(c => c.leftUp)
    val rightUpRegion = exploreAttackingRegion(coordinate, board)(c => c.rightUp)
    val rightDownRegion = exploreAttackingRegion(coordinate, board)(c => c.rightDown)
    val leftDownRegion = exploreAttackingRegion(coordinate, board)(c => c.leftDown)
    leftRegion ++ rightRegion ++ upRegion ++ downRegion ++ leftUpRegion ++ rightUpRegion ++ rightDownRegion ++ leftDownRegion
  }
}

/** Representing Chess King with its moving logic*/
case class King(override val coordinate: Coordinate, override val color: Color) extends ChessFigure(coordinate, color, 'k') {
  override def buildFigureWithNewCoordinate(newCoordinate: Coordinate): ChessFigure = King(newCoordinate, color)

  override def attackingRegion(board: ChessBoard): Set[Coordinate] = {
    val possibleMoves = Set(
      coordinate.up,coordinate.left, coordinate.right, coordinate.down,
      coordinate.leftUp, coordinate.leftDown, coordinate.rightUp, coordinate.rightDown
    )
    filterNotAlienCoordinates(possibleMoves, board)
  }
}

