package com.whitehat.chess

import scala.collection.mutable
import scala.util.{Failure, Try}

case class ChessBoard(squares: Array[Array[BoardSquare]], played: Color = Black) {

  val color2Figures = buildColor2FiguresMap(squares)
  val color2ControlRegion = color2Figures.mapValues { figures =>
    figures.flatMap(_.findAttackingRegion(this))
  }

  val color2King: Map[Color, King]= {
    val map = mutable.Map.empty[Color, King]
    for(y <- squares.indices) {
      for(x <- squares.indices) {
        squares(y)(x) match {
          case king: King => map.put(king.color, king)
          case _ => ()
        }
      }
    }
    map.toMap
  }

  val kingIsUnderAttack: Map[Color, Boolean] = color2King.map {
    case (color, king) =>
      val enemyColor = color.enemyColor
      (color, color2ControlRegion(enemyColor).contains(king.coordinate))
  }

  val checkmate: Boolean = SimpleCheckmateChecker.check(played.enemyColor, this)

  def atCoordinate(coordinate: Coordinate): Option[BoardSquare] = {
    if(coordinate.y >= 0 && coordinate.y < squares.size && coordinate.x >= 0 && coordinate.x < squares(coordinate.y).size) {
      Some(squares(coordinate.y)(coordinate.x))
    } else None
  }

  def isEnemyOrEmptyAt(coordinate: Coordinate, color: Color): Boolean = {
    atCoordinate(coordinate) match {
      case figure: ChessFigure => color != figure.color
      case _ => true
    }
  }

  private def buildColor2FiguresMap(squares: Array[Array[BoardSquare]]): mutable.Map[Color, Set[ChessFigure]] = {
    val map = mutable.Map.empty[Color, Set[ChessFigure]]
    for(y <- squares.indices) {
      for(x <- squares(y).indices) {
        squares(y)(x) match {
          case EmptySquare => ()
          case figure: ChessFigure =>
            val others = map.getOrElse(figure.color, Set.empty[ChessFigure])
            map.put(figure.color, others + figure)
        }
      }
    }
    map
  }
}

object ChessBoard {

  /** Represent initial board setup*/
  val initialBoard: ChessBoard =
    ChessBoard(
      Array[Array[BoardSquare]](
        Array(
          Rook  (Coordinate(0,0), White),
          Knight(Coordinate(1,0), White),
          Bishop(Coordinate(2,0), White),
          Queen (Coordinate(3,0), White),
          King(Coordinate(4,0), White),
          Bishop(Coordinate(5,0), White),
          Knight(Coordinate(6,0), White),
          Rook  (Coordinate(7,0), White)
        ), Array(
          Pawn(Coordinate(0,1), White),
          Pawn(Coordinate(1,1), White),
          Pawn(Coordinate(2,1), White),
          Pawn(Coordinate(3,1), White),
          Pawn(Coordinate(4,1), White),
          Pawn(Coordinate(5,1), White),
          Pawn(Coordinate(6,1), White),
          Pawn(Coordinate(7,1), White)
        )
      ) ++ Array.fill[BoardSquare](4, 8)(EmptySquare) ++ Array[Array[BoardSquare]](
        Array(
          Pawn(Coordinate(0,6), Black),
          Pawn(Coordinate(1,6), Black),
          Pawn(Coordinate(2,6), Black),
          Pawn(Coordinate(3,6), Black),
          Pawn(Coordinate(4,6), Black),
          Pawn(Coordinate(5,6), Black),
          Pawn(Coordinate(6,6), Black),
          Pawn(Coordinate(7,6), Black)
        ),Array(
          Rook  (Coordinate(0,7), Black),
          Knight(Coordinate(1,7), Black),
          Bishop(Coordinate(2,7), Black),
          Queen (Coordinate(3,7), Black),
          King(Coordinate(4,7), Black),
          Bishop(Coordinate(5,7), Black),
          Knight(Coordinate(6,7), Black),
          Rook  (Coordinate(7,7), Black)
        )
      )
    )

  /**
    * Main method for handling move.
    * @param player Player who moves
    * @param move Move information (from to coordinate)
    * @param board Board before the move
    * @return If move was successful, it returns new board after move otherwise it returns Exception
    */
  def move(player: Player, move: PlayerMove, board: ChessBoard): Try[ChessBoard] = {
    val PlayerMove(from, to) = move
    (board.atCoordinate(from), board.atCoordinate(to)) match {
      case (None, _) => Failure(new RuntimeException(s"Illegal move, there's not such coordinate $from on the board"))
      case (_, None) => Failure(new RuntimeException(s"Illegal move, there's not such coordinate $to on the board"))
      case (Some(EmptySquare), _) => Failure(new RuntimeException(s"There's not figure at coordinate $from"))
      case (Some(attacker: ChessFigure), _) if player.color != attacker.color =>
        Failure(new RuntimeException(s"Illegal move, cannot move enemy unit"))
      case (Some(attacker: ChessFigure), Some(attacked: ChessFigure)) if attacker.color == attacked.color =>
        Failure(new RuntimeException(s"Illegal move, cannot attack allied unit"))
      case (Some(attacker: ChessFigure), _) =>

        ChessBoard.printAttackingRegion(attacker, board)
        val boardAfterMove = attacker.move(to, board)
        boardAfterMove.foreach(ChessBoard.printBoard)
        boardAfterMove
    }
  }

  /**
    * Print attacking region of the specific figure on the board
    * @param figure Chess figure
    * @param board Chess board
    */
  def printAttackingRegion(figure: ChessFigure, board: ChessBoard): Unit = {
    val attackingRegion = figure.findAttackingRegion(board)
    println(s"---Attacking region of ${figure}${figure.coordinate.rightUp}---")
    println(s" ${Range(0, 8).map(index => (index + 'a').toChar).mkString}")
    board.squares.reverse.zipWithIndex.foreach { case (line, yIndex) =>
      val attackedRegion = line.zipWithIndex map {
        case (f: ChessFigure, xIndex) if f == figure=> f.toString
        case (_, xIndex) if attackingRegion.contains(Coordinate(xIndex, yIndex).reversedByY) => "x"
        case _ => " "
      }
      println(s"${8 - yIndex}${attackedRegion.mkString}")
    }
    println()
  }


  /**
    * Print the board in the console
    * @param board Chess board
    */
  def printBoard(board: ChessBoard): Unit = {
    println("---Board after move---")
    println(s" ${Range(0, 8).map(index => (index + 'a').toChar).mkString}")
    board.squares.reverse.zipWithIndex.foreach {
      case (line, index) => println(s"${8 - index}${line.mkString}")
    }

    board.kingIsUnderAttack.filter(_._2).foreach {
      case (color, _) => println(s"$color king [${board.color2King(color).coordinate}] is under attack, Check!")
    }

    if(board.checkmate) println(s"${board.played} set checkmate to ${board.played.enemyColor.name}. Game is finished!")
    println("----------------------------------------------------------------")
  }
}
