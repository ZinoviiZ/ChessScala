package com.whitehat.chess

import com.whitehatgaming.{UserInput, UserInputFile}
import sun.net.www.http.HttpClient

import scala.util.{Failure, Success, Try}

object GameEngine {


  def main(args: Array[String]): Unit = {
    GameEngine(
      new UserInputFile("/Users/zinoviizubko/Desktop/ChessWhiteHat/WhiteHatTask/data/checkmate.txt")
    ).play()
  }
}

case class GameEngine(userInput: UserInput) {

  /** Play the game by using moves from the file*/
  def play(): Unit = {

    val board = ChessBoard.initialBoard
    var moveNumber: Int = 1

    /** Queue for changing players*/
    val playerQueue = scala.collection.mutable.Queue[Player](Player("A", White), Player("B", Black))
    val moveQueue = scala.collection.mutable.Queue.empty[PlayerMove]

    /** Initial board contains white figures at first. That's why if first move uses
      * not white figure (from initial board), we will mirror all input moves coordinates by Y*/
    var shouldReverseCoordinate = false
    Option(userInput.nextMove()).foreach {
      case Array(x1,y1,x2,y2) =>
        val playerMove = PlayerMove(Coordinate(x1, y1), Coordinate(x2,y2))
        shouldReverseCoordinate = board.atCoordinate(playerMove.from) != White
        moveQueue.enqueue(if (shouldReverseCoordinate) playerMove.reversedByY else playerMove)
    }

    var moveResult: Try[ChessBoard] = Success(board)
    while(moveQueue.nonEmpty && moveResult.isSuccess && !moveResult.get.checkmate) {
      Thread.sleep(1000)
      val player = playerQueue.dequeue()
      val move = moveQueue.dequeue()

      println(s"TurnNumber: $moveNumber | Player: $player | $move")
      moveResult = ChessBoard.move(player, move, moveResult.get)
      moveNumber += 1
      playerQueue.enqueue(player)

      Try(Option(userInput.nextMove())) match {
        case Success(Some(Array(x1,y1,x2,y2))) =>
          val playerMove = PlayerMove(Coordinate(x1, y1), Coordinate(x2,y2))
          moveQueue.enqueue(if (shouldReverseCoordinate) playerMove.reversedByY else playerMove)
        case Success(None) => ()
        case Failure(th) => th.printStackTrace()
      }
    }

    moveResult match {
      case Success(_) => println("Game has been finished successfully")
      case Failure(ex) => println(s"Game has been finished with exception: ${ex.getMessage}")
    }
  }
}

/** New possible functionality */
trait GameSaver {
  def saveBoard(board: ChessBoard): Try[Unit]
  def loadBoard(): Try[ChessBoard]
}

class FileGameSaver(filePath: String) extends GameSaver {
  override def saveBoard(board: ChessBoard): Try[Unit] = ???
  override def loadBoard(): Try[ChessBoard] = ???
}

class HttpGameSaver(http: HttpClient) extends GameSaver {
  override def saveBoard(board: ChessBoard): Try[Unit] = ???
  override def loadBoard(): Try[ChessBoard] = ???
}
