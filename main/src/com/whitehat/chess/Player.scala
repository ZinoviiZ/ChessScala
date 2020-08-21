package com.whitehat.chess

sealed abstract class Color(val asciiDiff: Int, val name: String) {
  val enemyColor: Color
}
object Black extends Color(0, "Black") {
  override val enemyColor: Color = White
}
object White extends Color('A' - 'a', "White") {
  override val enemyColor: Color = Black
}

case class Player(name: String, color: Color) {
  override def toString: String = s"${color.name}Player"
}

case class PlayerMove(from: Coordinate, to: Coordinate) {
  override def toString: String = s"Move from ${from.rightUp} to ${to.rightUp}"

  def reversedByY: PlayerMove = PlayerMove(from.reversedByY, to.reversedByY)
}

case class Coordinate(x: Int, y: Int) {
  override def toString: String = s"[$y,$x]"

  def left: Coordinate = Coordinate(x - 1, y)
  def right: Coordinate = Coordinate(x + 1, y)
  def up: Coordinate = Coordinate(x, y + 1)
  def down: Coordinate = Coordinate(x, y - 1)

  def leftUp: Coordinate = Coordinate(x - 1, y + 1)
  def rightUp: Coordinate = Coordinate(x + 1, y + 1)
  def leftDown: Coordinate = Coordinate(x - 1, y - 1)
  def rightDown: Coordinate = Coordinate(x + 1, y - 1)

  def reversed: Coordinate = Coordinate(7 - x, 7 - y)

  def reversedByY: Coordinate = Coordinate(x, 7 - y)
}