package OrgreEscape

import scala.io.StdIn.*

object OgreEscapeUI extends App:

  private val game = Adventure()
  private val player = game.player
  this.run()

  /** Runs the game. First, a welcome message is printed, then the player gets the chance to
    * play any number of turns until the game is over, and finally a goodbye message is printed. */
  private def run() =
    println(this.game.welcomeMessage)
    this.game.map.updateMap()
    this.game.printMap()
    while !this.game.isOver do
      this.playTurn()
    println("\n" + this.game.goodbyeMessage)





  /** Requests a command from the player, plays a game turn accordingly, and prints out a
    * report of what happened.  */
  private def playTurn() =
    game.playerInfo()
    print("Available Commands -> " + game.map.availableCommands(game.player))
    println("         Inventory -> "+ player.inventry)
    val command = readLine("Your Command: ")
    val turnReport = this.game.playTurn(command)
    if turnReport.nonEmpty then
      println(turnReport)

end OgreEscapeUI

