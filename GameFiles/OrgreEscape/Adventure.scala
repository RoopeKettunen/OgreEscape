package OrgreEscape
import scala.io.AnsiColor as AC


class Adventure:

  /** the name of the game */
  val title = "Dungeon Escape"

  val map = GameMap(40)
  /** The character that the player controls in the game. */
  val player = Player(map.thiefLoc, map)

  /** adds the intial items/interactive elements */
  map.addItem(Encounter("Pair of Leather Boots", "You might notice you now move an extra tile per turn!", "B", true, None), 8)
  map.addItem(Encounter("Steak", "Consider saving it for the boar :) or leave it to distract the ogre.", "S", true, None), 12)
  map.addItem(Encounter("Dagger", "What a sharp one! Looks like it'd be perfect for dealing with goblins!", "D", true, None), 16)
  map.addItem(Encounter("Goblin", "Scrawny little creature.", "G", false, Some(false)), 20)
  map.addItem(Encounter("Wild Boar", "boar", "WB", false, Some(false)), 28)

  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0

  def isComplete = this.player.location > 40

  def isOver = this.isComplete || this.player.hasQuit || map.ogreLoc+1 >= map.thiefLoc

  def welcomeMessage = "You are a thief running away from a ogre.\nKeep running, and feel free to use any items you might encounter!\n"

  def goodbyeMessage =
    if this.isComplete then
      "Home at last... and phew, just in time to pay your debt with the stolen gold. Well done!"
    else if map.ogreLoc+1 >= map.thiefLoc then
      "The ogre takes one big step on your fleshy little body and gets a warm little snack to enjoy."
    else  // game over due to player quitting
      "you quit :("
  def bolded(s: String):String = AC.BOLD + s + AC.RESET

  def printMap() =
    def inLight(ind: Int): Boolean =
      val light = (1+map.currentArea.lightLevel)
      if light > 3 then
        (ind > ((map.topLevel.length+1) * 1 + 20*2) && ind < ((map.topLevel.length+1) * 2)
        || ind > ((map.topLevel.length+1) * 2 + 20*2) && ind < ((map.topLevel.length+2) * 3)
        || ind > ((map.topLevel.length+1) * 0 + 20*2) && ind < ((map.topLevel.length+1) * 1))
      else
        (ind > ((map.topLevel.length+1) * 1 + map.thiefLoc*2 - light) && ind < ((map.topLevel.length+1) * 1 + map.thiefLoc*2 + light))
        || ind > ((map.topLevel.length+1) * 2 + map.thiefLoc*2 - light) && ind < ((map.topLevel.length+1) * 2 + map.thiefLoc*2 + light)

    val drawn = map.toString
    var index = 0
    while index < drawn.length do
      val d = drawn(index)
      if index < map.mapLen - 1 then
        if inLight(index) then
          if d == 'T' then
            print(AC.BLUE + d + AC.RESET)
          else if d == 'O' || d=='G' || d=='R'|| d=='E' then
            if map.thiefLoc > 40 then
              print(AC.MAGENTA_B + AC.BLACK + d + AC.RESET)
            else
              print(AC.RED + d + AC.RESET)
          else
            print(d)
        else
          if d == 'O' || d=='G' || d=='R'|| d=='E' then
            print(AC.BLACK_B + AC.RED + d + AC.RESET)
          else if d == 'T' then
              print(AC.MAGENTA_B+ AC.BLACK + d + AC.RESET)
          else if index > (map.botLevel.length+1)*3 + 20*2 && map.thiefLoc>=20 then
            print(AC.BOLD + d + AC.RESET)
          else if map.thiefLoc <40 then
            print(AC.BLACK_B + d + AC.RESET)
          else
            print(d)
      else
        print(d)
      index += 1
    println()

  def playerInfo() = println(bolded(s"Current Area: ${map.currentArea.name}, Light Level: " +
    s"${map.currentArea.lightLevel.toString}, and Ogre Speed: ${map.currentArea.ogreSpeed.toString}"))

  def playTurn(command: String) =
    val action = Action(command)
    val outcomeReport = action.execute(this.player)
    if outcomeReport.isDefined then
      println()
      println()
      println()
      this.turnCount += 1
      if command != "help" then
        map.ogreLoc += map.currentArea.ogreSpeed
      map.updateMap()
      printMap()
    outcomeReport.getOrElse(s"""Unknown command: "$command".""")

end Adventure

