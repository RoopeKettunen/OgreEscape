package OrgreEscape

import scala.collection.mutable.Map

class GameMap(val length: Int):

  val areas = Vector(Area("Cave", 2, 20, 1),
                     Area("Grassland", 4, 10, 2),
                     Area("Flatland", 6, 10, 3),
                     Area("HomeBase", 80, 5, 4))

  var currentArea = areas(0)
  var ogreLoc = 0
  var thiefLoc = 3
  private final val showAtATime = 11
  var itemList = Map[Int, Encounter]()
  var riding = false
  def addItem(item: Encounter, location: Int) = itemList(location) = item
  def removeItem(loc: Int) = itemList.remove(loc)
  def setOgre(loc: Int) = ogreLoc = loc
  def setThief(loc: Int) = thiefLoc = loc
  def hasItem(location: Int) = itemList.keys.toVector.contains(location)
  def front = thiefLoc + 1
  def visible = showAtATime + thiefLoc - 5
  def mapLen = topWall.length + botWall.length + topLevel.length + botLevel.length + 5

  var topWall = "W_"
  var botWall = "W-"
  var topLevel = "  "*visible
  var botLevel = "  "*visible

  def numberTrack =
    var ret = ""
    for x <- 0 to length do
      if x % 5 == 0 then
        if x > 9 then
          ret += x
        else
          ret += x.toString + " "
      else
        ret += "  "
    ret

  def updateMap()=
    var currentLoc = thiefLoc
    var index = 0
    while currentLoc > 0 do
      currentLoc -= areas(index).length
      currentArea = areas(index)
      index += 1

    def addLetter(letter: String, spot: Int, level: String) =
      level.substring(0, spot) + letter + level.substring(spot+1)

    // creating length of visible map
    topLevel = "  "*visible
    botLevel = "  "*visible
    if visible>20 then
      topWall = "W_"*20 + "  "*(visible-20)
    else
      topWall = "W_"*visible
    botWall = "W-"*visible
    if visible>40 then
      topWall = addLetter("|", 40*2+1, topWall)
      topLevel = addLetter("|", 40*2+1, topLevel)
      botLevel = addLetter("|", 40*2+1, botLevel)
      topWall = addLetter("<", 40*2, topWall)
      topLevel = addLetter("<", 40*2, topLevel)
      botLevel = addLetter("<", 40*2, botLevel)
    //adding items tp map
    for item <- itemList do
      val location = item._1
      if location < visible then
        if location - 1 <= thiefLoc then
          botLevel = addLetter(item._2.symbol, location*2, botLevel)
        else
          botLevel = addLetter("?", location*2, botLevel)

    //adding orgre to the map
    val ogreSpot = ogreLoc*2
    topLevel = addLetter("G", ogreSpot+2, topLevel)
    botLevel = addLetter("E", ogreSpot+2, botLevel)
    topLevel = addLetter("O", ogreSpot, topLevel)
    botLevel = addLetter("R", ogreSpot, botLevel)
    //adding theif to the map
    if ogreLoc + 1 >= thiefLoc then
      botWall = addLetter("T", thiefLoc*2, botWall)
    else
      if riding then
        topLevel = addLetter("T", thiefLoc*2, topLevel)
        botLevel = addLetter("W", thiefLoc*2, botLevel)
        botLevel = addLetter("B", thiefLoc*2+1, botLevel)
      else
        botLevel = addLetter("T", thiefLoc*2, botLevel)

  override def toString =
    topWall + "\n" +
    topLevel + "\n" +
    botLevel+ "\n" +
    botWall + "\n" +
    numberTrack

  def availableCommands(theif: Player): String =
    var commands = Vector[String]("pause", "run", "help")
    if hasItem(front) then
      if itemList(front).inanimate then
        commands = "pickup" +: commands
      else
        if theif.inventory.keys.toVector.contains("Dagger") then
          commands = "attack" +: commands
        if theif.inventory.keys.toVector.contains("Steak") then
          commands = "feed" +: commands
      if itemList(front).fed.isDefined then
        if itemList(front).fed.get then
          if itemList(front).name == "Wild Boar" then
            commands = "ride" +: commands
    var list = ""
    for x <- commands.sorted do
      list += x + ", "
    list.take(list.length - 2)

end GameMap

class Area(val name: String, val lightLevel: Int, val length: Int, val ogreSpeed: Int)


