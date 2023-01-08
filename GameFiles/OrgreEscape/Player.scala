package OrgreEscape

import scala.collection.mutable.Map


class Player(startingSquare: Int, worldMap: GameMap):

  var alive = true
  var inventory = Map[String, Encounter]("Stolen Gold" -> Encounter("Stolen Gold", "500 gold coins stolen from the ogre himself.","BG", true, None))
  private var currentLocation = startingSquare        // gatherer: ,changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag

  def hasQuit = this.quitCommandGiven
  def location = this.currentLocation
  def inFront = location + 1

  def run() =
    val somethingFront = worldMap.hasItem(inFront)
    var deltaX = 0
    if worldMap.riding then
      deltaX += 3
    else if inventory.keys.toVector.contains("Pair of Leather Boots") then
      deltaX += 2
    else
      deltaX += 1
    currentLocation += deltaX
    val result = passCheck(somethingFront)
    if result == "" then
      if worldMap.itemList.keys.toVector.contains(currentLocation) && !somethingFront then
        currentLocation -= 1
        deltaX -= 1
      worldMap.thiefLoc = currentLocation
      if worldMap.hasItem(inFront) then
        s"You run ${deltaX} space(s) forward and happen to see a ${worldMap.itemList(inFront).name} right in front of you."
      else
        s"You frantically run ${deltaX} space(s) forward."
    else
      worldMap.thiefLoc = currentLocation
      result

  def passCheck(was: Boolean): String =
    if was then
      if worldMap.hasItem(20) && worldMap.itemList(20).name == "Goblin" then
        if currentLocation >= 20 then
          inventory = inventory.empty
          worldMap.removeItem(20)
          "While running past the goblin, you fall flat on your face only to realize it was the Goblins leg you tripped on.\nHe proceeds to takes everthing you have, smiling greedily and dissapearing off into the cave."
        else
          ""
      else if !worldMap.riding && worldMap.hasItem(28) && worldMap.itemList(28).name == "Wild Boar"then
        if currentLocation >= 28 then
          currentLocation = 24
          worldMap.removeItem(28)
          "While trying to run past the Boar, you quickly find yourself headbutted backwards 4 spaces and some sore cheeks.\nThe boar runs off into the grasslands."
        else
          ""
      else
        ""
    else
      ""

  def pause() = "You stop running, maybe to hide behind something, or just out of exhaustion."
  def pickUp() =
    if worldMap.itemList.keys.toVector.contains(inFront) then
      val item = worldMap.removeItem(inFront).get
      inventory(item.name) = item
      s"You hastily pick up a ${item.name}."
    else
      "You crouch down to pick up... wait, you realize there's nothing there. And the ogre's footsteps only get louder."

  def inventry =
    var items = ""
    inventory.keys.foreach(items += _ + ", ")
    items.take(items.length - 2)

  def attack() =
    if inventory.keys.toVector.contains("Dagger") then
      if worldMap.hasItem(inFront) then
        if worldMap.itemList(inFront).name == "Wild Boar" then
          worldMap.removeItem(inFront)
          worldMap.addItem(Encounter("Steak", "Consider saving it for the boar :) or leave it to distract the ogre.", "S", true, None), inFront)
          s"You viciously stab the ${worldMap.itemList(inFront).name}, leaving a steak behind for the ogre to eat."
        else
          s"You viciously stab the ${worldMap.removeItem(inFront).get.name}, leaving only a pool of blood behind."
      else
        "You quickly pull out your blade and lunge out at the air. Not sure what you were going for..."
    else
      "Attack? Whatever you're attacking, don't think those fists are goona cut it."

  def ride() =
    if worldMap.hasItem(inFront) && worldMap.itemList(inFront).name == "Wild Boar" && worldMap.itemList(inFront).fed.get then
      worldMap.riding = true
      worldMap.removeItem(inFront)
      "You hop onto your new trusty steed. What a fast and beautiful creature."
    else if worldMap.riding then
      "You're already riding!"
    else
      "You try and ride... the air?"
  def feed(): String =
    if inventory.keys.toVector.contains("Steak") then
      if worldMap.hasItem(inFront) && !worldMap.itemList(inFront).inanimate then
        worldMap.itemList(inFront).fed = Some(true)
        inventory.remove("Steak")
        if worldMap.itemList(inFront).name == "Goblin" then
          worldMap.removeItem(inFront)
          "You feed the Goblin ahead your steak, and he runs off happily, dissapearing into the dark cave."
        else
          "You feed the Wild Boar your steak and devours the steak happily, beckoining you to perhaps, get on his back."
      else
        "You feed yourself the steak. Definetely the right choice."
    else
      "You got no steak to feed :(."

  def quit() =
    this.quitCommandGiven = true
    ""

end Player

