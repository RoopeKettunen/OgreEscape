package OrgreEscape

/** The class `Action` represents actions that a player may take in a text adventure game.
  * `Action` objects are constructed on the basis of textual commands and are, in effect,
  * parsers for such commands. An action object is immutable after creation.*/

class Action(input: String):

  private val commandText = input.trim.toLowerCase
  private val verb        = commandText.takeWhile( _ != ' ' )
  private val modifiers   = commandText.drop(verb.length).trim

  def execute(actor: Player) = this.verb match
    case "pause"    => Some(actor.pause())
    case "run"      => Some(actor.run())
    case "pickup"   => Some(actor.pickUp())
    case "attack"   => Some(actor.attack())
    case "feed"     => Some(actor.feed())
    case "ride"     => Some(actor.ride())
    case "quit"     => Some(actor.quit())
    case "help"     => Some("Type in lowercase one of the possible actions below. Your goal is to get away from the ogre.")
    case other      => None

  /** Returns a textual description of the action object, for debugging purposes. */
  override def toString = s"$verb (modifiers: $modifiers)"

end Action

