package OrgreEscape


class Encounter(val name: String, val description: String, val symbol: String, val inanimate: Boolean, var fed: Option[Boolean]):
  /** Returns a short textual representation of the item (its name, that is). */
  override def toString = this.name

end Encounter

