package collections.lms

import scala.collection.mutable.HashMap

import scala.virtualization.lms.common._

/**
 * An object representing the "phone mnemonics" example presented by
 * Martin Odersky at Scala eXchange 2011.
 */
object Mnemonics extends MnemonicsBase {

  /** A type representing strings as lists of characters. */
  override type LString = List[Char]

  /**
   * This method should implement the "phone mnemonics" example
   * presented by Martin Odersky at Scala eXchange 2011.  For more
   * details see http://player.vimeo.com/video/25126695.
   *
   * @param mnemonics a map of Mnemonics for digits (represented as a map
   *        from digits to lists of lower-case characters)
   * @param words a dictionary (represented as a list of lower-case words).
   * @param number a phone number (represented as a string of digits).
   * @return a list of possible word phrases (i.e. lists of words) that can
   *         represent the number.
   */
  def apply(
    mnemonics: Rep[HashMap[Char, LString]],
    words: Rep[List[LString]],
    number: Rep[LString]): Rep[List[List[LString]]] = {

    /**
     * Invert the mnemonics map to give a map from chars 'a' ... 'z'
     * to '2' ... '9'.
     */
    val charCode: Rep[HashMap[Char, Char]] =
      for (p ← mnemonics; ltr ← p._2) yield (ltr -> p._1)

    /**
     * Maps a word to the digit string it can represent,
     * e.g. "java" -> "5282".
     */
    def wordCode(word: Rep[LString]) = word map (charCode(_))

    /**
     * A map from digit strings to the words that represent them,
     * e.g. "5282" -> Set("java", "kata", "lava", ...)
     */
    val wordsForNum: Rep[HashMap[LString, List[LString]]] =
      (words groupBy wordCode)

    /** Return all ways to encode a number as a list of words */
    def encode: Rep[LString ⇒ List[List[LString]]] = doLambda { number ⇒
      if (number.isEmpty)
        List(List())
      else {
        for {
          splitPoint ← (unit(1) until (number.length + 1)).toList
          word ← wordsForNum getOrElse ((number take splitPoint), List())
          rest ← encode(number drop splitPoint)
        } yield word :: rest
      }
    }

    /**
     * Maps the number to a list of all word phrases that can
     * represent it.
     */
    encode(number)
  }
}
