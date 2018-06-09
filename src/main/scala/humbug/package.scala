
import humbug.codecs.Dynamic

package object humbug {
  type FieldID = Short

  type Fields = Map[FieldID, Dynamic]
}