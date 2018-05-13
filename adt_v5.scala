trait Expression[A] {
	def value(xpr: A): Int
}

object ExpressionEvaluator {
	def evaluate[A : Expression](expr: A): Int =
		implicitly[Expression[A]].value(expr)
}

trait Json[A]{
	def json(value: A): JsonValue
}

object Json {
	implicit val intJson = 
		new Json[Int] { def json(n: Int): JsonValue = JsonNumber(n) }

	implicit def pairJson[T1 : Json, T2 : Json] =
		new Json[(T1, T2)] { 
			def json(pair: (T1, T2)): JsonValue = JsonObject(
				Map("fst" -> implicitly[Json[T1]].json(pair._1),
		        	"snd" -> implicitly[Json[T2]].json(pair._2))
		)}

}

sealed trait JsonValue
case class JsonObject (entries: Map[String, JsonValue]) extends JsonValue
case class JsonArray (entries: Seq[JsonValue]) extends JsonValue
case class JsonNumber (value: Int) extends JsonValue
case class JsonString (value: String) extends JsonValue
case object JsonNull extends JsonValue

object JsonWriter {
	def write(value: JsonValue): String = value match {
		case JsonObject(entries) => {
			var serializedEntries = 
				for((key, value) <- entries) yield key + ": " + write(value)
			"{ " + (serializedEntries mkString ", ") + "}"
	    }
		case JsonArray(entries) => {
			var serializedEntries = 
				for(value <- entries) yield write(value)
			"{ " + (serializedEntries mkString ", ") + "}"
		}
	    case JsonString(value) => value
	    case JsonNumber(value) => value.toString
	    case JsonNull => "null"
	}

	def write[A: Json](value: A): String = {
		write(implicitly[Json[A]].json(value))
	}
}
