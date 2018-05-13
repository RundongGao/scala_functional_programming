sealed trait Expression
case class Number(value: Int) extends Expression
case class Plus(lhs: Expression, rhs: Expression) extends Expression
case class Minus(lhs: Expression, rhs: Expression) extends Expression

trait Json[A]{
	def json(value: A): JsonValue
}

object ExpressionEvaluator {
	def value(exp: Expression): Int = exp match {
		case Number(value) => value
		case Plus(lhs, rhs) => value(lhs) + value(rhs)
		case Minus(lhs, rhs) => value(lhs) - value(rhs)
	}
}
implicit val ExpressionJson = new Json[Expression] {
	def json(expr: Expression): JsonValue = expr match {
		case Number(value) => JsonNumber(value)
		case Plus(lhs, rhs) => JsonObject( 
			Map("op" -> JsonString("+"),
			    "lhs" -> json(lhs),
			    "rhs" -> json(rhs)))
	    case Minus(lhs, rhs) => JsonObject( 
		    Map("op" -> JsonString("-"),
			    "lhs" -> json(lhs),
			    "rhs" -> json(rhs)))
	}
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


