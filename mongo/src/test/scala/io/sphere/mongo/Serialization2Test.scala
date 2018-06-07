package io.sphere.mongo

import com.mongodb.{BasicDBObject, DBObject}
import org.scalatest.{MustMatchers, WordSpec}
import io.sphere.mongo.format.MongoFormat
import io.sphere.mongo.format.DefaultMongoFormats._
import io.sphere.mongo.generic.annotations.MongoKey
import io.sphere.mongo.magnolia.MongoFormatDerivation

object Serialization2Test {

  case class Something(a: Option[Int], b: Int = 2)

  object Color extends Enumeration {
    val Blue, Red, Yellow = Value
  }

  case class AnotherThing(
    @MongoKey("first_name") firstName: String
  )

}

class Serialization2Test extends WordSpec with MustMatchers {
  import Serialization2Test._

  "mongoProduct" must {
    "deserialize mongo object" in {
      val dbo = new BasicDBObject()
      dbo.put("a", Integer.valueOf(3))
      dbo.put("b", Integer.valueOf(4))

      val mongoFormat: MongoFormat[Something] = MongoFormatDerivation.gen
      val something = mongoFormat.fromMongoValue(dbo)
      something must be (Something(Some(3), 4))
    }

    "generate a format that serializes optional fields with value None as BSON objects without that field" in {
      val testFormat: MongoFormat[Something] = MongoFormatDerivation.gen

      val serializedObject = testFormat.toMongoValue(Something(None, 1)).asInstanceOf[DBObject]
      serializedObject.keySet().contains("b") must be(true)
      serializedObject.keySet().contains("a") must be(false)
    }

    "generate a format that use default values" in {
      val dbo = new BasicDBObject()
      dbo.put("a", Integer.valueOf(3))

      val mongoFormat: MongoFormat[Something] = MongoFormatDerivation.gen
      val something = mongoFormat.fromMongoValue(dbo)
      something must be (Something(Some(3), 2))
    }

    "can use a special field name" in {
      val testFormat: MongoFormat[AnotherThing] = MongoFormatDerivation.gen

      val serializedObject = testFormat.toMongoValue(AnotherThing("yann")).asInstanceOf[DBObject]
      serializedObject.keySet().contains("first_name") must be(true)
      serializedObject.keySet().contains("firstName") must be(false)
    }
  }

  "mongoEnum" must {
    "serialize and deserialize enums" in {
      val mongo: MongoFormat[Color.Value] = generic.mongoEnum(Color)

      // mongo java driver knows how to encode/decode Strings
      val serializedObject = mongo.toMongoValue(Color.Red).asInstanceOf[String]
      serializedObject must be ("Red")

      val enumValue = mongo.fromMongoValue(serializedObject)
      enumValue must be (Color.Red)
    }
  }

}
