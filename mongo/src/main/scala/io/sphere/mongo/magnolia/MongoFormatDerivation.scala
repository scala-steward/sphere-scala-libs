package io.sphere.mongo.magnolia

import com.mongodb.{BasicDBObject, DBObject}
import io.sphere.mongo.format.{MongoFormat, MongoNothing, toMongo}
import io.sphere.util.{Logging, Memoizer, Reflect}

import language.experimental.macros
import magnolia._

import scala.annotation.meta.getter
import scala.reflect.{ClassTag, classTag}

object MongoFormatDerivation extends Logging {

  type Typeclass[T] = MongoFormat[T]

  def combine[T <: Product : ClassTag](ctx: CaseClass[MongoFormat, T]): MongoFormat[T] = new MongoFormat[T] {
    val mongoClass = getMongoClassMeta(classTag[T].runtimeClass)
    val fields = mongoClass.fields

    override def toMongoValue(r: T): Any = {
      val dbo = new BasicDBObject
      if (mongoClass.typeHint.isDefined) {
        val th = mongoClass.typeHint.get
        dbo.put(th.field, th.value)
      }

      var i = 0
      ctx.parameters.foreach { p ⇒
        writeField(dbo, fields(i), p.dereference(r))(p.typeclass)
        i += 1
      }
      dbo
    }
    override def fromMongoValue(any: Any): T = any match {
      case dbo: DBObject ⇒
        var i = -1
        val fieldValues: Seq[Any] = ctx.parameters.map { p ⇒
          i += 1
          readField(fields(i), dbo)(p.typeclass)
        }
        ctx.rawConstruct(fieldValues)
      case _ => sys.error("Deserialization failed. DBObject expected.")
    }
  }

  def dispatch[T](ctx: SealedTrait[MongoFormat, T]): MongoFormat[T] =
    new MongoFormat[T] {

      override def toMongoValue(a: T): Any = ???
      override def fromMongoValue(any: Any): T = ???

//      def show(value: T): String = ctx.dispatch(value) { sub =>
//        sub.typeclass.show(sub.cast(value))
//      }
    }

  implicit def gen[T]: MongoFormat[T] = macro Magnolia.gen[T]


  type MongoEmbedded = io.sphere.mongo.generic.annotations.MongoEmbedded @getter
  type MongoKey = io.sphere.mongo.generic.annotations.MongoKey @getter
  type MongoIgnore = io.sphere.mongo.generic.annotations.MongoIgnore @getter
  type MongoTypeHint = io.sphere.mongo.generic.annotations.MongoTypeHint
  type MongoTypeHintField = io.sphere.mongo.generic.annotations.MongoTypeHintField

  /** The default name of the field used for type-hinting, taken from the MongoTypeHintField annotation. */
  val defaultTypeFieldName: String = classOf[MongoTypeHintField].getMethod("value").getDefaultValue.asInstanceOf[String]

  private case class MongoClassMeta(typeHint: Option[MongoClassMeta.TypeHint], fields: IndexedSeq[MongoFieldMeta])
  private object MongoClassMeta {
    case class TypeHint(field: String, value: String)
  }
  private case class MongoFieldMeta(
    name: String,
    default: Option[Any] = None,
    embedded: Boolean = false,
    ignored: Boolean = false
  )

  private val getMongoClassMeta = new Memoizer[Class[_], MongoClassMeta](clazz => {
    def hintVal(h: MongoTypeHint): String =
      if (h.value.isEmpty) defaultTypeValue(clazz)
      else h.value

    log.trace("Initializing Mongo metadata for %s".format(clazz.getName))

    val typeHintFieldAnnot = clazz.getAnnotation(classOf[MongoTypeHintField])
    val typeHintAnnot = clazz.getAnnotation(classOf[MongoTypeHint])
    val typeField = Option(typeHintFieldAnnot).map(_.value)
    val typeValue = Option(typeHintAnnot).map(hintVal)

    MongoClassMeta(
      typeHint = (typeField, typeValue) match {
        case (Some(field), Some(hint)) => Some(MongoClassMeta.TypeHint(field, hint))
        case (None       , Some(hint)) => Some(MongoClassMeta.TypeHint(defaultTypeFieldName, hint))
        case (Some(field), None)       => Some(MongoClassMeta.TypeHint(field, defaultTypeValue(clazz)))
        case (None       , None)       => None
      },
      fields = getMongoFieldMeta(clazz)
    )
  })

  private def getMongoFieldMeta(clazz: Class[_]): IndexedSeq[MongoFieldMeta] = {
    Reflect.getCaseClassMeta(clazz).fields.map { fm =>
      val m = clazz.getDeclaredMethod(fm.name)
      val name = Option(m.getAnnotation(classOf[MongoKey])).map(_.value).getOrElse(fm.name)
      val embedded = m.isAnnotationPresent(classOf[MongoEmbedded])
      val ignored = m.isAnnotationPresent(classOf[MongoIgnore])
      if (ignored && fm.default.isEmpty) {
        throw new Exception("Ignored Mongo field '%s' must have a default value.".format(fm.name))
      }
      MongoFieldMeta(name, fm.default, embedded, ignored)
    }
  }

  private def writeField[A: MongoFormat](dbo: DBObject, field: MongoFieldMeta, e: A): Unit =
    if (!field.ignored) {
      if (field.embedded)
        toMongo(e) match {
          case dbo2: DBObject => dbo.putAll(dbo2)
          case MongoNothing => ()
          case x => dbo.put(field.name, x)
        }
      else
        toMongo(e) match {
          case MongoNothing => ()
          case x => dbo.put(field.name, x)
        }

    }

  private def readField[A: MongoFormat](f: MongoFieldMeta, dbo: DBObject): A = {
    val mf = MongoFormat[A]
    def default = f.default.asInstanceOf[Option[A]].orElse(mf.default)
    if (f.ignored)
      default.getOrElse {
        throw new Exception("Missing default for ignored field '%s'.".format(f.name))
      }
    else if (f.embedded) mf.fromMongoValue(dbo)
    else {
      val value = dbo.get(f.name)
      if (value != null) mf.fromMongoValue(value)
      else {
        default.getOrElse {
          throw new Exception("Missing required field '%s' on deserialization.".format(f.name))
        }
      }
    }
  }

  private def defaultTypeValue(clazz: Class[_]): String =
    clazz.getSimpleName.replace("$", "")

}
