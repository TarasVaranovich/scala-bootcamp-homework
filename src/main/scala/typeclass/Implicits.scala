package typeclass

import typeclass.Implicits.SuperVipCollections4s.instances.rawMapSizeScore
import typeclass.Implicits.SuperVipCollections4s.syntax.{GetSizeScoreOps, sizeScore}

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

//fill in implementation gaps here making the ImplicitsHomeworkSpec pass!
object Implicits {

  /**
   * Lo and behold! Brand new super-useful collection library for Scala!
   *
   * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
   * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
   * of the data stored.
   *
   * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
   * a thing called size score. Its calculation rules:
   * - size score of a Byte is 1
   * - Int - 4 (as primitive JVM int consists of 4 bytes)
   * - Long - 8
   * - Char - 2 (one UTF-16 symbol is 2 bytes)
   * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
   * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
   * the fields
   * - score for any sequence (Array[T], List[T], Vector[T]) is
   * 12 (our old friend object header) + sum of scores of all elements
   * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
   */
  object SuperVipCollections4s {
    type SizeScore = Int

    val HEADER_SIZE: Short = 12

    /**
     * Type class
     */
    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object syntax {

      def sizeScore[A](a: A)(implicit sizeScore: GetSizeScore[A]): SizeScore = sizeScore.apply(a)

      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = {
          val getSizeScore: GetSizeScore[T] = implicitly[GetSizeScore[T]]
          getSizeScore.apply(inner)
        }
      }
    }

    /**
     * Mutable key-value cache which limits the size score of the data scored.
     *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
     * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
     * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
     * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
     *
     * @param maxSizeScore max size score for the stored data
     * @tparam K key type
     * @tparam V value type
     */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      //with this you can use .sizeScore syntax on keys and values

      /*
      mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
       */
      private val map = mutable.LinkedHashMap.empty[K, V]

      def put(key: K, value: V): Unit = {
        val currentSizeScore = rawMapSizeScore(map.toMap)
        val entrySizeScore = sizeScore(key) + sizeScore(value)
        if (entrySizeScore > maxSizeScore) println(s"Map size is '$maxSizeScore' but required '$entrySizeScore''")
        else if (entrySizeScore + currentSizeScore <= maxSizeScore) map.put(key, value)
        else cleanMapToSizeScore(map, maxSizeScore - entrySizeScore).put(key, value)
      }

      @tailrec
      private def cleanMapToSizeScore(map: mutable.LinkedHashMap[K, V], sizeScore: SizeScore): mutable.LinkedHashMap[K, V] = {
        val firstKey = map.keys.head
        map.remove(firstKey)
        if (rawMapSizeScore(map.toMap) > sizeScore) cleanMapToSizeScore(map, sizeScore) else map
      }

      def get(key: K): Option[V] = map.get(key)
    }

    /**
     * Cool custom immutable multi-map collection - does not extend the standard library collection types
     * (yes, this is a feature)
     */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
     * Type-class allowing us to iterate over different "collection-like" types with one type arg
     */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    /**
     * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
     */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]
      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object instances {

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }
      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }
      //Provide Iterate2 instances for Map and PackedMultiMap!
      //if the code doesn't compile while you think it should - sometimes full rebuild helps!
      implicit val mapIterator: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keys.iterator
        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.values.iterator
      }

      implicit val packedMultiMapIterator: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.toMap.keys.iterator
        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.toMap.values.iterator
      }

      /*
      replace this big guy with proper implicit instances for types:
      - Byte, Char, Int, Long
      - String
      - Array[T], List[T], Vector[T], Map[K,V], PackedMultiMap[K,V]
        - points to karma if you provide those in a generic way
        (Iterate and Iterate2 type-classes might be helpful!)

      If you struggle with writing generic instances for Iterate and Iterate2, start by writing instances for
      List and other collections and then replace those with generic instances.
       */
      implicit def stubGetSizeScore[T]: GetSizeScore[T] = new GetSizeScore[T] {
        override def apply(value: T): SizeScore = 0
      }

      implicit def stubGetSizeScore[T: GetSizeScore]: GetSizeScore[T] = new GetSizeScore[T] {
        override def apply(value: T): SizeScore = 0
      }
      
      implicit def listSizeScore[T: GetSizeScore]: GetSizeScore[List[T]] = new GetSizeScore[List[T]] {
        override def apply(value: List[T]): SizeScore = HEADER_SIZE + value.map(sizeScore(_)).sum
      }

      implicit def arraySizeScore[T: GetSizeScore]: GetSizeScore[Array[T]] = new GetSizeScore[Array[T]] {
        override def apply(value: Array[T]): SizeScore = HEADER_SIZE + value.map(sizeScore(_)).sum
      }

      implicit def vectorSizeScore[T: GetSizeScore]: GetSizeScore[Vector[T]] = new GetSizeScore[Vector[T]] {
        override def apply(value: Vector[T]): SizeScore = HEADER_SIZE + value.map(sizeScore(_)).sum
      }

      implicit def mapSizeScore[A: GetSizeScore, B: GetSizeScore]: GetSizeScore[Map[A, B]] = new GetSizeScore[Map[A, B]] {
        override def apply(value: Map[A, B]): SizeScore = HEADER_SIZE + rawMapSizeScore(value)
      }

      implicit def packedMultiMapSizeScore[A: GetSizeScore, B: GetSizeScore]: GetSizeScore[PackedMultiMap[A, B]] = new GetSizeScore[PackedMultiMap[A, B]] {
        override def apply(value: PackedMultiMap[A, B]): SizeScore = HEADER_SIZE + rawMapSizeScore(value.inner.toMap)
      }
      //TODO: refactor to apply each type of map
      def rawMapSizeScore[A: GetSizeScore, B: GetSizeScore](map: Map[A, B]): SizeScore =
        map
          .map { case (key: A, value: B) => (sizeScore(key), sizeScore(value)) }
          .map { case (key: Int, value: Int) => key + value }
          .sum

      implicit val byteSizeScore: GetSizeScore[Byte] = new GetSizeScore[Byte] {
        override def apply(value: Byte): SizeScore = 1
      }

      implicit val charSizeScore: GetSizeScore[Char] = new GetSizeScore[Char] {
        override def apply(value: Char): SizeScore = 2
      }

      implicit val intSizeScore: GetSizeScore[Int] = new GetSizeScore[Int] {
        override def apply(value: Int): SizeScore = 4
      }

      implicit val longSizeScore: GetSizeScore[Long] = new GetSizeScore[Long] {
        override def apply(value: Long): SizeScore = 8
      }

      implicit val stringSizeScore: GetSizeScore[String] = new GetSizeScore[String] {
        override def apply(value: String): SizeScore = HEADER_SIZE + value.length * 2
      }
    }
  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {

    import SuperVipCollections4s._
    import SuperVipCollections4s.instances._

    final case class Twit(
                           id: Long,
                           userId: Int,
                           hashTags: Vector[String],
                           attributes: PackedMultiMap[String, String],
                           fbiNotes: List[FbiNote],
                         )
    object Twit {
      implicit val twitGetSizeScore: GetSizeScore[Twit] = (value: Twit) =>
        HEADER_SIZE + value.id.sizeScore + value.userId.sizeScore + value.hashTags.sizeScore +
          value.attributes.sizeScore + value.fbiNotes.sizeScore
    }

    final case class FbiNote(
                              month: String,
                              favouriteChar: Char,
                              watchedPewDiePieTimes: Long,
                            )
    object FbiNote {
      implicit val fbiNoteGetSizeScore: GetSizeScore[FbiNote] = (value: FbiNote) =>
        HEADER_SIZE + value.month.sizeScore + value.favouriteChar.sizeScore + value.watchedPewDiePieTimes.sizeScore
    }

    trait TwitCache {
      def put(twit: Twit): Unit
      def get(id: Long): Option[Twit]
    }

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {

      import instances._

      private val cache = new MutableBoundedCache[Long, Twit](maxSizeScore)
      override def put(twit: Twit): Unit = cache.put(twit.id, twit)
      override def get(id: Long): Option[Twit] = cache.get(id)
    }
  }
}
