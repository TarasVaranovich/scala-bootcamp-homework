package typeclass

import typeclass.Implicits.SuperVipCollections4s.syntax.sizeScore

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

    /**
     * Type class
     */
    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object syntax {

      def sizeScore[A](a: A)(implicit sizeScore: GetSizeScore[A]): SizeScore = sizeScore.apply(a)


      //      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
      //        def sizeScore: SizeScore = syntax.sizeScore[T].apply(inner)
      //      }
      implicit class GetSizeScoreOps[T](inner: T) {
        def sizeScore(implicit getSizeScore: GetSizeScore[T]): SizeScore = getSizeScore.apply(inner)
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

      def put(key: K, value: V): Unit = ???

      def get(key: K): Option[V] = ???
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
        override def apply(value: T): SizeScore = 33
      }

      //TODO: Why implicit class recognize parameter as a list but implicit method as a list type?
      implicit def listSizeScore[T]: GetSizeScore[List[T]] = new GetSizeScore[List[T]] {
        override def apply(value: List[T]): SizeScore = 12 + value.map(sizeScore).sum
      }

      implicit def arraySizeScore[T]: GetSizeScore[Array[T]] = new GetSizeScore[Array[T]] {
        override def apply(value: Array[T]): SizeScore = 46
      }

      implicit def vectorSizeScore[T]: GetSizeScore[Vector[T]] = new GetSizeScore[Vector[T]] {
        override def apply(value: Vector[T]): SizeScore = 47
      }

      implicit def mapSizeScore[A, B]: GetSizeScore[Map[A, B]] = new GetSizeScore[Map[A, B]] {
        override def apply(value: Map[A, B]): SizeScore = 48
      }

      implicit def packedMultiMapSizeScore[A, B]: GetSizeScore[PackedMultiMap[A, B]] = new GetSizeScore[PackedMultiMap[A, B]] {
        override def apply(value: PackedMultiMap[A, B]): SizeScore = 49
      }

      /**
       * Type class instance for 'Byte'
       */
      implicit val byteSizeScore: GetSizeScore[Byte] = new GetSizeScore[Byte] {
        override def apply(value: Byte): SizeScore = 1
      }

      /**
       * Type class instance for 'Char'
       */
      implicit val charSizeScore: GetSizeScore[Char] = new GetSizeScore[Char] {
        override def apply(value: Char): SizeScore = 2
      }

      /**
       * Type class instance for 'Int'
       */
      implicit val intSizeScore: GetSizeScore[Int] = new GetSizeScore[Int] {
        override def apply(value: Int): SizeScore = 4
      }

      /**
       * Type class instance for 'Long'
       */
      implicit val longSizeScore: GetSizeScore[Long] = new GetSizeScore[Long] {
        override def apply(value: Long): SizeScore = 8
      }

      /**
       * Type class instance for 'String'
       */
      implicit val stringSizeScore: GetSizeScore[String] = new GetSizeScore[String] {
        override def apply(value: String): SizeScore = 12 + value.length * 2
      }
    }
  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {

    import SuperVipCollections4s._

    final case class Twit(
                           id: Long,
                           userId: Int,
                           hashTags: Vector[String],
                           attributes: PackedMultiMap[String, String],
                           fbiNotes: List[FbiNote],
                         )

    final case class FbiNote(
                              month: String,
                              favouriteChar: Char,
                              watchedPewDiePieTimes: Long,
                            )

    trait TwitCache {
      def put(twit: Twit): Unit
      def get(id: Long): Option[Twit]
    }

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    def createTwitCache(maxSizeScore: SizeScore): TwitCache = ???
  }
}
