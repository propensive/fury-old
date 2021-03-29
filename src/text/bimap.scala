package fury.text

object Bimap {
  def apply[K, V](relations: Map[K, V]): Bimap[K, V] = Bimap(relations, relations.to[List].map(_.swap).toMap)
  def apply[K, V](relations: (K, V)*): Bimap[K, V] = Bimap(relations.toMap, relations.map(_.swap).toMap)
}

case class Bimap[K, V](relations: Map[K, V], private val inverse: Map[V, K]) {
  def +(kv: (K, V)): Bimap[K, V] = updated(kv._1, kv._2)
  def -(k: K): Bimap[K, V] = Bimap(relations - k, inverse -- relations.get(k))
  def flip: Bimap[V, K] = Bimap(inverse, relations)
  def apply(k: K): V = relations(k)
  def contains(k: K): Boolean = relations.contains(k)
  def get(k: K): Option[V] = relations.get(k)
  def updated(k: K, v: V): Bimap[K, V] = Bimap(relations.updated(k, v), inverse.updated(v, k))
  def filter(pred: K => Boolean) = this -- relations.keys.filterNot(pred)
  def --(ks: TraversableOnce[K]): Bimap[K, V] = ks.foldLeft(this)(_ - _)
  def ++(bimap: Bimap[K, V]): Bimap[K, V] = bimap.relations.foldLeft(this)(_ + _)

  def map[V2](fn: V => V2): Bimap[K, V2] = {
    val elems = relations.map { case (k, v) => (k, fn(v)) }
    Bimap(elems, elems.map(_.swap).toMap)
  }
}

trait Relation[A, C] {
  type Return[A1, C1] <: Relation[A1, C1]
  //def +(kv: (A, C)): Return[A, C]
  //def ++(assoc: Assoc[A, C]): Return[A, C]
  def relations: Set[(A, C)]
  def updated(a: A, c: C): Return[A, C]
  def --(as: TraversableOnce[A]): Return[A, C]
  def apply(a: A): Set[C]
  def contains(a: A): Boolean
  def flip: Relation[C, A]
  def keys: Set[A]
  def joinRight[H, D](assoc: Relation[C, D]): Relation[A, D] = Join(this, assoc)
  def joinLeft[H, Z](assoc: Relation[Z, A]): Relation[Z, C] = Join(assoc, this)
  def -(key: A): Return[A, C]
  def filter(pred: A => Boolean): Return[A, C]
  def updateAll(a: A)(fn: C => C): Return[A, C]
}

object Assoc {
  def apply[K, V](kvs: (K, V)*): Assoc[K, V] =
    kvs.foldLeft(Assoc[K, V](Map[K, Set[V]](), Map[V, Set[K]]()))(_ + _)
}

case class Assoc[K, V](map: Map[K, Set[V]], private val inverse: Map[V, Set[K]]) extends Relation[K, V] {
  type Return[A, C] = Assoc[A, C]
  def +(kv: (K, V)): Assoc[K, V] = updated(kv._1, kv._2)
  def ++(assoc: Assoc[K, V]): Assoc[K, V] = assoc.relations.foldLeft(this)(_ + _)
  def ++(elems: TraversableOnce[(K, V)]): Assoc[K, V] = elems.foldLeft(this)(_ + _)
  def relations: Set[(K, V)] = map.flatMap { case (k, vs) => vs.map(k -> _) }.to[Set]
  def updated(k: K, v: V): Assoc[K, V] = Assoc(append(map, k, v), append(inverse, v, k))
  def --(ks: TraversableOnce[K]): Assoc[K, V] = ks.foldLeft(this)(_ - _)
  def apply(k: K): Set[V] = map.get(k).getOrElse(Set())
  def contains(k: K): Boolean = map.contains(k)
  def flip: Assoc[V, K] = Assoc(inverse, map)
  def keys: Set[K] = map.keys.to[Set]
  def updateAll(k: K)(fn: V => V): Return[K, V] = (this - k) ++ this(k).map(k -> fn(_))
  
  def -(k: K): Assoc[K, V] = Assoc(map - k, map(k).foldLeft(inverse) {
    case (acc, next) => if(acc(next) == Set(k)) acc - next else acc.updated(next, acc(next) - k)
  })

  def filter(pred: K => Boolean): Assoc[K, V] = this -- keys.filterNot(pred)

  private def append[A, B](map: Map[A, Set[B]], a: A, b: B): Map[A, Set[B]] =
    map.updated(a, map.get(a).fold(Set(b))(_ + b))

}

object Join {
  def apply[A, B, C](xs: (A, B, C)*): Join[A, B, C] = Join[A, B, C](Assoc[A, B](), Assoc[B, C]()) ++ xs
}

case class Join[A, B, C](leftRelation: Relation[A, B], rightRelation: Relation[B, C]) extends Relation[A, C] {
  type Return[A1, C1] = Join[A1, B, C1]
  
  case class Projection[K, V](get: Relation[K, V], set: Relation[K, V] => Return[A, C]) {
    def +(kv: (K, V)): Return[A, C] = updated(kv._1, kv._2)
    def -(k: K): Return[A, C] = set(get - k)
    
    def ++(kvs: TraversableOnce[(K, V)]): Return[A, C] =
      set(kvs.foldLeft(get) { case (acc, kv) => acc.updated(kv._1, kv._2) })
    
    def --(ks: TraversableOnce[K]): Return[A, C] = set(get -- ks)
    def updated(k: K, v: V): Return[A, C] = set(get.updated(k, v))
    def filter(pred: K => Boolean): Return[A, C] = set(get.filter(pred))
    def apply(k: K): Set[V] = get(k)
  }

  def +(triple: (A, B, C)): Return[A, C] =
    Join(leftRelation.updated(triple._1, triple._2), rightRelation.updated(triple._2, triple._3))
  
  def ++(triples: TraversableOnce[(A, B, C)]): Return[A, C] = triples.foldLeft(this)(_ + _)
  def -(a: A): Return[A, C] = Join(leftRelation - a, rightRelation -- leftRelation(a))
  def --(as: TraversableOnce[A]): Return[A, C] = as.foldLeft(this)(_ - _)
  def flip: Relation[C, A] = Join[C, B, A](rightRelation.flip, leftRelation.flip)
  def keys: Set[A] = leftRelation.keys
  def triples: Set[(A, B, C)] = leftRelation.relations.flatMap { case (a, b) => rightRelation(b).map((a, b, _)) }
  def relations: Set[(A, C)] = triples.map { case (a, b, c) => (a, c) }
  def contains(a: A): Boolean = leftRelation.contains(a)
  def apply(a: A): Set[C] = leftRelation(a).flatMap(rightRelation(_))
  def updated(a: A, c: C): Return[A, C] = Join(leftRelation, leftRelation(a).foldLeft(rightRelation)(_.updated(_, c)))
  def filter(pred: A => Boolean): Return[A, C] = this -- keys.filterNot(pred)
  
  def updateAll(a: A)(fn: C => C): Return[A, C] =
    Join(leftRelation, leftRelation(a).foldLeft(rightRelation) { case (acc, b) =>
      acc.updateAll(b)(fn)
    })
  
  lazy val left: Projection[B, A] = Projection(leftRelation.flip, rel => Join(rel.flip, rightRelation))
  lazy val right: Projection[B, C] = Projection(rightRelation, rel => Join(leftRelation, rel))
}