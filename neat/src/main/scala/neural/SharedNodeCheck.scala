package neural

import scala.collection.immutable.TreeSet

object SharedNodeCheck {
  def check[A, B <: AnyRef](networks: Iterable[A], mapping: (A) => Iterable[B]): Unit = {
    val ordering = new IdentityOrdering[B]
    val itemSets = networks.map(mapping(_).foldLeft(
      TreeSet.newBuilder[B](ordering)
    ) { (builder, item) =>
      builder += item
    }.result()).toSeq

    for (
      set1 <- itemSets;
      set2 <- itemSets.dropWhile(_ ne set1).drop(1)
    ) {
      require(set1.intersect(set2).isEmpty, s"Shared nodes: s1: $set1, s2: $set2")
    }
  }

  private class IdentityOrdering[T <: AnyRef] extends Ordering[T] {
    override def compare(x: T, y: T): Int = {
      if (x eq y) {
        0
      } else {
        Integer.compare(System.identityHashCode(x), System.identityHashCode(y))
      }
    }
  }

}
