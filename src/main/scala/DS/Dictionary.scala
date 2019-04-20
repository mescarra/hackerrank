package DS

object Dictionary {
  def addOrUpdate[K, V](m: collection.mutable.Map[K, V], k: K, kv: (K, V), f: V => V) {
    m.get(k) match {
      case Some(e) => m.update(k, f(e))
      case None    => m += kv
    }
  }
}
