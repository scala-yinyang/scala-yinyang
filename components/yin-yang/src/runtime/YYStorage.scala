package ch.epfl.yinyang.runtime

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.{ DoubleLinkedList, HashSet }
import scala.ref.WeakReference

object YYStorage {
  // Debugging information
  private var runtimeCompileCount = 0
  private var compileTimeCompiled = false
  private var checkCount = 0
  private var checkTime = 0L
  private var hashTime = 0L

  final def getHashLookupTime() = hashTime

  @inline final def getCompileTimeCompiled() = compileTimeCompiled
  @inline final def setCompileTimeCompiled() = compileTimeCompiled = true
  @inline final def resetCompileTimeCompiled() = compileTimeCompiled = false

  @inline final def getRuntimeCompileCount() = runtimeCompileCount
  @inline final def incrementRuntimeCompileCount() = runtimeCompileCount += 1

  @inline final def getCheckTimeCount() = (checkTime, checkCount)
  @inline final def incrementCheckTimeCount(dt: Long) = { checkTime += dt; checkCount += 1 }

  private var compileTimeCompileCount = 0
  private var alreadyCountedCompileTimeUIDs: List[Long] = Nil
  @inline
  final def getCompileTimeCompileCount() = compileTimeCompileCount
  @inline
  final def incrementCompileTimeCompileCount(uID: Long) = {
    if (!alreadyCountedCompileTimeUIDs.contains(uID)) {
      compileTimeCompileCount += 1
      alreadyCountedCompileTimeUIDs ::= uID
    }
  }

  /** DSL instances are stored and retrieved by their UID. */
  final private val caches = new ConcurrentHashMap[Long, YYCache]

  def guardedLookup[FunctionT](id: Long, lazyCache: => YYCache, refs: Array[Any]): FunctionT = {
    val startTime = System.nanoTime()

    var cache = caches.get(id)
    cache = (if (cache == null) {
      // Null check because we don't want to eval lazyCache
      val c = lazyCache
      caches.putIfAbsent(id, c)
      c
    } else cache)
    hashTime += (System.nanoTime() - startTime)

    val fun = cache.guardedLookup[FunctionT](refs)
    incrementCheckTimeCount(System.nanoTime() - startTime)
    fun
  }
}

class YYCache {
  def guardedLookup[FunctionT](refs: Array[Any]): FunctionT = ???
}

