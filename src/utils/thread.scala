package fury.utils

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, ThreadFactory}

object Threads {

  private val baseFactory = Executors.defaultThreadFactory()

  def factory(prefix: String, daemon: Boolean = false): ThreadFactory = new ThreadFactory {
    private val threadCounter = new AtomicInteger(0)
    override def newThread(runnable: Runnable): Thread = {
      val thread = baseFactory.newThread(runnable)
      thread.setName(s"$prefix-${threadCounter.getAndIncrement}")
      thread.setDaemon(daemon)
      thread
    }
  }

}