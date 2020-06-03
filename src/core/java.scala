package fury.core

import fury.model._, fury.text._, fury.io._

import guillotine._

import scala.concurrent._
import scala.collection.{mutable => cm}
import scala.util.Try

import java.{io => ji, util => ju, security => js, net => jn}

object Java {

  def run(name: String,
          classpath: List[String],
          main: ClassRef,
          securePolicy: Boolean,
          env: Map[String, String],
          properties: Map[String, String],
          policy: Policy,
          layout: Layout,
          args: List[String],
          noSecurity: Boolean,
          stdin: ji.InputStream,
          output: String => Unit,
          capture: Boolean)
         (implicit log: Log, ec: ExecutionContext, environment: Environment)
         : Try[Int] = {
    layout.sharedDir.mkdir()

    implicit val defaultEnvironment: Environment =
      Environment((environment.variables ++ env).updated("SHARED", layout.sharedDir.value), environment.workDir)

    val policyFile = Installation.policyDir.extant() / ju.UUID.randomUUID().toString
    policy.save(policyFile).get

    val allProperties: Map[String, String] = {
      val withPolicy = if(noSecurity) properties else
          properties.updated("java.security.manager", "").updated("java.security.policy", policyFile.value)
      
      withPolicy.updated("fury.sharedDir", layout.sharedDir.value)
    }

    val propArgs = allProperties.map { case (k, v) => if(v.isEmpty) str"-D$k" else str"-D$k=$v" }.to[List]

    Sandbox.run(name, policy, classpath, main, args)
  }
}

object Sandbox extends SecurityManager() {
  private[this] val policies: cm.HashMap[ThreadGroup, Policy] = cm.HashMap()
  
  private[this] val required: cm.Map[ThreadGroup, Set[Permission]] = cm.HashMap().withDefault { t => Set() }

  val rootGroup: ThreadGroup = Thread.currentThread.getThreadGroup

  def group(threadGroup: ThreadGroup): Option[ThreadGroup] =
    if(threadGroup == rootGroup) None
    else if(policies.contains(threadGroup)) Option(threadGroup) else group(threadGroup.getParent)

  def run(name: String, policy: Policy, classpath: List[String], main: ClassRef, args: List[String])
         (implicit log: Log): Try[Int] =
    Try {
      log.info("Java.run")
      val threadGroup = new ThreadGroup(name)
      var exitStatus: Int = 0
      var cause: Option[Throwable] = None
  
  
      val thread: Thread = new Thread(threadGroup, str"sandbox-$name") {
        override def run(): Unit = try {
          log.info("Inside thread: "+classpath)
          val classLoader = new jn.URLClassLoader(classpath.map(new ji.File(_).toURI.toURL).to[Array])
          log.info("Inside 1")
          
          Thread.currentThread.setContextClassLoader(classLoader)
          log.info("Inside 2")
          
          val mainClass = classLoader.loadClass(main.key)
          log.info("Inside 3")
          val mainMethod = mainClass.getMethod("main", classOf[Array[String]])
          log.info("Inside 4")
          try mainMethod.invoke(null, args.to[Array]: Array[String]) catch {
            case e: Throwable => log.info(e.toString)
          }
          
          log.info("Finished block")

        } catch {
          case ExitVm(code) =>
            log.info("Finished")
            exitStatus = code
          case exception: Throwable =>
            log.info(exception.toString)
            exitStatus = 1
            cause = Some(exception)
        }
      }
  
      log.info("thread.setContextClassLoader")
  
      synchronized { policies(threadGroup) = policy }
      log.info("starting thread")
      thread.start()
      log.info("running thread")
      //thread.join()
      log.info("Joined thread")
      synchronized { policies.remove(threadGroup) }
      log.info(cause.toString)
      cause.foreach(_.getStackTrace().map(_.toString).foreach(log.info(_)))
      exitStatus
    }

  override def checkPermission(perm: js.Permission): Unit = ()

  /*override def checkPermission(perm: js.Permission): Unit = perm match {
    case _: ju.PropertyPermission =>
    case _: RuntimePermission =>
    case _: java.lang.reflect.ReflectPermission =>
    case _: ji.FilePermission =>
    case perm =>
    println("Checking permission "+perm)
    group(Thread.currentThread.getThreadGroup).foreach { group =>
      policies.get(group).foreach { policy =>
        val classRef = ClassRef(perm.getClass.getName)
        val target = perm.getName
        if(target.startsWith("exitVM")) throw new ExitVm(perm.getName.drop(7).toInt)
        else {
          val actions = Option(perm.getActions).flatMap { a => if(a.isEmpty) None else Some(a) }
          if(actions.isEmpty) None else Some(actions.mkString(","))
          synchronized { required(group) += Permission(classRef, target, actions) }
          //if(!policy.check(classRef, target, actions)) throw new SecurityException()
        }
      }
    }
  }*/
}