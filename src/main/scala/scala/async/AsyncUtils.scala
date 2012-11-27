/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

/*
 * @author Philipp Haller
 */
object AsyncUtils {

  private def enabled(level: String) = sys.props.getOrElse(s"scala.async.$level", "false").equalsIgnoreCase("true")

  private def verbose = enabled("debug")
  private def trace   = enabled("trace")

  private[async] def vprintln(s: => Any): Unit = if (verbose) println(s"[async] $s")

  private[async] def trace(s: => Any): Unit = if (trace) println(s"[async] $s")
}
