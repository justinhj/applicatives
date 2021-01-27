package org.justinhj

import zio.console._
import zio.ExitCode
object ZioLayers extends zio.App {

  override def run(args: List[String]) = {

    putStrLn("Hello, World!").map(_ => ExitCode.success)


  }



}