/*

    Fury, version 0.15.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.utils

import java.io._
import scala.util._

/**
  * Classic splitter of {@link OutputStream}. Named after the Unix 'tee' command. It allows a stream to be
  * branched off into more than one stream.
  * 
  * Based on the TeeOutputStream from Apache Commons IO.
  */
class TeeOutputStream(streams: OutputStream*) extends OutputStream {
  override def write(b: Array[Byte], off: Int, len: Int): Unit =
    synchronized(streams.foreach(_.write(b, off, len)))
  
  override def write(b: Array[Byte]): Unit = synchronized(streams.foreach(_.write(b)))
  override def write(b: Int): Unit = synchronized(streams.foreach(_.write(b)))
  override def flush(): Unit = streams.foreach(_.flush())
  override def close(): Unit = streams.map { stream => Try(stream.close()) }
}
