/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.6.7. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package fury.utils

import java.io.{IOException, PrintWriter}
import java.nio.ByteBuffer
import java.nio.channels.{ReadableByteChannel, SelectableChannel, SelectionKey, Selector}

import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.blocking

class Drain(private val ec: ExecutionContext){
  
  private val selector = Selector.open()
  
  def register(drainable: Drainable)(implicit ec: ExecutionContext): SelectionKey = {
    selector.wakeup()
    drainable.source.configureBlocking(false)
    drainable.source.register(selector, SelectionKey.OP_READ, drainable.sink)
  }
  
  private val buffer = ByteBuffer.allocate(1024)
  
  Future(blocking {
    while(true) {
      if(selector.keys().isEmpty) Thread.sleep(100)
      else{
        selector.select(1000)
        selector.selectedKeys().asScala.foreach{ key =>
          val drainable = key.attachment().asInstanceOf[Drainable]
          try{
            val bytesRead = key.channel().asInstanceOf[ReadableByteChannel].read(buffer)
            if(bytesRead > 0){
              buffer.flip()
              drainable.sink.write(buffer.asCharBuffer.toString)
            }
          } catch {
            case e: IOException => drainable.onError(e)
          } finally {
            buffer.clear()
          }
        }
      }    
    }
  })(ec)
  
}

case class Drainable(
  source: SelectableChannel with ReadableByteChannel,
  sink: PrintWriter,
  onError: Throwable => Unit
)