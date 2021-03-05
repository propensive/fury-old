/*

    Fury, version 0.33.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

sealed trait Event
case class LogMessage(str: String) extends Event
case class TaskProgress(task: String, progress: Int) extends Event

case class Task(name: String, progress: Int)
case class Activity(tasks: Set[Task])

case class State(stream: Stream[Event] = Stream(), activities: Set[Activity] = Set())

object Bus {
  private var state: State = State()
  def apply(): State = Bus.synchronized(state)
  def put(event: Event): Unit = Bus.synchronized { receive(event) }

  private def receive(event: Event): Unit = state = update(state, event)

  private def update(state: State, event: Event): State = event match {
    case LogMessage(str: String)      => state
    case TaskProgress(task, progress) => state
  }
}