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
package fury.lsp

import org.eclipse.lsp4j.jsonrpc.services.JsonRequest
import org.eclipse.lsp4j.InitializeParams
import java.util.concurrent.CompletableFuture
import org.eclipse.lsp4j.ServerCapabilities
import org.eclipse.lsp4j.InitializeResult
import scala.xml.Source
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.InitializedParams

case class SourceSuggestionsParams(project: String, module: String)

class FuryLanguageServer {

  @JsonRequest("initialize")
  def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    val capabilities = new ServerCapabilities()
    CompletableFuture.completedFuture(new InitializeResult(capabilities))
  }

  @JsonNotification("initialized")
    def initialized(params: InitializedParams): CompletableFuture[Unit] =
      CompletableFuture.completedFuture(Unit) 

  @JsonRequest("fury/moduleTypeSuggestions")
  def moduleTypeSuggestions(): CompletableFuture[List[String]] = {
    val moduleTypes = List("app", "bench", "compiler", "lib", "plugin")
    CompletableFuture.completedFuture(moduleTypes)
  }
  
  @JsonRequest("fury/sourceSuggestions")
  def sourceSuggestions(params: SourceSuggestionsParams): CompletableFuture[List[String]] = {
    CompletableFuture.completedFuture(List())
  }
}