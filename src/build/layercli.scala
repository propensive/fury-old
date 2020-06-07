/*

    Fury, version 0.16.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury

import fury.model._, fury.core._, fury.text._, fury.io._, Args._

import guillotine._

import scala.util._

case class LayerCli(cli: Cli)(implicit log: Log) {
  def init: Try[ExitStatus] = for {
    layout <- cli.newLayout
    call   <- cli.call()
    _      <- Layer.init(layout)
    _      =  Bsp.createConfig(layout)
  } yield log.await()

  def projects: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(RawArg)
    cli       <- cli.hint(ProjectArg, layer.projects.map(_.id))
    table     <- ~Tables().projects(None)
    cli       <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    call      <- cli.call()
    col       <- ~cli.peek(ColumnArg)
    projectId <- ~cli.peek(ProjectArg)
    raw       <- ~call(RawArg).isSuccess
    projects  <- layer.allProjects(layout)
    table     <- ~Tables().show(table, cli.cols, projects.distinct, raw, col, projectId, "project")
    _         <- ~log.infoWhen(!raw)(conf.focus())
    _         <- ~log.rawln(table)
  } yield log.await()

  def select: Try[ExitStatus] = for {
    layout       <- cli.layout
    conf         <- Layer.readFuryConf(layout)
    baseLayer    <- Layer.get(conf.layerRef, conf.published)
    layer        <- Layer.retrieve(conf)
    focus        <- Layer.readFuryConf(layout)
    currentLayer <- ~Some(focus.path)
    absTree      <- ~baseLayer.importTree.getOrElse(Nil)
    relLayers    <- ~layer.imports.map { sr => ImportPath(sr.id.key) }
    parentLayer  <- if(baseLayer == layer) ~None else ~Some(ImportPath(".."))
    cli          <- cli.hint(LayerArg, absTree.to[Set] ++ relLayers ++ parentLayer -- currentLayer)
    call         <- cli.call()
    relPath      <- call(LayerArg)
    newPath      <- focus.path.dereference(relPath)
    _            <- verifyLayers(newPath, absTree)
    newConf      <- ~focus.copy(path = newPath)
    _            <- Layer.saveFuryConf(newConf, layout)
  } yield log.await()


  def verifyLayers(path: ImportPath, list: List[ImportPath]): Try[Unit] =
    if (list.map(_.path).contains(path.path))
      Success()
    else
      Failure(LayersFailure(path))

  def cloneLayer: Try[ExitStatus] = for {
    cli        <- cli.hint(DirArg)
    cli        <- cli.hint(EditorArg)
    cli        <- cli.hint(DocsArg)
    cli        <- cli.hint(ImportArg, Layer.pathCompletions().getOrElse(Nil))
    call       <- cli.call()
    edit       <- ~call(EditorArg).isSuccess
    useDocsDir <- ~call(DocsArg).isSuccess
    layerName  <- call(ImportArg)
    layerRef   <- Layer.resolve(layerName)
    published  <- Layer.published(layerName)
    layer      <- Layer.get(layerRef, published)
    _          <- layer.verify(true)
    dir        <- call(DirArg).pacify(layerName.suggestedName.map { n => Path(n.key) })
    pwd        <- cli.pwd
    dir        <- ~(if(useDocsDir) Xdg.docsDir else pwd).resolve(dir).uniquify()
    _          <- ~log.info(msg"Cloning layer $layerName into ${if(useDocsDir) dir else dir.relativizeTo(pwd)}")
    _          <- ~dir.mkdir()
    newLayout  <- cli.newLayout
    layout     =  newLayout.copy(baseDir = dir)
    optRepo    <- ~layer.mainRepo.flatMap(layer.repos.findBy(_).toOption)
    _          <- optRepo.fold(Try(()))(_.doCleanCheckout(layout))
    _          <- ~log.info(msg"Saving Fury configuration file ${layout.confFile.relativizeTo(layout.pwd)}")
    published  <- Layer.published(layerName)
    _          <- Layer.saveFuryConf(FuryConf(layerRef, ImportPath.Root, published), layout)
    _          <- Bsp.createConfig(layout)
    _          <- ~log.info(msg"Cloning complete")
    
    _          <- if(edit) VsCodeSoftware.installedPath(cli.env, false).flatMap { path =>
                    implicit val env: Environment = cli.env
                    sh"${path.value} ${dir.value}".exec[Try[String]]
                  }
                  else Success(())
  } yield log.await()

  def publish: Try[ExitStatus] = for {
    layout        <- cli.layout
    conf          <- Layer.readFuryConf(layout)
    cli           <- cli.hint(RemoteLayerArg, List(layout.pwd.name) ++ conf.published.map(_.url.path))
    cli           <- cli.hint(RawArg)
    cli           <- cli.hint(BreakingArg)
    cli           <- cli.hint(PublicArg)
    cli           <- cli.hint(DescriptionArg)
    cli           <- cli.hint(ForceArg)
    call          <- cli.call()
    token         <- ManagedConfig().token.ascribe(NotAuthenticated()).orElse(ConfigCli(cli).doAuth)
    layer         <- Layer.retrieve(conf)
    base          <- Layer.get(conf.layerRef, None)
    
    currentPub    <- if(conf.path.isEmpty) ~conf.published else for {
                       parent <- Layer.dereference(base, conf.path.init)
                       imprt  <- parent.imports.findBy(conf.path.last)
                     } yield imprt.remote

    defaultId     <- Try(currentPub.map(_.url.path).flatMap(RemoteLayerId.unapply(_)))
    remoteLayerId <- call(RemoteLayerArg).toOption.orElse(defaultId).ascribe(MissingParam(RemoteLayerArg))
    breaking      <- ~call(BreakingArg).isSuccess
    public        <- ~call(PublicArg).isSuccess
    raw           <- ~call(RawArg).isSuccess
    force         <- ~call(ForceArg).isSuccess
    description   <- ~call(DescriptionArg).toOption
    _             <- layer.verifyConf(false, conf, quiet = false, force)
    _             <- ~log.info(msg"Publishing layer to service ${ManagedConfig().service}")
    ref           <- Layer.share(ManagedConfig().service, layer, token)
    
    pub           <- Service.tag(ManagedConfig().service, ref.ipfsRef, remoteLayerId.group, remoteLayerId.name,
                         breaking, public, conf.published.fold(0)(_.version.major),
                         conf.published.fold(0)(_.version.minor), description, token)

    _             <- if(raw) ~log.rawln(str"${ref} ${pub}") else {
                       log.info(msg"Shared layer ${LayerRef(ref.key)}")
                       
                       ~log.info(msg"Published version ${pub.version}${if(public) " " else
                           " privately "}to ${pub.url}")
                     }

    _             <- conf.path match {
                       case ImportPath.Root =>
                         Layer.saveFuryConf(FuryConf(ref, conf.path, Some(pub)), layout)
                       case path            => for {
                         parent <- Layer.dereference(base, path.init)
                         imprt  <- parent.imports.findBy(path.last)
                         parent <- ~parent.copy(imports = parent.imports + imprt.copy(remote = Some(pub)))

                         _      <- Layer.commit(parent, conf.copy(path = conf.path.init,
                                       published = conf.published), layout, false)
                         conf   <- Layer.readFuryConf(layout)
                         _      <- Layer.saveFuryConf(conf.copy(path = path), layout)
                       } yield ()
                     }
  } yield log.await()

  def share: Try[ExitStatus] = for {
    layout <- cli.layout
    cli    <- cli.hint(RawArg)
    cli    <- cli.hint(PublicArg)
    cli    <- cli.hint(ForceArg)
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.retrieve(conf)
    call   <- cli.call()
    force  <- ~call(ForceArg).isSuccess
    public <- ~call(PublicArg).isSuccess
    raw    <- ~call(RawArg).isSuccess
    _      <- layer.verifyConf(false, conf, quiet = raw, force)

    ref    <- if(!public) Layer.store(layer)
              else for {
                token  <- ManagedConfig().token.ascribe(NotAuthenticated()).orElse(ConfigCli(cli).doAuth)
                ref    <- Layer.share(ManagedConfig().service, layer, token)
              } yield ref

    _      <- if(raw) ~log.rawln(str"${ref.ipfsRef.uri}") else ~log.info(msg"Shared at ${ref.ipfsRef.uri}")
  } yield log.await()

  def commit: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.retrieve(conf)
    call   <- cli.call()
    _      <- layer.verifyConf(true, conf, quiet = false, force = false)
    ref    <- Layer.store(layer)
    _      <- ~log.info(msg"Writing layer database to ${layout.layerDb.relativizeTo(layout.baseDir)}")
    _      <- Layer.writeDb(layer, layout)
    gitDir <- ~GitDir(layout)

    _      <- ~log.info(msg"Adding Fury files to ${layout.layerDb.relativizeTo(layout.baseDir)} and "+
                  msg"${layout.confFile.relativizeTo(layout.baseDir)} to the current repo.")

    _      <- gitDir.add(layout.layerDb, force = true)
    _      <- gitDir.add(layout.confFile, force = true)
    _      <- ~log.info(msg"Don't forget to run ${ExecName("git commit")} to commit the layer to the repo.")
  } yield log.await()

  def addImport: Try[ExitStatus] = for {
    layout        <- cli.layout
    conf          <- Layer.readFuryConf(layout)
    layer         <- Layer.retrieve(conf)
    cli           <- cli.hint(ImportNameArg)
    cli           <- cli.hint(ImportArg, Layer.pathCompletions().getOrElse(Nil))
    call          <- cli.call()
    layerName     <- call(ImportArg)
    
    nameArg       <- cli.peek(ImportNameArg).orElse(layerName.suggestedName).ascribe(MissingParam(
                         ImportNameArg))

    newLayerRef   <- Layer.resolve(layerName)
    pub           <- Layer.published(layerName)
    newLayer      <- Layer.get(newLayerRef, pub)
    _             <- newLayer.verify(false)
    ref           <- ~Import(nameArg, newLayerRef, pub)
    layer         <- ~Layer(_.imports).modify(layer)(_ + ref.copy(id = nameArg))
    _             <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def unimport: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(ImportIdArg, layer.imports.map(_.id))
    call      <- cli.call()
    importArg <- call(ImportIdArg)
    layer     <- ~Layer(_.imports).modify(layer)(_.evict(importArg))
    _         <- Layer.commit(layer, conf, layout)
  } yield log.await()

  def undo: Try[ExitStatus] = for {
    call     <- cli.call()
    layout   <- cli.layout
    conf     <- Layer.readFuryConf(layout)
    layer    <- Layer.get(conf.layerRef, conf.published)
    previous <- layer.previous.ascribe(CannotUndo())
    layer    <- Layer.get(previous, None)
    layerRef <- Layer.store(layer)
    _        <- ~log.info(msg"Reverted to layer $layerRef")
    _        <- Layer.saveFuryConf(conf.copy(layerRef = layerRef), layout)
  } yield log.await()

  def diff: Try[ExitStatus] = for {
    layout <- cli.layout
    conf   <- Layer.readFuryConf(layout)
    layer  <- Layer.retrieve(conf)
    cli    <- cli.hint(RawArg)
    cli    <- cli.hint(ImportArg)
    table  <- ~Tables().differences("Current", "Other")
    cli    <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
    call   <- cli.call()
    col    <- ~cli.peek(ColumnArg)
    raw    <- ~call(RawArg).isSuccess
                  
    other  <- call(ImportArg).orElse(conf.published.map { layer => IpfsRef(layer.layerRef.key) }.ascribe(
                  NoOtherLayer()))

    other  <- Layer.resolve(other)
    other  <- Layer.get(other, None)
    rows   <- ~Diff.gen[Layer].diff(layer, other)
    table  <- ~Tables().show[Difference, Difference](table, cli.cols, rows, raw, col)
    _      <- if(!rows.isEmpty) ~log.rawln(table) else ~log.info("No changes")
  } yield log.await()
  
  def pull: Try[ExitStatus] = for {
    layout    <- cli.layout
    conf      <- Layer.readFuryConf(layout)
    layer     <- Layer.retrieve(conf)
    cli       <- cli.hint(RecursiveArg)
    cli       <- cli.hint(LayerVersionArg)
    cli       <- cli.hint(AllArg)
    cli       <- cli.hint(ImportIdArg, layer.imports.map(_.id))
    call      <- cli.call()
    version   <- ~call(LayerVersionArg).toOption
    recursive <- ~call(RecursiveArg).isSuccess
    all       <- ~call(AllArg).isSuccess
    importId  <- ~call(ImportIdArg)
    current   <- Try(!all && importId.isFailure)
    
    imports   <- if(all || current) Try(layer.imports.map(_.id).to[List])
                 else ~importId.toOption.fold(List[ImportId]())(List(_))

    _         <- call.atMostOne(AllArg, LayerVersionArg)
    _         <- call.atMostOne(AllArg, ImportIdArg)
    conf      <- if(current) updateCurrent(layer, conf, version) else ~conf
    layer     <- updateAll(layer, ImportPath.Empty, imports, recursive, if(current) None else version)
    _         <- Layer.commit(layer, conf, layout, force = true)
  } yield log.await()

  private def updateCurrent(layer: Layer, conf: FuryConf, version: Option[LayerVersion]): Try[FuryConf] = for {

    _                  <- if(conf.path != ImportPath.Root) Failure(RootLayerNotSelected(conf.path))
                          else Success(())
    
    published          <- conf.published.ascribe(ImportHasNoRemote())
    (newPub, artifact) <- getNewLayer(published, version, ImportPath.Root)
    newLayer           <- Layer.get(artifact.layerRef, Some(newPub))
    layerRef           <- Layer.store(newLayer)
  } yield conf.copy(layerRef = layerRef, published = Some(newPub))

  private def updateAll(layer: Layer,
                        importPath: ImportPath,
                        imports: List[ImportId],
                        recursive: Boolean,
                        version: Option[LayerVersion])
                       : Try[Layer] =
    ~imports.foldLeft(layer)(updateOne(_, importPath, _, recursive, version).getOrElse(layer))

  private def updateOne(layer: Layer,
                        importPath: ImportPath,
                        importId: ImportId,
                        recursive: Boolean,
                        version: Option[LayerVersion])
                       : Try[Layer] = for {
    imported           <- layer.imports.findBy(importId)
    published          <- imported.remote.ascribe(ImportHasNoRemote())
    (newPub, artifact) <- getNewLayer(published, version, importPath / importId)
    layer              <- ~(Layer(_.imports(importId).remote)(layer) = Some(newPub))
    newLayer           <- Layer.get(artifact.layerRef, Some(newPub))

    newLayer           <- if(recursive) updateAll(newLayer, importPath / importId,
                              newLayer.imports.map(_.id).to[List], recursive, None) else ~newLayer
    
    layerRef           <- Layer.store(newLayer)
    layer              <- ~(Layer(_.imports(importId).layerRef)(layer) = layerRef)
  } yield layer

  private def getNewLayer(published: PublishedLayer, version: Option[LayerVersion], importPath: ImportPath)
                         : Try[(PublishedLayer, Artifact)] =
    for {
      artifact <- version.fold(Service.latest(published.url.domain, published.url.path,
                      Some(published.version))) { v =>
                    Service.fetch(published.url.domain, published.url.path, v)
                  }
      
      newPub   <- ~PublishedLayer(FuryUri(published.url.domain, published.url.path), artifact.version,
                      LayerRef(artifact.ref))
      
      _         = if(artifact.version != published.version)
                    log.info(msg"Updated layer ${importPath} from ${published.url} from version "+
                        msg"${published.version} to ${artifact.version}")
  } yield (newPub, artifact)

  def list: Try[ExitStatus] = {
    for {
      layout    <- cli.layout
      conf      <- Layer.readFuryConf(layout)
      layer     <- Layer.retrieve(conf)
      cli       <- cli.hint(RawArg)
      table     <- ~Tables().imports
      cli       <- cli.hint(ColumnArg, table.headings.map(_.name.toLowerCase))
      cli       <- cli.hint(ImportIdArg, layer.imports.map(_.id))
      call      <- cli.call()
      col       <- ~cli.peek(ColumnArg)
      importId  <- ~cli.peek(ImportIdArg)
      raw       <- ~call(RawArg).isSuccess
      rows      <- ~layer.imports.to[List].map { i => (i, Layer.get(i.layerRef, i.remote)) }
      table     <- ~Tables().show(table, cli.cols, rows, raw, col, importId, "import")
      _         <- ~log.infoWhen(!raw)(conf.focus())
      _         <- ~log.rawln(table)
    } yield log.await()
  }
}