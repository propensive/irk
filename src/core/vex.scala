package vex

import gossamer.*
import rudiments.*
import acyclicity.*
import euphemism.*
import jovian.*
import guillotine.*
import kaleidoscope.*
import escapade.*
import gastronomy.*
import harlequin.*
import iridescence.*, solarized.*
import eucalyptus.*
import slalom.*
import exoskeleton.*
import profanity.*
import xylophone.*
import scintillate.*
import clairvoyant.*
import java.nio.BufferOverflowException

import scala.collection.mutable as scm
import scala.concurrent.*

erased given CanThrow[AppError | RootParentError] = compiletime.erasedValue

import timekeeping.long
import encodings.Utf8

given Env = envs.enclosing
import rendering.ansi

given LogFormat[SystemOut.type, AnsiText] = LogFormat.timed
given Log()

case class Message(module: Text, path: Text, line: Int, from: Int, to: Int, message: Text,
                       content: IArray[Char])

case class Build(pwd: Directory, repos: Map[DiskPath, Text], publishing: Option[Publishing],
    index: Map[Text, Step] = Map()):
  val steps: Set[Step] = index.values.to(Set)
  
  lazy val graph: Dag[Step] throws BrokenLinkError =
    val links = index.values.map:
      step => step -> step.links.map(resolve(_)).to(Set)
    
    Dag(links.to(Seq)*)

  lazy val linearization: List[Step] throws BrokenLinkError = graph.sorted
  def sourceDirs: List[Directory] = steps.flatMap(_.sources).to(List)

  lazy val hashes: Map[Step, Text] throws BrokenLinkError | IoError | StreamCutError |
      ExcessDataError =
    def recur(todo: List[Step], hashes: Map[Step, Text])
             : Map[Step, Text] throws BrokenLinkError | StreamCutError | ExcessDataError =
      if todo.isEmpty then hashes
      else try
        val step = todo.head
        val inputsHash: List[Bytes] = step.srcFiles.to(List).map(Vex.hashFile)
        val jarsHash: List[Bytes] = step.jars.to(List).map(_.digest[Crc32].bytes)
        val linksHash: List[Bytes] = step.links.to(List).map(index(_)).map(hashes(_).bytes)
        val newHash = (inputsHash ++ linksHash).digest[Crc32].encode[Base64]
        
        recur(todo.tail, hashes.updated(step, newHash))
      catch case err: Error => throw AppError(t"An unknown error occurred", err)
    val t0 = System.currentTimeMillis
    val result = recur(linearization, Map())
    val time = System.currentTimeMillis - t0
    //Out.println(ansi"Calculated ${Yellow}(${index.size}) hashes in ${Yellow}[${time}ms]")
    result

  @targetName("addAll")
  infix def ++(build: Build): Build =
    Build(build.pwd, build.repos, publishing.orElse(build.publishing), index ++ build.index)
  
  def resolve(id: Text): Step throws BrokenLinkError = index.get(id).getOrElse:
    throw BrokenLinkError(id)
  
  def transitive[T](fn: Step => Set[T])(step: Step): Set[T] throws BrokenLinkError =
    graph.reachable(step).flatMap(fn)
  
  def transitiveIn[T](fn: Step => Set[T])(step: Step): Set[T] throws BrokenLinkError =
    (graph.reachable(step) - step).flatMap(fn)

  def bases: Set[Directory] = steps.map(_.pwd).to(Set)

  def cache: Map[Step, Text] =
    try
      val caches = bases.map(_.path / t".vex").filter(_.exists()).map:
        cacheFile => Json.parse(cacheFile.file().read[Text](64.kb)).as[Cache].hashes
      
      caches.flatten.flatMap:
        case Hash(id, hash, _) =>
          try Set(resolve(id) -> hash) catch case e: BrokenLinkError => Set()
      .to(Map)
    catch
      case err: JsonParseError =>
        throw AppError(t"The cache file is not in the correct format", err)
      
      case err: StreamCutError =>
        throw AppError(t"The stream was cut while reading the cache file", err)
      
      case err: IoError =>
        throw AppError(t"There was an IO error while reading the cache",err)
      
      case err: JsonAccessError =>
        throw AppError(t"The cache file did not contain the correct JSON format", err)
      
      case err: Error =>
        throw AppError(t"An unexpected error occurred", err)
  

  def updateCache(step: Step, binDigest: Text): Unit =
    synchronized:
      try
        val cacheFile = (step.pwd / t".vex")
        val cache =
          if cacheFile.exists()
          then Cache:
            Json.parse(cacheFile.file().read[Text](64.kb)).as[Cache].hashes.filter:
              hash => try resolve(hash.id).pwd == step.pwd catch case err: BrokenLinkError => false
          else Cache(Set())
        
        val newHash = Hash(step.id, hashes(step), binDigest)
        val newCache = cache.copy(hashes = cache.hashes.filter(_.id != step.id) + newHash)
        
        newCache.json.show.bytes.writeTo:
          if cacheFile.exists() then cacheFile.file(Expect).delete()
          cacheFile.file()
      catch
        case err: JsonParseError =>
          throw AppError(t"The cache file is not in the correct format", err)
        
        case err: StreamCutError =>
          throw AppError(t"The stream was cut while reading the cache file", err)
        
        case err: IoError =>
          throw AppError(t"There was an IO error while reading the cache", err)
        
        case err: JsonAccessError =>
          throw AppError(t"The cache file did not contain the correct JSON format", err)
        
        case err: Error =>
          throw AppError(t"An unexpected error occurred", err)


case class Step(path: File, publishing: Option[Publishing], name: Text,
                    id: Text, links: Set[Text], resources: Set[Directory],
                    sources: Set[Directory], jars: Set[Text], dependencies: Set[Dependency],
                    version: Text, docs: List[DiskPath], artifact: Option[DiskPath],
                    main: Option[Text]):
  def publish(build: Build): Publishing = publishing.orElse(build.publishing).getOrElse:
    throw AppError(t"There are no publishing details for $id")

  def projectId: Text = id.cut(t"/").head
  def moduleId: Text = id.cut(t"/").last
  def dashed: Text = t"$projectId-$moduleId"
  def pwd: Directory = path.parent
  def group(build: Build): Text = publish(build).group
  def docFile: DiskPath = output(t"-javadoc.jar")
  def srcsPkg: DiskPath = output(t"-sources.jar")
  def pomFile: DiskPath = output(t".pom")

  private def output(extension: Text): DiskPath =
    pwd / t"bin" / t"$dashed-$version$extension"
  
  def compilable(f: File): Boolean = f.name.endsWith(t".scala") || f.name.endsWith(t".java")

  def srcFiles: Set[File] throws IoError =
    sources.flatMap(_.path.descendantFiles(!_.name.startsWith(t"."))).filter(compilable)

  def classpath(build: Build)(using Stdout): Set[DiskPath] throws IoError | BrokenLinkError =
    build.graph.reachable(this).flatMap:
      step => step.jars.map(Vex.fetchFile(_).await()).map(_.path) + step.classesDir.path
  
  def allResources(build: Build)(using Stdout): Set[Directory] throws IoError | BrokenLinkError =
    build.graph.reachable(this).flatMap(_.resources)

  def compileClasspath(build: Build)(using Stdout): Set[DiskPath] throws IoError | BrokenLinkError =
    classpath(build) - classesDir.path

  def classesDir: Directory = synchronized:
    try (Vex.cacheDir / t"cls" / projectId / moduleId).directory(Ensure)
    catch
      case err: IoError =>
        throw AppError(t"Could not write to the user's home directory", err)

  def pomDependency(build: Build): Dependency = Dependency(group(build), dashed, version)

  def pomDependencies(build: Build): List[Dependency] =
    try links.map(build.resolve(_)).map(_.pomDependency(build)).to(List) ++
        dependencies
    catch case err: BrokenLinkError => throw AppError(t"Couldn't resolve dependencies", err)

  def compile(hashes: Map[Step, Text], oldHashes: Map[Step, Text], build: Build,
                  scriptFile: File)
             (using Stdout)
             : List[Message] =
    try
      val t0 = System.currentTimeMillis
      classesDir.children.foreach(_.delete())
      val cp = compileClasspath(build)
      val messages = Compiler.compile(id, srcFiles.to(List), cp, classesDir, scriptFile)
      val time = System.currentTimeMillis - t0
      
      if messages.isEmpty then
        Out.println(ansi"Compilation of ${Green}[${name}] succeeded in ${Yellow}[${time}ms]")
        val digestFiles = classesDir.path.descendantFiles().to(List).sortBy(_.name).to(LazyList)
        val digest = digestFiles.map(_.read[Bytes](1.mb).digest[Crc32]).to(List).digest[Crc32]
        build.updateCache(this, digest.encode[Base64])
      else Out.println(ansi"Compilation of ${Green}[${name}] failed in ${Yellow}[${time}ms]")
      
      messages
    
    catch
      case err: IoError =>
        throw AppError(t"Could not read the source files", err)
      
      case err: StreamCutError =>
        throw AppError(t"The stream from scala-cli was broken before it completed", err)

      case err: ExcessDataError =>
        throw AppError(t"The scala-cli process returned too much data", err)

      case err: BrokenLinkError => err match
        case BrokenLinkError(ref) =>
          throw AppError(t"There was an unsatisfied reference to $ref", err)

      case err: Error =>
        throw AppError(t"An unexpected error occurred", err)

case class BuildConfig(imports: Option[List[Text]], publishing: Option[Publishing],
                           modules: List[Module], repos: Option[List[Repo]]):
  
  def gen(build: Build, seen: Set[Text], files: DiskPath*)(using Stdout)
         : Build throws IoError | AppError | BuildfileError =
    
    repos.getOrElse(Nil).foreach:
      case Repo(base, uri) =>
        val root = build.pwd.path + Relative.parse(base)
        if !root.exists() then
          Out.println(ansi"Cloning repository $uri to $base")
          Vex.cloneRepo(root, uri)

    files.to(List) match
      case Nil =>
        build
      
      case path :: tail =>
        Out.println(ansi"Reading build file $Violet(${path.relativeTo(build.pwd.path).get.show})"
            .render)
        val steps: Map[Text, Step] = modules.map:
          module =>
            def relativize(text: Text): DiskPath = path.file(Expect).parent.path + Relative.parse(text)
            val links = module.links.getOrElse(Set())
            val resources = module.resources.getOrElse(Set()).map(relativize).map(_.directory(Expect))
            val sources = module.sources.map(relativize).map(_.directory(Expect))
            val dependencies = module.dependencies.getOrElse(Set())
            val docs = module.docs.getOrElse(Nil).map(relativize)
            val version = module.version.getOrElse(t"1.0.0")
            val artifactPath = module.artifact.map(path.parent + Relative.parse(_))
            
            Step(path.file(), publishing, module.name, module.id, links, resources, sources,
                module.jars.getOrElse(Set()), dependencies, version, docs, artifactPath,
                module.main)
        .mtwin.map(_.id -> _).to(Map)
        
        val importPaths = imports.getOrElse(Nil).map:
          p => (path.parent + Relative.parse(p))
        
        val reposMap = repos.getOrElse(Nil).map:
          repo => (build.pwd.path + Relative.parse(repo.base)) -> repo.url
        .to(Map)

        Vex.readBuilds(build ++ Build(build.pwd, reposMap, build.publishing, steps), seen,
            (importPaths ++ tail)*)

case class Publishing(username: Text, group: Text, url: Text, organization: Organization,
                          developers: List[Developer])

case class Repo(base: Text, url: Text):
  def basePath(dir: DiskPath): DiskPath = dir + Relative.parse(base)

case class Module(name: Text, id: Text, links: Option[Set[Text]], resources: Option[Set[Text]],
                      sources: Set[Text], jars: Option[Set[Text]], docs: Option[List[Text]],
                      dependencies: Option[Set[Dependency]], version: Option[Text],
                      artifact: Option[Text], main: Option[Text])

case class AppError(message: Text, cause: Maybe[Error] = Unset) extends Error(cause)

case class BuildfileError() extends Error:
  def message: Text = t"There was an error in the buildfile"

case class BrokenLinkError(link: Text) extends Error:
  def message: Text = t"The reference to $link cannot be resolved"

object Vex extends Daemon():
  def homeDir: Directory =
    try Unix.parse(Sys.user.home()).get.directory(Expect)
    catch
      case err: IoError =>
        throw AppError(t"The user's home directory could not be determined", err)
      
      case err: KeyNotFoundError =>
        throw AppError(t"The user's home directory could not be determined", err)
      
  def cacheDir: Directory =
    try (homeDir.path / t".cache" / t"vex").directory(Ensure)
    catch case err: IoError =>
      throw AppError(t"The user's cache directory could not be created", err)
  
  private val prefixes = Set(t"scala/", t"dotty/", t"compiler.properties", t"scala-asm.properties",
      t"incrementalcompiler.version.properties", t"library.properties", t"org/", t"com/", t"NOTICE")
  
  def vexJar(scriptFile: File)(using Stdout): File throws StreamCutError = synchronized:
    val jarFile = cacheDir.path / t"lib" / t"base.jar"
    
    try if jarFile.exists() then jarFile.file(Expect) else
      jarFile.parent.directory()
      val entries = Zip.read(scriptFile).filter { e => prefixes.exists(e.path.show.startsWith(_)) }
      Zip.write(jarFile, entries)
      jarFile.file(Expect)

    catch case err: IoError =>
      throw AppError(t"The Vex binary could not be copied to the user's cache directory")

  def fetchFile(ref: Text)(using Stdout): Future[File] =
    val libDir = cacheDir / t"lib"
    if ref.startsWith(t"https:") then
      val dest = libDir / t"${ref.digest[Crc32].encode[Hex].lower}.jar"
      if dest.exists() then Future:
        try dest.file(Expect) catch case err: IoError =>
          // FIXME: This exception is thrown inside a Future
          throw AppError(t"Could not access the dependency JAR, $ref", err)
      else Future:
        Out.println(ansi"Downloading JAR dependency ${ref}")
        try
          libDir.directory()
          val file = dest.file(Create)
          Uri(ref).writeTo(file)
          file
        catch
          case err: StreamCutError =>
            throw AppError(t"Could not download the file $ref", err)
          
          case err: IoError =>
            throw AppError(t"The downloaded file could not be written to ${dest.fullname}", err)
        
    else
      Future:
        try Unix.parse(ref).get.file(Expect) catch case err: IoError =>
          throw AppError(t"Could not access the dependency JAR, $ref", err)

  def main(using cli: CommandLine): ExitStatus = try
    cli.args match
      case t"compile" :: params => Vex.build(false, params.headOption.map(_.show), false, None, cli.pwd, cli.env, cli.script)
      case t"publish" :: params => Vex.build(true, params.headOption.map(_.show), false, None, cli.pwd, cli.env, cli.script)
      case t"watch" :: params   => Vex.build(false, params.headOption.map(_.show), true, None, cli.pwd, cli.env, cli.script)
      case t"stop" :: params    => Vex.stop(cli)
      case params               => Vex.build(false, params.headOption.map(_.show), false, None, cli.pwd, cli.env, cli.script)
  
  catch
    case err: Throwable =>
      Out.println(StackTrace(err).ansi)
      ExitStatus.Fail(1)
  
  private lazy val fileHashes: FileCache[Bytes] = new FileCache()

  private def init(target: Option[Text], pwd: Directory)(using Stdout)
                  : Build throws IoError | BuildfileError =
    val path = pwd / (target.getOrElse(t"build")+t".vex")
    
    try readBuilds(Build(pwd, Map(), None), Set(), path)
    catch case err: IoError =>
      Out.println(ansi"Configuration file $Violet(${path.show})")
      sys.exit(1)
  
  def hashFile(file: File): Bytes throws IoError | AppError =
    try fileHashes(file.path.show, file.modified):
      file.read[Bytes](1.mb).digest[Crc32].bytes
    catch
      case err: StreamCutError  => throw AppError(t"The stream was cut while hashing a file", err)
      case err: ExcessDataError => throw AppError(t"The file was too big to hash", err)
      case err: Error           => throw AppError(t"An unexpected error occurred", err)
  
  def cloneRepo(path: DiskPath, url: Text): Unit =
    try sh"git clone -q $url ${path.fullname}".exec[Unit]()
    catch case err: ExecError => throw AppError(t"Could not run `git clone` for repository $url")

  def readBuilds(build: Build, seen: Set[Text], files: DiskPath*)(using Stdout)
                : Build throws BuildfileError =
    try
      files.to(List) match
        case Nil =>
          build
        
        case path :: tail =>
          def digest: Text = hashFile(path.file()).encode[Base64]
          if path.exists() && seen.contains(digest) then readBuilds(build, seen, tail*)
          else if path.exists() then
            val buildConfig = Json.parse(path.file().read[Text](1.mb)).as[BuildConfig]
            buildConfig.gen(build, seen + digest, files*)
          else throw AppError(txt"""Build contains an import reference to a nonexistant build""")
            
    catch
      case err: IoError => err match
        case IoError(op, reason, path) =>
          throw AppError(t"There was an I/O error", err)
      
      case err: ExcessDataError =>
        Out.println(t"The configuration file was larger than 1MB")
        throw BuildfileError()
      
      case err: StreamCutError =>
        throw AppError(t"The configuration file could not be read completely", err)
      
      case err: JsonParseError =>
        Out.println(t"The configuration file was not valid JSON")
        throw BuildfileError()
      
      case err: AppError =>
        throw err

      case err: BuildfileError =>
        throw AppError(t"There was an error in the buildfile", err)

      case err: Error =>
        throw AppError(t"An unexpected error occurred", err)

  def readImports(seen: Map[Text, File], files: File*)(using Stdout): Set[File] =
    case class Imports(repos: Option[List[Repo]], imports: Option[List[Text]]):
      def repoList: List[Repo] = repos.getOrElse(Nil)
      def gen(seen: Map[Text, File], files: File*): Set[File] =
        files.to(List) match
          case file :: tail =>
            val importFiles: List[File] = imports.getOrElse(Nil).flatMap:
              path =>
                val ref = file.parent.path + Relative.parse(path)
                try
                  if ref.exists() then
                    Out.println(t"The import $ref exists")
                    List((file.parent.path + Relative.parse(path)).file(Expect))
                  else
                    Out.println(t"Build file $ref does not exist; attempting to clone")
                    if ref.parent.exists()
                    then throw AppError(t"The build ${ref.name} does not exist in ${ref.parent}")
                    else
                      repoList.find(_.basePath(file.parent.path) == ref.parent) match
                        case None =>
                          throw AppError(txt"""Could not find a remote repository containing the import $path""")
                        
                        case Some(repo) =>
                          Vex.cloneRepo(ref.parent, repo.url)
                          List(ref.file(Expect))
                          
                catch case err: IoError =>
                  throw AppError(t"Imported build $ref could not be read", err)
            
            readImports(seen, (importFiles ++ tail)*)
          
          case Nil => seen.values.to(Set)
    
    files.to(List) match
      case Nil =>
        seen.values.to(Set)
      
      case file :: tail =>
        try
          def interpret(digest: Option[Text]) = Json.parse(file.read[Text](1.mb)).as[Imports].gen(digest.fold(seen)(seen.updated(_, file)), files*)
          
          if file.exists() then
            val digest: Text = hashFile(file).encode[Base64]
            if !seen.contains(digest) then interpret(Some(digest)) else readImports(seen, tail*)
          else interpret(None)

        catch case err: Error => readImports(seen, tail*)

  def stop(cli: CommandLine)(using Stdout): ExitStatus =
    Out.println(t"Shutting down Vex")
    cli.shutdown()
    ExitStatus.Ok

  def build(publishSonatype: Boolean, target: Option[Text], watch: Boolean = false,
                oldBuild: Option[Build], pwd: Directory, env: Map[Text, Text],
                scriptFile: File)
           (using Stdout)
           : ExitStatus =
    def restart(): ExitStatus =
      if watch then
        try
          val path = pwd / (target.getOrElse(t"build")+t".vex")
          val dirs = readImports(Map(), path.file(Expect)).map(_.parent).to(List)
          waitForChange(dirs)
          Vex.build(false, target, true, None, pwd, env, scriptFile)
        catch case err: Error =>
          Out.println(err.toString.show)
          ExitStatus.Fail(1)
      else ExitStatus.Ok
    try
      import unsafeExceptions.canThrowAny
      val build = init(target, pwd)
      val oldHashes = build.cache
      Out.println("Starting build")

      val futures = build.graph.traversal[Future[Set[Message]]]:
        (set, step) => Future.sequence(set).flatMap:
          results =>
            Future.sequence(step.jars.map(Vex.fetchFile(_))).flatMap:
              downloads => Future:
                blocking:
                  val messages = results.flatten
                  if messages.isEmpty then
                    try
                      if oldHashes.get(step) != build.hashes.get(step) || step.main.isDefined then
                        Out.println(ansi"Compiling ${Green}[${step.name}]...")
                        messages ++ step.compile(build.hashes, oldHashes, build, scriptFile)
                      else messages
                    catch
                      case err: ExcessDataError =>
                        messages + Message(step.id, t"<unknown>", 0, 0, 0, t"too much data was received", IArray())
      
                      case err: StreamCutError =>
                        messages + Message(step.id, t"<unknown>", 0, 0, 0, t"the stream was cut prematurely", IArray())
                  else
                    Out.println(t"Skipping compilation of ${step.name}")
                    messages
      .values

      val messages: List[Message] = Future.sequence(futures).await().to(Set).flatten.to(List)
      val success = messages.isEmpty

      if success then Out.println(t"Compilation completed successfully")
      else Out.println(t"Compilation failed\n")
      
      // FIXME: Files should be sorted by last-modified
      messages.groupBy(_.path).foreach:
        case (path, messages) =>
          val syntax = ScalaSyntax.highlight(String(messages.head.content.unsafeMutable).show)
          messages.sortBy(-_.line).groupBy(_.line).foreach:
            case (line, messages) => messages.last match
              case Message(module, path, line, from, to, message, _) =>
                val bg = Bg(Srgb(0.16, 0.06, 0.03))
                Out.println(ansi"${colors.Black}(${Bg(colors.Purple)}( $module ))${colors.Purple}(${Bg(colors.Crimson)}( ${colors.Black}($path:${line + 1}:$from) ))$bg${colors.Crimson}()$bg${escapes.EraseLine}")
                
                def format(line: Int) =
                  if line >= syntax.length then ansi""
                  else syntax(line).map:
                    case Token.Code(code, flair) => flair match
                      case Flair.Type              => ansi"${colors.YellowGreen}(${code})"
                      case Flair.Term              => ansi"${colors.CadetBlue}(${code})"
                      case Flair.Symbol            => ansi"${colors.Turquoise}(${code})"
                      case Flair.Keyword           => ansi"${colors.DarkOrange}(${code})"
                      case Flair.Modifier          => ansi"${colors.Chocolate}(${code})"
                      case Flair.Ident             => ansi"${colors.BurlyWood}(${code})"
                      case Flair.Error             => ansi"${colors.OrangeRed}($Underline(${code}))"
                      case Flair.Number            => ansi"${colors.Gold}(${code})"
                      case Flair.String            => ansi"${colors.Plum}(${code})"
                      case other                   => ansi"${code}"
                    case Token.Space(n)          => ansi" "*n
                    case Token.Newline           => throw Impossible("Should not have a newline")
                  .join
                
                val margin = (line + 2).show.length
                
                if line > 1 then
                  Out.print(ansi"${colors.Orange}(${line.show.pad(margin, Rtl)})${colors.Gray}(║)$bg ")
                  Out.println(ansi"${format(line - 1)}${escapes.EraseLine}")
                
                val code = format(line)
                Out.print(ansi"${colors.Orange}($Bold(${(line + 1).show.pad(margin, Rtl)}))${colors.Gray}(║)$bg ")
                Out.print(ansi"${code.take(from)}")
                Out.print(ansi"${Underline}(${colors.OrangeRed}(${code.plain.slice(from, to)}))")
                Out.println(ansi"${code.drop(to)}${escapes.EraseLine}")
                
                if line + 1 < syntax.length
                then
                  Out.print(ansi"${colors.Orange}(${(line + 2).show.pad(margin, Rtl)})${colors.Gray}(║)$bg ")
                  Out.println(ansi"${format(line + 1)}${escapes.EraseLine}${escapes.Reset}")
                
                messages.reverse.foreach:
                  case m@Message(module, path, line, from, to, message, _) =>
                    if m != messages.last then
                      Out.print(ansi"${colors.Orange}($Bold(${(line + 1).show.pad(margin, Rtl)}))${colors.Gray}(║)$bg ")
                      Out.print(ansi"${code.take(from)}")
                      Out.print(ansi"${Underline}(${colors.OrangeRed}(${code.plain.slice(from, to)}))")
                      Out.println(ansi"${code.drop(to)}${escapes.EraseLine}${escapes.Reset}")
                    Out.println(ansi"${colors.Gray}(${t" "*margin}╟${t"─"*from}┴${t"─"*(to - from)}┘)${escapes.EraseLine}${escapes.Reset}")
                    message.cut(t"\n").foreach:
                      line =>
                        Out.println(ansi"${t" "*margin}${colors.Gray}(║) ${Bold}($line)${escapes.EraseLine}${escapes.Reset}")
                    if m != messages.head then Out.println(ansi"${t" "*margin}${colors.Gray}(║)")
                Out.println(ansi"${t" "*margin}${colors.Gray}(╨)${escapes.EraseLine}${escapes.Reset}")
                Out.println(ansi"${escapes.Reset}")

      if success then
        build.linearization.foreach:
          step => step.artifact.foreach:
            artifact =>
              if !watch then
                Out.println(ansi"Building $Violet(${artifact.show}) artifact...")
                val inputJars = step.classpath(build) + vexJar(scriptFile).path
                val zipStreams = inputJars.to(LazyList).flatMap:
                  path =>
                    if path.isFile then
                      Zip.read(path.file(Expect)).filter(_.path.parts.last != t"MANIFEST.MF")
                    else if path.isDirectory then
                      path.descendantFiles().map:
                        file => Zip.Entry(file.path.relativeTo(path).get, () => file.read[DataStream](10.mb))
                    else LazyList()
                
                val resourceStreams = step.allResources(build).flatMap:
                  dir => dir.path.descendantFiles().map:
                    file =>
                      Zip.Entry(file.path.relativeTo(dir.path).get, () => file.read[DataStream](1.mb))
                .to(LazyList)
                
                val basicMf = ListMap(t"Manifest-Version" -> t"1.0", t"Created-By" -> t"Vex 0.2.0")
                
                val manifest = step.main.fold(basicMf)(basicMf.updated(t"Main-Class", _)).flatMap:
                  (k, v) =>
                    val (first, rest) = t"$k: $v".snip(72)
                    first :: rest.s.grouped(71).map(t" "+_.show).to(List)
                .join(t"", t"\n", t"\n")
                
                def manifestStream(): DataStream = LazyList(manifest.bytes)
                val mfEntry = Zip.Entry(Relative.parse(t"META-INF/MANIFEST.MF"), manifestStream)
                
                val header = if artifact.name.endsWith(t".jar") then Bytes.empty else
                    (Classpath() / t"exoskeleton" / t"invoke").resource.read[Bytes](100.kb)
                
                Zip.write(artifact, mfEntry #:: resourceStreams #::: zipStreams, header)
                artifact.file(Expect).setPermissions(executable = true)
              
        if publishSonatype then Sonatype.publish(build, env.get(t"SONATYPE_PASSWORD"))
      
      if watch then
        val dirs = build.sourceDirs.to(List) ++ build.bases.to(List)
        Vex.build(false, target, true, if waitForChange(dirs) then None else Some(build), build.pwd, env, scriptFile)
      else ExitStatus.Ok
    catch
      case err: BrokenLinkError =>
        Out.println(t"The reference to ${err.link} could not be resolved")
        restart()
      case err: IoError =>
        throw AppError(t"An I/O error occurred", err)
      
      case err: TtyError =>
        Out.println(t"Could not capture TTY")
        ExitStatus.Fail(1)
      
      case err: InotifyError =>
        Out.println(t"The limit of the number if inotify instances has been exceeded")
        ExitStatus.Fail(1)
      
      case err: StreamCutError =>
        Out.println(t"The stream was cut")
        restart()
      
      case err: ExcessDataError =>
        Out.println(t"Too much data")
        restart()
      
      case err: BuildfileError =>
        Out.println(t"There was an error with the build file")
        restart()

  def waitForChange(dirs: List[Directory])(using Stdout)
                   : Boolean throws InotifyError | IoError =
    Out.println(t"Watching ${dirs.size} directories for changes")
    val watchers = dirs.map:
      case dir: Unix.Directory => dir.watch()

    val stream = LazyList.multiplex(watchers.map(_.stream.filter(_ != Unix.FileEvent.NoChange))*)
    if stream.isEmpty then sys.exit(0)
    else
      watchers.foreach(_.stop())
      
      stream.head match
        case Unix.FileEvent.Modify(file) =>
          Out.println(ansi"The file $Violet(${file.path.show}) was modified")
          !file.path.name.startsWith(t".") && file.path.name.endsWith(t".vex")
        
        case Unix.FileEvent.Delete(path) =>
          Out.println(ansi"The file $Violet(${path.show}) was deleted")
          !path.name.startsWith(t".") && path.name.endsWith(t".vex")
        
        case Unix.FileEvent.NewFile(file) =>
          Out.println(ansi"The file $Violet(${file.path.show}) was created")
          !file.path.name.startsWith(t".") && file.path.name.endsWith(t".vex")
        
        case Unix.FileEvent.NewDirectory(dir) =>
          Out.println(ansi"The directory $Violet(${dir.path.show}) was created")
          false
        
        case _ =>
          false

case class Hash(id: Text, digest: Text, bin: Text)
case class Cache(hashes: Set[Hash])
case class Versioning(versions: List[Version])

case class Version(digest: Text, major: Int, minor: Int):
  def version: Text = t"$major.$minor"

@xmlLabel("organization")
case class Organization(name: Text, url: Text)

@xmlLabel("license")
case class License(name: Text, url: Text, distribution: Text)


@xmlLabel("scm")
case class Scm(url: Text, connection: Text)

@xmlLabel("developer")
case class Developer(id: Text, name: Text, url: Text)

object Dependency:
  given Json.Reader[Dependency] = summon[Json.Reader[Text]].map:
    case s"$group:$artifact:$version" => Dependency(group.show, artifact.show, version.show)

@xmlLabel("dependency")
case class Dependency(groupId: Text, artifactId: Text, version: Text)

@xmlLabel("project")
case class Project(modelVersion: Text, groupId: Text, artifactId: Text, version: Text,
    licenses: List[License], name: Text, description: Text, inceptionYear: Text, url: Text,
    organization: Organization, scm: Scm, developers: List[Developer],
    dependencies: List[Dependency])

object Pom:
  def apply(build: Build, step: Step, year: Int, url: Text, git: Text, publishing: Publishing)
           : Project =
    Project(
      modelVersion = t"4.0.0",
      groupId = step.group(build),
      artifactId = step.dashed,
      version = step.version,
      licenses = List(License(t"Apache 2", t"http://www.apache.org/licenses/LICENSE-2.0.txt", t"repo")),
      name = step.name,
      description = step.name,
      inceptionYear = year.show,
      url = url,
      organization = publishing.organization,
      scm = Scm(t"https://$git/", t"scm:git:git@$git.git"),
      developers = publishing.developers,
      dependencies = step.pomDependencies(build)
    )

class FileCache[T]:
  private val files: scm.HashMap[Text, (Long, T)] = scm.HashMap()
  
  def apply(filename: Text, modified: Long)(calc: => T): T throws IoError =
    if !files.get(filename).fold(false)(_(0) == modified) then files(filename) = modified -> calc
    files(filename)(1)
given realm: Realm = Realm(t"vex")


object Compiler:
  import dotty.tools.dotc.*, reporting.*

  class CustomReporter() extends Reporter:
    var errors: scm.ListBuffer[Diagnostic] = scm.ListBuffer()
    def doReport(diagnostic: Diagnostic)(using core.Contexts.Context): Unit =
      errors += diagnostic

  val Scala3 = new dotty.tools.dotc.Compiler()

  def compile(id: Text, files: List[File], inputs: Set[DiskPath], out: Directory, script: File)
             (using Stdout)
             : List[vex.Message] =
    import unsafeExceptions.canThrowAny
    import dotty.tools.*, io.{File as _, *}, repl.*, dotc.core.*
    
    val reporter = CustomReporter()
    try
      val separator: Text = try Sys.path.separator() catch case err: KeyNotFoundError => t":"
      //val classpath = Unix.parse(t"/home/propensive/niveau/scala/dist/target/pack/lib").get.directory(Expect).children.map(_.fullname)
      val classpath: List[Text] = inputs.map(_.fullname).to(List) :+ Vex.vexJar(script).fullname
      val classpathText = classpath.join(separator)
      
      val callbackApi = new interfaces.CompilerCallback:
        override def onClassGenerated(source: interfaces.SourceFile,
                                          generatedClass: interfaces.AbstractFile,
                                          className: String): Unit =
          ()//Out.println(t"Generated class ${className}")
  
        override def onSourceCompiled(source: interfaces.SourceFile): Unit =
          ()//Out.println(t"Compiled source ${source.toString}")
      
      object driver extends dotc.Driver:
        val currentCtx =
          val ctx = initCtx.fresh
          setup(Array[String]("-d", out.fullname.s, "-deprecation", "-feature", "-Wunused:all",
              "-new-syntax", "-Yrequire-targetName", "-Ysafe-init", "-Yexplicit-nulls",
              "-Ycheck-all-patmat", ""), ctx).map(_(1)).getOrElse(ctx)
        
        def run(files: List[File], classpath: Text): List[Diagnostic] =
          val ctx = currentCtx.fresh
          val newCtx = ctx
            .setReporter(reporter)
            .setCompilerCallback(callbackApi)
            .setSetting(ctx.settings.language, List("experimental.fewerBraces", "experimental.saferExceptions", "experimental.erasedDefinitions"))
            .setSetting(ctx.settings.classpath, classpath.s)
          
          val sources = files.to(List).map:
            file => PlainFile(Path(file.fullname.s))
          
          val run = Scala3.newRun(using newCtx)
          run.compile(sources)
          finish(Scala3, run)(using newCtx)
          reporter.errors.to(List)

      driver.run(files, classpathText).flatMap:
        diagnostic =>
          if diagnostic.level == 2 && diagnostic.position.isPresent then
            val pos = diagnostic.position.get.nn
            val line = pos.line
            val file = pos.source.nn.name.nn
            val content = pos.source.nn.content.nn
            List(vex.Message(id, file.show, line, pos.startColumn, pos.endColumn, diagnostic.message.show, content.unsafeImmutable))
          else Nil

    catch case err: Throwable =>
      Out.println(StackTrace(err).ansi)
      List(vex.Message(id, t"", 0, 0, 0, t"The compiler crashed", IArray()))

object Zip:
  import java.io.*
  import java.util.zip.*
  
  case class Entry(path: Relative, get: () => DataStream):
    def apply(): DataStream = get()

  def read(file: jovian.File): LazyList[Entry] =
    val zipFile = ZipFile(file.javaFile).nn
    zipFile.entries.nn.asScala.to(LazyList).filter(!_.getName.nn.endsWith("/")).map:
      entry =>
        def getEntry(): DataStream = Util.readInputStream(zipFile.getInputStream(entry).nn, 1.mb)
        Entry(Relative.parse(entry.getName.nn.show), getEntry)
  
  def write(path: jovian.DiskPath, inputs: LazyList[Entry], prefix: Maybe[Bytes] = Unset)
           (using Stdout)
           : Unit throws StreamCutError =
    val fileOut = FileOutputStream(path.javaFile).nn
    prefix.option.foreach:
      bytes =>
        fileOut.write(bytes.unsafeMutable)
        fileOut.flush()
    
    val zipOut = ZipOutputStream(fileOut).nn
    
    val dirs = inputs.map(_.path).map(_.parent).to(Set).flatMap:
      dir => (0 to dir.parts.length).map { n => Relative(0, dir.parts.take(n)) }.to(Set)
    .to(List).map(_.show+t"/").sorted
    
    val epoch = java.nio.file.attribute.FileTime.fromMillis(946684800000L)
    Out.println(t"Writing $path...")
    for dir <- dirs do
      val entry = ZipEntry(dir.s).nn.setLastAccessTime(epoch).nn.setCreationTime(epoch).nn
          .setLastModifiedTime(epoch).nn
      
      zipOut.putNextEntry(entry)
      zipOut.closeEntry()
    
    val entries = inputs.to(List).distinctBy(_.path.show).sortBy(_.path.show)
    
    for entry <- entries do
      val zipEntry = ZipEntry(entry.path.show.s).nn.setLastAccessTime(epoch).nn.setCreationTime(
          epoch).nn.setLastModifiedTime(epoch).nn
      
      zipOut.putNextEntry(zipEntry)
      Util.write(entry(), zipOut)
      zipOut.closeEntry()

    zipOut.close()
    fileOut.close()

object Cache:
  import scala.collection.mutable.HashMap
  private val latest: HashMap[Text, Text] = HashMap()
  private val locks: HashMap[DiskPath, Lock] = HashMap()
  
  private class Lock()

  private def synchronously(path: DiskPath)(block: Directory => Unit): Directory throws IoError =
    Cache.synchronized:
      locks.get(path).getOrElse:
        val lock = Lock()
        locks(path) = lock
        lock
    .synchronized:
      if path.isDirectory then path.directory(Expect) else
        val dir = path.directory(Create)
        block(dir)
        dir

  def apply[T: Hashable](key: Text, input: T)(make: (T, Directory) => Unit)
           : Directory throws IoError =
    val hash = input.digest[Crc32].encode[Hex].lower
    
    synchronously(Vex.cacheDir / hash):
      workDir =>
        make(input, workDir)
        
        Cache.synchronized:
          latest.get(key).foreach:
            oldHash => if oldHash != hash then (Vex.cacheDir / hash).directory().delete()
          
          latest(key) = hash
          workDir