package irk

import rudiments.*
import gossamer.*
import serpentine.*
import joviality.*
import turbulence.*

import java.io as ji
import java.nio.file as jnf
import java.util.zip.*

object Zip:

  sealed trait ZipEntry:
    def path: Relative

  case class ZipPath(path: Relative, diskPath: DiskPath[Unix]) extends ZipEntry
  case class Entry(path: Relative, in: ji.InputStream) extends ZipEntry

  def read(file: joviality.File[Unix]): LazyList[Entry] =
    val zipFile = ZipFile(file.javaFile).nn
    
    zipFile.entries.nn.asScala.to(LazyList).filter(!_.getName.nn.endsWith("/")).map: entry =>
      Entry(Relative.parse(entry.getName.nn.show), zipFile.getInputStream(entry).nn)

  // 00:00:00, 1 January 2000
  val epoch = jnf.attribute.FileTime.fromMillis(946684800000L)

  def write(base: joviality.File[Unix], path: joviality.DiskPath[Unix], inputs: LazyList[ZipEntry], prefix: Maybe[Bytes] = Unset)
           (using Stdout)
           : Unit throws StreamCutError | IoError =
    val tmpPath = Irk.tmpDir.tmpPath()
    base.copyTo(tmpPath)
    val uri: java.net.URI = java.net.URI.create(t"jar:file:${tmpPath.show}".s).nn
    
    val fs =
      try jnf.FileSystems.newFileSystem(uri, Map("zipinfo-time" -> "false").asJava).nn
      catch case exception: jnf.ProviderNotFoundException =>
        throw AppError(t"Could not create JAR filesystem: ${uri.toString}")

    val dirs = unsafely(inputs.map(_.path).map(_.parent)).to(Set).flatMap: dir =>
      (0 to dir.parts.length).map(dir.parts.take(_)).map(Relative(0, _)).to(Set)
    .to(List).map(_.show+t"/").sorted

    for dir <- dirs do
      val dirPath = fs.getPath(dir.s).nn
      if jnf.Files.notExists(dirPath) then
        jnf.Files.createDirectory(dirPath)
        jnf.Files.setAttribute(dirPath, "creationTime", epoch)
        jnf.Files.setAttribute(dirPath, "lastAccessTime", epoch)
        jnf.Files.setAttribute(dirPath, "lastModifiedTime", epoch)

    inputs.foreach:
      case Entry(path, in) =>
        val entryPath = fs.getPath(path.show.s).nn
        jnf.Files.copy(in, entryPath, jnf.StandardCopyOption.REPLACE_EXISTING)
        jnf.Files.setAttribute(entryPath, "creationTime", epoch)
        jnf.Files.setAttribute(entryPath, "lastAccessTime", epoch)
        jnf.Files.setAttribute(entryPath, "lastModifiedTime", epoch)
      case ZipPath(path, file) =>
        val filePath = fs.getPath(path.show.s).nn
        jnf.Files.copy(file.javaPath, filePath, jnf.StandardCopyOption.REPLACE_EXISTING)
        jnf.Files.setAttribute(filePath, "creationTime", epoch)
        jnf.Files.setAttribute(filePath, "lastAccessTime", epoch)
        jnf.Files.setAttribute(filePath, "lastModifiedTime", epoch)
    
    fs.close()

    val fileOut = ji.BufferedOutputStream(ji.FileOutputStream(path.javaFile).nn)
    
    prefix.option.foreach: prefix =>
      fileOut.write(prefix.mutable(using Unsafe))
      fileOut.flush()
    
    fileOut.write(jnf.Files.readAllBytes(tmpPath.javaPath))
    fileOut.close()
    java.nio.file.Files.delete(tmpPath.javaPath)
