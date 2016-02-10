// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import java.util.TimeZone
import org.apache.commons.vfs2.FileObject

object DiffUtil {

  def compareContents(original: Seq[String], revised: Seq[String], originalFile: FileObject, revisedFile: FileObject): String = {
    import collection.JavaConverters._
    val diff = difflib.DiffUtils.diff(original.asJava, revised.asJava)
    val originalInfo = originalFile.getURL.getPath + "\t" + fileModificationTimeOrEpoch(originalFile)
    val revisedInfo = revisedFile.getURL.getPath + "\t" + fileModificationTimeOrEpoch(revisedFile)
    if (diff.getDeltas.isEmpty) ""
    else difflib.DiffUtils.generateUnifiedDiff(originalInfo, revisedInfo, original.asJava, diff, 1).asScala.mkString("", "\n", "\n")
  }

  def fileModificationTimeOrEpoch(file: FileObject): String = {
    val format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss Z")
    if (file.exists)
      format.format(new Date(file.getContent.getLastModifiedTime))
    else {
      format.setTimeZone(TimeZone.getTimeZone("UTC"))
      format.format(new Date(0L))
    }
  }
}
