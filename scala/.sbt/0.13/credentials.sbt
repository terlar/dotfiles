credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

import coursier.Keys._
coursierUseSbtCredentials := true

import org.ensime.EnsimeCoursierKeys._
import org.ensime.EnsimeKeys._
ensimeIgnoreMissingDirectories := true
