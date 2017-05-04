credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

import coursier.Keys._
coursierUseSbtCredentials := true
