FROM openjdk:11
RUN apt update
RUN apt install -y make
RUN mkdir /vex
RUN git clone https://github.com/lampepfl/dotty /vex/scala
RUN mkdir -p /vex/bin
RUN curl -Lo /vex/sbt.tgz https://github.com/sbt/sbt/releases/download/v1.6.1/sbt-1.6.1.tgz
RUN tar xvf /vex/sbt.tgz -C /vex
ENV PATH="/vex/sbt/bin:${PATH}"
RUN /vex/scala/bin/scalac -version
RUN mkdir /vex/lib

RUN curl -Lo /vex/lib/jawn-parser.jar \
  https://repo1.maven.org/maven2/org/typelevel/jawn-parser_3/1.2.0/jawn-parser_3-1.2.0.jar

RUN curl -Lo /vex/lib/jawn-ast.jar \
  https://repo1.maven.org/maven2/org/typelevel/jawn-ast_3/1.2.0/jawn-ast_3-1.2.0.jar

RUN curl -Lo /vex/lib/jawn-util.jar \
  https://repo1.maven.org/maven2/org/typelevel/jawn-util_3/1.2.0/jawn-util_3-1.2.0.jar

RUN curl -Lo /vex/lib/servlet-api.jar \
  https://repo1.maven.org/maven2/javax/servlet/javax.servlet-api/3.0.1/javax.servlet-api-3.0.1.jar

RUN curl -Lo /vex/lib/jna.jar \
  https://repo1.maven.org/maven2/net/java/dev/jna/jna/5.3.1/jna-5.3.1.jar

RUN unzip -q -o -d /vex/bin /vex/lib/jawn-parser.jar
RUN unzip -q -o -d /vex/bin /vex/lib/jawn-ast.jar
RUN unzip -q -o -d /vex/bin /vex/lib/jawn-util.jar
RUN unzip -q -o -d /vex/bin /vex/lib/servlet-api.jar
RUN unzip -q -o -d /vex/bin /vex/lib/jna.jar
RUN unzip -q -o -d /vex/bin /vex/scala/dist/target/pack/lib/flexmark-0.42.12.jar
RUN unzip -q -o -d /vex/bin /vex/scala/dist/target/pack/lib/flexmark-util-0.42.12.jar
RUN unzip -q -o -d /vex/bin /vex/scala/dist/target/pack/lib/flexmark-formatter-0.42.12.jar
RUN unzip -q -o -d /vex/bin /vex/scala/dist/target/pack/lib/flexmark-ext-tables-0.42.12.jar
RUN rm -r /vex/bin/META-INF
ADD src /vex/src
ADD niveau /vex/niveau

RUN javac -classpath /vex/lib/jna.jar \
  -d /vex/bin \
  /vex/niveau/mod/profanity/src/java/profanity/Termios.java

RUN cd /vex && scala/bin/scalac \
  -classpath bin \
  -language:experimental.fewerBraces \
  -language:experimental.saferExceptions \
  -language:experimental.erasedDefinitions \
  -d bin \
  src/core/*.scala \
  niveau/mod/acyclicity/src/core/*.scala \
  niveau/mod/adversaria/src/core/*.scala \
  niveau/mod/caesura/src/core/*.scala \
  niveau/mod/cataract/src/core/*.scala \
  niveau/mod/clairvoyant/src/css/*.scala \
  niveau/mod/clairvoyant/src/time/*.scala \
  niveau/mod/clairvoyant/src/html/*.scala \
  niveau/mod/clairvoyant/src/http/*.scala \
  niveau/mod/contextual/src/core/*.scala \
  niveau/mod/cosmopolite/src/core/*.scala \
  niveau/mod/escapade/src/core/*.scala \
  niveau/mod/escritoire/src/core/*.scala \
  niveau/mod/eucalyptus/src/core/*.scala \
  niveau/mod/euphemism/src/core/*.scala \
  niveau/mod/exoskeleton/src/core/*.scala \
  niveau/mod/gastronomy/src/core/*.scala \
  niveau/mod/gesticulate/src/core/*.scala \
  niveau/mod/gossamer/src/core/*.scala \
  niveau/mod/guillotine/src/core/*.scala \
  niveau/mod/harlequin/src/core/*.scala \
  niveau/mod/honeycomb/src/core/*.scala \
  niveau/mod/iridescence/src/core/*.scala \
  niveau/mod/jovian/src/core/*.scala \
  niveau/mod/kaleidoscope/src/core/*.scala \
  niveau/mod/probably/src/core/*.scala \
  niveau/mod/probably/src/cli/*.scala \
  niveau/mod/probably/src/tolerance/*.scala \
  niveau/mod/punctuation/src/core/*.scala \
  niveau/mod/punctuation/src/ansi/*.scala \
  niveau/mod/punctuation/src/html/*.scala \
  niveau/mod/profanity/src/java/**/*.java \
  niveau/mod/profanity/src/core/*.scala \
  niveau/mod/rudiments/src/core/*.scala \
  niveau/mod/scintillate/src/core/*.scala \
  niveau/mod/scintillate/src/server/*.scala \
  niveau/mod/scintillate/src/servlet/*.scala \
  niveau/mod/slalom/src/core/*.scala \
  niveau/mod/wisteria/src/core/*.scala \
  niveau/mod/xylophone/src/core/*.scala

RUN unzip -q -o -d /vex/bin /vex/scala/dist/target/pack/lib/tasty-core*.jar
RUN unzip -q -o -d /vex/bin /vex/scala/dist/target/pack/lib/compiler-interface*.jar
RUN unzip -q -o -d /vex/bin /vex/scala/dist/target/pack/lib/scala-library*.jar
RUN unzip -q -o -d /vex/bin /vex/scala/dist/target/pack/lib/scala3-compiler*.jar
RUN unzip -q -o -d /vex/bin /vex/scala/dist/target/pack/lib/scala3-library*.jar
RUN unzip -q -o -d /vex/bin /vex/scala/dist/target/pack/lib/scala3-staging*.jar
RUN unzip -q -o -d /vex/bin /vex/scala/dist/target/pack/lib/scala3-interfaces*.jar
RUN unzip -q -o -d /vex/bin /vex/scala/dist/target/pack/lib/scala3-tasty-inspector*.jar
RUN unzip -q -o -d /vex/bin /vex/scala/dist/target/pack/lib/scala-asm*.jar
RUN cp /vex/niveau/mod/exoskeleton/res/exoskeleton/invoke /vex/bin/exoskeleton/invoke

RUN jar cfe /vex/vex.jar vex.Vex \
  -C /vex/bin NOTICE \
  -C /vex/bin compiler.properties \
  -C /vex/bin incrementalcompiler.version.properties \
  -C /vex/bin library.properties \
  -C /vex/bin scala-asm.properties \
  -C /vex/bin xsbti \
  -C /vex/bin scala \
  -C /vex/bin dotty \
  -C /vex/bin acyclicity \
  -C /vex/bin adversaria \
  -C /vex/bin caesura \
  -C /vex/bin cataract \
  -C /vex/bin clairvoyant \
  -C /vex/bin com \
  -C /vex/bin contextual \
  -C /vex/bin cosmopolite \
  -C /vex/bin escapade \
  -C /vex/bin escritoire \
  -C /vex/bin eucalyptus \
  -C /vex/bin euphemism \
  -C /vex/bin exoskeleton \
  -C /vex/bin gastronomy \
  -C /vex/bin gesticulate \
  -C /vex/bin gossamer \
  -C /vex/bin guillotine \
  -C /vex/bin harlequin \
  -C /vex/bin honeycomb \
  -C /vex/bin iridescence \
  -C /vex/bin javax \
  -C /vex/bin jovian \
  -C /vex/bin kaleidoscope \
  -C /vex/bin org \
  -C /vex/bin probably \
  -C /vex/bin profanity \
  -C /vex/bin punctuation \
  -C /vex/bin rudiments \
  -C /vex/bin scintillate \
  -C /vex/bin slalom \
  -C /vex/bin vex \
  -C /vex/bin wisteria \
  -C /vex/bin xylophone

RUN cat /vex/niveau/mod/exoskeleton/res/exoskeleton/invoke /vex/vex.jar > /vex/bootstrap
RUN chmod +x /vex/bootstrap
RUN rm /vex/vex.jar
ADD build.vex /vex/build.vex
RUN cd /vex && ./bootstrap
RUN mv /vex/vex-* /vex/vex
RUN md5sum /vex/vex > bootstrap.md5
RUN cd /vex && ./vex
RUN mv /vex/vex-* /vex/vex
RUN md5sum /vex/vex > vex.md5
RUN diff vex.md5 bootstrap.md5
