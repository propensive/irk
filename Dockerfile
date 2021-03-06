FROM openjdk:11
RUN apt update
RUN apt install -y make
RUN mkdir /irk
RUN git clone https://github.com/lampepfl/dotty /irk/scala
RUN mkdir -p /irk/bin
RUN curl -Lo /irk/sbt.tgz https://github.com/sbt/sbt/releases/download/v1.6.1/sbt-1.6.1.tgz
RUN tar xvf /irk/sbt.tgz -C /irk
ENV PATH="/irk/sbt/bin:${PATH}"
ENV GITHUB_ACTIONS="true"
RUN /irk/scala/bin/scalac -version
RUN mkdir /irk/lib

RUN curl -Lo /irk/lib/jawn-parser.jar \
  https://repo1.maven.org/maven2/org/typelevel/jawn-parser_3/1.2.0/jawn-parser_3-1.2.0.jar

RUN curl -Lo /irk/lib/jawn-ast.jar \
  https://repo1.maven.org/maven2/org/typelevel/jawn-ast_3/1.2.0/jawn-ast_3-1.2.0.jar

RUN curl -Lo /irk/lib/jawn-util.jar \
  https://repo1.maven.org/maven2/org/typelevel/jawn-util_3/1.2.0/jawn-util_3-1.2.0.jar

RUN curl -Lo /irk/lib/servlet-api.jar \
  https://repo1.maven.org/maven2/javax/servlet/javax.servlet-api/3.0.1/javax.servlet-api-3.0.1.jar

RUN curl -Lo /irk/lib/jna.jar \
  https://repo1.maven.org/maven2/net/java/dev/jna/jna/5.3.1/jna-5.3.1.jar

RUN unzip -q -o -d /irk/bin /irk/lib/jawn-parser.jar
RUN unzip -q -o -d /irk/bin /irk/lib/jawn-ast.jar
RUN unzip -q -o -d /irk/bin /irk/lib/jawn-util.jar
RUN unzip -q -o -d /irk/bin /irk/lib/servlet-api.jar
RUN unzip -q -o -d /irk/bin /irk/lib/jna.jar
RUN unzip -q -o -d /irk/bin /irk/scala/dist/target/pack/lib/flexmark-0.42.12.jar
RUN unzip -q -o -d /irk/bin /irk/scala/dist/target/pack/lib/flexmark-util-0.42.12.jar
RUN unzip -q -o -d /irk/bin /irk/scala/dist/target/pack/lib/flexmark-formatter-0.42.12.jar
RUN unzip -q -o -d /irk/bin /irk/scala/dist/target/pack/lib/flexmark-ext-tables-0.42.12.jar
RUN rm -r /irk/bin/META-INF
ADD src /irk/src
ADD one.jar /irk/one.jar
RUN unzip -q /irk/one.jar -d /

RUN javac -classpath /irk/lib/jna.jar \
  -d /irk/bin \
  /one/mod/profanity/src/java/profanity/Termios.java

RUN cp -r /one/mod/gesticulate/res/gesticulate /irk/bin/

RUN cd /irk && scala/bin/scalac \
  -classpath bin \
  -Xmax-inlines 64 \
  -deprecation \
  -feature \
  -Wunused:all \
  -new-syntax \
  -Yrequire-targetName \
  -Ysafe-init \
  -Yexplicit-nulls \
  -Ycheck-all-patmat \
  -language:experimental.fewerBraces \
  -language:experimental.saferExceptions \
  -language:experimental.erasedDefinitions \
  -d bin \
  /one/mod/acyclicity/src/core/*.scala \
  /one/mod/adversaria/src/core/*.scala \
  /one/mod/caesura/src/core/*.scala \
  /one/mod/cataclysm/src/core/*.scala \
  /one/mod/anticipation/src/css/*.scala \
  /one/mod/anticipation/src/file/*.scala \
  /one/mod/anticipation/src/html/*.scala \
  /one/mod/anticipation/src/http/*.scala \
  /one/mod/anticipation/src/time/*.scala \
  /one/mod/anticipation/src/uri/*.scala \
  /one/mod/contextual/src/core/*.scala \
  /one/mod/cosmopolite/src/core/*.scala \
  /one/mod/escapade/src/core/*.scala \
  /one/mod/escritoire/src/core/*.scala \
  /one/mod/eucalyptus/src/core/*.scala \
  /one/mod/euphemism/src/core/*.scala \
  /one/mod/exoskeleton/src/core/*.scala \
  /one/mod/gastronomy/src/core/*.scala \
  /one/mod/gesticulate/src/core/*.scala \
  /one/mod/gossamer/src/core/*.scala \
  /one/mod/guillotine/src/core/*.scala \
  /one/mod/harlequin/src/core/*.scala \
  /one/mod/imperial/src/core/*.scala \
  /one/mod/iridescence/src/core/*.scala \
  /one/mod/joviality/src/core/*.scala \
  /one/mod/kaleidoscope/src/core/*.scala \
  /one/mod/probably/src/cli/*.scala \
  /one/mod/probably/src/core/*.scala \
  /one/mod/probably/src/tolerance/*.scala \
  /one/mod/profanity/src/core/*.scala \
  /one/mod/profanity/src/java/**/*.java \
  /one/mod/rudiments/src/core/*.scala \
  /one/mod/serpentine/src/core/*.scala \
  /one/mod/surveillance/src/core/*.scala \
  /one/mod/telekinesis/src/client/*.scala \
  /one/mod/telekinesis/src/uri/*.scala \
  /one/mod/turbulence/src/core/*.scala \
  /one/mod/wisteria/src/core/*.scala \
  /one/mod/xylophone/src/core/*.scala \
  /one/mod/honeycomb/src/core/*.scala \
  /one/mod/punctuation/src/ansi/*.scala \
  /one/mod/punctuation/src/core/*.scala \
  /one/mod/punctuation/src/html/*.scala \
  /one/mod/scintillate/src/server/*.scala \
  /one/mod/scintillate/src/servlet/*.scala \
  /one/mod/tarantula/src/core/*.scala

RUN cd /irk && scala/bin/scalac \
  -classpath bin \
  -Xmax-inlines 64 \
  -deprecation \
  -feature \
  -Wunused:all \
  -new-syntax \
  -Yrequire-targetName \
  -Ysafe-init \
  -Yexplicit-nulls \
  -Ycheck-all-patmat \
  -language:experimental.fewerBraces \
  -language:experimental.saferExceptions \
  -language:experimental.erasedDefinitions \
  -d bin \
  src/core/*.scala

RUN unzip -q -o -d /irk/bin /irk/scala/dist/target/pack/lib/tasty-core*.jar
RUN unzip -q -o -d /irk/bin /irk/scala/dist/target/pack/lib/compiler-interface*.jar
RUN unzip -q -o -d /irk/bin /irk/scala/dist/target/pack/lib/scala-library*.jar
RUN unzip -q -o -d /irk/bin /irk/scala/dist/target/pack/lib/scala3-compiler*.jar
RUN unzip -q -o -d /irk/bin /irk/scala/dist/target/pack/lib/scala3-library*.jar
RUN unzip -q -o -d /irk/bin /irk/scala/dist/target/pack/lib/scala3-staging*.jar
RUN unzip -q -o -d /irk/bin /irk/scala/dist/target/pack/lib/scala3-interfaces*.jar
RUN unzip -q -o -d /irk/bin /irk/scala/dist/target/pack/lib/scala3-tasty-inspector*.jar
RUN unzip -q -o -d /irk/bin /irk/scala/dist/target/pack/lib/scala-asm*.jar
RUN cp /one/mod/exoskeleton/res/exoskeleton/invoke /irk/bin/exoskeleton/invoke
ADD doc/.version /irk/.version
RUN echo 'Manifest-Version: 1.0' > /irk/manifest
RUN echo -n 'Created-By: Irk ' >> /irk/manifest
RUN cat /irk/.version >> /irk/manifest
RUN echo 'Implementation-Title: Irk' >> /irk/manifest
RUN echo -n 'Implementation-Version: ' >> /irk/manifest
RUN cat /irk/.version >> /irk/manifest
RUN echo 'Main-Class: irk.Irk' >> /irk/manifest

RUN jar cmf /irk/manifest /irk/irk.jar \
  -C /irk/bin NOTICE \
  -C /irk/bin compiler.properties \
  -C /irk/bin incrementalcompiler.version.properties \
  -C /irk/bin library.properties \
  -C /irk/bin scala-asm.properties \
  -C /irk/bin xsbti \
  -C /irk/bin scala \
  -C /irk/bin dotty \
  -C /irk/bin acyclicity \
  -C /irk/bin adversaria \
  -C /irk/bin anticipation \
  -C /irk/bin caesura \
  -C /irk/bin cataclysm \
  -C /irk/bin com \
  -C /irk/bin contextual \
  -C /irk/bin cosmopolite \
  -C /irk/bin escapade \
  -C /irk/bin escritoire \
  -C /irk/bin eucalyptus \
  -C /irk/bin euphemism \
  -C /irk/bin exoskeleton \
  -C /irk/bin gastronomy \
  -C /irk/bin gesticulate \
  -C /irk/bin gossamer \
  -C /irk/bin guillotine \
  -C /irk/bin harlequin \
  -C /irk/bin honeycomb \
  -C /irk/bin iridescence \
  -C /irk/bin irk \
  -C /irk/bin imperial \
  -C /irk/bin javax \
  -C /irk/bin joviality \
  -C /irk/bin kaleidoscope \
  -C /irk/bin org \
  -C /irk/bin probably \
  -C /irk/bin profanity \
  -C /irk/bin punctuation \
  -C /irk/bin rudiments \
  -C /irk/bin scintillate \
  -C /irk/bin serpentine \
  -C /irk/bin surveillance \
  -C /irk/bin tarantula \
  -C /irk/bin telekinesis \
  -C /irk/bin turbulence \
  -C /irk/bin wisteria \
  -C /irk/bin xylophone

RUN cat /one/mod/exoskeleton/res/exoskeleton/invoke /irk/irk.jar > /irk/bootstrap
RUN chmod +x /irk/bootstrap
RUN rm /irk/irk.jar
ADD build.irk /irk/build.irk
RUN cd /irk && ./bootstrap
RUN mv /irk/irk /irk/irk-bootstrap
RUN md5sum /irk/irk-bootstrap | cut -d' ' -f1 > bootstrap.md5
RUN cd /irk && ./irk-bootstrap
RUN md5sum /irk/irk | cut -d' ' -f1 > irk.md5
RUN diff irk.md5 bootstrap.md5
