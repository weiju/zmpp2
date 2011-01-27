require 'buildr/scala'

repositories.remote << 'http://www.ibiblio.org/maven2'
repositories.remote << 'http://zmpp.sourceforge.net/maven2'

define 'zmpp' do
  project.group = 'org.zmpp'
  project.version = '2.0-SNAPSHOT'

  define 'zmpp-common' do
    test.with 'org.scala-tools.testing:specs_2.8.0:jar:1.6.5'
    test.using :specs
    package :jar
  end

  define 'zmpp-glk' do
    compile.with project('zmpp-common')
    test.with 'org.scala-tools.testing:specs_2.8.0:jar:1.6.5'
    test.using :specs
    package :jar
  end

  define 'zmpp-glulx' do
    compile.with projects('zmpp-common', 'zmpp-glk')
    test.with 'org.scala-tools.testing:specs_2.8.0:jar:1.6.5'
    test.using :specs
    package :jar
  end

  define 'zmpp-tads3' do
    compile.with project('zmpp-common')
    test.with 'org.scala-tools.testing:specs_2.8.0:jar:1.6.5'
    test.using :specs
    run.using :main => ["org.zmpp.tads3.Tads3Main",
                        "/Users/weiju/Games/tads3/IMustPlay.t3"]
  end

  define 'zmpp-zcode' do
    compile.with project('zmpp-common')
    test.with 'org.scala-tools.testing:specs_2.8.0:jar:1.6.5'
    test.using :specs
    package :jar
  end
end
