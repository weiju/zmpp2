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
end
