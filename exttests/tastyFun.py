#!/usr/bin/python

#FromTasty should be in classpath (TODO as a param)
projectPath = '/home/vova/scala-projects/new-tasty/tasty/'
exttestsFolder = projectPath + 'exttests/'
checkFolder = exttestsFolder + 'check/'
testFolder = exttestsFolder + 'tests/'

def checkTasty( testName, testClass, fromTastyName ):
  #testName = test1
  #testClass = Test.scala or a/b/c/d/Test.scala
  #fromTastyName = Test

  #path to test file (for its compilation)
  testPath = testFolder + testName
  #path to res
  checkPath = checkFolder + testName + '.check' #test1.check - file with result output

  #read result from file
  try:
    with open (checkPath, "r") as checkFile:
      data = checkFile.read()

  except IOError:
    data = ''

  #commands to run
  cleanCommand = 'cd ' + testPath + " && find . -type f -name '*.class' -delete"
  scalacCommand = '/home/vova/scala/scala-2.11.5/bin/scalac -Ybackend:GenBCode ' +\
  '-Xplugin:' + projectPath + 'plugin/target/scala-2.11/tasty_2.11.5-0.1.0-SNAPSHOT.jar ' + testClass #Test.scala - should be added folder to find
  fromTastyCommand = 'java -Xmx768m -Xms768m ' +\
  '-Xbootclasspath/a:/home/vova/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.5.jar:/home/vova/.ivy2/cache/org.scala-lang/scala-reflect/jars/scala-reflect-2.11.5.jar:/home/vova/.ivy2/cache/me.d-d/scala-compiler/jars/scala-compiler-2.11.5-20150416-144435-09c4a520e1.jar:/home/vova/.ivy2//cache/jline/jline/jars/jline-2.12.jar:/home/vova/scala-projects/my-dotty/dotty/bin/../target/scala-2.11/dotty_2.11-0.1-SNAPSHOT.jar ' +\
  '-classpath /home/vova/scala-projects/my-dotty/dotty/bin/../target/scala-2.11/dotty_2.11-0.1-SNAPSHOT.jar:/home/vova/scala-projects/my-dotty/dotty/bin/../target/scala-2.11/dotty_2.11-0.1-SNAPSHOT-tests.jar -Dscala.usejavacp=true dotty.tools.dotc.FromTasty ' +\
  '-Xprint:front ' + fromTastyName #Test - it should be added to classpath

  runCommand = cleanCommand + '&&' + scalacCommand + '&&' + fromTastyCommand + '&&' + cleanCommand

  import subprocess
  proc = subprocess.Popen([runCommand],
    stdin = subprocess.PIPE,
    stdout = subprocess.PIPE,
    stderr = subprocess.PIPE,
    shell = True
  )
  (out, err) = proc.communicate()

  #print result
  if data != '' and data in out:
    okStr = 'Test: ' + testName + ' completed'
    print '\033[1;32m' + okStr + '\033[1;m'
  else:
    badStr = 'Test: ' + testName + ' failed'
    print '\033[1;31m' + badStr + '\033[1;m'

  #print out
  #print data

checkTasty('test1', 'Class1.scala', 'Class1')
checkTasty('trait1', 'Trait1.scala', 'X')
checkTasty('trait2', 'Trait2.scala', 'X2')
checkTasty('package1', 'Package1.scala', 'aaa.bbb.ccc.ddd.Test2')
checkTasty('package2', 'Package2.scala', 'aaa.bbb.ccc.ddd.eee.fff.Test')
checkTasty('traitInh1', 'TraitInh1.scala', 'TrInh2')

#TODO
#separate files - create module - funcs and invocation
#repackage the plugin before running
#sbt 'project tasty' package
#generate .res file with current output if the results are not same
#generated .res file from previous run should be deleted