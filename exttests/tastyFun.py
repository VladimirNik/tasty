#!/usr/bin/python

import re

#FromTasty should be in classpath (TODO as a param)
projectPath = '/home/vova/scala-projects/new-tasty/tasty'
scalacPath = '/home/vova/scala-projects/backendPlugin/scala/build/quick/bin/scalac'
dotcPath = '/home/vova/scala-projects/my-dotty/dotty/bin/dotc'
exttestsFolder = projectPath + '/exttests/'
checkFolder = exttestsFolder + 'check/'
testFolder = exttestsFolder + 'tests/'
useGenBCode = True

def checkTasty( testName, testClass, fromTastyName, testPattern = '', quickTestMode = False ):
  #testName = test1
  #testClass = Test.scala or a/b/c/d/Test.scala
  #fromTastyName = Test

  testAll = testPattern == ''
  data = ''

  if not testAll:
    p = re.compile(testPattern)
    found = re.match(p, testName)

  if testAll or found:
    #path to test file (for its compilation)
    testPath = testFolder + testName
    #path to res
    checkPath = checkFolder + testName + '.check' #test1.check - file with result output

    #read result from file
    #in quickTestMode we don't need to read data we just try to see if there are problems
    #with the compilation of unpickled trees in dotc
    if not quickTestMode:
      try:
        with open (checkPath, "r") as checkFile:
          data = checkFile.read()

      except IOError:
        data = ''

    #commands to run
    cleanCommand = 'cd ' + testPath + " && find . -type f -name '*.class' -delete"
    scalacCommand = scalacPath + (' -Ybackend:GenBCode ' if useGenBCode else ' ') +\
    '-Xplugin:' + projectPath + '/plugin/target/scala-2.11/tasty_2.11.7-0.1.0-SNAPSHOT.jar ' + testClass
    fromTastyCommand = dotcPath + ' -tasty ' + ' -Xprint:front ' + ' -Ycheck:all ' + fromTastyName
  
    runCommand = cleanCommand + '&&' + scalacCommand + '&&' + fromTastyCommand + '&&' + cleanCommand

    import subprocess
    proc = subprocess.Popen([runCommand],
      stdin = subprocess.PIPE,
      stdout = subprocess.PIPE,
      stderr = subprocess.PIPE,
      shell = True
    )
    (out, err) = proc.communicate()

    checkedData = quickTestMode or (data != '' and ((data in out) or (data in err)))

    #print result
    if checkedData and not (('error' in out) or ('error' in err)):
      okStr = 'Test: ' + testName + ' completed'
      print '\033[1;32m' + okStr + '\033[1;m'
    else:
      badStr = 'Test: ' + testName + ' failed'
      print '\033[1;31m' + badStr + '\033[1;m'
    
    #print 'scalacCommand: ' + scalacCommand
    #if not(data in out) and not(data in err):
    #  print 'out: ' + out
    #  print 'err: ' + err
    #  print 'data: ' + data
  
#TODO
#repackage the plugin before running
#sbt 'project tasty' package
#generate .res file with current output if the results are not same
#generated .res file from previous run should be deleted
