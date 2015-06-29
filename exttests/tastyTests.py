#!/usr/bin/python

from tastyFun import checkTasty, projectPath

moveCommand = "cd " + projectPath
sbtCommand = "sbt 'project tasty' package"
packageCommand = moveCommand + ' && ' + sbtCommand

import subprocess
proc = subprocess.Popen([packageCommand],
  stdin = subprocess.PIPE,
  stdout = subprocess.PIPE,
  stderr = subprocess.PIPE,
  shell = True
)
(out, err) = proc.communicate()
print out

checkTasty('test1', 'Class1.scala', 'Class1')
checkTasty('trait1', 'Trait1.scala', 'X')
checkTasty('trait2', 'Trait2.scala', 'X2')
checkTasty('package1', 'Package1.scala', 'aaa.bbb.ccc.ddd.Test2')
checkTasty('package2', 'Package2.scala', 'aaa.bbb.ccc.ddd.eee.fff.Test')
checkTasty('traitInh1', 'TraitInh1.scala', 'TrInh2')
checkTasty('param1', 'Test.scala', 'Test')
checkTasty('param2', 'Test.scala', 'Test')
checkTasty('classInh', 'Test.scala', 'Test')
checkTasty('object1', 'Test.scala', 'Test')
