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
checkTasty('classInh2', 'Test.scala', 'Test')
checkTasty('object1', 'Test.scala', 'Test')
checkTasty('object2', 'Test.scala', 'Test')
checkTasty('object3', 'Test.scala', 'aaa.bbb.ccc.Test')
checkTasty('object4', 'Test.scala', 'Test')
checkTasty('object5', 'Test.scala', 'aaa.bbb.ccc.Test')
checkTasty('objectsWithInnerClasses1', 'Test.scala', 'Test')
checkTasty('objectsWithInnerClasses2', 'Test.scala', 'Test')
checkTasty('typedef1', 'TypeDef1.scala', 'Test')
checkTasty('typedef2', 'TypeDef2.scala', 'Test')
checkTasty('types1', 'Test.scala', 'Test')
checkTasty('types2', 'Test.scala', 'Test')
checkTasty('constrInv', 'Test.scala', 'Test')
checkTasty('traitWithTypeParams1', 'Test.scala', 'Test')
checkTasty('refinedType', 'Test.scala', 'Test')
checkTasty('functions1', 'Test.scala', 'Test')
checkTasty('while', 'Test.scala', 'Test')
checkTasty('bynameparamtype', 'Test.scala', 'Test')
checkTasty('imports', 'Test.scala', 'Test')
checkTasty('patternmatching1', 'Test.scala', 'Test')
checkTasty('this1', 'Test.scala', 'Test')
checkTasty('this2', 'Test.scala', 'Test')
checkTasty('this3', 'Test.scala', 'Test')
checkTasty('super1', 'Test.scala', 'Test')
checkTasty('companion1', 'Test.scala', 'Test')
checkTasty('companion2', 'Test.scala', 'Test')
checkTasty('classWithSelf1', 'Test.scala', 'Test')
checkTasty('classWithSelf2', 'Test.scala', 'Test')
checkTasty('traitWithSelf', 'Test.scala', 'Test')
checkTasty('objectWithSelf1', 'Test.scala', 'Test')
checkTasty('objectWithSelf2', 'Test.scala', 'Test')
checkTasty('objectWithSelf3', 'Test.scala', 'Test')
checkTasty('objectWithSelf4', 'Test.scala', 'Test')
checkTasty('objectWithInnerObject1', 'Test.scala', 'Test')
checkTasty('objectWithInnerObject2', 'Test.scala', 'Test')
checkTasty('expandedCaseClass', 'Test.scala', 'Test')
checkTasty('tryCatchFinally1', 'Test.scala', 'Test')
checkTasty('constrTest1', 'Test.scala', 'Test')
checkTasty('constrTest2', 'Test.scala', 'Test')