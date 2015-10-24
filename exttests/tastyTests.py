#!/usr/bin/python

import sys
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

pattern = ''
if len(sys.argv) > 1:
  pattern = sys.argv[1]

print 'pattern: ' + pattern

checkTasty('test1', 'Class1.scala', 'Class1', pattern)
checkTasty('helloWorld', 'Test.scala', 'Test', pattern)
checkTasty('trait1', 'Trait1.scala', 'X', pattern)
checkTasty('trait2', 'Trait2.scala', 'X2', pattern)
checkTasty('package1', 'Package1.scala', 'aaa.bbb.ccc.ddd.Test2', pattern)
checkTasty('package2', 'Package2.scala', 'aaa.bbb.ccc.ddd.eee.fff.Test', pattern)
checkTasty('traitInh1', 'TraitInh1.scala', 'TrInh2', pattern)
checkTasty('param1', 'Test.scala', 'Test', pattern)
checkTasty('param2', 'Test.scala', 'Test', pattern)
checkTasty('classInh', 'Test.scala', 'Test', pattern)
checkTasty('classInh2', 'Test.scala', 'Test', pattern)
checkTasty('classInh3', 'Test.scala', 'Test', pattern)
checkTasty('classInh4', 'Test.scala', 'Test', pattern)
checkTasty('classInh5', 'Test.scala', 'Test', pattern)
checkTasty('classInh6', 'Test.scala', 'Test', pattern)
checkTasty('classInh7', 'Test.scala', 'Test', pattern)
checkTasty('classInh8', 'Test.scala', 'Test', pattern)
checkTasty('caseClass1', 'Test.scala', 'Test', pattern)
checkTasty('newTest1', 'Test.scala', 'Test', pattern)
checkTasty('newTest2', 'Test.scala', 'Test', pattern)
checkTasty('newTest3', 'Test.scala', 'Test', pattern)
checkTasty('object1', 'Test.scala', 'Test', pattern)
checkTasty('object2', 'Test.scala', 'Test', pattern)
checkTasty('object3', 'Test.scala', 'aaa.bbb.ccc.Test', pattern)
checkTasty('object4', 'Test.scala', 'Test', pattern)
checkTasty('object5', 'Test.scala', 'aaa.bbb.ccc.Test', pattern)
checkTasty('objectsWithInnerClasses1', 'Test.scala', 'Test', pattern)
checkTasty('objectsWithInnerClasses2', 'Test.scala', 'Test', pattern)
checkTasty('typedef1', 'TypeDef1.scala', 'Test', pattern)
checkTasty('typedef2', 'TypeDef2.scala', 'Test', pattern)
checkTasty('types1', 'Test.scala', 'Test', pattern)
checkTasty('types2', 'Test.scala', 'Test', pattern)
checkTasty('constrInv', 'Test.scala', 'Test', pattern)
checkTasty('traitWithTypeParams1', 'Test.scala', 'Test', pattern)
checkTasty('refinedType', 'Test.scala', 'Test', pattern)
checkTasty('functions1', 'Test.scala', 'Test', pattern)
checkTasty('while', 'Test.scala', 'Test', pattern)
checkTasty('bynameparamtype', 'Test.scala', 'Test', pattern)
checkTasty('imports', 'Test.scala', 'Test', pattern)
checkTasty('patternmatching1', 'Test.scala', 'Test', pattern)
checkTasty('this1', 'Test.scala', 'Test', pattern)
checkTasty('this2', 'Test.scala', 'Test', pattern)
checkTasty('this3', 'Test.scala', 'Test', pattern)
checkTasty('super1', 'Test.scala', 'Test', pattern)
checkTasty('companion1', 'Test.scala', 'Test', pattern)
checkTasty('companion2', 'Test.scala', 'Test', pattern)
checkTasty('classWithSelf1', 'Test.scala', 'Test', pattern)
checkTasty('classWithSelf2', 'Test.scala', 'Test', pattern)
checkTasty('classWithSelf3', 'Test.scala', 'Test', pattern)
checkTasty('traitWithSelf', 'Test.scala', 'Test', pattern)
checkTasty('objectWithSelf1', 'Test.scala', 'Test', pattern)
checkTasty('objectWithSelf2', 'Test.scala', 'Test', pattern)
checkTasty('objectWithSelf3', 'Test.scala', 'Test', pattern)
checkTasty('objectWithSelf4', 'Test.scala', 'Test', pattern)
checkTasty('objectWithInnerObject1', 'Test.scala', 'Test', pattern)
checkTasty('objectWithInnerObject2', 'Test.scala', 'Test', pattern)
checkTasty('expandedCaseClass', 'Test.scala', 'Test', pattern)
checkTasty('tryCatchFinally1', 'Test.scala', 'Test', pattern)
checkTasty('constrTest1', 'Test.scala', 'Test', pattern)
checkTasty('constrTest2', 'Test.scala', 'Test', pattern)
checkTasty('constrTest3', 'Test.scala', 'Test', pattern)
checkTasty('constrTest4', 'Test.scala', 'Test', pattern)
checkTasty('constrTest5', 'Test.scala', 'Test', pattern)
checkTasty('constrTest6', 'Test.scala', 'Test', pattern)
checkTasty('constrWithMultiArgs1', 'Test.scala', 'Test', pattern)
checkTasty('constrWithMultiArgs2', 'Test.scala', 'Test', pattern)