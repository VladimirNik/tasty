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
quickTestMode = False
if len(sys.argv) > 1:
  pattern = sys.argv[1]
if len(sys.argv) > 2:
  quickTestMode = True

print 'pattern: ' + pattern
print 'quickTestMode: ' + str(quickTestMode)

checkTasty('test1', 'Class1.scala', 'Class1', pattern, quickTestMode)
checkTasty('helloWorld', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('trait1', 'Trait1.scala', 'X', pattern, quickTestMode)
checkTasty('trait2', 'Trait2.scala', 'X2', pattern, quickTestMode)
checkTasty('package1', 'Package1.scala', 'aaa.bbb.ccc.ddd.Test2', pattern, quickTestMode)
checkTasty('package2', 'Package2.scala', 'aaa.bbb.ccc.ddd.eee.fff.Test', pattern, quickTestMode)
checkTasty('traitInh1', 'TraitInh1.scala', 'TrInh2', pattern, quickTestMode)
checkTasty('param1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('param2', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('classInh', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('classInh2', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('classInh3', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('classInh4', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('classInh5', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('classInh6', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('classInh7', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('classInh8', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('classInh9', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('classInh10', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('classInh11', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('caseClass1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('newTest1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('newTest2', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('newTest3', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('object1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('object2', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('object3', 'Test.scala', 'aaa.bbb.ccc.Test', pattern, quickTestMode)
checkTasty('object4', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('object5', 'Test.scala', 'aaa.bbb.ccc.Test', pattern, quickTestMode)
checkTasty('objectsWithInnerClasses1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('objectsWithInnerClasses2', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('typedef1', 'TypeDef1.scala', 'Test', pattern, quickTestMode)
checkTasty('typedef2', 'TypeDef2.scala', 'Test', pattern, quickTestMode)
checkTasty('types1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('types2', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('constrInv', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('traitWithTypeParams1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('refinedType', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('functions1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('while', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('bynameparamtype', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('imports', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('patternmatching1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('this1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('this2', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('this3', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('super1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('companion1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('companion2', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('classWithSelf1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('classWithSelf2', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('classWithSelf3', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('traitWithSelf', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('objectWithSelf1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('objectWithSelf2', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('objectWithSelf3', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('objectWithSelf4', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('objectWithInnerObject1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('objectWithInnerObject2', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('expandedCaseClass', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('tryCatchFinally1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('constrTest1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('constrTest2', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('constrTest3', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('constrTest4', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('constrTest5', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('constrTest6', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('constrWithMultiArgs1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('constrWithMultiArgs2', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('overloadedConstr1', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('overloadedConstr2', 'Test.scala', 'Test', pattern, quickTestMode)
checkTasty('overloadedConstr3', 'Test.scala', 'Test', pattern, quickTestMode)