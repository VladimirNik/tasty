#!/usr/bin/python

from tastyFun import checkTasty

checkTasty('test1', 'Class1.scala', 'Class1')
checkTasty('trait1', 'Trait1.scala', 'X')
checkTasty('trait2', 'Trait2.scala', 'X2')
checkTasty('package1', 'Package1.scala', 'aaa.bbb.ccc.ddd.Test2')
checkTasty('package2', 'Package2.scala', 'aaa.bbb.ccc.ddd.eee.fff.Test')
checkTasty('traitInh1', 'TraitInh1.scala', 'TrInh2')