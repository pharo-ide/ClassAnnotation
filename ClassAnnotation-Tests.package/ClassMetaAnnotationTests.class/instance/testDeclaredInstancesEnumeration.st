tests
testDeclaredInstancesEnumeration

	| enumerated |
	enumerated := OrderedCollection new.
	ClassMetaAnnotationExample1 declaredInstancesDo: [:each | enumerated add: each].
	
	self assert: enumerated asArray equals: ClassMetaAnnotationExample1 declaredInstances asArray